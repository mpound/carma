#include <carma/util/Time.h>
#include <carma/util/Trace.h>
#include <carma/util/ThreadQuit.h>
#include <carma/util/StartPthread.h>
#include <carma/util/ErrorException.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/programLogging.h>
#include <carma/util/ScopedPthreadMutexLock.h>
#include <carma/util/AutoPthreadQuitAndJoinGroup.h>
using namespace carma::util;

#include <carma/linelength/RMSTracker.h>
#include <carma/linelength/ComediDevice.h>
#include <carma/linelength/LLWorkerBoard0Thread.h>
using namespace carma::linelength;

#include <sstream>

#include <fftw3.h>

#define NUM_ANT_CHANS   24
#define NUM_REF_CHANS   3

#define NUM_PHASE_CHANS (NUM_ANT_CHANS + NUM_REF_CHANS)

#define CPTRACE6(args...) CPTRACE(Trace::TRACE6, ##args)

/*----------------------------------------------------------------------------*/
/* LLWorkerBoard0Thread Object                                                */
/*----------------------------------------------------------------------------*/

LLWorkerBoard0Thread::LLWorkerBoard0Thread(const std::string &device)
    : device_(device)
{
    programLogDebugIfPossible("LLWorkerBoard0Thread ctor");
}

LLWorkerBoard0Thread::~LLWorkerBoard0Thread()
{
    programLogDebugIfPossible("LLWorkerBoard0Thread dtor");
}

/*
 * Create the expected comedi command structure
 *
 * This scans are ordered to make most of the code in this class easy to
 * understand. Due to hardware limitations, the antennas are in blocks of 8,
 * and the spare (24th antenna) is interleaved on the second block.
 *
 * The digital multiplexer on the NI PXI-6031E card doesn't care what order
 * we sample in, so we make it as easy as possible to understand rather than
 * letting the hardware configuration leak into the rest of the code.
 */
static void
createComediCommand(ComediDevice &comedi, comedi_cmd &cmd, std::vector<unsigned int> &chans)
{
    const int range = comedi.findRange(-1.0, 1.0);
    const int aref = AREF_DIFF;

    // Reference 1-3 Phase
    for (int i = 0; i < 3; i++)
        chans.push_back(CR_PACK(i + 0, range, aref));

    // Antenna 1-8 Phase
    for (int i = 0; i < 8; i++)
        chans.push_back(CR_PACK(i + 16, range, aref));
    // Antenna 9-15 Phase
    for (int i = 0; i < 7; i++)
        chans.push_back(CR_PACK(i + 32, range, aref));
    // Antenna 16-23 Phase
    for (int i = 0; i < 8; i++)
        chans.push_back(CR_PACK(i + 48, range, aref));
    // Antenna 24 Phase
    chans.push_back(CR_PACK(39, range, aref));

    // Comedi Library Command Structure
    cmd.subdev          = 0;                        // analog input
    cmd.flags           = TRIG_ROUND_NEAREST;       // default

    cmd.start_src       = TRIG_NOW;                 // default
    cmd.start_arg       = 0;                        // default

    cmd.scan_begin_src  = TRIG_TIMER;               // default
    cmd.scan_begin_arg  = 500000;                   // 2000 Hz == 500000 ns == 0.5 ms

    cmd.convert_src     = TRIG_TIMER;               // default
    cmd.convert_arg     = 18500;                    // 500000 ns / 27 chans == 18500 ns per conversion

    cmd.scan_end_src    = TRIG_COUNT;               // default
    cmd.scan_end_arg    = chans.size();             // 54 channels

    cmd.stop_src        = TRIG_NONE;                // run indefinitely
    cmd.stop_arg        = 0;                        // run indefinitely

    cmd.chanlist        = &chans.at(0);             // 54 elements of CR_PACK()
    cmd.chanlist_len    = chans.size();             // 54 channels
}

// Convert a ComediSamplePtr buffer from scan-oriented data into
// channel-oriented data. We want all data from a single channel in a single
// vector. This is the format that FFT expects.
//
// This is basically numpy.transpose() of a 2D matrix
static std::vector< std::vector < double > >
convertBufferToChannelData(ComediSamplePtr buf)
{
    std::vector< std::vector < double > > vec(buf->nchannels);
    for (size_t i = 0; i < buf->samples.size(); i++) {
        vec.at(i % buf->nchannels).push_back(buf->samples.at(i));
    }

    return vec;
}

// Convert a vector in FFTW3 Halfcomplex format into a vector of
// std::complex<double>, throwing away the 0 Hz (DC) term. The DC term is
// useless for our purposes.
static std::vector< std::complex<double> >
convertToComplex(const std::vector<double> &input)
{
    // skip DC term
    int i = 1;
    int j = input.size() - 1;

    std::vector< std::complex<double> > v;
    while (i < j) {
        const std::complex<double> cd(input.at(i), input.at(j));
        v.push_back(cd);

        i++;
        j--;
    }

    return v;
}

static const int NSCANS = 40;

// This is the FFT "bin" corresponding to the 50Hz sine wave that we expect.
// At a sampling rate of 2000 Hz, 40 scans make up a complete waveform:
// 2000 Hz / 50 Hz -> 40 scans
//
// The formula below expects the sampling rate to be 2000 Hz, and calculates
// the correct FFT bin. The calculation assumes that the 0 Hz (DC) term has
// already been thrown away by convertToComplex().
static const int EXPECTED_BIN = (NSCANS / 40) - 1;

void LLWorkerBoard0Thread::run()
try {
    // Phase RMS tracker
    typedef boost::shared_ptr<RMSTracker> RMSTrackerPtr;
    std::vector<RMSTrackerPtr> phaseLog;
    for (int i = 0; i < NUM_PHASE_CHANS; i++) {
        phaseLog.push_back(RMSTrackerPtr(new RMSTracker(25)));
    }

    // create comedi device and command structure
    ComediDevice comedi(this->device_);
    comedi_cmd cmd;
    std::vector<unsigned int> chans;
    createComediCommand(comedi, cmd, chans);

    // setup external 10MHz reference
    comedi.setupExternalReference(100);

    // create fftw plan
    std::vector<double> fft_out(NSCANS);
    fftw_plan plan = fftw_plan_r2r_1d(fft_out.size(), &fft_out[0], &fft_out[0], FFTW_R2HC, FFTW_PATIENT);

    // print a debugging message showing how many instructions we're using
    {
        double add = 0;
        double mul = 0;
        double fma = 0;
        fftw_flops(plan, &add, &mul, &fma);

        std::ostringstream oss;
        oss << "fftw_flops: add=" << add << " mul=" << mul << " fma=" << fma;
        programLogDebugIfPossible(oss.str());
    }

    // 40 scans are accumulated per buffer.
    // 2000 Hz / 50 Hz == 40 scans == 1 entire sine wave period per buffer
    comedi.startCapture(&cmd, NSCANS);

    // The AutoPthreadQuitAndJoinGroup will be destructed before the
    // ComediDevice object. This will force the ComediDevice::thread() function
    // to exit before the object is destroyed.
    AutoPthreadQuitAndJoinGroup group;
    group.insert(StartPthreadWithRef(ComediDevice::thread, comedi));

    while (true) {
        CPTRACE6("      Worker Board0 thread, waiting for next sample");

        ThreadQuitTestSelf();

        // wait up to 1 seconds for the next sample
        struct timespec ts;
        clock_gettime(CLOCK_REALTIME, &ts);
        ts.tv_sec += 1;
        ComediSamplePtr buf = comedi.waitForNextSample(ts);

        // check to make sure we actually got a sample and not a timeout
        if (buf.get() == NULL) {
            CPTRACE6("      Worker Board0 thread, wait timeout!");
            continue;
        }

        ThreadQuitTestSelf();

        CPTRACE6("      got it!");

        // allocate a new data packet
        LLBoard0DataPtr data(new LLBoard0Data());

        // save data timestamp and raw data
        data->timestamp = Time::computeCurrentFrame();
        data->rawData = buf;

        // save reference lock information (from DIO lines)
        data->refLockInfo = buf->dio;

        // transpose the buffer to put every channel's data in its own vector
        std::vector < std::vector < double > > channelData = convertBufferToChannelData(buf);

        // FFT each channel
        for (size_t chan = 0; chan < channelData.size(); chan++) {
            std::vector<double> &samples = channelData.at(chan);

            // defensively check the length
            if (fft_out.size() != samples.size()) {
                std::ostringstream oss;
                oss << "FFT data size and buffer size do not match!";
                programLogErrorIfPossible(oss.str());
                throw CARMA_ERROR(oss.str());
            }

            // Execute FFT on original input array to avoid data copy.
            // Output is to fft_out, in halfcomplex format
            fftw_execute_r2r(plan, &samples[0], &fft_out[0]);

            // Convert FFTW3 Halfcomplex format into std::complex<double>,
            // ignoring the 0 Hz DC term.
            const std::vector< std::complex<double> > cdv = convertToComplex(fft_out);

            // NOTE: this is nothing more than the FFT result, comprising
            // amplitude and phase. The phase difference from the reference
            // has not been taken into account at all.
            const std::complex<double> c = cdv.at(EXPECTED_BIN);

            // add phase data to RMS tracker
            phaseLog.at(chan)->add(arg(c));

            // add data to outgoing data packet
            if (chan < 3) {
                data->refComplex.push_back(c);
                data->refPhaseRMS.push_back(phaseLog.at(chan)->rmsMean());
            } else {
                data->antComplex.push_back(c);
                data->antPhaseRMS.push_back(phaseLog.at(chan)->rmsMean());
            }
        }

        // publish the data
        {
            ScopedPthreadMutexLock lock(this->mutex_);
            this->data_ = data;
        }
    } // while true
} catch (...) {
    if (CaughtExceptionIsThreadQuitRequestedError()) {
        programLogDebugIfPossible("LLWorkerBoard0Thread: exit requested");
        return;
    }

    std::ostringstream oss;
    oss << "LLWorkerBoard0Thread::run exception: " << getStringForCaught();
    programLogErrorIfPossible(oss.str());
}

LLBoard0DataPtr LLWorkerBoard0Thread::getNewestData() const
{
    ScopedPthreadMutexLock lock(this->mutex_);
    return this->data_;
}

// vim: set expandtab ts=4 sts=4 sw=4:
