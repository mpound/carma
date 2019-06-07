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

#include <carma/linelength/ComediDevice.h>
#include <carma/linelength/LLWorkerBoard1Thread.h>
using namespace carma::linelength;

#include <sstream>

#include <comedilib.h>

#define CPTRACE6(args...) CPTRACE(Trace::TRACE6, ##args)

/*----------------------------------------------------------------------------*/
/* LLWorkerBoard1 Object                                                      */
/*----------------------------------------------------------------------------*/

LLWorkerBoard1Thread::LLWorkerBoard1Thread(const std::string &device)
    : device_(device)
{
    programLogDebugIfPossible("LLWorkerBoard1Thread ctor");
}

LLWorkerBoard1Thread::~LLWorkerBoard1Thread()
{
    programLogDebugIfPossible("LLWorkerBoard1Thread dtor");
}

static void
createComediCommand(ComediDevice &comedi, comedi_cmd &cmd, std::vector<unsigned int> &chans)
{
    const int range = comedi.findRange(-5.0, 5.0);
    const int aref = AREF_GROUND;

    // Create the list of channels for the DAQ card to sample. The hardware is
    // comprised of several DB-25 connectors, each carrying 8 antennas worth of
    // data. Due to wiring issues, the 16th input is unused, and should show up
    // as 'Antenna 24'.
    //
    // It is sampled last so that unpacking the data returned from the DAQ card
    // is trivial.

    // Antenna 1-8 Optical Power
    for (int i = 0; i < 8; i++)
        chans.push_back(CR_PACK(i + 16, range, aref));
    // Antenna 9-15 Optical Power
    for (int i = 0; i < 7; i++)
        chans.push_back(CR_PACK(i + 32, range, aref));
    // Antenna 16-23 Optical Power
    for (int i = 0; i < 8; i++)
        chans.push_back(CR_PACK(i + 48, range, aref));
    // Antenna 24 Optical Power
    chans.push_back(CR_PACK(39, range, aref));

    // Antenna 1-8 AGC
    for (int i = 0; i < 8; i++)
        chans.push_back(CR_PACK(i + 24, range, aref));
    // Antenna 9-15 AGC
    for (int i = 0; i < 7; i++)
        chans.push_back(CR_PACK(i + 40, range, aref));
    // Antenna 16-23 AGC
    for (int i = 0; i < 8; i++)
        chans.push_back(CR_PACK(i + 56, range, aref));
    // Antenna 24 AGC
    chans.push_back(CR_PACK(47, range, aref));

    // Reference 1-3 AGC
    for (int i = 0; i < 3; i++)
        chans.push_back(CR_PACK(i + 0, range, aref));
    // Reference 1-3 RF Power
    for (int i = 0; i < 3; i++)
        chans.push_back(CR_PACK(i + 8, range, aref));

    // Comedi Library Command Structure
    cmd.subdev          = 0;                        // analog input
    cmd.flags           = TRIG_ROUND_NEAREST | TRIG_WAKE_EOS; // wake at end of scan

    cmd.start_src       = TRIG_NOW;                 // default
    cmd.start_arg       = 0;                        // default

    cmd.scan_begin_src  = TRIG_TIMER;               // default
    cmd.scan_begin_arg  = 50000000;                 // 50 ms per scan

    cmd.convert_src     = TRIG_TIMER;               // default
    cmd.convert_arg     = 50000000 / chans.size();  // convert at evenly spaced intervals

    cmd.scan_end_src    = TRIG_COUNT;               // default
    cmd.scan_end_arg    = chans.size();             // 54 channels

    cmd.stop_src        = TRIG_NONE;                // run indefinitely
    cmd.stop_arg        = 0;                        // run indefinitely

    cmd.chanlist        = &chans.at(0);             // 54 elements of CR_PACK()
    cmd.chanlist_len    = chans.size();             // 54 channels
}

void LLWorkerBoard1Thread::run()
try {
    // create comedi device and command structure
    ComediDevice comedi(this->device_);
    comedi_cmd cmd;
    std::vector<unsigned int> chans;
    createComediCommand(comedi, cmd, chans);

    // setup external 10MHz reference
    comedi.setupExternalReference(100);

    // 10 scans are accumulated per buffer
    comedi.startCapture(&cmd, 10);

    // The AutoPthreadQuitAndJoinGroup will be destructed before the
    // ComediDevice object. This will force the ComediDevice::thread() function
    // to exit before the object is destroyed.
    AutoPthreadQuitAndJoinGroup group;
    group.insert(StartPthreadWithRef(ComediDevice::thread, comedi));

    while (true) {
        CPTRACE6("      Worker Board1 thread, waiting for next sample");

        ThreadQuitTestSelf();

        // wait up to 1 seconds for the next sample
        struct timespec ts;
        clock_gettime(CLOCK_REALTIME, &ts);
        ts.tv_sec += 1;
        ComediSamplePtr buf = comedi.waitForNextSample(ts);

        // check to make sure we actually got a sample and not a timeout
        if (buf.get() == NULL) {
            CPTRACE6("      Worker Board1 thread, wait timeout!");
            continue;
        }

        ThreadQuitTestSelf();

        CPTRACE6("      got it!");

        const std::vector<double> avgData = buf->calculateAverage();
        LLBoard1DataPtr data(new LLBoard1Data());

        // save data timestamp and raw data
        data->timestamp = Time::computeCurrentFrame();
        data->rawData = buf;

        // Antenna 1-24 Optical Power in mW
        for (int i = 0; i < 24; i++)
            data->antopt.push_back(avgData.at(i) / 0.9);

        // Antenna 1-24 AGC in volts
        for (int i = 0; i < 24; i++)
            data->antagc.push_back(avgData.at(i + 24));

        // Reference 1-3 AGC in volts
        for (int i = 0; i < 3; i++)
            data->refagc.push_back(avgData.at(i + 48));

        // Reference 1-3 RF Power in dBm
        for (int i = 0; i < 3; i++)
            data->refpower.push_back(50 * avgData.at(i + 51) - 31);

        // publish the data packet
        ScopedPthreadMutexLock lock(this->mutex_);
        this->buffer_ = data;

    } // while true
} catch (...) {
    if (CaughtExceptionIsThreadQuitRequestedError()) {
        programLogInfoIfPossible("LLWorkerBoard1Thread: exit requested");
        return;
    }

    std::ostringstream oss;
    oss << "LLWorkerBoard1Thread::run exception: " << getStringForCaught();
    programLogErrorIfPossible(oss.str());
}

LLBoard1DataPtr LLWorkerBoard1Thread::getNewestData() const
{
    ScopedPthreadMutexLock lock(this->mutex_);
    return this->buffer_;
}

// vim: set expandtab ts=4 sts=4 sw=4:
