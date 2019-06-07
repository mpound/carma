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
#include <carma/linelength/FiberIFThread.h>
using namespace carma::linelength;

#include <boost/foreach.hpp>
#include <comedilib.h>

#define CPTRACE6(args...) CPTRACE(Trace::TRACE6, ##args)

/*----------------------------------------------------------------------------*/
/* FiberIF Object                                                             */
/*----------------------------------------------------------------------------*/

FiberIFThread::FiberIFThread(const std::string &device)
    : device_(device)
    , mutex_()
    , buffer_(16)
{
    CPTRACE6("FiberIFThread ctor");
}

FiberIFThread::~FiberIFThread()
{
    CPTRACE6("FiberIFThread dtor");
}

static void
createComediCommand(ComediDevice &comedi, comedi_cmd &cmd, std::vector<unsigned int> &chans)
{
    // TODO FIXME: [0V, 2V] should be sufficient
    const int range = comedi.findRange(-5.0, 5.0);
    const int aref = AREF_GROUND;

    // Create the list of channels for the DAQ card to sample. The hardware is
    // comprised of several DB-25 connectors, each carrying 8 antennas worth of
    // data. Due to wiring issues, the 16th input is unused, and should show up
    // as 'Antenna 24'.
    //
    // It is sampled last so that unpacking the data returned from the DAQ card
    // is trivial.

    // Polarization 1 (L)
    for (int i = 0; i < 8; i++)
        chans.push_back(CR_PACK(i + 16, range, aref));
    for (int i = 0; i < 7; i++)
        chans.push_back(CR_PACK(i + 24, range, aref));
    for (int i = 0; i < 8; i++)
        chans.push_back(CR_PACK(i + 32, range, aref));
    chans.push_back(CR_PACK(31, range, aref));

    // Polarization 2 (R)
    for (int i = 0; i < 8; i++)
        chans.push_back(CR_PACK(i + 40, range, aref));
    for (int i = 0; i < 7; i++)
        chans.push_back(CR_PACK(i + 48, range, aref));
    for (int i = 0; i < 8; i++)
        chans.push_back(CR_PACK(i + 56, range, aref));
    chans.push_back(CR_PACK(55, range, aref));

    // Comedi Library Command Structure
    cmd.subdev          = 0;                        // analog input
    cmd.flags           = TRIG_ROUND_NEAREST | TRIG_WAKE_EOS; // wake at end of scan

    cmd.start_src       = TRIG_NOW;                 // default
    cmd.start_arg       = 0;                        // default

    cmd.scan_begin_src  = TRIG_TIMER;               // default
    cmd.scan_begin_arg  = 100000000;                // 100 ms per scan

    cmd.convert_src     = TRIG_TIMER;               // default
    cmd.convert_arg     = 100000000 / chans.size(); // convert at evenly spaced intervals

    cmd.scan_end_src    = TRIG_COUNT;               // default
    cmd.scan_end_arg    = chans.size();             // 48 channels

    cmd.stop_src        = TRIG_NONE;                // run indefinitely
    cmd.stop_arg        = 0;                        // run indefinitely

    cmd.chanlist        = &chans.at(0);             // 48 elements of CR_PACK()
    cmd.chanlist_len    = chans.size();             // 48 channels
}

void FiberIFThread::run()
try {
    // create comedi device and command structure
    ComediDevice comedi(this->device_);
    comedi_cmd cmd;
    std::vector<unsigned int> chans;
    createComediCommand(comedi, cmd, chans);

    // 1 scan is accumulated per buffer (no averaging)
    comedi.startCapture(&cmd, 1);

    // The AutoPthreadQuitAndJoinGroup will be destructed before the
    // ComediDevice object. This will force the ComediDevice::thread() function
    // to exit before the object is destroyed.
    AutoPthreadQuitAndJoinGroup group;
    group.insert(StartPthreadWithRef(ComediDevice::thread, comedi));

    while (true) {
        CPTRACE6("FiberIF: waiting for next sample");

        ThreadQuitTestSelf();

        // wait up to 1 seconds for the next sample
        struct timespec ts;
        clock_gettime(CLOCK_REALTIME, &ts);
        ts.tv_sec += 1;
        ComediSamplePtr buf = comedi.waitForNextSample(ts);

        // check to make sure we actually got a sample and not a timeout
        if (buf.get() == NULL) {
            CPTRACE6("FiberIF: wait timeout");
            continue;
        }

        ThreadQuitTestSelf();

        CPTRACE6("FiberIF: got it!");

        const std::vector<double> avgData = buf->calculateAverage();
        FiberIFDataPtr data(new FiberIFData());

        // Pack averaged data into buffer: 1.8 Volts per mW
        for (int i = 0; i < 24; i++)
            data->pol1.push_back(avgData.at(i) / 1.8);

        for (int i = 0; i < 24; i++)
            data->pol2.push_back(avgData.at(i + 24) / 1.8);

        // add the data packet to the buffer
        ScopedPthreadMutexLock lock(this->mutex_);
        this->buffer_.push_back(data);

    } // while true
} catch (...) {
    if (CaughtExceptionIsThreadQuitRequestedError()) {
        programLogInfoIfPossible("FiberIFThread: exit requested");
        return;
    }

    std::ostringstream oss;
    oss << "FiberIFThread::run exception: " << getStringForCaught();
    programLogErrorIfPossible(oss.str());
}

bool FiberIFThread::empty() const
{
    ScopedPthreadMutexLock lock(this->mutex_);
    return this->buffer_.empty();
}

size_t FiberIFThread::size() const
{
    ScopedPthreadMutexLock lock(this->mutex_);
    return this->buffer_.size();
}

FiberIFDataPtr FiberIFThread::getData()
{
    ScopedPthreadMutexLock lock(this->mutex_);
    FiberIFDataPtr ret;

    if (!this->buffer_.empty()) {
        ret = this->buffer_.front();
        this->buffer_.pop_front();
    }

    return ret;
}

// vim: set expandtab ts=4 sts=4 sw=4:
