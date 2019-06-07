
/**@file
 * Class definition for Lobe Rotator Master for Carma.
 * Written with heavy kibitzing from wbdcMaster.
 *
 * @author Colby Kraybill, Steve Scott
 * $Id: LoberotatorMaster.cc,v 1.41 2014/06/24 21:46:00 scott Exp $
 */

#include <iomanip>

// Carma includes
#include "carma/loberotator/LoberotatorMaster.h"
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/switchyard/Switchyard.h"
#include "carma/util/FrameAlignedTimer.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/StartPthread.h"
#include "carma/util/ThreadQuit.h"
#include "carma/util/Trace.h"
#include "carma/util/UserException.h"

using namespace std;
using namespace log4cpp;
using namespace carma;
using namespace carma::canbus;
using namespace carma::util;
using namespace carma::loberotator;
using namespace carma::monitor;

///////////////////////////
// START OF CONSTRUCTORS //
///////////////////////////

// *********************************************************************
// For emulation
LoberotatorMaster::LoberotatorMaster(int holdoff, 
                                     const double autoWriteDelayInS,
                                     const double syautoWriteDelayInS) :
        holdoff_(holdoff-FIXED_HOLDOFF),
        signalPathMon_(),
        loSwitchyard_( carma::switchyard::LOSWITCHYARD_NODE,
                       *this,
                       signalPathMon_.lOSwitchyard().state(),
                       signalPathMon_.lOSwitchyard().switchyard(),
                       signalPathMon_.lOSwitchyard().xac() ),
        llSwitchyard_( carma::switchyard::LLSWITCHYARD_NODE,
                       *this,
                       signalPathMon_.lLSwitchyard().state(),
                       signalPathMon_.lLSwitchyard().switchyard(),
                       signalPathMon_.lLSwitchyard().xac() ),
        emulate_(true)
{
    initialize(autoWriteDelayInS, syautoWriteDelayInS);
}


// *********************************************************************
LoberotatorMaster::LoberotatorMaster( int modulBusNo, int slotNo,
                                      int holdoff, bool simulate,
                                     const double autoWriteDelayInS,
                                     const double syautoWriteDelayInS) :
     Master(modulBusNo, slotNo, simulate, true),
     holdoff_(holdoff-FIXED_HOLDOFF),
     signalPathMon_(),
     loSwitchyard_( carma::switchyard::LOSWITCHYARD_NODE,
                   *this,
                   signalPathMon_.lOSwitchyard().state(),
                   signalPathMon_.lOSwitchyard().switchyard(),
                   signalPathMon_.lOSwitchyard().xac() ),
     llSwitchyard_( carma::switchyard::LLSWITCHYARD_NODE,
                   *this,
                   signalPathMon_.lLSwitchyard().state(),
                   signalPathMon_.lLSwitchyard().switchyard(),
                   signalPathMon_.lLSwitchyard().xac() ),
     emulate_(false)
{
    initialize(autoWriteDelayInS, syautoWriteDelayInS);
}


/////////////////////////
// END OF CONSTRUCTORS //
/////////////////////////

// Destroy....  DESTROY!!!
// *********************************************************************
LoberotatorMaster::~LoberotatorMaster()
{
    void *result;
    int status;

    try {

        // Kill run thread
        status = pthread_cancel( runThreadId_ );
        if ( status != 0 )
            throw CARMA_EXCEPTION( carma::canbus::PthreadFailException,
			       "LoberotatorMaster::~LoberotatorMaster()"
			       " - Error destroying run thread. "
			       + (const string)strerror( status ) );

        // Block on the run thread to quit.
        status = pthread_join(runThreadId_, &result);
        if ( status != 0 )
            throw CARMA_EXCEPTION( carma::canbus::PthreadFailException,
				 "LoberotatorMaster::~LoberotatorMaster()"
				 " - Error joining on run thread. "
				 + (const string)strerror(status) );


        // Check return status and make sure it's good.
        if ( result != PTHREAD_CANCELED )
            throw CARMA_EXCEPTION( carma::canbus::PthreadFailException,
				 "Master::~Master() - Read thread "
				 "returned with invalid value." );

        // Stop update thread
        RequestThreadQuit(updateThread_);

        // Remove all devices from the Master::device_ map 
        removeDevices();

        for (int lr=0; lr < N_CHAN; lr++) {
            delete loberotator_[lr];
        }

        delete globalLrb_;

    } catch ( carma::util::ErrorException &ex ) {
        throw CARMA_EXCEPTION(UserException, ex.what());
    }

}

Loberotator &
LoberotatorMaster::getGlobalLoberotator( )
{
    return *globalLrb_;
}

switchyard::Switchyard &
LoberotatorMaster::getLoSwitchyard( )
{
    return loSwitchyard_;
}

switchyard::Switchyard &
LoberotatorMaster::getLlSwitchyard( )
{
    return llSwitchyard_;
}


// **********************************************************************
void LoberotatorMaster::initialize(const double autoWriteDelayInS,
                                   const double syautoWriteDelayInS)
{
    int status;
    done_ = false;

    // Create a common shared monitor system
    mon_ = new LoberotatorSubsystem();
    // Start autowriter
    mon_->startAutoWriter( autoWriteDelayInS );
    signalPathMon_.startAutoWriter(syautoWriteDelayInS );

    hostname_ = Program::getHostname(true);

    // Create and store refs to the Loberotator objects
    for (int lr=0; lr < N_CHAN; lr++) {
        nodeType n = static_cast<nodeType>(lr+1);
        insert(new carma::loberotator::Loberotator(n, this, mon_));
    }

    // Define Loberotator CAN network...
    addDevices();

    // Start new thread to run the master in
    status = pthread_create( &runThreadId_, NULL, runThreadEntry,
        (void *)this );

    if ( status != 0 )
        throw CARMA_EXCEPTION( carma::canbus::PthreadFailException,
           "LoberotatorMaster::initialize() "
           "- Unable to start run thread. "
           + (const string)strerror( status ) );

    // Initialize the done mutex.
    pthread_mutex_init( &doneMutex_, NULL );

    // Start the update thread...
    try {
        updateThread_ =
            StartPthreadWithRef(LoberotatorMaster::updateThreadStaticWrapper,
                 *this);
    }
    catch (std::runtime_error& e) {
        ostringstream o;
        o << "Caught exception when starting LoberotatorMaster update thread"
          << "\n" << e.what();
        throw CARMA_ERROR(o);
    }
}


// **********************************************************************
void LoberotatorMaster::updateThreadStaticWrapper(LoberotatorMaster& lrm)
{
    lrm.updateThread();
}

// ***************************************************************************
void LoberotatorMaster::updateThread()
{
    static int count = 0;
    static bool firstTime = true;
    static int lastFrameCount = Time::computeCurrentFrame();
    const float traceInterval = 0.5; // Seconds
    const int   traceIntervalCounts = static_cast<int>(round(2*traceInterval));
    double dummy;
    FrameAlignedTimer timer(holdoff_*1000*1000, 1);
    while (true) {
        timer.ResetNextFireTimeAndWait();
        double mjd = Time::MJD();
        double secs = Time::SECONDS_PER_DAY*mjd;
        int    frameDelay =
            static_cast<int>(500*modf(2*secs, &dummy)); // in msec
        updatePhaseAndRate();
        double finSecs        = Time::SECONDS_PER_DAY*Time::MJD();
        double updateTime     = 1000*(finSecs - secs); // In milliseconds
        int currentFrameCount = Time::computeFrame(mjd);
        int framesSkipped     = currentFrameCount - lastFrameCount -1;
        lastFrameCount        = currentFrameCount;
        if (!firstTime && (framesSkipped != 0)) {
            ostringstream log;
            log << "Loberotator frame skip: count="
                << count
                << " skippedFrames=" << framesSkipped
                << " delay=" << frameDelay
                << "msec upd="
                << setiosflags(ios::fixed) << setprecision(1) << updateTime
                << "msec";
            programLogCriticalIfPossible( log.str() );
        }
        if (!firstTime && (count++%traceIntervalCounts == 0)) {
            if(false)CARMA_CPTRACE(Trace::TRACE2,
                "LRmaster::updateThread: count=" << count
                << " skippedFrames=" << framesSkipped
                << " delay=" << frameDelay
                << "msec upd="
                << setiosflags(ios::fixed) << setprecision(1) << updateTime
                << "msec");
            // Machine readable performance data
            CARMA_CPTRACE(Trace::TRACE2,
                "LRmaster::updateThread: performanceData "
                << Time::getTimeString(mjd, 3)
                << setw(3) << framesSkipped
                << setw(5) << frameDelay
                << setw(6)
                << setiosflags(ios::fixed) << setprecision(1) << updateTime
                << " ");
        }
        firstTime = false;
    }
}

// ***************************************************************************
// Timing matters here. The XAC msg buffer is only one deep per address,
// so we must separate the mul/div and phase/rate for a channel by
// controlling the order in which they go into the output buffer
// (which is 400 deep in hardware).
void LoberotatorMaster::updatePhaseAndRate() const
{
    for (int c=0; c < N_CHAN; c++) {
        loberotator(c).updatePhaseAndRate();  // Calculations only
        loberotator(c).sendPhaseAndRate();
    }
    for (int c=0; c < N_CHAN; c++) {
        loberotator(c).sendMulDiv();
    }
}

 // ***************************************************************************
void LoberotatorMaster::addDevices( )
{

     for (int lr=0; lr < N_CHAN; lr++) {
        // While we send commands to all the loberotator channels,
        // monitoring info is only sent to the boards,
        // with ids=1,5,9,13...
        // So we only register these, so that we only get sim messages
        // from these node ids.
        if (lr%4 == 0) Master::addDevice(&loberotator(lr));
    }

    globalLrb_ = new carma::loberotator::Loberotator(0, this, mon_);

    globalLrb_->setBusId(ALL_BUSSES);

    Master::addDevice( &llSwitchyard_ );
    Master::addDevice( &loSwitchyard_ );

}

// *************************************************************************
void LoberotatorMaster::removeDevices ()
{
    apiType api;
    Device *dev;

    // ADB - Shouldn't we loop through all channels here or atleast boards?
    api = globalLrb_->getApi();
    dev = getDevice( api, 1 );
    Master::removeDevice( api, 1 );

    Master::removeDevice( switchyard::Switchyard::getApiId(),
                          switchyard::LOSWITCHYARD_NODE );
    Master::removeDevice( switchyard::Switchyard::getApiId(),
                          switchyard::LLSWITCHYARD_NODE );
    delete dev;
}


// *************************************************************************
bool LoberotatorMaster::isDone()
{
    bool tmp;

    pthread_mutex_lock( &doneMutex_ );
    tmp = done_;
    pthread_mutex_unlock( &doneMutex_ );

    return tmp;
}


// *************************************************************************
void LoberotatorMaster::quit()
{
    pthread_mutex_lock(&doneMutex_);
    done_ = true;
    pthread_mutex_unlock(&doneMutex_);
}

// *************************************************************************
void* LoberotatorMaster::runThreadEntry(void *arg)
{
    LoberotatorMaster *This = static_cast<LoberotatorMaster *>(arg);
    This->run();
    return 0;
}

// *************************************************************************
void LoberotatorMaster::run()
{
    Master::run();
}

// *************************************************************************
void LoberotatorMaster::updateStatus()
{

   map<canbus::busIdType, canbus::busStatusType> busStatus = getBusStatus();
   map<canbus::busIdType, canbus::busStatusType>::iterator busIter;
   unsigned int busIndex;

   if ( busStatus.size() > 1 )
       throw CARMA_EXCEPTION(carma::util::ErrorException,
            "LoberotatorMaster::updateStatus()"
            " - Size of busStatus map exceeds assumed max size of 1 bus.");

   busIndex = 0;
   busIter = busStatus.begin();
   carma::monitor::Bus &  cb = mon_->can().bus();
   Host& ch = mon_->can().host();

   while ( busIter != busStatus.end() ) {
       cb.busId().setValue(busIter->first);
       cb.halfSecRxMsgRate().setValue(busIter->second.rxMsgRate);
       cb.halfSecTxMsgRate().setValue(busIter->second.txMsgRate);
       cb.avgRxMsgRate().setValue(busIter->second.oneMinRxMsgRate);
       cb.avgTxMsgRate().setValue(busIter->second.oneMinTxMsgRate);
       cb.nRxErrors().setValue(busIter->second.rxErrors);
       cb.nTxErrors().setValue(busIter->second.txErrors);
       cb.nLostFastMsgs().setValue(busIter->second.fastMsgsLost);
       cb.nLostSlowMsgs().setValue(busIter->second.slowMsgsLost);
       cb.busState().setValue(static_cast<Bus::BusStateMonitorPointEnum::BUSSTATE>
           (busIter->second.state));
       busIter++;
       busIndex++;
   }

   ch.nActiveNodes().setValue(getOnlineNodeCount() );
   ch.nOfflineNodes().setValue( getOfflineNodeCount() );
   ch.nLatePackets().setValue( getLatePacketCount() );
   ch.nDonglelessPackets().setValue(getDonglelessPacketCount());
   ch.nUnknownPackets().setValue( getUnknownPacketCount() );
   ch.hostname().setValue( hostname_ );
   mon_->timestamp().setValue(Time::MJD());
}

