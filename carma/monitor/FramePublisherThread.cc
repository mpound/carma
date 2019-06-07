/**
 * $Id: FramePublisherThread.cc,v 1.14 2012/12/19 00:15:56 abeard Exp $ 
 * 
 * Frame publisher that incorporates a timer thread.  
 * 
 * Author: N. S. Amarnath
 * 
 * Version: $Revision: 1.14 $ * $Date: 2012/12/19 00:15:56 $ 
 *
 * $CarmaCopyright$
 */


#include "carma/monitor/FramePublisherThread.h"

#include <iosfwd>

#include "carma/monitor/FramePublisher.h"
#include "carma/monitor/MonitorPointSet.h"
#include "carma/monitor/MonitorSubsystem.h"
#include "carma/monitor/SubsystemFrameBuffer.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Trace.h"
#include "carma/util/Time.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;

namespace {

const size_t kPublishLogPeriod = 0;

void blockTimers ()
{
    sigset_t        signalSet;

    sigprocmask (SIG_BLOCK, 0, &signalSet);
    sigaddset (&signalSet, SIGALRM);
    sigprocmask (SIG_BLOCK, &signalSet, 0);
}


void unblockTimers ()
{
    sigset_t        signalSet;

    sigemptyset (&signalSet);
    sigaddset (&signalSet, SIGALRM);
    sigprocmask (SIG_UNBLOCK, &signalSet, 0);
}

}  // namespace < anonymous >


FramePublisherThread::FramePublisherThread(
    MonitorSubsystem &  subsystem, 
    const std::string & senderName,
    carma::corba::Client & client )
try :
dispatcher_( new FramePublisher( MONITOR_CHANNEL_NAME, senderName, client ) ),
subsystem_( subsystem ),
publishCount_( 0 ),
nextPublishLogCount_( kPublishLogPeriod )
{
    while ( !subsystem_.monitorPointSet().getBuffer().readNewest() ) {
        // Stall until we get one
    }
    
    programLogInfoIfPossible( senderName +
                              ".FramePublisherThread c'tor completing" );
} catch ( ... ) {
    programLogErrorIfPossible(
        "FramePublisherThread c'tor exiting on an exception: " +
        getStringForCaught() );
}


FramePublisherThread::~FramePublisherThread( )
try {
} catch ( ... ) {
    // Just stifle any exception
}


void 
FramePublisherThread::publishFrame( SubsystemFrameBuffer & buffer )
try {
    double timestamp[3] = {0.0, 0.0, 0.0};
    CPTRACE (::carma::util::Trace::TRACEALL,  "FramePublisherThread::publishFrame - Entered." );

    TransportSubsystemFrame transFrame;

    blockTimers();

    if ( !buffer.isPublished() ) {
       // set the timestamps in the frame
       buffer.setPublishTime();
       timestamp[0] = ::carma::util::Time::MJD();
       buffer.writeToTransport (transFrame);
       timestamp[1] = ::carma::util::Time::MJD();
       dispatcher_->dispatchNotification (transFrame);
       timestamp[2] = ::carma::util::Time::MJD();
    }

    buffer.setPublished();
    unblockTimers();

    CPTRACE ( ::carma::util::Trace::TRACEALL,  "FramePublisherThread::publishFrame - Time to write subsystem frame "
            << static_cast<int>(1000*::carma::util::Time::SECONDS_PER_DAY*(timestamp[1] - timestamp[0]))
            << " msecs\n" 
            << "Time to publish frame = "
            << static_cast<int>(1000*::carma::util::Time::SECONDS_PER_DAY*(timestamp[2] - timestamp[1]))
            << " msecs\n" );
} catch ( ... ) {
    programLogErrorIfPossible(
        "FramePublisherThread::publishFrame exiting on an exception: " +
        getStringForCaught() );

    throw;
}


void
FramePublisherThread::operator()()
try { 
             
    ScopedLogNdc ndc( "FramePublisherThread" );

    while ( true ) {

        boost::this_thread::interruption_point();

        SubsystemFrameBuffer & buffer = 
            subsystem_.getMonitorPointSet().getBuffer();

        buffer.read();

        publishFrame( buffer );

        ++publishCount_;

        if ( (kPublishLogPeriod != 0) && 
             (publishCount_ >= nextPublishLogCount_) ) 
        {
            nextPublishLogCount_ = publishCount_ + kPublishLogPeriod;

            ostringstream oss;

            oss << publishCount_ << " frames published so far";

            programLogInfoIfPossible( oss.str() );
        }

    } // while ( true )
    
    CPTRACE ( ::carma::util::Trace::TRACEALL, 
              "FramePublisherThread::action - About to leave...." );
} catch ( ... ) {
    programLogErrorIfPossible(
        "FramePublisherThread::action exiting on an exception: " +
        getStringForCaught() );

    throw;
}
