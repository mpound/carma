/**
 * $Id: FrameCollatorThread.cc,v 1.19 2014/04/23 23:39:42 scott Exp $ 
 * 
 * Frame collator that incorporates a subscriber.  
 * 
 * Author: N. S. Amarnath
 * 
 * Version: $Revision: 1.19 $ * $Date: 2014/04/23 23:39:42 $ 
 *
 * $CarmaCopyright$
 */

#include "carma/monitor/FrameCollatorThread.h"

#include "carma/corba/Server.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"
#include "carma/monitor/FrameSubscriber.h"
#include "carma/monitor/FramePublisher.h"
#include "carma/monitor/monitorframe.h"

#include <boost/thread.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <sstream>

using namespace boost;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;
using namespace std;


namespace {

const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE3;


string
makeThreadName( const string & receiverName )
{
    return receiverName + ".FrameCollatorThread";
}

}  // namespace < anonymous >



FrameCollatorThread::FrameCollatorThread(
    const string&   receiverName,
    const double    delayInS,
    const bool      rawMode,
    corba::Server&  server) :
subscriber_(new FrameSubscriber(delayInS, rawMode)),
server_( server )
{
    CARMA_CPTRACE(TRACE_CTOR_DTOR, "FrameCollatorThread::FrameCollatorThread()" );

    server_.addNotificationServantFunctor< 
            FrameSubscriber, TransportSubsystemFrame >( *subscriber_,
                                                        MONITOR_CHANNEL_NAME,
                                                        receiverName );
}


FrameCollatorThread::~FrameCollatorThread( )
try {
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "FrameCollatorThread::~FrameCollatorThread()" );
    const ScopedLogNdc ndc( "FrameCollatorThread::~FrameCollatorThread" );
    
    if ( !server_.terminated() )
        server_.stop();

} catch ( ... ) {
    try {
        programLogErrorIfPossible(
            "Stifling exception in "
            "FrameCollatorThread::~FrameCollatorThread - " +
            getStringForCaught() );
    } catch ( ... ) {
        // Just stifle any exceptions
    }

    // Just stifle any exceptions
    return;
}

void
FrameCollatorThread::writeMonitorSystemToIPQ(const double nextFireTimeInMJD,
                                             const double delayInSeconds,
                                             const int    clearDelayInFrames) 
{
    subscriber_->writeMonitorSystemToIPQ(nextFireTimeInMJD, 
                                         delayInSeconds, clearDelayInFrames); 
}

void
FrameCollatorThread::setFirstFireTime( const double firstFireTimeMJD )
{
    subscriber_->setFirstFireTime( firstFireTimeMJD );
}

void
FrameCollatorThread::operator( ) ( )
{
    const ScopedLogNdc ndc( "FrameCollatorThread::operator( ) ( ) - Functor" );
    
    try {
        // Use polling version of run to expedite shutdown - using run directly
        // can induce several seconds of additional wait after shutdown.
        while ( !server_.terminated() ) {
            server_.work();
            boost::this_thread::sleep( boost::posix_time::milliseconds( 1 ) );
        }

    } catch ( ... ) {
        const string caught( getStringForCaught() );
        programLogErrorIfPossible("FrameCollatorThread::operator()() - Caught "
            "exception stifling: " + caught );
    }
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "FrameCollatorThread::operator() - exiting cleanly." );
}
