/**
 * $Id: MonitorSystemIPQwriter.cc,v 1.23 2014/04/23 23:39:43 scott Exp $ 
 * 
 * Frame timer that incorporates a timer thread.  
 * 
 * Author: N. S. Amarnath
 * 
 * Version: $Revision: 1.23 $ * $Date: 2014/04/23 23:39:43 $ 
 */


#include "carma/monitor/MonitorSystemIPQwriter.h"

#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"
#include "carma/util/ErrorException.h"
#include "carma/monitor/FrameCollatorThread.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/SystemFrameBuffer.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;

namespace {

    const int MILLISECONDS_PER_DAY = 86400000;

    int diffInMS( const double mjd1, const double mjd2 )
    {
        return static_cast< int >( MILLISECONDS_PER_DAY * fabs( mjd1 - mjd2 ) );
    }

    const string monitorSystemIPQwriterThreadName( 
        "carma.monitor.monitorSystemIPQwriterThread" );

} // namespace < unnamed >



// public

MonitorSystemIPQwriter::MonitorSystemIPQwriter(
    const double         delayInS, 
    FrameCollatorThread& collatorThread,
    const int            clearDelayInFrames) :
FrameIPQwriter( monitorSystemIPQwriterThreadName,
                delayInS ),
collatorThread_( collatorThread ),
delayInS_( delayInS ),
clearDelayInFrames_(clearDelayInFrames)
{
    const long delayFrames = static_cast< long >( delayInS_ / 0.5 );
    const double nextFireTime = resetNextFireTime( delayFrames );
    collatorThread_.setFirstFireTime( nextFireTime );
}


MonitorSystemIPQwriter::~MonitorSystemIPQwriter( )
{
}


// protected



// private


void
MonitorSystemIPQwriter::writeBuffer()
try {
    const double startMJD = util::Time::MJD();

    collatorThread_.writeMonitorSystemToIPQ(
            getNextFireTime( ), delayInS_, clearDelayInFrames_);

    const double finishMJD = util::Time::MJD();

    const double elapsedS = ( finishMJD - startMJD ) * Time::SECONDS_PER_DAY;
    if ( elapsedS > 0.5 ) {
        ostringstream err;
        err << "MonitorSystemIPQwriter::writeBuffer() - Elapsed buffer write "
            << "time is " << elapsedS << "!";
        programLogErrorIfPossible( err.str( ) );
    }

} catch ( ... ) {
    logCaughtAsError( );
    throw; // Rethrow
}
