#include "carma/util/Program.h"

#include <sstream>
#include <stdexcept>
#include <cmath>
#include <cerrno>
#include <cstring>
#include <ctime>

#include <unistd.h>

#include <log4cpp/NDC.hh>

#include "carma/control/WorkerPool.h"
#include "carma/util/Trace.h"
#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"
#include "carma/util/WorkResult.h"
#include "carma/util/WorkResultSetWaitError.h"
#include "carma/util/WorkRequest.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;
using namespace carma::control;


namespace {


struct ::timespec
convertSecsToTimespec( const double secs )
{
    struct ::timespec result;

    const double wholeSecs = floor( secs );
    const double wholeNanos = ceil( 1.0e+9 * (secs - wholeSecs) );

    result.tv_sec = static_cast< ::time_t >( wholeSecs );
    result.tv_nsec = static_cast< long >( wholeNanos );
    
    return result;
}


void
sleepForDuration( const struct ::timespec & duration )
{
    struct ::timespec rqtp = duration;
    
    while ( true ) {
        struct ::timespec rmtp;

        if ( ::nanosleep( &rqtp, &rmtp ) == 0 )
            break;

        const int savedErrno = errno;

        if ( savedErrno != EINTR ) {
            ostringstream oss;

            oss << "nanosleep failed with errno=" << savedErrno
                << " (" << strerror( savedErrno ) << ")";

            throw CARMA_ERROR( oss.str() );
        }

        rqtp = rmtp;
    }
}


class MyImpl : public WorkRequest::Impl {
    public:
        static WorkRequest make( const string &     requestId,
                                 const WorkResult & result,
                                 double             serviceSleepSecs,
                                 bool               exceptionOutcome );

    private:
        MyImpl( const string &     requestId,
                const WorkResult & result,
                double             serviceSleepSecs,
                bool               exceptionOutcome );

        void serviceImpl( );

        const struct ::timespec serviceSleepDuration_;
        const bool              exceptionOutcome_;
};


MyImpl::MyImpl( const string &     requestId,
                const WorkResult & result,
                const double       serviceSleepSecs,
                const bool         exceptionOutcome ) :
WorkRequest::Impl( requestId, result ),
serviceSleepDuration_( convertSecsToTimespec( ::std::max( serviceSleepSecs,
                                                          0.0 ) ) ),
exceptionOutcome_( exceptionOutcome )
{
}


WorkRequest
MyImpl::make( const string &     requestId,
              const WorkResult & result,
              const double       serviceSleepSecs,
              const bool         exceptionOutcome )
{
    return WorkRequest( new MyImpl( requestId,
                                    result,
                                    serviceSleepSecs,
                                    exceptionOutcome ) );
}


void
MyImpl::serviceImpl( )
{
    if ( (serviceSleepDuration_.tv_sec != 0) ||
         (serviceSleepDuration_.tv_nsec != 0) )
        sleepForDuration( serviceSleepDuration_ );

    if ( exceptionOutcome_ ) {
        CARMA_CPTRACE( Trace::TRACE1, "throwing deliberate exception" );

        throw CARMA_ERROR( "deliberate exception" );
    }
}


}  // namespace < anonymous >


//
// @version 0.2
//
// @usage use it
//
// @description
//   A test binary that unit tests the carma::control::WorkerPool class.
//
// @key individual 25 int
//      Number of requests to queue individually
//
// @key group 25  int
//      Number of requests to queue as a group
//
// @key serviceSleep 0.265 double
//      Number of seconds to sleep servicing each request
//
// @key queueSleep 0.0 double
//      Number of seconds to sleep between queuing requests
//
// @key threads 23 int
//      Number of threads to use in the worker pool
//
// @key timeout 30.0 double
//      Timeout value in seconds to use for the waits with a timeout. Values
//      less than 0.0 will result in 0.0 being used.
//
// @key logWorkReqStats false bool
//      Whether or not to log stats on each work request.
//
// @logger TEST_FACILITY carma.test.control.tWorkerPool
//

int
Program::main( )
try {
    log4cpp::NDC::push( "main thread" );

    const int individualParam = getIntParameter( "individual" );
    const int groupParam = getIntParameter( "group" );
    const double serviceSleepParam = getDoubleParameter( "serviceSleep" );
    const double queueSleepParam = getDoubleParameter( "queueSleep" );
    const int threadsParam = getIntParameter( "threads" );
    const double timeoutParam = getDoubleParameter( "timeout" );
    const bool logWorkReqStats = getBoolParameter( "logWorkReqStats" );

    const ::size_t individual = ((individualParam < 0) ? 0 : individualParam);
    const ::size_t group = ((groupParam < 0) ? 0 : groupParam);
    const double serviceSleepSecs =
        ((serviceSleepParam < 0.0) ? 0.0 : serviceSleepParam);
    const double queueSleepSecs =
        ((queueSleepParam < 0.0) ? 0.0 : queueSleepParam);
    const struct ::timespec queueSleepTimeSpec =
        convertSecsToTimespec( queueSleepSecs );
    const ::size_t threads = ((threadsParam < 1) ? 1 : threadsParam);
    const double timeout = ((timeoutParam < 0.0) ? 0.0 : timeoutParam);

    const unsigned long timeoutMillis =
        static_cast< unsigned long >( floor( timeout * 1000.0 ) );

    {
        CARMA_CPTRACE( Trace::TRACE1,
                       "Constructing worker pool of " << threads <<
                       " threads" );

        WorkerPool pool( "worker pool", threads, logWorkReqStats );

        if ( (individual + group) > 0 ) {
            WorkResultSet wrs( "result set" );

            {
                {
                    ostringstream oss;

                    oss << "Queueing " << individual
                        << " requests individually";

                    programLogInfoIfPossible( oss.str() );
                }

                for ( ::size_t i = 0; i < individual; ++i ) {
                    if ( (i != 0) && (queueSleepSecs != 0.0) )
                        sleepForDuration( queueSleepTimeSpec );
                        
                    string key;

                    {
                        ostringstream oss;

                        oss << "individual request #" << (i + 1);

                        key = oss.str( );
                    }

                    const bool excOutcome = (i > (individual / 2));

                    set< string > keysToRemove;

                    const WorkResult wr = wrs.addKey( key );

                    keysToRemove.insert( key );

                    try {
                        const WorkRequest request =
                            MyImpl::make( key,
                                          wr,
                                          serviceSleepSecs,
                                          excOutcome );

                        set< WorkRequest > requestGroup;

                        requestGroup.insert( request );

                        set< string > temp;

                        pool.queueRequestGroup( requestGroup );

                        keysToRemove.swap( temp );
                    } catch ( ... ) {
                        wrs.removeKeys( keysToRemove );

                        throw;
                    }
                }
            }

            set< string > groupKeysToRemove;

            try {
                {
                    ostringstream oss;

                    oss << "Queueing " << group << " requests as a group";

                    programLogInfoIfPossible( oss.str() );
                }

                if ( (individual > 0) && (queueSleepSecs != 0.0) )
                    sleepForDuration( queueSleepTimeSpec );

                set< WorkRequest > requestGroup;

                for ( ::size_t i = 0; i < group; ++i ) {
                    string key;

                    {
                        ostringstream oss;

                        oss << "group request #" << (i + 1);

                        key = oss.str( );
                    }

                    const bool excOutcome = (i > (group / 2));

                    const WorkResult result = wrs.addKey( key );

                    groupKeysToRemove.insert( key );

                    requestGroup.insert(
                        MyImpl::make( key,
                                      result,
                                      serviceSleepSecs,
                                      excOutcome ) );
                }

                set< string > temp;

                pool.queueRequestGroup( requestGroup );

                groupKeysToRemove.swap( temp );
            } catch ( ... ) {
                wrs.removeKeys( groupKeysToRemove );

                throw;
            }

            {
                programLogInfoIfPossible(
                    "Waiting for all results requiring normal..." );

                bool bad1 = false;
                bool good1 = false;

                try {
                    wrs.waitForAll( timeoutMillis,
                                    true,
                                    WorkResultSet::LATE_DROPPED_POST_STATE );

                    bad1 = true;
                } catch ( const WorkResultSet::WaitError & waitError ) {
                    if ( waitError.hadUnfinishedKeys() != false )
                        throw;

                    if ( waitError.singleUnfinishedKey() != false )
                        throw;

                    if ( waitError.getUnfinishedKeys().empty() != true )
                        throw;

                    if ( waitError.hadAbnormals() != true )
                        throw;

                    if ( waitError.getAbnormals().empty() != false )
                        throw;

                    good1 = true;
                }

                if ( bad1 || (good1 != true) )
                    throw CARMA_ERROR( "waitForAll problem" );

                programLogInfoIfPossible(
                    "Waiting for all results requiring normal produced"
                    " expected exception" );
            }

            {
                programLogInfoIfPossible(
                    "Waiting for all results allowing abnormals..." );

                wrs.waitForAll( timeoutMillis,
                                false,
                                WorkResultSet::LATE_DROPPED_POST_STATE );

                programLogInfoIfPossible( "All results ready" );
            }
        }

        CARMA_CPTRACE( Trace::TRACE1, "Destructing the worker pool" );
    }

    programLogInfoIfPossible( "All tests done" );

    return EXIT_SUCCESS;
} catch ( const BaseException & e ) {
    CARMA_CPTRACE( Trace::TRACE1,
                   "Coming out on a BaseException (" <<
                   e.getLogString( ) << ")" );

    throw;
} catch ( const ::std::exception & e ) {
    CARMA_CPTRACE( Trace::TRACE1,
                   "Coming out on a ::std::exception (" << e.what( ) << ")" );

    throw;
} catch ( ... ) {
    CARMA_CPTRACE( Trace::TRACE1, "Coming out on an unknown exception" );

    throw;
}
