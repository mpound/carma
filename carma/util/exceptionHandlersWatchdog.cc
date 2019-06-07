#include "carma/util/exceptionHandlersWatchdog.h"

#include <pthread.h>
#include <signal.h>

#include "carma/util/ExceptionUtils.h"
#include "carma/util/StartPthread.h"
#include "carma/util/PeriodicTimer.h"
#include "carma/util/ThreadQuit.h"
#include "carma/util/programExtras.h"
#include "carma/util/Trace.h"
#include "carma/util/Logger.h"

using namespace ::std;
using namespace carma::util;


namespace {


struct Args {
    int secondsBetweenChecks;
    int checksBetweenLogMessages;
};


void
threadEntry( const Args & args ) {

    sigset_t allSignals;
    sigfillset(&allSignals);
    pthread_sigmask(SIG_BLOCK, &allSignals, NULL);

    // human-readable name for debugging
#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 12)
    pthread_setname_np(pthread_self(), "ehwatchdog");
#endif

    struct ::timespec period;
    
    period.tv_sec = args.secondsBetweenChecks;
    period.tv_nsec = 0;
    
    PeriodicTimer clock( period );
    
    {
        struct ::timeval now;

        gettimeofday( &now, 0 );
        
        clock.ResetNextFireAbsoluteTime( now );
    }
    
    int checksSinceLastLogMessage = 0;
    int terminateChangesSinceLastLogMessage = 0;
    int unexpectedChangesSinceLastLogMessage = 0;
    
    while ( true ) {
        clock.WaitForNextFireTime( );
        
        Trace * traceObject = getProgramTraceObjectIfAvailable( );
            
        if ( traceObject != 0 )
            traceObject->write( Trace::TRACE7, "Checking handlers" );

        const terminate_handler prevTerminateHandler =
            set_terminate( terminateHandler );
            
        const unexpected_handler prevUnexpectedHandler =
            set_unexpected( unexpectedHandler );

        ++checksSinceLastLogMessage;

        if ( prevTerminateHandler != terminateHandler ) {
            ++terminateChangesSinceLastLogMessage;

            traceObject->write( Trace::TRACE1, "terminate() handler changed" );
        } else
            traceObject->write( Trace::TRACE7, "terminate() handler is fine" );

        if ( prevUnexpectedHandler != unexpectedHandler ) {
            ++unexpectedChangesSinceLastLogMessage;

            traceObject->write( Trace::TRACE1, "unexpected() handler changed" );
        } else
            traceObject->write( Trace::TRACE7, "unexpected() handler is fine" );

        if ( checksSinceLastLogMessage < args.checksBetweenLogMessages )
            continue;

        if ( (terminateChangesSinceLastLogMessage == 0) &&
             (unexpectedChangesSinceLastLogMessage == 0) )
            continue;
            
        log4cpp::Category * logger = getProgramLoggerIfAvailable( );
        
        if ( logger == 0 )
            continue;
            
        if ( terminateChangesSinceLastLogMessage != 0 ) {
            *logger << log4cpp::Priority::ERROR
                    << "terminate() handler changed "
                    << terminateChangesSinceLastLogMessage
                    << " times in the last "
                    << checksSinceLastLogMessage
                    << " checks (one check every ~"
                    << args.secondsBetweenChecks
                    << " seconds)";
        }
                
        if ( unexpectedChangesSinceLastLogMessage != 0 ) {
            *logger << log4cpp::Priority::ERROR
                    << "unexpected() handler changed "
                    << unexpectedChangesSinceLastLogMessage
                    << " times in the last "
                    << checksSinceLastLogMessage
                    << " checks (one check every ~"
                    << args.secondsBetweenChecks
                    << " seconds)";
        }
        
        checksSinceLastLogMessage = 0;
        terminateChangesSinceLastLogMessage = 0;
        unexpectedChangesSinceLastLogMessage = 0;
    }
}


bool gStarted = false;
bool gRunning = false;
::pthread_t gThread;

}  // namespace < anonymous >


void
carma::util::startExceptionHandlersWatchdog( ) {
    set_terminate( terminateHandler );
    set_unexpected( unexpectedHandler );
    
    {
        if ( gStarted == false ) {
            gStarted = true;
            
            Args args;
            
            args.secondsBetweenChecks = 5;
            args.checksBetweenLogMessages = 10;
            
            gThread = StartPthreadWithCopy( threadEntry,
                                            args,
                                            "Exception Handlers Watchdog" );

            gRunning = true;
        }
    }
}


void
carma::util::stopExceptionHandlersWatchdog( ) {
    if ( gRunning ) {
        gRunning = false;
        
        RequestThreadQuit( gThread );
        
        void * threadResult = 0;
        
        ::pthread_join( gThread, &threadResult );
    }
}
