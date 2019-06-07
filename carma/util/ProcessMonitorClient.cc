#include "carma/util/ProcessMonitorClient.h"

#include "carma/corba/Client.h"
#include "carma/util/AutoPthreadQuitAndJoinGroup.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/FrameAlignedTimer.h"
#include "carma/util/Orb.h"
#include "carma/util/ProcessMonitor.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/StartPthread.h"
#include "carma/util/ThreadQuit.h"
#include "carma/util/Trace.h"

#include <string>

using namespace carma;
using namespace carma::util;
using namespace std;

// ADB Unfinished business.  This class uses three separate paradigms for corba
// bullshit in order to work across the board.  The plan is to eventually use
// one and simultaneously dice about 25k lines of spaghetti from carma.  Until
// that happens COMPLETELY this needs to use all three paradigms.  In the 
// meantime you should think twice before modifying this class as you are 
// likely to fix something in one paradigm only to break it for apps using 
// others.

namespace {

// If we can't contact the name server or resolve the proc mon, retry every...
const int CONNECTION_RETRY_SECONDS = 5;
const int REPORT_EVERY_N_FAILURES = 300 / CONNECTION_RETRY_SECONDS; 
const long UPDATE_PERIOD_IN_FRAMES = 10;

const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE1;
const Trace::TraceLevel TRACE_THREADING = Trace::TRACE1;

string
parseORBServerId( )
{
    const int argc = Program::getExtraArgc();
    char ** argv = Program::getExtraArgv();

    string orbServerId = "";
    for ( int arg = 0; arg < argc; ++arg )
    {
        if ( string( argv[arg] ) == "-ORBServerId" &&
                arg + 1 < argc ) {
            orbServerId = string( argv[arg + 1 ] );
            break;
        }
    }

    return orbServerId;
}

} // namespace < unnamed >


class ProcessMonitorClient::Private {
public:

    Private( );
    Private( Orb * orb );

    struct ThreadArgs 
    {
        bool useOrb;
        util::Orb * orb;
        string serverId;
    }; // struct Private::ThreadArgs

    class PMCThreadQuitRequestHandler : public ThreadQuitRequestHandler {
    public:
        
        PMCThreadQuitRequestHandler( FrameAlignedTimer & timer );
        
        void HandleQuitRequest( ::pthread_t thread );
    private:
        FrameAlignedTimer & timer_;
    };

    static void thread( const ThreadArgs & args );

private:

    ::pthread_t threadId_;
    FrameAlignedTimer timer; 
    AutoPthreadQuitAndJoinGroup threadQuitOnDestruction;

}; // ProcessMonitorClient::Private


ProcessMonitorClient::Private::PMCThreadQuitRequestHandler::
    PMCThreadQuitRequestHandler( FrameAlignedTimer & timer ) : timer_( timer ) 
{ 
    // Nothing 
}

void
ProcessMonitorClient::Private::PMCThreadQuitRequestHandler::HandleQuitRequest( 
    ::pthread_t ignored ) 
{
    timer_.InterruptPresentOrNextWait( );
}

void
ProcessMonitorClient::Private::thread( 
    const ProcessMonitorClient::Private::ThreadArgs & args )
try {
        
    sigset_t allSignals;
    sigfillset(&allSignals);
    pthread_sigmask(SIG_BLOCK, &allSignals, NULL);

    CARMA_CPTRACE( TRACE_THREADING, "Starting thread." );

    ScopedThreadQuitRegisterSelf quittable; 

    const string name = PROCESS_MONITOR_NAME; // Defined in IDL
    
    // Employ a random number generator seeded with this pid to ensure
    // that not all carma apps call alive() at the exact same time.
    ::srandom( Program::getProgram().getPid() ); 

    // Yes I know % breaks random but I don't need a perfect distribution here.
    const long offsetNanos = random() % 500000000; 
    FrameAlignedTimer timer( offsetNanos, UPDATE_PERIOD_IN_FRAMES );
        
    PMCThreadQuitRequestHandler tqrh( timer );

    ScopedThreadQuitRequestHandlerSelf scopeTqrhs( tqrh );

    ProcessMonitor_var procMonClient;

    unsigned consecutiveConnectionFailures = 0;

    while ( !Program::getProgram().imrTerminationRequested( ) ) 
    {

        ThreadQuitTestSelf( );
    
        try {

            if ( !args.useOrb ) {
                procMonClient = Program::getProgram().getCorbaClient().
                    resolveName< ProcessMonitor >( name );
            } else if ( args.orb != 0 ) {
                procMonClient = args.orb->resolveName< ProcessMonitor >( name );
            } else {
                throw CARMA_ERROR("Neither corba::Client or util::Orb exists.");
            }
                
            CARMA_CPTRACE( Trace::TRACE4, "Alive!" );
            procMonClient->alive( args.serverId.c_str() ); 

            consecutiveConnectionFailures = 0;

            while ( !Program::getProgram().imrTerminationRequested( ) )
            {
                ThreadQuitTestSelf( );
                CARMA_CPTRACE( Trace::TRACE4, "Alive!!" );
                procMonClient->alive( args.serverId.c_str() ); 
                ThreadQuitTestSelf( );
                timer.ResetNextFireTimeAndWait( UPDATE_PERIOD_IN_FRAMES - 1 );
            }

            CARMA_CPTRACE( TRACE_THREADING, "Thread exiting connected loop." );

            return;
        
        } catch (...) {
            procMonClient = ProcessMonitor::_nil();
            if ( consecutiveConnectionFailures % REPORT_EVERY_N_FAILURES == 0 )
            {
                ostringstream err;
                if ( consecutiveConnectionFailures > 0 ) {
                    err << consecutiveConnectionFailures << " consecutive "
                        << "connection failures (reporting every " 
                        << REPORT_EVERY_N_FAILURES * CONNECTION_RETRY_SECONDS
                        << " seconds).  Error message for this particular "
                        << "exception: " << getStringForCaught();
                } else {
                    err << getStringForCaught();
                }

                programLogErrorIfPossible( err.str() );
            }
            ++consecutiveConnectionFailures;
        } 

        ThreadQuitTestSelf( );

        ::sleep( CONNECTION_RETRY_SECONDS );

        ThreadQuitTestSelf( );
    }

    CARMA_CPTRACE( TRACE_THREADING, "Thread exiting connection loop." );

} catch (...) {
    if ( CaughtExceptionIsThreadQuitRequestedError( ) ) {
        CARMA_CPTRACE( TRACE_THREADING, "Thread exiting cleanly." );
    } else {
        const string err( getStringForCaught() );
        CARMA_CPTRACE( TRACE_THREADING, "Thread exiting dirty: " + err ); 
    }
}

ProcessMonitorClient::Private::Private( )
{
    const string serverId( parseORBServerId( ) );
    if ( serverId == "" ) 
        throw CARMA_ERROR( "-ORBServerId not specified." ); 

    Private::ThreadArgs args;
    args.useOrb = false;
    args.orb = 0;
    args.serverId = serverId;
    
    threadId_ = StartPthreadWithCopy( ProcessMonitorClient::Private::thread,
                                      args,
                                      "ProcessMonitorClient::thread" );
    threadQuitOnDestruction.insert( threadId_ );
}
    
ProcessMonitorClient::Private::Private( Orb * orb )
{
    const string serverId( parseORBServerId( ) );
    if ( serverId == "" ) 
        throw CARMA_ERROR( "-ORBServerId not specified." ); 

    Private::ThreadArgs args;
    args.useOrb = true;
    args.orb = orb;
    args.serverId = serverId;
    
    threadId_ = StartPthreadWithCopy( ProcessMonitorClient::Private::thread,
                                      args,
                                      "ProcessMonitorClient::thread" );
    threadQuitOnDestruction.insert( threadId_ );
}

ProcessMonitorClient::ProcessMonitorClient( )
    : private_( new ProcessMonitorClient::Private( ) )
{
    // Nothing
}

ProcessMonitorClient::ProcessMonitorClient( Orb * orb )
    : private_( new ProcessMonitorClient::Private( orb ) )
{
    // Nothing
}

ProcessMonitorClient::~ProcessMonitorClient( ) 
{
    // Nothing
}
