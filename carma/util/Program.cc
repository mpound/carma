//  $Id: Program.cc,v 1.282 2014/04/18 16:53:05 iws Exp $

// #define NO_CORBA
//  -DNO_CORBA : used during development to speed up compilation
//  by taking out the expensive CORBA header tree parsing. It should never
//  be turned off during production, and you better know what you are doing
//  if this is turned back on again.

// #define NO_SIGNAL
//  -DNO_SIGNAL : used to check out behavior without signal catchers
//  again this is something you should not turn off during production


#include "carma/util/Program.h"

#ifndef NO_CORBA
#   include "carma/corba/corba.h"
#endif


#include <memory>
#include <vector>
#include <sstream>
#include <exception>
#include <cerrno>
#include <cstdlib>

#include <unistd.h>

#include <sys/resource.h>
#include <sys/wait.h>

#include <libgen.h>

#include <signal.h>
#include <setjmp.h>
#include <execinfo.h>

#include "carma/corba/Client.h"
#include "carma/corba/Server.h"
#include "carma/util/BaseException.h"
#include "carma/util/demangle.h"
#include "carma/util/Trace.h"
#include "carma/util/Logger.h"
#include "carma/util/ProcessMonitorClient.h"
#include "carma/util/RuntimeDirs.h"
#include "carma/util/Backtrace.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/exceptionHandlersWatchdog.h"
#include "carma/util/loggingUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/signalUtils.h"
#include "carma/util/SyslogRedirector.h"
#include "carma/util/AutoFreedMallocPtr.h"
#include "carma/util/programInternalConfig.h"
#include "carma/util/Orb.h"

using namespace ::std;
using namespace ::log4cpp;
using namespace carma;
using namespace carma::util;


namespace CORBA {
    class Exception;
    class SystemException;
}  // namespace CORBA


namespace {

// file private typedefs

typedef enum {
    // Please note that these states are in a particular order.

    NOT_CONSTRUCTED_SINGLETON_STATE = 0,
    BEING_CONSTRUCTED_SINGLETON_STATE,
    CONSTRUCTED_SINGLETON_STATE,
    BEING_DESTRUCTED_SINGLETON_STATE,
    DESTRUCTED_SINGLETON_STATE,
} SingletonState;


// file private constants

const string kProgramVersion =
    "Program:: $Date: 2014/04/18 16:53:05 $ $Revision: 1.282 $";

const string kMandatoryDefaultValueString = "@mandatory";
const string kNoDefaultValueString1 = "@noDefault";
const string kNoDefaultValueString2 = "@nodefault";

const int kSignalTable[ ] = {
       SIGHUP,
    // SIGQUIT,
    // SIGTRAP,
    // SIGIOT,
    // SIGKILL,
       SIGUSR1,
       SIGUSR2,
       SIGPIPE,
    // SIGALRM,
       SIGTERM,
       SIGSTKFLT,
    // Sigchld is also sent if a child exits normally, so let's ignore it...
    // SIGCHLD,
    // SIGCONT,
    // SIGSTOP,
    // SIGTSTP,
       SIGTTIN,
       SIGTTOU,
       SIGURG,
       SIGXCPU,
       SIGXFSZ,
       SIGVTALRM,
    // SIGWINCH,
       SIGIO,
       SIGPWR,
       SIGSYS
};


const ::size_t kSignalTableCount =
    (sizeof( kSignalTable ) / sizeof( kSignalTable[ 0 ] ));

const ::size_t kSignalActionTableCount = kSignalTableCount + 20;

struct SignalActionTableEntry {
    bool             installed;
    int              signalNumber;
    struct sigaction oldSignalAction;
    struct sigaction signalAction;
};


// file private globals

// NOTE: These are globals so we can manage their lifetime outside of the
//       lifetime of the Program instance.
//       They file private so that no one can muck with them (or even see them)
//       outside of this file (this includes the client's implementation of
//       Program::main).

size_t           gSignalHandlerEnterCount = 0;
size_t           gSignalHandlerExitCount = 0;

int              gSignalHandlerSignalNumberArg = 0;

bool             gSignalHandlerGotSiginfo = false;
siginfo_t        gSignalHandlerSiginfo;

int              gSignalHandlerBtCount = 0;
bool             gSignalHandlerBtIncomplete = true;
void *           gSignalHandlerBtAddresses[ 64 ];

bool             gSignalHandlerGotCodeAddress = false;
void *           gSignalHandlerCodeAddress;

sigjmp_buf       gSignalJumpBuf;

SignalActionTableEntry gSignalActionTable[ kSignalActionTableCount ];

SingletonState   gSingletonState = NOT_CONSTRUCTED_SINGLETON_STATE;
Program *        gSingleton = 0;

::pid_t          gPid = 0;

const char *     gArg0 = 0;

int              gArgc = 0;
char **          gArgv = 0;

int              gExtraArgc = 0;
char **          gExtraArgv = 0;

string           gArgsLogString;

// file private functions

#ifdef NO_SIGNAL


void
installSignalHandlers( )
{
}

void
removeSignalHandlers( )
{
}


#else  // #ifdef NO_SIGNAL


bool
runningUnderADebugger( )
{
    return false;
}


bool
runningUnderProfiler( )
{
#    if defined( CARMA_GPROF )
#        if ( CARMA_GPROF != 1 )
#            error CARMA_GPROF is defined but has a value other than 1
#        else
             const bool status = true;
#        endif
#    else
         const bool status = false;
#    endif

    return status;
}


void
signalHandler( const int         signalNumber,
               siginfo_t * const sigInfo,
               void *            ucontextAsVoidPtr ) {
    // rh. use this for gprof and gconv
    //if (signalNumber == SIGINT)
    //exit(2);

    ++gSignalHandlerEnterCount;

    if ( gSignalHandlerEnterCount == 1 ) {
        gSignalHandlerSignalNumberArg = signalNumber;

        if ( sigInfo == 0 )
            gSignalHandlerGotSiginfo = false;
        else {
            gSignalHandlerSiginfo = *sigInfo;
            gSignalHandlerGotSiginfo = true;
        }

        const int maxCount =
            (sizeof( gSignalHandlerBtAddresses ) /
             sizeof( gSignalHandlerBtAddresses[ 0 ] ));

        const int btCount = ::backtrace( gSignalHandlerBtAddresses, maxCount );

        gSignalHandlerBtCount = btCount;
        gSignalHandlerBtIncomplete = (btCount == maxCount);

        if ( ucontextAsVoidPtr == 0 )
            gSignalHandlerGotCodeAddress = false;
        else {
            const ucontext_t & ucontext =
                *(static_cast< const ucontext_t *>( ucontextAsVoidPtr ));

            
#if __WORDSIZE == 64
            const greg_t ip = ucontext.uc_mcontext.gregs[ REG_RIP ];
#else
            const greg_t ip = ucontext.uc_mcontext.gregs[ REG_EIP ];
#endif
            void * const codeAddress = reinterpret_cast< void * >( ip );

            if ( codeAddress == 0 )
                gSignalHandlerGotCodeAddress = false;
            else {
                gSignalHandlerCodeAddress = codeAddress;
                gSignalHandlerGotCodeAddress = true;
            }
        }
    }

    ++gSignalHandlerExitCount;

    siglongjmp( gSignalJumpBuf, 1 );
}


void
setSignalAction( const int                signalNumber,
                 struct sigaction * const newSignalAction,
                 struct sigaction * const oldSignalAction ) {
    const int sigactionResult = ::sigaction( signalNumber,
                                             newSignalAction,
                                             oldSignalAction );

    if ( sigactionResult != 0 ) {
        const int savedErrno = errno;

        ostringstream oss;

        oss << "sigaction returned " << sigactionResult
            << " (errno=" << savedErrno << ")";

        throw CARMA_ERROR( oss.str() );
    }
}


void
installSignalHandler( const int                signalNumber,
                      SignalActionTableEntry & tableEntry ) {
    if ( tableEntry.installed == true )
        throw CARMA_ERROR( "Table entry already installed" );

    tableEntry.signalNumber = signalNumber;

    tableEntry.signalAction.sa_sigaction = signalHandler;
    setSignalSetToFull( tableEntry.signalAction.sa_mask );
    tableEntry.signalAction.sa_flags = SA_SIGINFO;
    tableEntry.signalAction.sa_restorer = 0;

    setSignalAction( signalNumber,
                     &(tableEntry.signalAction),
                     &(tableEntry.oldSignalAction) );

    tableEntry.installed = true;
}


void
installSignalHandlers( )
{
    for ( ::size_t j = 0; j < kSignalActionTableCount; ++j )
        gSignalActionTable[ j ].installed = false;

    ::size_t i = 0;

    if ( runningUnderADebugger() == false ) {
        installSignalHandler( SIGSEGV, gSignalActionTable[ i ] ); ++i;
        installSignalHandler( SIGILL, gSignalActionTable[ i ] ); ++i;
        installSignalHandler( SIGABRT, gSignalActionTable[ i ] ); ++i;
        installSignalHandler( SIGBUS, gSignalActionTable[ i ] ); ++i;
        installSignalHandler( SIGFPE, gSignalActionTable[ i ] ); ++i;
    }

    if ( runningUnderProfiler() == false ) {
        installSignalHandler( SIGPROF, gSignalActionTable[ i ] );
        ++i;
    }

    for ( ; i < kSignalTableCount; ++i )
        installSignalHandler( kSignalTable[ i ], gSignalActionTable[ i ] );
}


void
removeSignalHandlers( )
{
    for ( ::size_t i = 0; i < kSignalActionTableCount; ++i ) {
        if ( gSignalActionTable[ i ].installed == false )
            continue;

        gSignalActionTable[ i ].installed = false;

        struct sigaction action;

        setSignalAction( gSignalActionTable[ i ].signalNumber,
                         &(gSignalActionTable[ i ].oldSignalAction),
                         &action );
    }
}


#endif // #ifdef NO_SIGNAL/#else


void
emitErrorMessage( const string & message )
{
    cerr << "Error: carma/util/Program.cc:  " << message << "\n";
    cerr << "  Backtrace is:\n";

    Backtrace bt;
    bt.captureNoThrow();

    cerr << bt.formattedAsString( "    ", "\n" );

    cerr << endl;
}


void
tryToLogAndTraceErrorMessage( const string & message )
try {
    Category * const logger = ProgramBase::getLoggerIfAvailable();
    Trace * const trace = ProgramBase::getTraceObjectIfAvailable();

    if ( (logger != 0) || (trace != 0) ) {
        {
            const string msg =
                string( "Error: carma/util/Program.cc:  " ) + message;

            if ( logger != 0 )
                *logger << Priority::ERROR << msg;

            if ( trace != 0 )
                trace->write( Trace::TRACE1, msg );
        }

        string btText;

        {
            Backtrace bt;

            bt.captureNoThrow();

            btText = bt.formattedAsString( "    ", "\n" );
        }

        if ( logger != 0 ) {
            *logger << Priority::ERROR << "Backtrace is:";
            logMultipleLines( *logger, Priority::ERROR, btText );
        }

        if ( trace != 0 ) {
            trace->write( Trace::TRACE1,
                          string( "Backtrace is:\n" ) + btText );
        }
    }
} catch ( ... ) {
    // Just stifle any exceptions
}


bool
isLeadingSubstringOf( const string & s1, const string & s2 )
{
    return (s1 == s2.substr( 0, s1.size() ));
}


string
quoteString( const string & s )
{
    return string( "\"" ) + s + string( "\"" );
}


string
getMyHostname( )
{
    string result;

    {
        const ::size_t kHostnameMaxChars = 255;

        // NOTE: I allocate one extra char to hold a final safety net '\0' char
        vector< char > hostname;

        hostname.resize( kHostnameMaxChars + 1 );

        char * pHostname = &(hostname[ 0 ]);

        if ( gethostname( pHostname, kHostnameMaxChars ) != 0 ) {
            emitErrorMessage( "gethostname failed" );

            throw runtime_error( "gethostname failed" );
        }

        // Set up a final safety net '\0' char
        pHostname[ kHostnameMaxChars ] = '\0';

        result = pHostname;
    }

    // cout << "gethostname returned " << quoteString( result ) << "." << endl;

    return result;
}


facilityType
generateInitialFacility( const bool         haveInitialFacilityString,
                         const char * const initialFacilityName ) {
    facilityType result = DEFAULT_FACILITY;

    if ( haveInitialFacilityString ) {
        const string initialFacilityString = initialFacilityName;

        if ( initialFacilityString == "MONITOR_FACILITY" )
            result = MONITOR_FACILITY;
        else if ( initialFacilityString == "CONTROL_FACILITY" )
            result = CONTROL_FACILITY;
        else if ( initialFacilityString == "INTERFEROMETRY_FACILITY" )
            result = INTERFEROMETRY_FACILITY;
        else if ( initialFacilityString == "ENVIRONMENT_FACILITY" )
            result = ENVIRONMENT_FACILITY;
        else if ( initialFacilityString == "RX_FACILITY" )
            result = RX_FACILITY;
    }

    return result;
}


string
generateInitialLogname( const bool         haveInitialLogname,
                        const char * const initialLogname ) {
    if ( haveInitialLogname )
        return initialLogname;
    else
        return "carma.YOU.SHOULD.HAVE.PUT.A.HIERARCHY.HERE";
}


Trace::TraceLevel
convertToTraceLevelEnum( const int traceLevel )
{
    if ( traceLevel <= 0 )
        return static_cast< Trace::TraceLevel >( 0 );

    if ( traceLevel <= 7 )
        return static_cast< Trace::TraceLevel >( traceLevel );

    return static_cast< Trace::TraceLevel >( 7 );
}


string
getLogStringForArg( const char * const argCString )
{
    if ( argCString == 0 )
        return "< NULL arg >";

    const char kQuoteChar = '"';

    const string arg = argCString;

    bool needOuterQuotes = false;
    {
        const string kWs = " \t";
        const string kWsAndQuote = kWs + kQuoteChar;

        string::size_type pos = arg.find_first_of( kWsAndQuote );

        while ( pos != string::npos ) {
            if ( arg[ pos ] != '"' ) {
                // pos is unquoted whitespace

                needOuterQuotes = true;
                break;
            }

            // pos is an opening quote

            string::size_type closingQuotePos =
                arg.find_first_of( kQuoteChar, (pos + 1) );

            if ( closingQuotePos == string::npos ) {
                // There is no closing quote

                needOuterQuotes =
                    (arg.find_first_of( kWs, (pos + 1) ) != string::npos);

                break;
            }

            pos = arg.find_first_of( kWsAndQuote, (closingQuotePos + 1) );
        }
    }

    if ( needOuterQuotes )
        return kQuoteChar + arg + kQuoteChar;
    else
        return arg;
}


string
getLogStringForArgs( const int     argc,
                     char ** const argv )
{
    string result;

    if ( argc == 0 )
        result = "< zero argc >";
    else if ( argv == 0 )
        result = "< NULL argv >";
    else {
        for ( int i = 0; i < argc; ++i ) {
            const string argString = getLogStringForArg( argv[ i ] );

            if ( argString.empty() == false ) {
                if ( result.empty() == false )
                    result += " ";

                result += argString;
            }
        }
    }

    return result;
}


const string kCvsRevTagPrefix = "$" "Revision: ";  // Avoid CVS substitution
const string kCvsRevTagSuffix = "$";


string
prettyUpVersionString( const string & versionString )
{
    string result = versionString;

    while ( true ) {
        const string::size_type i = result.find( kCvsRevTagPrefix );

        if ( i == string::npos )
            break;

        const string::size_type j = i + kCvsRevTagPrefix.size();
        const string::size_type jEnd = result.find( kCvsRevTagSuffix, j );

        if ( jEnd == string::npos )
            break;

        const string tagValue = result.substr( j, (jEnd - j) );

        const string newText = "CVS Revision " + tagValue;

        result.replace( i, (jEnd + kCvsRevTagSuffix.size() - i), newText );
    }

    return result;
}


bool
getUmaskForString( const string & umaskString,
                   mode_t &       umaskValue )
{
    if ( umaskString.size() != 4 )
        return false;

    if ( umaskString.at( 0 ) != '0' )
        return false;

    if ( umaskString.find_first_not_of( "01234567" ) != string::npos )
        return false;

    umaskValue = ((static_cast< mode_t >( umaskString.at( 1 ) - '0' ) << 6) |
                  (static_cast< mode_t >( umaskString.at( 2 ) - '0' ) << 3) |
                  (static_cast< mode_t >( umaskString.at( 3 ) - '0' )));

    return true;
}


string
getStringForUmask( const mode_t umaskValue )
{
    string umaskString;

    umaskString += '0';
    umaskString += static_cast< char >( '0' + ((umaskValue >> 6) & 0x07) );
    umaskString += static_cast< char >( '0' + ((umaskValue >> 3) & 0x07) );
    umaskString += static_cast< char >( '0' + (umaskValue & 0x07) );

    return umaskString;
}


void
atexitHandler( )
try {
    if ( gSingletonState < CONSTRUCTED_SINGLETON_STATE )
        return;
        
    if ( gSingletonState > CONSTRUCTED_SINGLETON_STATE )
        return;
        
    if ( gSingleton == 0 )
        return;
        
    Category * const logger = gSingleton->getLoggerIfAvailable();
    Trace * const trace = gSingleton->getTraceObjectIfAvailable();
    
    if ( (logger == 0) && (trace == 0) )
        return;
        
    if ( logger != 0 )
        *logger << Priority::ERROR << "Program exiting (exit() called?).";
        
    if ( trace != 0 )
        trace->write( Trace::TRACE1, "Program exiting (exit() called?)." );
    
    string btText;
    {
        Backtrace bt;
        
        bt.captureNoThrow();
        
        btText = bt.formattedAsString( "  exiting bt ",
                                       "\n" );
    }
    
    if ( logger != 0 ) {
        *logger << Priority::ERROR << "Backtrace is:";
        logMultipleLines( *logger, Priority::ERROR, btText );
    }

    if ( trace != 0 ) {
        trace->write( Trace::TRACE1,
                      string( "Backtrace is:\n" ) + btText );
    }
} catch ( ... ) {
    return;
}


char gTimeZoneEnvVariable[] = "TZ=GMT";


}  // namespace < anonymous >


//
// the system keywords, known to all CARMA programs
// the program keywords have a simular structure, but it is extracted
// from the doxygenized comment source and turned into _keys.cc source code
//
// if you add new system keywords, this is the rough recipe for doing this:
// 1) add it to kSystemKeywords_ (the list below)
// 2) optionally set a default at the start of ProgramBase::initializeCarma
// 3) set the value from the command line (or default) in
//    ProgramBase::initializeCarma
//
const KeyTabEntry ProgramBase::kSystemKeywords_[ ] = {
    { "corbaRrtt", "60",
               "int",
               "@autogen",
               "Default corba relative round-trip timeout (S)." },
    { "debug", "0",
               "int",
               "@autogen",
               "debug level" },

    { "daemonize", "false",
                   "bool",
                   "@autogen",
                   "program should run as a daemon?" },

    { "syslog", "true",
                "bool",
                "@autogen",
                "use syslog for logging?"},

    { "logfile", "",
                 "string",
                 "@autogen",
                 "log filename for logging (stdout/syslog)"},

    { "logname", "@noDefault",
                 "string",
                 "@autogen",
                 "set logname, e.g. carma.system.subsystem"},

    { "imr", "@noDefault",
             "string",
             "@autogen",
             "IMR hostname and optional port in hostname<:port> format"},

    { "nice", "0",
              "int",
              "@autogen",
              "nice level" },

    { "redirectStdout", "false", "bool", "@autogen", "redirect all stdout output to syslog" },
    { "redirectStderr", "false", "bool", "@autogen", "redirect all stderr output to syslog" },

    { "traceLevel", "0",
                    "int",
                    "@autogen",
                    "traceLevel=n prints out all messages with priority <= n "
                    "(0 == none)" },

    { "traceFile", "stdout",
                   "string",
                   "@autogen",
                   "File for Trace output" },

    { "traceVerbose", "true",
                      "bool",
                      "@autogen",
                      "Print out in full log format. If false, print simple lines" },

    { "useDBMS", (Config::kUseDbmsDefaultValue ? "true" : "false"),
                 "bool",
                 "@autogen",
                 "Use the DBMS as the tag ID authority?" },

    { "umask", "0002",
               "string",
               "@autogen",
               "Octal umask value to set for the program at start up" },


    { "useGMT", "true",
                "bool",
                "@autogen",
                "Use GMT as program timezone" },

    { 0, 0, 0, 0, 0 }
};


// main( ) is defined here, application programmers will need to define
// Program::main( ) instead of the usual main( )
// as well as a set of (actually optional) keywords, usage, version, description
// that the "keys" program parses into C++ code to be linked in during program
// compilation.
//
// The main() function simply calls carma_main(), which is defined below.
// The reason for having main() call carma_main() is to provide the main()
// entry point to program's linked with libcarmautil, while still allowing
// non-CARMA programs (e.g. python) to dynamically load and initialize
// libcarmautil.so without having to call libcarmautil's main(), which would be
// masked by the calling program's own main().

int
main( const int     argc,
      char ** const argv )
{
    return carma_main(argc, argv);
}


int
carma_main( const int     argc,
            char ** const argv )
{
    set_terminate( terminateHandler );
    set_unexpected( unexpectedHandler );
    atexit( atexitHandler );

    if ( (argc < 1) || (argv == 0) || (argv[ 0 ] == 0) ) {
        emitErrorMessage( "argc and/or argv are confused" );

        return 1;
    }

    gArgsLogString = getLogStringForArgs( argc, argv );

    gArg0 = argv[ 0 ];

    gArgc = argc;
    gArgv = argv;

    int retval = 1;

    try {
        Program::getProgram();  // force it to be built if it hasn't already

        if ( (gSingletonState != CONSTRUCTED_SINGLETON_STATE) ||
             (gSingleton == 0) ) {
            emitErrorMessage( "Bad program singleton state" );

            throw logic_error( "Bad program singleton state" );
        }

        try {
            retval = ProgramBase::run( *gSingleton );
        } catch ( ... ) {
            retval = 1;
        }

        gSingletonState = BEING_DESTRUCTED_SINGLETON_STATE;

        try {
            gSingleton->state_ = ProgramBase::BEING_DESTRUCTED_STATE;

            if ( true ) {
                Program * deadProgramWalking = 0;

                ::std::swap( deadProgramWalking, gSingleton );

                delete deadProgramWalking;
            } else
                gSingleton->advanceState( ProgramBase::DESTRUCTED_STATE );
        } catch ( ... ) {
            gSingletonState = DESTRUCTED_SINGLETON_STATE;

            throw;
        }

        gSingletonState = DESTRUCTED_SINGLETON_STATE;
    } catch ( ... ) {
        if ( retval == 0 )
            retval = 1;
    }

    return retval;
}


ProgramBase::ProgramBase( ) :
state_( BEING_CONSTRUCTED_STATE ),
progname_( "< unknown >" ),
logger_( 0 ),
logHostName_( "localhost" ),
imrHostnameWasSpecified_( false ),
corbaServer_( 0 ),
debugLevel_( 0 ),
nice_( 0 ),
useGMT_(true),
traceLevel_( 0 ),
traceVerbose_( true ),
traceObject_( 0 ),
facility_( generateInitialFacility( kHaveInitialLoggerInfo_,
                                    kInitialFacilityName_ ) ),
logname_( generateInitialLogname( kHaveInitialLoggerInfo_,
                                  kInitialLogname_ ) ),
instanceLognameSet_( false ),
useDBMS_( false )
{
    if ( debugLevel_ > 0 )
        cout << "ProgramBase::ProgramBase() called." << endl;

    advanceState( CONSTRUCTED_STATE );
}


ProgramBase::~ProgramBase( )
try {
    try {
        if ( state_ != BEING_DESTRUCTED_STATE ) {
            state_ = BEING_DESTRUCTED_STATE;

            emitErrorMessage( "Bad program instance state" );
        }

        if ( debugLevel_ > 0 ) {
            cout << "ProgramBase::~ProgramBase(): "
                 << progname_
                 << " ended."
                 << endl;
        }
    } catch ( ... ) {
        // just stifle the exception
    }

    try {
        processMonitorClient_.reset();
        corbaServer_.reset();
    } catch (...) {
        
    }

    try {
        Trace * deadTraceObjectWalking = 0;

        ::std::swap( deadTraceObjectWalking, traceObject_ );

        delete deadTraceObjectWalking;
    } catch ( ... ) {
        // just stifle the exception
    }

    advanceState( DESTRUCTED_STATE );
} catch ( ... ) {
    // just stifle the exception

    return;
}


void
ProgramBase::advanceState( const StateType newState ) {
    if ( newState > state_ ) {
        const bool skippedSome = (newState != (state_ + 1));

        state_ = newState;

        if ( skippedSome )
            emitErrorMessage( "Skipped some states" );
    } else {
        if ( newState == state_ )
            emitErrorMessage( "State did not advance" );
        else
            emitErrorMessage( "Tried to move state backwards" );
    }
}

void 
ProgramBase::renice( void ) {
    
    errno = 0;
    int currentNice = ::getpriority( PRIO_PROCESS, 0 );
    int errnoVal = errno;
    if ( currentNice == -1 && errnoVal != 0 ) {
	ostringstream os;
        os << "Unable to determine my current nice value." 
	    << " Reason: "
	    << ::strerror( errnoVal )
	    << ". Will try to set Program nice anyway."
	    ;
        getLogger() << Priority::INFO << os.str();
    }

    // if no change in nice value, do nothing.
    if ( currentNice == nice_ )
	return;

    errno = 0;
    errnoVal = 0;
    // With PRIO_PROCESS, we will still set nice value for 
    // system scope threads in the process.
    // Note PRIO_PGRP fails for "permission denied".
    // see man setpriority
    int retVal = ::setpriority( 0, PRIO_PROCESS, nice_ );
    if ( retVal != 0 ) {
        errnoVal = errno;
	ostringstream os;
        os << "Unable to renice myself with nice = " 
	    << nice_
	    << ". Reason: "
	    << ::strerror( errnoVal )
	    ;
        getLogger() << Priority::INFO << os.str();
    }
}

int
ProgramBase::run( Program & program ) {
    volatile int returnValue = 0;  // success = 0, failure = -1

    try  {
        // initialize all, daemonize, catchSignals
        returnValue = program.initializeCarma();

        // nothing to do after all ... return right now
        if ( returnValue )
            return 0;
    } catch ( const BaseException & ) {
        returnValue = -1;
    }

    if ( returnValue == 0 ) {
#ifdef NO_SIGNAL
        const int setSignalJumpBufReturn = 0;
#else
        volatile int setSignalJumpBufReturn = sigsetjmp( gSignalJumpBuf, 1 );
#endif

        if ( setSignalJumpBufReturn == 0 ) {
            installSignalHandlers();

            const Priority::PriorityLevel priority = Priority::ERROR;

            try  {
	      const AutoExceptionHandlersWatchdog watchdogStartStop;

                returnValue = program.main();

            } catch ( const BaseException & e )  {
                // code for BaseException

                e.logException( priority );

                getLogger() << priority
                            << " Terminating "
                            << program.progname_
                            << " :BaseException caught in Program - "
                            << getStringForCaught();

                returnValue = -1;

#ifndef NO_CORBA

            } catch ( const CORBA::SystemException & e )  {
                // code for Orbacus system exception

                getLogger() << priority
                            << " Terminating "
                            << program.progname_
                            << " :CORBA::SystemException caught in Program - "
                            << e
                            << " " << e._info().c_str();

                returnValue = -1;
            } catch ( const CORBA::Exception & e )  {
                // code for Orbacus CORBA exceptions

                getLogger() << priority
                            << " Terminating "
                            << program.progname_
                            << " :CORBA::Exception caught in Program - "
                            << e;

                returnValue = -1;

#endif  // #ifndef NO_CORBA
            } catch ( const ::std::exception & e )  {
                // code for standard C++ exception

                getLogger() << priority
                            << " Terminating " << program.progname_ << ": "
                            << demangleTypeName( typeid( e ) )
                            << " exception caught in Program - "
                            << e.what();

                returnValue = -1;
            } catch ( const char * const & e )  {
                getLogger() << priority
                            << " Terminating " << program.progname_ << ": "
                            << " const char * caught in Program - "
                            << e;

                returnValue = -1;
            } catch ( char * const & e )  {
                getLogger() << priority
                            << " Terminating " << program.progname_ << ": "
                            << " char * caught in Program - "
                            << e;

                returnValue = -1;
            } catch ( const ::std::string & e )  {
                getLogger() << priority
                            << " Terminating " << program.progname_ << ": "
                            << " ::std::string caught in Program - "
                            << e;

                returnValue = -1;
            } catch ( ... ) {
                // code for catchall

                getLogger() << priority
                             << " Terminating "
                             << program.progname_
                             << " :Unknown exception caught in Program.";

                returnValue = -1;
            }
                    
            removeSignalHandlers();
        } else {
            // we appear to have actually caught a signal

            returnValue = -1;

#ifdef NO_SIGNAL
            emitErrorMessage( "signal caught with NO_SIGNAL defined!" );
#endif

            removeSignalHandlers();

            getLogger() << Priority::ERROR << "Program Terminating: "
                         << "Long jumped out and terminating.";

            if ( (gSignalHandlerEnterCount != 1) ||
                 (gSignalHandlerExitCount != 1) ) {
                getLogger() << Priority::ERROR << "Program Terminating: "
                             << "Signal handler was not invoked exactly once.";
            } else {
                getLogger() << Priority::ERROR << "Program Terminating: "
                             << "Signal handler invoked.";

                const int signalNumArg = gSignalHandlerSignalNumberArg;

                const char * signalNumArgText =
                    getTextForSignalNumber( signalNumArg );

                if ( signalNumArgText == 0 )
                    signalNumArgText = "< unknown >";

                ostringstream oss;

                getLogger() << Priority::ERROR << "Program Terminating: "
                             << "Signal number was " << signalNumArg
                             << " (" << signalNumArgText << ").";

                if ( gSignalHandlerGotSiginfo == false ) {
                    getLogger() << Priority::ERROR << "Program Terminating: "
                                 << "Signal info was not available.";
                } else {
                    const siginfo_t siginfo = gSignalHandlerSiginfo;

                    getLogger() << Priority::ERROR << "Program Terminating: "
                                 << "Signal info was "
                                 << getStringForSiginfo( siginfo )
                                 << ".";
                }

                if ( gSignalHandlerBtCount < 1 ) {
                    getLogger() << Priority::ERROR << "Program Terminating: "
                                 << "Backtrace was not available.";
                } else {
                    const int btCount = gSignalHandlerBtCount;
                    const bool btIncomplete = gSignalHandlerBtIncomplete;

                    const string completenessString =
                        (btIncomplete ? "Incomplete" : "Complete");

                    getLogger()
                        << Priority::ERROR << "Program Terminating: "
                        << completenessString << " mangled backtrace was:";

                    AutoFreedMallocPtr< char * >
                        mangledLinesBlock(
                            ::backtrace_symbols( gSignalHandlerBtAddresses,
                                                 btCount ) );

                    char * const * const mangledLines =
                        mangledLinesBlock.get();

                    if ( mangledLines == 0 ) {
                        getLogger()
                            << Priority::ERROR
                            << "Program Terminating: "
                            << "  backtrace_symbols failed";
                    } else {
                        for ( int i = 0; i < btCount; ++i ) {
                            getLogger()
                                << Priority::ERROR
                                << "Program Terminating:   mangled bt #"
                                << i
                                << ": "
                                << mangledLines[ i ];
                        }
    
                        getLogger()
                            << Priority::ERROR
                            << "Program Terminating: "
                            << completenessString
                            << " demangled backtrace was:";
    
                        for ( int i = 0; i < btCount; ++i ) {
                            const char * const mangledLine = mangledLines[ i ];
    
                            string demangledLine;
    
                            if ( mangledLine == 0 )
                                demangledLine = "< unknown >";
                            else {
                                try {
                                    demangledLine =
                                        Backtrace::demangleSymbolLine(
                                            mangledLine );
                                } catch ( ... ) {
                                    demangledLine = mangledLine;
                                }
                            }
    
                            getLogger()
                                << Priority::ERROR
                                << "Program Terminating:   demangled bt #"
                                << i
                                << ": "
                                << demangledLine;
                        }
                    }
                }

                if ( gSignalHandlerGotCodeAddress == false ) {
                    getLogger() << Priority::ERROR << "Program Terminating: "
                                 << "Code address was not available.";
                } else {
                    void * const codeAddress = gSignalHandlerCodeAddress;

                    getLogger() << Priority::ERROR << "Program Terminating: "
                                 << "Code address was " << codeAddress;

                    AutoFreedMallocPtr< char * >
                        mangledLinesBlock(
                            ::backtrace_symbols( &codeAddress, 1 ) );

                    char * const * const mangledLines =
                        mangledLinesBlock.get();

                    if ( mangledLines == 0 ) {
                        getLogger()
                            << Priority::ERROR
                            << "Program Terminating: "
                            << "backtrace_symbols failed";
                    } else {
                        const char * const mangledLine = mangledLines[0];
    
                        if ( mangledLine == 0 ) {
                            getLogger()
                                << Priority::ERROR
                                << "Program Terminating: "
                                << "Code address does not map to a symbol";
                        } else {
                            getLogger()
                                << Priority::ERROR
                                << "Program Terminating: "
                                << "Code address maps to mangled symbol "
                                << mangledLine;
    
                            string demangledLine;
    
                            try {
                                demangledLine =
                                    Backtrace::demangleSymbolLine( mangledLine );
                            } catch ( ... ) {
                                demangledLine = mangledLine;
                            }
    
                            getLogger()
                                << Priority::ERROR
                                << "Program Terminating: "
                                << "Code address maps to demangled symbol "
                                << demangledLine;
                        }
                    }
                }
            }
        }
    }

    try  {
        {
            Category & logger = getLogger();
    
            if ( returnValue == 0 ) {
                logger << Priority::NOTICE
                       << "Program "
                       << program.progname_
                       << "("
                       << program.getPid()
                       << ") terminating...";
            } else {
                logger << Priority::NOTICE
                       << "Program "
                       << program.progname_
                       << "("
                       << program.getPid()
                       << ") terminating with return value "
                       << returnValue
                       << "...";
            }
            
            // close logger and get rid of it
            logger.shutdown();
        }
    
        if ( program.debugLevel_ > 0 ) {
            ::size_t numUnread = 0;
    
            ParameterInfoMap::const_iterator iMap =
                program.parameterInfoMap_.begin();
    
            const ParameterInfoMap::const_iterator iMapEnd =
                program.parameterInfoMap_.end();
    
            while ( iMap != iMapEnd ) {
                const bool unspecifiedNoDefaultWasChecked =
                    ((iMap->second.valueSpecified == false) &&
                     (iMap->second.specifyType == NO_DEFAULT_PARAM_SPECIFY_TYPE) &&
                     (iMap->second.valueSpecifiedWasChecked == true));
    
                if ( (unspecifiedNoDefaultWasChecked == false) &&
                     (iMap->second.valueWasRead == false) ) {
                    ++numUnread;
    
                    if ( numUnread == 1 )
                        cerr << "The following keywords have never been read:";
    
                    cerr << "  " << iMap->first << endl;
                }
    
                ++iMap;
            }
    
            if ( numUnread > 0 )
                cerr << endl;
        }
    } catch ( ... )  {
        returnValue = -1;
    }

    return returnValue;
}


void
ProgramBase::verifyStateIsInRange( const StateType rangeBegin,
                                   const StateType rangeEnd ) const {
    const StateType state = state_;

    if ( (state < rangeBegin) || (state >= rangeEnd) ) {
        string stateName = "< unknown >";

        switch ( state ) {
            case BEING_CONSTRUCTED_STATE:
                stateName = "BEING_CONSTRUCTED_STATE";
                break;

            case CONSTRUCTED_STATE:
                stateName = "CONSTRUCTED_STATE";
                break;

            case ADDING_KEY_DEFINITIONS_STATE:
                stateName = "ADDING_KEY_DEFINITIONS_STATE";
                break;

            case KEY_DEFINITIONS_ADDED_STATE:
                stateName = "KEY_DEFINITIONS_ADDED_STATE";
                break;

            case PARSING_COMMAND_LINE_STATE:
                stateName = "PARSING_COMMAND_LINE_STATE";
                break;

            case COMMAND_LINE_PARSED_STATE:
                stateName = "COMMAND_LINE_PARSED_STATE";
                break;

            case INITIALISING_SYSTEM_KEYWORD_MEMBERS_STATE:
                stateName = "INITIALISING_SYSTEM_KEYWORD_MEMBERS_STATE";
                break;

            case SYSTEM_KEYWORD_MEMBERS_INITIALISED_STATE:
                stateName = "SYSTEM_KEYWORD_MEMBERS_INITIALISED_STATE";
                break;

            case LOGGER_AVAILABLE_STATE:
                stateName = "LOGGER_AVAILABLE_STATE";
                break;

            case TRACE_OBJECT_AVAILABLE_STATE:
                stateName = "TRACE_OBJECT_AVAILABLE_STATE";
                break;

            case FULLY_INITIALISED_STATE:
                stateName = "FULLY_INITIALISED_STATE";
                break;

            case BEING_DESTRUCTED_STATE:
                stateName = "BEING_DESTRUCTED_STATE";
                break;

            case DESTRUCTED_STATE:
                stateName = "DESTRUCTED_STATE";
                break;
        }

        const string msg =
            string( "bad program instance state (state = " ) +
            stateName +
            string( ")" );

        emitErrorMessage( msg );
    }
}


void
ProgramBase::verifyStateIsNormal( ) const
{
    verifyStateIsInRange( FULLY_INITIALISED_STATE,
                          BEING_DESTRUCTED_STATE );
}


void
ProgramBase::verifyStateIsInitializingSystemKeywordMembers( ) const
{
    verifyStateIsInRange( INITIALISING_SYSTEM_KEYWORD_MEMBERS_STATE,
                          SYSTEM_KEYWORD_MEMBERS_INITIALISED_STATE );
}


void
ProgramBase::setLogger( )
{
    verifyStateIsInRange( SYSTEM_KEYWORD_MEMBERS_INITIALISED_STATE,
                          BEING_DESTRUCTED_STATE );

    const Priority::PriorityLevel priority = Priority::ERROR;

    logger_ = 0;

    if ( syslog_ )  {
        Category & sysLog = getDefaultLogger();

        logger_ = &sysLog;

        if ( logFileName_.empty() == false ) {
            // At this point, (syslog_ && (logFileName_.empty() == false))
            // so we need to make an appender

            Category & fileLog =
                Logger::getFilelogger( progname_,
                                       logFileName_,
                                       logname_ );

            Appender * fileAppender = fileLog.getAppender();

            if ( fileAppender == 0 ) {
                Category & logger = *logger_;

                logger << priority
                       << "File appender returned from file logger "
                       << "which was obtained from Logger::getFilelogger "
                       << "is NULL. ";

                throw CARMA_ERROR( "ProgramBase::setLogger - "
                                   "invalid file appender" );
            }

            logger_->setAdditivity( true );
            logger_->getRoot().removeAllAppenders();
            logger_->addAppender( fileAppender );
        }
    } else if ( logFileName_.empty() == false )  {
        Category & fileLog =
            Logger::getFilelogger( progname_,
                                   logFileName_,
                                   logname_ );

        logger_ = &fileLog;
    } else {
        Category & fileLog =
            Logger::getFilelogger( progname_,
                                   "/dev/null",
                                   logname_ );

        logger_ = &fileLog;
    }
}


void
ProgramBase::setInstanceLogname( const string & logname )
{
    verifyStateIsInRange( SYSTEM_KEYWORD_MEMBERS_INITIALISED_STATE,
                          BEING_DESTRUCTED_STATE );

    const bool alreadySet = instanceLognameSet_;

    logname_ = logname;
    instanceLognameSet_ = true;

    setLogger();

    if ( kHaveInitialLoggerInfo_ == false ) {
        getLogger() << Priority::ERROR
                     << "ProgramBase::setInstanceLogname called but a @logger "
                     << "tag was never defined";

        CARMA_CPTRACE( Trace::TRACE1,
                       "ProgramBase::setInstanceLogname called but a @logger "
                       "tag was never defined" );
    }

    if ( alreadySet ) {
        getLogger() << Priority::ERROR
                     << "ProgramBase::setInstanceLogname called multiple times";

        CARMA_CPTRACE( Trace::TRACE1,
                       "ProgramBase::setInstanceLogname called multiple times" );
    }
}

void
ProgramBase::adjustTraceLevel( const int newTraceLevel )
{
    verifyStateIsNormal();

    if ( traceObject_ != 0 ) {
        if ( newTraceLevel <= 0 ) {
            string msg;
            {
                ostringstream oss;

                oss << "Adjusting trace level to " << newTraceLevel;

                msg = oss.str();
            }

            traceObject_->write( Trace::TRACE1, msg );
        }

        const Trace::TraceLevel newTraceLevelEnum =
            convertToTraceLevelEnum( newTraceLevel );

        traceObject_->setObjectTraceLevel( newTraceLevelEnum );
    }

    traceLevel_ = newTraceLevel;

    if ( (logger_ != 0) || (traceObject_ != 0) ) {
        string msg;
        {
            ostringstream oss;

            oss << "Trace level adjusted to " << traceLevel_;

            msg = oss.str();
        }

        if ( logger_ != 0 )
            *logger_ << Priority::INFO << msg;

        if ( traceObject_ != 0 )
            traceObject_->write( Trace::TRACE1, msg );
    }
}


void
ProgramBase::setTrace( )
{
    verifyStateIsInRange( LOGGER_AVAILABLE_STATE,
                          BEING_DESTRUCTED_STATE );

    {
        Trace * deadTraceObjectWalking = 0;

        ::std::swap( deadTraceObjectWalking, traceObject_ );

        delete deadTraceObjectWalking;
    }

    const Trace::TraceLevel traceLevelEnum =
        convertToTraceLevelEnum( traceLevel_ );

    // set up trace object
    if ( (traceFile_ == "stdout") ||
         (traceFile_ == "stderr") ||
         (traceFile_ == "cout") ||
         (traceFile_ == "cerr") ||
         (traceFile_ == "std::cout") ||
         (traceFile_ == "std::cerr") ) {
        traceObject_ = new Trace( traceLevelEnum,
                                  "stdout",
                                  traceVerbose_,
                                  progname_,
                                  facility_ );
    } else {
        traceObject_ = new Trace( traceLevelEnum,
                                  traceFile_,
                                  traceVerbose_,
                                  progname_,
                                  facility_ );
    }
}

void
ProgramBase::setCorba( ) 
{
    verifyStateIsNormal( );

    bool noPCS = false; // no Program corba::Server.
    const ParameterInfoMap::iterator iMap = parameterInfoMap_.find( "noPCS" );
    if ( iMap != parameterInfoMap_.end() )
        noPCS = getBoolParameter( "noPCS" );

    if ( imrHostnameWasSpecified_ && !noPCS ) {
    
        const int extraArgc = getExtraArgc();
        char **   extraArgv = getExtraArgv();

        corbaServer_ = auto_ptr< carma::corba::Server >( new 
            carma::corba::Server( extraArgc, extraArgv, corbaRrtt_ ) );

        bool serverIdSpecified = false;
        for ( int i = 0; i < extraArgc; ++i ) {
            if ( string( extraArgv[i] ) == "-ORBServerId" ) {
                serverIdSpecified = true;
                break;
            }
        }

        if ( serverIdSpecified ) {
            processMonitorClient_ = auto_ptr< carma::util::ProcessMonitorClient >(
                new carma::util::ProcessMonitorClient( ) );
        } 


    } 

}

Category &
ProgramBase::getDefaultLogger( ) const
{
    verifyStateIsInRange( SYSTEM_KEYWORD_MEMBERS_INITIALISED_STATE,
                          BEING_DESTRUCTED_STATE );

    return Logger::getSyslogger( progname_,
                                 getLogHostname(),
                                 logname_,
                                 Priority::INFO,
                                 facility_ );
}


Category &
ProgramBase::getLogger()
{
    Program & p = Program::getProgram();

    p.verifyStateIsInRange( SYSTEM_KEYWORD_MEMBERS_INITIALISED_STATE,
                            BEING_DESTRUCTED_STATE );

    Category * logger = p.logger_;

    if ( logger != 0 )
        return *logger;

    return p.getDefaultLogger();
}


Trace *
ProgramBase::getTraceObject( )
{
    Program & p = Program::getProgram();

    p.verifyStateIsInRange( TRACE_OBJECT_AVAILABLE_STATE,
                            BEING_DESTRUCTED_STATE );

    return p.traceObject_;
}


Category *
ProgramBase::getLoggerIfAvailable( )
{
    if ( gSingletonState != CONSTRUCTED_SINGLETON_STATE )
        return 0;

    if ( gSingleton == 0 )
        return 0;

    if ( gSingleton->state_ < LOGGER_AVAILABLE_STATE )
        return 0;

    if ( gSingleton->state_ >= BEING_DESTRUCTED_STATE )
        return 0;

    return &(ProgramBase::getLogger());
}


Trace *
ProgramBase::getTraceObjectIfAvailable( )
{
    if ( gSingletonState != CONSTRUCTED_SINGLETON_STATE )
        return 0;

    if ( gSingleton == 0 )
        return 0;

    if ( gSingleton->state_ < TRACE_OBJECT_AVAILABLE_STATE )
        return 0;

    if ( gSingleton->state_ >= BEING_DESTRUCTED_STATE )
        return 0;

    return ProgramBase::getTraceObject();
}


void
ProgramBase::docKey( const string &        name,
                     const ParameterInfo & parameterInfo )
{
    cout << name;

    switch ( parameterInfo.specifyType ) {
        case NORMAL_PARAM_SPECIFY_TYPE:
            if ( parameterInfo.defaultValueString.empty() )
                cout << "=\"\"";
            else
                cout << "=" << parameterInfo.defaultValueString;
            break;

        case MANDATORY_PARAM_SPECIFY_TYPE:
            cout << " [MANDATORY parameter]";
            break;

        case NO_DEFAULT_PARAM_SPECIFY_TYPE:
            cout << " [no default]";
            break;
    }

    cout << "\n";

    cout << "\t" << parameterInfo.help << "\n";
}


void
ProgramBase::docKeys( const bool system,
                      const bool declOrder ) const
{
    if ( declOrder == false ) {
        // Alphabetical order - which the map gives us naturally

        ParameterInfoMap::const_iterator iMap =
            parameterInfoMap_.begin();

        const ParameterInfoMap::const_iterator iMapEnd =
            parameterInfoMap_.end();

        while ( iMap != iMapEnd ) {
            if ( iMap->second.system == system )
                docKey( iMap->first, iMap->second );

            ++iMap;
        }
    } else {
        // decl ordering

        // Warning: This is maybe a little hairy to try and follow. I should
        //          do something easier to understand than a map of iterators
        //          into another map!

        typedef map< ::size_t, ParameterInfoMap::const_iterator >
                DeclOrderingMap;

        DeclOrderingMap declOrdering;

        // create a new ordering based on the decl indices
        {
            ParameterInfoMap::const_iterator iMap =
                parameterInfoMap_.begin();

            const ParameterInfoMap::const_iterator iMapEnd =
                parameterInfoMap_.end();

            while ( iMap != iMapEnd ) {
                if ( iMap->second.system == system )
                    declOrdering[ iMap->second.declIndex ] = iMap;

                ++iMap;
            }
        }

        // emit the key docs using the new ordering

        DeclOrderingMap::const_iterator iDeclOrdering =
            declOrdering.begin();

        const DeclOrderingMap::const_iterator iDeclOrderingEnd =
            declOrdering.end();

        while ( iDeclOrdering != iDeclOrderingEnd ) {
            const ParameterInfoMap::const_iterator iMap = iDeclOrdering->second;

            docKey( iMap->first, iMap->second );

            ++iDeclOrdering;
        }
    }

    cout.flush();
}


// initializeCarma has to initialize a CARMA program.
// Initializes the logger, sets up generic CARMA signal handlers,
// daemonizes program (if required)

int
ProgramBase::initializeCarma( )
{
    if ( state_ != CONSTRUCTED_STATE )
        emitErrorMessage( "Bad program instance state" );

    {
        const string arg0 = getArg0();

        const string::size_type lastSlashPos = arg0.find_last_of( '/' );

        if ( lastSlashPos != string::npos )
            progname_ = arg0.substr( (lastSlashPos + 1) );
        else
            progname_ = arg0;
    }

    advanceState( ADDING_KEY_DEFINITIONS_STATE );

    // add system keys
    {
        ::size_t sysDeclIndex = 0;

        while ( true ) {
            if ( kSystemKeywords_[ sysDeclIndex ].key == 0 )
                break;

            addKey( kSystemKeywords_[ sysDeclIndex ], true, sysDeclIndex );

            ++sysDeclIndex;
        }

        if ( debugLevel_ > 0 )
            cout << "Found " << sysDeclIndex << " system keywords." << endl;
    }

    // add program keys
    {
        ::size_t progDeclIndex = 0;

        while ( true ) {
            if ( kKeywords_[ progDeclIndex ].key == 0 )
                break;

            addKey( kKeywords_[ progDeclIndex ], false, progDeclIndex );

            ++progDeclIndex;
        }

        if ( debugLevel_ > 0 )
            cout << "Found " << progDeclIndex << " program keywords." << endl;
    }

    advanceState( KEY_DEFINITIONS_ADDED_STATE );

    advanceState( PARSING_COMMAND_LINE_STATE );

    if ( gArgc > 1 ) {           // handle common system shortcuts
        const string arg1 = gArgv[ 1 ];

        if ( arg1.substr( 0, 2 ) == "--" ) {
            const string afterDashDash = arg1.substr( 2 );

            if ( afterDashDash.empty() ) {
                // rest of the commandline is supposed to be skipped
            } else if ( isLeadingSubstringOf( afterDashDash, "help" ) ) {
                cerr << kProgramVersion << "\n";
                cerr << "Current system options:\n";
                cerr << " --help         this help\n";
                cerr << " --keywords     show program keys, values and help\n";
                cerr << " --system       show system keys, values and help\n";
                cerr << " --version      show program version\n";
                cerr << " --usage        show program usage line\n";
                cerr << " --description  show program description section\n";
                cerr << " --show         show some debugging info\n";
                cerr << " --             arguments after this are not processed";
                cerr << endl;

                if ( gArgc == 2 )
                    return 1;
            } else if ( isLeadingSubstringOf( afterDashDash, "keywords" ) ) {
                docKeys( false, true );

                if ( gArgc == 2 )
                    return 1;
            } else if ( isLeadingSubstringOf( afterDashDash, "system" ) ) {
                docKeys( true, true );

                if ( gArgc == 2 )
                    return 1;
            } else if ( isLeadingSubstringOf( afterDashDash, "version" ) ) {
                cout << progname_ << " : "
                     << prettyUpVersionString( kVersion_ ) << endl;

                if ( gArgc == 2 )
                    return 1;

                return 0;
            } else if ( isLeadingSubstringOf( afterDashDash, "usage" ) ) {
                cout <<  getUsageString() << endl;

                if ( gArgc == 2 )
                    return 1;
            } else if ( isLeadingSubstringOf( afterDashDash, "description" ) ) {
                cout << getDescriptionString() << endl;

                if ( gArgc == 2 )
                    return 1;
            } else if ( isLeadingSubstringOf( afterDashDash, "show" ) ) {
                show();

                if ( gArgc == 2 )
                    return 1;
            } else {
                cerr << "Unrecognized argument \"" << arg1 << "\"." << endl;

                exit( 1 );
            }
        }
    }

    int dashDashArgIndex = 0;  // index of "--" arg if present and 0 otherwise

    for ( int i = 1; i < gArgc; ++i ) {        // parse command line
        const string argI = gArgv[ i ];

        if ( argI == "--" ) {
            dashDashArgIndex = i;

            break;
        }

        if ( debugLevel_ > 0 )
            cout << "Parsing " << argI << endl;

        string key;
        string valueString;

        {
            const string::size_type equalsPos =
                argI.find_first_of( '=' );

            if ( equalsPos == string::npos ) {
                cerr << "parameter \"" << argI << "\" has no value." << endl;

                exit( 1 );
            }

            key = argI.substr( 0, equalsPos );

            if ( key.empty() ) {
                cerr << "parameter \"" << argI << "\" must be named." << endl;

                exit( 1 );
            }

            valueString = argI.substr( equalsPos + 1 );
        }

        const ParameterInfoMap::iterator iMap =
            parameterInfoMap_.find( key );

        if ( iMap == parameterInfoMap_.end() ) {
            cerr << "unknown parameter \"" << key << "\"." << endl;

            exit( 1 );
        }

        if ( iMap->second.valueSpecified ) {
            cerr << "parameter \"" << key << "\" duplicated." << endl;

            exit( 1 );
        }

        iMap->second.valueString = valueString;
        iMap->second.valueSpecified = true;
    } // for (i)

    // set up the "extra" arg array
    if ( dashDashArgIndex == 0 ) {
        gExtraArgc = 1;
        gExtraArgv = gArgv;
    } else {
        // shift any "extra" arguments up to the front of argv
        const int numExtras = gArgc - (dashDashArgIndex + 1);

        for ( int i = 0; i < numExtras; ++i )
            gArgv[ 1 + i ] = gArgv[ (dashDashArgIndex + 1) + i ];

        gExtraArgc = 1 + numExtras;
        gExtraArgv = gArgv;
    }

    gArgc = 0;
    gArgv = 0;

    advanceState( COMMAND_LINE_PARSED_STATE );

    // Check if any mandatory parameters remain unspecified or if any
    // parameters were explicitely specified with a value of the magic
    // mandatory default value string.
    {
        bool haveFirstOne = false;

        ParameterInfoMap::const_iterator iMap =
            parameterInfoMap_.begin();

        const ParameterInfoMap::const_iterator iMapEnd =
            parameterInfoMap_.end();

        while ( iMap != iMapEnd ) {
            const bool mandatoryAndUnspecified =
                ((iMap->second.specifyType == MANDATORY_PARAM_SPECIFY_TYPE) &&
                 (iMap->second.valueSpecified == false));

            const bool specifiedBad =
                ((iMap->second.valueSpecified == true) &&
                 ((iMap->second.valueString == kMandatoryDefaultValueString) ||
                  (iMap->second.valueString == kNoDefaultValueString1) ||
                  (iMap->second.valueString == kNoDefaultValueString2)));

            if ( mandatoryAndUnspecified || specifiedBad ) {
                if ( haveFirstOne == false ) {
                    cerr << "Insufficient or invalid parameters for "
                         << progname_
                         << ":"
                         << endl;

                    haveFirstOne = true;
                }

                if ( mandatoryAndUnspecified ) {
                    cerr << "  mandatory parameter "
                         << quoteString( iMap->first )
                         << " did not have a value specified."
                         << endl;
                } else {
                    cerr << "  parameter "
                         << quoteString( iMap->first )
                         << " was explicitly specified to have an "
                         << "illegal value of "
                         << quoteString( iMap->second.valueString )
                         << "."
                         << endl;
                }
            }

            ++iMap;
        }

        if ( haveFirstOne ) {
            cerr << endl;

            exit( 1 );
        }
    }

    advanceState( INITIALISING_SYSTEM_KEYWORD_MEMBERS_STATE );

    const string umaskString = getStringParameter( "umask", true );

    mode_t newUmask = 0;
    const bool goodUmaskString =
        getUmaskForString( umaskString, newUmask );
        
    mode_t oldUmask = 0;

    if ( goodUmaskString )
        oldUmask = ::umask( newUmask );

    // set the command line based values for the system keywords
    daemonize_      = getBoolParameter( "daemonize", true );
    debugLevel_     = getIntParameter( "debug", true );
    logFileName_    = getStringParameter( "logfile", true );
    nice_           = getIntParameter( "nice", true );
    useDBMS_        = getBoolParameter( "useDBMS", true );
    useGMT_         = getBoolParameter("useGMT", true);

    // this code section has to match the technique used in Logger.cc
    // since 'localhost' and gethostname() can be different
    // and this will cause confusion
    logHostName_ = getMyHostname();

    const bool lognameParameterWasSpecified =
        parameterWasSpecified( "logname", true );

    string lognameParameterValue;

    if ( lognameParameterWasSpecified )
        lognameParameterValue = getStringParameter( "logname", true );

    syslog_     = getBoolParameter( "syslog", true );
    traceLevel_ = getIntParameter( "traceLevel", true );
    traceVerbose_ = getBoolParameter( "traceVerbose", true );
    traceFile_  = getStringParameter( "traceFile", true );
    corbaRrtt_  = getIntParameter( "corbaRrtt", true );

    if ( parameterWasSpecified( "imr", true ) ) {
        const string imrValue = getStringParameter( "imr", true );

        string lowercasedImrValue;
        lowercasedImrValue.resize( imrValue.size() );

        for ( string::size_type i = 0; i < imrValue.size(); ++i )
            lowercasedImrValue[ i ] = tolower( imrValue[ i ] );

        if ( lowercasedImrValue == "localhost" )
            imrHostnameSpecified_ = getMyHostname();
        else
            imrHostnameSpecified_ = imrValue;

        imrHostnameWasSpecified_ = true;
    } else {
        imrHostnameWasSpecified_ = false;
    }

    if (getBoolParameter( "redirectStdout", true )) {
        redirectFdToSyslog(STDOUT_FILENO, "stdout");
    }

    if (getBoolParameter( "redirectStderr", true )) {
        // assume that the user wants glibc error messages for stderr redirection
        forceGlibcErrorsToStderr();
        redirectFdToSyslog(STDERR_FILENO, "stderr");
    }

    advanceState( SYSTEM_KEYWORD_MEMBERS_INITIALISED_STATE );

    if ( lognameParameterWasSpecified )
        setInstanceLogname( lognameParameterValue );

    setLogger();

    advanceState( LOGGER_AVAILABLE_STATE );

    setTrace();

    advanceState( TRACE_OBJECT_AVAILABLE_STATE );
    
    advanceState( FULLY_INITIALISED_STATE );

    setCorba();

    if ( debugLevel_ > 4 )
        show();    // show many things

    if ( daemonize_ ) {
        if ( debugLevel_ > 0 )
            cout << "Now daemonizing " << getArg0() << endl;

        daemon( 1, 0 );  // assumes that programs want to work in the current
                         // working directory, but want to close cin, cout
                         // and cerr. WARNING: This is a BSD specific piece
                         // of code. Supported on Linux (for now) but not on
                         // Solaris. glibc call, so it can be made to work
                         // on Solaris.

        if ( debugLevel_ > 0 )
            cerr << "This daemonized line should never be seen....." << endl;
    }

    renice();
    
    if (useGMT_) {
        putenv(gTimeZoneEnvVariable);
        tzset();
    }


    getLogger() << Priority::NOTICE
                << "Program starting"
                << ", pid=" << getPid()
                << ", commandLine=[" << gArgsLogString << "]";

    if ( goodUmaskString != true ) {
        const string msg =
            "Bad umask parameter value \"" + umaskString + "\"";

        getLogger() << Priority::CRIT << msg;

        cerr << msg << endl;

        exit( 1 );
    } else if ( newUmask != oldUmask ) {
        getLogger() << Priority::INFO
                    << "Program umask was changed from "
                    << getStringForUmask( oldUmask ) << " to "
                    << getStringForUmask( newUmask );
    }

    return 0;
}


#ifndef NO_CORBA

bool
ProgramBase::orbInit(Orb* orb)
{
    verifyStateIsNormal();

    const int extraArgc = getExtraArgc();
    char **   extraArgv = getExtraArgv();

    // if we find -ORBDefaultInitRef in the arg list, ignore the imr keyword
    for ( int i = 0; i < extraArgc; ++i ) {
        if ( string( extraArgv[ i ] ) == "-ORBDefaultInitRef" ) {
            if ( imrHostnameWasSpecified_ ) 
                programLogWarnIfPossible( "Both the imr keyword and "
                    "-ORBDefaultInitRef extra arg were specified.  I am "
                    "assuming you are trying something special and going "
                    "with the -ORBDefaultInitRef arg.  I may throw on this "
                    "in the future." );
            return orb->set(extraArgc, extraArgv, "-");
        }
    }

    // if we don't find it, the imr keyword needs to be defined.  
    if ( imrHostnameWasSpecified_ == false )
        throw CARMA_ERROR( "carma::util::Program: imr keyword not defined" );

    return orb->set( extraArgc, extraArgv, imrHostnameSpecified_);
}

#endif  // #ifndef NO_CORBA

carma::corba::Server &
ProgramBase::getCorbaServer( ) 
{
    verifyStateIsNormal( );

    if ( corbaServer_.get( ) == 0 ) {
        std::string msg( "getCorbaServer called but corbaServer_ is NULL. " 
            "Try specifying imr=<hostname> on the command line." );
        throw CARMA_ERROR(  msg );
    }

    return *corbaServer_;
}
    
carma::corba::Client &
ProgramBase::getCorbaClient( ) 
{
    // For now just return the client owned by the server.
    return getCorbaServer().client();
}

bool
ProgramBase::imrTerminationRequested( )
{
    // Make sure we are in our normal running state.
    verifyStateIsNormal();

    if ( corbaServer_.get( ) == 0 ) {
        return false;
    } else { 
        if ( corbaServer_->terminated() )
            return true;
        else
            corbaServer_.get( )->work( ); // Service potential quit requests
        return corbaServer_->terminated( );
    }
}

string
ProgramBase::getUsageString( ) const
{
    verifyStateIsInRange( KEY_DEFINITIONS_ADDED_STATE,
                          BEING_DESTRUCTED_STATE );

    string result = kUsage_;

    if ( result == "@autogen" ) {
        ostringstream oss;

        oss << "Usage: " << progname_;

        const ParameterInfoMap::const_iterator iMapBegin =
            parameterInfoMap_.begin();

        const ParameterInfoMap::const_iterator iMapEnd =
            parameterInfoMap_.end();

        // first, show mandatory parameters
        {
            ParameterInfoMap::const_iterator iMap = iMapBegin;

            while ( iMap != iMapEnd ) {
                if ( iMap->second.specifyType == MANDATORY_PARAM_SPECIFY_TYPE ) {
                    oss << " " << iMap->first << "=";

                    switch ( iMap->second.type ) {
                        case BOOL_PARAM_TYPE:
                            oss << "true/false";
                            break;

                        default:
                            oss << "<value>";
                            break;
                    }
                }

                ++iMap;
            }
        }

        // second, show optional program parameters
        {
            ParameterInfoMap::const_iterator iMap = iMapBegin;

            while ( iMap != iMapEnd ) {
                if ( (iMap->second.specifyType != MANDATORY_PARAM_SPECIFY_TYPE) &&
                     (iMap->second.system == false) ) {
                    oss << " [" << iMap->first << "=";

                    switch ( iMap->second.type ) {
                        case BOOL_PARAM_TYPE:
                            oss << "true/false";
                            break;

                        default:
                            oss << "<value>";
                            break;
                    }

                    oss << "]";
                }

                ++iMap;
            }
        }

        // third, show optional system parameters
        {
            ParameterInfoMap::const_iterator iMap = iMapBegin;

            while ( iMap != iMapEnd ) {
                if ( (iMap->second.specifyType != MANDATORY_PARAM_SPECIFY_TYPE) &&
                     iMap->second.system ) {
                    oss << " [" << iMap->first << "=";

                    switch ( iMap->second.type ) {
                        case BOOL_PARAM_TYPE:
                            oss << "true/false";
                            break;

                        default:
                            oss << "<value>";
                            break;
                    }

                    oss << "]";
                }

                ++iMap;
            }
        }

        result = oss.str();
    }

    return result;
}


string
ProgramBase::getDescriptionString( ) const
{
    verifyStateIsInRange( KEY_DEFINITIONS_ADDED_STATE,
                          BEING_DESTRUCTED_STATE );

    string result = kDescription_;

    return result;
}


bool
ProgramBase::haveImrHostname( ) const
{
    verifyStateIsNormal();

    return imrHostnameWasSpecified_;
}


string
ProgramBase::getImrHostname( ) const
{
    verifyStateIsNormal();

    string result;

    if ( imrHostnameWasSpecified_ == false )
        emitErrorMessage( "fetching an imr host name that we don't have" );
    else
        result = imrHostnameSpecified_;

    return result;
}


int
ProgramBase::getTraceLevel( ) const
{
    verifyStateIsNormal();

    return traceLevel_;
}

int
ProgramBase::getNiceLevel( ) const
{
    verifyStateIsNormal();

    return nice_;
}


bool
ProgramBase::getUseDBMS( )
{
    Program & p = Program::getProgram();

    p.verifyStateIsNormal();

    return p.useDBMS_;
}


// private methods and functions


void
ProgramBase::show( )
{
    cout << "SHOW-begin" << endl;
    cout << "  Program: " << progname_ << endl;
    cout << "  Usage: " << getUsageString() << endl;
    cout << "  Version: " << prettyUpVersionString( kVersion_ ) << endl;
    cout << "  Description: " << endl << getDescriptionString() << endl;

    for ( int i = 1; i < gArgc; ++i )
        cout << "  arg(" << i << ") = " << gArgv[ i ] << endl;

    {
        ParameterInfoMap::const_iterator iMap =
            parameterInfoMap_.begin();

        const ParameterInfoMap::const_iterator iMapEnd =
            parameterInfoMap_.end();

        while ( iMap != iMapEnd ) {
            cout << "  key = " << iMap->first;

            if ( iMap->second.system )
                cout << " (**system key**)";

            cout << endl;

            cout << "    def = " << iMap->second.defaultValueString << endl;
            cout << "    help = " << iMap->second.help << endl;

            ++iMap;
        }
    }

    // really don't need these anymore if there is a good match
    // between system keywords and their getXparameter() call...
    cout << " daemonize:     " << (daemonize_ ? "true" : "false") << endl;
    cout << " debug level:   " << debugLevel_ << endl;
    cout << " trace level:   " << traceLevel_ << endl;
    cout << " syslog:        " << (syslog_ ? "true" : "false") << endl;
    cout << " log file name: " << logFileName_ << endl;
    cout << " log host name: " << logHostName_ << endl;

    {
        cout << " imr host name: ";

        if ( imrHostnameWasSpecified_ )
            cout << imrHostnameSpecified_;
        else
            cout << "< unspecified >";

        cout << endl;
    }

    cout << " use DBMS:      " << (useDBMS_ ? "true" : "false") << endl;

    cout << "SHOW-end" << endl;
}


ProgramBase::ParamType
ProgramBase::convertParameterType( const string & typeName ) {
    // first, look for our "preferred" type names

    if ( typeName == "bool" )
        return BOOL_PARAM_TYPE;

    if ( typeName == "double" )
        return DOUBLE_PARAM_TYPE;

    if ( (typeName == "int") || (typeName == "integer") )
        return INT_PARAM_TYPE;

    if ( typeName == "string" )
        return STRING_PARAM_TYPE;


    // second, look for our "terse" type names

    if ( typeName == "b" )
        return BOOL_PARAM_TYPE;

    if ( typeName == "d" )
        return DOUBLE_PARAM_TYPE;

    if ( typeName == "i" )
        return INT_PARAM_TYPE;

    if ( typeName == "s" )
        return STRING_PARAM_TYPE;


    // Houston, we have a problem

    {
        const string msg = string( "Unknown parameter type " ) +
                           quoteString( typeName );

        emitErrorMessage( msg );
    }

    return STRING_PARAM_TYPE;
}


void
ProgramBase::addKey( const KeyTabEntry & kt,
                     const bool          system,
                     const ::size_t      declIndex ) {
    if ( debugLevel_ > 0 )
        cout << "adding keyword " << quoteString( kt.key ) << "." << endl;

    const string key = kt.key;

    if ( parameterInfoMap_.find( key ) != parameterInfoMap_.end() ) {
        const string msg = string( "Duplicate keyword " ) + quoteString( key );

        emitErrorMessage( msg );

        return;
    }

    const string defaultValueString = kt.val;

    ParamType type = STRING_PARAM_TYPE;

    if ( kt.type == 0 ) {
        const string msg =
            string( "null parameter type pointer for keyword " ) +
            quoteString( key );

        emitErrorMessage( msg );

        type = STRING_PARAM_TYPE;
    } else
        type = convertParameterType( kt.type );


    ParameterInfo parameterInfo;

    parameterInfo.system = system;
    parameterInfo.declIndex = declIndex;

    parameterInfo.defaultValueString = defaultValueString;
    parameterInfo.type = type;
    parameterInfo.usageValue = kt.usageValue;
    parameterInfo.help = kt.help;

    if ( defaultValueString == kMandatoryDefaultValueString ) {
        parameterInfo.specifyType = MANDATORY_PARAM_SPECIFY_TYPE;
        parameterInfo.valueString = string();
    } else if ( (defaultValueString == kNoDefaultValueString1) ||
                (defaultValueString == kNoDefaultValueString2) ) {
        parameterInfo.specifyType = NO_DEFAULT_PARAM_SPECIFY_TYPE;
        parameterInfo.valueString = string();
    } else {
        parameterInfo.specifyType = NORMAL_PARAM_SPECIFY_TYPE;
        parameterInfo.valueString = defaultValueString;
    }

    parameterInfo.valueSpecified = false;
    parameterInfo.valueSpecifiedWasChecked = false;
    parameterInfo.valueWasRead = false;

    parameterInfoMap_[ key ] = parameterInfo;
}


string
ProgramBase::getParameterValueString( const string &  key,
                                      const bool      system,
                                      const ParamType type,
                                      const bool      checkType )
{
    const ParameterInfoMap::iterator iMap = parameterInfoMap_.find( key );

    if ( iMap == parameterInfoMap_.end() ) {
        const string msg = string( "Unknown keyword " ) + quoteString( key );

        tryToLogAndTraceErrorMessage( msg );
        emitErrorMessage( msg );

        return string();
    }

    if ( iMap->second.system != system ) {
        string msg;

        if ( iMap->second.system ) {
            msg = string( "Trying to access system keyword " ) +
                  quoteString( key );
                  string( " as though it was a program keyword" );
        } else {
            msg = string( "Trying to access program keyword " ) +
                  quoteString( key ) +
                  string( " as though it was a system keyword" );
        }

        tryToLogAndTraceErrorMessage( msg );
        emitErrorMessage( msg );
    }

    if ( checkType && (iMap->second.type != type) ) {
        const string msg = string( "Type mismatch with keyword " ) +
                           quoteString( key );

        tryToLogAndTraceErrorMessage( msg );
        emitErrorMessage( msg );
    }

    if ( iMap->second.specifyType == NO_DEFAULT_PARAM_SPECIFY_TYPE ) {
        if ( iMap->second.valueSpecifiedWasChecked == false ) {
            const string msg =
                string( "trying to get value of user-specified-only keyword " ) +
                quoteString( key ) +
                string( " without first checking if is has been specified" );

            tryToLogAndTraceErrorMessage( msg );
            emitErrorMessage( msg );
        }

        if ( iMap->second.valueSpecified == false ) {
            const string msg =
                string( "Trying to get value of user-specified-only keyword " ) +
                quoteString( key ) +
                string( " which the user did not specify on the command line" );

            tryToLogAndTraceErrorMessage( msg );
            emitErrorMessage( msg );
        }
    }

    iMap->second.valueWasRead = true;  // mark keyword as read

    return iMap->second.valueString;
}


bool
ProgramBase::parameterWasSpecified( const string & key,
                                    const bool     system )
{
    if ( system )
        verifyStateIsInitializingSystemKeywordMembers();
    else
        verifyStateIsNormal();

    const ParameterInfoMap::iterator iMap = parameterInfoMap_.find( key );

    if ( iMap == parameterInfoMap_.end() ) {
        const string msg = string( "Unknown keyword " ) + quoteString( key );

        emitErrorMessage( msg );

        return true;
    }

    if ( iMap->second.system != system ) {
        string msg;

        if ( iMap->second.system ) {
            msg = string( "Trying to access system keyword " ) +
                  quoteString( key ) +
                  string( " as though it was a program keyword" );
        } else {
            msg = string( "Trying to access program keyword " ) +
                  quoteString( key ) +
                  string( " as though it was a system keyword" );
        }

        emitErrorMessage( msg );
    }

    iMap->second.valueSpecifiedWasChecked = true;

    return iMap->second.valueSpecified;
}


bool
ProgramBase::getBoolParameter( const string & key,
                               const bool     system )
{
    if ( system )
        verifyStateIsInitializingSystemKeywordMembers();
    else
        verifyStateIsNormal();

    const string paramValueString =
        getParameterValueString( key, system, BOOL_PARAM_TYPE, true );

    string trimmedLower;

    {
        const string::size_type lastNonWhitespace =
            paramValueString.find_last_not_of( " \t" );

        if ( lastNonWhitespace != string::npos ) {
            trimmedLower =
                paramValueString.substr( 0, (lastNonWhitespace + 1) );
        }

        for ( ::size_t i = 0; i < trimmedLower.size(); ++i )
            trimmedLower[ i ] = tolower( trimmedLower[ i ] );
    }

    bool retVal = false;

    if ( (trimmedLower == "t") ||
         (trimmedLower == "true") ||
         (trimmedLower == "y") ||
         (trimmedLower == "yes") ||
         (trimmedLower == "1") )
        retVal = true;
    else if ( (trimmedLower == "f") ||
              (trimmedLower == "false") ||
              (trimmedLower == "n") ||
              (trimmedLower == "no") ||
              (trimmedLower == "0") )
        retVal = false;
    else {
        ostringstream oss;

        oss << "Problem converting keyword " << quoteString( key )
            << " value string " << quoteString( paramValueString )
            << " to a boolean (returning "
            << (retVal ? "true" : "false") << ")";

        emitErrorMessage( oss.str() );
    }

    return retVal;
}


double
ProgramBase::getDoubleParameter( const string & key,
                                 const bool     system )
{
    if ( system )
        verifyStateIsInitializingSystemKeywordMembers();
    else
        verifyStateIsNormal();

    const string paramValueString =
        getParameterValueString( key, system, DOUBLE_PARAM_TYPE, true );

    const string::size_type lastNonWhitespace =
        paramValueString.find_last_not_of( " \t" );

    const char * pvsBegin = paramValueString.c_str();
    const char * pvsEnd;

    if ( lastNonWhitespace == string::npos )
        pvsEnd = pvsBegin;
    else
        pvsEnd = pvsBegin + (lastNonWhitespace + 1);

    char * endPtr = 0;

    errno = 0;
    const double retVal = strtod( pvsBegin, &endPtr );
    const int errnoVal = errno;

    if ( (errnoVal != 0) || (endPtr != pvsEnd) ) {
        ostringstream oss;

        oss << "Problem converting keyword " << quoteString( key )
            << " value string " << quoteString( paramValueString )
            << " to a double (returning " << retVal << ")";

        emitErrorMessage( oss.str() );
    }

    return retVal;
}


int
ProgramBase::getIntParameter( const string & key,
                              const bool     system )
{
    if ( system )
        verifyStateIsInitializingSystemKeywordMembers();
    else
        verifyStateIsNormal();

    const string paramValueString =
        getParameterValueString( key, system, INT_PARAM_TYPE, true );

    const string::size_type lastNonWhitespace =
        paramValueString.find_last_not_of( " \t" );

    const char * pvsBegin = paramValueString.c_str();
    const char * pvsEnd;

    if ( lastNonWhitespace == string::npos )
        pvsEnd = pvsBegin;
    else
        pvsEnd = pvsBegin + (lastNonWhitespace + 1);

    char * endPtr = 0;
    long retVal = 0;
    int errnoVal = -1;

    if ( (pvsBegin != pvsEnd) &&
         (*pvsBegin == '0') &&
         ((pvsBegin + 1) != pvsEnd) &&
         ((*(pvsBegin + 1) == 'x') || (*(pvsBegin + 1) == 'X')) ) {
        // hex notation
        errno = 0;
        retVal = strtol( pvsBegin, &endPtr, 16 );
        errnoVal = errno;
    } else {
        // decimal notation
        errno = 0;
        retVal = strtol( pvsBegin, &endPtr, 10 );
        errnoVal = errno;
    }


    if ( (errnoVal != 0) || (endPtr != pvsEnd) ) {
        ostringstream oss;

        oss << "Problem converting keyword " << quoteString( key )
            << " value string " << quoteString( paramValueString )
            << " to an integer (returning " << retVal << ")";

        emitErrorMessage( oss.str() );
    }

    return retVal;
}


string
ProgramBase::getStringParameter( const string & key,
                                 const bool     system )
{
    if ( system )
        verifyStateIsInitializingSystemKeywordMembers();
    else
        verifyStateIsNormal();

    return getParameterValueString( key, system, STRING_PARAM_TYPE, true );
}


string
ProgramBase::getParameterRawValueString( const string & key,
                                         const bool     system )
{
    if ( system )
        verifyStateIsInitializingSystemKeywordMembers();
    else
        verifyStateIsNormal();

    return getParameterValueString( key, system, STRING_PARAM_TYPE, false );
}


bool
ProgramBase::parameterWasSpecified( const string & key )
{
    return parameterWasSpecified( key, false );
}


bool
ProgramBase::getBoolParameter( const string & key )
{
    return getBoolParameter( key, false );
}


double
ProgramBase::getDoubleParameter( const string & key )
{
    return getDoubleParameter( key, false );
}


int
ProgramBase::getIntParameter( const string & key )
{
    return getIntParameter( key, false );
}


string
ProgramBase::getStringParameter( const string & key )
{
    return getStringParameter( key, false );
}


string
ProgramBase::getParameterRawValueString( const string & key )
{
    return getParameterRawValueString( key, false );
}


int
ProgramBase::getDebugLevel( ) const
{
    verifyStateIsNormal();

    return debugLevel_;
}


bool
ProgramBase::DebugLevel( const int level ) const
{
    verifyStateIsNormal();

    return (level >= debugLevel_);
}


string
ProgramBase::getLogHostname( ) const
{
    verifyStateIsInRange( SYSTEM_KEYWORD_MEMBERS_INITIALISED_STATE,
                          BEING_DESTRUCTED_STATE );

    return logHostName_;
}

string
ProgramBase::getLogname( ) const
{
    verifyStateIsNormal();

    return logname_;
}


int
ProgramBase::getExtraArgc( )
{
    if ( gExtraArgv == 0 ) {
        emitErrorMessage( "Bad gExtraArgv state" );
    }

    return gExtraArgc;
}


char **
ProgramBase::getExtraArgv( )
{
    if ( gExtraArgv == 0 ) {
        emitErrorMessage( "Bad gExtraArgv state" );
    }

    return gExtraArgv;
}


string
ProgramBase::getArg0( )
{
    if ( gArg0 == 0 ) {
        emitErrorMessage( "Bad gArg0 state" );

        return "< unknown >";
    } else
        return gArg0;
}


::pid_t
ProgramBase::getPid( )
{
    if ( gPid == 0 )
        gPid = ::getpid();

    return gPid;
}


string
ProgramBase::getCwd( )
{
    char cwd[PATH_MAX + 1];
    string result;

    if ( getcwd( cwd, PATH_MAX ) != cwd ) {
        emitErrorMessage( "Error retrieving current working directory" );
        result = "< unknown >";
    } else {
        cwd[PATH_MAX] = '\0'; // Safety net null
        result = cwd;
    }

    return result;
}


string
ProgramBase::getHostname( const bool shorten )
{
    const string hostname = getMyHostname();

    if ( shorten )
        return hostname.substr( 0, hostname.find('.') );
    else
        return hostname;
}


string
ProgramBase::getConfDir()
{
    return RuntimeDirs::getConfDir(getArg0());
}


string
ProgramBase::getConfFile( const string & filename )
{
    return RuntimeDirs::getConfFile(getArg0(), filename);
}


string
ProgramBase::getRootDir( )
{
    return RuntimeDirs::getRootDir(getArg0());
}


string
ProgramBase::getExecutableDir( )
{
    return RuntimeDirs::getExecutableDir(getArg0());
}


string
ProgramBase::getExecutable( )
{
    return RuntimeDirs::getExecutable(getArg0());
}



Program::Program( )
{
}


Program::~Program( )
try {
} catch ( ... ) {
    // just stifle the exception

    return;
}


Program &
Program::getProgram( )
{
    if ( gSingletonState == NOT_CONSTRUCTED_SINGLETON_STATE )  {
        gSingletonState = BEING_CONSTRUCTED_SINGLETON_STATE;

        try {
            gSingleton = new Program;
        } catch ( ... ) {
            gSingletonState = NOT_CONSTRUCTED_SINGLETON_STATE;

            throw;
        }

        gSingletonState = CONSTRUCTED_SINGLETON_STATE;
    }

    if ( gSingletonState >= BEING_DESTRUCTED_SINGLETON_STATE ) {
        ostringstream oss;

        oss << "Someone is trying to get the Program singleton too late "
            << "(singleton state is ";

        switch ( gSingletonState ) {
            case BEING_DESTRUCTED_SINGLETON_STATE:
                oss << "BEING_DESTRUCTED_SINGLETON_STATE";
                break;

            case DESTRUCTED_SINGLETON_STATE:
                oss << "DESTRUCTED_SINGLETON_STATE";
                break;

            default:
                oss << "< unknown >";
                break;
        }

        oss << ")";

        emitErrorMessage( oss.str() );
    }

    if ( gSingleton == 0 )
        throw runtime_error( "Program singleton pointer is null" );

    return *gSingleton;
}
