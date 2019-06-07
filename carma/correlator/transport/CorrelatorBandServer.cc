// $Id: CorrelatorBandServer.cc,v 1.30 2014/06/20 15:53:17 mpound Exp $
//
// CorrelatorBandServer.cc
//
// An IMR-startable Correlator band server
//

// CARMA classes
#include "carma/corba/corba.h"
#include "carma/corba/Server.h"
#include "carma/util/ConfigChecker.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"

#include "carma/correlator/obsRecord2/aceUtils.h"

// ACE classes
#include <ace/ARGV.h>
#include <ace/Log_Msg.h>
#include <ace/Signal.h>
#include <ace/Event_Handler.h>
#include <ace/Reactor.h>

// Correlator classes
#include <cobra/CorrelatorBandServer.h>


// standard classes
#include <sstream>
#include <memory>

using namespace ::std;
using namespace ::log4cpp;
using namespace cobra;
using namespace carma;
using namespace carma::correlator;
using namespace carma::correlator::obsRecord2;
using namespace carma::util;

/**
 * @description
 *  Correlator band server.
 *  The correlator band server is the correlator hardware server.
 *  It provides the control interface, data interface, and
 *  monitor interface to the correlator hardware. The server
 *  is started with a configuration file containing the specific
 *  configuration of hardware (eg. CARMA or SZA).
 *
 * @usage Usage: CorrelatorBandServer
 *
 * @key i @mandatory              string  configuration ini-file
 * @key s CARMA                   string  configuration system key
 * @key m 500MHz                  string  configuration mode key
 * @key a -1                      int     array number
 * @key b -1                      int     band number
 * @key v 0                       int     verbose logging level (0, 1, or 2)
 * @key y false                   bool    ACE logging to syslog. Note this will override the logfile keyword for ACE messages.
 * @key z false                   bool    daemonize
 * @key redirectAceLogging false  bool    Whether to redirect ACE logging
 * @key simulate false            bool    pass --simulate to cobra::CorrelatorBandServer
 * @key nodata false              bool    pass -D (no-data) flag to cobra::CorrelatorBandServer
 * @key nomonitor false           bool    pass -M (no-monitor-data) flag to cobra::CorrelatorBandServer
 * @key delay     true            bool    pass -d (apply delay correction) flag to cobra::CorrelatorBandServer
 *
 * @logger DEFAULT_FACILITY carma.correlator.transport.bandserver
 */

// ----------------------------------------------------------------
// SIGINT/SIGHUP signal handler
// ----------------------------------------------------------------

class ShutdownHandler :
    public ACE_Event_Handler {
    public:
        typedef ACE_Event_Handler parent;

        ShutdownHandler(ACE_Reactor *r = ACE_Reactor::instance());
        ~ShutdownHandler();

        // Signal event
        virtual int handle_signal(
            int signum, siginfo_t *, ucontext_t *);
        virtual int handle_exception(
            ACE_HANDLE handle);
        virtual int handle_timeout(
            const ACE_Time_Value &tv, const void *);

};

ShutdownHandler::
ShutdownHandler(ACE_Reactor *r) :
    parent(r)
{
    ACE_DEBUG((LM_DEBUG, "ShutdownHandler: constructor\n"));
}

ShutdownHandler::
~ShutdownHandler()
try {
    ACE_DEBUG((LM_DEBUG, "ShutdownHandler: destructor\n"));
    reactor(0);
} catch ( ... ) {
    // Just stifle any exeptions

    return;
}

int
ShutdownHandler::
handle_signal(int signum, siginfo_t *, ucontext_t *)
{
    switch (signum) {
        case SIGINT:
            ACE_ERROR((LM_ERROR, "ShutdownHandler: SIGINT received.\n"));
            break;
        case SIGHUP:
            ACE_ERROR((LM_ERROR, "ShutdownHandler: SIGHUP received.\n"));
            break;
        default:
            ACE_ERROR((LM_ERROR, "ShutdownHandler: signal received.\n"));
            break;
    }

    if (reactor()) {
        // Schedule handle_exception()
        if (!reactor()->reactor_event_loop_done()) {
            ACE_DEBUG((LM_DEBUG, "ShutdownHandler: notify\n"));
            reactor()->notify(this);
        }
    } else {
        ACE_DEBUG((LM_DEBUG, "ShutdownHandler: ERROR reactor is null!\n"));
    }
    return 0;
}

int
ShutdownHandler::
handle_exception(
    ACE_HANDLE handle)
{
    ACE_DEBUG((LM_DEBUG, "ShutdownHandler: handle_exception()\n"));

    // End the reactor event loop at non-signal context
    if (!reactor()->reactor_event_loop_done()) {
        ACE_DEBUG((LM_DEBUG, "ShutdownHandler: end reactor event loop.\n"));
        reactor()->end_reactor_event_loop();
    } else {
        ACE_DEBUG((LM_DEBUG, "ShutdownHandler: reactor is done\n"));
    }
    return 0;
}

int ShutdownHandler::handle_timeout(const ACE_Time_Value &tv, const void *arg)
{
    carma::util::Program &program = carma::util::Program::getProgram();

    // If the IMR has not requested that we shut down, return
    if (!program.imrTerminationRequested())
        return 0;

    // End the reactor event loop at non-signal context
    if (!reactor()->reactor_event_loop_done()) {
        ACE_DEBUG((LM_DEBUG, "ShutdownHandler: end reactor event loop.\n"));
        reactor()->end_reactor_event_loop();
    } else {
        ACE_DEBUG((LM_DEBUG, "ShutdownHandler: reactor is done\n"));
    }

    return 0;
}

// No-op signal handler
namespace {
    extern "C" void sigpipe_handler (int /* signum */) { /* nop */ }
}

// ----------------------------------------------------------------
// Application
// ----------------------------------------------------------------

int
Program::main( )
{
  try {
    const char *fn = "CorrelatorBandServer::main";
    ScopedLogNdc ndc(fn);
    // Argument keys
    const int arrayNumber = getIntParameter("a");
    const int bandNumber = getIntParameter("b");

    if (arrayNumber < 0 && bandNumber < 0) {
      programLogErrorIfPossible("Option a=<ARRAY> and/or b=<BAND> is mandatory; neither found");
      return EXIT_FAILURE;  // or throw "bad argument"
    }

    {
        ostringstream oss;

        oss << getLogname() << (bandNumber < 0 ? arrayNumber : bandNumber);

        setInstanceLogname( oss.str() );
    }

    const string inifile = getConfFile(getStringParameter("i"));
    const string system  = getStringParameter("s");
    const string mode    = getStringParameter("m");
    const int  verbose   = getIntParameter("v");
    const bool syslog    = getBoolParameter("y");
    const bool daemonize = getBoolParameter("z");
    const bool redirectAceLogging = getBoolParameter( "redirectAceLogging" );
    const bool nodata    = getBoolParameter("nodata");
    const bool nomonitor = getBoolParameter("nomonitor");
    const bool simulate  = getBoolParameter("simulate");
    const bool delay     = getBoolParameter("delay");

    if ( redirectAceLogging )
        installAceLoggingBackend();

    // CARMA logger

    programLogNoticeIfPossible( "starting. config file= " + inifile );

    // Convert keywords to a single argument string
    string cobraCommandLine;
    {
        ostringstream oss;
        oss << "CorrelatorBandServer"
            << " -i " << inifile
            << " -s " << system
            << " -m " << mode;
      if (arrayNumber >= 0) {
        oss << " -a " << arrayNumber;
      }
      if (bandNumber >= 0) {
        oss << " -b " << bandNumber;
      }

        if ( verbose > 0) // use two v's for debug level logging
            oss << (verbose == 1 ? " -v" : " -vv");

        if ( daemonize )
            oss << " -z";

        if ( syslog )
            oss << " --syslog";

        if ( nodata )
                oss << " -D";

        if ( nomonitor )
                oss << " -M";

        if ( simulate )
                oss << " --simulate";

        if ( delay )
            oss << " -d "; // process delays

        cobraCommandLine = oss.str();

    }

    programLogNoticeIfPossible(
        "cobra command line=\"" + cobraCommandLine + "\"" );

    ACE_ERROR((LM_ERROR, "Main: correlator band server starting\n"));
    ACE_ARGV cobraArgs( cobraCommandLine.c_str(), 0 );

    // Handle (ignore) SIGPIPE from disconnected clients
    ACE_Sig_Action no_sigpipe((ACE_SignalHandler)sigpipe_handler, SIGPIPE);

    // Clear SIGINT+SIGHUP, and all tasks will inherit this mask
    ACE_Sig_Set shutdown_sigset;
    shutdown_sigset.sig_add(SIGTERM);
    shutdown_sigset.sig_add(SIGINT);
    shutdown_sigset.sig_add(SIGHUP);
    ACE_OS::sigprocmask(SIG_BLOCK, shutdown_sigset, 0);

    // Note: the handler and server are dynamically allocated so
    // the their destruction can be triggered prior to main
    // exiting message (for debugging). Their destruction
    // can occur after that point without issue, it just
    // looks nicer to have the messages before the exiting message.

    // Band server
    ACE_DEBUG((LM_DEBUG, "Main: start the server\n"));
    auto_ptr<CorrelatorBandServer> server(new CorrelatorBandServer);
    if (server.get() == 0) {
        ACE_ERROR((LM_ERROR, "Main: server allocation failed\n"));
        programLogCriticalIfPossible( "server allocation failed" );
        return EXIT_FAILURE;
    }

    int status = server->init( cobraArgs.argc(), cobraArgs.argv() );
    if (status < 0) {
        ACE_ERROR((LM_ERROR, "Main: server init failed\n"));
        programLogCriticalIfPossible( "server init failed" );
        return EXIT_FAILURE;
    }

    // Signal handler
    //
    // Note: this is created after the server, since the server
    // may daemonize, closing all open file handles, including
    // the signal handler notification pipe.
    //
    ACE_Reactor *reactor = ACE_Reactor::instance();
    auto_ptr<ShutdownHandler> shutdown(new ShutdownHandler(reactor));
    if (shutdown.get() == 0) {
        ACE_ERROR((LM_ERROR, "Main: handler allocation failed\n"));
        programLogCriticalIfPossible( "handler allocation failed" );
        return EXIT_FAILURE;
    }

    // Register the handler for SIGINT and SIGHUP
    reactor->register_handler(shutdown_sigset, shutdown.get());

    // Unblock SIGINT/HUP
    ACE_DEBUG((LM_DEBUG, "Main: unblock signals\n"));
    ACE_OS::sigprocmask(SIG_UNBLOCK, shutdown_sigset, 0);

    // Register a periodic timer to check for CORBA termination
    ACE_Time_Value period(0, 250);
    const long timerid = reactor->schedule_timer(shutdown.get(), 0, period, period);

    // Run until SIGINT/HUP
    ACE_DEBUG((LM_DEBUG, "Main: run the reactor\n"));
    errno = 0;
    status = reactor->run_reactor_event_loop();
    ACE_DEBUG((LM_DEBUG, "Main: reactor ended\n"));
    if (status < 0) {
        ACE_ERROR((LM_ERROR, "Main: errno %d - %m\n", errno));
    }

    // Close the server before removing the signal handler
    // so that ctrl-C from the command line will be caught
    // and ignored until threads have joined.

    // Close the server
    ACE_DEBUG((LM_DEBUG, "Main: stop the server threads\n"));
    status = server->fini();
    if (status < 0) {
        ACE_ERROR((LM_ERROR, "Main: server fini failed\n"));
        programLogCriticalIfPossible( "server fini failed" );
        return EXIT_FAILURE;
    }
    ACE_DEBUG((LM_DEBUG, "Main: destroy the server\n"));
    server.reset(0);

    // Remove the periodic timer
    status = reactor->cancel_timer(timerid);
    if (status < 0) {
        ACE_ERROR((LM_ERROR, "Main: cancel timer failed - %m\n"));
    }

    // Remove the signal handler
    ACE_DEBUG((LM_DEBUG, "Main: remove the signal handler\n"));
    status = reactor->remove_handler(shutdown_sigset);
    if (status < 0) {
        ACE_ERROR((LM_ERROR,
            "Main: signal handler remove failed - %m\n"));
    }
    ACE_DEBUG((LM_DEBUG, "Main: destroy the handler\n"));
    shutdown.reset(0);

    // Log exit (even when not in verbose mode)
    ACE_ERROR((LM_ERROR, "Main: correlator band server exiting\n"));
    programLogInfoIfPossible( "ending" );

  } catch ( ... ) {

    ostringstream oss;

    oss << "Exception caught: " << getStringForCaught();

    programLogCriticalIfPossible( oss.str() );

    cerr << oss.str() << endl;

    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
