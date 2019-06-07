// $Id: CorrelatorBandTimeChecker.cc,v 1.13 2012/02/16 21:36:22 abeard Exp $
//
// CorrelatorBandTimeChecker.cc
//
// An IMR-startable Correlator band time checker
// 

#include "carma/corba/corba.h"
#include "carma/correlator/obsRecord2/aceUtils.h"
#include "carma/util/ConfigChecker.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"

// ACE classes
#include <ace/ARGV.h>
#include <ace/Log_Msg.h>
#include <ace/Signal.h>
#include <ace/Event_Handler.h>
#include <ace/Reactor.h>
//#include <cobra/CorrelatorBandTimeChecker.h>
#include <cobra/CorrelatorBandMonitorChecker.h>

#include <sstream>

using namespace ::std;
using namespace ::log4cpp;
using namespace cobra;
using namespace carma;
using namespace carma::correlator;
using namespace carma::correlator::obsRecord2;
using namespace carma::util;

/**
 * @description
 *  Correlator band time checker.
 *  A correlator control client that contacts a band control
 *  server and monitors the DSP time.
 *
 * @usage Usage: CorrelatorBandTimeChecker
 *
 * @key i @mandatory              string  configuration ini-file
 * @key s CARMA                   string  configuration system key
 * @key m 500MHz                  string  configuration mode key
 * @key b @mandatory              int     band number
 * @key v false                   bool    verbose logging
 * @key z false                   bool    daemonize
 * @key redirectAceLogging false  bool    Whether to redirect ACE logging
 *
 * @logger DEFAULT_FACILITY carma.correlator.transport.bandchecker
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
    // Just stifle any exceptions
    
    return;
}

int 
ShutdownHandler::
handle_signal(int signum, siginfo_t *, ucontext_t *)
{
    switch (signum) {
        case SIGINT:
            ACE_DEBUG((LM_DEBUG, "ShutdownHandler: SIGINT received.\n"));
            break;
        case SIGHUP:
            ACE_DEBUG((LM_DEBUG, "ShutdownHandler: SIGHUP received.\n"));
            break;
        default:    
            ACE_DEBUG((LM_DEBUG, "ShutdownHandler: signal received.\n"));
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

// ----------------------------------------------------------------
// Application
// ----------------------------------------------------------------

int
Program::main( )
{
  try {
    // Argument keys
    const string inifile = getConfFile(getStringParameter("i"));
    const string system  = getStringParameter("s");
    const string mode    = getStringParameter("m");
    const int bandNumber = getIntParameter("b");
    const bool verbose   = getBoolParameter("v");
    bool daemonize       = getBoolParameter("z");
    const bool redirectAceLogging = getBoolParameter( "redirectAceLogging" );

    if ( redirectAceLogging )
        installAceLoggingBackend();

    // CARMA logger
    ACE_ERROR((LM_ERROR, "Main: correlator band time checker starting\n"));
    log4cpp::Category& log = getLogger();
    log << log4cpp::Priority::INFO 
            << "CorrelatorBandTimeChecker starting\n";

    // Convert keywords to a single argument string
    string cobraCommandLine;
    {
        ostringstream oss;
        
        oss << " -i " << inifile
            << " -s " << system
            << " -m " << mode
            << " -b " << bandNumber;

        if ( verbose )
            oss << " -v";

        if ( daemonize )
            oss << " -z";
        
        cobraCommandLine = oss.str();
        
        programLogInfoIfPossible(
            "cobra command line=\"" + cobraCommandLine + "\"" );
    }
    
    ACE_ARGV cobraArgs( cobraCommandLine.c_str(), 0 );

    // Clear SIGINT+SIGHUP, and all tasks will inherit this mask
    ACE_Sig_Set shutdown_sigset;
    shutdown_sigset.sig_add(SIGINT);
    shutdown_sigset.sig_add(SIGHUP);
    ACE_OS::sigprocmask(SIG_BLOCK, shutdown_sigset, 0);

    // Note: the handler and task are dynamically allocated so
    // the their destruction can be triggered prior to main
    // exiting message (for debugging). Their destruction
    // can occur after that point without issue, it just
    // looks nicer to have the messages before the exiting message.
    
    // Start the time checker task
//  auto_ptr<cobra::TimeCheckTask> checker(new cobra::TimeCheckTask);
    auto_ptr<cobra::MonitorChecker> checker(new cobra::MonitorChecker);
    if (checker.get() == 0) {
        ACE_ERROR((LM_ERROR, "Main: checker allocation failed\n"));
	programLogCriticalIfPossible( "Main: checker allocation failed");
        return EXIT_FAILURE;
    }
    int status = checker->init( cobraArgs.argc(), cobraArgs.argv() );
    if (status < 0) {
        ACE_ERROR((LM_ERROR, "Main: checker init failed\n"));
	programLogCriticalIfPossible( "Main: checker init failed");
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
        programLogCriticalIfPossible("Main: handler allocation failed");
        return EXIT_FAILURE;
    }

    // Register the handler for SIGINT and SIGHUP
    reactor->register_handler(shutdown_sigset, shutdown.get());
    
    // Unblock SIGINT/HUP
    ACE_DEBUG((LM_DEBUG, "Main: unblock signals\n"));
    ACE_OS::sigprocmask(SIG_UNBLOCK, shutdown_sigset, 0);
    
    // Run until SIGINT/HUP
    ACE_DEBUG((LM_DEBUG, "Main: run the reactor\n"));
    errno = 0;
    status = reactor->run_reactor_event_loop(); 
    ACE_DEBUG((LM_DEBUG, "Main: reactor ended\n"));
    if (status < 0) {
        ACE_ERROR((LM_ERROR, "Main: errno %d - %m\n", errno));
    }

    // Close the task before removing the signal handler
    // so that ctrl-C from the command line will be caught
    // and ignored until threads have joined.
    
    // Close the task
    ACE_DEBUG((LM_DEBUG, "Main: stop the checker task\n"));
    status = checker->fini();
    if (status < 0) {
        ACE_ERROR((LM_ERROR, "Main: checker fini failed\n"));
        programLogCriticalIfPossible("Main: checker fini failed");
        return EXIT_FAILURE;
    }
    ACE_DEBUG((LM_DEBUG, "Main: destroy the checker\n"));
    checker.reset(0);
    
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
    ACE_ERROR((LM_ERROR, "Main: correlator band time checker ending\n"));
    log << log4cpp::Priority::INFO 
            << "CorrelatorBandTimeChecker ending\n";
    
  } catch (...) {
    std::cerr << "Exception caught:" << getStringForCaught() << endl;
    return 1;
  }
      
  return 0;
}
