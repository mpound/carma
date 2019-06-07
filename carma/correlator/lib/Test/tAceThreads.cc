// $Id: tAceThreads.cc,v 1.5 2012/02/15 21:59:14 abeard Exp $
//
// tAceThreads.cc
//

// ACE classes
#include "ace/ARGV.h"
#include "ace/Log_Msg.h"
#include "ace/Reactor.h"
#include "ace/Event_Handler.h"
#include "ace/Task.h"
#include "ace/Signal.h"

// CARMA classes
#include "carma/corba/corba.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/ConfigChecker.h"

using namespace std;
using namespace carma::util;
using namespace log4cpp;

/**
 * @description
 * The following application demonstrates the use of an
 * ACE_Task with threads, and a main thread that blocks
 * waiting for shutdown notification.
 *
 * If the test is started from the command line, then ctrl-C
 * generates a SIGINT and the main thread unblocks and
 * stops the ACE_Task thread. 
 * 
 * @usage Usage: tAceThreads
 *
 * @noKeys
 *
 * @logger DEFAULT_FACILITY carma.correlator.lib.test.ace
 */

#define LOG(msg) do {ACE_DEBUG((LM_DEBUG, msg)); log_ << log4cpp::Priority::INFO << msg; } while(0);

class ShutdownHandler : 
	public ACE_Event_Handler {
	public:
		typedef ACE_Event_Handler parent;
	
		ShutdownHandler(
			ACE_Reactor        *r,
			log4cpp::Category& log);
		
		~ShutdownHandler();
		
		// Signal event
		virtual int handle_signal(
			int signum, siginfo_t *, ucontext_t *);
		virtual int handle_exception(
			ACE_HANDLE handle);

	private:
	    log4cpp::Category& log_;
};

ShutdownHandler::
ShutdownHandler(
	ACE_Reactor *r,
	log4cpp::Category& log) :
	parent(r),
	log_(log)
{
	LOG("ShutdownHandler: constructor\n");
}

ShutdownHandler::
~ShutdownHandler()
{
	LOG("ShutdownHandler: destructor\n");
	reactor(0);
}

int 
ShutdownHandler::
handle_signal(int signum, siginfo_t *, ucontext_t *)
{
	switch (signum) {
		case SIGINT:
			LOG("ShutdownHandler: SIGINT received.\n");
			break;
		case SIGHUP:
			LOG("ShutdownHandler: SIGHUP received.\n");
			break;
		default:	
			LOG("ShutdownHandler: signal received.\n");
			break;
	}
			
	if (reactor()) {
		// Schedule handle_exception()
		if (!reactor()->reactor_event_loop_done()) {
			LOG("ShutdownHandler: notify\n");
			reactor()->notify(this);
		}
	} else {
		LOG("ShutdownHandler: ERROR reactor is null!\n");
	}
	return 0;
}

int
ShutdownHandler::
handle_exception(
	ACE_HANDLE handle)
{
	LOG("ShutdownHandler: handle_exception()\n");

	// End the reactor event loop at non-signal context
	if (!reactor()->reactor_event_loop_done()) {
		LOG("ShutdownHandler: end reactor event loop.\n");
		reactor()->end_reactor_event_loop();
	} else {
		LOG("ShutdownHandler: reactor is done\n");
	}
	return 0;
}



class Test : public ACE_Task<ACE_MT_SYNCH> {
	public:
		Test(log4cpp::Category& log);
		~Test();

		// ACE_Task members
		virtual int open(void *args = 0);
		virtual int close(u_long flags = 0);
		virtual int svc(void);
	private:
		sig_atomic_t done_;
	    log4cpp::Category& log_;
};

Test::
Test(log4cpp::Category& log) :
	done_(0),
	log_(log)
{
	LOG("Test: constructor\n");
}

Test::
~Test()
{
	LOG("Test: destructor\n");
}
int 
Test::
open(void *args)
{
	LOG("Test: open()\n");
	// Create a single, joinable thread
	return activate();
}

int 
Test::
close(u_long flags)
{
	LOG("Test: close\n");
	if (flags != 0) {
		done_ = 1;
		LOG("Test: wait for thread\n");
		int status = this->wait();
		if (status < 0) {
			LOG("Test: thread wait failed\n");
			return -1;
		}
		LOG("Test: thread joined\n");
	}
	return 0;
}

int 
Test::
svc(void)
{
	LOG("Test: svc()\n");
	while (!done_) {
		LOG("Test: work ...\n");
		ACE_OS::sleep(1);
	}
	return 0;
}



int Program::main()
{
  try {
    
	// Logging
	log4cpp::Category& log_ = getLogger();

	// Clear SIGINT+SIGHUP, and all tasks will inherit this mask
	ACE_Sig_Set shutdown_sigset;
	shutdown_sigset.sig_add(SIGINT);
	shutdown_sigset.sig_add(SIGHUP);
	ACE_OS::sigprocmask(SIG_BLOCK, shutdown_sigset, 0);
	
	// Signal handler
	ACE_Reactor *reactor = ACE_Reactor::instance();
	ShutdownHandler shutdown(reactor, log_);

	// Register the handler for SIGINT and SIGHUP
	reactor->register_handler(shutdown_sigset, &shutdown);

	// Thread test
	LOG("Main: test create\n");
	Test test(log_);
	LOG("Main: test open()\n");
	int status = test.open();
	if (status < 0) {
		LOG("Main: test open() failed\n");
		return -1;
	}
	LOG("Main: test open() succeeded\n");

	// Unblock SIGINT/HUP
	LOG("Main: unblock signals\n");
	ACE_OS::sigprocmask(SIG_UNBLOCK, shutdown_sigset, 0);
	
	// Run until SIGINT/HUP
	LOG("Main: run the reactor\n");
	errno = 0;
	status = reactor->run_reactor_event_loop();	
	LOG("Main: reactor ended\n");
	if (status < 0) {
		LOG("Main: reactor error\n");
	}
	
	LOG("Main: test close()\n");
	status = test.close(1);
	if (status < 0) {
		LOG("Main: test close() failed\n");
		return -1;
	}
	LOG("Main: test close() succeeded\n");

	// Remove the signal handler
	status = reactor->remove_handler(shutdown_sigset);
	if (status < 0) {
		LOG("Main: SIGINT handler remove failed - %m\n");
	}
	LOG("Main: ending\n");

  
  } catch (...) {
    std::cerr << getStringForCaught() << endl;
    return 1;
  }
      
  return 0;
}
