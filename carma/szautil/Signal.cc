#include <stdio.h>
#include <stdlib.h>
#include <iostream>

#include "carma/szautil/Exception.h"
#include "carma/szautil/Signal.h"

using namespace sza::util;
using namespace std;

/**.......................................................................
 * Initialize the static members here.
 */
Signal* Signal::signal_ = 0;

/**.......................................................................
 * Constructor with no sigmask or handler.
 */
Signal::Signal(int sigNo)
{
  privateConstructor(sigNo, NULL, NULL, NULL);
}

/**.......................................................................
 * Handler constructor with no sigmask.
 */
Signal::Signal(int sigNo, SIG_HANDLER(*handler))
{
  privateConstructor(sigNo, handler, NULL, NULL);
}

/**.......................................................................
 * Handler constructor with sigmask.
 */
Signal::Signal(int sigNo, SIG_HANDLER(*handler), sigset_t* sigSet)
{
  privateConstructor(sigNo, handler, NULL, sigSet);
}

/**.......................................................................
 * Action constructor with no sigmask.
 */
Signal::Signal(int sigNo, SIG_ACTION(*action))
{
  privateConstructor(sigNo, NULL, action, NULL);
}

/**.......................................................................
 * Action constructor with sigmask.
 */
Signal::Signal(int sigNo, SIG_ACTION(*action), sigset_t* sigSet)
{
  privateConstructor(sigNo, NULL, action, sigSet);
}

/**.......................................................................
 * The constructor that gets called under the hood by all of the above.
 */
void Signal::privateConstructor(int sigNo, SIG_HANDLER(*handler), 
				SIG_ACTION(*action), sigset_t* sigSet)
{
  // Set the static pointer pointing to ourselves.

  signal_ = this;

  // Make a copy of the signal mask

  if(sigSet == 0)
    sigSetIsEmpty = true;
  else {
    sigSetIsEmpty = false;
    sigSet_ = *sigSet;
  }

  setUpSigAction();
}

/**.......................................................................
 * Install the signal handler.
 */
void Signal::setUpSigAction()
{
  // Now construct the mask of signals to block in our signal handler.
  
  struct sigaction sat;
  
  // First, empty the signal mask.
  
  sigemptyset(&sat.sa_mask);
  
  // Add in the passed set, if the set was non-NULL.  SIGRTMAX is, as
  // far as I know, the highest allowable signal.
  
  if(!sigSetIsEmpty)
    for(unsigned isig=1; isig <= SIGRTMAX; isig++)
      if(sigismember(&sigSet_, isig))
	sigaddset(&sat.sa_mask, isig);
	 
  // Setting the SA_SIGINFO flag causes extra information to be passed
  // to the signal handler.  If we want to use this information, we
  // should install out signal handler using the sa_sigaction field of
  // the sigaction struct, instead of the sa_handler field, and our
  // signal handler takes three arguments instead of one.  
  //
  // Stevens 1993 (p. 283) says that the AT&T standard claimed that
  // SA_SIGINFO caused signals to be reliably queued if multiple
  // signals were delivered while those signals were blocked, but that
  // it isn't true.  Apparently if multiple signals are delivered
  // while that signal is blocked, the signal is delivered only once.

  sat.sa_flags = SA_SIGINFO;

  if(userhandler_ != 0 || useraction_ != 0) {

    // Set the action to take on delivery of this signal.

    if(userhandler_ != 0)
      sat.sa_handler = userhandler_;
    else if(useraction_ != 0)
      sat.sa_sigaction = useraction_;

    // Now install the signal handler.
    
    if(sigaction(sigNo_, &sat, NULL) != 0)
      throw Error("Signal::Signal: Error in sigaction().\n");
  }
}

/**.......................................................................
 * Destructor
 */
Signal::~Signal() {}


/**.......................................................................
 * Install a handler.
 */
void Signal::installHandler(SIG_HANDLER(*handler))
{
  userhandler_ = handler;
  setUpSigAction();
}

/**.......................................................................
 * Install a handler (as an action).
 */
void Signal::installHandler(SIG_ACTION(*action))
{
  useraction_ = action;
  setUpSigAction();
}
