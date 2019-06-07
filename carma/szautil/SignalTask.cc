#include <iomanip>

#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/SignalTask.h"
#include "carma/szautil/Thread.h"
#include "carma/szautil/TimeVal.h"

using namespace sza::util;
using namespace std;

#define SIGNALTASK_IO_SIGNAL SIGRTMIN

//-----------------------------------------------------------------------
// SignalTask::TimerInfo methods
//-----------------------------------------------------------------------

/**.......................................................................
 * Constructor.
 */
SignalTask::TimerInfo::TimerInfo(string name, 
				 int sigNo, 
				 unsigned long initSec, 
				 unsigned long initNsec,			
				 unsigned long intervalSec, 
				 unsigned long intervalNsec) : 
  sigNo_(sigNo), initSec_(initSec), initNsec_(initNsec),
  intervalSec_(intervalSec), intervalNsec_(intervalNsec), name_(name) 
{
  enabled_ = false;

  // Attempt to allocate and initialize the timer.

  timer_ = 0;
  timer_ = new AbsTimer(sigNo_); // Allocate an AbsTimer without
				 // installing a signal handler for
				 // it.
  if(timer_ == 0)
    throw Error("SignalTask::TimerInfo::TimerInfo: "
		"Out of memory.\n");

  timer_->setInitialDelay(initSec_, initNsec_);
  timer_->setIntervalDelay(intervalSec_, intervalNsec_);
}

/**.......................................................................
 * Destructor.
 */
SignalTask::TimerInfo::~TimerInfo() 
{
  // Delete the timer.

  if(timer_ != 0) {
    delete(timer_);
  }
}

/**.......................................................................
 * Re-arm a periodic timer.
 */
void SignalTask::TimerInfo::reArm()
{
  timer_->reArm();
}

//-----------------------------------------------------------------------
// SignalTask::SignalHandler methods
//-----------------------------------------------------------------------

/**.......................................................................
 * Constructor.
 */
SignalTask::SignalHandler::SignalHandler(int sigNo, 
					 SIGNALTASK_HANDLER_FN(*handler),
					 void* args) :
  sigNo_(sigNo), timer_(0) 
{
  // A NULL handler argument should not be interpreted as an error, as
  // handlers may be added later, but it should not be added as a
  // valid callback function.

  if(handler != 0)
    handlers_.push_back(HandlerPair(handler, args));
}

/**.......................................................................
 * Constructor with TimerInfo pointer to the timer associated with
 * this signal handler.
 */
SignalTask::SignalHandler::SignalHandler(int sigNo, 
					 SIGNALTASK_HANDLER_FN(*handler),
					 TimerInfo* timer,
					 void* args) :
  sigNo_(sigNo), timer_(timer) 
{
  // A NULL handler argument should not be interpreted as an error, as
  // handlers may be added later, but it should not be added as a
  // valid callback function.

  if(handler != 0) 
    handlers_.push_back(HandlerPair(handler, args));
}

/**.......................................................................
 * Destructor.
 */
SignalTask::SignalHandler::~SignalHandler() {};

/**.......................................................................
 * Re-arm any periodic timer associated with this handler.
 */
void SignalTask::SignalHandler::reArm()
{
  if(timer_ != 0)
    timer_->reArm();
}

/**.......................................................................
 * Add a handler to the set attached to this signal
 */
void SignalTask::SignalHandler::addHandler(SIGNALTASK_HANDLER_FN(*handler), void* args)
{
  // If we already have a reference to this handler, don't add it again.

  for(unsigned ihand=0; ihand < handlers_.size(); ihand++)
    if(handlers_[ihand].handler_ == handler)
      return;

  // Else add a new element to the vector of handlers for this signal.

  handlers_.push_back(HandlerPair(handler, args));
}

/**.......................................................................
 * Remove a handler to the set attached to this signal
 */
void SignalTask::SignalHandler::removeHandler(SIGNALTASK_HANDLER_FN(*handler))
{
  // Iterate over known handlers, looking for a match.

  for(vector<SignalTask::SignalHandler::HandlerPair>::iterator hand=handlers_.begin();
      hand != handlers_.end(); hand++) {
    if((*hand).handler_ == handler) {
      handlers_.erase(hand);
      break;
    }
  }
}

//-----------------------------------------------------------------------
// SignalTask methods
//-----------------------------------------------------------------------

/**.......................................................................
 * Initialize static pointer to NULL.
 */
SignalTask* SignalTask::signaltask_ = 0;

/**.......................................................................
 * Constructor.
 */
SignalTask::SignalTask(bool spawnThread) :
  spawned_(spawnThread)
{
  spawnedThread_ = 0;

  privateConstructor();

  // If spawning in a separate thread, start up now.

  if(spawnThread) {

    // Block all signals in the calling thread

    sigset_t allSignals;
    sigfillset(&allSignals);
    pthread_sigmask(SIG_BLOCK, &allSignals, NULL);

    // And start up a new thread which will handle signals

    spawnedThread_ = new Thread(startUp, 0, 0, "SignalTask");
    spawnedThread_->start(this);
  }
}

/**.......................................................................
 * Constructor.
 */
void SignalTask::privateConstructor()
{
  signaltask_ = this;

  id_ = pthread_self();

  sigIo_ = SIGNALTASK_IO_SIGNAL;

  // Install our checkMsgQ() method as the signal handler for the I/O
  // signal.

  installSignal(sigIo_, &checkMsgQ);
}

/**.......................................................................
 * Destructor: Delete any timers managed by this task.
 */
SignalTask::~SignalTask() 
{
  // If spawned in a separate thread, cancel it now.

  if(spawnedThread_ != 0)
    delete spawnedThread_;

  // And delete any allocated resources

  for(unsigned itimer=0; itimer < timers_.size(); itimer++)
    delete timers_[itimer];

  for(unsigned isig=0; isig < signalHandlers_.size(); isig++)
    delete signalHandlers_[isig];
}

/**.......................................................................
 * Override GenericTask::fwdTaskMsg() to raise a signal to the signaltask
 * when a message has arrived.
 */
void SignalTask::fwdTaskMsg(SignalTaskMsg* msg) 
{
  // Write the message to our message queue.

  sendTaskMsg(msg);

  // Raise a signal to the thread which instantiated this object that
  // an I/O event has arrived.  Check if we spawned a thread on
  // startup to make sure we are raising to the right thread.

  if(spawned_)
    spawnedThread_->raise(sigIo_);
  else
    pthread_kill(id_, sigIo_);
}

/**.......................................................................
 * Send a message to install a timer.
 */
void SignalTask::sendInstallTimerMsg(string name,
				     int sigNo, 
				     unsigned long initSec,
				     unsigned long initNsec,
				     unsigned long intervalSec,
				     unsigned long intervalNsec,
				     SIGNALTASK_HANDLER_FN(*handler))
{
  SignalTaskMsg msg;

  DBPRINT(true, DEBUG_SIGNAL, "Inside");

  msg.packInstallTimerMsg(name, sigNo, initSec, initNsec, intervalSec, 
			  intervalNsec, handler);

  fwdTaskMsg(&msg);
}

/**.......................................................................
 * Respond to a message to install a timer.
 */
void SignalTask::installTimer(SignalTaskMsg* msg)
{
  installTimer(msg->body.installTimer.name, 
	       msg->body.installTimer.sigNo,
	       msg->body.installTimer.initSec,
	       msg->body.installTimer.initNsec,
	       msg->body.installTimer.intervalSec,
	       msg->body.installTimer.intervalNsec,
	       msg->body.installTimer.handler);
}

/**.......................................................................
 * Create a new timer, and install a handler for its signal.
 */
void SignalTask::installTimer(string name, 
			      int sigNo, 
			      unsigned long initSec, 
			      unsigned long initNsec,
			      unsigned long intervalSec, 
			      unsigned long intervalNsec,
			      SIGNALTASK_HANDLER_FN(*handler))
{
  // First check if a timer already exists for this signal.

  for(unsigned itimer=0; itimer < timers_.size(); itimer++) {

    if(timers_[itimer]->sigNo_ == sigNo) {
      ostringstream os;
      os << "SignalTask::installTimer: "
	 << "A timer is already installed for signal: "
	 << sigNo << endl << ends;
    throw Error(os.str());
    }

    if(timers_[itimer]->name_ == name) {
      ostringstream os;
      os << "SignalTask::installTimer: "
	 << "A timer with name: " << name << " is already installed."
	 << endl << ends;
    throw Error(os.str());
    }
  }

  // Now check if a handler already exists for this signal.

  for(unsigned ihand=0; ihand < signalHandlers_.size(); ihand++)
    if(signalHandlers_[ihand]->sigNo_ == sigNo) {
      ostringstream os;
      os << "SignalTask::installTimer: "
	 << "A handler is already installed for signal: "
	 << sigNo << endl << ends;
    throw Error(os.str());
    }

  // Now add a new element to the vector of timers managed by this
  // task.  
  //
  // Note that timers_ has to be a vector of type <TimerInfo*> rather
  // than <TimerInfo> because if we were to call
  // push_back(TimerInfo(...)) here, this would construct a TimerInfo
  // and its AbsTimer member (initializing its timer_t member in
  // timer_create()), then call a bitwise copy constructor, then call
  // the destructor for the first object, which calls timer_delete()
  // on its timer_t member.  So what would be left in timers_[] is a
  // copy-constructed AbsTimer object with a timer_t argument which no
  // longer referred to a valid timer.  This will throw an EINVAL
  // error in timer_settime().

  TimerInfo* timer = new TimerInfo(name, sigNo, initSec, initNsec,
				   intervalSec, intervalNsec); 
  timers_.push_back(timer);

  // And add a new element to the vector of handlers managed by this
  // task.

  signalHandlers_.push_back(new SignalHandler(sigNo, handler, timer));

  // And add this signal to the set of signals handled by this task.
  
  DBPRINT(true, Debug::DEBUG10, "Adding signal: " << sigNo);

  sigaddset(&handledSignals_, sigNo);
}

/**.......................................................................
 * Send a message to add a handler to the set attached to a named
 * timer
 */
void SignalTask::sendAddHandlerMsg(string name,
				   SIGNALTASK_HANDLER_FN(*handler),
				   bool add)
{
  SignalTaskMsg msg;

  DBPRINT(true, DEBUG_SIGNAL, "Inside");

  msg.packAddHandlerMsg(name, handler, add);

  fwdTaskMsg(&msg);
}

/**.......................................................................
 * Add a handler to the list of handlers attached to a named timer.
 */
void SignalTask::addHandler(string name, SIGNALTASK_HANDLER_FN(*handler), void* args)
{
  // Find the timer by name, if it exists

  DBPRINT(true, Debug::DEBUG9, "inside addHandler");

  for(vector<TimerInfo*>::iterator timer=timers_.begin(); 
      timer != timers_.end(); timer++) {

    TimerInfo* tptr = *timer;
    
    if(tptr->name_ == name) {
      
      // Get the handler corresponding to this timer's signal

      SignalHandler* sigHandler = getHandler(tptr->sigNo_);

      if(sigHandler != 0) {
	unsigned nPrev = sigHandler->handlers_.size();

	sigHandler->addHandler(handler, args);

	// If we've just added the first handler for this signal, and
	// the timer was previously enabled, restart the timer.

	if(Debug::debugging(Debug::DEBUG9)) {
	  if(nPrev==0 && tptr->enabled_) {

	    cout << "Starting timer: " << name << endl;

	    cout << tptr->initSec_ << ", " << tptr->initNsec_
		 << tptr->intervalSec_ << ", " << tptr->intervalNsec_;

	  }
	}

	if(nPrev==0 && tptr->enabled_)
	  tptr->timer_->start();
      }

      DBPRINT(true, DEBUG_DELAY, "Handler: " << tptr->name_ 
	      << " now has " << sigHandler->handlers_.size()
	      << " attached");

      return;
    }
  }
}

/**.......................................................................
 * Remove a handler from the list of handlers attached to a named timer.
 */
void SignalTask::removeHandler(string name, SIGNALTASK_HANDLER_FN(*handler))
{
  // Find the timer by name, if it exists

  for(vector<TimerInfo*>::iterator timer=timers_.begin(); 
      timer != timers_.end(); timer++) {

    TimerInfo* tptr = *timer;

    if(tptr->name_ == name) {

      // Get the handler corresponding to this timer's signal

      SignalHandler* sigHandler = getHandler(tptr->sigNo_);

      if(sigHandler != 0) {

	// Remove the requested handler from the list of handlers for
	// this signal

	sigHandler->removeHandler(handler);

	// If after removal, there are no more handlers listed for
	// this signal, stop the timer.

	if(sigHandler->handlers_.size() == 0)
	  tptr->timer_->stop();
      }
      return;
    }
  }
}

/**.......................................................................
 * Send a message to install a signal.
 */
void SignalTask::sendInstallSignalMsg(int sigNo, 
				      SIGNALTASK_HANDLER_FN(*handler),
				      void* arg)
{
  SignalTaskMsg msg;
  DBPRINT(true, Debug::DEBUG3, "Sending install signal msg");
  msg.packInstallSignalMsg(sigNo, handler, arg);

  fwdTaskMsg(&msg);
}

/**.......................................................................
 * Respond to a message to install a signal.
 */
void SignalTask::installSignal(SignalTaskMsg* msg)
{
  installSignal(msg->body.installSignal.sigNo, 
		msg->body.installSignal.handler,
		msg->body.installSignal.arg);
}

/**.......................................................................
 * Install a handler for a signal.
 */
void SignalTask::installSignal(int sigNo, SIGNALTASK_HANDLER_FN(*handler), void* args)
{
  // Now check if a handler already exists for this signal.

  for(unsigned ihand=0; ihand < signalHandlers_.size(); ihand++)
    if(signalHandlers_[ihand]->sigNo_ == sigNo) {
      ostringstream os;
      os << "SignalTask::installTimer: "
	 << "A handler is already installed for signal: "
	 << sigNo << endl << ends;
    throw Error(os.str());
    }

  // And add a new element to the vector of handlers managed by this task.

  signalHandlers_.push_back(new SignalHandler(sigNo, handler, args));

  // And add this signal to the set of signals handled by this task.

  DBPRINT(true, Debug::DEBUG10, "Adding signal: " << sigNo);

  sigaddset(&handledSignals_, sigNo);
}

/**.......................................................................
 * Send a message to add a handler to the set attached to this timer.
 */
void SignalTask::sendAddHandlerMsg(int sigNo,
				   SIGNALTASK_HANDLER_FN(*handler),
				   bool add)
{
  SignalTaskMsg msg;

  msg.packAddHandlerMsg(sigNo, handler, add);

  fwdTaskMsg(&msg);
}

/**.......................................................................
 * Add a handler for a signal.
 */
void SignalTask::addHandler(int sigNo, SIGNALTASK_HANDLER_FN(*handler), void* args)
{
  // Check if a handler already exists for this signal.

  for(unsigned ihand=0; ihand < signalHandlers_.size(); ihand++)
    if(signalHandlers_[ihand]->sigNo_ == sigNo) {
      signalHandlers_[ihand]->addHandler(handler, args);
      break;
    }
  return;
}

/**.......................................................................
 * Return a pointer to the SignalHandler object for a given signal, or
 * NULL if none exists.
 */
SignalTask::SignalHandler* SignalTask::getHandler(int sigNo)
{
  // Check if a handler already exists for this signal.

  for(unsigned ihand=0; ihand < signalHandlers_.size(); ihand++)
    if(signalHandlers_[ihand]->sigNo_ == sigNo) {
      return signalHandlers_[ihand];
      break;
    }
  return 0;
}

/**.......................................................................
 * Get the timer associated with a name.
 */
SignalTask::TimerInfo* SignalTask::getTimer(std::string name)
{
  for(vector<TimerInfo*>::iterator timer=timers_.begin(); 
      timer != timers_.end(); timer++) 
    if((*timer)->name_ == name) 
      return *timer;
  
  return 0;
}

/**.......................................................................
 * Get the timer associated with a signal
 */
SignalTask::TimerInfo* SignalTask::getTimer(int sigNo)
{
  for(vector<TimerInfo*>::iterator timer=timers_.begin(); 
      timer != timers_.end(); timer++) 
    if((*timer)->sigNo_ == sigNo) 
      return *timer;
  
  return 0;
}

/**.......................................................................
 * Remove a handler for a signal.
 */
void SignalTask::removeHandler(int sigNo, SIGNALTASK_HANDLER_FN(*handler))
{
  // Check if a handler already exists for this signal.

  for(unsigned ihand=0; ihand < signalHandlers_.size(); ihand++)
    if(signalHandlers_[ihand]->sigNo_ == sigNo) {
      signalHandlers_[ihand]->removeHandler(handler);
      break;
    }
  return;
}

/**.......................................................................
 * Respond to a message to enable/disable a timer.
 */
void SignalTask::sendEnableTimerMsg(string name, bool enable)
{
  SignalTaskMsg msg;

  DBPRINT(true, DEBUG_SIGNAL, "Inside");

  msg.packEnableTimerMsg(name, enable);

  fwdTaskMsg(&msg);
}

/**.......................................................................
 * Respond to a message to enable/disable a timer.
 */
void SignalTask::enableTimer(SignalTaskMsg* msg)
{
  if(msg->body.enableTimer.enable)
    startTimer(msg->body.enableTimer.name);
  else
    stopTimer(msg->body.enableTimer.name);
}

/**.......................................................................
 * Start the timers
 */
void SignalTask::startTimers()
{
  for(unsigned itimer = 0; itimer < timers_.size(); itimer++) 
      timers_[itimer]->timer_->start();
}

/**.......................................................................
 * Start a named timer.
 */
void SignalTask::startTimer(string name)
{
  // Find a named timer

  TimerInfo* timer = getTimer(name);

  if(timer != 0) {

    // Mark this timer as enabled

    timer->enabled_ = true;

    // But only start the timer if there is at least one handler
    // attached.

    SignalHandler* sigHandler = getHandler(timer->sigNo_);

    if(sigHandler != 0) 
      if(sigHandler->handlers_.size() > 0)
	timer->timer_->start();
  }
  return;
}

/**.......................................................................
 * Stop all timers
 */
void SignalTask::stopTimers()
{
  for(unsigned itimer = 0; itimer < timers_.size(); itimer++) 
    timers_[itimer]->timer_->stop();
}

/**.......................................................................
 * Stop a named timer.
 */
void SignalTask::stopTimer(string name)
{
  // Find a named timer

  TimerInfo* timer = getTimer(name);

  if(timer != 0) {
    timer->enabled_ = false;
    timer->timer_->stop();
  }
  return;
}

/**.......................................................................
 * A run method for this task.
 */
void SignalTask::run()
{
  int sigNo;               // A received signal.

  while(true) {
    
    // Wait for the next catchable signal to be sent to the process.
    //
    // Note that sigwait() atomically unblocks the specified signals
    // while it is waiting and reblocks them before returning, so
    // there is no danger of missing any signals that are delivered
    // between each call to sigwait().
    
    sigwait(&handledSignals_, &sigNo);
    
    // Handle the signal.
    
    for(unsigned isig=0; isig < signalHandlers_.size(); isig++) {

      if(sigNo == signalHandlers_[isig]->sigNo_) {

 	// And call any handlers which have been installed for this
	// signal
	
	for(unsigned ihand=0; 
	    ihand < signalHandlers_[isig]->handlers_.size(); ihand++)
	  if(signalHandlers_[isig]->handlers_[ihand].handler_ != 0) {
	    
	    SIGNALTASK_HANDLER_FN(*handler) = signalHandlers_[isig]->handlers_[ihand].handler_;
	    void* args = signalHandlers_[isig]->handlers_[ihand].args_;
	    
	    try {
	      handler(sigNo, args);
	    } catch(Exception& err) {
	      COUT("Caught an exception: " << err.what() << endl
		   << "While calling handlers(" 
		   << signalHandlers_[isig]->handlers_.size() << ")"
		   << " for signal: " << sigNo);
	    }
	  }

	break; // Found the handler for this signal
      
      }
    
    }
  }
}

/**.......................................................................
 * Check our message queue for pending messages.
 */
void SignalTask::serviceMsgQ()
{
  bool stop=false;
  int nready; // Number of file descriptors ready for reading
  int fd = msgq_.fd();
  
  if(fd < 0)
    throw Error("SignalTask::serviceMsgQ: "
		"Received invalid file descriptor");
  
  // We use a timeout of zero seconds, which causes select() to return
  // immediately.

  TimeVal timeout(0, 0, 0);
  
  // Loop, checking the message queue file descriptor for readability
  
  fd_set rfds = msgq_.rfds();
  
  while(!stop && 
	(nready=select(fd+1, &rfds, NULL, NULL, timeout.timeVal())) > 0) {
    
    // If no file descriptors were ready, throw an exception
    
    if(nready != 1)
      throw Error("SignalTask::serviceMsgQ: Error");
    
    processTaskMsg(&stop);
  };
}

/**.......................................................................
 * Check our message queue for pending messages.
 */
void SignalTask::processTaskMsg(bool* stop)
{
  SignalTaskMsg msg;
	    
  msgq_.readMsg(&msg);
	    
  switch (msg.type) {
  case SignalTaskMsg::ADD_SIGNAL_HANDLER:
    if(msg.body.addSignalHandler.add)
      addHandler(msg.body.addSignalHandler.sigNo, 
		 msg.body.addSignalHandler.handler);
    else
      removeHandler(msg.body.addSignalHandler.sigNo, 
		 msg.body.addSignalHandler.handler);
    break;
  case SignalTaskMsg::ADD_TIMER_HANDLER:

    if(msg.body.addTimerHandler.add)
      addHandler(msg.body.addTimerHandler.name, 
		 msg.body.addTimerHandler.handler);
    else
      removeHandler(msg.body.addTimerHandler.name, 
		    msg.body.addTimerHandler.handler);
    break;
  case SignalTaskMsg::SIG_INSTALL_TIMER: 
    installTimer(&msg);
    break;
  case SignalTaskMsg::SIG_INSTALL_SIGNAL:
    installSignal(&msg);
    break;
  case SignalTaskMsg::SIG_ENABLE_TIMER:
    enableTimer(&msg);
    break;
  default: 
    break;
  }
}

/**.......................................................................
 * A static function to check our message queue.
 */
SIGNALTASK_HANDLER_FN(SignalTask::checkMsgQ)
{
  signaltask_->serviceMsgQ();
}

/**.......................................................................
 * Return which signal this object uses for I/O events.
 */
int SignalTask::getIoSig()
{
  return sigIo_;
}

/**.......................................................................
 * A startup function for the spawned thread.
 */
THREAD_START(SignalTask::startUp)
{
  SignalTask* signalTask = (SignalTask*)arg;
  signalTask->spawnedThread_->broadcastReady();
  signalTask->run();
}

pthread_t SignalTask::getThreadId()
{
  return id_;
}
