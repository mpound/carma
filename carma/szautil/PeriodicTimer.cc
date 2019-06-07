#include "carma/szautil/PeriodicTimer.h"

#include<iostream>

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
PeriodicTimer::PeriodicTimer() :
SpawnableTask<PeriodicTimerMsg>(true)
{
  timeOut_.activate(false);
}

/**.......................................................................
 * Destructor.
 */
PeriodicTimer::~PeriodicTimer() {}

/**.......................................................................
 * Main Task event loop: when this is called, the task blocks forever
 * in select(), or until a stop message is received.
 */
void PeriodicTimer::serviceMsgQ()
{
  try {
    bool stop=false;
    int nready; // number of file descriptors ready for reading
    
    if(msgq_.fd() < 0) {
      ThrowError("Received NULL file descriptor");
    }
    
    while(!stop) {
      
      nready = select(fdSet_.size(), fdSet_.readFdSet(), 
		      NULL, NULL, timeOut_.tVal());

      switch (nready) {

	// If no file descriptors were ready, is it time to contact the
	// PERIODIC_TIMER ftp server and retrieve the UT1-UTC ephemeris?
      
      case 0:

	registerTimeOut();

	break;

      case -1:

	stop = true;
	ThrowSysError("select()");
	break;

      default:

	// If a message is waiting to be read, process it now

	if(fdSet_.isSetInRead(msgq_.fd())) {
	  processTaskMsg(&stop);
	}
	 
	break;
      }
    }
  } catch(Exception& err) {
    COUT("Caught an exception: " << err.what());
  }
}

void PeriodicTimer::processMsg(PeriodicTimerMsg* msg)
{
  switch (msg->type) {
  case PeriodicTimerMsg::TIMER:
    executeEnableTimer(msg->body.timer.enable, msg->body.timer.intervalInSeconds);
    break;
  case PeriodicTimerMsg::ADD_HANDLER:
    executeAddHandler(msg->body.addHandler.fn, msg->body.addHandler.args);
    break;
  case PeriodicTimerMsg::REM_HANDLER:
    executeRemoveHandler(msg->body.remHandler.fn);
    break;
  default:
    ThrowError("Unrecognized message type: " << msg->type);
    break;
  }
}

/**.......................................................................
 * Public method to add a handler to be called when the ephemeris file
 * is updated
 */
void PeriodicTimer::addHandler(PERIODIC_TIMER_HANDLER(*fn), void* args)
{
  PeriodicTimerMsg msg;
  msg.genericMsgType_ = GenericTaskMsg::TASK_SPECIFIC;
  msg.type = PeriodicTimerMsg::ADD_HANDLER;
  msg.body.addHandler.fn = fn;
  msg.body.addHandler.args = args;
  sendTaskMsg(&msg);
}

void PeriodicTimer::removeHandler(PERIODIC_TIMER_HANDLER(*fn))
{
  PeriodicTimerMsg msg;
  msg.genericMsgType_ = GenericTaskMsg::TASK_SPECIFIC;
  msg.type = PeriodicTimerMsg::REM_HANDLER;
  msg.body.remHandler.fn = fn;
  sendTaskMsg(&msg);
}

void PeriodicTimer::enableTimer(bool enable, unsigned intervalInSeconds)
{
  PeriodicTimerMsg msg;
  msg.genericMsgType_ = GenericTaskMsg::TASK_SPECIFIC;
  msg.type = PeriodicTimerMsg::TIMER;
  msg.body.timer.enable = enable;
  msg.body.timer.intervalInSeconds = intervalInSeconds;
  sendTaskMsg(&msg);
}

void PeriodicTimer::executeEnableTimer(bool enable, unsigned intervalInSeconds)
{
  if(enable) {
    timeOut_.setIntervalInSeconds(intervalInSeconds);
    timeOut_.activate(true);
  } else {
    timeOut_.activate(false);
  }
}

/**.......................................................................
 * Execute a request to add a handler
 */
void PeriodicTimer::executeAddHandler(PERIODIC_TIMER_HANDLER(*fn), void* args)
{
  Handler handler;
  handler.fn_   = fn;
  handler.args_ = args;

  handlers_.push_back(handler);
}

/**.......................................................................
 * Execute a request to remove a handler
 */
void PeriodicTimer::executeRemoveHandler(PERIODIC_TIMER_HANDLER(*fn))
{
  std::vector<Handler>::iterator iter=handlers_.begin();

  for(; iter != handlers_.end(); iter++) {
    if(iter->fn_ == fn)
      break;
  }
    
  if(iter != handlers_.end())
    handlers_.erase(iter);
}

/**.......................................................................
 * Call any handler that was registered
 */
void PeriodicTimer::callHandlers()
{
  for(unsigned iHandler=0; iHandler < handlers_.size(); iHandler++) {
    Handler& handler = handlers_[iHandler];
    (*handler.fn_)(handler.args_);
  }
}

void PeriodicTimer::registerTimeOut()
{
  callHandlers();
  timeOut_.reset();
}
