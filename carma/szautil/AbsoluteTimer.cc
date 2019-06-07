#include "carma/szautil/AbsoluteTimer.h"

#include<iostream>

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
AbsoluteTimer::AbsoluteTimer() :
SpawnableTask<AbsoluteTimerMsg>(true)
{
  timeOut_.activate(false);
}

/**.......................................................................
 * Destructor.
 */
AbsoluteTimer::~AbsoluteTimer() {}

/**.......................................................................
 * Main Task event loop: when this is called, the task blocks forever
 * in select(), or until a stop message is received.
 */
void AbsoluteTimer::serviceMsgQ()
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

void AbsoluteTimer::processMsg(AbsoluteTimerMsg* msg)
{
  switch (msg->type) {
  case AbsoluteTimerMsg::TIMER:
    executeEnableTimer(msg->body.timer.enable, 
		       msg->body.timer.delayInSeconds, msg->body.timer.delayInNanoSeconds, 
		       msg->body.timer.offsetInNanoSeconds,
		       msg->body.timer.intervalInSeconds, msg->body.timer.intervalInNanoSeconds);
    break;
  case AbsoluteTimerMsg::ADD_HANDLER:
    executeAddHandler(msg->body.addHandler.fn, msg->body.addHandler.args);
    break;
  case AbsoluteTimerMsg::REM_HANDLER:
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
void AbsoluteTimer::addHandler(ABSOLUTE_TIMER_HANDLER(*fn), void* args)
{
  AbsoluteTimerMsg msg;
  msg.genericMsgType_ = GenericTaskMsg::TASK_SPECIFIC;
  msg.type = AbsoluteTimerMsg::ADD_HANDLER;
  msg.body.addHandler.fn = fn;
  msg.body.addHandler.args = args;
  sendTaskMsg(&msg);
}

void AbsoluteTimer::removeHandler(ABSOLUTE_TIMER_HANDLER(*fn))
{
  AbsoluteTimerMsg msg;
  msg.genericMsgType_ = GenericTaskMsg::TASK_SPECIFIC;
  msg.type = AbsoluteTimerMsg::REM_HANDLER;
  msg.body.remHandler.fn = fn;
  sendTaskMsg(&msg);
}

void AbsoluteTimer::enableTimer(bool enable, 
				unsigned delayInSeconds,    unsigned delayInNanoSeconds,
				unsigned offsetInNanoSeconds,
				unsigned intervalInSeconds, unsigned intervalInNanoSeconds)
{
  AbsoluteTimerMsg msg;
  msg.genericMsgType_ = GenericTaskMsg::TASK_SPECIFIC;
  msg.type = AbsoluteTimerMsg::TIMER;
  msg.body.timer.enable = enable;

  msg.body.timer.delayInSeconds        = delayInSeconds;
  msg.body.timer.delayInNanoSeconds    = delayInNanoSeconds;

  msg.body.timer.offsetInNanoSeconds   = offsetInNanoSeconds;

  msg.body.timer.intervalInSeconds     = intervalInSeconds;
  msg.body.timer.intervalInNanoSeconds = intervalInNanoSeconds;

  sendTaskMsg(&msg);
}

void AbsoluteTimer::executeEnableTimer(bool enable, 
				       unsigned delayInSeconds,    unsigned delayInNanoSeconds,
				       unsigned offsetInNanoSeconds,
				       unsigned intervalInSeconds, unsigned intervalInNanoSeconds)
{
  if(enable) {
    firstTimeoutSinceReconfiguration_ = true;

    // Set the initial delay to fire

    double dDelayInSeconds    = (double)delayInSeconds + (double)(delayInNanoSeconds) / 1e9;
    initialDelay_.setSeconds(dDelayInSeconds);

    // Set the offset from the 1-second boundary

    offset_.setNanoSeconds((double) offsetInNanoSeconds);

    // Set the interval


    double dIntervalInSeconds = (double)(intervalInSeconds) + (double)(intervalInNanoSeconds) / 1e9;

    interval_.setSeconds(dIntervalInSeconds);

    computeNextTimeout();

  } else {
    timeOut_.activate(false);
  }
}

/**.......................................................................
 * Execute a request to add a handler
 */
void AbsoluteTimer::executeAddHandler(ABSOLUTE_TIMER_HANDLER(*fn), void* args)
{
  Handler handler;
  handler.fn_   = fn;
  handler.args_ = args;

  handlers_.push_back(handler);
}

/**.......................................................................
 * Execute a request to remove a handler
 */
void AbsoluteTimer::executeRemoveHandler(ABSOLUTE_TIMER_HANDLER(*fn))
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
void AbsoluteTimer::callHandlers()
{
  for(unsigned iHandler=0; iHandler < handlers_.size(); iHandler++) {
    Handler& handler = handlers_[iHandler];
    (*handler.fn_)(handler.args_);
  }
}

void AbsoluteTimer::registerTimeOut()
{
  callHandlers();
  computeNextTimeout();
}

/**.......................................................................
 * Compute the time interval until the next time we should time out
 */
void AbsoluteTimer::computeNextTimeout()
{
  TimeVal timeVal;

  timeVal.setToCurrentTime();

  double currentTimeInSeconds;
  double deltaSeconds;

  currentTimeInSeconds = timeVal.getTimeInSeconds();

  // First, compute the first time we want this timer to fire, after
  // any initial delay

  if(firstTimeoutSinceReconfiguration_) {
    firstTimeoutSinceReconfiguration_ = false;

    double baseTimeInSeconds = currentTimeInSeconds + initialDelay_.seconds();

    unsigned absSeconds = (unsigned)baseTimeInSeconds;
    double remainderInSeconds = baseTimeInSeconds - (double)absSeconds;

    // If the current time is past the offset for the current second
    // boundary, start the timer at offset seconds after the next
    // second boundary

    if(remainderInSeconds > offset_.seconds()) {
      firstTimeToFireInSeconds_ = (double)(absSeconds) + 1.0 + offset_.seconds();

      // Else start the timer at offset seconds after the current
      // second boundary

    } else {
      firstTimeToFireInSeconds_ = (double)(absSeconds) + offset_.seconds();
    }

    deltaSeconds = firstTimeToFireInSeconds_ - currentTimeInSeconds;

    // Else calculate the number of intervals since the first time this timer fired

  } else {
    unsigned nInterval = (unsigned)((currentTimeInSeconds - firstTimeToFireInSeconds_) / interval_.seconds());
    
    // And set the next time to fire to nInterval + 1

    deltaSeconds = (firstTimeToFireInSeconds_ + (nInterval+1) * interval_.seconds()) - currentTimeInSeconds;
  }

  unsigned seconds  = (unsigned)deltaSeconds;
  unsigned nSeconds = (unsigned)((deltaSeconds - (double)seconds) * 1e9);

  // Set the new interval

  timeOut_.setInterval(seconds, nSeconds);

  // And force internal timeVal and timeSpec representations to adopt it

  timeOut_.reset();
  timeOut_.activate(true);
}
