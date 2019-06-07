#include "carma/szautil/ArrayMapDataFrameManager.h"
#include "carma/szautil/MonitorPoint.h"
#include "carma/szautil/RegMapDataFrameManager.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
MonitorPoint::MonitorPoint(ArrayMapDataFrameManager* frame, ArrRegMap* regMap,
			   RegMapBlock* block, CoordRange* range) 
{
  setTo(frame, regMap, block, range);
}

/**.......................................................................
 * Constructor.
 */
MonitorPoint::MonitorPoint(RegMapDataFrameManager* frame,
			   RegMapBlock* block, CoordRange* range) 
{
  setTo(frame, block, range);
}

/**.......................................................................
 * Constructor.
 */
MonitorPoint::MonitorPoint(ArrayMapDataFrameManager* frame, ArrRegMap* regMap,
			   RegMapBlock* block, int index)
{
  setTo(frame, regMap, block, index);
}

/**.......................................................................
 * Constructor.
 */
MonitorPoint::MonitorPoint(RegMapDataFrameManager* frame,
			   RegMapBlock* block, int index) 
{
  setTo(frame, block, index);
}

/**.......................................................................
 * Initialize this monitor point
 */
MonitorPoint::MonitorPoint(ArrayMapDataFrameManager* frame, ArrRegMap* regMap, 
			   RegMapBlock* block)
{
  setTo(frame, regMap, block);
}

/**.......................................................................
 * Initialize this monitor point
 */
MonitorPoint::MonitorPoint(RegMapDataFrameManager* frame, RegMapBlock* block)
{
  setTo(frame, block);
}

/**.......................................................................
 * Initialize this monitor point
 */
void MonitorPoint::setTo(ArrayMapDataFrameManager* frame, ArrRegMap* regMap, 
			 RegMapBlock* block, int index) 
{
  CoordRange coordRange(index);
  setTo(frame, regMap, block, &coordRange);
}

/**.......................................................................
 * Initialize this monitor point
 */
void MonitorPoint::setTo(RegMapDataFrameManager* frame, RegMapBlock* block, 
			 int index) 
{
  CoordRange coordRange(index);
  setTo(frame, block, &coordRange);
}

/**.......................................................................
 * Initialize this monitor point
 */
void MonitorPoint::setTo(ArrayMapDataFrameManager* frame, ArrRegMap* regMap,
			 RegMapBlock* block, CoordRange* range) 
{
  if(frame == 0 || block == 0)
    ThrowError("Null argument received");

  isArrMap_ = true;
  arrMapFm_ = frame;
  regMap_   = regMap;
  block_    = block;

  // If the index specifies a real index, set the range accordingly

  if(range != 0) 
    coordRange_ = *range;

  // Else fill the range from the axis descriptor

  else 
    block_->axes_->fillRange(coordRange_);

  // Set up the default axisRange

  axisRange_.setTo(block_->axes_, &coordRange_);

  // Pre-set the type of this data point

  dataType_.type_ = dataType_.typeOf(block);
  
  // Resize the vector to accomodate all possible indices of this register

  handlers_.resize(block->nEl());
}

/**.......................................................................
 * Initialize this monitor point
 */
void MonitorPoint::setTo(RegMapDataFrameManager* frame, RegMapBlock* block, 
			  CoordRange* range) 
{
  if(frame == 0 || block == 0)
    ThrowError("Null argument received");

  isArrMap_ = false;
  regMapFm_ = frame;
  regMap_   = 0;
  block_    = block;

  // If the index specifies a real index, set the range accordingly

  if(range != 0) 
    coordRange_ = *range;

  // Else fill the range from the axis descriptor

  else 
    block_->axes_->fillRange(coordRange_);

  // Set up the default axisRange

  axisRange_.setTo(block_->axes_, &coordRange_);

  // Pre-set the type of this data point

  dataType_.type_ = dataType_.typeOf(block);
  
  // Resize the vector to accomodate all possible indices of this register

  handlers_.resize(block->nEl());
}

/**.......................................................................
 * Destructor.
 */
MonitorPoint::~MonitorPoint() {}

/**.......................................................................
 * See if a handler has been registered for this monitor point
 */
void MonitorPoint::checkHandlers(bool isSim, sza::util::AxisRange& range)
{
  // Iterate over all elements specified for this register

  for(range.reset(); !range.isEnd(); ++range) {

    // Check if we have any handlers registered for this element
  
    std::list<ConditionHandler>& handlerList = handlers_[range.currentElement()];

    if(handlerList.size() > 0) {
      
      // Create a vector of pointers to each element in the list.  We
      // will use this vector to iterate over all handlers currently
      // registered.  We do this because we don't want to iterate over
      // the list itself in case someone inserts a new condition while
      // we are inside our loop.  We use a mutex lock to protect the
      // list while the vector is constructed.
      
      guard_.lock();
      
      std::vector<ConditionHandler*> handlersToCheck;
      
      for(std::list<ConditionHandler>::iterator handler=handlerList.begin(); 
	  handler != handlerList.end(); handler++) 
	handlersToCheck.push_back(&(*handler));
      
      guard_.unlock();
      
      // Now iterate over the vector we just created.
      
      int count=0;
      for(unsigned iHandler=0; iHandler < handlersToCheck.size(); iHandler++) {
	
	ConditionHandler& handler = *handlersToCheck[iHandler];

	// If the handler was called...

	if(handler.checkCondition(isSim, dataType_)) {

	  // Check of this condition is persistent.  If not, remove it
	  // from the list.  If it is, reset the handler.

	  if(!handler.isPersistent_) {
	    handlerList.remove(handler);
	    count++;
	  } else {
	    handler.reset();
	  }
	} else {
	}
      }

      // Update the handler count if any were called

      updateHandlerCount(-count);
    }

    // Increment to the next value 

    ++dataType_;
  }
}

/**.......................................................................
 * Initialize a handler container
 */
MonitorPoint::ConditionHandler::ConditionHandler()
{
  handler_           = 0;
  arg_               = 0;
  packetCount_       = 0;
  stablePacketCount_ = 0;
  giveUpPacketCount_ = 0;
  isPersistent_      = false;
}

/**.......................................................................
 * Initialize a handler container
 */
MonitorPoint::ConditionHandler::ConditionHandler(const ConditionHandler& handler)
{
  *this = handler;
}

/**.......................................................................
 * Initialize a handler container
 */
MonitorPoint::ConditionHandler::ConditionHandler(ConditionHandler& handler)
{
  *this = handler;
}

/**.......................................................................
 * Assign a handler container
 */
void MonitorPoint::ConditionHandler::operator=(const ConditionHandler& handler)
{
  *this = (ConditionHandler&) handler;
}

void MonitorPoint::ConditionHandler::operator=(ConditionHandler& handler)
{
  condition_         = handler.condition_;
  handler_           = handler.handler_;
  arg_               = handler.arg_;
  packetCount_       = handler.packetCount_;
  stablePacketCount_ = handler.stablePacketCount_;
  giveUpPacketCount_ = handler.giveUpPacketCount_;
  isPersistent_      = handler.isPersistent_;
  message_           = handler.message_;
  comment_           = handler.comment_;
}

/**.......................................................................
 * Initialize a handler container
 */
bool MonitorPoint::ConditionHandler::operator==(const ConditionHandler& handler)
{
  return  (handler_ == handler.handler_) &&
    (arg_ == handler.arg_) &&
    (packetCount_ == handler.packetCount_) &&
    (stablePacketCount_ == handler.stablePacketCount_) &&
    (giveUpPacketCount_ == handler.giveUpPacketCount_);
}

/**.......................................................................
 * Call the handler
 */
void MonitorPoint::ConditionHandler::callHandler(bool conditionWasSatisfied)
{
  if(handler_ != 0) {
    handler_(arg_, conditionWasSatisfied, message_, comment_);
  }
}

/**.......................................................................
 * Format the condition associated with this handler
 */
std::string 
MonitorPoint::ConditionHandler::format(std::string& reg)
{
  return condition_.format(reg);
}

/**.......................................................................
 * Install a handler to be called when this monitor point meets the
 * specified condition
 */
void MonitorPoint::
registerConditionHandler(MONITOR_CONDITION_HANDLER(*handler), 
			 void* arg,
			 std::string message,
			 std::string comment,
			 MonitorCondition& condition, 
			 bool persistent,
			 sza::util::CoordRange* coordRange)
{

  // If the coordinate range wasn't specified, install the condition
  // for all elements of this register
  
  AxisRange range(block_, coordRange);

  // Update the handler count to reflect the handlers we will add.  Do
  // this before the loop in which we actually add them, in case the
  // list is evaluated by checkHandlers() while we are in the loop.
  // We don't care if the handler count is > the number of handlers
  // actually registered -- this may just cause an occasional
  // unnecessary evaulation of list.size().  But we don't ever want
  // checkHandlers() to remove a handler and decrement the count
  // before we've incremented it, otherwise the handler will never get
  // evaluated

  updateHandlerCount(range.nEl());

  // And keep a pointer to the handler for this index
  
  ConditionHandler condHandler;
  
  // We don't care if there already is a condition for this monitor
  // point.  A new condition overrides the old one.
  
  condHandler.handler_           = handler;
  condHandler.arg_               = arg;
  condHandler.message_           = message;
  condHandler.comment_           = comment;
  condHandler.condition_         = condition;
  condHandler.packetCount_       = 0;
  condHandler.stablePacketCount_ = 0;
  condHandler.giveUpPacketCount_ = 0;
  condHandler.isPersistent_      = persistent;

  // Iterate over all elements specified in the range, inserting the
  // same condition for each one.  
  
  unsigned index, count=0;

  guard_.lock();

  for(range.reset(); !range.isEnd(); ++range) {
    index = range.currentElement();

    list<ConditionHandler>& condList = handlers_[index];

    condList.insert(condList.end(), condHandler);
  }

  guard_.unlock();
}

/**.......................................................................
 * Update the handler count
 */
void MonitorPoint::updateHandlerCount(int count)
{
  guard_.lock();

  handlerCount_ += count;

  guard_.unlock();
}

/**.......................................................................
 * Reset the condition for this monitor point
 */
void MonitorPoint::reset()
{
  guard_.lock();

  for(unsigned iHandler=0; iHandler != handlers_.size(); iHandler++) {
    for(std::list<ConditionHandler>::iterator iter = handlers_[iHandler].begin();
	iter != handlers_[iHandler].end(); iter++) {
      iter->reset();
    }
  }
  
  guard_.unlock();
}

/**.......................................................................
 * Check the condition we are handling.  Return true if the condition
 * was met, or if we are ready to stop checking for this condition
 */
bool MonitorPoint::ConditionHandler::
checkCondition(bool isSim, DataType& dataType)
{
  packetCount_++;
  
  // If we have a handler for this monitor point, and the packet count
  // exceeds the limit set in the condition for this monitor point,
  // check if the current value satisfies the condition.
  
  MonitorCondition& condition = condition_;

  if(handler_ != 0 && packetCount_ > condition.packetCount_) {
    
    // If this condition is satisfied by the current value, increment
    // the count of consecutive packets during which the condition has
    // been met.  Note that simulated packets don't count.
    
    if(!isSim && condition.isSatisfiedBy(dataType)) {
      
      stablePacketCount_++;
      
      // If the packet has been stable for as long as stipulated by
      // the condition, call the handler

      if(stablePacketCount_ > condition.stablePacketCount_) {
	callHandler(true);
	return true;
      }
      
    } else {
      
      // Reset the count of consecutive packets during which this
      // monitor point has been stable
      
      stablePacketCount_ = 0;
      
      // And check if we should give up at this point
      
      giveUpPacketCount_++;
      
      if(condition.giveUpPacketCount_ > 0 && 
	 giveUpPacketCount_ > condition.giveUpPacketCount_) {

	callHandler(false);
	return true;
      }
    }
  }

  // This condition should continue to be checked
  
  return false;
}

/**.......................................................................
 * Check the condition we are handling.  Return true if the condition
 * was met, or if we are ready to stop checking for this condition
 */
void MonitorPoint::ConditionHandler::
reset()
{
  packetCount_       = 0;
  stablePacketCount_ = 0;
  giveUpPacketCount_ = 0;
}

/**.......................................................................
 * An operator for printing this object
 */
std::string 
MonitorPoint::format(std::string& reg)
{
  ostringstream os;

  for(unsigned iList=0; iList < handlers_.size(); iList++) {
    std::list<ConditionHandler>& handlerList = handlers_[iList];
    
    for(std::list<ConditionHandler>::iterator iHandler=handlerList.begin(); 
	iHandler != handlerList.end(); iHandler++) {
      os << iHandler->format(reg);

      // Return after the first valid handler for this monitor point.
      // This is because, at least for now, if the monitor point
      // manages multiple indices N of a register, we only want to
      // format the condition once, not N times.

      return os.str();
    }
    
  }

  return os.str();
}

bool MonitorPoint::isDelta()
{
  for(unsigned iList=0; iList < handlers_.size(); iList++) {
    std::list<ConditionHandler>& handlerList = handlers_[iList];
    
    for(std::list<ConditionHandler>::iterator iHandler=handlerList.begin(); 
	iHandler != handlerList.end(); iHandler++) {
      return iHandler->condition_.isDelta();
    }
  }

  ThrowError("No handler is registered"); 
}

unsigned MonitorPoint::nFrame()
{
  for(unsigned iList=0; iList < handlers_.size(); iList++) {
    std::list<ConditionHandler>& handlerList = handlers_[iList];
    
    unsigned count=0;
    for(std::list<ConditionHandler>::iterator iHandler=handlerList.begin(); 
	iHandler != handlerList.end(); iHandler++, count++) {
      return iHandler->condition_.nFrame();
    }
  }

  ThrowError("No handler is registered"); 
}

double MonitorPoint::min()
{
  for(unsigned iList=0; iList < handlers_.size(); iList++) {
    std::list<ConditionHandler>& handlerList = handlers_[iList];
    
    for(std::list<ConditionHandler>::iterator iHandler=handlerList.begin(); 
	iHandler != handlerList.end(); iHandler++) {
      return iHandler->condition_.min();
    }
  }

  ThrowError("No handler is registered"); 
}

double MonitorPoint::max()
{
  for(unsigned iList=0; iList < handlers_.size(); iList++) {
    std::list<ConditionHandler>& handlerList = handlers_[iList];
    
    for(std::list<ConditionHandler>::iterator iHandler=handlerList.begin(); 
	iHandler != handlerList.end(); iHandler++) {
      return iHandler->condition_.max();
    }
  }

  ThrowError("No handler is registered"); 
}



