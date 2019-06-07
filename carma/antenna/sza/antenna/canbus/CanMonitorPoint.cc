#include "carma/antenna/sza/antenna/canbus/CanCommand.h"
#include "carma/antenna/sza/antenna/canbus/CanMonitorPoint.h"
#include "carma/antenna/sza/antenna/control/SzaShare.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"

using namespace std;
using namespace sza::antenna::canbus;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
CanMonitorPoint::CanMonitorPoint(sza::antenna::control::SzaShare* share, 
				 RegMapBlock* block) :
  share_(share), block_(block)
{
  dataType_.type_ = dataType_.typeOf(block);

  for(unsigned iHandler=0; iHandler < block->nEl(); iHandler++)
    handlers_.push_back(ConditionHandler());
}

/**.......................................................................
 * Destructor.
 */
CanMonitorPoint::~CanMonitorPoint() {}

void CanMonitorPoint::writeReg(bool isSim, bool b, sza::util::CoordRange* range)
{
  dataType_ = b;
  share_->writeReg(block_, b, range);
  checkHandler(isSim, range);
}

void CanMonitorPoint::writeReg(bool isSim, unsigned char uc, sza::util::CoordRange* range)
{
  dataType_ = uc;
  share_->writeReg(block_, uc, range);
  checkHandler(isSim, range);
}

void CanMonitorPoint::writeReg(bool isSim, char ch, sza::util::CoordRange* range)
{
  dataType_ = ch;
  share_->writeReg(block_, ch, range);
  checkHandler(isSim, range);
}

void CanMonitorPoint::writeReg(bool isSim, unsigned short us, sza::util::CoordRange* range)
{
  dataType_ = us;
  share_->writeReg(block_, us, range);
  checkHandler(isSim, range);
}

void CanMonitorPoint::writeReg(bool isSim, short s, sza::util::CoordRange* range)
{
  dataType_ = s;
  share_->writeReg(block_, s, range);
  checkHandler(isSim, range);
}

void CanMonitorPoint::writeReg(bool isSim, unsigned int ui, sza::util::CoordRange* range)
{
  dataType_ = ui;
  share_->writeReg(block_, ui, range);
  checkHandler(isSim, range);
}

void CanMonitorPoint::writeReg(bool isSim, int i, sza::util::CoordRange* range)
{
  dataType_ = i;
  share_->writeReg(block_, i, range);
  checkHandler(isSim, range);
}

void CanMonitorPoint::writeReg(bool isSim, float f, sza::util::CoordRange* range)
{
  dataType_ = f;
  share_->writeReg(block_, f, range);
  checkHandler(isSim, range);
}

void CanMonitorPoint::writeReg(bool isSim, double d, sza::util::CoordRange* range)
{
  dataType_ = d;
  share_->writeReg(block_, d, range);
  checkHandler(isSim, range);
}

//-----------------------------------------------------------------------
// Pointer versions are straight-throughs with no checking
//-----------------------------------------------------------------------

void CanMonitorPoint::writeReg(bool isSim, bool* b, sza::util::CoordRange* range)
{
  share_->writeReg(block_, b, range);
}

void CanMonitorPoint::writeReg(bool isSim, unsigned char* uc, sza::util::CoordRange* range)
{
  share_->writeReg(block_, uc, range);
}

void CanMonitorPoint::writeReg(bool isSim, char* ch, sza::util::CoordRange* range)
{
  share_->writeReg(block_, ch, range);
}

void CanMonitorPoint::writeReg(bool isSim, unsigned short* us, sza::util::CoordRange* range)
{
  share_->writeReg(block_, us, range);
}

void CanMonitorPoint::writeReg(bool isSim, short* s, sza::util::CoordRange* range)
{
  share_->writeReg(block_, s, range);
}

void CanMonitorPoint::writeReg(bool isSim, unsigned int* ui, sza::util::CoordRange* range)
{
  share_->writeReg(block_, ui, range);
}

void CanMonitorPoint::writeReg(bool isSim, int* i, sza::util::CoordRange* range)
{
  share_->writeReg(block_, i, range);
}

void CanMonitorPoint::writeReg(bool isSim, float* f, sza::util::CoordRange* range)
{
  share_->writeReg(block_, f, range);
}

void CanMonitorPoint::writeReg(bool isSim, double* d, sza::util::CoordRange* range)
{
  share_->writeReg(block_, d, range);
}

/**.......................................................................
 * See if a handler has been registered for this monitor point
 */
void CanMonitorPoint::checkHandler(bool isSim, sza::util::CoordRange* range)
{
  unsigned index;
  if(range==0)
    index = 0;
  else {
    sza::util::Coord coord = range->startCoord();
    index = block_->axes_->elementOffsetOf(coord);
  }

  // Increment the packet count for this monitor point

  ConditionHandler& handler = handlers_[index];
  
  if(handler.handler_ == 0)
    return;

  handler.packetCount_++;

  // If we have a handler for this monitor point, and the packet count
  // exceeds the limit set in the condition for this monitor point,
  // check if the current value satisfies the condition.

  CanMonitorCondition& condition = handler.condition_;

  if(handler.handler_ != 0 && handler.packetCount_ > condition.packetCount_) {

    // If this condition is satisfied by the current value, increment
    // the count of consecutive packets during which the condition has
    // been met.  Note that simulated packets don't count.

    if(!isSim && condition.isSatisfiedBy(dataType_)) {

      //      COUT("Condition was satisfied: " << handler.stablePacketCount_);

      handler.stablePacketCount_++;

      // If the packet has been stable for as long as stipulated by
      // the condition, call the handler

      if(handler.stablePacketCount_ > condition.stablePacketCount_)
	handler.callHandler(true);

    } else {

      //      COUT("Condition was not satisfied: " << condition.giveUpPacketCount_ 
      //	   << ", " << handler.giveUpPacketCount_);
 
      // Reset the count of consecutive packets during which this
      // monitor point has been stable

      handler.stablePacketCount_ = 0;

      // And check if we should give up at this point

      handler.giveUpPacketCount_++;

      if(condition.giveUpPacketCount_ > 0 && handler.giveUpPacketCount_ > condition.giveUpPacketCount_)
	handler.callHandler(false);

    }
  }
}

/**.......................................................................
 * Initialize a handler container
 */
CanMonitorPoint::ConditionHandler::ConditionHandler()
{
  handler_     = 0;
  arg_         = 0;
  packetCount_ = 0;
}

/**.......................................................................
 * Call the handler
 */
void CanMonitorPoint::ConditionHandler::callHandler(bool conditionWasSatisfied)
{
  if(handler_ != 0) {

    // Make a copy of the pointer to our handler.  This is so that we
    // can remove the handler first, before calling it.  We do this
    // because calling the handler might result in a new handler being
    // installed for the same monitor point by the same process.  If
    // we mutex protected this resource, we could deadlock, and if we
    // simply removed it after the call, we might be removing the
    // newly-installed handler. This way the handler can be removed
    // and possibly overwritten without conflict.

    CAN_MONITOR_CONDITION_HANDLER(*tmpHandler) = handler_;
    void* tmpArg = arg_;

    // Remove the handler first

    handler_ = 0;
    arg_     = 0;

    // And call it via our proxy

    tmpHandler(tmpArg, conditionWasSatisfied, message_);
  }
}

/**.......................................................................
 * Install a handler to be called when this monitor point meets the
 * specified condition
 */
void CanMonitorPoint::
registerConditionHandler(CAN_MONITOR_CONDITION_HANDLER(*handler), 
			 void* arg,
			 std::string& message,
			 CanMonitorCondition& condition, 
			 sza::util::Coord* coord)
{
  // Get the requested index of this register

  unsigned index = coord==0 ? 0 : block_->axes_->elementOffsetOf(coord);

  // And keep a pointer to the handler for this index

  ConditionHandler& condHandler = handlers_[index];

  // We don't care if there already is a condition for this monitor
  // point.  A new condition overrides the old one.

  condHandler.handler_           = handler;
  condHandler.arg_               = arg;
  condHandler.message_           = message;
  condHandler.condition_         = condition;
  condHandler.packetCount_       = 0;
  condHandler.stablePacketCount_ = 0;
  condHandler.giveUpPacketCount_ = 0;
}

