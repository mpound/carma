#include "carma/antenna/sza/antenna/canbus/CanMonitorCondition.h"

#include "carma/szautil/Debug.h"

using namespace std;
using namespace sza::util;
using namespace sza::antenna::canbus;

/**
 * Constructor for no condition
 */
CanMonitorCondition::CanMonitorCondition(unsigned packetCount, 
					 unsigned stablePacketCount,
					 unsigned giveUpPacketCount)
{
  setTo(packetCount, stablePacketCount, giveUpPacketCount);
}

void CanMonitorCondition::setTo(unsigned packetCount, 
				unsigned stablePacketCount,
				unsigned giveUpPacketCount)
{
  checkPacketCount(packetCount, stablePacketCount, giveUpPacketCount);

  // The default constructor defaults to a condition which is always
  // true

  fn_ = sza::util::DataTypeTruthFn::alwaysTrue;

  packetCount_       = packetCount;
  stablePacketCount_ = stablePacketCount;
  giveUpPacketCount_ = giveUpPacketCount;
}

/**
 * Constructor for a single-valued condition
 */
CanMonitorCondition::CanMonitorCondition(DataTypeTruthFn fn, DataType val, 
					 unsigned packetCount, 
					 unsigned stablePacketCount,
					 unsigned giveUpPacketCount)
{
  setTo(fn, val, packetCount, stablePacketCount, giveUpPacketCount);
}

void CanMonitorCondition::setTo(DataTypeTruthFn fn, 
				DataType op1, 
				unsigned packetCount, 
				unsigned stablePacketCount,
				unsigned giveUpPacketCount)
{
  // Check that the passed function and the arguments make sense

  if(!fn.isSingleValued())
    ThrowError("Operator is inconsistent with a single parameter");

  // Check that the packet count configuration doesn't guarantee that
  // the command will fail
  
  checkPacketCount(packetCount, stablePacketCount, giveUpPacketCount);

  fn_  = fn;
  op1_ = op1;

  packetCount_       = packetCount;
  stablePacketCount_ = stablePacketCount;
  giveUpPacketCount_ = giveUpPacketCount;
}

/**
 * Constructor for a dual-valued condition
 */
CanMonitorCondition::CanMonitorCondition(DataTypeTruthFn fn, 
					 DataType op1, DataType op2, 
					 unsigned packetCount, 
					 unsigned stablePacketCount,
					 unsigned giveUpPacketCount)
{
  setTo(fn, op1, op2, packetCount);
}

void CanMonitorCondition::setTo(DataTypeTruthFn fn, 
				DataType op1, DataType op2, 
				unsigned packetCount, 
				unsigned stablePacketCount,
				unsigned giveUpPacketCount)
{
  // Check that the passed function and the arguments make sense

  if(!fn.isDualValued())
    ThrowError("Operator is inconsistent with two parameters");

  // Check that the packet count configuration doesn't guarantee that
  // the command will fail

  checkPacketCount(packetCount, stablePacketCount, giveUpPacketCount);

  fn_  = fn;
  op1_ = op1;
  op2_ = op2;

  packetCount_       = packetCount;
  stablePacketCount_ = stablePacketCount;
  giveUpPacketCount_ = giveUpPacketCount;
}

void CanMonitorCondition::operator=(const CanMonitorCondition& condition)
{
  fn_  = condition.fn_;
  op1_ = condition.op1_;
  op2_ = condition.op2_;

  packetCount_       = condition.packetCount_;
  stablePacketCount_ = condition.stablePacketCount_;
  giveUpPacketCount_ = condition.giveUpPacketCount_;
}

/**.......................................................................
 * Destructor.
 */
CanMonitorCondition::~CanMonitorCondition() {}

/**.......................................................................
 * Check if a value satisfies the requested condition
 */
bool CanMonitorCondition::isSatisfiedBy(sza::util::DataType& dataType)
{
  if(fn_.isZeroValued()) {
    DBPRINT(true, Debug::DEBUG15, "function is zero-valued");
    return fn_.evaluate(dataType);
  } else if(fn_.isSingleValued()) {
    DBPRINT(true, Debug::DEBUG15, "function is single-valued");
    return fn_.evaluate(dataType, op1_);
  } else if(fn_.isDualValued()) {
    DBPRINT(true, Debug::DEBUG15, "function is dual-valued");
    return fn_.evaluate(dataType, op1_, op2_);
  }

  DBPRINT(true, Debug::DEBUG15, "function is neither");
  return false;
}

/**.......................................................................
 * Check the packet count to see if it makes sense
 */
void CanMonitorCondition::checkPacketCount(unsigned packetCount, 
					   unsigned stablePacketCount, 
					   unsigned giveUpPacketCount)
{
  if(giveUpPacketCount > 0 && giveUpPacketCount < stablePacketCount)
    ThrowError("Stable packet count cannot be greater than the timeout threshold");
}
