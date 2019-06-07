#include "carma/szautil/Debug.h"
#include "carma/szautil/MonitorCondition.h"

#include <iomanip>

using namespace std;
using namespace sza::util;

/**
 * Constructor for no condition
 */
MonitorCondition::MonitorCondition(unsigned packetCount, 
				   unsigned stablePacketCount,
				   unsigned giveUpPacketCount)
{
  setTo(packetCount, stablePacketCount, giveUpPacketCount);
}

void MonitorCondition::setTo(unsigned packetCount, 
			     unsigned stablePacketCount,
			     unsigned giveUpPacketCount)
{
  checkPacketCount(packetCount, stablePacketCount, giveUpPacketCount);

  // The default constructor defaults to a condition which is always
  // true

  fn_ = sza::util::DataTypeTruthFn::alwaysTrue;

  isDeltaCondition_  = false;
  first_ = true;

  packetCount_       = packetCount;
  stablePacketCount_ = stablePacketCount;
  giveUpPacketCount_ = giveUpPacketCount;
}

/**
 * Constructor for a single-valued condition
 */
MonitorCondition::MonitorCondition(DataTypeTruthFn fn, 
				   DataType op1, 
				   bool delta,
				   unsigned packetCount, 
				   unsigned stablePacketCount,
				   unsigned giveUpPacketCount)
{
  setTo(fn, op1, delta, packetCount, stablePacketCount, giveUpPacketCount);
}

void MonitorCondition::setTo(DataTypeTruthFn fn, 
			     DataType op1, 
			     bool delta,
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

  isDeltaCondition_  = delta;
  first_ = true;

  packetCount_       = packetCount;
  stablePacketCount_ = stablePacketCount;
  giveUpPacketCount_ = giveUpPacketCount;
}

/**
 * Constructor for a dual-valued condition
 */
MonitorCondition::MonitorCondition(DataTypeTruthFn fn, 
				   DataType op1, DataType op2, 
				   bool delta,
				   unsigned packetCount, 
				   unsigned stablePacketCount,
				   unsigned giveUpPacketCount)
{
  setTo(fn, op1, op2, delta, packetCount, stablePacketCount, giveUpPacketCount);
}

void MonitorCondition::setTo(DataTypeTruthFn fn, 
			     DataType op1, DataType op2, 
			     bool delta,
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

  isDeltaCondition_  = delta;
  first_ = true;

  packetCount_       = packetCount;
  stablePacketCount_ = stablePacketCount;
  giveUpPacketCount_ = giveUpPacketCount;
}

void MonitorCondition::operator=(const MonitorCondition& condition)
{
  fn_  = condition.fn_;
  op1_ = condition.op1_;
  op2_ = condition.op2_;

  isDeltaCondition_  = condition.isDeltaCondition_;
  first_ = true;

  packetCount_       = condition.packetCount_;
  stablePacketCount_ = condition.stablePacketCount_;
  giveUpPacketCount_ = condition.giveUpPacketCount_;
}

/**.......................................................................
 * Destructor.
 */
MonitorCondition::~MonitorCondition() {}

/**.......................................................................
 * Check if a value satisfies the requested condition
 */
bool MonitorCondition::isSatisfiedBy(sza::util::DataType& dataType)
{
  bool status=false;

  // If this was a condition on the delta of a register value, store
  // the current value

  DataType current = dataType;

  // If this was a condition on the delta of a register value, test
  // the difference between the current value and the last

  if(isDeltaCondition_) {

    if(first_) {
      last_  = dataType;
      first_ = false;
    } 

    // Subtract the last value from the current values

    dataType -= last_;

    // And take the absolute value

    dataType.convertToAbs();
  }

  if(fn_.isZeroValued()) {
    status = fn_.evaluate(dataType);
#if 0
    if(status)
      COUT(dataType);
#endif
  } else if(fn_.isSingleValued()) {
    status = fn_.evaluate(dataType, op1_);
#if 0
    if(status)
      COUT(dataType << " " << op1_);
#endif
  } else if(fn_.isDualValued()) {
    status = fn_.evaluate(dataType, op1_, op2_);
#if 0
    if(status)
      COUT(dataType << " " << op1_ << " " << op2_);
#endif
  }

  // If this was a condition on the delta of a register value, store
  // the current value as the last value

  if(isDeltaCondition_)
    last_ = current;

  return status;
}

/**.......................................................................
 * Check the packet count to see if it makes sense
 */
void MonitorCondition::checkPacketCount(unsigned packetCount, 
					unsigned stablePacketCount, 
					unsigned giveUpPacketCount)
{
  if(giveUpPacketCount > 0 && giveUpPacketCount < stablePacketCount)
    ThrowError("Stable packet count cannot be greater than the timeout threshold");
}

std::string MonitorCondition::format(std::string& reg)
{
  ostringstream os;

#if 0
  if(isDeltaCondition_) {
    os << "delta(" << reg << ")";
  }  else {
    os << "      " << reg;
  }
#else
  if(isDeltaCondition_) {
    os << "delta " << reg;
  }  else {
    os << "      " << reg;
  }
#endif

  std::string str = os.str();
  os.str("");

  os << fn_.format(str, op1_, op2_);
  os << " (for > " << std::right << std::setw(4) << stablePacketCount_ << " frames)";
  return os.str();
}
