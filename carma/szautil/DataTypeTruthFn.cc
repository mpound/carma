#include "carma/szautil/DataTypeTruthFn.h"
#include "carma/szautil/Debug.h"
#include <iomanip>

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructors.
 */
DataTypeTruthFn::DataTypeTruthFn(DATATYPE_TRUTH_FN0(*fn))
{
  fn_.fn0_ = (fn == 0 ? alwaysTrue : fn);
  type_ = FN0;
}

DataTypeTruthFn::DataTypeTruthFn(DATATYPE_TRUTH_FN1(*fn))
{
  fn_.fn1_ = fn;
  type_ = FN1;
}

DataTypeTruthFn::DataTypeTruthFn(DATATYPE_TRUTH_FN2(*fn))
{
  fn_.fn2_ = fn;
  type_ = FN2;
}

/**.......................................................................
 * Destructor.
 */
DataTypeTruthFn::~DataTypeTruthFn() {}

bool DataTypeTruthFn::evaluate(DataType& compVal)
{
  checkType(FN0);
  return fn_.fn0_(compVal);
}

bool DataTypeTruthFn::evaluate(DataType& compVal, DataType& op1)
{
  checkType(FN1);
  return fn_.fn1_(compVal, op1);
}

bool DataTypeTruthFn::evaluate(DataType& compVal, DataType& op1, DataType& op2)
{
  checkType(FN2);
  return fn_.fn2_(compVal, op1, op2);
}

void DataTypeTruthFn::checkType(Type type)
{
  if(type_ != type)
    ThrowError((type==FN0 ? "Argument is " : "Arguments are ") 
	       << "incompatible with this function type");
}

bool DataTypeTruthFn::
alwaysTrue(DataType& compVal)
{
  return true;
}

bool DataTypeTruthFn::
equals(DataType& compVal, DataType& op1)
{
  return compVal == op1;
}

bool DataTypeTruthFn::
greaterThan(DataType& compVal, DataType& op1)
{
  return compVal > op1;
}

bool DataTypeTruthFn::
lessThan(DataType& compVal, DataType& op1)
{
  return compVal < op1;
}

bool DataTypeTruthFn::
greaterThanEq(DataType& compVal, DataType& op1)
{
  return compVal >= op1;
}

bool DataTypeTruthFn::
lessThanEq(DataType& compVal, DataType& op1)
{
  return compVal <= op1;
}

bool DataTypeTruthFn::
greaterThanAndLessThan(DataType& compVal, DataType& op1, DataType& op2)
{
  return compVal > op1 && compVal < op2;
}

bool DataTypeTruthFn::
greaterThanAndLessThanEq(DataType& compVal, DataType& op1, DataType& op2)
{
  return compVal > op1 && compVal <= op2;
}

bool DataTypeTruthFn::
greaterThanEqAndLessThan(DataType& compVal, DataType& op1, DataType& op2)
{
  return compVal >= op1 && compVal < op2;
}

bool DataTypeTruthFn::
greaterThanEqAndLessThanEq(DataType& compVal, DataType& op1, DataType& op2)
{
  return compVal >= op1 && compVal <= op2;
}

bool DataTypeTruthFn::
lessThanOrGreaterThan(DataType& compVal, DataType& op1, DataType& op2)
{
  return compVal < op1 || compVal > op2;
}

bool DataTypeTruthFn::
lessThanEqOrGreaterThan(DataType& compVal, DataType& op1, DataType& op2)
{
  return compVal <= op1 || compVal > op2;
}

bool DataTypeTruthFn::
lessThanOrGreaterThanEq(DataType& compVal, DataType& op1, DataType& op2)
{
  return compVal < op1 || compVal >= op2;
}

bool DataTypeTruthFn::
lessThanEqOrGreaterThanEq(DataType& compVal, DataType& op1, DataType& op2)
{
  return compVal <= op1 || compVal >= op2;
}

bool DataTypeTruthFn::isZeroValued()
{
  return type_ == FN0;
}

bool DataTypeTruthFn::isSingleValued()
{
  return type_ == FN1;
}

bool DataTypeTruthFn::isDualValued()
{
  return type_ == FN2;
}

std::string DataTypeTruthFn::format(std::string& reg, DataType& op1, DataType& op2)
{
  switch(type_) {
  case FN0:
    return formatZeroValuedFn(reg);
    break;
  case FN1:
    return formatSingleValuedFn(reg, op1);
    break;
  case FN2:
    return formatDualValuedFn(reg, op1, op2);
    break;
  default:
    ThrowError("Unrecognized function type");
    break;
  }
}

std::string DataTypeTruthFn::formatZeroValuedFn(std::string& reg)
{
  std::ostringstream os;

  os << formatReg(reg);

  return os.str();
}

std::string DataTypeTruthFn::formatSingleValuedFn(std::string& reg, DataType& op)
{
  std::ostringstream os;

  os << formatReg(reg) << formatOp(fn_.fn1_) << formatVal(op);

  return os.str();
}

std::string DataTypeTruthFn::formatDualValuedFn(std::string& reg, DataType& op1, DataType& op2)
{
  std::ostringstream os;

  os << formatReg(reg) << formatOp1(fn_.fn2_) << formatVal(op1) << formatLogical(fn_.fn2_)
     << formatOp2(fn_.fn2_) << formatVal(op2);

  return os.str();
}

std::string DataTypeTruthFn::formatOp(DATATYPE_TRUTH_FN1(*fn_))
{
  std::ostringstream os;

  if(fn_ == equals)
    return "==";

  else if(fn_ == greaterThan)
    return ">";

  else if(fn_ == lessThan)
    return "<";

  else if(fn_ == greaterThanEq)
    return ">=";

  else if(fn_ == lessThanEq)
    return "<=";
}

std::string DataTypeTruthFn::formatOp1(DATATYPE_TRUTH_FN2(*fn_))
{
  std::ostringstream os;
  std::string opStr;

  if(fn_ == greaterThanAndLessThan)
    opStr = ">";

  else if(fn_ == greaterThanAndLessThanEq)
    opStr = ">";

  else if(fn_ == greaterThanEqAndLessThan)
    opStr = ">=";

  else if(fn_ == greaterThanEqAndLessThanEq)
    opStr = ">=";

  else if(fn_ == lessThanOrGreaterThan)
    opStr = "<";

  else if(fn_ == lessThanEqOrGreaterThan)
    opStr = "<=";

  else if(fn_ == lessThanOrGreaterThanEq)
    opStr = "<";

  else if(fn_ == lessThanEqOrGreaterThanEq)
    opStr = "<=";

  os << std::left << std::setw(4) << opStr;
  return os.str();
}

std::string DataTypeTruthFn::formatOp2(DATATYPE_TRUTH_FN2(*fn_))
{
  std::ostringstream os;
  std::string opStr;

  if(fn_ == greaterThanAndLessThan)
    opStr = "<";

  else if(fn_ == greaterThanAndLessThanEq)
    opStr = "<=";

  else if(fn_ == greaterThanEqAndLessThan)
    opStr = "<";

  else if(fn_ == greaterThanEqAndLessThanEq)
    opStr = "<=";

  else if(fn_ == lessThanOrGreaterThan)
    opStr = ">";

  else if(fn_ == lessThanEqOrGreaterThan)
    opStr = ">";

  else if(fn_ == lessThanOrGreaterThanEq)
    opStr = ">=";

  else if(fn_ == lessThanEqOrGreaterThanEq)
    opStr = ">=";

  os << " " << std::left << std::setw(2) << opStr << " ";
  return os.str();
}

std::string DataTypeTruthFn::formatLogical(DATATYPE_TRUTH_FN2(*fn_))
{
  std::string logStr;
  std::ostringstream os;

  if(fn_ == greaterThanAndLessThan)
    logStr = "&&";

  else if(fn_ == greaterThanAndLessThanEq)
    logStr = "&&";

  else if(fn_ == greaterThanEqAndLessThan)
    logStr = "&&";

  else if(fn_ == greaterThanEqAndLessThanEq)
    logStr = "&&";

  else if(fn_ == lessThanOrGreaterThan)
    logStr = "||";

  else if(fn_ == lessThanEqOrGreaterThan)
    logStr = "||";

  else if(fn_ == lessThanOrGreaterThanEq)
    logStr = "||";

  else if(fn_ == lessThanEqOrGreaterThanEq)
    logStr = "||";

  os << " " << logStr << " ";
  return os.str();
}

std::string DataTypeTruthFn::formatVal(DataType& val)
{
  std::ostringstream os;
  os << " " << std::setw(8) << val << " ";
  return os.str();
}

std::string DataTypeTruthFn::formatReg(std::string& reg)
{
  std::ostringstream os;
  os << " " << std::left << std::setw(50) << reg;
  return os.str();
}
