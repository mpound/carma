#include "carma/szautil/FunctionName.h"

#include<iostream>
#include<sstream>

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
FunctionName::FunctionName(const char* prettyFunction) 
{
  prettyFunction_ = prettyFunction;
}

/**.......................................................................
 * Destructor.
 */
FunctionName::~FunctionName() {}

string FunctionName::prettyFunction()
{
  return prettyFunction_;
}

string FunctionName::noArgs()
{
  return noArgs(prettyFunction_.c_str());
}

string FunctionName::prettyFunction(const char* prettyFunction)
{
  return prettyFunction;
}

string FunctionName::noArgs(const char* prettyFunction)
{
  std::string func(prettyFunction);
  ostringstream os;
  os << func.substr(0,func.find("(", 0));
  os << "()";
  return os.str();
}

