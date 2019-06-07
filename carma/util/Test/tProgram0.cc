// this is probably the simplest CARMA program ...
//
// note there are no command line arguments, no help,
// no nothing (needless to say, the authors of Program:: 
// find this behavior abnormal, to say the least).
//
// Please look at tProgram1.cc or tProgram2.cc how it
// really ought to look!
// 
// @noKeys
//
// @logger TEST_FACILITY carma.test.util.tProgram0
//
#include <iostream>

#include "carma/util/Program.h"

using namespace std;
using namespace carma::util;

int Program::main()
{
  cout << "Hello CARMA" << endl;
  return 0;
}


