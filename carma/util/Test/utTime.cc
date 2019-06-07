// $Id: utTime.cc,v 1.5 2006/02/23 22:51:23 tcosta Exp $


/**
 * Unit Test for Time 
 *   
 * @author: Steve Scott
 *
 * $CarmaCopyright$
 *
 */


#include "carma/util/Program.h"
#include "carma/util/Test/TimeTest.h"


//
// @noKeys
//
// @logger TEST_FACILITY carma.test.util.utTime
//

int carma::util::Program::main()
{
  TimeTest test("test1");

  test.runTest();

  return EXIT_SUCCESS;
}
