
/**
 * 
 * Implementation of the unit tests for the utility library
 *
 * @author: Steve Scott
 *
 * $Id: utilTest.cc,v 1.11 2006/10/02 16:46:24 tcosta Exp $ 
 *                  
 * $CarmaCopyright$
 *
 */

// @version $revision$
// @usage   utilTest <no parameters>
// @description 
// Test for the utility library
//
// @noKeys
//
// @logger TEST_FACILITY carma.test.util.utilTest
//

#include <cppunit/TextTestRunner.h>

#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"

#include "ErrorTest.h"
#include "IPQtest.h"
#include "ObserverTest.h"

#include <unistd.h>

using namespace carma::util;



int Program::main() 
{
    CppUnit::TextTestRunner runner;

    // Add in test suites here
    runner.addTest( ErrorTest::suite() );
    runner.addTest( IPQtest::suite() );
    runner.addTest( ObserverTest::suite() );

    const bool wasSuccessful = runner.run();

    return (wasSuccessful ? EXIT_SUCCESS : EXIT_FAILURE);
}

