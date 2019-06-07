
/**
 * @file
 * Implementation of the unit tests for the inteferometry library
 *
 * @author: Marc Pound
 *
 * $Id: interferometryTest.cc,v 1.7 2006/10/02 16:46:24 tcosta Exp $
 *                  
 * $CarmaCopyright$
 *
 */

// @version $Revision: 1.7 $
// @usage   interferometryTest <no parameters>
// @description 
// Test for the environment library
//
// @noKeys
// @logger TEST_FACILITY carma.test.interferometry.interferometryTest
//

#include <cppunit/TextTestRunner.h>
#include "carma/util/Program.h"
#include "DelayInfoTest.h"

#include <unistd.h>

using namespace carma::util;



int Program::main() 
{
    CppUnit::TextTestRunner runner;

    // Add in test suites here
    runner.addTest( DelayInfoTest::suite() );

    runner.run();

    return EXIT_SUCCESS;
}

