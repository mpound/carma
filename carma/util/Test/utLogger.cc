/**
 *  @file
 *  Implementation of the unit tests for the logger.
 *  @author: Marc Pound
 *  
 *  $Id: utLogger.cc,v 1.6 2006/10/02 16:46:24 tcosta Exp $
 *  
 *  $CarmaCopyright$
 *   
 */

//--------------------------------------- 
// @version $Revision: 1.6 $
// @usage   utLogger <no parameters>
// @description 
// Test for the logger.
//--------------------------------------- 
// @noKeys
//
// @logger TEST_FACILITY carma.test.util.utLogger

#include "carma/util/Program.h"
#include "cppunit/TextTestRunner.h"
#include "LoggerTest.h"
#include <unistd.h>

using namespace carma::util;

int Program::main()
{
    CppUnit::TextTestRunner runner;

    // Add in test suites here
    runner.addTest( LoggerTest::suite() );
    const bool wasSuccessful = runner.run();

    return (wasSuccessful ? EXIT_SUCCESS : EXIT_FAILURE);
}

