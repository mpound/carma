/**
 * @file
 * Implementation of the unit tests for the carma::dbms::Filter class
 *
 * @author: Dave Mehringer
 * @version $Id: tFilter.cc,v 1.4 2006/10/02 16:46:24 tcosta Exp $
 *
 * $CarmaCopyright$
 *
 */

// @usage   tFilter
// @description 
// Test for the carma::dbms filter classes
//
// @noKeys
//
// @logger TEST_FACILITY carma.test.dbms.tFilter

#include <cppunit/TextTestRunner.h>
#include "carma/util/Program.h"
#include "FilterTest.h"

using namespace std;
using namespace carma::util;

int Program::main() {
    CppUnit::TextTestRunner runner;
    // Add in test suites here
    runner.addTest( FilterTest::suite() );
    bool ok = runner.run();
    return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}

