/**
 * @file
 * Implementation of the unit tests for the carma::dbms::Table class
 *
 * @author: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */

// @usage   dbmsTest 
// @description 
// Test for the dbms library
//
// @noKeys
//
// @logger TEST_FACILITY carma.test.dbms.tTable

#include <cppunit/TextTestRunner.h>
#include "carma/util/Program.h"
#include "TableTest.h"

#include <unistd.h>

using namespace std;
using namespace carma::dbms;
using namespace carma::util;

int Program::main() {
    CppUnit::TextTestRunner runner;
    // Add in test suites here
    runner.addTest( TableTest::suite() );
    bool ok = runner.run();
    return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}

