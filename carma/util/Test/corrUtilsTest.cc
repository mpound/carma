
/**
 * @file
 * Implementation of the unit tests for the correlator utils
 *
 * @author: Marc Pound
 *
 *                  
 * $CarmaCopyright$
 *
 */

// @usage   correlatorlibTest <no parameters>
// @description 
// Test for the correlator/lib library
//
// @noKeys
// @logger DEFAULT_FACILITY carma.util.Test.corrUtilsTest
//

#include <cppunit/TextTestRunner.h>
#include "carma/util/Program.h"

#include "CorrUtilsTest.h"

#include <unistd.h>

using namespace carma::util;

int Program::main() 
{
    CppUnit::TextTestRunner runner;

    // Add in test suites here.  For any class Foo, you
    // must create carma::correlator::lib::test::FooTest,
    // following the existing examples.  Also don't forget
    // to add the #include "FooTest.h" statement above.
    
    runner.addTest( CorrUtilsTest::suite() );

    bool ok = runner.run();

    return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}

