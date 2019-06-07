
/**
 * @file
 * Implementation of the unit tests for the correlator/lib library
 *
 * @author: Marc Pound
 *
 * $Id: correlatorlibTest.cc,v 1.1 2010/02/25 20:38:39 mpound Exp $
 *                  
 * $CarmaCopyright$
 *
 */

// @usage   correlatorlibTest <no parameters>
// @description 
// Test for the correlator/lib library
//
// @noKeys
// @logger DEFAULT_FACILITY carma.correlator.lib.Test.correlatorlibTest
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

