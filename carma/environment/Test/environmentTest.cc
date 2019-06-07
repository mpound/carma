
/**
 * @file
 * Implementation of the unit tests for the environment library
 *
 * @author: Marc Pound
 *
 * $Id: environmentTest.cc,v 1.10 2011/05/04 20:36:23 iws Exp $
 *
 * $CarmaCopyright$
 *
 */

// @version $Revision: 1.10 $
// @usage   environmentTest <no parameters>
// @description
// Test for the environment library
// @logger ENVIRONMENT_FACILITY carma.environment.Test
//
// @noKeys
//

#include <cppunit/TextTestRunner.h>
#include <carma/util/Program.h>
using namespace carma::util;

#include "DPSTest.h"
#include "WSTest.h"

#include <unistd.h>

int Program::main()
{
    CppUnit::TextTestRunner runner;

    // Add in test suites here
    runner.addTest( DPSTest::suite() );
    runner.addTest( WSTest::suite() );

    bool ok = runner.run();

    return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}
