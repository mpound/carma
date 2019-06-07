
/**
 * @file carma/antenna/bima/Test/anrTest.cc
 * Implementation of the unit tests for the BIMA Rx library
 *
 * @author: Colby Gutierrez-Kraybill
 *
 * $Id: anrTest.cc,v 1.1 2006/12/02 05:33:05 colby Exp $
 *                  
 * $CarmaCopyright$
 *
 */

// @version $Revision: 1.1 $
// @usage   anrTest <no parameters>
// @description 
// Test for the BIMA AntennaNameResolver library
// @logger DEFAULT_FACILITY carma.bima.anrTest
//
// @noKeys
//

#ifndef CPPUNIT_STD_NEED_ALLOCATOR
#define CPPUNIT_STD_NEED_ALLOCATOR 0
#endif

#include <cppunit/TextTestRunner.h>
#include "carma/util/Program.h"

#include "AntennaNameResolverTest.h"

#include <unistd.h>

using namespace carma::util;

int Program::main() 
{
    CppUnit::TextTestRunner runner;

    // Add in test suites here
    runner.addTest( AntennaNameResolverTest::suite() );

    bool ok = runner.run();

    return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}

