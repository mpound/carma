
/**
 * @file carma/antenna/bima/Test/unitRxTest.cc
 * Implementation of the unit tests for the BIMA Rx library
 *
 * @author: Colby Gutierrez-Kraybill
 *
 * $Id: unitRxTest.cc,v 1.1 2006/11/30 11:38:54 colby Exp $
 *                  
 * $CarmaCopyright$
 *
 */

// @version $Revision: 1.1 $
// @usage   unitRxTest <no parameters>
// @description 
// Test for the BIMA Rx library
// @logger DEFAULT_FACILITY carma.bima.unitRxTest
//
// @noKeys
//

#ifndef CPPUNIT_STD_NEED_ALLOCATOR
#define CPPUNIT_STD_NEED_ALLOCATOR 0
#endif

#include <cppunit/TextTestRunner.h>
#include "carma/util/Program.h"

#include "UnitRxTest.h"

#include <unistd.h>

using namespace carma::util;

int Program::main() 
{
    CppUnit::TextTestRunner runner;

    // Add in test suites here
    runner.addTest( UnitRxTest::suite() );

    bool ok = runner.run();

    return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}

