
/**
 * @file carma/antenna/bima/Test/telemetryTest.cc
 * Implementation of the unit tests for the BIMA Rx library
 *
 * @author: Colby Gutierrez-Kraybill
 *
 * $Id: telemetryTest.cc,v 1.1 2006/12/02 08:18:32 colby Exp $
 *                  
 * $CarmaCopyright$
 *
 */

// @version $Revision: 1.1 $
// @usage   telemetryTest <no parameters>
// @description 
// Test for the BIMA Telemetry library
// @logger DEFAULT_FACILITY carma.bima.telemetryTest
//
// @noKeys
//

#ifndef CPPUNIT_STD_NEED_ALLOCATOR
#define CPPUNIT_STD_NEED_ALLOCATOR 0
#endif

#include <cppunit/TextTestRunner.h>
#include "carma/util/Program.h"

#include "TelemetryTest.h"

#include <unistd.h>

using namespace carma::util;

int Program::main() 
{
    CppUnit::TextTestRunner runner;

    // Add in test suites here
    runner.addTest( TelemetryTest::suite() );

    bool ok = runner.run();

    return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}

