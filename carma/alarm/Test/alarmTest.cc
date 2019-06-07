
/**
 * @file carma/alarm/Test/alarmTest.cc
 * Implementation of the unit tests for the alarm library
 *
 * @author: Colby Gutierrez-Kraybill
 *
 * $Id: alarmTest.cc,v 1.2 2010/07/22 18:02:22 iws Exp $
 *                  
 * $CarmaCopyright$
 *
 */

// @version $Revision: 1.2 $
// @usage   alarmTest <no parameters>
// @description 
// Test for the alarm library
// @logger DEFAULT_FACILITY carma.alarm.alarmTest
//
// @noKeys
//

#ifndef CPPUNIT_STD_NEED_ALLOCATOR
#define CPPUNIT_STD_NEED_ALLOCATOR 0
#endif

#include <cppunit/TextTestRunner.h>
#include "carma/util/Program.h"

#include "SoundsTableTest.h"

#include <unistd.h>

using namespace carma::util;

int Program::main() 
{
    CppUnit::TextTestRunner runner;

    // Add in test suites here
    runner.addTest( SoundsTableTest::suite() );

    bool ok = runner.run();

    return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}

