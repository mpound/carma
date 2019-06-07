
/**
 * @file carma/phasemonitor/Test/phasemonitorTest.cc
 * Implementation of the unit tests for the phasemonitor library
 *
 * @author: Colby Gutierrez-Kraybill
 *
 * $Id: phasemonitorTest.cc,v 1.4 2009/06/26 09:32:36 colby Exp $
 *                  
 * $CarmaCopyright$
 *
 */

// @version $Revision: 1.4 $
// @usage   phasemonitorTest <no parameters>
// @description 
// Test for the phasemonitor library
// @logger ENVIRONMENT_FACILITY carma.phasemonitor.Test  
//
// @noKeys
//

#ifndef CPPUNIT_STD_NEED_ALLOCATOR
#define CPPUNIT_STD_NEED_ALLOCATOR 0
#endif

#include <cppunit/TextTestRunner.h>
#include "carma/util/Program.h"
#include "AntennaParametersTest.h"
#include "PhaseMonitorDeviceTest.h"
#include "PhaseMonitorWorkerTest.h"
#include "PhaseMonitorSamplesTest.h"

#include <unistd.h>

using namespace carma::util;

int Program::main() 
{
    CppUnit::TextTestRunner runner;

    // Add in test suites here
    runner.addTest( AntennaParametersTest::suite() );
    runner.addTest( PhaseMonitorDeviceTest::suite() );
// Need to re-work sample result test
// after introduction of duplicate samples check
//    runner.addTest( PhaseMonitorWorkerTest::suite() );
//    runner.addTest( PhaseMonitorSamplesTest::suite() );

    bool ok = runner.run();

    return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}

