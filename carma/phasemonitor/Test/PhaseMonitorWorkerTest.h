/** @file
 * $Id: PhaseMonitorWorkerTest.h,v 1.1 2006/11/30 04:49:50 colby Exp $
 *
 * CppUnit test fixture for carma::phasemonitor::PhaseMonitorWorker
 *
 * Author: Colby Gutierrez-Kraybill
 * Version: $Revision: 1.1 $
 * $Date: 2006/11/30 04:49:50 $
 *
 */
#ifndef CARMA_PHASEMONITOR_PHASEMONITORWORKER_TEST_H
#define CARMA_PHASEMONITOR_PHASEMONITORWORKER_TEST_H

#include "carma/phasemonitor/AntennaParameters.h"
#include "carma/phasemonitor/PhaseMonitorDevice.h"
#include "carma/phasemonitor/PhaseMonitorWorker.h"
#include "carma/phasemonitor/PhaseMonitorSamples.h"

#ifndef CPPUNIT_STD_NEED_ALLOCATOR
#define CPPUNIT_STD_NEED_ALLOCATOR 0
#endif

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

/**
 * carma::phasemonitor::PhaseMonitorWorker test class for CppUnit.
 */
class PhaseMonitorWorkerTest : public CppUnit::TestFixture
{
  public:	

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();

    CPPUNIT_TEST_SUITE(PhaseMonitorWorkerTest);

    CPPUNIT_TEST( testGetPhases );
    CPPUNIT_TEST( testPhaseJumpCheck );
    CPPUNIT_TEST( testPMWECopyAndString );
    CPPUNIT_TEST( testOstream );

    CPPUNIT_TEST_SUITE_END();

    /** test that getPhases performs as expected */
    void testGetPhases();

    /** test that phaseJumpCheck works */
    void testPhaseJumpCheck();

    /** test PhaseMonitorWorkerException c'tors works */
    void testPMWECopyAndString();

    /** test ostream operator */
    void testOstream();

  private:

    // This must be long double because we are taking
    // differences resulting in a very small number.
    // Double representation "fluff" in the last digit
    // will move up or down in the mantissa depending
    // on the actual value in the comparison. Therefore
    // we normalize the comparison by the return value.
    long double _normalizedDiff; 

    // How small we allow diff to be and still pass a test.
    double _epsilon; 

    carma::phasemonitor::PhaseMonitorDevice* _tPMD;
    carma::phasemonitor::PhaseMonitorWorker* _tPMW;
    carma::phasemonitor::PhaseMonitorSamples* _tPMS;

    char *_nameTemplate;
    bool _tearDownNameTemplate;
    bool _tearDownUnlinkNameTemplate;

};
#endif // CARMA_PHASEMONITOR_PHASEMONITORWORKER_TEST_H
