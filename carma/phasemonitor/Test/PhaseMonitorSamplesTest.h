/** @file
 * $Id: PhaseMonitorSamplesTest.h,v 1.2 2006/11/30 04:49:50 colby Exp $
 *
 * CppUnit test fixture for carma::phasemonitor::PhaseMonitorSamples
 *
 * Author: Colby Gutierrez-Kraybill
 * Version: $Revision: 1.2 $
 * $Date: 2006/11/30 04:49:50 $
 *
 */
#ifndef CARMA_PHASEMONITOR_PHASEMONITORSAMPLES_TEST_H
#define CARMA_PHASEMONITOR_PHASEMONITORSAMPLES_TEST_H

#include "carma/util/Time.h"

#include "carma/phasemonitor/PhaseMonitorSamples.h"
#include "carma/phasemonitor/PhaseMonitorWorker.h"

#ifndef CPPUNIT_STD_NEED_ALLOCATOR
#define CPPUNIT_STD_NEED_ALLOCATOR 0
#endif

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

/**
 * carma::phasemonitor::PhaseMonitorSamples test class for CppUnit.
 */
class PhaseMonitorSamplesTest : public CppUnit::TestFixture
{
  public:	

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();

    CPPUNIT_TEST_SUITE(PhaseMonitorSamplesTest);

    CPPUNIT_TEST( testProcess );
    CPPUNIT_TEST( testPMSECopyAndString );

    CPPUNIT_TEST_SUITE_END();

    /** test samples processing routine with sample data */
    void testProcess();

  private:
    /** test PhaseMonitorSamplesException overloaded c'tors */
    void testPMSECopyAndString();

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

    carma::phasemonitor::PhaseMonitorSamples *_tPMS;
};
#endif // CARMA_PHASEMONITOR_PHASEMONITORSAMPLES_TEST_H
