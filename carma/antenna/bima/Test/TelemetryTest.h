/** @file
 * $Id: TelemetryTest.h,v 1.2 2006/12/04 20:16:56 colby Exp $
 *
 * CppUnit test fixture for carma::antenna::bima::TelemetryTest
 *
 * Author: Colby Gutierrez-Kraybill
 * Version: $Revision: 1.2 $
 * $Date: 2006/12/04 20:16:56 $
 *
 */
#ifndef CARMA_ANTENNA_BIMA_ANTENNANAMERESOLVER_TEST_H
#define CARMA_ANTENNA_BIMA_ANTENNANAMERESOLVER_TEST_H

#include "carma/antenna/bima/Telemetry.h"

#ifndef CPPUNIT_STD_NEED_ALLOCATOR
#define CPPUNIT_STD_NEED_ALLOCATOR 0
#endif

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

/**
 * carma::antenna::bima::TelemetryTest test class for CppUnit.
 */
class TelemetryTest : public CppUnit::TestFixture
{
  public:	

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();

    CPPUNIT_TEST_SUITE(TelemetryTest);

    CPPUNIT_TEST( testProcessMsgs );

    CPPUNIT_TEST_SUITE_END();

    /** exercise */
    void testProcessMsgs();

  private:
    carma::antenna::bima::Telemetry *_tT;

};
#endif // CARMA_ANTENNA_BIMA_ANTENNANAMERESOLVER_TEST_H
