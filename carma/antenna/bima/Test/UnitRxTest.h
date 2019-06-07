/** @file
 * $Id: UnitRxTest.h,v 1.3 2006/12/20 20:31:57 colby Exp $
 *
 * CppUnit test fixture for carma::antenna::bima::UnitRxTest
 *
 * Author: Colby Gutierrez-Kraybill
 * Version: $Revision: 1.3 $
 * $Date: 2006/12/20 20:31:57 $
 *
 */
#ifndef CARMA_ANTENNA_BIMA_UNITRX_TEST_H
#define CARMA_ANTENNA_BIMA_UNITRX_TEST_H

#include "carma/antenna/bima/Rx.h"
#include "carma/antenna/bima/LO.h"

#include <pthread.h>
#include <signal.h>

#ifndef CPPUNIT_STD_NEED_ALLOCATOR
#define CPPUNIT_STD_NEED_ALLOCATOR 0
#endif

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

/**
 * carma::antenna::bima::UnitRxTest test class for CppUnit.
 */
class UnitRxTest : public CppUnit::TestFixture
{
  public:	

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();

    CPPUNIT_TEST_SUITE(UnitRxTest);

    CPPUNIT_TEST( testLockX );
    CPPUNIT_TEST( testRxConfig );
    CPPUNIT_TEST( testTune );
    CPPUNIT_TEST( testSetLOTermAtten );
    CPPUNIT_TEST( testScanSIS );
    CPPUNIT_TEST( testRelockLogic );

    CPPUNIT_TEST_SUITE_END();

    /** test that Rx has read in its config info correctly */
    void testRxConfig();

    /** test exercise tune */
    void testTune();

    /** test exercise LOTermAtten set */
    void testSetLOTermAtten();

    /** test exercise scanSIS */
    void testScanSIS();

    /** exercise lockX */
    void testLockX();

    /** Test the routines used for relocking */
    void testRelockLogic();


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

    carma::antenna::bima::Rx *_tRx;
    carma::antenna::bima::LO *_tLO;
};
#endif // CARMA_ANTENNA_BIMA_UNITRX_TEST_H
