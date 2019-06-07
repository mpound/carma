/** @file
 * $Id: EphemerisTest.h,v 1.3 2007/10/05 15:00:28 mpound Exp $
 *
 * CppUnit test fixture for carma::services::Ephemeris
 * and IERStable.
 *
 * Author: Peter Teuben
 * Version: $Revision: 1.3 $
 * $Date: 2007/10/05 15:00:28 $
 *
 */
#ifndef CARMA_SERVICES_EPHEMERIS_TEST_H
#define CARMA_SERVICES_EPHEMERIS_TEST_H

#include "carma/services/Ephemeris.h"
#include "carma/services/IERSTable.h"

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

/**
 * carma::services::Ephemeris test class for CppUnit.
 */
class EphemerisTest : public CppUnit::TestFixture {
 public:
  /** initialization required by CppUnit::TestFixture */
  void setUp();

  /** clean up required by CppUnit::TestFixture */
  void tearDown();
	 
  CPPUNIT_TEST_SUITE(EphemerisTest);

  CPPUNIT_TEST( testEph1 );
  CPPUNIT_TEST( testIERS );

  CPPUNIT_TEST_SUITE_END();
	
  /** test apparent ra/dec  */
  void testEph1();

  /** 
   * Check validity of current IERS table. Nice side effect
   * that build will go orange when it is time to update
   * this table. 
   */
  void testIERS();
  
 private:
  carma::services::Ephemeris *tEph;
};
#endif //CARMA_SERVICES_TABLE_EPHEMERIS_H
