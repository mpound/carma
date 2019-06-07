/** @file
 * $Id: SourceCheckerTest.h,v 1.7 2009/03/24 00:17:34 mpound Exp $
 *
 * CppUnit test fixture for carma::services::SourceChecker
 * and IERStable.
 *
 * @author: Marc Pound
 * @version $Revision: 1.7 $
 */
#ifndef CARMA_SERVICES_SOURCECHECKER_TEST_H
#define CARMA_SERVICES_SOURCECHECKER_TEST_H

#include "carma/services/SourceChecker.h"
#include "carma/services/TelescopeStatus.h"
#include "carma/services/Types.h"

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

/**
 * carma::services::SourceChecker test class for CppUnit.
 */
class SourceCheckerTest : public CppUnit::TestFixture {
 public:

  /** initialization required by CppUnit::TestFixture */
  void setUp();

  /** clean up required by CppUnit::TestFixture */
  void tearDown();
	 
  CPPUNIT_TEST_SUITE( SourceCheckerTest );

  CPPUNIT_TEST( testAzWrapLimits );
  CPPUNIT_TEST( testZenithBlindSpot );
  CPPUNIT_TEST( testOptimumWrapValues );
  CPPUNIT_TEST( bug571Test );
  CPPUNIT_TEST( testGetNearest );

  CPPUNIT_TEST_SUITE_END();
	
  /** test azimuth wrap code */
  void testAzWrapLimits( void );

  /** test blind spot calculations */
  void testZenithBlindSpot( void );

  /** 
   * test optimum wrap value calculation 
   * for all possible antenna types 
   * and azimuth quadrants
   */
  void testOptimumWrapValues( void );

  /** 
   * compute optimum wrap values exactly the
   * way control::DriveHandle does.  This substantially
   * uses the SourceChecker class.
   * @param checker intialized SourceChecker 
   * @param antType antenna type enum value
   * @param antAzDegrees antenna current azimuth in degrees
   * @param timeToTrack time required to track the source in minutes.
   */
  carma::services::AzWrapType computeOptimumWrapValue( 
	    carma::services::SourceChecker & checker,
	    carma::services::AntennaType antType,
	    double antAzDegrees,
	    double timeToTrack	
	    );

  // called by testOptimumWrapValues
  void testOvroOptimumWrapValues( void );
  void testBimaOptimumWrapValues( void );

  // regression test for bug 571
  void bug571Test( void );
  std::string pMode( const carma::services::AzWrapType ) const;

  // test getNearest function
  void testGetNearest();

};
#endif //CARMA_SERVICES_TABLE_SOURCECHECKER_H
