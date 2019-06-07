/** @file
 * $Id: AtmosphereTest.h,v 1.1 2010/02/18 22:14:09 abeard Exp $
 *
 * CppUnit test fixture for carma::environment::Atmosphere 
 *
 * Author: Marc Pound
 * Version: $Revision: 1.1 $
 * $Data: $
 *
 */
#ifndef CARMA_SERVICES_ATMOSPHERE_TEST_H
#define CARMA_SERVICES_ATMOSPHERE_TEST_H

#include "carma/services/Atmosphere.h"

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

/**
 * carma::services::Atmosphere test class for CppUnit.
 */
class AtmosphereTest : public CppUnit::TestFixture {
public:	

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();
	 
    CPPUNIT_TEST_SUITE(AtmosphereTest);

    CPPUNIT_TEST( testSafeRoutines );
    CPPUNIT_TEST( testSatPressure );
    CPPUNIT_TEST( testZenithRefractivity );
    CPPUNIT_TEST( testPathlength );
    CPPUNIT_TEST( testWaterRoutines );
    // ignore for now until i figure out ALMA problem
//    CPPUNIT_TEST( testRefractionCorrection );

    CPPUNIT_TEST_SUITE_END();
	
    /** test the "safeXX" routines that return good weather values */
    void testSafeRoutines();

    /** test the saturated pressure calculation */
    void testSatPressure();

    /** test the zenith refractivity calculation */
    void testZenithRefractivity();

    /** test the refraction correction calculation */
    void testRefractionCorrection();

    /** test the path length calculation */
    void testPathlength();

    /** test the water routines*/
    void testWaterRoutines();
	
private:

    // This must be long double because we are taking
    // differences resulting in a very small number.
    // Double representation "fluff" in the last digit
    // will move up or down in the mantissa depending
    // on the actual value in the comparison. Therefore
    // we normalize the comparison by the return value.
    long double normalizedDiff; 

    // How small we allow diff to be and still pass a test.
    double epsilon; 
	
    carma::services::Atmosphere* tAtm;
	
};
#endif //CARMA_SERVICES_ATMOSPHERE_TEST_H
