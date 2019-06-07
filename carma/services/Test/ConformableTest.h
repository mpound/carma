/** 
 * @file
 *
 * CppUnit test fixture for carma::services::ConformableQuantities
 * and its subclasses
 * $Id: ConformableTest.h,v 1.6 2006/12/01 21:08:56 mpound Exp $
 *
 * Author: Marc Pound
 *
 */
#ifndef CARMA_SERVICES_CONFORMABLE_TEST_H
#define CARMA_SERVICES_CONFORMABLE_TEST_H

#include "carma/services/ConformableQuantity.h"
#include "carma/services/Types.h"

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

/**
 * carma::services::Conformable test class for CppUnit.
 */
class ConformableTest : public CppUnit::TestFixture {
public:	

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();
	 
    CPPUNIT_TEST_SUITE(ConformableTest);

    CPPUNIT_TEST( testAngles );
    CPPUNIT_TEST( testDistance );
    CPPUNIT_TEST( testFrequency );
    CPPUNIT_TEST( testTemperature );
    CPPUNIT_TEST( testVelocity );


    CPPUNIT_TEST_SUITE_END();
	
    /** test the Angle class */
    void testAngles();

    /** test the Distance class */
    void testDistance();

    /** test the Frequency class */
    void testFrequency();

    /** test the Temperature class */
    void testTemperature();

    /** test the Velocity class */
    void testVelocity();

private:

    // return value and value to test against.
    double retVal, testVal;

    // This must be long double because we are taking
    // differences resulting in a very small number.
    // Double representation "fluff" in the last digit
    // will move up or down in the mantissa depending
    // on the actual value in the comparison. Therefore
    // we normalize the comparison by the return value.
    long double normalizedDiff; 

    // How small we allow diff to be and still pass a test.
    double epsilon; 
   

};
#endif //CARMA_SERVICES_CONFORMABLE_TEST_H
