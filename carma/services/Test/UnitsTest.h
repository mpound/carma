/** @file
 * $Id: UnitsTest.h,v 1.3 2004/10/01 15:32:33 mpound Exp $
 *
 * CppUnit test fixture for carma::services::Units 
 *
 * Author: Marc Pound
 * Version: $Revision: 1.3 $
 * $Data: $
 *
 */
#ifndef CARMA_SERVICES_UNITS_TEST_H
#define CARMA_SERVICES_UNITS_TEST_H

#include "carma/services/Units.h"

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

/**
 * carma::services::Units test class for CppUnit.
 */
class UnitsTest : public CppUnit::TestFixture {
public:	

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();
	 
    CPPUNIT_TEST_SUITE(UnitsTest);

    CPPUNIT_TEST( testConversions );

    CPPUNIT_TEST_SUITE_END();
	

    /** test the units conversion routines */
    void testConversions();

private:

    // return value and value to test againats.
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
#endif //CARMA_SERVICES_UNITS_TEST_H
