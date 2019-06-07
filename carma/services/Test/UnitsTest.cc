/** 
 * @file 
 *
 * $Id: UnitsTest.cc,v 1.9 2005/05/18 19:23:53 mpound Exp $
 *
 * CppUnit test fixture for carma::services::Units
 * For these tests we cannot use == since
 * we are testing floating point numbers. Therefore we test
 * that the normalizedDifference between the expected
 * answer and the returned answer is less than
 * numeric_limits.epsilon(), which is ~10^-7 for floats.
 * In all cases, we spot check over a range of input values
 * and compare returns against hand-calculated values.
 * Functions are monotonic, so spot checking is good enough.
 *
 * Author: Marc Pound
 * Version: $Revision: 1.9 $
 * $Date: 2005/05/18 19:23:53 $
 *
 */
#include <cmath>
#include <limits>
#include <iostream>
#include "UnitsTest.h"

#include "carma/services/ConformabilityException.h"

using namespace carma::services;
using namespace CppUnit;
using namespace ::std;

void UnitsTest::setUp() 
{   

    // Our tests are based on epsilon(), the smallest
    // double that is not zero.  We will test against
    // 10 times this because epsilon() ~ 10^-16 and the
    // last digit of a double representation is unreliable.
    numeric_limits<double> doubleLimits;
    epsilon = 10.0 * doubleLimits.epsilon();
}

void UnitsTest::tearDown() 
{   

}

void UnitsTest::testConversions()
{

    Units units;

    // do some spot checking of values
    testVal = M_PI;
    retVal = units.convert("180 degrees","radians");

    normalizedDiff = fabsl((testVal - retVal)/retVal);
    CPPUNIT_ASSERT( normalizedDiff < epsilon );

    retVal = units.convert(180, "degrees","radians");
    normalizedDiff = fabsl((testVal - retVal)/retVal);
    CPPUNIT_ASSERT( normalizedDiff < epsilon );

    testVal = 180.0;
    retVal = units.convert(M_PI,"radians", "degrees");
    normalizedDiff = fabsl((testVal - retVal)/retVal);
    CPPUNIT_ASSERT( normalizedDiff < epsilon );
    
    testVal = 8046.72;
    retVal = units.convert(5,"miles","meters");
    normalizedDiff = fabsl((testVal - retVal)/retVal);
    CPPUNIT_ASSERT( normalizedDiff < epsilon );

    testVal = 2.54;
    retVal = units.convert("1 inch","cm");
    normalizedDiff = fabsl((testVal - retVal)/retVal);
    CPPUNIT_ASSERT( normalizedDiff < epsilon );

    double aa = units.convert(M_PI,"radians","degrees");
    double bb = units.convert(3600.0,"arcsec","degrees");
    testVal = 181.0;
    retVal = aa + bb;
    normalizedDiff = fabsl((testVal - retVal)/retVal);
    CPPUNIT_ASSERT( normalizedDiff < epsilon );

    try {
    //svedberg  1e-13 s    # Used for measuring the sedimentation
    //	                   # coefficient for centrifuging.

    	retVal = units.convert("svedberg","avogadro");
    } catch (const ConformabilityException& e) {
        CPPUNIT_ASSERT( 1 == 1 );
	cout << "UnitsTest caught exception: \n"<< e.what()
	     << "(Don't worry, this is part of the test)"
	     << endl;
	// if we got to here then the test was succesful.
    } catch ( const std::exception& ex ) {
	cout << "UnitsTest caught exception: \n"<< ex.what() << endl;
	// if something else was thrown, the test failed.
        CPPUNIT_ASSERT( 1 == 0 ); 
    } 
}
