/** 
 * @file 
 *
 * $Id: PhysicalTest.cc,v 1.1 2004/09/09 19:11:55 mpound Exp $
 *
 * CppUnit test fixture for carma::services::Physical
 * For these tests we cannot use == since
 * we are testing floating point numbers. Therefore we test
 * that the difference between the expected
 * answer and the returned answer is less than
 * numeric_limits.epsilon(), which is ~10^-7 for floats.
 * In all cases, we spot check over a range of input values
 * and compare returns against hand-calculated values.
 * Functions are monotonic, so spot checking is good enough.
 *
 * Author: Marc Pound
 * Version: $Revision: 1.1 $
 * $Date: 2004/09/09 19:11:55 $
 *
 */
#include <cmath>
#include <limits>
#include <iostream>
#include "PhysicalTest.h"


using namespace carma::services::constants;
using namespace CppUnit;
using namespace std;

void PhysicalTest::setUp() 
{   

}

void PhysicalTest::tearDown() 
{   

}

void PhysicalTest::testConstants()
{

    numeric_limits<double> doubleLimits;
    double testVal, diff;

    testVal = 299792458.0;
    diff = abs(Physical::C - testVal);
    CPPUNIT_ASSERT( diff <= doubleLimits.epsilon() ); 

    testVal = 6.6742E-11;
    diff = abs(Physical::G  - testVal);
    CPPUNIT_ASSERT( diff <= doubleLimits.epsilon() ); 

    testVal = 1.3806505E-23;
    diff = abs(Physical::K - testVal);
    CPPUNIT_ASSERT( diff <= doubleLimits.epsilon() ); 

    testVal = 6.6260693E-34;
    diff = abs(Physical::H - testVal);
    CPPUNIT_ASSERT( diff <= doubleLimits.epsilon() ); 

    testVal = -273.15;
    diff = abs(Physical::ABS_ZERO - testVal);
    CPPUNIT_ASSERT( diff <= doubleLimits.epsilon() ); 

}

