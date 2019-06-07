/** 
 * @file 
 *
 * $Id: GlobalTest.cc,v 1.1 2004/09/09 19:11:55 mpound Exp $
 *
 * CppUnit test fixture for carma::services::Astro
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
#include "GlobalTest.h"


using namespace carma::services;
using namespace CppUnit;
using namespace std;

void GlobalTest::setUp() 
{   

}

void GlobalTest::tearDown() 
{   

}

void GlobalTest::testConstants()
{

    unsigned short testVal;

    testVal = 9;
    CPPUNIT_ASSERT( Global::nBimaAntennas() == testVal );

    testVal = 6;
    CPPUNIT_ASSERT( Global::nOvroAntennas() == testVal );

    testVal = 8;
    CPPUNIT_ASSERT( Global::nSzaAntennas() == testVal );

    testVal = 23;
    CPPUNIT_ASSERT( Global::maxAntennas() == testVal );

    testVal = 16;
    CPPUNIT_ASSERT( Global::nWidebandBands() == testVal );

    testVal = 8;
    CPPUNIT_ASSERT( Global::nSpectralLineBands() == testVal );

}

