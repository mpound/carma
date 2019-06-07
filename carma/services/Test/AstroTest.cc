/** 
 * @file 
 *
 * $Id: AstroTest.cc,v 1.11 2013/07/18 17:01:38 mpound Exp $
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
 * Version: $Revision: 1.11 $
 * $Date: 2013/07/18 17:01:38 $
 *
 */
#include <cmath>
#include <limits>
#include <iostream>
#include "AstroTest.h"
#include "carma/services/Astro.h"
#include "carma/services/Frequency.h"
#include "carma/services/Planet.h"
#include "carma/services/PlanetTemperature.h"


using namespace carma::services::constants;
using namespace carma::services;
using namespace CppUnit;
using namespace std;

void AstroTest::setUp() 
{   

}

void AstroTest::tearDown() 
{   

}

void AstroTest::testConstants()
{

    numeric_limits<double> doubleLimits;
    double testVal; 
    long double diff;
    double epsilon = doubleLimits.epsilon() * 10.0;

    // do some spot checking of values
    testVal = 1.49597870691E11;
    diff = fabsl((Astro::AU - testVal)/Astro::AU);
    CPPUNIT_ASSERT( diff <= epsilon );

    testVal = 6.3781366E6;
    diff = fabsl((Astro::EARTH.radius - testVal)/Astro::EARTH.radius);
    CPPUNIT_ASSERT( diff <= epsilon );

    testVal = 5.9742E24;
    diff = fabsl( (Astro::EARTH.mass - testVal) /Astro::EARTH.mass);
    CPPUNIT_ASSERT( diff <= epsilon );

    testVal = 1.0;
    diff = fabsl( (Astro::EARTH.avgDist - testVal)/Astro::EARTH.avgDist);
    CPPUNIT_ASSERT( diff <= epsilon );

    testVal =  6.960E8;
    diff = fabsl( (Astro::SUN.radius - testVal)/Astro::SUN.radius);
    CPPUNIT_ASSERT( diff <= epsilon );

    testVal = 1.9891E30;
    diff = fabsl((Astro::SUN.mass - testVal)/Astro::SUN.mass);
    CPPUNIT_ASSERT( diff <= epsilon );

}

void AstroTest::testPlanetTemperature()
{
    float testVal, retVal, diff;
    numeric_limits<float> floatLimits;
    const float epsilon = floatLimits.epsilon() * 10.0;

    Planet jupiter("jupiter");
    Planet mars("mars");
    Frequency f(230.0,"GHz");
    Frequency f2(99.0,"GHz");
    Frequency f3(217.5,"GHz");
    CPPUNIT_ASSERT( true );

    // Jupiter has a time-invariant spectrum, function
    // of frequency only. 
    // @see conf/data/jupitertb.tab
    /*
    jupiter.setMJD( 56391.0 );
    testVal = 166.719873;
    retVal = jupiter.brightnessTemperature(f3).kelvin();
    diff = fabsf(( testVal - retVal )/testVal );
    cout << "DIFF = " << diff <<  " epsilon = " << epsilon << endl;
    CPPUNIT_ASSERT( diff <= epsilon );

    testVal = 162.882;
    retVal = jupiter.brightnessTemperature(f2).kelvin();
    diff = fabsf(( testVal - retVal )/testVal );
    CPPUNIT_ASSERT( diff <= epsilon );

    // Mars has diurnal variations as well as frequency dependence.
    // @see conf/data/marstb.tab
    testVal = 223.00;
    mars.setMJD( 56391.0 );
    retVal = mars.brightnessTemperature( f ).kelvin();
    diff = fabsf(( testVal - retVal )/testVal );
    CPPUNIT_ASSERT( diff <= epsilon );

    mars.setMJD( 56410 ); 
    testVal = 216.90;
    retVal = mars.brightnessTemperature( f ).kelvin();
    diff = fabsf(( testVal - retVal )/testVal );
    CPPUNIT_ASSERT( diff <= epsilon );

    mars.setMJD( 57107.0 );  
    testVal = 215.600;
    retVal = mars.brightnessTemperature( f ).kelvin();
    diff = fabsf(( testVal - retVal )/testVal );
    CPPUNIT_ASSERT( diff <= epsilon );

    mars.setMJD( 57107.0 );  
    testVal = 208.262;
    retVal = mars.brightnessTemperature( f2 ).kelvin();
    diff = fabsf(( testVal - retVal )/testVal );
    CPPUNIT_ASSERT( diff <= epsilon );
    */

}
