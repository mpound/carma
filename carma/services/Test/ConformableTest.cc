/** 
 * @file 
 *
 * $Id: ConformableTest.cc,v 1.20 2007/10/05 15:00:28 mpound Exp $
 *
 * CppUnit test fixture for carma::services::ConformableQuantities
 * and its derived classes.
 *
 * Author: Marc Pound
 * Version: $Revision: 1.20 $
 * $Date: 2007/10/05 15:00:28 $
 *
 */
#include <cmath>
#include <limits>
#include <iostream>
#include <iomanip>
#include "ConformableTest.h"
#include "carma/services/ConformableQuantity.h"
#include "carma/services/ConformabilityException.h"
#include "carma/services/Angle.h"
#include "carma/services/AngularRate.h"
#include "carma/services/DecAngle.h"
#include "carma/services/Distance.h"
#include "carma/services/Frequency.h"
#include "carma/services/HourAngle.h"
#include "carma/services/Pressure.h"
#include "carma/services/Temperature.h"
#include "carma/services/Velocity.h"
#include "carma/services/Units.h"
#include "carma/util/IllegalArgumentException.h"


using namespace carma::services;
using namespace CppUnit;
using namespace std;

void ConformableTest::setUp() 
{   

    // Our tests are based on epsilon(), the smallest
    // double that is not zero.  We will test against
    // 10 times this because epsilon() ~ 10^-16 and the
    // last digit of a double representation is unreliable.
    numeric_limits<double> doubleLimits;
    epsilon = 10.0 * doubleLimits.epsilon();
    //cout << "ConformableTest:  epsilon = " << epsilon << endl;
}

void ConformableTest::tearDown() 
{   

}

void ConformableTest::testDistance()
{

    Distance d(20000.0,"leagues");
    testVal = 20000.0*2640.0;
    retVal = d.convert("fathoms");
    normalizedDiff = fabsl(( testVal - retVal )/retVal);
    /*
    cout.precision(17);
    cout << "testVal = " << testVal 
	 << " retVal = " << retVal
	 << " epsilon = " << epsilon 
	 << " normalizedDiff ="   << normalizedDiff 
	 << endl;
    */
    CPPUNIT_ASSERT( normalizedDiff < epsilon );

    Distance zero(0.0, "km");

    // check on CARMA 0 = Infinite convention
    CPPUNIT_ASSERT(zero.isInfinite());
    
    Distance q(0.35, "AU");
    Distance p(3.0E11, "m");
    Distance r = q + p; // result is in km
    testVal = 352359254.74185;
    retVal = r.getValue();
    normalizedDiff = fabsl(( retVal - testVal )/retVal);
    CPPUNIT_ASSERT( normalizedDiff < epsilon );

    try {
	Distance negative(-20,"miles");
    } catch ( const carma::util::IllegalArgumentException& e ) {
        CPPUNIT_ASSERT( 1 == 1 );
	cout << "ConformableTest caught exception:\n" << e.what()
	     << "(Don't worry, this is part of the test)"
	     << endl;
        // if we got to here then the test was succesful.
    } catch ( const std::exception& ex ) {
       // if something else was thrown, the test failed.
        cout << "ConformableTest caught exception:\n" << ex.what() << endl;
        CPPUNIT_ASSERT( 1 == 0 ); 
    } 

}

void ConformableTest::testAngles()
{

    // test the += operator
    Angle q(3.0,"degrees");
    Angle r(4.0,"degrees");
    Angle s = q+r;
    s += q;
    cout << s << " should be 10 degrees" << endl;

    Angle a(M_PI, "radians");
    Angle b(3600.0,"arcseconds"); 
    testVal = 181.0;  // if b = 7200, testVal = 182.0!
    Angle c = a + b; // operator overloaded here.

    retVal = c.convert("degrees");
    normalizedDiff = fabsl(( retVal - testVal )/retVal);
    /*
    std::cout.precision(17);
    std::cout << "ConformableTest DEBUG " << std::endl;
    std::cout << "               retVal: " << scientific << retVal << std::endl;
    std::cout << "               normalizedDiff: " << scientific << normalizedDiff << std::endl;
    */
    CPPUNIT_ASSERT( normalizedDiff < epsilon );

    // test modulo 2PI here
    c = a + a; 
    normalizedDiff = fabsl( c.radians() );
    CPPUNIT_ASSERT( normalizedDiff < epsilon );

    HourAngle haa(5.0,"hours");
    HourAngle hab(-10.0,"hr");
    HourAngle hac = haa + hab;

    testVal = hac.hours();
    retVal = -5.0;
    normalizedDiff = fabsl(( testVal - retVal )/retVal); // -1 + 1 = 0
    CPPUNIT_ASSERT( normalizedDiff < epsilon );

    // test modulo 12 hours here
    HourAngle had(9.0,"hours");
    hac = had + had;
    testVal = hac.hours();
    retVal = -6.0;
    normalizedDiff = fabsl(( testVal - retVal )/retVal); // 18hrs - 24 hrs = -6 hrs
    CPPUNIT_ASSERT( normalizedDiff < epsilon );

    // first try instantiating a Declination that is larger than 90 degrees
    
    try {
        DecAngle bigdec(191923490.0,"degrees");
    } catch (const ConformabilityException& cex) {
        CPPUNIT_ASSERT( 1 == 1 );
	cout << "ConformableTest caught exception:\n" << cex.what()
	     << "(Don't worry, this is part of the test)"
	     << endl;
        // if we got to here then the test was succesful.
    } catch ( const std::exception& ex ) {
       // if something else was thrown, the test failed.
        cout << "ConformableTest caught exception:\n" << ex.what() << endl;
        CPPUNIT_ASSERT( 1 == 0 ); 
    } 

    // try a Declination that is larger than 90 degrees through addition
    try {
       DecAngle dec(50.0,"degrees");
       DecAngle aDecAngle = dec + dec;
    } catch (const ConformabilityException& cex) {
        CPPUNIT_ASSERT( 1 == 1 );
	cout << "ConformableTest caught exception:\n" << cex.what()
	     << "(Don't worry, this is part of the test)"
	     << endl;
        // if we got to here then the test was succesful.
    } catch ( const std::exception& ex ) {
       // if something else was thrown, the test failed.
        cout << "ConformableTest caught exception:\n" << ex.what() << endl;
        CPPUNIT_ASSERT( 1 == 0 ); 
    } 

    // now try making a Declination that is less than -90 degrees
    try {
       DecAngle ndec(-47.0,"degrees");
       DecAngle anotherDecAngle = ndec + ndec;
    } catch (const ConformabilityException& cex) {
        CPPUNIT_ASSERT( 1 == 1 );
	cout << "ConformableTest caught exception:\n" << cex.what()
	     << "(Don't worry, this is part of the test)"
	     << endl;
        // if we got to here then the test was succesful.
    } catch ( const std::exception& ex ) {
       // if something else was thrown, the test failed.
        cout << "ConformableTest caught exception:\n" << ex.what() << endl;
        CPPUNIT_ASSERT( 1 == 0 ); 
    } 

    // test unit conversion, addition
    DecAngle dec2(M_PI_4, "radians");     // 45 degrees
    DecAngle dec3(-7200.0,"arcseconds");  // -2 degrees
    testVal = 43.0;  // degrees
    DecAngle dec4 = dec2 + dec3; // operator overloaded here.
    retVal = dec4.degrees();
    normalizedDiff = fabsl(( testVal - retVal )/retVal); 
    CPPUNIT_ASSERT( normalizedDiff < epsilon );

    // test negative angles and modulo 2Pi or not
    Angle negative2(-M_PI_4,"radians");
    testVal = -45;
    retVal  = negative2.degrees(false);
    normalizedDiff = fabsl(( testVal - retVal )/retVal); 
    CPPUNIT_ASSERT( normalizedDiff < epsilon );

    testVal = 315;
    retVal = negative2.degrees(true);
    normalizedDiff = fabsl(( testVal - retVal )/retVal); 
    CPPUNIT_ASSERT( normalizedDiff < epsilon );

    Angle negative(-4.0,"radians");
    CPPUNIT_ASSERT( negative.radians(false) < 0 );
    CPPUNIT_ASSERT( negative.degrees() < 0 ); // default is false
    CPPUNIT_ASSERT( negative.degrees(true) > 0 );


}
 
void ConformableTest::testFrequency()
{

    Frequency f1(115.271204,"GHz");

    testVal = 2.600757583828135;
    retVal = f1.wavelength("mm");
    normalizedDiff = fabsl(( testVal - retVal )/retVal); 
    CPPUNIT_ASSERT( normalizedDiff < epsilon );
    
    Frequency f2(5477.0,"kHz");
    retVal = f2.megahertz();
    testVal = 5.477;
    normalizedDiff = fabsl(( testVal - retVal )/retVal);
    CPPUNIT_ASSERT( normalizedDiff < epsilon  );
    
    try {
	Frequency negative(-20,"THz");
    } catch ( const carma::util::IllegalArgumentException& e ) {
        CPPUNIT_ASSERT( 1 == 1 );
	cout << "ConformableTest caught exception:\n" << e.what()
	     << "(Don't worry, this is part of the test)"
	     << endl;
        // if we got to here then the test was succesful.
    } catch ( const std::exception& ex ) {
       // if something else was thrown, the test failed.
        cout << "ConformableTest caught exception:\n" << ex.what() << endl;
        CPPUNIT_ASSERT( 1 == 0 ); 
    } 
}

void ConformableTest::testTemperature()
{

    Temperature t1(-40.0,"F");
    testVal = -40.0;  // same on both scales!
    retVal = t1.celsius();
    normalizedDiff = fabsl(( testVal - retVal )/retVal); 
    CPPUNIT_ASSERT( normalizedDiff < epsilon );

    Temperature t2(0.0,"C");
    testVal = 32.0;  // freezing point of water
    retVal = t2.fahrenheit();
    normalizedDiff = fabsl(( testVal - retVal )/retVal); 
    CPPUNIT_ASSERT( normalizedDiff < epsilon );

    Temperature t3(50.0,"C");
    testVal = 323.15;
    retVal = t3.kelvin();
    normalizedDiff = fabsl(( testVal - retVal )/retVal); 
    CPPUNIT_ASSERT( normalizedDiff < epsilon );

    // make sure conversion to own unit works!
    testVal = 99.9;
    Temperature t4(testVal,"K");
    retVal = t4.kelvin();
    normalizedDiff = fabsl(( testVal - retVal )/retVal); 
    CPPUNIT_ASSERT( normalizedDiff < epsilon );

    testVal = 77.7;
    Temperature t5(testVal,"fahrenheit");
    retVal = t5.fahrenheit();
    normalizedDiff = fabsl(( testVal - retVal )/retVal); 
    CPPUNIT_ASSERT( normalizedDiff < epsilon );

    testVal = 88.8;
    Temperature t6(testVal,"centigrade");
    retVal = t6.celsius();
    normalizedDiff = fabsl(( testVal - retVal )/retVal); 
    CPPUNIT_ASSERT( normalizedDiff < epsilon );

    try {
	Temperature negative(-300,"C");
    } catch ( const carma::util::IllegalArgumentException& e ) {
        CPPUNIT_ASSERT( 1 == 1 );
	cout << "ConformableTest caught exception:\n" << e.what()
	     << "(Don't worry, this is part of the test)"
	     << endl;
        // if we got to here then the test was succesful.
    } catch ( const std::exception& ex ) {
       // if something else was thrown, the test failed.
        cout << "ConformableTest caught exception:\n" << ex.what() << endl;
        CPPUNIT_ASSERT( 1 == 0 ); 
    } 

    // test the reset function.
    try {
      t3.reset(100.0,"Celsius");
    } catch ( const carma::util::IllegalArgumentException& e ) {
        cout << 
	    "ConformableTest caught exception in trying to reset to Celsisu:\n" 
	    << e.what() 
	    << endl;
        CPPUNIT_ASSERT( 1 == 0 ); 
    } catch ( const std::exception& ex ) {
       // if something else was thrown, the test failed.
        cout << "ConformableTest caught exception:\n" << ex.what() << endl;
        CPPUNIT_ASSERT( 1 == 0 ); 
    } 

}


void ConformableTest::testVelocity()
{
    velocityFrameType vftype = Velocity::translateFrame("RADI-LSR");
    CPPUNIT_ASSERT( vftype == FRAME_LSR );
    velocityDefType vdtype = Velocity::translateDefinition("OPT-HELIO");
    CPPUNIT_ASSERT( vdtype == VEL_OPTICAL );
    cout << "PASSED VelFrame/VelDef Tests." << endl;
    Velocity kms(20,"km/s");
    retVal = kms.convert("USmiles/hour");
    testVal = 44738.63636363636363636363;
    normalizedDiff = fabsl(( testVal - retVal )/retVal);
    /*
    std::cout.precision(17);
    std::cout << "ConformableTest DEBUG " << std::endl;
    std::cout << "                retVal: " << scientific << retVal << std::endl;
    std::cout << "                normalizedDiff: " << scientific << normalizedDiff << std::endl;
    */
    CPPUNIT_ASSERT( normalizedDiff < epsilon );
}


