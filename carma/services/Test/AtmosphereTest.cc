/** 
 * @file carma/environment/Test/AtmosphereTest.cc
 *
 * $Id: AtmosphereTest.cc,v 1.1 2010/02/18 22:14:09 abeard Exp $
 *
 * CppUnit test fixture for carma::environment::Atmosphere 
 * For these tests we cannot use == since
 * we are testing floating point numbers. Therefore we test
 * that the normalized difference between the expected
 * answer and the returned answer is less than
 * numeric_limits.epsilon(), which is ~10^-7 for floats.
 * In all cases, we spot check over a range of input values
 * and compare returns against hand-calculated values.
 * Functions are monotonic, so spot checking is good enough.
 * Even though these functions return doubles, we test with float 
 * values since the equations aren't really that precise.
 *
 * Author: Marc Pound
 * Version: $Revision: 1.1 $
 * $Date: 2010/02/18 22:14:09 $
 *
 */
#include <cmath>
#include <limits>
#include <iostream>
#include "AtmosphereTest.h"
#include "carma/services/Angle.h"
#include "carma/services/Physical.h"
#include "carma/services/Units.h"
#include "carma/services/Pressure.h"
#include "carma/services/Temperature.h"


using namespace carma::services;
using namespace CppUnit;
using namespace std;

void AtmosphereTest::setUp() 
{   
    this->tAtm = new carma::services::Atmosphere();
    // Our tests are based on epsilon(), the smallest
    // float that is not zero.  
    numeric_limits<float> floatLimits;
    epsilon = floatLimits.epsilon();
}

void AtmosphereTest::tearDown() 
{   
        delete tAtm;
}

// Spot check the safe routines.
// Want to make sure they return good
// sane values regardless of input.
void AtmosphereTest::testSafeRoutines() 
{

    double testVal, retVal;

    //------- TEST PRESSURE ----------------------
    // test a normal pressure, millibars;
    // returned value should be testVal
    testVal = 1000.0;
    retVal = tAtm->safeAtmPressure(testVal);
    normalizedDiff = fabsl(( retVal - testVal) /retVal);
    CPPUNIT_ASSERT( normalizedDiff < epsilon ); 

    // test a bad (low) pressure, millibars;
    // returned pressure should be DEFAULT
    testVal = 8.376;
    retVal = tAtm->safeAtmPressure(testVal);
    normalizedDiff = fabsl((retVal - Atmosphere::DEFAULT_ATM_PRESSURE)/retVal);
    CPPUNIT_ASSERT( normalizedDiff < epsilon ); 

    // test a bad (high) pressure, millibars;
    // returned pressure should be DEFAULT
    testVal = 456788.124312;
    retVal = tAtm->safeAtmPressure(testVal);
    normalizedDiff = fabsl((retVal - Atmosphere::DEFAULT_ATM_PRESSURE)/retVal);
    CPPUNIT_ASSERT( normalizedDiff < epsilon ); 

    //------- TEST AIR TEMPERATURE ----------------------
    // test a normal air temperature, Kelvin;
    // returned value should be testVal
    testVal = 280.1;
    retVal = tAtm->safeAirTemperature(testVal);
    normalizedDiff = fabsl((retVal - testVal)/retVal);
    CPPUNIT_ASSERT( normalizedDiff < epsilon ); 

    // test a bad (low) air temperature, Kelvin;
    // returned value should be DEFAULT
    testVal = -34.12;
    retVal = tAtm->safeAirTemperature(testVal);
    normalizedDiff = fabsl((retVal - Atmosphere::DEFAULT_AIR_TEMP)/retVal);
    CPPUNIT_ASSERT( normalizedDiff < epsilon ); 

    // test a bad (high) air temperature, Kelvin;
    // returned value should be DEFAULT
    testVal = 742.1;
    retVal = tAtm->safeAirTemperature(testVal);
    normalizedDiff = fabsl((retVal - Atmosphere::DEFAULT_AIR_TEMP)/retVal);
    CPPUNIT_ASSERT( normalizedDiff < epsilon ); 
    
    //------- TEST DEWPOINT TEMPERATURE ----------------------
    // test a normal dewpoint temperature, Kelvin;
    // returned value should be testVal
    testVal = 297.0;
    retVal = tAtm->safeDewPoint(testVal);
    normalizedDiff = fabsl((retVal - testVal)/retVal);
    CPPUNIT_ASSERT( normalizedDiff < epsilon ); 

    // test a bad (low) dewpoint temperature, Kelvin;
    // returned value should be DEFAULT
    testVal = 3.14159;
    retVal = tAtm->safeDewPoint(testVal);
    normalizedDiff = fabsl((retVal - Atmosphere::DEFAULT_DEW_POINT)/retVal);
    CPPUNIT_ASSERT( normalizedDiff < epsilon ); 

    // test a bad (high) dewpoint temperature, Kelvin;
    // returned value should be DEFAULT
    testVal = 2718.28;
    retVal = tAtm->safeDewPoint(testVal);
    normalizedDiff = fabsl((retVal - Atmosphere::DEFAULT_DEW_POINT)/retVal);
    CPPUNIT_ASSERT( normalizedDiff < epsilon ); 

    //------- TEST RELATIVE HUMIDITY ----------------------
    // test a normal RH, %;
    // returned value should be testVal
    testVal = 50.0;
    retVal = tAtm->safeRelativeHumidity(testVal);
    normalizedDiff = fabsl((retVal - testVal)/retVal);
    CPPUNIT_ASSERT( normalizedDiff < epsilon ); 

    // test a bad (low) RH, %;
    // returned value should be DEFAULT
    testVal = -21.0;
    retVal = tAtm->safeRelativeHumidity(testVal);
    normalizedDiff = fabsl((retVal - Atmosphere::DEFAULT_RH)/retVal);
    CPPUNIT_ASSERT( normalizedDiff < epsilon ); 

    // test a bad (high) RH, %;
    // returned value should be DEFAULT
    testVal = 110.4;
    retVal = tAtm->safeRelativeHumidity(testVal);
    normalizedDiff = fabsl((retVal - Atmosphere::DEFAULT_RH)/retVal);
    CPPUNIT_ASSERT( normalizedDiff < epsilon ); 

    //-------------- NOW TEST THE CONFORMABLE QUANTITIES VERSION -----
    //@todo must implement == operator for CQs
    /*
    Pressure goodpressure(700,"mbar");
    Pressure p = tAtm->safeAtmPressure(goodpressure);
    CPPUNIT_ASSERT( p == goodpressure );

    Pressure badpressure(1,"mbar");
    Pressure defpressure(Atmosphere::DEFAULT_ATM_PRESSURE,"mbar");
    p = tAtm->safeAtmPressure(badpressure);
    CPPUNIT_ASSERT( p == defpressure );

    Temperature goodtemperature(70,"F");
    Temperature t = tAtm->safeAirTemperature(goodtemperature);
    CPPUNIT_ASSERT( t == goodtemperature );

    Temperature badtemperature(700,"K");
    Temperature deftemperature(Atmosphere::DEFAULT_AIR_TEMP,"K");
    t = tAtm->safeAirTemperature(badtemperature);
    CPPUNIT_ASSERT( t == deftemperature );

    t = tAtm->safeDewPoint(goodtemperature);
    CPPUNIT_ASSERT( t == goodtemperature );
    t = tAtm->safeDewPoint(badtemperature);
    CPPUNIT_ASSERT( t == deftemperature );

    Angle goodangle(25,"degrees");
    Angle badangle(-25,"degrees");
    Angle defangle(Atmosphere::MIN_ELEVATION,"radians");

    Angle a = tAtm->safeElevation(goodangle);
    CPPUNIT_ASSERT( a == goodangle );
    a = tAtm->safeElevation(badangle);
    CPPUNIT_ASSERT( a == defangle );
    */
}

// test the saturated pressure function
void AtmosphereTest::testSatPressure() 
{

    // Choose test values that represent the range of
    // expected air temperatures
    double testVal[4] = {
	  // minimum allowable air temp
          Atmosphere::MIN_AIR_TEMP, 

	  // degenerate case: 273.15 should return the leading
          // coefficient Atmosphere::CC_A = 6.105.
         -carma::services::constants::Physical::ABS_ZERO, 

	  // standard value "room temperature"
          Atmosphere::DEFAULT_AIR_TEMP, 

	  // maximum allowable air temp
          Atmosphere::MAX_AIR_TEMP 
    };

    // These are the expected saturated pressures for the
    // corresponding test values. I calculated these by hand.
    double expectedVal[4] = {
           0.45192791,
           6.10500000,
	  35.46070496,
	  88.34388879
    };
    double retVal;


    for(int i = 0; i < 4; i++ ) {
	retVal = tAtm->computeSaturatedPressure(testVal[i]);
        normalizedDiff = fabsl((retVal - expectedVal[i])/retVal); 
	/*cout.precision(12);
	  cout << "SATPRES["<<i<<"] "
	     << " retVal: " << retVal
	     << " expectedVal: " << expectedVal[i]
	     << " n_diff: " << normalizedDiff
	     << " epsilon: " << epsilon
	     << endl;
	     */
        CPPUNIT_ASSERT( normalizedDiff < epsilon ); 
    }

}

// test the frequency dependence function

// test the zenith refractivity function 
void AtmosphereTest::testZenithRefractivity()
{

    // full range of air temperatures as above.
    double testTemp[4] = {
	  // minimum allowable air temp
          Atmosphere::MIN_AIR_TEMP, 

	  // degenerate case: 273.15 should return the leading
          // coefficient Atmosphere::CC_A = 6.105.
          -carma::services::constants::Physical::ABS_ZERO, 

	  // standard value "room temperature"
          Atmosphere::DEFAULT_AIR_TEMP, 

	  // maximum allowable air temp
          Atmosphere::MAX_AIR_TEMP 
    };

    double testPres[4] = { 
	  // minimum allowable atmospheric pressure 
          //Atmosphere::MIN_ATM_PRESSURE,
	  870.0,
	  // an ok value.
          950.0,
	  // standard value 1 atmosphere
          Atmosphere::STANDARD_ATM_PRESSURE,
	  // maximum allowable atmospheric pressure
          Atmosphere::MAX_ATM_PRESSURE,
    };

    double testRH[4]   = { 
          //Atmosphere::MIN_RH, // min allowable
	  10.0,
          30.0,               // ok
          Atmosphere::DEFAULT_RH, //default
          Atmosphere::MAX_RH, // max allowable
    };

    // frequencies at which to test
    double testFreq[4] = {
	1.0E9,
	50.0E9,
	115.271E9,
	230.0E9,
    };

    // hand calculated expected values for quadruplets above. 
    // See Marc's comb macro "zrfc"
    double expectedVal[4] = { 
	279.264203416,  // no frequency correction, 1Ghz
	279.071591561,  // no frequency correction 
	//279.844619870, // 50GHz, with frequency correction 
	298.9100919,     // 115.271 GHZ, 25% RH, no frequency correction
	//337.038335485, // 115.271 GHZ, 25% RH, with frequency correction
	602.817643052  // 230 GHZ, no frequency correction
	//623.975336688  //230 GHZ,  with frequency correction
    };
    double retVal;

    for(int i = 0; i < 4; i++ ) {
	retVal = tAtm->computeZenithRefractivity(testTemp[i],
		                                 testPres[i],
						 testRH[i],
						 testFreq[i]);
        normalizedDiff = fabsl((retVal - expectedVal[i])/retVal); 
	/*
	cout.precision(12);
	cout << " T[" << i << "]: " << testTemp[i]
	<< " P[" << i << "]: " << testPres[i]
	<< " RH[" << i << "]: " << testRH[i]
	<< " F[" << i << "]: " << testFreq[i]
	<< endl;
	cout << "ZR[" << i << "]: retVal is " 
	     << retVal 
	     << " expectedVal is " << expectedVal[i]
	     << " normalizedDiff is " << normalizedDiff << endl;
	     */
        CPPUNIT_ASSERT( normalizedDiff < epsilon ); 
    }
}

// Check that pathlength is monotonic, and behaves nicely
// at low and high elevations.
// This test is difficult to check with CPPUNIT since
// there are so many test return values.  What I've done
// is plotted the values returned by this test and
// compared the plot to TMS figure 13.35.  My plot looks
// like theirs.
void AtmosphereTest::testPathlength()
{

    // full range of air temperatures as above.
    double testTemp[4] = {
	  // minimum allowable air temp
          Atmosphere::MIN_AIR_TEMP, 

	  // degenerate case: 273.15 should return the leading
          // coefficient Atmosphere::CC_A = 6.105.
         -carma::services::constants::Physical::ABS_ZERO, 

	  // standard value "room temperature"
          Atmosphere::DEFAULT_AIR_TEMP, 

	  // maximum allowable air temp
          Atmosphere::MAX_AIR_TEMP 
    };

    double testPres[4] = { 
	  // minimum allowable atmospheric pressure 
          //Atmosphere::MIN_ATM_PRESSURE,
	  870.0,
	  // an ok value.
          950.0,
	  // standard value 1 atmosphere
          Atmosphere::STANDARD_ATM_PRESSURE,
	  // maximum allowable atmospheric pressure
          Atmosphere::MAX_ATM_PRESSURE,
    };

    double testRH[4]   = { 
          //Atmosphere::MIN_RH, // min allowable
	  10.0,
          30.0,               // ok
          Atmosphere::DEFAULT_RH, //default
          Atmosphere::MAX_RH, // max allowable
    };

    // frequencies at which to test
    double testFreq[4] = {
	1.0E9,
	50.0E9, // corner case
	115.271E9,
	230.0E9,
    };

    double elevation, elevRad, retVal;

    for (int i=0;i<4;i++) {
//	cout << "#  ELEVATION(d)   PATHLENGTH(m) " << endl;
	for( elevation=0.0; elevation <= 120.0; elevation +=1.0) {
	    elevRad = elevation * M_PI / 180.0 ;
	    retVal = tAtm->computePathlength(testTemp[i], testPres[i],
					     testRH[i] , elevRad,
					     testFreq[i]);
//	    cout << elevation << "   " << retVal << endl;
	}
    }

    CPPUNIT_ASSERT( 1 == 1 );

}

void AtmosphereTest::testRefractionCorrection() {
    carma::services::Units units;
    //SlaLib sla;

    /*
    // full range of air temperatures as above.
    double testTemp[4] = {
	  // minimum allowable air temp
          Atmosphere::MIN_AIR_TEMP, 

	  // degenerate case: 273.15 should return the leading
          // coefficient Atmosphere::CC_A = 6.105.
         -carma::services::constants::Physical::ABS_ZERO, 

	  // standard value "room temperature"
          Atmosphere::DEFAULT_AIR_TEMP, 

	  // maximum allowable air temp
          Atmosphere::MAX_AIR_TEMP 
    };

    double testPres[4] = { 
	  // minimum allowable atmospheric pressure 
          //Atmosphere::MIN_ATM_PRESSURE,
	  870.0,
	  // an ok value.
          950.0,
	  // standard value 1 atmosphere
          Atmosphere::STANDARD_ATM_PRESSURE,
	  // maximum allowable atmospheric pressure
          Atmosphere::MAX_ATM_PRESSURE,
    };

    double testRH[4]   = { 
          //Atmosphere::MIN_RH, // min allowable
	  10.0,
          30.0,               // ok
          Atmosphere::DEFAULT_RH, //default
          Atmosphere::MAX_RH, // max allowable
    };

    // frequencies at which to test
    double testFreq[4] = {
	1.0E9,
	50.0E9, // corner case
	115.271E9,
	230.0E9,
    };
    */

    double AlmaPres = 550.0;
    double AlmaTemp = -carma::services::constants::Physical::ABS_ZERO;
    double AlmaFreq = 230.0E9;
    double AlmaOptical = 0.6E-6; // 0.6 microns
    double AlmaRH   = 0.0;
    double AlmaAltitude = 5000.0; // meters
    //double AlmaLongitude = units.convert(68.0,"degrees","radians");
    //double AlmaLatitude  = units.convert(23.0,"degrees","radians");

    double /*elevation,*/ elevRad, retVal, /*slaVal,*/ wavelength, zRad;
    //double terminate = units.convert(0.001,"arcsec","radians");
    //double lapseRate = 0.0065; // K/meter
    //double altitude = 0.0; // height above sea level in meters
   // double latitude = units.convert(40.18,"degrees","radians");

    // zobs is the observed zenith distance, i.e. already corrected
    // for refraction.  slaRefro returns the the normalized difference between
    // the uncorrected and corrected zenith distance: dZ = ZD_in_vacuo - zobs.
    // This quantity is positive.
    // computeRefractionCorrection() works the other way around:
    // it takes and UNcorrected elevation and computes the correction
    // elevOBS - elev_in_vacuo. This quantity is also positive. (since
    // ZD = 90 - elev)
    // Therefore we want to call slaRefro first and then feed
    // the in vacuo elevation to computeRefractionCorrection.
    // elev_in_vacuo = 90 - ZD_in_vacuo 
    //               = 90 - (dZ + zobs)
    //               = 90 - (slaVal + zobs)
    //               = 90 - slaVal - zobs
    cout.precision(8);

    // test against ALMA numbers
    double ulichVal, opticalVal;
    wavelength = carma::services::constants::Physical::C/AlmaOptical; //Hz
    cout << "\n#  In Vacuo Elevation(d)   "
	     << "Refraction correction(arcsec)  Ulich(arcsec) ALMA Optical (arcsec)" 
	     << endl;
    cout << "# wavelength = 0.6 micron = " << wavelength << " Hz " << endl;
    for(double zobs=89.0; zobs >= 0.0; zobs -=1.0) {
        zRad = units.convert(zobs, "degrees", "radians");
	elevRad = (M_PI/2.0) - zRad;
	ulichVal = tAtm->ulichRefractionCorrection(
		                AlmaTemp,
				AlmaPres,
				AlmaRH,
				elevRad,
				AlmaFreq);

	retVal = tAtm->computeRefractionCorrection(
		                AlmaTemp,
				AlmaPres,
				AlmaRH,
				elevRad,
				AlmaFreq,
				AlmaAltitude);

	opticalVal = tAtm->computeRefractionCorrection(
		                AlmaTemp,
				AlmaPres,
				AlmaRH,
				elevRad,
				wavelength,
				AlmaAltitude);

	cout << 90.0 - zobs << "  "
	     << units.convert(retVal,"radians","arcsec") << "   "
	     << units.convert(ulichVal,"radians","arcsec") << "   "
	     << units.convert(opticalVal,"radians","arcsec") << "   "
	     << endl;
    }

    /*
    // test against some other standard numbers
    for (int i=0;i<4;i++) {
	cout << "\n#  In Vacuo Elevation(d)   "
	     << "Refraction correction(arcsec)  SLALIB(arcsec)" 
	     << endl;
        wavelength = units.convert(
	     carma::services::constants::Physical::C/almaFreq,"m","microns"
	     );
	for(double zobs=90.0; zobs >= 0.0; zobs -=1.0) {

	    zRad = units.convert(zobs, "degrees", "radians");
	    sla.slaRefro(zRad, altitude,
		    testTemp[i], testPres[i], testRH[i]/100.0,
		    wavelength, latitude, lapseRate, terminate,&slaVal);

	    // in radians.
	    elevRad = (M_PI/2.0) - zRad - slaVal;
	    // in degrees
	    elevation = units.convert(elevRad,"radians","degrees");

	    retVal = tAtm->computeRefractionCorrection(
		                             testTemp[i], 
					     testPres[i],
					     testRH[i], 
					     elevRad,
					     testFreq[i],
					     altitude);

	    slaVal = units.convert(slaVal,"radians","arcsec");
	    cout << elevation << "   " 
		 << units.convert(retVal,"radians","arcsec") << "   "
		 << slaVal 
		 << endl;
	}
    }
    */

    CPPUNIT_ASSERT( 1 == 1 );

}

void AtmosphereTest::testWaterRoutines()
{
    // waterColumn() exercizes waterVaporDensity() and waterPartialPressure()
    // full range of air temperatures as above.
    double testTemp[4] = {
	  // minimum allowable air temp
          Atmosphere::MIN_AIR_TEMP, 

          // 0 Celsius.
         -carma::services::constants::Physical::ABS_ZERO, 

	  // standard value "room temperature"
          Atmosphere::DEFAULT_AIR_TEMP, 

	  // maximum allowable air temp
          Atmosphere::MAX_AIR_TEMP 
    };

    double testRH[4]   = { 
          Atmosphere::MIN_RH, // min allowable
          30.0,               // ok
          Atmosphere::DEFAULT_RH, //default
          Atmosphere::MAX_RH, // max allowable
    };

    // expected answers, calculated by hand.
    // See Marc's comb macro "mmh2"
    double expectedVal[4] = {
	0.036418713951,
	2.6152096969,
	11.525689585,
       108.86884058
    };

    for( int i = 0; i < 4; i++ ) 
    {
	double retVal = tAtm->waterColumn(testTemp[i],testRH[i]);
        normalizedDiff = fabsl ( (retVal - expectedVal[i])/ retVal ); 
        CPPUNIT_ASSERT( normalizedDiff < epsilon ); 
    }

    Atmosphere::dewPointMethodType dpType[3] = { 
	Atmosphere::CLAUSIUS_CLAPEYRON,
	Atmosphere::MAGNUS_TETENS,
	Atmosphere::HOFFMAN_WELCH,
    };

    for( unsigned short i = 0; i < 4; i++ ) 
    {
	// check that dewpoint/relative humidity methods invert properly, 
	// by computing the dewpoint then using that to recover the
	// input RH.
	// Test all the dewpoint computation methods.
	for ( unsigned short j = 0 ; j< 3; j++ ) {
	    double dewPoint = tAtm->computeDewPoint(
				testTemp[i], testRH[i], dpType[j]
				);
	    double rh = tAtm->computeHumidity(testTemp[i],dewPoint,dpType[j]);
	    /*
	    cerr << endl
		 << " step " << i << " method type " << dpType[j] << endl
		 << " Input RH: " << testRH[i]  << endl
		 << " Output RH: " << rh<< endl
		 << " % diff " << normalizedDiff*100
		 << endl;
	   */
	    if ( j != 0 ) {
		normalizedDiff = fabsl ( (rh - testRH[i])/ rh); 
		CPPUNIT_ASSERT( normalizedDiff < epsilon ); 

		//do the same check with with isConsistentDewPoint(...)
		CPPUNIT_ASSERT( 
			tAtm->isConsistentDewPoint( 
			    Temperature( dewPoint, "K"),
			    Temperature( testTemp[i], "K"),
			    rh, dpType[j], epsilon)
			);
	    } else {
		// Clausius-Clapeyron inversion not self-consistent
		// to better than 5% because of different sources
		// for coefficients.  See my todo comment in
		// Atmosphere::computeSaturatedPressure().
		CPPUNIT_ASSERT( 
			tAtm->isConsistentDewPoint( 
			    Temperature( dewPoint, "K"),
			    Temperature( testTemp[i], "K"),
			    rh, dpType[j], 5.0 )
			);
	    }
	}
    }
}
