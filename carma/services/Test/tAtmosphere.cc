#include "carma/util/Program.h"

#include "carma/services/Atmosphere.h"
#include "carma/services/Location.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/services/Physical.h"

#include <cassert>

//
// @version	$Revision: 1.1 $ $Date: 2010/02/18 22:14:09 $
//
// @usage  excersize program for Atmosphere
//
// @description
//	Test program for Atmospheric calculations. 
//      Compute methods we have
//      0   Clausius-Clapeyron method
//      1   Magnus-Tetens
//      2   Hoffman & Welch ( adapted BIMA code )
//      It returns temperatures, humidity and water content in the atmosphere
//      For given elevation, frequency and pressure , it also computes the 
//      refraction correction, in arcsecs.
//
// @logger ENVIRONMENT_FACILITY carma.environment.Test.tAtmosphere
//
//
// @key   Ta             25 d  Ambient temperature, Celsius
// @key   RH           30.5 d  Relative Humidity, percent (0..100)
// @key   Td     @noDefault d  Dewpoint temperature, Celsius
// @key   observatory  ovro s  Name of the observatory (for altitude)
// @key   method          0 i  Dewpoint/RH Compute Method
// @key   test            f b  Show various min/max values in Atmosphere
// @key   el             45 d  Elevation (in def) for refraction correction
// @key   freq          100 d  Frequency (in GHz) for refraction correction
// @key   pressure      780 d  Pressure (mb) for refraction correction
//


using namespace std;
using namespace carma::services;
using namespace carma::util;
using namespace carma::services::constants;

int carma::util::Program::main()
{
  const string methodName[3] = {
      "Clausius-Clapeyron",
      "Magnus-Tetens",
      "Hoffman-Welch (adapted BIMA code)"
  };
  double pressure = getDoubleParameter("pressure");
  double freq     = getDoubleParameter("freq");
  double el       = getDoubleParameter("el");
  bool Qtest = getBoolParameter("test");
  Atmosphere::dewPointMethodType method 
      = static_cast<Atmosphere::dewPointMethodType>(getIntParameter("method"));

  if (method < 0 || method > 2 ) {
      cerr << "No such method: " << method << endl;
      cerr << "Available methods are:" << endl;
      for(int i = 0; i < 3; i++ ) 
	  cerr << i << " " << methodName[i] << endl;
      return(EXIT_FAILURE);
  }

  double T0 = -Physical::ABS_ZERO;        // T0 is now +273 !!
  double Ta, Td, RH;
  std::string obs = getStringParameter("observatory");
  Location loc(obs);
  Atmosphere atm;
  double alt = loc.getAltitude().meters();

  if (Qtest) {
    cout << "ABS_ZERO                 : " << T0 << endl;
    cout << "ATM_PRESSURE dfl/min/max : "  
	 << atm.DEFAULT_ATM_PRESSURE << " "
	 << atm.MIN_ATM_PRESSURE << " " 
	 << atm.MAX_ATM_PRESSURE << endl;
    cout << "AIR_TEMP dfl/min/max     : "
	 << atm.DEFAULT_AIR_TEMP << " "
	 << atm.MIN_AIR_TEMP << " "
	 << atm.MAX_AIR_TEMP << endl;
    cout << "RH dfl/min/max           : "
	 << atm.DEFAULT_RH << " "
	 << atm.MIN_RH << " "
	 << atm.MAX_RH << endl;
    cout << "DEW_POINT dfl/min/max    : "
	 << atm.DEFAULT_DEW_POINT << " "
	 << atm.MIN_DEW_POINT << " "
	 << atm.MAX_DEW_POINT << endl;
    return EXIT_SUCCESS;
  }
  
  try {
    Ta = getDoubleParameter("Ta") + T0;   // Ta now in Kelvin
    if (parameterWasSpecified("Td")) {
      Td = getDoubleParameter("Td") + T0;
      RH = atm.computeHumidity(Ta,Td,method);
      cout << "tAtmosphere" << endl;
      cout << "method = " << methodName[method] << endl;
      //cout << "method = " << method << endl;
      cout << "Ta = " << Ta-T0 << " C" << endl;
      cout << "Td = " << Td-T0 << " C" << endl;
      cout << "RH = " << RH << " % [computed]" << endl;
    } else {
      RH = getDoubleParameter("RH");
      Td = atm.computeDewPoint(Ta, RH, method);
      cout << "tAtmosphere" << endl;
      cout << "method = " << methodName[method] << endl;
      cout << "Ta = " << Ta-T0 << " C" << endl;
      cout << "Td = " << Td-T0 << " C [computed]" << endl;
      cout << "RH = " << RH << " %" << endl;
    }
    cout << "Saturated vapor pressure = " 
	 << atm.computeSaturatedPressure(Ta) << " mbar" << endl;
    cout << "Water Vapor pressure = " 
	 << atm.waterPartialPressure(Ta,RH) << " mbar" << endl;
    cout << "Water Vapor density  = " 
	 << atm.waterVaporDensity(Ta, RH) << " g/m^3" << endl;
    cout << "Water column         = " 
	 << atm.waterColumn(Ta, RH) << " mm" << endl;

    const double rad2deg = 180.0/M_PI;
    double r = atm.computeRefractionCorrection(
					       Ta,           // K
					       pressure,     // Mbar
					       RH,           // %
					       el / rad2deg, // radians
					       freq * 1e9,   // Hz
					       alt           // m
					       );
    r = r * rad2deg *  3600.0;   // convert radians to arcsec
    cout << "Frequency      = " << freq << " GHz" << endl;
    cout << "Pressure       = " << pressure << " mbar" << endl;
    cout << "Elevation      = " << el << " deg" << endl;
    cout << "Refraction     = " << r << "  arcsec; = " << r/60.0 << " arcmin." << endl;

  } catch (NotFoundException& nfe) {
    cerr << "Program::main : " << nfe.getMessage() << endl;
    return EXIT_FAILURE;
  } catch (...) {
    cerr << "Program::main : an uncaught error" << endl;
    return EXIT_FAILURE;
  }
  
  // Test lastSafeXXX methods...
  try {
    Atmosphere atm;
    
    // Verify that defaults are returned if these haven't been called yet.
    assert( atm.lastSafeAirTemperature( ).kelvin( ) == atm.DEFAULT_AIR_TEMP );
    assert( atm.lastSafeAtmPressure( ).millibar( ) == atm.DEFAULT_ATM_PRESSURE );
    assert( atm.lastSafeRelativeHumidity( ) == atm.DEFAULT_RH );
    assert( atm.lastSafeDewPoint( ).kelvin( ) == atm.DEFAULT_DEW_POINT );
     
    // Verify that last safe values are saved and returned after init.
    const double aGoodAirTemp = atm.DEFAULT_AIR_TEMP + 1.0;
    atm.lastSafeAirTemperature( aGoodAirTemp );
    const double aBadAirTemp = atm.MIN_AIR_TEMP - 1.0;
    assert( atm.lastSafeAirTemperature( aBadAirTemp ).kelvin( )
            == aGoodAirTemp );
    
    const double aGoodAtmPressure = atm.DEFAULT_ATM_PRESSURE + 1.0;
    atm.lastSafeAtmPressure( aGoodAtmPressure );
    const double aBadAtmPressure = atm.MIN_ATM_PRESSURE - 1.0;
    assert( atm.lastSafeAtmPressure( aBadAtmPressure ).millibar( )
            == aGoodAtmPressure );
    
    const double aGoodRelHumidity = atm.DEFAULT_RH + 1.0;
    atm.lastSafeRelativeHumidity( aGoodRelHumidity );
    const double aBadRelHumidity = atm.MIN_RH - 1.0;
    assert( atm.lastSafeRelativeHumidity( aBadRelHumidity ) 
            == aGoodRelHumidity );
    
    const double aGoodDewPoint = atm.DEFAULT_DEW_POINT + 1.0;
    atm.lastSafeDewPoint( aGoodDewPoint );
    const double aBadDewPoint = atm.MIN_DEW_POINT - 1.0;
    assert( atm.lastSafeDewPoint( aBadDewPoint ).kelvin( )
            == aGoodDewPoint );
    
  } catch ( ... ) {
    return EXIT_FAILURE;
  }
  
  // Test the lastSafeXXX overloads too
  try {
    Atmosphere atm;
    
    // Verify that last safe values are saved and returned after init.
    const Temperature aGoodAirTemp( atm.DEFAULT_AIR_TEMP + 1.0, "K" );
    atm.lastSafeAirTemperature( aGoodAirTemp );
    const Temperature aBadAirTemp( atm.MIN_AIR_TEMP - 1.0, "K" );
    assert( atm.lastSafeAirTemperature( aBadAirTemp ).kelvin( ) == 
            aGoodAirTemp.kelvin( ) );
    
    const Pressure aGoodAtmPressure( atm.DEFAULT_ATM_PRESSURE + 1.0, 
                                     "millibar" );
    atm.lastSafeAtmPressure( aGoodAtmPressure );
    const Pressure aBadAtmPressure( atm.MIN_ATM_PRESSURE - 1.0,
                                    "millibar" );
    assert( atm.lastSafeAtmPressure( aBadAtmPressure ).millibar( ) == 
            aGoodAtmPressure.millibar( ) );
    
    const Temperature aGoodDewPoint( atm.DEFAULT_DEW_POINT + 1.0, "K" );
    atm.lastSafeDewPoint( aGoodDewPoint );
    const Temperature aBadDewPoint( atm.MIN_DEW_POINT - 1.0, "K" );
    assert( atm.lastSafeDewPoint( aBadDewPoint ).kelvin( ) == 
            aGoodDewPoint.kelvin( ) );
    
    // Finally verify that straight return is as expected.
    assert( atm.lastSafeAirTemperature( ).kelvin( ) == 
            aGoodAirTemp.kelvin( ) );
    assert( atm.lastSafeAtmPressure( ).millibar( ) ==
            aGoodAtmPressure.millibar( ) );
    assert( atm.lastSafeDewPoint( ).kelvin( ) ==
            aGoodDewPoint.kelvin( ) );
  } catch ( ... ) {
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
