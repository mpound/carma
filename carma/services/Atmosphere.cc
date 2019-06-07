/**
 * @file Atmosphere.cc
 * $Id: Atmosphere.cc,v 1.1 2010/02/18 22:14:08 abeard Exp $
 * @author Marc Pound
 */
#include <iostream>
#include <cmath>
#include <limits>
#include <iostream>
#include <sys/time.h>
#include "carma/services/Atmosphere.h"
#include "carma/services/Astro.h"
#include "carma/services/Physical.h"
#include "carma/util/BaseException.h"

using namespace std;
using namespace carma::services;
using namespace carma::services::constants;

//------------------------------------------------------
//            PUBLIC METHODS
//------------------------------------------------------

// our decided-upon values for the various atmospheric scaleheights
// in meters
const Atmosphere::scaleHeightType Atmosphere::scaleHeight = 
   { 
     2216.0, // IR
     9480.0, // DRY
     2103.0, // WET
     1800.0  // OPACITy

   };

const double Atmosphere::SW_INDUCED_DIPOLE;
const double Atmosphere::HW_B;
const double Atmosphere::MAX_DEW_POINT = MAX_AIR_TEMP;


// constructor does nothing
Atmosphere::Atmosphere( ) :
    lastSafeAirTemperature_( DEFAULT_AIR_TEMP, "K" ),
    lastSafeAtmPressure_( DEFAULT_ATM_PRESSURE, "mbar" ),
    lastSafeDewPoint_( DEFAULT_DEW_POINT, "K" ),
    lastSafeRelativeHumidity_( DEFAULT_RH )
{ }

// destructor does nothing
Atmosphere::~Atmosphere() { }

double Atmosphere::computeZenithRefractivity( 
    const double airTemp,
    const double atmPressure,
    const double relHumid,
    const double frequency ) const
{
    // partial pressure of water vapor.
    // NB: the division by 100 to change percent humidity to a number
    // between 0 and 1 is already taken care of in the
    // fixed SMITH_WEINTRAUB coefficients.
    double ppw = relHumid * computeSaturatedPressure(airTemp);

    double zenithRefrac;
    if ( isOptical(frequency) ) {
	//For now, we do the simple BIMA/OVRO optical refraction
	//which is the Smith-Weintraub equation w/o the permanent
	//dipole term.  Future enhancement will use more sophisticated
	//formulation.
	 zenithRefrac =   SW_DRY_AIR * atmPressure / airTemp 
	                - SW_INDUCED_DIPOLE * ppw / airTemp ;
    } else {
    // equation 14 from design doc. (Smith Weintraub equation)
	 zenithRefrac =   SW_DRY_AIR * atmPressure / airTemp 
	                - SW_INDUCED_DIPOLE * ppw / airTemp 
	                + SW_PERM_DIPOLE * ppw / (airTemp * airTemp);
    }

    // multiply by frequency dependence equation
    // NB: We have decided to postpone this correction until
    // after first light, maybe indefinitely.
    //zenithRefrac *= freqDependence(frequency);
    
    return zenithRefrac;
}

double Atmosphere::computeRefractionCorrection(
    const double airTemp, 
    const double atmPressure,
    const double relHumid,
    const double elevation,
    const double frequency,
    const double altitude ) const
{
    // Until I figure out the problem with reproducing Mangum's figure
    // use the standard NRAO correction, which I have verified.
    if ( 1 == 1 )  {
     return ulichRefractionCorrection(airTemp, atmPressure,
	                              relHumid,elevation, frequency);
    } else {
    double zenithRefrac = computeZenithRefractivity(airTemp,atmPressure,
	                                       relHumid, frequency);
    double mapf  = computeMappingFunction(airTemp, atmPressure,
	                                  relHumid, elevation, frequency,
					  altitude);

    // return value in radians which should be ADDED to the in vacuo
    // elevation.  10^-6 scaling is to go from refractivity to refraction
    // angle.
    double rcorr = zenithRefrac * cos(elevation) * mapf * units.micro();
    //cout << "#YANG MAPF*COS(E): "<< mapf*cos(elevation) << endl;

    return rcorr;
    }
}

double Atmosphere::ulichRefractionCorrection(
    const double airTemp, 
    const double atmPressure, 
    const double relHumid,
    const double elevation,
    const double frequency ) const 
{
    double zenithRefrac = computeZenithRefractivity(airTemp,atmPressure,
	                                       relHumid, frequency);
    double mapf  = ulichMappingFunction(elevation);
    //cout << "#ULICH MAPF: "<< mapf << endl;

    double rcorr = zenithRefrac * mapf * units.micro();
    return rcorr;

}

// returns surface saturated water vapor pressure
double Atmosphere::computeSaturatedPressure( const double airTemp ) const
{

    // see equation 13 of InterferometryDesign design document 
    // ABS_ZERO is negative
    double tempRatio = (airTemp + Physical::ABS_ZERO) / airTemp;
    double scaledTemp = airTemp / (-Physical::ABS_ZERO);
    double satPressure = CC_A * 
	                 exp( CC_B * tempRatio - CC_C * log(scaledTemp) );

    //@todo move to this formula everywhere for internal consistency
    //*or* derive the equivalent AWIPS values from the CC values.
    //satPressure = exp(AWIPS_C15 - AWIPS_C1 * airTemp - AWIPS_C2 / airTemp );

    return satPressure;

}

// returns water vapor density, in g/m^3
double Atmosphere::waterVaporDensity( const double airTemp, 
                                      const double relHumid ) const 
{
  // We can calcuated the density of water vapor from the ideal
  // gas law:
  // pV = m R_i T
  // where R_i is the individual gas constant for the molecule in question.
  // p is partial pressure of water
  // R_i (water) = 461.5 J/kg*K
  //
  // density = m/V, so
  // density = p/(R_i T)

  const double ppw = waterPartialPressure(airTemp, relHumid);
 
  // Since R_WATER is in SI, we convert pressure to Pascals, which will give
  // the density in kg/m^3.  We want final answer in g/m^3, so two 
  // conversions are needed.  Yes, I know the last one is exactly 1000, but
  // code readabilty trumps all.  Besides, we may change units later.
  double factor = units.convert("mbar","Pa") * units.convert("kg/m^3","g/m^3");

  return ( factor * ppw / (Physical::R_WATER * airTemp) );
}

double Atmosphere::waterPartialPressure(const double airTemp, 
                                        const double relHumid) const
{
    // partial pressure of water vapor.
    // divide by 100 to go from RH% -> (0..1)
    return ( relHumid / 100.0 )  * computeSaturatedPressure(airTemp);
}

// returns total water column, in mm
double Atmosphere::waterColumn( const double airTemp, 
                                const double relHumid) const
{
  // Yes, we divide by 1 here, which is no-op, but it is for
  // the sake of code readability.
  const double rho_water = 1.0;   // density of water in g/cm^3
  double value = waterVaporDensity(airTemp, relHumid) 
                 * scaleHeight.opacity / rho_water;

  // conversion to millimeters from our current funny units.
  double factor = units.convert("m (g/m^3)/(g/cm^3)","mm");
  return ( value  * factor );

}

double Atmosphere::computeDewPoint(
    const double airTemp, 
    const double relHumid, 
	const dewPointMethodType method) const
{
  double dewTemp;
  double alpha, b, ppw, Tc;

  switch (method) {

      default:
      case CLAUSIUS_CLAPEYRON:
	// partial pressure of water vapor.
	ppw = waterPartialPressure(airTemp, relHumid);
	b = AWIPS_C15 - log(ppw);
	//b = CC_B - log(ppw);
	dewTemp = ( b - sqrt(b*b-AWIPS_C3) )/AWIPS_C4;
	break;

      case MAGNUS_TETENS:
    // The Magnus-Tetens formula is explained here:
    // http://www.paroscientific.com/dewpoint.htm
    // Good over the range Tc = 0 to 60 C, Tdew = 0 to 50 C.
	Tc = airTemp + Physical::ABS_ZERO;
	alpha = MT_A * Tc/(MT_B + Tc) + log(relHumid/100.0);
	dewTemp = MT_B *alpha/(MT_A - alpha) - Physical::ABS_ZERO;
	break;

      case HOFFMAN_WELCH:
	// Hoffman & Welch from HatCreek's weatherman1.c code.
	// This is the integrated form Clausius Clapeyron equation where 
	// one temperature bound is set to infinity.
	// ln(P1/P2) = H/R (1/T2 - 1/T1)
	// let T2 = infinity, P2 = P0
	//
	// P = P0*exp(-H/RT)
	// where P is saturated water vapor pressure
	//       P0 is vapor pressure of water at infinite temperature
	//       H is the enthalpy of evaporation of water
	//       R is the gas constant
	
	ppw = HW_A * exp(-HW_B/airTemp) * (relHumid/100.0);  // pp of water, [mm Hg]
	dewTemp = -HW_B/log(ppw/HW_A);   // Kelvin
	break;

  }

  return dewTemp;
}


double Atmosphere::computeHumidity(
    const double airTemp, 
    const double dewTemp, 
    const dewPointMethodType method) const
{
  double relHumid;
  double Tc = airTemp + Physical::ABS_ZERO;
  double Td = dewTemp + Physical::ABS_ZERO;
  double ratioB, ratioA;
  switch ( method ) {
      default:
      case CLAUSIUS_CLAPEYRON:
        // This is the definition of relative humidity!
	relHumid = 100.0 * computeSaturatedPressure(dewTemp)/computeSaturatedPressure(airTemp);
        break;
      case MAGNUS_TETENS:
	ratioA = (MT_A * Tc)/(MT_B + Tc);
	ratioB = (MT_A * Td - Td * ratioA - MT_B * ratioA)/(MT_B + Td);
	relHumid = 100.0 * exp( ratioB );
        break;
      case HOFFMAN_WELCH:
	// Hoffman & Welch from HatCreek's weatherman1.c code
	double Ph2o = HW_A * exp(-HW_B/dewTemp);
	relHumid = 100*Ph2o*exp(HW_B/airTemp)/HW_A;
        break;
  }
  return relHumid;
}

// returns refractivity integrated through the atmosphere,
// that is, the pathlength.
double Atmosphere::computePathlength( 
    const double airTemp,
    const double atmPressure,
    const double relHumid,
    double elevation,
    const double frequency,
    const double altitude ) const
{

    // If we are observing "over the top", then the
    // elevation can be larger than 90 degrees.
    // This will give a negative tan(elev) value and
    // therefore a negative pathlength, which is unphysical.
    // So put elevation into first quadrant if needed.
    // This is okay since the atmosphere is assumed to
    // be azimuthally symmetric.  
    if (elevation > M_PI_2 ) {
	elevation = M_PI - elevation;
    }

    // If we are below the minimum elevation,
    // then return L_max as defined by TMS eqn 13.40
    // At 7 degrees, we underpredict L by 2.2% for a standard atmosphere.
    if (elevation < MIN_ELEVATION) {
	//cout << "using horizon path length" << endl;
	return computeHorizonPathlength(airTemp, atmPressure, 
		                        relHumid, frequency);
    }
    
    // leading coefficient, 1/sin(E) safe because
    // we've already eliminated low elevations
    double factor = units.micro()/(airTemp*sin(elevation));

    // Cotangent squared of elevation.
    // Do it by cos/sin instead of 1/tan because tan
    // diverges at 90 degrees.  cos/sin is safe because
    // we have already eliminated small elevations where
    // sin(E) would diverge.
    double cotE = cos(elevation)/sin(elevation);
    double cotsqE = cotE*cotE;

    // saturated H2O pressure, millibars
    double satPressure = computeSaturatedPressure(airTemp);

    // partial pressure times 100.
    // NB: the division by 100 to change percent humidity to a number
    // between 0 and 1 is already taken care of in the
    // fixed SMITH_WEINTRAUB coefficients.
    double ppw = relHumid * satPressure;
    
    // add the antenna height above the ground plane to
    // the earth radius to get total height above the
    // center of the earth.
    double adjustedRadius = Astro::EARTH.radius + altitude;

    // compute three terms in the equation separately.
    // Since scaleheight and Earth radius are in meters
    // the resultant pathlength will be in meters.
    double term1 = SW_DRY_AIR * atmPressure * scaleHeight.dry;
    term1 *= (1.0 - cotsqE * scaleHeight.dry / adjustedRadius);

    // ----------------- TEST: "EXACT" FROM TMS -----------
    // Attempt to reproduce TMS figure 13.35, making a guess as
    // to what they used for atmPressure, scaleHeight.dry, and EARTH.radius.
    // This should give about L=2.31 meters at zenith.
    //term1 = SW_DRY_AIR * DEFAULT_ATM_PRESSURE * 8.0;
    //term1 *= (1.0 - cotsqE * 8.0 / 6370.0); 
    // ---------------- TEST--------------------------

    double term2 = -SW_INDUCED_DIPOLE * ppw * scaleHeight.wet;
    term2 *= (1.0 - cotsqE * scaleHeight.wet / adjustedRadius);
 
    double term3 = SW_PERM_DIPOLE * ppw * scaleHeight.ir;
    term3 *= (1.0 - cotsqE * scaleHeight.ir / adjustedRadius);
    term3 /= airTemp;

    // -------------- TEST EXACT FROM TMS --------------------------
    // In figure 13.35, they set rho_v to zero, which is equivalent
    // to ignoring the second and third terms.
    //term2 = 0.;
    //term3 = 0.;
    // -------------- TEST EXACT FROM TMS --------------------------
    
    // pathlength in meters
    double pathlength = factor*(term1 + term2 + term3); 

    // multiply by frequency dependence equation.
    // NB: We have decided to postpone this correction until
    // after first light
    //pathlength *= freqDependence(frequency); 

    /*cout << "elev " << elevation << endl;
    cout << "factor " << factor << endl;
    cout << "cotsqE " << cotsqE << endl;
    cout << "satPres " << satPressure << endl;
    cout << "term1 " << term1 << endl;
    cout << "term2 " << term2 << endl;
    cout << "term3 " << term3 << endl;
    cout << "freqDep " << freqDependence(frequency) << endl;
    cout << "pathlength (m) " << pathlength << endl;
    */

    return pathlength ;
}

double Atmosphere::computeHorizonPathlength(
    const double airTemp,
    const double atmPressure,
    const double relHumid,
    const double frequency ) const
{
    // First, get the zenith refractivity.
    double pathlength = computeZenithRefractivity(airTemp,
	                                          atmPressure,
	 					  relHumid,
						  frequency);

    //double zrefrac = pathlength;

    // TMS equation 13.40 computes horizon pathlength in
    // whatever units Earth radius and atmospheric scale height
    // happen to be in, and uses a coefficient out front of 10^-6. 
    // Note we use the "wet" scale height, since TMS uses 2 kilometers.

    double factor = M_PI * Astro::EARTH.radius * scaleHeight.wet / 2.0;
    pathlength *= sqrt(factor);
    pathlength *= units.micro();

    // ratio should be about 0.14 for pathlength in meters,
    // before applying frequency dependence.
    //cout << " pathlength/zrefrac = " << pathlength/zrefrac;
    
    // multiply by frequency dependence equation.
    // NB: We have decided to postpone this correction until
    // after first light
    //pathlength *= freqDependence(frequency);

    return pathlength; 

}


// returns the bounded air temperature
double Atmosphere::safeAirTemperature( const double airTemp )
{
    if ( ! isSafeAirTemperature( airTemp ) )
    {
	    return DEFAULT_AIR_TEMP;
    } else {
	    return airTemp;
    }
}

bool Atmosphere::isSafeAirTemperature( const double airTemp ) 
{
    if ( ( airTemp < MIN_AIR_TEMP ) ||
	 ( airTemp > MAX_AIR_TEMP ) ||
	 isnan( airTemp )
       ) 
    {
	    return false;
    } else {
	    return true;
    }
}

bool Atmosphere::isSafeAirTemperature( Temperature airTemp )
{
    return isSafeAirTemperature( airTemp.kelvin() );
}

// returns the bounded air temperature
Temperature Atmosphere::safeAirTemperature( Temperature airTemp ) 
{
    if ( ! isSafeAirTemperature( airTemp ) ) {
	    return Temperature(DEFAULT_AIR_TEMP, "K");
    } else {
	    return airTemp;
    }
}

Temperature Atmosphere::lastSafeAirTemperature( const double airTemp )
{
    if ( isSafeAirTemperature( airTemp ) )
        lastSafeAirTemperature_.reset( airTemp, "K" );

    return lastSafeAirTemperature_;
}

Temperature Atmosphere::lastSafeAirTemperature( const Temperature airTemp )
{
    if ( isSafeAirTemperature( airTemp ) )
        lastSafeAirTemperature_ = airTemp;

    return lastSafeAirTemperature_;
}

Temperature Atmosphere::lastSafeAirTemperature( ) const
{
    return lastSafeAirTemperature_;
}

bool Atmosphere::isSafeRelativeHumidity( double relHumid ) 
{
    if ( ( relHumid < MIN_RH ) ||
	 ( relHumid > MAX_RH ) ||
	   isnan(relHumid) 
        ) 
    {
	    return false;
    } else {
	    return true;
    }
}

// returns the bounded RH%
double Atmosphere::safeRelativeHumidity( double relHumid ) 
{
    if ( ! isSafeRelativeHumidity( relHumid ) )
    {
	    return DEFAULT_RH;
    } else {
	    return relHumid;
    }
}

double Atmosphere::lastSafeRelativeHumidity( const double relHumid ) 
{
    if ( isSafeRelativeHumidity( relHumid ) )
        lastSafeRelativeHumidity_ = relHumid;

    return lastSafeRelativeHumidity_;
}

double Atmosphere::lastSafeRelativeHumidity( ) const
{
    return lastSafeRelativeHumidity_;
}

bool Atmosphere::isSafeAtmPressure( double atmPressure ) 
{
    if ( ( atmPressure < MIN_ATM_PRESSURE ) ||
	 ( atmPressure > MAX_ATM_PRESSURE ) ||
	   isnan(atmPressure) 
        ) 
    {
	    return false;
    } else {
	    return true;
    }

}
// returns the bounded atmospheric pressure
double Atmosphere::safeAtmPressure( double atmPressure ) 
{
    if ( !isSafeAtmPressure( atmPressure ) )
    {
	    return DEFAULT_ATM_PRESSURE;
    } else {
	    return atmPressure;
    }

}

bool Atmosphere::isSafeAtmPressure( Pressure atmPressure ) 
{
    return isSafeAtmPressure( atmPressure.millibar() );
}
// returns the bounded atmospheric pressure
Pressure Atmosphere::safeAtmPressure( Pressure atmPressure ) 
{
    if ( !isSafeAtmPressure( atmPressure ) )
    {
	    return Pressure(DEFAULT_ATM_PRESSURE,"mbar");
    } else {
	    return atmPressure;
    }
}

Pressure Atmosphere::lastSafeAtmPressure( const Pressure atmPressure )
{
    if ( isSafeAtmPressure( atmPressure ) ) 
        lastSafeAtmPressure_ = atmPressure;
        
    return lastSafeAtmPressure_;
}

Pressure Atmosphere::lastSafeAtmPressure( const double atmPressure ) 
{
    if ( isSafeAtmPressure( atmPressure ) )
        lastSafeAtmPressure_.reset( atmPressure, "mbar" );
        
    return lastSafeAtmPressure_;
}

Pressure Atmosphere::lastSafeAtmPressure( ) const
{
    return lastSafeAtmPressure_;
}

bool Atmosphere::isSafeDewPoint( double dewPoint ) 
{
    if ( ( dewPoint < MIN_DEW_POINT ) ||
	 ( dewPoint > MAX_DEW_POINT ) ||
	 isnan( dewPoint )
       ) 
    {
	    return false;
    } else {
	    return true;
    }

}
double Atmosphere::safeDewPoint( double dewPoint ) 
{
    if ( ! isSafeDewPoint( dewPoint ) )
    {
	    return DEFAULT_DEW_POINT;
    } else {
	    return dewPoint;
    }

}

bool Atmosphere::isSafeDewPoint( Temperature dewPoint ) 
{
    return isSafeDewPoint( dewPoint.kelvin() );
}

Temperature Atmosphere::safeDewPoint( Temperature dewPoint ) 
{
    if ( ! isSafeDewPoint( dewPoint ) )
    {
	    return Temperature(DEFAULT_DEW_POINT,"K");;
    } else {
	    return dewPoint;
    }

}

Temperature Atmosphere::lastSafeDewPoint( const Temperature dewPoint )
{
    if ( isSafeDewPoint( dewPoint ) )
        lastSafeDewPoint_ = dewPoint;
        
    return lastSafeDewPoint_;
}

Temperature Atmosphere::lastSafeDewPoint( const double dewPoint )
{
    if ( isSafeDewPoint( dewPoint ) )
        lastSafeDewPoint_.reset( dewPoint, "K" );

    return lastSafeDewPoint_;
}

Temperature Atmosphere::lastSafeDewPoint( ) const
{
    return lastSafeDewPoint_;
}

double Atmosphere::safeElevation( double elevation ) 
{
    if ( ! isSafeElevation(elevation) )
    {
	    return MIN_ELEVATION;
    } else {
	    return elevation;
    }

}

bool Atmosphere::isConsistentDewPoint( 
	                        Temperature dewPoint, 
	                        Temperature airTemp, 
				double relHumid, 
				dewPointMethodType method,
	                        double consistencyPercent 
				)

{
    double dp = computeDewPoint( airTemp.kelvin(), relHumid, method );
    double percentDiff = fabs( ( dp - dewPoint.kelvin() )/dp );
    return ( percentDiff <= consistencyPercent );
}

bool Atmosphere::isSafeElevation( double elevation ) 
{
    return  ! ( elevation < MIN_ELEVATION || isnan(elevation) );
}

bool Atmosphere::isSafeElevation( Angle elevation ) 
{
    return isSafeElevation( elevation.radians() );
}

Angle Atmosphere::safeElevation( Angle elevation ) 
{
    if ( ! isSafeElevation( elevation ) ) 
    {
	    return Angle(MIN_ELEVATION,"radians");
    } else {
	    return elevation;
    }

}

bool Atmosphere::isSafeWindSpeed( double windSpeed ) 
{
    if ( ( windSpeed < MIN_WIND_SPEED ) ||
	 ( windSpeed > MAX_WIND_SPEED ) ||
	 isnan(windSpeed)
       ) 
    {
	    return false;
    } else {
	    return true;
    }

}

bool Atmosphere::isSafeWindSpeed( Velocity windSpeed ) 
{
    return isSafeWindSpeed( windSpeed.mph() );
}

double Atmosphere::safeWindSpeed( double windSpeed ) 
{
    if ( ! isSafeWindSpeed( windSpeed ) ){
	    return DEFAULT_WIND_SPEED;
    } else {
	    return windSpeed;
    }

}

Velocity Atmosphere::safeWindSpeed( Velocity windSpeed ) 
{
    if ( ! isSafeWindSpeed( windSpeed ) ) {
	    return Velocity(DEFAULT_WIND_SPEED,"mph");;
    } else {
	    return windSpeed;
    }

}


//------------------------------------------------------
//            PRIVATE METHODS
//------------------------------------------------------
double Atmosphere::freqDependence( double freq )
{
    // convert Hz to GHz
    freq *= units.nano();

    // If freq is less than to fiducial frequency,
    // then the scale factor is unity.
    //std::cout.setf(ios::fixed);
    //std::cout.precision(8);
    if ( freq < MIN_REFRAC_FREQ) {
	//std::cout << "Freq " << freq << " <= " << MIN_REFRAC_FREQ << endl;

	return 1.0;
    } else {
	//std::cout << "Freq " << freq << " > " << MIN_REFRAC_FREQ << endl;
	// scale factor is (1 + A*freq + B*freq^2) 
	return ( 1.0 + REFRAC_COEFF_A * freq +
	         REFRAC_COEFF_B * freq * freq
	       );
    }
}

double Atmosphere::computeMappingFunction(
	               double airTemp, double atmPressure, 
                       double relHumid, 
                       double elevation,
		       double frequency,
		       double altitude) const
{


    double a1, a2; // the individual refraction expressions. 
    
    // Set up some intermediate values.

    // Be sure to guard against zero elevation
    double sinE = sin(safeElevation(elevation)); 

    double p0  = atmPressure + P_OFFSET;
    double t0  = airTemp + T_OFFSET;

    // partial pressure of water vapor
    double ppw = waterPartialPressure(airTemp, relHumid);

    double nez = normalizedEffectiveZenith(airTemp, elevation, altitude);

    // If the frequency is greater than 3 THz, use the optical
    // refraction experessions, otherwise use the radio.
    if ( isOptical(frequency) ) {
        double w0 = units.convert(Physical::C/frequency,"m","micron") 
	            + WAVE_OFFSET;
        a1 = a1Optical(p0,t0,ppw,w0);
        a2 = a2Optical(p0,t0,ppw,w0);
    } else {
        a1 = a1Radio(p0,t0,ppw);
        a2 = a2Radio(p0,t0,ppw);
    }
    cout.precision(6);

    // Mapping function is a continued fraction. Do it in parts.
    double f     = sinE + MAP1 / (nez + MAP2);
    double value = sinE +   a1 / (nez + a2/f);
    /*
    cout << "#MAP: "
	 << "temp: "<< t0  << "  " 
	 << "pres: "<< p0  << "  "
	 << "PPW:" << ppw << "  "
	 << "freq:" << frequency << "  "
	 << "wave:" << w0 << "  "
	 << "Erad: " << elevation << "  "
	 << "nu: " <<frequency << "  "
	 << "ALT:" <<altitude  << "  "
	 << "NEZ: " <<nez    << "  "
         << "a1: " <<a1     << "  "
         << "a2: " <<a2     << "  "
         << "f: " <<f     << "  "
         << "return: " <<1.0/value << "  "
	 << endl;
    */
    return 1.0/value;

}

double Atmosphere::ulichMappingFunction( const double elevation) const
{
    double el = safeElevation(elevation);
    double sinE = sin(el);
    double cosE = cos(el);
    double tanE = tan((87.5*M_PI/180.0) - el);
    return cosE/(sinE+0.00175*tanE);
}

bool Atmosphere::isOptical(double frequency) const {
    if ( frequency > (3.0 * units.tera()) ) {
	return true;
    } else {
	return false;
    }
}

double Atmosphere::normalizedEffectiveZenith(
    const double airTemp, 
    const double elevation, 
    const double altitude) const
{

    // the normalized effective zenith is
    // sqrt(r0/2H)*tan(E).
    // However, in all equations it appears as 
    // (I^2)*csc(E) = (r0/2H)*tan^2(E)*csc(E)
    //              = (r0/2H)*[sin^2(E)/cos^2(E)]*[1/sin(E)]
    //              = (r0/2H)*sin(E)/cos^2(E).
    //
    //This quantity diverges at cos(E)=0, or E=90 degrees.
   
    // Check for elevation = 90, which would cause divergence.
    // If so, just return a large number.
    numeric_limits<double> dLimits;
    double diff = abs(elevation - M_PI/2);
    if(diff <= dLimits.epsilon()) {
	// Return a number as close to infinity as we are allowed.
	// returning a large number here is fine because
	// it will be put in the denominators of the
	// continued fraction mapping function. And besides
	// the whole thing gets multipied by cos(elevation)
	// in the end, which is zero.
	return dLimits.max(); 
    }

    double effHeight  = airTemp * RMG; // effective height, in meters
    double fact = (altitude + Astro::EARTH.radius)/(2.0*effHeight);
    /*
    cout << "#In nez: Erad: "<< elevation 
	<< " airTemp:   " << airTemp
	<< " radius:   " << altitude + Astro::EARTH.radius
	<< " effheight: " << effHeight 
	<< " fact: " << fact << endl;
    */
    return fact * sin(elevation)/pow(cos(elevation),2.0);
}

double Atmosphere::a1Radio(
    const double p0, 
    const double t0, 
    const double ppw) const
{
    double value = A1_RAD_1 + A1_RAD_P*p0 + A1_RAD_PPW*ppw 
                 + A1_RAD_PPWSQ*ppw*ppw + A1_RAD_T*t0
                 + A1_RAD_TSQ*t0*t0; 
    return value;
}
    
double Atmosphere::a2Radio( 
    const double p0,
    const double t0, 
    const double ppw) const 
{
    double value = A2_RAD_1 + A2_RAD_P*p0 + A2_RAD_PPW*ppw 
                 + A2_RAD_PPWSQ*ppw*ppw + A2_RAD_T*t0
                 + A2_RAD_TSQ*t0*t0; 
    return value;

}

double Atmosphere::a1Optical(
    const double p0,
    const double t0, 
    const double ppw, 
    const double w0) const
{
    double value = A1_OPT_1 + A1_OPT_P*p0 + A1_OPT_PPW*ppw 
                 + A1_OPT_PPWSQ*ppw*ppw + A1_OPT_T*t0
                 + A1_OPT_TSQ*t0*t0 + A1_OPT_WAVE*w0
                 + A1_OPT_WAVESQ*w0*w0;
    return value;

}
    
double Atmosphere::a2Optical(
    const double p0, 
    const double t0, 
    const double ppw, 
    const double w0) const
{
    double value = A2_OPT_1 + A1_OPT_P*p0 + A2_OPT_PPW*ppw 
                 + A2_OPT_PPWSQ*ppw*ppw + A2_OPT_T*t0
                 + A2_OPT_TSQ*t0*t0 + A2_OPT_WAVE*w0
                 + A2_OPT_WAVESQ*w0*w0;
    return value;

}

