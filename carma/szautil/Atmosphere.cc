#include "carma/szautil/Atmosphere.h"
#include "carma/szautil/Coordinates.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

#include "carma/szaslalib/slalib.h"

#include <iomanip>

using namespace std;
using namespace sza::util;

Wavelength   Atmosphere::opticalWavelength_  = Wavelength(Length::Microns(), 0.57);
Angle        Atmosphere::refracAccuracy_     = Angle(Angle::MilliArcSec(), 10.0);
const double Atmosphere::tropoLapseRate_     = 0.0065;

const Length Atmosphere::dryScaleHeight_     = Length(Length::Kilometers(), 8.0);
const Length Atmosphere::wetScaleHeight_     = Length(Length::Kilometers(), 2.0);

// The sec(z) approximation used in the calculation of the pathlength
// here is fairly accurate to a zenith angle of about 80 degrees
// (elevation = 10 degrees).  At angles larger than this, it diverges
// quickly.  As a result, we will return the horizon approximation for
// elevations > pathlengthElevationLimit_

const Angle  Atmosphere::pathlengthElevationLimit_ = Angle(Angle::Degrees(), 10.0);

/**.......................................................................
 * Constructor.
 */
Atmosphere::Atmosphere() 
{
  initialize();
}

void Atmosphere::initialize() 
{
  humidity_     = 0.0;
  pressure_     = 0.0;
  
  lacking_ = ALLREFRAC;
  coefficientsAreSet_ = false;
}

/**.......................................................................
 * Destructor.
 */
Atmosphere::~Atmosphere() 
{
}

/**.......................................................................
 * Fully-specified method for computing refraction coefficients
 */
Atmosphere::RefractionCoefficients
Atmosphere::refractionCoefficients(Length altitude, Temperature airTemp,
				   double pressure, double humidity,
				   Wavelength wavelength,
				   Angle latitude,
				   double tropoLapseRate,
				   Angle accuracy)
{
  RefractionCoefficients coeff;

  COUT("Calling slaRefco with humidity = " << humidity);

  slaRefco(altitude.meters(), airTemp.K(), pressure,
	   humidity, wavelength.microns(), latitude.radians(), 
	   tropoLapseRate, accuracy.mas(), &coeff.a, &coeff.b);

  DBPRINT(true, Debug::DEBUG9, "Inside refractionCoefficients: parameters are: " << std::endl
	    << "Altitude   = " << std::setprecision(6) << altitude.meters()   << " m " << std::endl
	    << "Air Temp   = " << std::setprecision(6) << airTemp.K()         << " K " << std::endl
	    << "Pressure   = " << std::setprecision(6) << pressure            << " mBar" << std::endl
	    << "Humidity   = " << std::setprecision(6) << humidity            << " %" << std::endl
	    << "Wavelength = " << std::setprecision(6) << wavelength.microns()<< " microns" << std::endl
	    << "Latitude   = " << std::setprecision(6) << latitude.radians()  << " rad " << std::endl
	    << "TLR        = " << std::setprecision(6) << tropoLapseRate      << " K/m " << std::endl
	    << "Accuracy   = " << std::setprecision(6) << accuracy.mas()      << " mas " << std::endl
	    << "Coeff A    = " << std::setprecision(6) << coeff.a             << std::endl
	<< "Coeff B    = " << std::setprecision(6) << coeff.b             << std::endl);

  return coeff;
}

/**.......................................................................
 * Fully un-specified versions
 */
Atmosphere::RefractionCoefficients
Atmosphere::refractionCoefficients()
{
  return refractionCoefficients(altitude_, airTemperature_, 
				pressure_, humidity_, 
				wavelength_, latitude_,
				tropoLapseRate_, 
				refracAccuracy_);
}

Atmosphere::RefractionCoefficients
Atmosphere::opticalRefractionCoefficients()
{
  return refractionCoefficients(altitude_, airTemperature_, 
				pressure_, humidity_, 
				opticalWavelength_, latitude_,
				tropoLapseRate_, 
				refracAccuracy_);
}

// Set methods

void Atmosphere::setAirTemperature(Temperature airTemp) 
{
  airTemperature_ = airTemp;
  lacking_ &= ~TEMP;
}

void Atmosphere::setPressure(double pressure) 
{
  pressure_ = pressure;
  lacking_ &= ~PRESSURE;
}

void Atmosphere::setHumidity(double humidity) 
{
  humidity_ = humidity;
  lacking_ &= ~HUMIDITY;
}

void Atmosphere::setWavelength(sza::util::Wavelength wavelength) 
{
  wavelength_ = wavelength;
  lacking_ &= ~WAVE;
}

void Atmosphere::setFrequency(sza::util::Frequency frequency) 
{
  wavelength_ = sza::util::Wavelength(frequency);
  lacking_   &= ~WAVE;
}

void Atmosphere::setRx(sza::util::Rx::Id rxId) 
{
  wavelength_ = sza::util::Wavelength(Rx::getSkyFrequency(rxId));
  lacking_   &= ~WAVE;
}

void Atmosphere::setAltitude(Length altitude) 
{
  altitude_ = altitude;
  lacking_ &= ~ALTITUDE;
}

void Atmosphere::setLatitude(Angle latitude) 
{
  latitude_ = latitude;
  lacking_ &= ~LATITUDE;
}

bool Atmosphere::canComputeRefraction() 
{
  return !(lacking_ & ALLREFRAC);
}

bool Atmosphere::canComputeOpticalRefraction() 
{
  return !(lacking_ & ALLOPTICAL);
}

/**.......................................................................
 * Set the internal refraction coefficients for the passed
 * values
 */
void Atmosphere::setRefractionCoefficients(Length altitude, Temperature airTemp,
					   double pressure, double humidity,
					   Wavelength wavelength,
					   Angle latitude,
					   double tropoLapseRate,
					   Angle accuracy)
{
  // Set the refractionCoefficients

  coeff_ = refractionCoefficients(altitude, airTemp, pressure, humidity, 
				  wavelength, latitude, tropoLapseRate, 
				  accuracy);
  coefficientsAreSet_ = true;
}

/**.......................................................................
 * Set the internal refraction coefficients from stored values
 */
void Atmosphere::setRefractionCoefficients()
{
  if(canComputeRefraction()) {
    coeff_ = refractionCoefficients();
    coefficientsAreSet_ = true;
  } else {
    ThrowError("Not enough information to calculate refraction coefficients");
  }
}

/**.......................................................................
 * Calculate the refraction correction, in radians
 */
Angle Atmosphere::offset(Angle& elevation, RefractionCoefficients& coeff)
{
  double rad = elevation.radians();

  double c  = cos(rad);
  double c2 = c*c;
  double c3 = c2*c;
  
  double s  = sin(rad);
  double s2 = s*s;
  double s3 = s2*s;
  double s4 = s3*s;

  return Angle(Angle::Radians(), (coeff.a * c * s3 + coeff.b * s * c3)/
	       (s4 + (coeff.a * s2 + 3.0 * coeff.b * c2)));
}

/**.......................................................................
 * Calculate the refraction correction, in radians
 */
Angle Atmosphere::offset(Angle& elevation)
{
  if(!coefficientsAreSet_) 
    setRefractionCoefficients();
  
  return offset(elevation, coeff_);
}

/**.......................................................................
 * Method for computing the saturated vapor pressure
 */
Pressure Atmosphere::saturatedVaporPressure(Temperature airTemp)
{
  double tk     = airTemp.K();
  double prefac = 6.11 * pow(tk/273, -5.3);
  double expfac = exp(25.2 * (tk - 273) / tk);

  Pressure svp;
  svp.setMilliBar(prefac * expfac);

  return svp;
}

/**.......................................................................
 * Method for computing the refractivity of (wet + dry) air.
 *
 * The Smith-Weintraub equation, using coefficients given in TMS 13.74
 * (2nd ed).
 */
double Atmosphere::refractivityTotal(Temperature airTemp, Pressure pressure, double relHumid)
{
  return refractivityDry(airTemp, pressure, relHumid) + 
    refractivityWet(airTemp, pressure, relHumid);
}

/**.......................................................................
 * Method for calculating the refractivity of dry air
 */
double Atmosphere::refractivityDry(Temperature airTemp, Pressure pressure, double relHumid)
{
  double tk    = airTemp.K();
  double ptmb  = pressure.milliBar();

  // Get the partial pressure of the water vapor

  Pressure pvs = saturatedVaporPressure(airTemp);
  double pvmb  = relHumid * pvs.milliBar();

  // Get the partial pressure of the dry component alone

  double pdmb  = ptmb - pvmb;

  // Return the dry portion of the refractivity only

  return (77.6 * pdmb / tk);
}

/**.......................................................................
 * Method for calculating the refractivity of water vapor
 */
double Atmosphere::refractivityWet(Temperature airTemp, Pressure pressure, double relHumid)
{
  double tk     = airTemp.K();

  // Get the partial pressure of the water vapor

  Pressure pvs = saturatedVaporPressure(airTemp);
  double pvmb  = relHumid * pvs.milliBar();

  // Return the portion of the refractivity from water vapor only

  return (64.8 * pvmb / tk) + (3.776e5 * pvmb / (tk*tk));
}

/**.......................................................................
 * Return the pathlength associated with the component of given scale
 * height and refractivity
 */ 
Length Atmosphere::excessPathlength(double refractivity, Length scaleHeight, Length altitude, Angle elevation)
{
  double r   = altitude.meters() + Coordinates::earthEqRadiusMeters_;
  double rat = scaleHeight.meters() / r;
  Length pathlength;

  // The following expression diverges at z = 0

  if(elevation > pathlengthElevationLimit_) {
    double secz = 1.0 / cos(M_PI/2 - elevation.radians());
    pathlength.setMeters(1e-6 * refractivity * scaleHeight.meters() * ((1 + rat) - rat * secz*secz) * secz);
  } else {
    pathlength.setMeters(1e-6 * refractivity * sqrt(M_PI * r * scaleHeight.meters() / 2));
  }

  return pathlength;
} 

/**.......................................................................
 * Return the total pathlength 
 */
Length Atmosphere::excessPathlengthTotal(Angle elevation)
{
  if(!canComputePathlength()) {
    ThrowError("Unable to compute pathlength");
  }

  Pressure press;
  press.setMilliBar(pressure_);

  return excessPathlengthTotal(airTemperature_, press, humidity_, altitude_, 
			       elevation);
}

bool Atmosphere::canComputePathlength() 
{
  return !(lacking_ & ALLPATH);
}

/**.......................................................................
 * Return the total pathlength 
 */
Length Atmosphere::excessPathlengthTotal(Temperature airTemp, Pressure pressure, 
					 double relHumid, Length altitude, 
					 Angle elevation)
{
  double refracDry = refractivityDry(airTemp, pressure, relHumid);
  double refracWet = refractivityWet(airTemp, pressure, relHumid);

  return excessPathlength(refracDry, dryScaleHeight_, altitude, elevation) + 
    excessPathlength(refracWet, wetScaleHeight_, altitude, elevation);
}
