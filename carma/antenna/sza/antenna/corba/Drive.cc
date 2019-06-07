#include <stdlib.h>
#include <iostream>
#include <string>

#include "carma/antenna/sza/antenna/corba/Drive.h"

using namespace sza::antenna::corba;
/*
 * Return a pointer to an object describing the current pointing mode
 */
void Drive::scheduleNextSequenceNo(unsigned long seq)
{
  std::cout << "Drive::scheduleNextSequenceNo() stub" << std::endl;
}

/*
 * Stow the telescope
 */
void Drive::stow(carma::antenna::common::DriveControl::Position position,
		 unsigned long seq)
{
  std::cout << "Drive::stow() stub" << std::endl;
};

/*
 * Stope the telescope
 */
void Drive::stop()
{
  std::cout << "Drive::stop() stub" << std::endl;
};
/*
 * Track to minimize snow accumulation
 */
void Drive::trackSnow()
{
  std::cout << "Drive::snowTrack() stub" << std::endl;
};
/*
 * Track to minimize wind damage
 */
void Drive::trackWind()
{
  std::cout << "Drive::trackWind() stub" << std::endl;
};
/*
 * Set the maximum tracking rate
 */
void Drive::setMaxRate(float azRate, float elRate)
{
  std::cout << "Drive::setMaxRates() stub" << std::endl;
};

void Drive::setAzMaxRate(float azRate)
{
  std::cout << "Drive::setAzMaxRates() stub" << std::endl;
};

void Drive::setElMaxRate(float elRate)
{
  std::cout << "Drive::setElMaxRates() stub" << std::endl;
};

/*.......................................................................
 * Set the encoder limits
 */
void Drive::setEncoderLimits(long azMin, long azMax,
			     long elMin, long elMax)
{
  std::cout << "Drive::setEncoderLimits() stub" << std::endl;
};

/*.......................................................................
 * Set the encoder calibation
 */
void Drive::setEncoderCountsPerTurn(long az, long el)
{
  std::cout << "Drive::setEncoderCountsPerTurn() stub" << std::endl;
};

/*.......................................................................
 * Set the encoder zero points
 */
void Drive::setEncoderZeros(double az, double el)
{
  std::cout << "Drive::setEncoderZeros() stub" << std::endl;
};

/*
 * Set the telescope location
 */
void Drive::setAntLocation(double longitude, double latitude, double altitude)
{
  std::cout << "Drive::antLocation() stub" << std::endl;
};

/*
 * Set the telescope location
 */
void Drive::setFiducialLocation(double longitude, double latitude, double altitude)
{
  std::cout << "Drive::fiducialLocation() stub" << std::endl;
};

/*
 * Set the telescope location
 */
void Drive::setRelativeLocation(double north, double east, double up)
{
  std::cout << "Drive::setRelativeLocation() stub" << std::endl;
};

/*
 * Set the pointing mode of the telescope
 */
void Drive::selectAperture(carma::antenna::common::
			   DriveControl::Aperture model)
{
  std::cout << "Drive::setPointing() stub" << std::endl;
};
/*
 * Set the Azimuth wrap mode
 */
void Drive::setWrap(carma::antenna::common::DriveControl::AzWrapMode azWrapMode,
		    bool elOverTheTop)
{
  std::cout << "Drive::setWrap() stub" << std::endl;
};

/*
 * Move to a requested AZ position
 */
void Drive::setAzel(double az, double el, unsigned long seq)
{
  std::cout << "Drive::setAzel() stub" << std::endl;
};

/*
 * Move to a requested AZ position
 */
void Drive::setAz(double az, unsigned long seq)
{
  std::cout << "Drive::setAz() stub" << std::endl;
};

/*
 * Move to a requested EL position
 */
void Drive::setEl(double el, unsigned long seq)
{
  std::cout << "Drive::setEl() stub" << std::endl;
};

/**.......................................................................
 * Set the slew rate.
 */
void Drive::setSlewRate(unsigned long axes,
			long az, long el)
{
  std::cout << "Drive::setSlewRate() stub" << std::endl;
}

/*
 * Move to a requested AZ/EL position
 */
void Drive::slew(const char* source, unsigned long axes,
		 double az, double el)
{
  std::cout << "Drive::slew() stub" << std::endl;
};

/*
 * Move to a requested RA/DEC position
 */
void Drive::setRaDec(double mjd, double ra,
		     double dec, double distance)
{
  std::cout << "Drive::setRaDec() stub" << std::endl;
};

/*
 * Update the UT1-UTC interpolation parameters
 */
void Drive::setUt1Utc(double mjd, double ut1utc)
{
  std::cout << "Drive::setUt1Utc() stub" << std::endl;
};
/*
 * Update the Equation of the equinoxes interpolation parameters
 */
void Drive::setEqnEqx(double mjd, double eqneqx )
{
  std::cout << "Drive::setEqnEqx() stub" << std::endl;
};
/*
 * Update the refraction corrections
 */
void Drive::setRefrac(double a, double b )
{
  std::cout << "Drive::setRefrac() stub" << std::endl;
};
/*
 * Install pointing offsets
 */
void Drive::setMountOffset(double az, double el, unsigned long seq)
{
  std::cout << "Drive::setAzElOffsets() stub" << std::endl;
};

void Drive::track(const char* source,
		  const carma::antenna::common::DriveControl::RaDecTriplet& positionTriplet,
		  carma::antenna::common::DriveControl::AzWrapMode azWrapMode,
		  bool overTheTop,
		  unsigned long seq)
{
  std::cout << "Drive::track() stub" << std::endl;
}

void Drive::updateRaDec(const carma::antenna::common::DriveControl::RaDecEpoch& position,
		 carma::antenna::common::DriveControl::AzWrapMode azWrapMode)
{
  std::cout << "Drive::updateRaDec() stub" << std::endl;
};

void Drive::updateWeather(float ambientTemp,
		   float barometricPressure,
		   float relativeHumidity,
		   float dewpointTemp,
		   float windSpeed,
		   float windDirection)
{
  std::cout << "Drive::updateWeather() stub" << std::endl;
};

void Drive::setAzMountOffset(double az,
		      unsigned long seq)
{
  std::cout << "Drive::setAzMountOffset() stub" << std::endl;
};

void Drive::setElMountOffset(double el,
		      unsigned long seq)
{
  std::cout << "Drive::setElMountOffset() stub" << std::endl;
};

void Drive::setOffset(double az,
	       double el,
	       unsigned long seq)
{
  std::cout << "Drive::setOffset() stub" << std::endl;
};

void Drive::setAzOffset(double az,
		 unsigned long seq)
{
  std::cout << "Drive::setAzOffset() stub" << std::endl;
};

void Drive::setElOffset(double el,
		 unsigned long seq)
{
  std::cout << "Drive::setElOffset() stub" << std::endl;
};

void Drive::
setAperturePointingConstants(carma::antenna::common::DriveControl::Aperture
			     aperture,
			     float azOffset,
			     float elOffset,
			     float sag)
{
  std::cout << "Drive::setAperturePointingConstants() stub" << std::endl;
};

void Drive::setTiltmeterZero(float aftForward,
		      float leftRight)
{
  std::cout << "Drive::setTiltmeterZero() stub" << std::endl;
};

void Drive::setTolerance(float toleranceInArcsecs)
{
  std::cout << "Drive::setTolerance() stub" << std::endl;
};

void Drive::setSafeRange(float azLow,
			 float azHigh,
			 float elLow,
			 float elHigh)
{
  std::cout << "Drive::setSafeRange() stub" << std::endl;
};

