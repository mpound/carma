#ifndef DRIVE_H
#define DRIVE_H

/**
 * @file Drive.h
 *
 * Tagged: Fri Nov 14 12:37:54 UTC 2003
 *
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/corba/Corba.h"

#include "carma/antenna/common/DriveControl.h"

#include "carma/antenna/sza/antenna/corba/PointingModel.h"

/**
 * Define a Drive class
 */
namespace sza {
  namespace antenna {
    namespace corba {


      class Drive { 

      public:

	  void scheduleNextSequenceNo(unsigned long seq);

	  void setAntLocation(double longitude, double latitude,
			      double altitude);

	  void setFiducialLocation(double longitude, double latitude,
				   double altitude);

	  void setRelativeLocation(double north, double east, double up);

	  void setAzel(double az, double el, unsigned long seq);

	  void setAz(double az, unsigned long seq);

	  void setEl(double el, unsigned long seq);

	  void setMountOffset(double az, double el, unsigned long seq);

	  void setEncoderCountsPerTurn(long az, long el);

	  void setEncoderLimits(long azMin, long azMax,
				long elMin, long elMax);

	  void setEncoderZeros(double az, double el);

	  void setEqnEqx(double mjd, double eqneqx);

	  void setMaxRate(float azRate, float elRate);

	  void setAzMaxRate(float azRate);

	  void setElMaxRate(float elRate);

	  void selectAperture(carma::antenna::common::DriveControl::Aperture
			      model);

	  void setRaDec(double mjd, double ra, double dec, double distance);

	  void setRefrac(double a, double b );

	  void setSlewRate(unsigned long axes,
			   long az, long el);

	  void setUt1Utc(double mjd, double ut1utc);

	  void setWrap(carma::antenna::common::DriveControl::AzWrapMode
		       azWrapMode, bool elOverTheTop);

	  void stop();

	  void slew(const char* source, unsigned long axisMask,
		    double az, double el);

	  void trackSnow();

	  void trackWind();

	  void stow(carma::antenna::common::DriveControl::Position position,
		    unsigned long seq);

	  void track(const char* source,
		     const carma::antenna::common::DriveControl::RaDecTriplet& positionTriplet,
		     carma::antenna::common::DriveControl::AzWrapMode azWrapMode,
		     bool overTheTop,
		     unsigned long seq);


	  virtual void updateRaDec(const carma::antenna::common::DriveControl::RaDecEpoch& position,
				   carma::antenna::common::DriveControl::AzWrapMode azWrapMode);

	  virtual void updateWeather(float ambientTemp,
				     float barometricPressure,
				     float relativeHumidity,
				     float dewpointTemp,
				     float windSpeed,
				     float windDirection);

	  virtual void setAzMountOffset(double az, unsigned long seq);

	  virtual void setElMountOffset(double el, unsigned long seq);

	  virtual void setOffset(double az, double el, unsigned long seq);

	  virtual void setAzOffset(double az, unsigned long seq);

	  virtual void setElOffset(double el, unsigned long seq);

	  virtual void setAperturePointingConstants(carma::antenna::common::DriveControl::Aperture aperture,
						    float azOffset,
						    float elOffset,
						    float sag);

	  virtual void setTiltmeterZero(float aftForward,
					float leftRight);

	  virtual void setTolerance(float toleranceInArcsecs);

	  virtual void setSafeRange(float azLow,
				    float azHigh,
				    float elLow,
				    float elHigh);

	}; // End namespace sza

    }; // End namespace corba
  }; // End namespace antenna
}; // End namespace sza

#endif
