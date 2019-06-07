#ifndef SZA_ANTENNA_CORBA_DRIVEPROXY_H
#define SZA_ANTENNA_CORBA_DRIVEPROXY_H

/**
 * @file DriveProxy.h
 *
 * Tagged: Thu Nov 13 16:53:36 UTC 2003
 *
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/corba/Proxy.h"

#include "carma/antenna/sza/control/szaDriveControl.h"

#include "carma/szautil/Angle.h"
#include "carma/szautil/Axis.h"
#include "carma/szautil/Length.h"
#include "carma/szautil/OffsetMsg.h"
#include "carma/szautil/PointingMode.h"
#include "carma/szautil/Rx.h"


// Must undef macro SystemException in carma::util::BaseException.h,
// since this causes references to CORBA::SystemException below to be
// misinterpreted

namespace sza {
  namespace antenna {
    namespace corba {

      /**
       * Incomplete specification of AntennaDrive lets us use it without
       * defining it yet.
       */
      class PointingModelProxy;

      /**
       * Create a DriveProxy class in namespace carma.  This will be
       * served up as the CORBA Drive DO, whose methods send messages
       * to the AntennaDrive message queue.
       */
      class DriveProxy : public Proxy {

      public:

	/**
	 * Constructor with a pointer to the parent AntennaDrive.
	 *
	 * @throws Exception
	 */
	DriveProxy(sza::antenna::control::AntennaMaster* parent);

	/**
	 * Destructor.
	 */
	~DriveProxy();

	void setAntLocation(double longitude, double latitude,
			    double altitude);

	void setAzel(sza::util::Axis::Type axes, double az,
		     double el, CORBA::ULong seq);

	void setAzel(double az, double el, CORBA::ULong seq);

	void setAz(double az, CORBA::ULong seq);

	void setEl(double el, CORBA::ULong seq);

	void setMountOffset(double az, double el, CORBA::ULong seq);

	void setEqnEqx(double mjd, double eqneqx);

	void setMaxRate(float azRate, float elRate);

	void setAzMaxRate(float azRate);

	void setElMaxRate(float elRate);

	void selectAperture(carma::antenna::common::DriveControl::Aperture
			    model);

	void setRefrac(double a, double b );

	void setSlewRate(unsigned long axes,
			 long az, long el);

	void setUt1Utc(double mjd, double ut1utc);

	void stop();

	void slew(const char* source, unsigned long axisMask,
		  double az, double el);

	void trackSnow();

	void trackWind();

	void stow(carma::antenna::common::DriveControl::Position position,
		  CORBA::ULong seq=0);

	void extendTrack(const carma::antenna::common::DriveControl::RaDecEpoch& position,
			 bool newSource=false, unsigned seq=0, char* source=0,
			 carma::antenna::common::DriveControl::AzWrapMode azWrapMode=carma::antenna::common::DriveControl::ZERO);

	void track(const char* source,
		   const carma::antenna::common::DriveControl::RaDecTriplet& positionTriplet,
		   carma::antenna::common::DriveControl::AzWrapMode azWrapMode,
		   bool overTheTop,
		   CORBA::ULong seq);


	void updateRaDec(const carma::antenna::common::DriveControl::RaDecEpoch& position,
			 carma::antenna::common::DriveControl::AzWrapMode azWrapMode);

	void updateWeather(float ambientTemp,
			   float barometricPressure,
			   float relativeHumidity,
			   float dewpointTemp,
			   float windSpeed,
			   float windDirection);

	void setAzMountOffset(double az, CORBA::ULong seq);

	void setElMountOffset(double el, CORBA::ULong seq);

	void setOffset(sza::util::OffsetMsg::Axis axes,
		       double az,
		       double el,
		       CORBA::ULong seq=0);

	void setOffset(double az, double el, CORBA::ULong seq);

	void setAzOffset(double az, CORBA::ULong seq);

	void setElOffset(double el, CORBA::ULong seq);

	void setAperturePointingConstants(carma::antenna::common::
					  DriveControl::Aperture aperture,
					  float azOffset,
					  float elOffset,
					  float sag);

	void setTilts(double azTilt1, double azTilt2,
		      double elTilt1, double elTilt2);

	void setTiltmeterZero(float aftForward,
			      float leftRight);

	void setTolerance(float toleranceInArcsecs);

	void setSafeRange(float azLow,
			  float azHigh,
			  float elLow,
			  float elHigh);

	//-----------------------------------------------------------------------
	// CORBA methods to set the pointing model terms
	//-----------------------------------------------------------------------

	void setEncoderCountsPerTurn(unsigned long azCountsPerTurn, unsigned long elCountsPerTurn);

	void setEncoderLimits(unsigned long azMinCount, unsigned long azMaxCount,
			      unsigned long elMinCount, unsigned long elMaxCount);

	void setEncoderZeros(double azEncZeroDeg, double elEncZeroDeg);

	void setTilts(double haTiltDeg, double latTiltDeg, double elTiltDeg);

	void setCollimation(carma::antenna::common::DriveControl::Aperture aperture,
			    double xCollimationDeg, double yCollimationDeg);

	void setFlexure(carma::antenna::common::DriveControl::Aperture aperture,
			double sinCoeffDeg, double cosCoeffDeg);

	void setFlexure(sza::util::PointingMode::Type model, sza::util::Angle& sFlex, sza::util::Angle& cFlex);

	void setMountPointingConstants(CORBA::ULong azEncoderCountsPerTurn, CORBA::ULong elEncoderCountsPerTurn,
				       CORBA::ULong azMinEncoderCount,      CORBA::ULong azMaxEncoderCount,
				       CORBA::ULong elMinEncoderCount,      CORBA::ULong elMaxEncoderCount,
				       double        azEncoderZeroDegrees,   double        elEncoderZeroDegrees,
				       double        haTiltDegrees,  double latTiltDegrees, double elTiltDegrees,
				       double        opticalXCollimationDegrees, double   opticalYCollimationDegrees,
				       double        opticalFlexureSinDegrees,   double   opticalFlexureCosDegrees,
				       double        radioXCollimationDegrees,   double   radioYCollimationDegrees,
				       double        radioFlexureSinDegrees,     double   radioFlexureCosDegrees);

	void setWrapMode(carma::antenna::common::DriveControl::AzWrapMode azWrapMode);

	//-----------------------------------------------------------------------
	// Local methods
	//-----------------------------------------------------------------------

	void setSite(sza::util::Angle& lng, sza::util::Angle& lat, sza::util::Length& alt);
	void setLocation(sza::util::Length& up, sza::util::Length& east, sza::util::Length& north);

	void setEncoderCountsPerTurn(unsigned az, unsigned el);
	void setEncoderLimits(unsigned azMin, unsigned azMax, unsigned elMin, unsigned elMax);
	void setEncoderZeros(sza::util::Angle& azZero, sza::util::Angle& elZero);

	void setTilts(sza::util::Angle& haTilt, sza::util::Angle& latTilt, sza::util::Angle& elTilt);
	void setCollimation(sza::util::PointingMode::Type model, sza::util::Angle& x, sza::util::Angle& y);

	void setFlexure();

	static sza::util::Rx::Id apertureToRxId(carma::antenna::common::DriveControl::Aperture model);
	static carma::antenna::common::DriveControl::Aperture rxIdToAperture(sza::util::Rx::Id rxId);

	/**
	 * The object that will be served as the CORBA DO to handle
	 * pointing model commands.
	 */
	PointingModelProxy* pointingModel_;

      }; // End class DriveProxy

    }; // End namespace corba
  }; // End namespace antenna
}; // End namespace sza

#endif


