/**
 * @file
 * BIMA DriveControl implementation.
 *
 * @author Colby Gutierrez-Kraybill
 *
 * $Revision: 1.31 $
 * $Id: DriveControlImpl.h,v 1.31 2012/02/21 21:06:58 abeard Exp $
 */


#ifndef CARMA_ANTENNA_BIMA_DRIVECONTROL_H
#define CARMA_ANTENNA_BIMA_DRIVECONTROL_H

#include "carma/antenna/bima/control/bimaDriveControl.h"
#include "carma/corba/corba.h"
#include "carma/util/IPQwriter.h"

#include <string>

namespace log4cpp {
    class Category;
}

namespace carma
{
  namespace antenna
    {
      namespace bima
        {

          // Forward declarations
          class DriveCommand;
          class SharedMemory;

          /**
           *  Implementation class for DriveControl idl
           */
          class DriveControlImpl
            {
            public:

              /**
               * Constructor.
               */
              DriveControlImpl( std::string antenna,
                                bool emulate );

              /**
               * Destructor.
               */
              ~DriveControlImpl();

              /**
               *  Stow antenna. Position is an enumerated type
               */
              void stow(carma::antenna::common::DriveControl::Position position,
                        CORBA::ULong seq);

              /**
               *  Stop the antenna
               */
              void stop();

              /**
               *  Track sun to melt snow on antenna
               */
              void trackSnow();

              /**
               * Minimize antenna profile to heavy wind.
               */
              void trackWind();

              /**
               *  Set maximum drive rate that can be requested by the drive sytem
               *  software
               */
              void setMaxRate( float azRate, float elRate );

              void setTolerance( float arcsec );

              void selectAperture(carma::antenna::common::DriveControl::Aperture
                                  aperature);

              void setTiltmeterZero(float aftForward, float leftRight);

              void setAperturePointingConstants(
                                                carma::antenna::common::DriveControl::Aperture aperture,
                                                float azOffset, float elOffset, float sag);

              /**
               * Sets the maximum azimuth drive rate that can be requested..
               * @param azRate max rate in azimuth (degrees/sec).
               *                This is an "on the sky" rate, not on the encoder
               * @see setAzMaxRate
               */
              void setAzMaxRate( float azRate);

              /**
               * Sets the maximum elevation drive rate that can be requested..
               * @param elRate max rate in elevation (degrees/sec)
               * @see setMaxRate
               */
              void setElMaxRate( float elRate);


              /**
               *  Set encoder limits
               */
              void setEncoderLimits(long azMin, long azMax,
                                    long elMin, long elMax);

              /**
               *  Set Encoder counts per 1 revolution
               */
              void setEncoderCountsPerTurn(long az, long el);

              /**
               *  Set the antenna location (absolute)
               */
              void setAntLocation(double longitude, double latitude,
                                  double altitude);

            private:

              /**
               *  Set the value of the sequence number to be associated with the
               *  next Drive subsystem command.
               */
              void setNextSequenceNo( CORBA::ULong seq );

            public:

              /**
               *  Go to requested Az/El
               */
              void setAzel(double az, double el, CORBA::ULong seq);

              /**
               *  Go to requested Az
               */
              void setAz(double az, CORBA::ULong seq);

            private:

              void setAz(double az);

            public:

              /**
               *  Go to requested El
               */
              void setEl(double el, CORBA::ULong seq);

            private:

              void setEl(double el);

            public:

              /**
               *  Set the slew rate
               */
              void setSlewRate(unsigned long axes,
                               long azRate, long elRate);

              /**
               *  Set RA/Dec with a source name for tracking.
               */
              void track(
                const char* source,
                const ::carma::antenna::common::DriveControl::RaDecTriplet & positionTriplet,
                carma::antenna::common::DriveControl::AzWrapMode azWrapMode,
                CORBA::Boolean overTheTop,
                CORBA::ULong seq);

              /**
               * Update ra and dec epoch for a source we are already tracking.
               */
              void updateRaDec(
                const ::carma::antenna::common::DriveControl::RaDecEpoch & position,
                carma::antenna::common::DriveControl::AzWrapMode azWrapMode );

              void updateWeather(float ambientTemp, float barometricPressure,
                                 float relativeHumidity, float dewpoint,
                                 float windSpeed, float windDirection);

              void setMountOffset(double az, double el, CORBA::ULong seq);

              void setAzMountOffset(double az, CORBA::ULong seq);

              void setElMountOffset(double el, CORBA::ULong seq);

            private:

              void setAzMountOffset( double az );

              void setElMountOffset( double el );

            public:

              void setOffset(double az, double el, CORBA::ULong seq);

              /**
               * Az astronomical offsets.
               *
               * @param az azimuth offset  on the sky (not encoder) (arcmin)
               * @see setOffset
               */
              void setAzOffset( double az, CORBA::ULong seq );

              /**
               * El astronomical offsets.
               * @param el elevation offset (arcmin)
               * @see setOffset
               */
              void setElOffset(double el, CORBA::ULong seq);

            private:

              void setAzOffset( double az );

              void setElOffset(double el );

            public:

              void setPointingModelCoefs(
                const ::carma::antenna::bima::control::DriveControl::sequence_double& dazCoefs,
                const ::carma::antenna::bima::control::DriveControl::sequence_double& delCoefs);

	    void setSafeRange( float azLow, float azHigh,
			      float elLow, float elHigh);

            private:
              static const std::string className_;
              log4cpp::Category &log_;
              bool emulate_;
	      bool _safeCalled;
              std::string antenna_;
              carma::antenna::bima::SharedMemory *bimaShm_;
              carma::util::IPQwriter<DriveCommand> *drvWriter_;



            }; // End class DriveControlImpl

        } // End namespace bima
    } // End namespace antenna
} // End namespace carma

#endif // CARMA_ANTENNA_BIMA_DRIVECONTROL_H
