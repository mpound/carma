/**
 * @file
 * Class declaration for ovro::DriveControl CORBA implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.22 $
 * $Date: 2012/02/15 21:05:00 $
 * $Id: DriveControlImpl.h,v 1.22 2012/02/15 21:05:00 abeard Exp $
 */
#ifndef CARMA_ANTENNA_OVRO_DRIVECONTROLIMPL_H
#define CARMA_ANTENNA_OVRO_DRIVECONTROLIMPL_H

#include "carma/corba/corba.h"

#include "carma/antenna/common/DriveControl.h"
#include "carma/antenna/common/OpticalTelControl.h"

#include <string>

namespace log4cpp {
    class Category;
} // namespace log4cpp

namespace carma {
namespace antenna {
namespace ovro {

    class DriveEngine;

    /**
     * Drive control implementation.
     */
    class DriveControlImpl { 
    public:

        /**
         * Constructor
         */
        explicit DriveControlImpl(
            DriveEngine & driveEngine,
            const ::std::string & opticalTelControlDOName,
            bool simulate );

        virtual ~DriveControlImpl( ); // Prevent explicit destruction

        void stow( carma::antenna::common::DriveControl::Position position,
                   CORBA::ULong seq );

        void stop( );

        void trackSnow( );

        void trackWind( );

        void setAntLocation( CORBA::Double longitude,
                             CORBA::Double latitude,
                             CORBA::Double altitude );

        void setAzel( CORBA::Double az, CORBA::Double el, CORBA::ULong seq );

        void setAz( CORBA::Double az, CORBA::ULong seq );

        void setEl( CORBA::Double el, CORBA::ULong seq );

        void setMaxRate( CORBA::Float azRate, CORBA::Float elRate );

        void setAzMaxRate( CORBA::Float azRate );

        void setElMaxRate( CORBA::Float elRate );

        void track(
            const char * source,
            const carma::antenna::common::DriveControl::RaDecTriplet & positionTriplet,
            carma::antenna::common::DriveControl::AzWrapMode azWrapMode,
            CORBA::Boolean overTheTop,
            CORBA::ULong seq );

        void updateRaDec(
            const ::carma::antenna::common::DriveControl::RaDecEpoch & position,
            carma::antenna::common::DriveControl::AzWrapMode azWrapMode );

        void updateWeather( CORBA::Float ambientTemp,
                            CORBA::Float barometricPressure,
                            CORBA::Float relativeHumidity,
                            CORBA::Float dewpointTemp,
                            CORBA::Float windSpeed,
                            CORBA::Float windDirection );

        void setMountOffset( CORBA::Double az,
                             CORBA::Double el,
                             CORBA::ULong seq );

        void setAzMountOffset( CORBA::Double az,
                               CORBA::ULong seq );

        void setElMountOffset( CORBA::Double el,
                               CORBA::ULong seq );

        void setOffset( CORBA::Double az,
                        CORBA::Double el,
                        CORBA::ULong seq );

        void setAzOffset( CORBA::Double az,
                          CORBA::ULong seq );

        void setElOffset( CORBA::Double el,
                          CORBA::ULong seq );

        void setAperturePointingConstants(
            carma::antenna::common::DriveControl::Aperture aperture,
            CORBA::Float azOffset,
            CORBA::Float elOffset,
            CORBA::Float sag );

        void setTiltmeterZero( CORBA::Float aftForward,
                               CORBA::Float leftRight );

        void selectAperture(
            carma::antenna::common::DriveControl::Aperture aperture );

        void setTolerance( CORBA::Float tolerance );

        void setMountPointingConstants( CORBA::Double m1,
                                        CORBA::Double m2,
                                        CORBA::Double m3,
                                        CORBA::Double m4,
                                        CORBA::Double m5 );

        void setSafeRange( CORBA::Float azLow, CORBA::Float azHigh,
                           CORBA::Float elLow, CORBA::Float elHigh );

        void setRawDriveVoltages( CORBA::Float azVoltage,
                                  CORBA::Float elVoltage );

        void setRawDriveRates( CORBA::Float azRateInDegPerMin,
                               CORBA::Float elRateInDegPerMin );

        void setEngineeringMode( CORBA::Boolean enable );

        void updateConfigurationData( );

        void freezeTilt( );

        void thawTilt( );

        void toggleBacklashCorrection( CORBA::Boolean enable );
    
    protected:

    private:

        DriveEngine & driveEngine_;

        const bool simulate_;
        const ::std::string opticalTelControlDOName_;
        carma::antenna::common::OpticalTelControl_var opticalTelControl_;

    }; // class DriveControlImpl

}}} // namespace carma::antenna::ovro

#endif
