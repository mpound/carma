/**
 * @file
 * carma::antenna::ovro::DriveEngine class declaration.
 *
 * <dl><dt><b>Author </b></dt><dd>Andrew Beard</dl>
 * Version: $Revision: 1.26 $
 * $Date: 2011/12/21 22:56:37 $
 * $Id: DriveEngine.h,v 1.26 2011/12/21 22:56:37 mpound Exp $
 */
#ifndef CARMA_ANTENNA_OVRO_DRIVEENGINE_H
#define CARMA_ANTENNA_OVRO_DRIVEENGINE_H

#include <memory>
#include <pthread.h>
#include <string>
#include <vector>

namespace carma {

namespace monitor {
    class OvroSubsystem;
} // namespace monitor

namespace services {
    class Angle;
    class Location;
} // namespace services

namespace antenna {
namespace ovro {

    class Drive;
    class Encoder;
    class Tiltmeter;

    struct RaDecEpoch {
        double mjd;         /**< Epoch of ra/dec update */
        double raRadians;   /**< Right Ascension in radians ( 0 to 2 PI ). */
        double decRadians;  /**< Declination in radians ( -PI to PI ). */
    };

    typedef ::std::vector< RaDecEpoch > RaDecTriplet;
    typedef RaDecTriplet::iterator RaDecTripletIter;
    typedef RaDecTriplet::const_iterator RaDecTripletConstIter;
        
    /** 
     * Responsible for storing and maintaining drive parameters,
     * calculating drive rate updates and regularly sending them to 
     * the drive hardware.
     */
    class DriveEngine {
    public:

        explicit DriveEngine( Drive & drive,
                              const Encoder & azEncoder,
                              const Encoder & elEncoder,
                              const Tiltmeter & tiltmeter,
                              carma::monitor::OvroSubsystem & mon );

        /* virtual */ ~DriveEngine( );

        /**
         * Set azimuth and elevation
         * @param azimuth Azimuth as an Angle object.
         * @param elevation Elevation as an Angle object.
         */
        void setAzEl( const carma::services::Angle & angle,
                      const carma::services::Angle & elevation );

        /**
         * Set azimuth
         * @param azimuth Azimuth as an Angle object.
         */
        void setAz( const carma::services::Angle & azimuth );

        /**
         * Set elevation
         * @param elevation Elevation as an Angle object.
         */
        void setEl( const carma::services::Angle & elevation );

        /**
         * Engineering command to set raw drive motor control voltages.
         * This is an engineering command which sets raw motor drive voltages
         * and should be used with the utmost care.
         * @param azRawVoltage Azimuth raw voltage in volts.
         * @param elRawVoltage Elevation raw voltage in volts.
         */
        void setRawDriveVoltages( float azVoltage, float elVoltage );

        /**
         * Engineering command to set raw drive slew rates.
         * Be careful with this.
         * @param azRateInDegPerMin Azimuth slew rate in degrees per minute.
         * @param elRateInDegPerMin Elevation slew rate in degrees per minute.
         */
        void setRawDriveRates( float azRateInDegPerMin, 
                               float elRateInDegPerMin );
         
        /**
         * Set engineering mode. 
         * In engineering mode, control of the drive is disabled completely.
         * @param enable True to enable engineering mode, false to disable.
         */
        void setEngineeringMode( bool enable );
        
        /** 
         * Set azimuth offset
         */
        void setAzOffset( const carma::services::Angle & offset );

        /**
         * Set elevation offset
         */
        void setElOffset( const carma::services::Angle & offset );

        /**
         * Set offset
         */
        void setOffset( const carma::services::Angle & azOffset,
                        const carma::services::Angle & elOffset );

        /**
         * Set source name
         * @param source Source name.
         */
        void setSource( std::string source );

        /**
         * Set azimuth maz rate on the sky.
         * @param azMaxRate Azimuth max sky rate in degrees/second.
         */
        void setAzMaxRate( double azMaxRateInDegPerSec );

        /**
         * Set elevation max rate.
         * @param elMaxRate Elevation max rate in degrees/second.
         */
        void setElMaxRate( double elMaxRateInDegPerSec );

        /**
         * Wrap states.
         */
        enum WrapType {
            NO_TURN,         /*< Add Nothing to requested AZ EL */
            ADD_TURN,        /*< Add 360 degrees to requested AZ EL */
            SUBTRACT_TURN    /*< Subtract 360 degrees from requested AZ EL */
        };

        /**
         * Set new Ra and Dec triplet.
         * @param Vector of Ra/Dec instants.
         * @param wrap Wrap mode.
         */
        void setRaDec( const RaDecTriplet & raDecTriplet,
                       WrapType wrap = NO_TURN );

        /**
         * Update current Ra/Dec position with a new epoch.
         * @param RaDecEpoch Ra/Dec at specified time.
         * @param wrap Wrap mode.
         */
        void updateRaDec( const RaDecEpoch & raDecEpoch,
                          WrapType wrap = NO_TURN ); 

        /**
         * Set weather parameters needed for refraction corrections,
         * snow track and wind track.
         * @param ambientTemp Ambient temperature in Celcius.
         * @param barometricPressure Barometric pressure in millibars.
         * @param relativeHumidity Relative humidity in percent.
         * @param dewpointTemp Dewpoint temperature in Celsius.
         * @param windSpeed Wind speed in miles/hour.
         * @param windDirection Direction wind is coming from, in degrees, with
         *        north at 0, increasing to the east.  This is opposite the
         *        direction the wind is blowing.
         */
        void setWeather( double ambientTemp,
                         double barometricPressure,
                         double relativeHumidity,
                         double dewpointTemp,
                         double windSpeed,
                         double windDirection );

        /**
         * Set antenna location
         * @param location Const reference to a location instance.
         */
        void setLocation( const carma::services::Location & location );

        enum ApertureType {
            OPTICAL,   
            RADIO1MM, 
            RADIO3MM, 
            RADIO1CM,
            APERTURE_COUNT
        };

        /** 
         * Select aperture.
         * Replaces current aperture pointing constants with those of the 
         * selected aperture.
         * @throw IllegalStateException if aperture constants have not been set.
         * @param aperture New desired aperture.
         * @see setAperturePointingConstants
         */
        void selectAperture( ApertureType aperture );
            
        /**
         * Set aperture pointing constants
         * These constants represent on the sky collimation errors for a 
         * particular aperture. Input constants are stored away for future 
         * use (i.e. you only need to call these once for each aperture).
         */
        void setAperturePointingConstants( 
            ApertureType aperture,
            const carma::services::Angle & azOffset,
            const carma::services::Angle & elOffset,
            const carma::services::Angle & sag );

        /** 
         * Set mount pointing constants.
         */
        void setMountPointingConstants(
            const carma::services::Angle & azEncoderZero,
            const carma::services::Angle & elEncoderZero,
            const carma::services::Angle & nonOrthogonalityOfAxes,
            const carma::services::Angle & azimuthVerticalityNorthSouth,
            const carma::services::Angle & azimuthVerticalityEastWest );

        /**
         * Set Tiltmeter Zeros
         */
        void setTiltmeterZeros( 
            const carma::services::Angle & aftForwardZero, 
            const carma::services::Angle & leftRightZero ); 

        /**
         * Set Az Encoder Zero.
         * Also set via setMountPointingConstants.
         */
        void setAzEncoderZero( const carma::services::Angle & azEncoderZero );
                                
        /**
         * Set El Encoder Zero.
         * Also set via setMountPointingConstants.
         */
        void setElEncoderZero( const carma::services::Angle & azEncoderZero );

        /**
         * Set both az and el mount offsets simultaneously.
         */
        void setMountOffsets( const carma::services::Angle & azMountOffset,
                              const carma::services::Angle & elMountOffset );

        /**
         * Set Az Mount Offset.
         */
        void setAzMountOffset( const carma::services::Angle & azMountOffset );

        /**
         * Set El Mount Offset.
         */
        void setElMountOffset( const carma::services::Angle & elMountOffset );

        /**
         * Track a position 60 degrees into the wind in order to minimize 
         * snow buildup on the backing structure (where it is more difficult
         * to remove.
         */
        void snow( );

        /**
         * Stop
         */
        void stop( );

        /**
         * Stow antenna close to zenith.
         */
        void stowZenith( );

        /**
         * Set anti-collision safe ranges.
         * The stow safe position is calculated as the middle of these ranges.
         * This routine must be called prior to calling stowSafe.
         * @param azLow Safe azimuth range low value (range inclusive).
         * @param azHigh Safe azimuth range high value (range inclusive).
         * @param elLow Safe elevation range low value (range inclusive).
         * @param elHigh Safe elevation range high value (range inclusive).
         */
        void setSafeRange( const carma::services::Angle & azLow,
                           const carma::services::Angle & azHigh,
                           const carma::services::Angle & elLow,
                           const carma::services::Angle & elHigh );
        
        /**
         * Stow antenna to a safe position.
         * The safe position is defined to be the middle of the safe range.
         * @pre Must call setSafeRange before calling this.  
         * @throw IllegalStateException if setSafeRange not called prior.
         * @see setSafeRange
         */
        void stowSafe( );

        /**
         * Set sequence number.
         * The drive engine does nothing with this number except store it and
         * write it to the monitor system.
         */
        void setSequenceNumber( unsigned long seqNo );

        /**
         * Set acquisition tolerance.
         */
        void setTolerance( const carma::services::Angle & tolerance );

        /**
         * Update Drive module configuration data from file.
         */
        void updateConfigurationData( );

        /**
         * Freeze tilt ( debug )
         */
        void freezeTilt( );

        /**
         * Thaw tilt ( debug )
         */
        void thawTilt( );

        /**
         * Toggle backlash correction.
         */
        void toggleBacklashCorrection( bool enable );

    protected:

        // Nothing

    private:

        void writeMonitorData( );

        void driveLoopThread( );

        static void driveLoopThreadEntry( DriveEngine & This );

        class Acquisition;
        class DriveState;
        class Limits;
        class Pointing;
        class Servo;
        class SourcePosition;
        class Timing;
        class Tracking;

        struct EngineeringInfo;

        Drive & drive_;
        ::std::auto_ptr< Acquisition > acquisition_;
        ::std::auto_ptr< DriveState > driveState_;
        ::std::auto_ptr< EngineeringInfo > engineering_;
        ::std::auto_ptr< Limits > limits_;
        ::std::auto_ptr< Pointing > pointing_;
        ::std::auto_ptr< Servo > azServo_;
        ::std::auto_ptr< Servo > elServo_;
        ::std::auto_ptr< SourcePosition > sourcePosition_;
        ::std::auto_ptr< Timing > timing_;
        ::std::auto_ptr< Tracking > tracking_;
        ::pthread_t driveLoopThreadId_;
        ::pthread_mutex_t mutex_;

    }; // class DriveEngine

}}} // namespace carma::antenna::ovro
#endif
