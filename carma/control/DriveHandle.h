#ifndef CARMA_CONTROL_DRIVEHANDLE_H
#define CARMA_CONTROL_DRIVEHANDLE_H

/**
 * @file
 *
 * Carma control interface to the common antenna API.
 *
 * @author: Amar Amarnath
 * @author: Marc Pound
 *
 * $CarmaCopyright$
 *
 */
 

#include <string>
#include <memory>
#include <vector>

#include "carma/corba/corba.h"
#include "carma/antenna/common/DriveControl.h"
#include "carma/control/antennaHandleUtils.h"
#include "carma/control/AzWrapMode.h"
#include "carma/control/MjdTriplet.h"
#include "carma/control/RemoteObjHandleT.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/services/Ephemeris.h"
#include "carma/services/Location.h"
#include "carma/services/SourceChecker.h"
#include "carma/services/Source.h"
#include "carma/services/Types.h"

namespace carma {
// forward declarations
    namespace monitor {
        class MonitorSystem;
    } // namespaec monitor

    namespace services {
        class Angle;
        class Pressure;
        class Temperature;
        class Velocity;
    }  // namespace carma::services
// end forward declarations

namespace control {

typedef RemoteObjHandleT< antenna::common::DriveControl >
        DriveControlRemoteObjHandle;
        
//! @brief Manages antenna drive control DO connections
class DriveHandle : public DriveControlRemoteObjHandle {
public:
    /**
     * @brief Constructor
     *
     * @param carmaAntNo const long - CARMA antenna number
     * @param coords antenna location coordinates
     * @param monitorSystem carma monitor system
     *   Allows antenna to get a reference to its own monitor stream.
     * @param antenna const monitor::ControlSubsystemBase::Antenna&
     *              antenna control monitor system, which contains control 
     *              and monitor points set by the control subsystem.
     */
    DriveHandle( unsigned short                   carmaAntNo,
                 const services::Location &       coords,
                 monitor::MonitorSystem &         monitorSystem,
                 monitor::ControlSubsystemBase::Antenna & antenna
               );

    virtual ~DriveHandle( );

    /**
     * For use the first time this drive tracks a given object.
     * This method calls track() three times to send a full triplet 
     * to the drives.  All tracking offsets are set to zero.
     *
     * @param source The source name
     * @param Triplet of MJDs aligned to the antenna update cycle.
     * @param userCatalog - The fully qualified name of the user source 
     * catalog to search for the source, before looking in the system
     * catalog.
     * @param azWrapMode  - The azimuth wrap mode, one of ZERO, ADD, SUB, TIME
     * @see carma::control::AzWrapMode
     * @param time when azWrapMode==TIME, an az wrap will be requested that
     *             allows the source to be tracked for 'time' minutes. Ignored
     *             if azWrapMode != TIME.
     * @param preferredSequenceNo will be used unless it is already the one 
     *                            returned by the monitor system, in which case
     *                            one that is ten greater will be used.
     */
    // note source and userCatalog cannot be string& because 
    // makeHandleMethodFunctorGroup barfs on that.
    void startTrack( const ::std::string source,
                     const carma::control::MjdTriplet mjdTriplet,
                     const ::std::string userCatalog,
                     const carma::control::AzWrapMode azWrapMode,
                     double time,
                     int preferredSequenceNo );
    
    /**
     * Version of startTrack that takes a Source object instead
     * of a source name and catalog.  This avoids multiple file
     * system reads, which can cause command timeouts if the
     * filesystem is under stress.  See bug 906
     */
    // weird, need the "X" here (i.e. a different method name) 
    // or I get an unresolved overloaded
    // function in the makeHandleMethodFunctorGroup call in 
    // SubarrayControlTrack.cc line 208
    void startTrackX( const carma::services::Source source,
                     const carma::control::MjdTriplet mjdTriplet,
                     const carma::control::AzWrapMode azWrapMode,
                     double time,
                     int preferredSequenceNo );

    /**
     * Continue tracking the current source with new equatorial offsets.
     * This method will send a new triplet to the drive DO.
     * @param Triplet of MJDs aligned to the antenna update cycle.
     * @param userCatalog - The fully qualified name of the user source 
     * catalog to search for the source, before looking in the system
     * catalog.
     * @param azWrapMode  - The azimuth wrap mode, one of ZERO, ADD, SUB, TIME
     * @param preferredSequenceNo will be used unless it is already the one 
     *                            returned by the monitor system, in which case
     *                            one that is ten greater will be used.
     * @see carma::control::AzWrapMode
     */
    void trackCurrentSourceWithOffsets( 
        const carma::control::MjdTriplet mjdTriplet, 
        const ::std::string userCatalog,
        carma::control::AzWrapMode azWrapMode, 
        int preferredSequenceNo );

    void trackCurrentSourceWithOffsets( 
        const carma::control::MjdTriplet mjdTriplet, 
        carma::control::AzWrapMode azWrapMode, 
        int preferredSequenceNo );

    /**
     * Method to set antenna's absolute location.
     * @param location The longitude, latitude, and altitude, represented 
     * by a Location object. The input is expected to be in the geocentric
     * coordinate system (ECEF), but gets changed to WGS84 before it is sent
     * to the antennas. Baselines and interferometry geometry are done in ECEF
     * but antennas are pointed in WGS84.
     */
    void setLocation( const services::Location & location );

    /**
     * Method to get antenna's location as longitude, latitude, altitude.
     *
     * @param none
     * @return Location 
     */
    services::Location getLocation() const;

    /**
     * @return a copy of this DriveHandle's ephemeris object.
     */
    services::Ephemeris getEphemeris() const;

    /**
     * Set antenna Az/El offsets for astronomy or engineering
     *
     * @param azArcmin offset (arcmin)
     * @param elArcmin offset (arcmin)
     * @param preferredSequenceNo will be used unless it is already the one 
     *                            returned by the monitor system, in which case
     *                            one that is ten greater will be used.
     */
    void setOffset (double azArcmin, double elArcmin, int preferredSequenceNo );

    /**
     * Set antenna Az offset for astronomy or engineering
     *
     * @param azArcmin offset (arcmin)
     * @param preferredSequenceNo will be used unless it is already the one 
     *                            returned by the monitor system, in which case
     *                            one that is ten greater will be used.
     */
    void setAzOffset (double azArcmin, int preferredSequenceNo) ;

    /**
     * Set antenna El offset for astronomy or engineering
     *
     * @param elArcmin offset (arcmin)
     * @param preferredSequenceNo will be used unless it is already the one 
     *                            returned by the monitor system, in which case
     *                            one that is ten greater will be used.
     */
    void setElOffset (double elArcmin, int preferredSequenceNo) ;

    /**
     * Set antenna azimuth and elevation.
     *
     * @param azDegrees  azimuth
     * @param elDegrees elevation
     * @param preferredSequenceNo will be used unless it is already the one 
     *                            returned by the monitor system, in which case
     *                            one that is ten greater will be used.
     */
    void setAzel( double azDegrees, double elDegrees, int preferredSequenceNo );

    /**
     * Set antenna azimuth.
     *
     * @param azDegrees  azimuth
     * @param preferredSequenceNo will be used unless it is already the one 
     *                            returned by the monitor system, in which case
     *                            one that is ten greater will be used.
     */
    void setAz( double azDegrees, int preferredSequenceNo );

    /**
     * Set antenna elevation.
     *
     * @param elDegrees elevation
     * @param preferredSequenceNo will be used unless it is already the one 
     *                            returned by the monitor system, in which case
     *                            one that is ten greater will be used.
     */
    void setEl( double elDegrees, int preferredSequenceNo );

    /**
     * Set mount offsets.
     * 
     * @param azArcmin Azimuth offset in arcminutes on the sky.
     * @param elArcmin Elevation offset in arcminutes on the sky.
     * @param preferredSequenceNo will be used unless it is already the one 
     *                            returned by the monitor system, in which case
     *                            one that is ten greater will be used.
     */
    void setMountOffset( double azArcmin, 
                         double elArcmin, 
                         int preferredSequenceNo );

    /**
     * Move the antenna one of the stow positions (zenith, service, safe).
     * @param position Stow position to move to.
     * @param preferredSequenceNo will be used unless it is already the one 
     *                            returned by the monitor system, in which case
     *                            one that is ten greater will be used.
     */
    void stow( antenna::common::DriveControl::Position position,
               int preferredSequenceNo );

    /**
     * Stop the antenna at its current azimuth and elevation.
     */
    void stop( );

    /**
    * Set the azimuth and elevation ranges within which
    * this antenna is safe from collisions with other antennas.
    * Note: an invocation DriveControl::stow(SAFE) must throw 
    * an exception if valid safe
    * ranges have not been set with this command beforehand.
    * @param azLow - the low end of the safe azimuth range, in degrees.
    * @param azHigh - the high end of the safe azimuth range, in degrees.
    * @param elLow - the low end of the safe elevation range, in degrees.
    * @param elHigh - the high end of the safe elevation range, in degrees.
    * @param carmaAntNoSeq Sequence of carma antenna numbers. A sequence of
    *                      the single value zero means all subarray antennas.
    * @see antenna::common::DriveControl::setSafeRange()
    */
    void setSafeRange(float azLow, float azHigh, float elLow, float elHigh);
    
    /**
     * Updates tracking information to antenna, 
     *
     * @param mjdAnt Timestamp for antenna tracking 
     */
    void updateTracking( double mjd );

    /**
     * Updates weather information to drive
     *
     * @param mjd, double, time instant for which tracking info is to be 
     *        updated
     * @param airTemp Ambient temperature
     * @param atmPressure Atmospheric pressure
     * @param relHumid Relative humidity, percent (a number between 0 and 100)
     * @param dewPointTemp Dew point temperature
     * @param windspeed   Wind speed
     * @param windDirection Wind direction, 0 degrees = north, 90 = east
     */
    void updateWeather( double                        mjd,
                        const services::Temperature & airTemp,
                        const services::Pressure    & atmPressure,
                        double                        relhumid,
                        const services::Temperature & dewpointTemp, 
                        const services::Velocity    & windSpeed,
                        const services::Angle       & windDirection,
                        bool                          logGoodSend );

    /**
     * Defines equatorial offsets in ra and dec from the input source.
     *
     * Note that these are sky offsets, not coordinate offsets; 
     * to convert an ra coord offset to a sky offset, multiply by cos(dec) 
     * Has no effect if not in equatorial mode. These offsets are set to 
     * zero when a new track command is given. 
     */
    void setEquatOffset(double ra, double dec);
    
    /**
     * Check if the drive mode is in an appropriate state for
     * calling setEquatOffset.  This method will throw an 
     * exception if the drive mode is wrong.
     * This method is necessary to inform observers what they did
     * wrong, since firing off setEquatOffset in a WorkerPool swallows
     * the exception message.  So call this first in a iterative loop,
     * before doing WorkerPool magic.
     *
     * This method is not expensive since the call stays inside
     * the drive handle and never goes to the DO.
     */
    void checkModeForEquatOffset( void );


    /** @return the antenna number associated with this drive. */
    unsigned short getCarmaAntennaNo( ) const;

    /**
     * Returns antenna name as full carma name. For OVRO antenna
     * 3 returns "carma3".
     *
     * @return std::string full, carma name of antenna
     */
    ::std::string getCarmaAntennaName( ) const;

    /**
     * Returns antenna name as full type qualified name. For OVRO antenna
     * 3 returns "ovro3".
     *
     * @return std::string full, type qualified name of antenna
     */
    ::std::string getTypedAntennaName( ) const;

    /**
     * @return the name of the source this antenna is tracking.
     */
    ::std::string getSourceName() ;
    carma::services::Source getSource() ;

    //! @brief Set OVRO mount pointing constants
    void setOvroMountPointingConstants( double m1,
                                        double m2,
                                        double m3,
                                        double m4,
                                        double m5 );

    //! @brief Set BIMA mount pointing constants
    //!
    //! @note The vector argument is passed by value so we can safely
    //!       insure that it is fully and deeply copied across the thread
    //!       barrier when this method is forked out to the worker pool
    void setBimaMountPointingConstants( ::std::vector< double > dazCoefs,
                                        ::std::vector< double > delCoefs );

    //! @brief Set SZA mount pointing constants

    void setSzaMountPointingConstants(
          double azEncoderCountsPerTurn,     double elEncoderCountsPerTurn,
          double azMinEncoderCount,          double azMaxEncoderCount,
          double elMinEncoderCount,          double elMaxEncoderCount,
          double azEncoderZeroDegrees,       double elEncoderZeroDegrees,
          double haTiltDegrees,              double latTiltDegrees, double elTiltDegrees,
          double opticalXCollimationDegrees, double opticalYCollimationDegrees, 
          double opticalFlexureSinDegrees,   double opticalFlexureCosDegrees,
          double radioXCollimationDegrees,   double radioYCollimationDegrees, 
          double radioFlexureSinDegrees,     double radioFlexureCosDegrees);

    //! @brief Set SZA encoder limits

    void setSzaEncoderLimits(double azMinEncoderCount, 
                             double azMaxEncoderCount,
                             double elMinEncoderCount, 
                             double elMaxEncoderCount);

    //! @brief Set SZA encoder zeros

    void setSzaEncoderZeros(double azEncoderZeroDegrees, double elEncoderZeroDegrees);

    //! @brief Set SZA tilts

    void setSzaTilts(double haTiltDegrees, double latTiltDegrees, double elTiltDegrees);

    /**
     * Set the tracking threshold
     * @param threshold tracking threshold in HPBW (halfpower beam widths)
     */
    void trackingThreshold(float hpbw) ;
        
    /**
     * Check to see if the HPBW has changed enough to
     * warrant a threshold update, converting
     * from hpbw to arcsec. We use a 3% change criterion.
     * If it has, then update the threshold to the antennas.
     * This is used when the frequency is updated.
     * @param The current LO freq in GHz (used in conversion to arcsec).
     */
    void conditionallyUpdateTrackTolerance(const double freq) ;

    /**
     * Set the tracking threshold to the antennas.
     * @param tolerance The tracking threshold in HPBW.
     * @param freq The current LO freq in GHz (used in conversion to arcsec).
     */
    void updateTrackTolerance(const float tolerance, const double freq); 
      
     /**
      * Compare next sequence number with one returned by the monitor system.
      * If they are the same then the last drive action is complete.
      * @param monsys monitor system from which to retrieve completion
      * @param monitorDataErrorLimit number of consecutive monitor data
      * invalid limit before thowing an exception
      * @return true if last action is complete
      * @throws if number of consecutive monitor data errors is exceeed
      */
     bool isActionComplete( const monitor::MonitorSystem & monsys,
                            int                            monDataErrorLimit );
    
    typedef enum {
        SOURCE_MODE_RA_DEC,
        SOURCE_MODE_AZ_EL,
        SOURCE_MODE_IDLE,
        SOURCE_MODE_INVALID
    } SourceMode;
    

    /**
     * Method to get source mode of antenna - it can point at nothing,
     * or at a source specified by RA/DEC, or a point in the sky specified
     * by Az/El.
     *
     * @param none
     * @return const enum SourceMode mode for antenna
     * @see SourceMode
     */
    SourceMode getSourceMode( ) const;

    void test( double aSeconds,
               double bSeconds,
               long   whichTest );
    
private:

    /**
     * "Track" a fixed source, e.g. the transmitter.
     * Knowledge of what are fixed sources is kept in the Ephemeris
     * class. Latitude and longitude of fixed sources are given 
     * in Observatory.cat; Ephemeris computes their azimuth and 
     * elevations from the current Location.
     *
     * @param source The source name
     */
    void trackFixed( const std::string & source, int preferredSequenceNo );

    /**
     * Method to tell antenna to track specified source.
     *
     * @param source The source name
     * @param mjd MJD as double
     * @param discontinuity True if this call represents a source change
     * @param resetTrackOffsets - If true, reset all tracking offsets
     * by calling setEquatOffset(0,0)
     * @param userCatalog - The fully qualified name of the user source 
     * catalog to search for the source, before looking in the system
     * catalog.
     * The Ephemeris member variable will 
     * throw services::SourceNotFoundException,
     * if the source cannot be found in either user or standard catalog and 
     * is not a planet.
     * @param azWrapMode  - The azimuth wrap mode, one of ZERO, ADD, SUB, TIME
     * @param preferredSequenceNo will be used unless it is already the one 
     *                            returned by the monitor system, in which case
     *                            one that is ten greater will be used.
     * @see carma::control::AzWrapMode
     */
    void track( const ::std::string & source,
                const carma::control::MjdTriplet & triplet,
                bool                  resetTrackOffsets,
                const std::string &   userCatalog,
                carma::control::AzWrapMode azWrapMode,
                int preferredSequenceNo );

    /**
     * Version of track that takes a Source object instead
     * of a source name and catalog.  This avoids multiple file
     * system reads, which can cause command timeouts if the
     * filesystem is under stress.  See bug 906
     */
    void track( const carma::services::Source & source,
                const carma::control::MjdTriplet & triplet,
                bool                  resetTrackOffsets,
                carma::control::AzWrapMode azWrapMode,
                int preferredSequenceNo );

    /**
     * Update antenna RA/Dec
     * 
     * @param sourceName,  name of source
     * @param mjd,  MJD for the requested RA/Dec
     * @param ra,  topocentric RA, in radians
     * @param dec, topocentric Dec, in radians
     * @param azWrapMode  - The azimuth wrap mode, one of ZERO, ADD, SUB, TIME
     * @see carma::control::AzWrapMode
     */
    void updateRaDec( const ::std::string & sourceName,
                      double mjd, double ra, double dec,
                      carma::control::AzWrapMode azWrapMode );

    /** Set the source tracking mode to SOURCE_MODE_IDLE */
    void setSourceModeToIdle( );
    /** Set the source tracking mode to SOURCE_MODE_RA_DEC */
    void setSourceModeToRaDec( );
    /** Set the source tracking mode to SOURCE_MODE_AZ_EL */
    void setSourceModeToAzEl( );

    /**
     * Updates UT1-UTC on the antenna drive DO
     *
     * @param mjd, double, time for which the UT1-UTC correction is sent
     */
    void updateAntennaSourceRaDec (double mjd) ;

    void setNextSequenceNo( int preferredSequenceNo );

    int nextSequenceNo_; 
    int consecutiveErrors_;
    int errLimit_;
    
    const unsigned short carmaAntNo_;
    const AntennaType antType_;
    
    /**
     * A reference to the Antenna monitor subsystem for this
     * antenna (drive), Use this reference to set monitor point values
     * associated with this drive.
     */
    monitor::MonitorSubsystem & antennaSubsystem_;
    /** 
     * Also need a reference to the parent monitorSystem_
     * for querying the current Az in computeOptimumWrapValue
     * which can only be obtained from AntennaCommon.drive()
     *
     * Alternative is to put an accessor into RemoveObjHandle<T>.
     */
    monitor::MonitorSystem & monitorSystem_;
    
    monitor::ControlSubsystemBase::Antenna & antenna_;

    SourceMode sourceMode_;
    
    double trackToleranceFreq_; // LO freq used to compute track tol asec
    float trackTolerance_;      // In half power beam widths
    
    /**
     * ephemeris containing the position of this antenna, used to
     * calculate apparent RA/DEC, etc.
     */
    carma::services::Ephemeris localEphemeris_;

    /**
     * Source checker object for verifying azimuth wrap.
     */
    carma::services::SourceChecker checker_;

    double raTrackingOffset_;
    double decTrackingOffset_;
    double raPhaseOffset_;
    double decPhaseOffset_;

    /**
     * Used to throw an exception if stow(SAFE) is called before
     * these safe azimuth and elevation ranges are set via
     * setSafeRange(...).  This saves us making the call across
     * the wire to remote drive object and catching its exception.
     */
    bool setSafeRangeCompleted_;

    /**
     * The beginning of the interval for which we should compute 
     * the optimum azimuth wrap.  That is we want to find the
     * azimuth wrap that the antenna can stay on from
     * optimumWrapTime_  to  optimumWrapTime_ + timeToTrack_.
     */
    double optimumWrapTime_;

    /** requested minutes to track without changing azimuth wrap */
    double timeToTrack_;

    /** 
     * The last wrap mode recieved from control system. This is
     * used in updateTracking() so that we always stay
     * on the azwrap mode computed during the initial
     * invocation of track()
     */
    carma::control::AzWrapMode lastControlWrapMode_;

    /** 
     * The last wrap mode sent to the drive.  
     * If there is a problem reading back the current
     * drive azimuth, this wrap mode is maintained (sent again
     * to the drives).
     */
    carma::antenna::common::DriveControl::AzWrapMode lastAntWrapMode_;


    /**
     * Send a track triplet to the drive DO.
     * @param Triplet of MJDs aligned to the antenna update cycle.
     * @param resetTrackOffsets - If true, reset all tracking offsets.
     * @param userCatalog - The fully qualified name of the user source 
     * catalog to search for the source, before looking in the system
     * catalog.
     * @param azWrapMode  - The azimuth wrap mode, one of ZERO, ADD, SUB, TIME
     * @param preferredSequenceNo will be used unless it is already the one 
     *                            returned by the monitor system, in which case
     *                            one that is ten greater will be used.
     * @see carma::control::AzWrapMode
     */
    void trackTriplet( const ::std::string & source,
                       const carma::control::MjdTriplet & mjdTriplet,
                       bool resetTrackOffsets,
                       const std::string & userCatalog,
                       carma::control::AzWrapMode azWrapMode, 
                       int preferredSequenceNo );

    /** 
     * version to avoid filesystem reads
     */
    void trackTriplet( const carma::services::Source & source,
                       const carma::control::MjdTriplet & mjdTriplet,
                       bool resetTrackOffsets,
                       carma::control::AzWrapMode azWrapMode, 
                       int preferredSequenceNo );


    /**
     * update the azimuth and elevation limits for this
     * antenna from the monitor system.
     * If read from monsys fails, fallback to default limits.
     */
     void updateWrapLimits();

    /** Throw Exception if we will go past hw limits.
      * Warn if we will go past sw limits.
      * @param azDegrees - requested azimuth in degrees
      * @param elDegrees - requested elevation in degrees
      * @param warnOnly  - log a warning message if outside
      * hardware limits, otherwise throw an exception.
      * Default is false (i.e., throw the exception)
      */
     void checkAgainstLimits(const double azDegrees, const double elDegrees,
                             const bool warnOnly );

    /**
     * set some default wrap limits for this antenna type
     * to be used if the monitor data are unavailable.
     */
     void defaultWrapLimits();

     /**
      * Given az/el limits and requested time to track, 
      * compute the optimum wrap directive to send to the drive.
      * @see services::SourceChecker::computeOptimumWrapValue
      */
     antenna::common::DriveControl::AzWrapMode computeOptimumWrapValue();

     /** Convert control azimuth wrap mode enumeration to a string */
     const ::std::string 
     azWrapModeToString( carma::control::AzWrapMode mode ) const;

     /** Convert DriveControl azimuth wrap mode enumeration to a string */
     const ::std::string 
     azWrapModeToString( carma::antenna::common::DriveControl::AzWrapMode mode ) const;

     /** 
      * Translate control::AzWrapMode to DriveControl::AzWrapMode
      * correcting or computing as necessary
      */
     carma::antenna::common::DriveControl::AzWrapMode
         controlWrapToDriveWrap( control::AzWrapMode mode );


     /**
      * Translate azimuth wrap enum between namespaces.
      */
     // nb. could have overloaded a general toDriveWrap() method.
     antenna::common::DriveControl::AzWrapMode
         servicesWrapToDriveWrap( services::AzWrapType wrap ) const;

     /**
      * Straight translation of DriveControl::AzWrapMOde to 
      * contol::AzWrapMode -- no computation.
      */
     control::AzWrapMode
         driveWrapToControlWrap( antenna::common::DriveControl::AzWrapMode ) const;

     /**
      * Translate antenna type enum between namespaces.
      */
     services::AntennaType
         controlAntTypeToServicesAntType( control::AntennaType antType ) const;

};  // class DriveHandle


}  // End namespace carma::control
}  // End namespace carma


#endif
