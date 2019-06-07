/**
 * @file
 * carma::antenna::ovro::DriveEngine class declaration.
 * Portions of this were ported from original VAX drive code written by
 * Mark J. Deazley circa 1992.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.101 $
 * $Date: 2014/09/08 17:19:37 $
 * $Id: DriveEngine.cc,v 1.101 2014/09/08 17:19:37 scott Exp $
 */

#include "carma/antenna/ovro/control/DriveEngine.h"

#include "carma/antenna/common/Tiltmeter.h"
#include "carma/antenna/ovro/canbus/Drive.h"
#include "carma/antenna/ovro/canbus/Encoder.h"
#include "carma/services/Atmosphere.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/services/AzElPair.h"
#include "carma/services/Ephemeris.h"
#include "carma/services/Physical.h"
#include "carma/services/trigFunctions.h"
#include "carma/services/Vector.h"
#include "carma/util/AutoPthreadQuitAndJoinGroup.h"
#include "carma/util/BaseException.h"
#include "carma/util/compileTimeCheck.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/FrameAlignedTimer.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/IllegalStateException.h"
#include "carma/util/programLogging.h"
#include "carma/util/QuadraticInterpolatorPositiveAngle.h" 
#include "carma/util/QuadraticInterpolatorSignedAngle.h" 
#include "carma/util/ScopedLock.h"
#include "carma/util/StartPthread.h"
#include "carma/util/ThreadQuit.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

#include <deque>
#include <iomanip>
#include <iostream>

using namespace carma;
using namespace carma::antenna::ovro;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::services::constants;
using namespace carma::util;
using namespace std;

namespace {

    const carma::util::Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE3; 
    const carma::util::Trace::TraceLevel TRACE_THREADING = Trace::TRACE3; 
    const carma::util::Trace::TraceLevel TRACE_RATE_CALCS = Trace::TRACE3;
    const carma::util::Trace::TraceLevel TRACE_POINTING = Trace::TRACE3;
    const carma::util::Trace::TraceLevel TRACE_TIMING = Trace::TRACE3;
    const carma::util::Trace::TraceLevel TRACE_ACQUISITION = Trace::TRACE3;
    
    // Physical conversion constants
    const double ARCMIN_PER_DEG = 60.0;
    const double ARCSEC_PER_MIN = 60.0;
    const double RAD_PER_DEG = M_PI / 180.0;
    const double DEG_PER_RAD = 1.0 / RAD_PER_DEG;
    const double ARCSEC_PER_RAD = ARCMIN_PER_DEG * ARCSEC_PER_MIN * DEG_PER_RAD;
    const double RAD_PER_ARCSEC = 1.0 / ARCSEC_PER_RAD;
    const double FRAMES_PER_SEC = 2.0;
    const double C_IN_M_PER_S = constants::Physical::C;
    const double FREQ_1MM_IN_HZ = C_IN_M_PER_S / ( 1.0e-3 );
    const double FREQ_3MM_IN_HZ = C_IN_M_PER_S / ( 3.0e-3 );
    const double FREQ_1CM_IN_HZ = C_IN_M_PER_S / ( 1.0e-2 );
    const double FREQ_550NM_IN_HZ = C_IN_M_PER_S / ( 550.0e-9 );

    // Application specific constants
    const long TIMER_OFFSET_NANOS = 25 * 1000 * 1000; 
    const unsigned long MIN_UPDATES_FOR_ACQUIRE = 2;
    const double UPDATE_PERIOD_IN_SEC = 0.5;   // Update per second
    const long UPDATE_PERIOD_IN_FRAMES = 
        static_cast< long >( UPDATE_PERIOD_IN_SEC * FRAMES_PER_SEC );

    // Max rates
    const double MAX_AZ_RATE_IN_ARCSEC_PER_SEC = 3600.0;
    const double MAX_EL_RATE_IN_ARCSEC_PER_SEC = 1800.0;
    const double MAX_AZ_RATE_IN_RAD_PER_SEC = 
        MAX_AZ_RATE_IN_ARCSEC_PER_SEC / ARCSEC_PER_RAD;
    const double MAX_EL_RATE_IN_RAD_PER_SEC =  
        MAX_EL_RATE_IN_ARCSEC_PER_SEC / ARCSEC_PER_RAD;
    const double MAX_AZ_RATE_IN_DEG_PER_MIN = 
        MAX_AZ_RATE_IN_ARCSEC_PER_SEC / 60.0; 
    const double MAX_EL_RATE_IN_DEG_PER_MIN =
        MAX_EL_RATE_IN_ARCSEC_PER_SEC / 60.0;

    // Software limits
    const double SW_MAX_AZ_DEGREES = 353.0;
    const double SW_MIN_AZ_DEGREES = -88.0; 
    const double SW_MAX_EL_DEGREES =  87.5;
    const double SW_MIN_EL_DEGREES =  4.0;

    // Hardware limit values used by this code
    // Values must be outside of the physical hardware limits
    const double HW_MAX_AZ_DEGREES = 358.0;
    const double HW_MIN_AZ_DEGREES = -98.0; 
    const double HW_MAX_EL_DEGREES =  92.0;
    const double HW_MIN_EL_DEGREES =  1.0;

    const double AZ_OVERLAP_TEST_ANGLE_DEGREES = 3.5;

    // Default stow zenith
    const double STOW_AZ_DEGREES = M_PI;
    const double STOW_EL_DEGREES = SW_MAX_EL_DEGREES - 0.5;
    
    // Servo loop constants.
    const double GAMMA = 1.0;
    const double BETA = 0.12;
    const double BACKLASH = 1.0 / 2047.0;
    const double CLIP = 5.0;
    
    // Running error count for rms calcs.
    const deque<double>::size_type MAX_RMS_ERRORS = 20;

    void compileTimeCheckApEnums( ) 
    {
        typedef AntennaCommon::SelectedApertMonitorPointEnum SelectedApertMPE;

        compileTimeCheck< static_cast<int>( SelectedApertMPE::OPTICAL ) == 
                          static_cast<int>( DriveEngine::OPTICAL ) > (); 
        compileTimeCheck< static_cast<int>( SelectedApertMPE::RADIO1MM ) ==
                          static_cast<int>( DriveEngine::RADIO1MM ) >();
        compileTimeCheck< static_cast<int>( SelectedApertMPE::RADIO3MM ) ==
                          static_cast<int>( DriveEngine::RADIO3MM ) >();
        compileTimeCheck< static_cast<int>( SelectedApertMPE::RADIO1CM ) ==
                          static_cast<int>( DriveEngine::RADIO1CM ) >();
    }

    string 
    apertureTypeAsString( const DriveEngine::ApertureType aperture )
    {
        switch ( aperture ) {
            case DriveEngine::OPTICAL: return "OPTICAL";
            case DriveEngine::RADIO1MM: return "RADIO1MM";
            case DriveEngine::RADIO3MM: return "RADIO3MM";
            case DriveEngine::RADIO1CM: return "RADIO1CM";
            default: return "<error>";
        }
        return "<error>";
    }

    typedef AntennaCommon::StateMonitorPointEnum CommonDriveState;
    typedef OvroSubsystem::DriveStateMonitorPointEnum OvroDriveState;

    CommonDriveState::STATE
    ovroDriveStateToCommonDriveState( 
        const OvroDriveState::DRIVESTATE ovroDriveState )
    {
        switch ( ovroDriveState ) {
            case OvroDriveState::TRACK: return CommonDriveState::TRACK;
            case OvroDriveState::CLOSE: return CommonDriveState::CLOSE;
            case OvroDriveState::SLEW: return CommonDriveState::SLEW;
            case OvroDriveState::STOW: return CommonDriveState::STOW;
            case OvroDriveState::SNOW: return CommonDriveState::SNOW;
            case OvroDriveState::STOP: return CommonDriveState::STOP;
            case OvroDriveState::DISABLE: return CommonDriveState::DISABLE;
            case OvroDriveState::HWLIMIT: return CommonDriveState::HWLIMIT;
            case OvroDriveState::SWLIMIT: return CommonDriveState::SWLIMIT;
            case OvroDriveState::ERROR: return CommonDriveState::ERROR;
            case OvroDriveState::LOCAL: return CommonDriveState::LOCAL;
            case OvroDriveState::TEST: return CommonDriveState::TEST;
            case OvroDriveState::FATAL: return CommonDriveState::FATAL;
            default: 
                throw CARMA_EXCEPTION( IllegalArgumentException, 
                                       "Invalid drive state." );
        }
    }

    typedef AntennaCommon::ModeMonitorPointEnum CommonDriveMode;
    typedef OvroSubsystem::DriveModeMonitorPointEnum OvroDriveMode;

    CommonDriveMode::MODE
    ovroDriveModeToCommonDriveMode(
        const OvroDriveMode::DRIVEMODE ovroDriveMode )
    {
        switch ( ovroDriveMode ) {
            case OvroDriveMode::AZEL:  return CommonDriveMode::AZEL;
            case OvroDriveMode::EQUAT: return CommonDriveMode::EQUAT;
            case OvroDriveMode::STOW:  return CommonDriveMode::STOW;
            case OvroDriveMode::SNOW:  return CommonDriveMode::SNOW;
            case OvroDriveMode::STOP:  return CommonDriveMode::STOP;
            case OvroDriveMode::TEST:  return CommonDriveMode::TEST;
            default:
                throw CARMA_EXCEPTION( IllegalArgumentException, 
                                       "Invalid drive mode." );
        }
    }

    typedef AntennaCommon::WrapLogicMonitorPointEnum CommonWrapLogic;
    typedef OvroSubsystem::WrapLogicMonitorPointEnum OvroWrapLogic;

    CommonWrapLogic::WRAPLOGIC
    wrapToCommonMonitorWrap( const DriveEngine::WrapType wrap ) 
    {
        switch ( wrap ) {
            case DriveEngine::NO_TURN:       return CommonWrapLogic::ZERO;
            case DriveEngine::ADD_TURN:      return CommonWrapLogic::ADD;
            case DriveEngine::SUBTRACT_TURN: return CommonWrapLogic::SUB;
            default:
                throw CARMA_EXCEPTION( IllegalArgumentException, 
                                       "Invalid wrap logic." );
        }
    }
    
    OvroWrapLogic::WRAPLOGIC
    wrapToOvroMonitorWrap( const DriveEngine::WrapType wrap ) 
    {
        switch ( wrap ) {
            case DriveEngine::NO_TURN: return OvroWrapLogic::ZERO;
            case DriveEngine::ADD_TURN: return OvroWrapLogic::ADD;
            case DriveEngine::SUBTRACT_TURN: return OvroWrapLogic::SUB;
            default:
                throw CARMA_EXCEPTION( IllegalArgumentException, 
                                       "Invalid wrap logic." );
        }
    }

    double
    millisecondsSinceCurrentFrame( double mjd )
    {
        const frameType frame = Time::computeCurrentFrame( );
        const double ms = 
            ( mjd - Time::MJD( frame ) ) * Time::MILLISECONDS_PER_DAY;
        return ms;
    }

    double
    cosEl( const Angle & el ) 
    {
        const double maxElRads = 0.1 * RAD_PER_DEG;

        if ( fabs( M_PI_2 - el.radians( ) ) >= maxElRads ) {
            return cos( el );
        } else {
            return ::cos( M_PI_2 - maxElRads );
        }
    }

    typedef carma::util::ScopedLock< pthread_mutex_t > ScopeLock;

} // namespace < unnamed >

/**
 * Pointing class. 
 * Reads encoders and applies overlap and pointing model 
 * corrections in order to arrive at at an actual position.
 */
class DriveEngine::Pointing {
public:

    explicit Pointing( const Drive & drive,
                       const Encoder & azEncoder, 
                       const Encoder & elEncoder,
                       const Tiltmeter & tiltmeter,
                       OvroSubsystem & mon );

    ~Pointing( );

    /**
     * Atomically update pointing position from the encoders and drive module. 
     * If either az, el or the az overlap state is invalid false is returned. 
     * In this case, the pointing position is considered invalid and any
     * attempt to retrieve the actual position will throw an exception. 
     * @param applyFineCorrections Enabled or disable fine pointing corrections.
     * @return true if successful, false if updated position is invalid.
     * @post If returning false, positions are invalid until the next
     *       successful update cycle. If true, they are valid until next update.
     * @see getActualPosition
     */
    bool updatePosition( bool applyFineCorrections );

    /**
     * Return overlap and pointing corrected positions.
     * @pre updatePosition() must be called for this update cycle AND must
     *      have returned successfully.
     */
    AzElPair getPosition( ) const;
    
    /**
     * Return raw encoder readings, with no corrections
     * @pre updatePosition() must be called for this update cycle AND must
     *      have returned successfully.
     */
    AzElPair getRawPosition() const;

    /** 
     * Check for az overlap switch failure.
     * @pre updatePosition() must be called for this update cycle AND must
     *      have returned successfully.
     */
    bool checkForAzOverlapSwitchFail() const;
    
    bool getControllerOvertemp() const;

    /** 
     * Write pointing related monitor data to monitor system.
     */
    void writeMonitorData( ) const;

    void selectAperture( DriveEngine::ApertureType aperture );

    void setAperturePointingConstants(
        DriveEngine::ApertureType aperture,
        const Angle & azOffset,
        const Angle & elOffset,
        const Angle & sag );

    void setTiltmeterZeros( 
        const Angle & aftForwardZero, 
        const Angle & leftRightZero );
    
    void setAzMountOffset( const Angle & azMountOffset );
    void setElMountOffset( const Angle & elMountOffset ); 

    void setAzEncoderOffset( const Angle & azEncoderOffset );
    void setElEncoderOffset( const Angle & elEncoderOffset );
    void setMountPointingConstants( 
        const Angle & nonOrthogonality,
        const Angle & axisAlignmentNorthSouth,
        const Angle & axisAlignmentEastWest );

    void updateTilts( bool update );

private:
    
    struct ApertureConstants {
        Angle azOffset;
        Angle elOffset;
        Angle sag;
    };
   
    struct MountConstants {
        Angle aftForwardTiltZero;
        Angle leftRightTiltZero;
        Angle azMountOffset;
        Angle elMountOffset;
        Angle azEncoderZero;                  // M1
        Angle elEncoderZero;                  // M2 
        Angle azElNonOrthogonality;           // M3
        Angle axisAlignmentNorthSouth;        // M4
        Angle axisAlignmentEastWest;          // M5
    };

    struct Tilts {
        Angle correctedAftForwardTilt;
        Angle correctedLeftRightTilt;
        Angle direction;
        Angle magnitude;
    };
    
    MountConstants getMountConstants( ) const;

    ApertureConstants getApertureConstants( ApertureType  ap ) const;

    typedef map< DriveEngine::ApertureType, 
                 ApertureConstants > ApertureConstantsMap;

    void applyPointingCorrections( AzElPair & coord,
                                   const bool applyFineCorrections ) const;

    void applyEncoderOffsetCorrections( AzElPair & coord ) const;

    void applyApertureAndMountCorrections( AzElPair & coord ) const;

    void updateLastCoefficientChange( );

    void updateAndCorrectTiltReadings( ); 

    void updateTiltMagnitudeAndDirection( const AzElPair & azel );
    bool allModulesOnlineAtLeastOnce_;
    bool updateTilts_;
    double lastCoefficientChange_;
    unsigned int timingErrorCount_;
    unsigned int offlineErrorCount_;
    const Angle azOverlapTestAngle_; // Where we check that az overlap is on.
    Drive::TimestampedAzOverlapState azOverlapState_;
    bool controllerOvertemp_;
    Encoder::TimestampedPosition rawAzPosition_;
    Encoder::TimestampedPosition rawElPosition_;
    AzElPair rawPosition_;
    AzElPair correctedPosition_;
    
    MountConstants mountConstants_;
    ApertureType currentAperture_;
    ApertureConstantsMap apertureConstants_;
    Tilts tilts_;
    const Drive & drive_;
    const Encoder & azEncoder_;
    const Encoder & elEncoder_;
    const Tiltmeter & tiltmeter_;
    OvroSubsystem::Drive & mon_;
    AntennaCommon::Drive & commonDrive_;
    OvroSubsystem& subsystem_;

}; // Class DriveEngine::Pointing

DriveEngine::Pointing::Pointing( const Drive & drive,
                                 const Encoder & azEncoder, 
                                 const Encoder & elEncoder,
                                 const Tiltmeter & tiltmeter,
                                 OvroSubsystem & mon ) :
    allModulesOnlineAtLeastOnce_( false ),
    updateTilts_( true ),
    lastCoefficientChange_( Time::MJD( ) ),
    timingErrorCount_( 0 ),
    offlineErrorCount_( 0 ),
    azOverlapTestAngle_( AZ_OVERLAP_TEST_ANGLE_DEGREES, Angle::DEGREES_STR ),
    controllerOvertemp_(false),
    currentAperture_( DriveEngine::RADIO3MM ),
    drive_( drive ),
    azEncoder_( azEncoder ), 
    elEncoder_( elEncoder ),
    tiltmeter_( tiltmeter ),
    mon_( mon.drive( ) ),
    commonDrive_( mon.antennaCommon( ).drive( )),
    subsystem_(mon)
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "DriveEngine::Pointing - Constructor." );
        
    apertureConstants_.insert( 
        ApertureConstantsMap::value_type( DriveEngine::OPTICAL, 
                                          ApertureConstants( ) ) );
    apertureConstants_.insert( 
        ApertureConstantsMap::value_type( DriveEngine::RADIO1MM, 
                                          ApertureConstants( ) ) );
    apertureConstants_.insert( 
        ApertureConstantsMap::value_type( DriveEngine::RADIO3MM, 
                                          ApertureConstants( ) ) );
    apertureConstants_.insert( 
        ApertureConstantsMap::value_type( DriveEngine::RADIO1CM, 
                                          ApertureConstants( ) ) );
}

DriveEngine::Pointing::~Pointing( )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "DriveEngine::Pointing - Destructor." );
}
    

bool
DriveEngine::Pointing::updatePosition( const bool applyFineCorrections ) 
{
    // Don't update position until all modules are seen at least once.
    if ( !allModulesOnlineAtLeastOnce_ ) {

        const canbus::deviceStateType azModState = azEncoder_.getState();
        const canbus::deviceStateType elModState = elEncoder_.getState();
        const canbus::deviceStateType driveModState = drive_.getState();
        const canbus::deviceStateType tiltModState = tiltmeter_.getState();

        if ( ( ( azModState == canbus::ONLINE ) && 
               ( elModState == canbus::ONLINE ) && 
               ( driveModState == canbus::ONLINE ) &&
               ( tiltModState == canbus::ONLINE ) )
             || 
             ( ( azModState == canbus::SIMULATED ) &&
               ( elModState == canbus::SIMULATED ) &&
               ( driveModState == canbus::SIMULATED ) &&
               ( tiltModState == canbus::SIMULATED ) ) ) 
        {
            allModulesOnlineAtLeastOnce_ = true;
        } else {
            return false; 
        }
    }

    const Encoder::TimestampedPosition azPosition =
        azEncoder_.getMostRecentPosition( );
    const Encoder::TimestampedPosition elPosition = 
        elEncoder_.getMostRecentPosition( );
    const Drive::TimestampedAzOverlapState azOverlap =
        drive_.getAzOverlapState( );

    const double now = Time::MJD( );

    const double azAgeInSeconds = 
        ( now - azPosition.mjd ) * Time::SECONDS_PER_DAY;
    const double elAgeInSeconds = 
        ( now - elPosition.mjd ) * Time::SECONDS_PER_DAY;
    const double azOverlapAgeInSeconds = 
        ( now - azOverlap.mjd ) * Time::SECONDS_PER_DAY;

    const double ageThreshInSeconds = 0.5;

    // Verify that new positions are current, if not, find out why and record. 
    // Note this won't detect errors when in SIM mode.
    if ( azAgeInSeconds < 0.0 || azAgeInSeconds > ageThreshInSeconds ) {
        if (azEncoder_.getState() != carma::canbus::ONLINE)
            ++offlineErrorCount_;
        else 
            ++timingErrorCount_; 

        return false;
    }

    if ( elAgeInSeconds < 0.0 || elAgeInSeconds > ageThreshInSeconds ) {        
        if ( elEncoder_.getState( ) != carma::canbus::ONLINE ) 
            ++offlineErrorCount_;
        else
            ++timingErrorCount_; 
    
        return false;
    }

    if ( azOverlapAgeInSeconds < 0.0 || 
         azOverlapAgeInSeconds > ageThreshInSeconds ) {        
        if ( drive_.getState( ) != carma::canbus::ONLINE ) 
            ++offlineErrorCount_;
        else
            ++timingErrorCount_;

        return false;
    }

    if ( tiltmeter_.getState( ) == canbus::ONLINE && updateTilts_ ) {
        updateAndCorrectTiltReadings();
    }

    azOverlapState_ = azOverlap;
    rawAzPosition_  = azPosition;
    rawElPosition_  = elPosition;

    // Set raw position as an AzElPair
    rawPosition_.azimuth   = azPosition.position;
    rawPosition_.elevation = elPosition.position;
    
    // Start with raw position,
    correctedPosition_ = rawPosition_;
    // then apply corrections
    applyPointingCorrections( correctedPosition_, applyFineCorrections );
    
    if ( updateTilts_ ) updateTiltMagnitudeAndDirection( correctedPosition_ );
    
    controllerOvertemp_ = drive_.getControllerOvertemp();

    return true;
}

AzElPair
DriveEngine::Pointing::getPosition( ) const
{
    return correctedPosition_;
}

AzElPair
DriveEngine::Pointing::getRawPosition( ) const
{
    return rawPosition_;
}

DriveEngine::Pointing::ApertureConstants 
DriveEngine::Pointing::getApertureConstants( const ApertureType  ap ) const
{
    typedef ApertureConstantsMap::const_iterator ApConstMapConstIt; 
    const ApConstMapConstIt it = apertureConstants_.find( ap );

    if ( it == apertureConstants_.end() ) {
        throw CARMA_EXCEPTION( IllegalStateException, "No aperture constants "
            "have been set for the selected aperture." );
    }

    return it->second;
}

void 
DriveEngine::Pointing::applyPointingCorrections( 
    AzElPair & coord,
    const bool applyFineCorrections ) const
{
    applyEncoderOffsetCorrections( coord );

    if ( applyFineCorrections ) {
        applyApertureAndMountCorrections( coord );
    }
}

void 
DriveEngine::Pointing::applyEncoderOffsetCorrections( AzElPair & coord ) const
{
    // Apply azimuth overlap correction
    if ( azOverlapState_.azOverlapDetected && 
         coord.azimuth.radians( false ) >= M_PI ) {
        coord.azimuth -= Angle( 2.0 * M_PI, Angle::RADIANS_STR );
    }
    
    // Apply encoder zero corrections.
    coord.azimuth -= mountConstants_.azEncoderZero;   
    coord.elevation -= mountConstants_.elEncoderZero; 

    // Apply tilt correction
    coord.elevation -= tilts_.correctedAftForwardTilt;
}

void
DriveEngine::Pointing::applyApertureAndMountCorrections( 
    AzElPair & coord ) const
{
    // Aperture and mount corrections align the beam with the local
    // Az/El reference frame (defined by North and the local horizon).
    // The corrections move both the mount structure itself (mount corrections)
    // and the particular aperture into the local Az/El reference frame.  All 
    // corrections are specified as collimation errors which is to say that
    // they are angles measured with respect to an ideal beam collimated with
    // the desired Az/El reference frame. Collimated errors are often described
    // as 'on the sky' errors though I personally find this term confusing.  
    // An easy way for me to envision this reference frame is to imagine a 
    // reticle at the focus of a perfect dish.  Azimuth and elevation are then
    // aligned with the x axis and y axis of the reticle.

    const ApertureConstants apc = getApertureConstants( currentAperture_ );

    const Angle totalAzMountOffset = 
        apc.azOffset + mountConstants_.azMountOffset;
    const Angle totalElMountOffset = 
        apc.elOffset + mountConstants_.elMountOffset;
    const Angle sagElOffset = apc.sag * cos( coord.elevation );

    const Angle elc = coord.elevation - totalElMountOffset - sagElOffset;

    const double sinaz = sin( coord.azimuth );
    const double cosaz = cos( coord.azimuth );

    const double sinelc = sin( elc );
    const double coselc = cosEl( elc );

    coord.azimuth -= ( ( totalAzMountOffset + 
                    ( mountConstants_.azElNonOrthogonality
                       - tilts_.correctedLeftRightTilt // LR tilt negated!
                       - ( mountConstants_.axisAlignmentNorthSouth * sinaz )
                       - ( mountConstants_.axisAlignmentEastWest * cosaz )
                     ) * sinelc 
                  ) / coselc ); // Convert back to encoder frame

    coord.elevation = elc 
                + mountConstants_.axisAlignmentNorthSouth * cosaz
                - mountConstants_.axisAlignmentEastWest * sinaz;
}

void DriveEngine::Pointing::updateLastCoefficientChange( ) 
{
    lastCoefficientChange_ = Time::MJD( );
}
    
void DriveEngine::Pointing::updateAndCorrectTiltReadings( )
{
    const Angle rawAftForwardTilt = tiltmeter_.getMostRecentAftForwardTilt( );
    tilts_.correctedAftForwardTilt = 
        rawAftForwardTilt - mountConstants_.aftForwardTiltZero;

    const Angle rawLeftRightTilt = tiltmeter_.getMostRecentLeftRightTilt( );
    tilts_.correctedLeftRightTilt = 
        rawLeftRightTilt - mountConstants_.leftRightTiltZero;
}

void DriveEngine::Pointing::updateTiltMagnitudeAndDirection( 
    const AzElPair & finalCorrectedAzEl )
{
    tilts_.direction = services::atan2( 
        tilts_.correctedLeftRightTilt.arcMinutes( ), 
        tilts_.correctedAftForwardTilt.arcMinutes( ) );

    // Place into azimuth coordinate system
    tilts_.direction += finalCorrectedAzEl.azimuth; 

    tilts_.magnitude.reset( 
        hypot( 
            tilts_.correctedAftForwardTilt.arcMinutes(), 
            tilts_.correctedLeftRightTilt.arcMinutes() ),
        Angle::ARCMIN_STR );
}
    
void
DriveEngine::Pointing::selectAperture( 
    const DriveEngine::ApertureType aperture )
{
    const ApertureConstantsMap::const_iterator 
        apConstants = apertureConstants_.find( aperture );
    if ( apConstants == apertureConstants_.end( ) ) {
        throw CARMA_EXCEPTION( IllegalStateException, "No aperture constants "
            "have been set for the selected aperture." );
    }

    currentAperture_ = aperture;
}

void
DriveEngine::Pointing::setAperturePointingConstants( 
    const DriveEngine::ApertureType aperture,
    const Angle & azOffset,
    const Angle & elOffset,
    const Angle & sag )
{
    ApertureConstants newConstants;    

    newConstants.azOffset = azOffset;
    newConstants.elOffset = elOffset;
    newConstants.sag = sag;

    apertureConstants_[ aperture ] = newConstants;

    updateLastCoefficientChange( );
}

void 
DriveEngine::Pointing::setTiltmeterZeros( 
    const Angle & aftForwardZero, 
    const Angle & leftRightZero )
{
    mountConstants_.aftForwardTiltZero = aftForwardZero;
    mountConstants_.leftRightTiltZero = leftRightZero;

    updateLastCoefficientChange( );
}

void
DriveEngine::Pointing::setAzMountOffset(
    const Angle & azMountOffset )
{
    mountConstants_.azMountOffset = azMountOffset;

    updateLastCoefficientChange( );
}

void
DriveEngine::Pointing::setElMountOffset(
    const Angle & elMountOffset )
{
    mountConstants_.elMountOffset = elMountOffset;

    updateLastCoefficientChange( );
}

void
DriveEngine::Pointing::setAzEncoderOffset(
    const Angle & azEncoderOffset )
{
    mountConstants_.azEncoderZero = azEncoderOffset;  

    updateLastCoefficientChange( );
}

void
DriveEngine::Pointing::setElEncoderOffset(
    const Angle & elEncoderOffset )
{
    mountConstants_.elEncoderZero = elEncoderOffset; 

    updateLastCoefficientChange( );
}
    
void 
DriveEngine::Pointing::setMountPointingConstants( 
    const Angle & nonOrthogonality,
    const Angle & axisAlignmentNorthSouth,
    const Angle & axisAlignmentEastWest )
{
    mountConstants_.azElNonOrthogonality = nonOrthogonality;
    mountConstants_.axisAlignmentNorthSouth = axisAlignmentNorthSouth;
    mountConstants_.axisAlignmentEastWest = axisAlignmentEastWest;

    updateLastCoefficientChange( );
}

void
DriveEngine::Pointing::writeMonitorData( ) const
{
    const double azMJD = rawAzPosition_.mjd;
    const double elMJD = rawElPosition_.mjd;
    const double azLatency = millisecondsSinceCurrentFrame( azMJD );
    const double elLatency = millisecondsSinceCurrentFrame( elMJD );
    const Angle azCorrection = 
        correctedPosition_.azimuth - rawAzPosition_.position;
    const Angle elCorrection =
        correctedPosition_.elevation - rawElPosition_.position;

    OvroSubsystem::System & system = mon_.system( );

    system.timingErrorCount( ).setValue( timingErrorCount_ );
    system.encoderOfflineCount( ).setValue( offlineErrorCount_ );
    system.azEncoderLatency( ).setValue( azLatency );
    system.elEncoderLatency( ).setValue( elLatency );
    system.azPointingCorrection( ).setValue( azCorrection.arcSeconds( ) );
    system.elPointingCorrection( ).setValue( elCorrection.arcSeconds( ) );
    
    OvroSubsystem::Constants & mc = mon_.point( ).constants( );
    mc.m1().setValue( mountConstants_.azEncoderZero.arcMinutes( ) );
    mc.m2().setValue( mountConstants_.elEncoderZero.arcMinutes( ) );
    mc.m3().setValue( mountConstants_.azElNonOrthogonality.arcMinutes( ) );
    mc.m4().setValue( mountConstants_.axisAlignmentNorthSouth.arcMinutes( ) );
    mc.m5().setValue( mountConstants_.axisAlignmentEastWest.arcMinutes( ) );
    
    mc.af0().setValue( mountConstants_.aftForwardTiltZero.arcMinutes( ) );
    mc.lr0().setValue( mountConstants_.leftRightTiltZero.arcMinutes( ) );

    double mag = tilts_.magnitude.arcMinutes( );
    subsystem_.tiltmeter().tiltMag().setValue(mag);
    mon_.point().tilt().setValue(mag);
    commonDrive_.point().magnitude().setValue(mag);

    double dir = tilts_.direction.degrees(true);
    subsystem_.tiltmeter().tiltDirection().setValue(dir);
    mon_.point().tiltDirection().setValue(dir);
    commonDrive_.point().direction().setValue(dir);
    
    // If a radio aperture is selected, use that, otherwise default to 3mm
    typedef AntennaCommon::SelectedApertMonitorPointEnum APE;

    ApertureConstants apcRadio;
    switch ( currentAperture_ ) {
        case DriveEngine::RADIO3MM:
            commonDrive_.point().constants().selectedApert( ).setValue( 
                APE::RADIO3MM );
            apcRadio = getApertureConstants( currentAperture_ );
            break;
        case DriveEngine::RADIO1MM:
            commonDrive_.point().constants().selectedApert( ).setValue(
                APE::RADIO1MM );
            apcRadio = getApertureConstants( currentAperture_ );
            break;
        case DriveEngine::RADIO1CM:
            commonDrive_.point().constants().selectedApert( ).setValue(
                APE::RADIO1CM );
            apcRadio = getApertureConstants( currentAperture_ );
            break;
        case DriveEngine::OPTICAL:
            commonDrive_.point().constants().selectedApert( ).setValue(
                APE::OPTICAL );
            apcRadio = getApertureConstants( DriveEngine::RADIO3MM ); // sic
            break;
        default:
            break;
    }
    

    mc.r1().setValue( apcRadio.azOffset.arcMinutes( ) );
    mc.r2().setValue( apcRadio.elOffset.arcMinutes( ) );
    mc.r3().setValue( apcRadio.sag.arcMinutes( ) );

    const ApertureConstants apcOptical = 
        getApertureConstants( DriveEngine::OPTICAL );
    mc.o1().setValue( apcOptical.azOffset.arcMinutes( ) );
    mc.o2().setValue( apcOptical.elOffset.arcMinutes( ) );
    mc.o3().setValue( apcOptical.sag.arcMinutes( ) );

    mon_.point( ).mountOffsetAz( ).setValue( 
        mountConstants_.azMountOffset.arcMinutes( ) );
    commonDrive_.point( ).mountOffsetAz( ).setValue(
        mountConstants_.azMountOffset.arcMinutes( ) );

    mon_.point( ).mountOffsetEl( ).setValue( 
        mountConstants_.elMountOffset.arcMinutes( ) );
    commonDrive_.point( ).mountOffsetEl( ).setValue(
        mountConstants_.elMountOffset.arcMinutes( ) );

    commonDrive_.point( ).constants( ).coefChange( ).setValue( 
        lastCoefficientChange_ );

    // Finally set all of the common aperture pointing coefficients.
    typedef AntennaCommon::SelectedApertMonitorPointEnum SelectedApertMPE;

    compileTimeCheckApEnums(); // Check that enums are numerically equivalent.

    for ( int ap = OPTICAL; ap < APERTURE_COUNT; ++ap ) {

        typedef AntennaCommon::ApertureCoefficients AntComApCoefs;
        AntennaCommon::Constants & comConstants = 
            commonDrive_.point().constants();
        AntComApCoefs & monAp = comConstants.apertureCoefficients( ap );

        const ApertureConstants apc = getApertureConstants( 
            static_cast< DriveEngine::ApertureType >( ap ) );

        monAp.elCollErr().setValue( apc.elOffset.arcMinutes() );
        monAp.crossElCollErr().setValue( apc.azOffset.arcMinutes() );
        monAp.sag().setValue( apc.sag.arcMinutes() ); 
    }
}
    
void 
DriveEngine::Pointing::updateTilts( const bool update )
{
    updateTilts_ = update;
}

bool 
DriveEngine::Pointing::checkForAzOverlapSwitchFail( ) const
{
    // Exploit the fact that the az overlap switch engages several degrees
    // before reaching 0 (e.g. @ +4degrees) and verify that it is indeed
    // engaged at this juncture.
    const bool fail = ( rawAzPosition_.position < azOverlapTestAngle_ &&  
                        !azOverlapState_.azOverlapDetected );

    if ( fail ) {
        ostringstream msg;
        msg << "DriveEngine::Pointing::checkForAzOverlapSwitchFail() - "
            << "Az overlap switch failure detected @ rawAzPosition_="
            << fixed << setprecision(3)
            << rawAzPosition_.position.degrees( ) << " deg - FATAL!";
        programLogErrorIfPossible( msg.str( ) );
    }

    return fail;
}

bool 
DriveEngine::Pointing::getControllerOvertemp() const
{
    const bool fail = controllerOvertemp_;

    if (fail) {
        ostringstream msg;
        msg << "DriveEngine::Pointing:getControllerOvertemp() - "
            << "At least one of the three drive controllers is showing "
            << "an overtemp contition - FATAL!";
        programLogErrorIfPossible(msg.str());
    }
    return controllerOvertemp_;
}

/**
 * Calculate nominal encoder rates per second.  
 * This class deals exclusively with encoder values, all inputs and outputs
 * are assumed to be encoder values.
 */
class DriveEngine::Servo {
public:

    /**
     * Constructor
     * @param maxEncoderRateInRadPerS Max encoder rate in radians per second.
     * @param clip Clip factor @see DriveEngine::Servo::clipToVelocityProfile.
     * @param backlash Backlash correction factor.
     * @see DriveEngine::Servo::setMaxEncoderRate
     */
    explicit Servo( double maxEncoderRateInRadPerS,
                    double clip,
                    double backlash );

    ~Servo( );

    double
    calculateRequestedRate( double requestedPositionInRad,
                            double currentPositionInRad );

    void setMaxEncoderRate( double maxEncoderRateInRadPerSec );

    double getServoBiasInArcMinutes( ) const;

    void toggleBacklashCorrection( bool enable );

private:

    /**
     * Clip rate to 'the velocity profile'.
     * Input rates are clipped to a velocity profile characterized
     * by the clip constructor parameter.  The max rate divided by 
     * the clip defines the max servo tracking rate.  The max rate 
     * multiplied by the clip defines the slew rate threshold.  More
     * specifically the velocity profile is as follows:
     * Tracking region (0 < input-rate < max-rate/clip) - Full error 
     *      correcting servo.
     * Constant region (max-rate/clip < input-rate < max-rate) - Drive
     *      at a constant rate = max-rate/clip.
     * Linear region (max-rate < input-rate < clip * max-rate) - Linear
     *      rate.
     * Slew region (input-rate > clip * max-rate) - Full slew rate.
     *
     * This profile is described in detail on page 74 of 
     * "A 10-Meter Telescope for Millimeter and Sub-Millimeter Astronomy", 
     * Robert B. Leighton, May 1978.
     * @param normalizedRate Requested rate to clip to velocity profile.
     */
    void 
    clipToVelocityProfile( double & rateInRadPerS );

    /**
     * The purpose of the backlash correction is to overcome the 'dead-zone'
     * where no gearing is engaged upon direction reversal.  This is done by
     * slightly *increasing* the rate in the new direction.
     * @param nominalRateInRadPerS Input rate to apply backlash correction to.
     * @todo This probably doesn't belong here but should rather be shoved
     * down into the canbus/Drive.h implementation.  The reason being that 
     * the backlash correction being applied is a single constant value for
     * both az and el with no consideration given for leverarm calcs and such.
     * If this instead existed in canbus/Drive, backlash corrections could
     * be better modeled to match the actual gearing it is trying to overcome.
     */
    void 
    applyBacklashCorrection( double & nominalRateInRadPerS );

    /**
     * Limit input normalized rate to the max normalized rate.
     */
    void 
    limitToMaxRate( double & rateInRadPerS );
     
    double lastRequestedPositionInRad_;
    double lastRequestedRateInRadPerS_; 
    double maxEncoderRateInRadPerS_; // Max encoder rate
    double error_;  // Most recent update error
    double bias_;   // Cumulative sum of errors

    const double clip_;
    const double backlash_;
    bool applyBacklash_;
};

DriveEngine::Servo::Servo( const double maxEncoderRateInRadPerS,
                           const double clip,
                           const double backlash ) :
        lastRequestedPositionInRad_( 0.0 ), 
        lastRequestedRateInRadPerS_( 0.0 ),
        maxEncoderRateInRadPerS_( maxEncoderRateInRadPerS ),
        error_( 0.0 ),
        bias_( 0.0 ),
        clip_( clip ),
        backlash_( backlash ),
        applyBacklash_( true )
{ }

DriveEngine::Servo::~Servo( ) { }

double
DriveEngine::Servo::calculateRequestedRate( 
        const double requestedPositionInRad, 
        const double currentPositionInRad )
{
    error_ = currentPositionInRad - lastRequestedPositionInRad_;

    // Apply a first order correction to account for the fact that this
    // is being ran several milliseconds after the half second
    const double secondsAfterCurrentHalfSec = ( Time::MJD( ) - 
        Time::MJD( Time::computeCurrentFrame( ) ) ) * Time::SECONDS_PER_DAY;

    const double correctedLastRequestedPositionInRad = 
        lastRequestedPositionInRad_ + 
        lastRequestedRateInRadPerS_ * secondsAfterCurrentHalfSec;

    const double nominalRateInRadPerS = 
        ( requestedPositionInRad - correctedLastRequestedPositionInRad ) / 
        ( UPDATE_PERIOD_IN_SEC - secondsAfterCurrentHalfSec );

    lastRequestedPositionInRad_ = requestedPositionInRad;

    double requestedRateInRadPerS = nominalRateInRadPerS - 
        ( ( GAMMA * error_ ) + ( BETA * bias_ ) ) / UPDATE_PERIOD_IN_SEC;
    
    bias_ += error_;  // Bias is reset by clip routine

    CARMA_CPTRACE( TRACE_RATE_CALCS, "calculateRequestedRate "
        << "- Rate prior to clip = " << requestedRateInRadPerS << ". "
        << " nominal=" << ( nominalRateInRadPerS * DEG_PER_RAD * 60.0 ) 
        << " deg/m, error=" << ( error_ * DEG_PER_RAD * 60.0 ) 
        << " deg/m, bias=" << bias_ << "." );

    clipToVelocityProfile( requestedRateInRadPerS );
    
    CARMA_CPTRACE( TRACE_RATE_CALCS, "calculateRequestedRate "
        << "- Rate post clip = " << requestedRateInRadPerS << "." );

    if ( applyBacklash_ ) {
        applyBacklashCorrection( requestedRateInRadPerS );
    }
    
    CARMA_CPTRACE( TRACE_RATE_CALCS, "calculateRequestedRate "
        << "- Rate after backlash = " << requestedRateInRadPerS << "." );

    // This should be nothing more than a check since clip should handle this.
    limitToMaxRate( requestedRateInRadPerS );
    
    lastRequestedRateInRadPerS_ = requestedRateInRadPerS;
    
    return requestedRateInRadPerS;
}

void 
DriveEngine::Servo::setMaxEncoderRate( const double maxEncoderRateInRadPerSec )
{
    maxEncoderRateInRadPerS_ = maxEncoderRateInRadPerSec;
}
    
double 
DriveEngine::Servo::getServoBiasInArcMinutes( ) const
{
    return bias_ * DEG_PER_RAD * ARCMIN_PER_DEG;
}

void
DriveEngine::Servo::toggleBacklashCorrection( const bool enable ) 
{
    applyBacklash_ = enable;
}

void
DriveEngine::Servo::clipToVelocityProfile( double & rateInRadPerS )
{
    const double absNormalizedRate = 
        fabs( rateInRadPerS ) / maxEncoderRateInRadPerS_;
    const double sign = ( rateInRadPerS < 0.0 ? -1.0 : 1.0 );

    if ( absNormalizedRate < 1.0 / clip_ ) {  
        // Tracking region - don't clip
        return;
    } else if ( absNormalizedRate < 1.0 ) {       
        // Constant region - clip to a constant fraction of the max slew rate
        bias_ = 0.0;
        rateInRadPerS = sign * maxEncoderRateInRadPerS_ / clip_;
    } else if ( absNormalizedRate < clip_ ) { 
        // Linear region - clip to linear function y = x/clip
        bias_ = 0.0;
        rateInRadPerS = rateInRadPerS / clip_;
    } else {                                      
        // Slew region  - let 'er rip at max rate
        bias_ = 0.0;
        rateInRadPerS = sign * maxEncoderRateInRadPerS_;
    }

} // clipToVelocityProfile
        
void
DriveEngine::Servo::applyBacklashCorrection( double & rateInRadPerS )
{
    // The purpose of the backlash correction is to overcome the 'dead-zone'
    // where no gearing is engaged upon direction reversal.  This is done by
    // slightly *increasing* the rate in the new direction.
    const double backlashCorrectionInRadPerS = 
        backlash_ * maxEncoderRateInRadPerS_;

    // Test if rates have reversed sign
    if ( rateInRadPerS * lastRequestedRateInRadPerS_ < 0.0 ) {

        if ( rateInRadPerS < 0.0 ) {
            rateInRadPerS -= backlashCorrectionInRadPerS;
        } else {
            rateInRadPerS += backlashCorrectionInRadPerS;
        }
    } 

} // applyBacklashCorrection

void
DriveEngine::Servo::limitToMaxRate( double & rateInRadPerS )
{
    const double absRateInRadPerS = fabs( rateInRadPerS );

    if ( absRateInRadPerS > maxEncoderRateInRadPerS_ ) {
        const double sign = ( rateInRadPerS >= 0.0 ? 1.0 : -1.0 ); 
        rateInRadPerS = maxEncoderRateInRadPerS_ * sign;
    }
} // limitToMaxRate

class DriveEngine::Limits {
public:
    
    enum AxisType {
        AZIMUTH,
        ELEVATION };

    Limits( OvroSubsystem & mon );

    void updateLimitState( const AzElPair & actualPosition );

    bool isAtSwLimit( ) const;

    bool isAtSwLimit( enum AxisType axis ) const;

    bool isAzForbidden(const AzElPair& azel) const;
    bool azWillTriggerSwLimit( const Angle & az ) const;
    void turnAzToWithinLimits( Angle & az ) const;

    void bindPositionToLimits( AzElPair & position ) const;
    
    void writeMonitorData( );

    void setSafeRange( const Angle & safeLowAz, 
                       const Angle & safeHighAz,
                       const Angle & safeLowEl, 
                       const Angle & safeHighEl );
    
    bool safeRangeSet( ) const;

    AzElPair getSafeStowPosition( );

    AzElPair getZenithStowPosition( ) const;

private:

    bool azAtPosLimit_; // Software limits
    bool azAtNegLimit_;
    bool elAtPosLimit_;
    bool elAtNegLimit_;
    bool safeSet_;
    bool isSafe_;
    Angle azSafeHigh_;
    Angle azSafeLow_;
    Angle elSafeHigh_;
    Angle elSafeLow_;
    OvroSubsystem::Drive & monDrive_;
    AntennaCommon::Drive & commonDrive_;

    const Angle AZ_SW_LIMIT_PLUS;
    const Angle AZ_SW_LIMIT_MINUS; 
    const Angle EL_SW_LIMIT_PLUS;
    const Angle EL_SW_LIMIT_MINUS;

    const Angle AZ_HW_LIMIT_PLUS;
    const Angle AZ_HW_LIMIT_MINUS;
    const Angle EL_HW_LIMIT_PLUS;
    const Angle EL_HW_LIMIT_MINUS;

    AzElPair safeStowPosition_;  
    const AzElPair ZENITH_STOW_POSITION;

}; // class DriveEngine::Limits

DriveEngine::Limits::Limits( OvroSubsystem & mon ) :
    azAtPosLimit_( false ),
    azAtNegLimit_( false ),
    elAtPosLimit_( false ),
    elAtNegLimit_( false ),
    safeSet_( false ),
    isSafe_( false ),
    monDrive_( mon.drive( ) ),
    commonDrive_( mon.antennaCommon( ).drive( ) ),
    AZ_SW_LIMIT_PLUS( SW_MAX_AZ_DEGREES, Angle::DEGREES_STR),
    AZ_SW_LIMIT_MINUS(SW_MIN_AZ_DEGREES, Angle::DEGREES_STR),
    EL_SW_LIMIT_PLUS( SW_MAX_EL_DEGREES, Angle::DEGREES_STR),
    EL_SW_LIMIT_MINUS(SW_MIN_EL_DEGREES, Angle::DEGREES_STR),
    AZ_HW_LIMIT_PLUS( HW_MAX_AZ_DEGREES, Angle::DEGREES_STR),
    AZ_HW_LIMIT_MINUS(HW_MIN_AZ_DEGREES, Angle::DEGREES_STR),
    EL_HW_LIMIT_PLUS( HW_MAX_EL_DEGREES, Angle::DEGREES_STR),
    EL_HW_LIMIT_MINUS(HW_MIN_EL_DEGREES, Angle::DEGREES_STR),
    safeStowPosition_( AzElPair( 
                        Angle( STOW_AZ_DEGREES, Angle::RADIANS_STR ),
                        Angle( STOW_EL_DEGREES, Angle::DEGREES_STR ) ) ),
    ZENITH_STOW_POSITION( AzElPair( 
                        Angle( STOW_AZ_DEGREES, Angle::RADIANS_STR ),
                        Angle( STOW_EL_DEGREES, Angle::DEGREES_STR ) ) )
{
    // Nothing
}

void
DriveEngine::Limits::updateLimitState( const AzElPair & actualPosition )
{
    // Determine if drives have reached azimuth or elevation limits.
    // In order to prevent fibrillation (rapidly moving in and out of limit),
    // limits are calculated differently depending on whether we are 
    // driving onto a limit (e.g. triggering a limit) or driving off of
    // a limit. When driving onto a limit, software limits are used per se.
    // When driving off of a limit, a hysteresis is used.  Note that 
    // the legacy vax system used the opposite scheme. 

    const Angle ZERO( 0.0, Angle::RADIANS_STR );
    const Angle LIMIT_HYSTERESIS( 0.1, Angle::DEGREES_STR );

    Angle hysteresis;

    if ( azAtPosLimit_ || azAtNegLimit_ ) { 
        hysteresis = LIMIT_HYSTERESIS; // Use hysteresis
    } else { 
        hysteresis = ZERO; // Use limits per se NOT hysteresis
    }

    azAtNegLimit_ = 
        ( actualPosition.azimuth <= ( AZ_SW_LIMIT_MINUS + hysteresis ) );
    azAtPosLimit_ =
        ( actualPosition.azimuth >= ( AZ_SW_LIMIT_PLUS - hysteresis ) );

    if ( elAtPosLimit_ || elAtNegLimit_ ) {
        hysteresis = LIMIT_HYSTERESIS; // Use hysteresis
    } else {
        hysteresis = ZERO; // Use limits per se NOT hysteresis
    }

    elAtNegLimit_ = 
        ( actualPosition.elevation <= ( EL_SW_LIMIT_MINUS + hysteresis ) );

    elAtPosLimit_ = 
        ( actualPosition.elevation >= ( EL_SW_LIMIT_PLUS - hysteresis ) );

    // Determine if antenna is in a 'safe' position
    isSafe_ = ( actualPosition.azimuth   > azSafeHigh_ || 
               actualPosition.azimuth   < azSafeLow_  || 
               actualPosition.elevation > elSafeHigh_ ||
               actualPosition.elevation < elSafeLow_ );
}

bool
DriveEngine::Limits::isAtSwLimit( ) const
{
    return ( azAtPosLimit_ || azAtNegLimit_ || elAtPosLimit_ || elAtNegLimit_ );
}
 
bool 
DriveEngine::Limits::isAzForbidden(const AzElPair& azel) const
{
    const Angle az = azel.azimuth;
    // A forbidden azimuth is one which is outside of the hardware limits.
    const bool azForbidden = ( az >= AZ_HW_LIMIT_PLUS || 
                               az <= AZ_HW_LIMIT_MINUS );
    if ( azForbidden ) {
        ostringstream msg;

        msg << "DriveEngine::Limits::isAzForbidden() - Azimuth of " 
            << fixed << setprecision(1) << az.degrees() 
            << " degrees is forbidden (outside of hardware limits) - FATAL!";

        programLogErrorIfPossible( msg.str( ) );
    }

    return azForbidden;
}

bool 
DriveEngine::Limits::isAtSwLimit( enum AxisType axis ) const
{
    if ( axis == AZIMUTH ) {
        return azAtPosLimit_ || azAtNegLimit_;
    } else if ( axis == ELEVATION ) {
        return elAtPosLimit_ || elAtNegLimit_;
    } else {
        throw CARMA_EXCEPTION( IllegalArgumentException, "Invalid axis." );
    }
}

bool 
DriveEngine::Limits::azWillTriggerSwLimit( const Angle & az ) const
{
    if ( az >= AZ_SW_LIMIT_PLUS || az <= AZ_SW_LIMIT_MINUS ) {
        return true;
    } else {
        return false;
    }
}

void
DriveEngine::Limits::turnAzToWithinLimits( Angle & az ) const
{
    while ( az >= AZ_SW_LIMIT_PLUS ) {
        az -= Angle( 2.0 * M_PI, Angle::RADIANS_STR );
    }

    while ( az <= AZ_SW_LIMIT_MINUS ) {
        az += Angle( 2.0 * M_PI, Angle::RADIANS_STR );
    }
}

void
DriveEngine::Limits::writeMonitorData( )
{
    OvroSubsystem::Limit & limitMon = monDrive_.limit();
    AntennaCommon::Limit & commonLimit = commonDrive_.limit();

    typedef OvroSubsystem::AzSwLimitMonitorPointEnum AzLimitEnum;
    typedef AntennaCommon::AzSwLimitMonitorPointEnum ComAzLimitEnum;

    if ( azAtPosLimit_ && azAtNegLimit_ ) {
        limitMon.azSwLimit( ).setValue( AzLimitEnum::ERROR );
    } else if ( azAtPosLimit_ ) {
        limitMon.azSwLimit( ).setValue( AzLimitEnum::PLUSLIM );
        commonLimit.azSwLimit( ).setValue( ComAzLimitEnum::PLUSLIM ); 
    } else if ( azAtNegLimit_ ) {
        limitMon.azSwLimit( ).setValue( AzLimitEnum::MINUSLIM );
        commonLimit.azSwLimit( ).setValue( ComAzLimitEnum::MINUSLIM ); 
    } else {
        limitMon.azSwLimit( ).setValue( AzLimitEnum::OK );   
        commonLimit.azSwLimit( ).setValue( ComAzLimitEnum::OK ); 
    }

    typedef OvroSubsystem::ElSwLimitMonitorPointEnum ElLimitEnum;
    typedef AntennaCommon::ElSwLimitMonitorPointEnum ComElLimitEnum;

    if ( elAtPosLimit_ && elAtNegLimit_ ) {
        limitMon.elSwLimit( ).setValue( ElLimitEnum::ERROR );
    } else if ( elAtPosLimit_ ) {
        limitMon.elSwLimit( ).setValue( ElLimitEnum::PLUSLIM );
        commonLimit.elSwLimit( ).setValue( ComElLimitEnum::PLUSLIM ); 
    } else if ( elAtNegLimit_ ) {
        limitMon.elSwLimit( ).setValue( ElLimitEnum::MINUSLIM );
        commonLimit.elSwLimit( ).setValue( ComElLimitEnum::MINUSLIM ); 
    } else {
        limitMon.elSwLimit( ).setValue( ElLimitEnum::OK );
        commonLimit.elSwLimit( ).setValue( ComElLimitEnum::OK ); 
    }

    commonLimit.azLowSwLimitVal( ).setValue( AZ_SW_LIMIT_MINUS.degrees( ) );
    commonLimit.azHighSwLimitVal( ).setValue( AZ_SW_LIMIT_PLUS.degrees( ) );
    commonLimit.elLowSwLimitVal( ).setValue( EL_SW_LIMIT_MINUS.degrees( ) );
    commonLimit.elHighSwLimitVal( ).setValue( EL_SW_LIMIT_PLUS.degrees( ) );

    commonLimit.azLowHwLimitVal( ).setValue( AZ_HW_LIMIT_MINUS.degrees( ) );
    commonLimit.azHighHwLimitVal( ).setValue( AZ_HW_LIMIT_PLUS.degrees( ) );
    commonLimit.elLowHwLimitVal( ).setValue( EL_HW_LIMIT_MINUS.degrees( ) );
    commonLimit.elHighHwLimitVal( ).setValue( EL_HW_LIMIT_PLUS.degrees( ) );

    typedef AntennaCommon::SafeStateMonitorPointEnum SafeStateEnum;

    if ( isSafe_ ) {
        commonDrive_.safeState( ).setValue( SafeStateEnum::SAFE );
    } else {
        commonDrive_.safeState( ).setValue( SafeStateEnum::UNSAFE );
    }

    commonDrive_.safeAzHigh( ).setValue( azSafeHigh_.degrees( ) );
    commonDrive_.safeAzLow( ).setValue( azSafeLow_.degrees( ) );
    commonDrive_.safeElHigh( ).setValue( elSafeHigh_.degrees( ) );
    commonDrive_.safeElLow( ).setValue( elSafeLow_.degrees( ) );
}
    
void 
DriveEngine::Limits::setSafeRange( const Angle & safeLowAz, 
                                   const Angle & safeHighAz,
                                   const Angle & safeLowEl, 
                                   const Angle & safeHighEl )
{
    if ( safeHighAz < safeLowAz || safeHighEl < safeLowEl ) {
        throw CARMA_EXCEPTION( IllegalStateException, "Limits::setSafeRange() -"
            " High safe range angle is less than the low range." );
    } 

    AzElPair highPosition( safeHighAz, safeHighEl );
    AzElPair lowPosition( safeLowAz, safeLowEl );

    bindPositionToLimits( highPosition );
    bindPositionToLimits( lowPosition );

    azSafeHigh_ = highPosition.azimuth;
    azSafeLow_ = lowPosition.azimuth;
    elSafeHigh_ = highPosition.elevation;
    elSafeLow_ = lowPosition.elevation;
    safeSet_ = true;

    safeStowPosition_.azimuth = ( azSafeHigh_ + azSafeLow_ ) / 2.0;
    safeStowPosition_.elevation = ( elSafeHigh_ + elSafeLow_ ) / 2.0;
}

bool 
DriveEngine::Limits::safeRangeSet( ) const
{
    return safeSet_;
}
    
AzElPair 
DriveEngine::Limits::getSafeStowPosition( ) 
{
    return safeStowPosition_;
}

AzElPair 
DriveEngine::Limits::getZenithStowPosition( ) const
{
    return ZENITH_STOW_POSITION;
}

void
DriveEngine::Limits::bindPositionToLimits( AzElPair & position ) const
{
    // Don't allow requested position to exceed sw limits.
    AzElPair boundPosition;

    if ( position.azimuth < AZ_SW_LIMIT_MINUS ) {
        boundPosition.azimuth = AZ_SW_LIMIT_MINUS;
    } else if ( position.azimuth > AZ_SW_LIMIT_PLUS ) {
        boundPosition.azimuth = AZ_SW_LIMIT_PLUS;
    } else {
        boundPosition.azimuth = position.azimuth;
    }

    if ( position.elevation < EL_SW_LIMIT_MINUS ) {
        boundPosition.elevation = EL_SW_LIMIT_MINUS;
    } else if ( position.elevation > EL_SW_LIMIT_PLUS ) {
        boundPosition.elevation = EL_SW_LIMIT_PLUS;
    } else {
        boundPosition.elevation = position.elevation;
    }
    
    position = boundPosition;
}
/**
 * Calculates source position based on ant location, on-the-sky offsets and 
 * weather parameters. This is in contrast to the Pointing class which handles 
 * offsets and corrections intrinsic to the dish structure itself. Essentially
 * this class wraps up the Ephemeris class and Ra/Dec interpolations.
 */
class DriveEngine::SourcePosition {
public:

    explicit SourcePosition( OvroSubsystem & mon,
                             const Limits & limits );

    ~SourcePosition( );
    
    AzElPair
    interpolateSourcePosition( carma::util::frameType framesInTheFuture ); 
    
    void
    setRaDec( double mjd, 
              double ra, 
              double dec,
              bool discontinuity,
              DriveEngine::WrapType wrap );

    void 
    setLocation( const Location & location );

    enum RefractionModel {
        RADIO,
        OPTICAL
    };

    void
    setRefractionModel( enum RefractionModel model );

    void 
    updateWeather( double ambientTempInCelcius, 
                   double barometricPressureInMillibar,
                   double relativeHumidityInPercent,
                   double dewpointTempInCelcius,
                   double windSpeedInMPH,
                   double windDirectionInDeg );

    void
    setSourceName( std::string source );

    AzElPair 
    calculateSnowTrackPosition( const AzElPair & currentPosition ) const;

    void
    writeMonitorData( );

private:

    struct WeatherInfo {
        WeatherInfo( );
        double ambientTempInC;
        double barometricPressureInMbar;
        double relativeHumidity;
        double dewPointInC;
        double windSpeedInMPH;
        double windDirectionInDeg;
    };

    string sourceName_;
    AzElPair snowTrackPosition_;
    WeatherInfo weather_;
    RefractionModel refractionModel_;
    services::Ephemeris ephemeris_; 
    util::QuadraticInterpolatorPositiveAngle raInterpolator_;
    util::QuadraticInterpolatorSignedAngle decInterpolator_;
    DriveEngine::WrapType wrap_;
    monitor::OvroSubsystem& mon_;
    monitor::AntennaCommon::Drive& commonDrive_;
    const Limits & limits_;
}; // class DriveEngine::SourcePosition

DriveEngine::SourcePosition::SourcePosition( OvroSubsystem & mon,
                                             const Limits & limits ) :
    sourceName_( "none" ),
    refractionModel_( RADIO ),
    raInterpolator_( 0.0 ),
    decInterpolator_( 0.0 ),
    wrap_( DriveEngine::NO_TURN ),
    mon_( mon ),
    commonDrive_( mon.antennaCommon( ).drive( ) ),
    limits_( limits )
{
    ephemeris_.setFreq( FREQ_3MM_IN_HZ );
    raInterpolator_.empty( );
    decInterpolator_.empty( );
}

DriveEngine::SourcePosition::~SourcePosition( )
{
    // Destructor
}

DriveEngine::SourcePosition::WeatherInfo::WeatherInfo( )
{
    ambientTempInC = Atmosphere::DEFAULT_AIR_TEMP + Physical::ABS_ZERO;
    barometricPressureInMbar = Atmosphere::DEFAULT_ATM_PRESSURE;
    relativeHumidity = Atmosphere::DEFAULT_RH;
    dewPointInC = Atmosphere::DEFAULT_DEW_POINT + Physical::ABS_ZERO;
    windSpeedInMPH = 0.0;
    windDirectionInDeg = 0.0;
}

void 
DriveEngine::SourcePosition::updateWeather( 
    const double ambientTempInCelcius, 
    const double barometricPressureInMillibar, 
    const double relativeHumidityInPercent,
    const double dewpointTempInCelcius, 
    const double windSpeedInMPH,
    const double windDirectionInDeg ) 
{
    const double ABS_ZERO_IN_C = Physical::ABS_ZERO; // -273.15
    ephemeris_.setWeather( 
        barometricPressureInMillibar,
        ambientTempInCelcius - ABS_ZERO_IN_C,
        relativeHumidityInPercent );

    weather_.ambientTempInC = ambientTempInCelcius;
    weather_.barometricPressureInMbar = barometricPressureInMillibar;
    weather_.relativeHumidity = relativeHumidityInPercent;
    weather_.dewPointInC = dewpointTempInCelcius;
    weather_.windSpeedInMPH = windSpeedInMPH;
    weather_.windDirectionInDeg = windDirectionInDeg;
}

void
DriveEngine::SourcePosition::setSourceName( const std::string source )
{
    sourceName_ = source;
}

void 
DriveEngine::SourcePosition::setLocation( const Location & location )
{
    ephemeris_.setLocation( location );
}

AzElPair
DriveEngine::SourcePosition::interpolateSourcePosition( 
    const frameType framesInTheFuture )
{
    const unsigned nRaPoints = raInterpolator_.getNpt( );
    const unsigned nDecPoints = decInterpolator_.getNpt( );

    if ( nRaPoints != nDecPoints || nRaPoints == 0 ) {
        ostringstream msg;
        msg << "Ra interpolator contains " << nRaPoints << " points while dec "
            << "interpolator contains " << nDecPoints << " points." << endl;
        programLogErrorIfPossible( msg.str( ) );
    }

    if ( nRaPoints < 3 ) {
        programLogWarnIfPossible( "Interpolators do not contain enough points "
            "for a quadratic interpolation - rather interpolation will be "
            "linear or constant (i.e. no interpolation)." );
    }
    
    const frameType currentFrame = Time::computeCurrentFrame( );
    const frameType updateFrame = currentFrame + framesInTheFuture;
    const double atMJD = Time::MJD( updateFrame );
    
    const double raInRad = raInterpolator_.evaluate( atMJD ); 
    const double decInRad = decInterpolator_.evaluate( atMJD );

    const Vector<double> updateAzEl = ephemeris_.getAzEl( atMJD, 
                                                          raInRad, 
                                                          decInRad );

    AzElPair updateAzElPair( Angle( updateAzEl[0], Angle::RADIANS_STR ),
                             Angle( updateAzEl[1], Angle::RADIANS_STR ) );

    switch ( wrap_ ) {
        case DriveEngine::NO_TURN:
            break;
        case DriveEngine::ADD_TURN:
            updateAzElPair.azimuth += Angle( 2.0 * M_PI, Angle::RADIANS_STR );
            break;
        case DriveEngine::SUBTRACT_TURN:
            updateAzElPair.azimuth -= Angle( 2.0 * M_PI, Angle::RADIANS_STR );
            break;
        default:
            break;
    }

    if ( limits_.azWillTriggerSwLimit( updateAzElPair.azimuth ) ) {
        limits_.turnAzToWithinLimits( updateAzElPair.azimuth );
    }
        
    return updateAzElPair;
}

void
DriveEngine::SourcePosition::setRaDec( const double mjd, 
                                       const double ra, 
                                       const double dec,
                                       const bool discontinuity,
                                       const DriveEngine::WrapType wrap )
{
    if ( discontinuity ) {
        raInterpolator_.empty( );
        decInterpolator_.empty( );
    }

    raInterpolator_.extend( mjd, ra );
    decInterpolator_.extend( mjd, dec );

    wrap_ = wrap;
}

void
DriveEngine::SourcePosition::setRefractionModel( 
    const enum RefractionModel model )
{
    // Set ephemeris observing frequencies and the 'refraction model' (for
    // monitoring only).  The frequencies do not need to be the exact observing
    // frequency as the ephemeris class only has two models - RADIO and OPTICAL.
    switch ( model ) {
        case RADIO:
            ephemeris_.setFreq( FREQ_3MM_IN_HZ );
            break;
        case OPTICAL:
            ephemeris_.setFreq( FREQ_550NM_IN_HZ ); 
            break;
        default:
            throw CARMA_EXCEPTION( IllegalArgumentException, "Bad Enum." );
            break;
    }
    refractionModel_ = model;
}

void
DriveEngine::SourcePosition::writeMonitorData( )
{
    // Write weather data
    OvroSubsystem::Weather & weather = mon_.drive( ).weather( ); 

    weather.ambientTemp( ).setValue( weather_.ambientTempInC );
    weather.dewpointTemp( ).setValue( weather_.dewPointInC );
    weather.barometricPressure( ).setValue( weather_.barometricPressureInMbar );
    weather.windSpeed( ).setValue( weather_.windSpeedInMPH );
    weather.windDirection( ).setValue( weather_.windDirectionInDeg );

    // Who would ever guess, but we need to set common calibration mps here too!
    const double ABS_ZERO_IN_C = Physical::ABS_ZERO; // -273.15
    mon_.antennaCommon( ).calibrator( ).ambTemp( ).setValue( 
        weather_.ambientTempInC - ABS_ZERO_IN_C );
    mon_.antennaCommon( ).calibrator( ).skyTemp( ).setValue(
        0.94 * ( weather_.ambientTempInC - ABS_ZERO_IN_C ) );

    // Set location
    const Location & loc = ephemeris_.getLocation();
    AntennaCommon::Location & monLoc = mon_.antennaCommon().location();

    monLoc.latitude( ).setValue( loc.getLatitude().degrees() );
    monLoc.longitude( ).setValue( loc.getLongitude().degrees() );
    monLoc.altitude( ).setValue( loc.getAltitude().meters() );

    // Write pointing data
    OvroSubsystem::Point & pointing = mon_.drive( ).point( );
    
    const double refractionInArcMin = ephemeris_.getRefrac( ) * ARCMIN_PER_DEG;
    pointing.refraction( ).setValue( refractionInArcMin );
    commonDrive_.point( ).refraction( ).setValue( refractionInArcMin ); 

    typedef OvroSubsystem::RefractionModelMonitorPointEnum RefractionModelMPE;
    typedef AntennaCommon::RefractionModelMonitorPointEnum CommonRefractionEnum;
    switch ( refractionModel_ ) {
        case OPTICAL:
            pointing.refractionModel( ).setValue( RefractionModelMPE::OPTICAL );
            commonDrive_.point( ).refractionModel( ).setValue(
                CommonRefractionEnum::OPTICAL );
            break;
        case RADIO:
            pointing.refractionModel( ).setValue( RefractionModelMPE::RADIO );
            commonDrive_.point( ).refractionModel( ).setValue(
                CommonRefractionEnum::RADIO );
            break;
    } 
    
    const double now = Time::MJD( );
    const double raInRad = raInterpolator_.evaluate( now );
    const double decInRad = decInterpolator_.evaluate(now );

    OvroSubsystem::Drive & drive = mon_.drive( );
    drive.rightAscension( ).setValue( raInRad );
    drive.declination( ).setValue( decInRad );
    drive.sourcename( ).setValue( sourceName_ );
    
    AntennaCommon::Drive & commonDrive = mon_.antennaCommon( ).drive( );
    commonDrive.rightAscension( ).setValue( raInRad ); 
    commonDrive.declination( ).setValue( decInRad );
    commonDrive.sourcename( ).setValue( sourceName_ );
    
    drive.track().wrapLogic( ).setValue( wrapToOvroMonitorWrap( wrap_ ) );
    commonDrive.track( ).wrapLogic( ).setValue( 
        wrapToCommonMonitorWrap( wrap_ ) );

} // DriveEngine::SourcePosition::writeMonitorData
    
AzElPair
DriveEngine::SourcePosition::calculateSnowTrackPosition( 
    const AzElPair & currentPosition ) const
{
    // When tracking snow, we track INTO the wind at a 60 degree azimuth offset
    // in order to prevent snow buildup on the BACK of the dish structure 
    // where it is more difficult to remove. 
    
    // Note that the wind vector is the direction the wind is coming from.  

    const Angle el( 20.0, Angle::DEGREES_STR );
    const Angle snowOffset( 60.0, Angle::DEGREES_STR );
    
    // Find the nearest valid position with a 60 degree offset to the wind. 
    // No preference is given to which direction the 60 degree offset is 
    // applied.
    // The algorithm is as follows:
    // a) Add a wrap to current wind value which is in the range [0-360].
    // b) Loop over successively smaller wraps, finding the smallest distance
    //    between the current position and potential offseted wind vectors.
    // c) The new direction is the closest valid offset.
    
    // Create windAz Angle object and add a wrap to it
    Angle windAz( weather_.windDirectionInDeg + 360.0, Angle::DEGREES_STR ); 
    Angle closestAngle( 4.0 * M_PI, Angle::RADIANS_STR );

    // Find the nearest valid wind position by iteratively subtracting wraps 
    // and testing for valid positions.
    // TODO: It is fairly easy to calculate the number of rewraps to perform.
    for ( int i = 0; i < 3 ; ++i ) {
        
        // Test wind + 60 degrees
        Angle high = windAz + snowOffset;
        Angle diff = high - currentPosition.azimuth;
        if ( !limits_.azWillTriggerSwLimit( high ) &&
             abs( diff.degrees( ) ) < abs( closestAngle.degrees( ) ) ) {
            closestAngle = diff;
        }
         
        // Test wind - 60 degrees
        Angle low = windAz - snowOffset;
        diff = low - currentPosition.azimuth;
        if ( !limits_.azWillTriggerSwLimit( low ) &&
             abs( diff.degrees( ) ) < abs( closestAngle.degrees( ) ) ) {
            closestAngle = diff;
        }
        
        // Rewrap 
        windAz -= Angle( 2.0 * M_PI, Angle::RADIANS_STR );
    }

    return AzElPair( currentPosition.azimuth + closestAngle, el );

} // DriveEngine::SourcePosition::calculateSnowTrackPosition( )

class DriveEngine::Acquisition {
public:

    explicit Acquisition( OvroSubsystem & mon );

    /** 
     * Set the acquisition tolerance and force reacquisition. 
     */
    void setToleranceAndReacquire( const Angle & tolerance );

    /**
     * Is source close to acquisition?
     * A source is close if the acquisition error is less than the tolerance
     * for a small amount of time (e.g. 1 frame).  If the source acquisition
     * error remains less than the threshold for subsequent frames, it will
     * become 'acquired'.
     */
    bool isSourceClose( ) const;

    /**
     * Is source acquired?
     * Source acquisition error has remained under the tolerance for at least
     * two consecutive frames.
     */
    bool isSourceAcquired( ) const;

    /**
     * Is az and/or el independently close?
     * Here close means within one to one half of the tracking threshold.
     * @return pair< bool, bool > Booleans for az (first) and el (second).
     */
    pair< bool, bool > isAzElClose( ) const;
    
    /**
     * Is az and/or el tracking?
     * Here tracking means less than one half of the tracking threshold.
     * @return pair< bool, bool > Booleans for az (first) and el (second).
     */
    pair< bool, bool > isAzElTracking( ) const;

    /**
     * Update acquisition state.
     */
    void updateAcquisitionState( const AzElPair & actualPosition,
                                 const AzElPair & requestedPosition ); 

    /**
     * Cache next sequence number pending reacquire.
     * Drive sequence numbers are sent down by the control system in order
     * to synchronize drive commands.  When called, the next sequence # 
     * is cached until 1) reacquire is called and 2) the source is reacquired
     * or an error occurs (e.g. a hw limit is reached), after which the 
     * cached sequence number is reported back in the monitor system.
     * @see reacquire
     */
    void 
    cacheNextSequenceNumberPendingReacquisition( unsigned long nextSeqNo );

    /**
     * Force source reacquisition.
     */
    void reacquire( );

    /**
     * Cancel acquisition if pending.
     * This is needed in order to release cached sequence numbers upon errors
     * such as reaching hw or sw limits.
     */
    void cancelAcquisitionIfPending( );

    void writeMonitorData( );

private:

    bool sourceWithinTolerance_;
    unsigned long updatesWithinToleranceCount_;
    pair< bool, bool > azElSourceClose_;
    pair< bool, bool > azElSourceAcquired_;
    Angle acquisitionTolerance_;
    unsigned long nextSequenceNumber_;
    unsigned long currentSequenceNumber_;
    bool pendingCallToReacquire_; // Are we pending a call to the reacquire()? 
    bool sequenceNumberSuccess_;
    OvroSubsystem & mon_;

}; // class DriveEngine::Acquisition

DriveEngine::Acquisition::Acquisition( OvroSubsystem & mon ) :
    sourceWithinTolerance_( false ),
    updatesWithinToleranceCount_( 0 ),
    azElSourceClose_( false, false ),
    azElSourceAcquired_( false, false ),
    acquisitionTolerance_( 5.0, Angle::ARCSEC_STR ),
    nextSequenceNumber_( 0 ),
    currentSequenceNumber_( 0 ),
    pendingCallToReacquire_( false ),
    sequenceNumberSuccess_( true ),
    mon_( mon )
{

}

void
DriveEngine::Acquisition::setToleranceAndReacquire( const Angle & tolerance )
{
    acquisitionTolerance_ = Angle( ::fabs( tolerance.radians( false ) ),
                                    Angle::RADIANS_STR );
    reacquire( ); // Always reacquire
}

bool
DriveEngine::Acquisition::isSourceClose( ) const
{
    return ( sourceWithinTolerance_ && 
           ( updatesWithinToleranceCount_ < MIN_UPDATES_FOR_ACQUIRE ) );
}

bool
DriveEngine::Acquisition::isSourceAcquired( ) const
{ 
    return ( sourceWithinTolerance_ && 
           ( updatesWithinToleranceCount_ >= MIN_UPDATES_FOR_ACQUIRE ) );
}

pair< bool, bool >
DriveEngine::Acquisition::isAzElClose( ) const
{
    return azElSourceClose_;
}

pair< bool, bool >
DriveEngine::Acquisition::isAzElTracking( ) const
{
    return azElSourceAcquired_;
}
    
void
DriveEngine::Acquisition::updateAcquisitionState( 
    const AzElPair & actualPosition,
    const AzElPair & requestedPosition )
{
    const bool noMod2PI( false );

    const AzElPair error( 
        actualPosition.azimuth - requestedPosition.azimuth,
        actualPosition.elevation - requestedPosition.elevation );

    const Angle errorAzSky = error.azimuth * cos( actualPosition.elevation );

    // Calculate acquisition error using hypot in small angle case
    const Angle acqError( ::hypot( fabs( errorAzSky.radians( noMod2PI ) ),
                                   fabs( error.elevation.radians( noMod2PI ) ) ), 
                          Angle::RADIANS_STR );

    // Is it less than the tolerance
    if ( acqError < acquisitionTolerance_ ) {
        sourceWithinTolerance_ = true;
        ++updatesWithinToleranceCount_;
    } else {
        sourceWithinTolerance_ = false;
        updatesWithinToleranceCount_ = 0;
    }
        
    // Update per axis track tolerance.
    const Angle axisTrackTolerance = 0.5 * acquisitionTolerance_;
    const bool azSourceAcquired = 
        ::fabs( errorAzSky.radians( noMod2PI ) ) < 
        axisTrackTolerance.radians( noMod2PI );
    const bool elSourceAcquired = 
        ::fabs( error.elevation.radians( noMod2PI ) ) < 
        axisTrackTolerance.radians( noMod2PI );
    const bool azSourceClose = ( azSourceAcquired ? false : 
        ::fabs( errorAzSky.radians( noMod2PI ) ) < 
        acquisitionTolerance_.radians( noMod2PI ) );
    const bool elSourceClose = ( elSourceAcquired ? false : 
        ::fabs( error.elevation.radians( noMod2PI ) ) < 
        acquisitionTolerance_.radians( noMod2PI ) );
    azElSourceClose_ = make_pair( azSourceClose, elSourceClose );
    azElSourceAcquired_ = make_pair( azSourceAcquired, elSourceAcquired );

    CARMA_CPTRACE( TRACE_ACQUISITION, "Az close " << azSourceClose 
        << ", errorAzSky.radians()=" << errorAzSky.radians( noMod2PI ) 
        << ", aquisitionTolerance.radians()=" 
        << acquisitionTolerance_.radians( noMod2PI ) );

    if ( !pendingCallToReacquire_ && isSourceAcquired( ) &&
          nextSequenceNumber_ != currentSequenceNumber_ ) {
        currentSequenceNumber_ = nextSequenceNumber_;
        sequenceNumberSuccess_ = true;
    }
}

void 
DriveEngine::Acquisition::cacheNextSequenceNumberPendingReacquisition( 
    unsigned long nextSeqNo ) 
{
    pendingCallToReacquire_ = true; // Now waiting for call to reacquire().
    nextSequenceNumber_ = nextSeqNo;
}

void 
DriveEngine::Acquisition::reacquire( )
{
    if ( !pendingCallToReacquire_ ) {
        // This will probably be common enough that we will want to remove
        // it at sometime in the future.  For now though it may catch control
        // system bugs where a move command of some sort hasn't been coupled
        // with a sequence #.
        programLogWarnIfPossible( 
            "Acquisition::reacquire( ) - Reacquire called but we're not "
            "pending reacquisition." );
    } else { // We are pending a call to reacquire()
        if ( nextSequenceNumber_ == currentSequenceNumber_ ) {
            programLogErrorIfPossible( 
                "Acquisition::acquire( ) - Reacquire is pending but the "
                "next sequence number is identical to the current (i.e. "
                "either the 'next' sequence # didn't get set, the current "
                "sequence # was prematurely set to the next or the "
                "pendingReacquire boolean was set incorrectly.");
        }
    }
    pendingCallToReacquire_ = false;
    sourceWithinTolerance_ = false;
    updatesWithinToleranceCount_ = 0;
    azElSourceClose_ = make_pair( false, false );
    azElSourceAcquired_ = make_pair( false, false );
}

void 
DriveEngine::Acquisition::cancelAcquisitionIfPending( )
{
    // Cancel any pending reacquisition and set the current
    // sequence number to the cached sequence number so that
    // it will be written to the monitor stream.  This will 
    // unblock commands waiting for acquisition.
    if ( nextSequenceNumber_ != currentSequenceNumber_ ) {
        // An acquisition is pending
        pendingCallToReacquire_ = false; 
        currentSequenceNumber_ = nextSequenceNumber_;
        sequenceNumberSuccess_ = false;
    }
}
    
void
DriveEngine::Acquisition::writeMonitorData( )
{
    AntennaCommon::Drive & comDrive = mon_.antennaCommon( ).drive( );

    comDrive.track( ).trackTolerance( ).setValue( 
        acquisitionTolerance_.arcSeconds( false ) );
    
    mon_.drive( ).sequenceNo( ).setValue( currentSequenceNumber_ );
    comDrive.driveSeqNum( ).setValue( currentSequenceNumber_ );

    mon_.drive( ).sequenceNoSuccess( ).setValue( sequenceNumberSuccess_ );
    comDrive.driveSeqNumSuccess( ).setValue( sequenceNumberSuccess_ );
}

class DriveEngine::Tracking {
public:

    explicit Tracking( Drive & drive, 
                    DriveEngine::Servo & azServo,
                    DriveEngine::Servo & elServo,
                    DriveEngine::Limits & limits,
                    OvroSubsystem & mon );

    ~Tracking( );

    void
    setAz( const Angle & azimuth );

    void setAzOffset( const Angle & offset );
    
    void setEl( const Angle & elevation );
    void setElOffset( const Angle & offset );
    void setAzEl( const AzElPair & azElPosition );
    AzElPair getRequestedPositionWithOffsets( ) const;
    void updateRatesAndErrors(const AzElPair& actualPosition,
                              const AzElPair& rawPosition );
    void setNewAzElDriveRates( const AzElPair & currentPosition );
    void writeMonitorData( );
    bool impossibleRateDetected() const;

private:
    AzElPair requestedPosition_; // Requested position sans offsets.
    AzElPair requestedPositionWithOffsets_;  // Requested position with offsets
    AzElPair actualPosition_;
    AzElPair oldActualPosition_;
    AzElPair rawPosition_;  // Straight from the encoders, no corrections
    AzElPair oldRawPosition_;
    bool firstRun_; // Needed to properly initialize actual and old position.
    AzElPair azElOffsetOnTheSky_;
    AzElPair encoderErrors_;
    double errorOnSkyInArcSec_;
    Angle azErrorOnTheSky_;
    deque<double> azErrors_;
    deque<double> elErrors_;
    double rmsAzError_;
    double rmsElError_;
    double azActualRateInDegPerMin;
    double elActualRateInDegPerMin;
    bool impossibleAzRateDetected_;
    bool impossibleElRateDetected_;
    bool firstUpdate_;

    Drive & drive_;
    DriveEngine::Servo & azServo_; 
    DriveEngine::Servo & elServo_; 
    DriveEngine::Limits & limits_;
    OvroSubsystem & mon_;
}; // class DriveEngine::Tracking
    
DriveEngine::Tracking::Tracking( Drive & drive,
                           DriveEngine::Servo & azServo,
                           DriveEngine::Servo & elServo, 
                           DriveEngine::Limits & limits,
                           OvroSubsystem & mon ) :
    requestedPosition_( ),
    actualPosition_( ),
    oldActualPosition_( ),
    rawPosition_(),
    oldRawPosition_(),
    firstRun_( true ),
    errorOnSkyInArcSec_( 0.0 ),
    rmsAzError_( 0.0 ),
    rmsElError_( 0.0 ),
    azActualRateInDegPerMin( 0.0 ),
    elActualRateInDegPerMin( 0.0 ),
    impossibleAzRateDetected_( false ),
    impossibleElRateDetected_( false ),
    firstUpdate_( true ),
    drive_( drive ),
    azServo_( azServo ),
    elServo_( elServo ),
    limits_( limits ),
    mon_( mon )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "DriveEngine::Tracking - Constructor." );
}

DriveEngine::Tracking::~Tracking( )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, 
                   "DriveEngine::Tracking - Destructor pre thread shutdown." );

    CARMA_CPTRACE( TRACE_CTOR_DTOR, 
                   "DriveEngine::Tracking - Destructor post thread shutdown." );
}

void 
DriveEngine::Tracking::setAz( const Angle & azimuth )
{
    requestedPosition_.azimuth = azimuth;
}

void
DriveEngine::Tracking::setAzOffset( const Angle & offset )
{
    azElOffsetOnTheSky_.azimuth = offset;
}

void
DriveEngine::Tracking::setEl( const Angle & elevation )
{
    requestedPosition_.elevation = elevation;
}

void
DriveEngine::Tracking::setElOffset( const Angle & offset ) 
{
    azElOffsetOnTheSky_.elevation = offset;
}

void
DriveEngine::Tracking::setAzEl( const AzElPair & azElPosition )
{
    requestedPosition_ = azElPosition;
}

AzElPair
DriveEngine::Tracking::getRequestedPositionWithOffsets( ) const
{
    return requestedPositionWithOffsets_;
}
    
void 
DriveEngine::Tracking::setNewAzElDriveRates( 
    const AzElPair & currentPosition )
{
    AzElPair localRequestedPosition = requestedPosition_;

    const double cosel = cosEl( requestedPosition_.elevation );

    // Add in az/el offsets
    localRequestedPosition.azimuth += azElOffsetOnTheSky_.azimuth / cosel;

    localRequestedPosition.elevation += azElOffsetOnTheSky_.elevation;

    limits_.bindPositionToLimits( localRequestedPosition );

    const double azRate = azServo_.calculateRequestedRate( 
        localRequestedPosition.azimuth.radians( false ),
        currentPosition.azimuth.radians( false ) );

    const double elRate = elServo_.calculateRequestedRate( 
        localRequestedPosition.elevation.radians( false ), 
        currentPosition.elevation.radians( false ) );

    CARMA_CPTRACE( Trace::TRACE1, "Position=( " 
        << currentPosition.azimuth.degrees( ) << ", " 
        << currentPosition.elevation.degrees( ) << " ) degrees from a "
        << "requested rate of ( " << azRate << ", " 
        << elRate << " )." );

    drive_.setAzElDriveRates( azRate, 
                              elRate, 
                              currentPosition.elevation );

    requestedPositionWithOffsets_ = localRequestedPosition;
}
    
void
DriveEngine::Tracking::updateRatesAndErrors( 
    const AzElPair& actualPosition,  const AzElPair& rawPosition)
{
    if ( firstRun_ ) {
        // We don't have an old position so just use the actual again.
        oldActualPosition_ = actualPosition;
        oldRawPosition_    = rawPosition;
        firstRun_ = false;
    } else {
        oldActualPosition_ = actualPosition_;
        oldRawPosition_    = rawPosition_;
    } 
    actualPosition_ = actualPosition;
    rawPosition_    = rawPosition;
    
    // Actual rates are based on uncorrected (raw) positions
    double diff = rawPosition_.azimuth.degrees() - 
                        oldRawPosition_.azimuth.degrees( );
    if (diff >  180.0) diff -= 360;
    if (diff < -180.0) diff += 360;     
    azActualRateInDegPerMin = diff/(UPDATE_PERIOD_IN_SEC/60.0);
    
    diff = rawPosition_.elevation.degrees() - 
                 oldRawPosition_.elevation.degrees( );
    if (diff >  180.0) diff -= 360;
    if (diff < -180.0) diff += 360;     
    elActualRateInDegPerMin = diff/(UPDATE_PERIOD_IN_SEC/60.0);
    
    if ( firstUpdate_ ) {
        firstUpdate_ = false;
    } else {
        // Unfortunately while slewing pointing corrections can be large
        // enough to produce perceived actual rates which are much larger than
        // the physically possible rate of the dish, hence the large comparison
        // factor to prevent false detections.
        if ( ::fabs( azActualRateInDegPerMin ) > 
                10.0 * MAX_AZ_RATE_IN_DEG_PER_MIN ) {
            ostringstream msg;
            msg << "DriveEngine::Tracking::updateRatesAndErrors() - "
                << "Impossible azimuth rate of " 
                <<fixed << setprecision(1) << azActualRateInDegPerMin 
                << " deg/min detected - FATAL!";

            programLogErrorIfPossible( msg.str( ) );
            impossibleAzRateDetected_ = true;
        }

        if ( ::fabs( elActualRateInDegPerMin ) >
             3.0 * MAX_EL_RATE_IN_DEG_PER_MIN ) {
            ostringstream msg;
            msg << "DriveEngine::Tracking::updateRatesAndErrors() - "
                << "Impossible elevation rate of " 
                <<fixed << setprecision(1)  << elActualRateInDegPerMin
                << " deg/min detected - FATAL!";

            programLogErrorIfPossible( msg.str( ) );
            impossibleElRateDetected_ = true;
        }
    }
    
    encoderErrors_.azimuth = 
        actualPosition_.azimuth - requestedPositionWithOffsets_.azimuth;

    encoderErrors_.elevation = 
        actualPosition_.elevation - requestedPositionWithOffsets_.elevation; 
    
    azErrorOnTheSky_ = 
        encoderErrors_.azimuth * cos( actualPosition_.elevation );
    
    errorOnSkyInArcSec_ = hypot( azErrorOnTheSky_.arcSeconds( ),
                                 encoderErrors_.elevation.arcSeconds( ) );

    // Calculate rms errors.
    if ( azErrors_.size( ) == MAX_RMS_ERRORS ) {
        azErrors_.pop_front( );
    }

    if ( elErrors_.size( ) == MAX_RMS_ERRORS ) {
        elErrors_.pop_front( );
    }
    
    azErrors_.push_back( 
        azErrorOnTheSky_.arcSeconds( )  * azErrorOnTheSky_.arcSeconds( ) );
    elErrors_.push_back( encoderErrors_.elevation.arcSeconds( ) * 
                         encoderErrors_.elevation.arcSeconds( ) );

    double azSum = 0.0;
    const deque<double>::const_iterator iAzBegin = azErrors_.begin( );
    const deque<double>::const_iterator iAzEnd = azErrors_.end( );
    for ( deque<double>::const_iterator i = iAzBegin; i != iAzEnd; ++i ) {
        azSum += *i;
    }

    rmsAzError_ = ::sqrt( azSum / azErrors_.size( ) ); // size >= 1
    
    double elSum = 0.0;
    const deque<double>::const_iterator iElBegin = elErrors_.begin( );
    const deque<double>::const_iterator iElEnd = elErrors_.end( );
    for ( deque<double>::const_iterator i = iElBegin; i != iElEnd; ++i ) {
        elSum += *i;
    }

    rmsElError_ = ::sqrt( elSum / elErrors_.size( ) ); // size >= 1
}
    
void
DriveEngine::Tracking::writeMonitorData( ) 
{
    OvroSubsystem::Drive & drive = mon_.drive( );
    AntennaCommon::Drive & commonDrive = mon_.antennaCommon( ).drive( );

    // Write tracking data to 'tracking' monitor subcontainer.
    OvroSubsystem::Track & track = mon_.drive( ).track( ); 
    
    track.requestedAzimuth( ).setValue( 
        requestedPositionWithOffsets_.azimuth.degrees( false ) );
    commonDrive.track( ).requestedAzimuth( ).setValue(
        requestedPositionWithOffsets_.azimuth.degrees( false ) );

    track.requestedElevation( ).setValue( 
        requestedPositionWithOffsets_.elevation.degrees( false ) );
    commonDrive.track( ).requestedElevation( ).setValue(
        requestedPositionWithOffsets_.elevation.degrees( false ) );

    track.actualAzimuth( ).setValue( 
        actualPosition_.azimuth.degrees( false ) );
    commonDrive.track( ).actualAzimuth( ).setValue(
        actualPosition_.azimuth.degrees( false ) );

    track.actualElevation( ).setValue( 
        actualPosition_.elevation.degrees( false ) );
    commonDrive.track( ).actualElevation( ).setValue(
        actualPosition_.elevation.degrees( false ) );
        
    track.azimuthRate( ).setValue( azActualRateInDegPerMin );
    commonDrive.track( ).azimuthRate( ).setValue( azActualRateInDegPerMin );

    track.elevationRate( ).setValue( elActualRateInDegPerMin );
    commonDrive.track( ).elevationRate( ).setValue( elActualRateInDegPerMin );

    track.errorAzimuth( ).setValue( encoderErrors_.azimuth.arcSeconds( ) );
    commonDrive.track( ).errorAzimuth( ).setValue( 
        encoderErrors_.azimuth.arcSeconds( ) );

    track.errorElevation( ).setValue( encoderErrors_.elevation.arcSeconds( ) );
    commonDrive.track( ).errorElevation( ).setValue( 
        encoderErrors_.elevation.arcSeconds( ) );

    track.errorAzimuthSky( ).setValue( azErrorOnTheSky_.arcSeconds( ) );
    commonDrive.track( ).errorAzimuthSky( ).setValue( 
        azErrorOnTheSky_.arcSeconds( ) );

    drive.errorSky( ).setValue( errorOnSkyInArcSec_ );
    commonDrive.errorSky( ).setValue( errorOnSkyInArcSec_ );

    track.rmsErrorAzimuth( ).setValue( rmsAzError_ );
    track.rmsErrorElevation( ).setValue( rmsElError_ );

    // Write more 'pointing' data ( this stuff is spread all over the place ).
    OvroSubsystem::Point & pointing = mon_.drive( ).point( );
    pointing.offsetAz( ).setValue( azElOffsetOnTheSky_.azimuth.arcMinutes( ) );
    pointing.offsetEl( ).setValue( azElOffsetOnTheSky_.elevation.arcMinutes( ));

    commonDrive.point( ).offsetAz( ).setValue( 
        azElOffsetOnTheSky_.azimuth.arcMinutes( ) );
    commonDrive.point( ).offsetEl( ).setValue(
        azElOffsetOnTheSky_.elevation.arcMinutes( ) );

    OvroSubsystem::System & system = mon_.drive( ).system( );
    system.azServoBias( ).setValue( azServo_.getServoBiasInArcMinutes( ) );
    system.elServoBias( ).setValue( elServo_.getServoBiasInArcMinutes( ) );
}

bool 
DriveEngine::Tracking::impossibleRateDetected( ) const
{
    return impossibleAzRateDetected_ || impossibleElRateDetected_;
}

/** 
 * Drive state class.
 * Keeps track of state information such as drive mode, sequence #, etc.
 */
class DriveEngine::DriveState {
public:
    
    explicit DriveState( DriveEngine::Acquisition & acquisition,
                         DriveEngine::Limits & limits,
                         carma::antenna::ovro::Drive & drive,
                         DriveEngine::SourcePosition & sourcePosition,
                         DriveEngine::Tracking & tracking,
                         carma::monitor::OvroSubsystem & mon );

    ~DriveState( );

    enum ModeType {
        STOW_SAFE,
        STOW_ZENITH,
        STOWED,
        STOP,
        SNOW,
        AZ_EL,        // Az/El coordinates (referenced WRT horizon and north).
        CELESTIAL,    // Ra/Dec
        OPEN_VOLTAGE, // Open loop voltage  
        OPEN_RATE,    // Open loop rates
    };

    void setMode( enum ModeType mode );
    void updateDriveState(const AzElPair& actualPosition, 
                          const AzElPair& rawPosition);
    enum ModeType getDriveMode() const;
    void writeMonitorData();
    void setDriveStateToError();
    void setAzOverlapFailure();
    void setImpossibleRate();
    void setForbiddenAz();
    void setControllerOvertemp();
    monitor::OvroSubsystem::DriveStateMonitorPointEnum::DRIVESTATE
        getDriveState();

private:

    OvroDriveMode::DRIVEMODE modeTypeAsOvroDriveMode( ModeType mode );

    enum ModeType driveMode_;
    monitor::OvroSubsystem::DriveStateMonitorPointEnum::DRIVESTATE driveState_;
    monitor::OvroSubsystem & mon_;
    SourcePosition & sourcePosition_;
    Tracking & tracking_;
    Acquisition & acquisition_;
    Limits & limits_;
    carma::antenna::ovro::Drive & drive_;
    bool azOverlapFailure_;
    bool impossibleRate_;
    bool forbiddenAz_;
    bool controllerOvertemp_;

}; // class DriveEngine::DriveState

DriveEngine::DriveState::DriveState( 
        DriveEngine::Acquisition & acquisition,
        DriveEngine::Limits & limits,
        carma::antenna::ovro::Drive & drive,
        DriveEngine::SourcePosition & sourcePosition,
        DriveEngine::Tracking & tracking,
        carma::monitor::OvroSubsystem & mon ) :
    driveMode_( STOP ),
    driveState_( OvroSubsystem::DriveStateMonitorPointEnum::STOP ),
    mon_( mon ),
    sourcePosition_( sourcePosition ),
    tracking_( tracking ),
    acquisition_( acquisition ),
    limits_( limits ),
    drive_( drive ),
    azOverlapFailure_(false),
    impossibleRate_(false),
    forbiddenAz_(false),
    controllerOvertemp_(false)
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, 
                   "DriveEngine::DriveState::DriveState - Constructor." );
}

DriveEngine::DriveState::~DriveState( ) 
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, 
                   "DriveEngine::DriveState::~DriveState - Destructor.");
}

void
DriveEngine::DriveState::setMode( const enum ModeType mode ) 
{
    switch ( mode ) {
        case STOW_ZENITH:
            tracking_.setAzEl( limits_.getZenithStowPosition( ) );
            sourcePosition_.setSourceName( "zenith" );
            sourcePosition_.setRaDec( Time::MJD( ) , 0.0, 0.0, 
                                      true, DriveEngine::NO_TURN );
            break;
        case STOW_SAFE:
            tracking_.setAzEl( limits_.getSafeStowPosition( ) );
            sourcePosition_.setSourceName( "safe" );
            sourcePosition_.setRaDec( Time::MJD( ) , 0.0, 0.0, 
                                      true, DriveEngine::NO_TURN);
            break;
        case STOWED:
            break; 
        case STOP:
            drive_.stop( ); // Stop immediately (don't wait for next update).
            sourcePosition_.setSourceName( "none" );
            sourcePosition_.setRaDec( Time::MJD( ) , 0.0, 0.0,
                                      true, DriveEngine::NO_TURN);
            break;
        case SNOW:
            sourcePosition_.setSourceName( "snow" );
            sourcePosition_.setRaDec( Time::MJD( ) , 0.0, 0.0, 
                                      true, DriveEngine::NO_TURN);
            break;
        case AZ_EL:
            sourcePosition_.setSourceName( "azel" );
            sourcePosition_.setRaDec( Time::MJD( ) , 0.0, 0.0, 
                                      true, DriveEngine::NO_TURN);
            break;
        case CELESTIAL:
            break;
        case OPEN_VOLTAGE:
        case OPEN_RATE:
            sourcePosition_.setSourceName( "none" );
            acquisition_.cancelAcquisitionIfPending( );
            sourcePosition_.setRaDec( Time::MJD( ) , 0.0, 0.0, 
                                      true, DriveEngine::NO_TURN);
            break;
        default:
            break;
    }

    driveMode_ = mode;
}

void 
DriveEngine::DriveState::updateDriveState(const AzElPair& actualPosition,
                                          const AzElPair& rawPosition )
{
    // Update dependent states first.
    limits_.updateLimitState( actualPosition );
    acquisition_.updateAcquisitionState( 
        actualPosition,
        tracking_.getRequestedPositionWithOffsets( ) );
    tracking_.updateRatesAndErrors(actualPosition, rawPosition);

    const bool acquired = acquisition_.isSourceAcquired( );
    const bool close = acquisition_.isSourceClose( );
    const Drive::BaseSwitchState baseSwitchState = drive_.getBaseSwitchState( );

    // Set the drive state - note we cancel any pending acquisition if an
    // error condition occurs, this assures release of a cached sequence #.
    if ( drive_.isAtHwLimit( ) ) {
        driveState_ = OvroDriveState::HWLIMIT;
        acquisition_.cancelAcquisitionIfPending( );
        drive_.stop( ); // Without this, we automatically drive off of limits.
    }
    else if ( baseSwitchState == Drive::LOCAL_CONTROL ) {
        driveState_ = OvroDriveState::LOCAL;
        acquisition_.cancelAcquisitionIfPending( );
    }
    else if ( baseSwitchState == Drive::OFF ) {
        driveState_ = OvroDriveState::DISABLE;
        acquisition_.cancelAcquisitionIfPending( );
    }
    else if ( limits_.isAtSwLimit( ) ) {
        driveState_ = OvroDriveState::SWLIMIT;
        acquisition_.cancelAcquisitionIfPending( );
    } 
    else if ( drive_.isInEngineeringMode( ) ) {
        driveState_ = OvroDriveState::TEST;
        acquisition_.cancelAcquisitionIfPending( );
    }
    else if ( drive_.getState( ) != carma::canbus::SIMULATED &&
              drive_.getState( ) != carma::canbus::ONLINE )  
    {
        // If the module is offline, this is all for nothing...
        driveState_ = OvroDriveState::ERROR;
        acquisition_.cancelAcquisitionIfPending( );
    }
    else if ( driveMode_ == AZ_EL || 
              driveMode_ == CELESTIAL || 
              driveMode_ == SNOW )
    {
        if ( acquired ) driveState_ = OvroDriveState::TRACK;
        else if ( close ) driveState_ = OvroDriveState::CLOSE;
        else driveState_ = OvroDriveState::SLEW;
    }
    else if ( driveMode_ == STOP ) {
        if ( !drive_.isDriveEnabled( ) ) {
            driveState_ = OvroDriveState::STOP;
        } else {
            // The drives take several seconds to stop and disable themselves
            // thus prior to disabling the drive the state is the same as 
            // before (i.e.) driveState_ = driveState_;
        }
        acquisition_.cancelAcquisitionIfPending( );
    } 
    else if ( driveMode_ == STOWED ) {
        if ( drive_.isDriveEnabled( ) ) {
            programLogWarnIfPossible( "Dish is STOWED but drive is enabled. "
                "This is normal for 1-2 frames while the drives time-out. "
                "Continuous occurrences signal an error." );
        } else {
            driveState_ = OvroDriveState::STOW;
        }
    }
    else if ( driveMode_ == STOW_SAFE ||
              driveMode_ == STOW_ZENITH )
    {
        driveState_ = OvroDriveState::SLEW;
    }
    else if ( driveMode_ == OPEN_VOLTAGE ||
              driveMode_ == OPEN_RATE ) 
    {
        driveState_ = OvroDriveState::TEST;
    }
    else {
        driveState_ = OvroDriveState::ERROR;
        acquisition_.cancelAcquisitionIfPending( );
    }
}

enum DriveEngine::DriveState::ModeType 
DriveEngine::DriveState::getDriveMode( ) const
{
    return driveMode_;
}

OvroDriveMode::DRIVEMODE 
DriveEngine::DriveState::modeTypeAsOvroDriveMode( const ModeType mode )
{
    switch ( mode ) {
        case STOW_ZENITH: return OvroDriveMode::STOW;
        case STOW_SAFE: return OvroDriveMode::STOW;
        case STOWED: return OvroDriveMode::STOW;
        case STOP: return OvroDriveMode::STOP;
        case SNOW: return OvroDriveMode::SNOW;
        case AZ_EL: return OvroDriveMode::AZEL;
        case CELESTIAL: return OvroDriveMode::EQUAT;
        case OPEN_VOLTAGE: return OvroDriveMode::TEST; 
        case OPEN_RATE: return OvroDriveMode::TEST; 
        default: throw CARMA_EXCEPTION( IllegalArgumentException, "Bad Enum." );
    }
}
    
void
DriveEngine::DriveState::writeMonitorData( )
{
    OvroSubsystem::Drive & drive = mon_.drive( );
    AntennaCommon::Drive & commonDrive = mon_.antennaCommon( ).drive( );
    OvroSubsystem::System & system = mon_.drive( ).system( );

    const CommonDriveState::STATE commonDriveState = 
        ovroDriveStateToCommonDriveState( driveState_ );

    drive.driveState( ).setValue( driveState_ );
    commonDrive.state( ).setValue( commonDriveState );

    const OvroDriveMode::DRIVEMODE odm = modeTypeAsOvroDriveMode( driveMode_ );
    const CommonDriveMode::MODE cdm = ovroDriveModeToCommonDriveMode( odm );
    drive.driveMode( ).setValue( odm );
    commonDrive.mode( ).setValue( cdm );

    AntennaCommon::Track & commonTrack = commonDrive.track();

    // Set axis specific drive states. 
    typedef AntennaCommon::AzimuthAxisStateMonitorPointEnum AzAxisStateMPE;
    typedef AntennaCommon::ElevationAxisStateMonitorPointEnum ElAxisStateMPE;
    if ( cdm == CommonDriveMode::AZEL || cdm == CommonDriveMode::EQUAT ) {

        const pair< bool, bool > azElClose = acquisition_.isAzElClose( );
        const pair< bool, bool > azElTracking = acquisition_.isAzElTracking( );
        if ( azElTracking.first ) 
            commonTrack.azimuthAxisState().setValue( AzAxisStateMPE::TRACK );
        else if ( azElClose.first )
            commonTrack.azimuthAxisState().setValue( AzAxisStateMPE::CLOSE );
        else 
            commonTrack.azimuthAxisState().setValue( AzAxisStateMPE::SLEW );

        if ( azElTracking.second )
            commonTrack.elevationAxisState().setValue( ElAxisStateMPE::TRACK );
        else if ( azElClose.second )
            commonTrack.elevationAxisState().setValue( ElAxisStateMPE::CLOSE );
        else
            commonTrack.elevationAxisState().setValue( ElAxisStateMPE::SLEW );

    } else {
        commonTrack.azimuthAxisState().setValue( AzAxisStateMPE::IRRELEVANT );
        commonTrack.elevationAxisState().setValue( ElAxisStateMPE::IRRELEVANT );
    }

    OvroSubsystem::Track & track = drive.track( );

    const bool swLimit = limits_.isAtSwLimit( );
    const bool hwLimit = drive_.isAtHwLimit( ); 

    typedef OvroSubsystem::LimitMonitorPointEnum LimitEnum;

    if ( swLimit && hwLimit ) {
        track.limit( ).setValue( LimitEnum::HWSW );
    } else if ( swLimit ) {
        track.limit( ).setValue( LimitEnum::SW );
    } else if ( hwLimit ) {
        track.limit( ).setValue( LimitEnum::HW );
    } else {
        track.limit( ).setValue( LimitEnum::OK );
    }

    // Theoretically OVRO antennas are always SAFE:
    commonDrive.safeState( ).setValue( 
        AntennaCommon::SafeStateMonitorPointEnum::SAFE );

    system.azOverlapSwitchFail().setValue(azOverlapFailure_);
    system.impossibleRate().setValue(impossibleRate_);
    system.forbiddenAz().setValue(forbiddenAz_);
}
    
void 
DriveEngine::DriveState::setDriveStateToError( )
{
    driveState_ = OvroDriveState::ERROR;
}
    
void 
DriveEngine::DriveState::setAzOverlapFailure( )
{
    driveState_       = OvroDriveState::FATAL;
    azOverlapFailure_ = true;
}

void 
DriveEngine::DriveState::setImpossibleRate( )
{
    driveState_     = OvroDriveState::FATAL;
    impossibleRate_ = true;
}

void 
DriveEngine::DriveState::setForbiddenAz( )
{
    driveState_  = OvroDriveState::FATAL;
    forbiddenAz_ = true;
}

void 
DriveEngine::DriveState::setControllerOvertemp( )
{
    driveState_         = OvroDriveState::FATAL;
    controllerOvertemp_ = true;
}

monitor::OvroSubsystem::DriveStateMonitorPointEnum::DRIVESTATE
    DriveEngine::DriveState::getDriveState() 
{
        return driveState_;
}

class DriveEngine::Timing {
public:    
    explicit Timing( const Encoder & azEncoder,
                     const Encoder & elEncoder,
                     OvroSubsystem & mon );

    /* virtual */ ~Timing( );

    void driveLoopWake( );
    void driveLoopDone( );
    void writeMonitorData( );

private:
    const Encoder & azEncoder_;
    const Encoder & elEncoder_;
    double loopWakeMJD_;
    double loopDoneMJD_;

    OvroSubsystem & mon_;
}; // Class DriveEngine::Timing

DriveEngine::Timing::Timing( const Encoder & azEncoder,
                             const Encoder & elEncoder,
                             OvroSubsystem & mon ) : 
    azEncoder_( azEncoder ),
    elEncoder_( elEncoder ),
    loopWakeMJD_( 0.0 ),
    loopDoneMJD_( 0.0 ),
    mon_( mon ) { }

DriveEngine::Timing::~Timing( ) { }

void 
DriveEngine::Timing::driveLoopWake( )
{
    loopWakeMJD_ = Time::MJD( );
}

void
DriveEngine::Timing::driveLoopDone( )
{
    loopDoneMJD_ = Time::MJD( );
}

void 
DriveEngine::Timing::writeMonitorData( )
{
    OvroSubsystem::System & system = mon_.drive( ).system( );

    system.loopWakeDelay( ).setValue( TIMER_OFFSET_NANOS / (1000000.0) );
    
    const double loopWakeActual = millisecondsSinceCurrentFrame( loopWakeMJD_ );
    system.loopWakeActual( ).setValue( loopWakeActual );

    const double loopDone = millisecondsSinceCurrentFrame( loopDoneMJD_ );  
    system.loopDone( ).setValue( loopDone );
}
    
struct DriveEngine::EngineeringInfo {

    EngineeringInfo( );

    double rawAzVoltage_;
    double rawElVoltage_;

    double rawAzRateInRadPerSec_;
    double rawElRateInRadPerSec_;
};

DriveEngine::EngineeringInfo::EngineeringInfo( ) :
    rawAzVoltage_( 0.0 ),
    rawElVoltage_( 0.0 ),
    rawAzRateInRadPerSec_( 0.0 ),
    rawElRateInRadPerSec_( 0.0 ) { }


DriveEngine::DriveEngine( carma::antenna::ovro::Drive & drive,
                          const Encoder & azEncoder, 
                          const Encoder & elEncoder,
                          const Tiltmeter & tiltmeter,
                          OvroSubsystem & mon ) : 
    drive_( drive ),
    acquisition_( new Acquisition( mon ) ),
    engineering_( new EngineeringInfo( ) ),
    limits_( new Limits( mon ) ),
    pointing_( new Pointing( drive, azEncoder, elEncoder, tiltmeter, mon ) ),
    azServo_( new Servo( MAX_AZ_RATE_IN_RAD_PER_SEC, CLIP, BACKLASH ) ),
    elServo_( new Servo( MAX_EL_RATE_IN_RAD_PER_SEC, CLIP, BACKLASH ) ), 
    timing_( new Timing( azEncoder, elEncoder, mon ) ),
    driveLoopThreadId_( 0 )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "DriveEngine::DriveEngine - Constructor." );

    sourcePosition_ = auto_ptr<SourcePosition>(
            new SourcePosition(mon, *limits_));

    tracking_ = auto_ptr<Tracking>( 
            new Tracking(drive, *azServo_, *elServo_, *limits_, mon ));

    driveState_ = auto_ptr< DriveEngine::DriveState >( 
        new DriveEngine::DriveState( *acquisition_, *limits_, drive, 
                                     *sourcePosition_, *tracking_, mon ) );

    pthread_mutex_init( &mutex_, 0 );

    // Kick off the driveLoop thread.
    driveLoopThreadId_ = StartPthreadWithRef(
        DriveEngine::driveLoopThreadEntry,
        ( *this ),
        "DriveEngine::driveLoopThread" );
}

DriveEngine::~DriveEngine( )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "DriveEngine::DriveEngine - Destructor." );
    
    try {
        AutoPthreadQuitAndJoinGroup autoquit;

        if ( driveLoopThreadId_ != 0 ) {
            autoquit.insert( driveLoopThreadId_ );
        }
    } catch ( ... ) {
        // Stifle
    }
}

void
DriveEngine::setAzEl( const Angle & azimuth,
                      const Angle & elevation )
{
    ScopeLock scopelock( mutex_ );

    tracking_->setAzEl( AzElPair( azimuth, elevation ) );
    driveState_->setMode( DriveState::AZ_EL );
    acquisition_->reacquire( ); // Force reacquire to return cached sequence #
}
    
void
DriveEngine::setAz( const Angle & azimuth )
{
    ScopeLock scopelock( mutex_ );
    
    tracking_->setAz( azimuth );
    driveState_->setMode( DriveState::AZ_EL );
    acquisition_->reacquire( ); // Force reacquire to return cached sequence #
}

void 
DriveEngine::setEl( const Angle & elevation )
{
    ScopeLock scopelock( mutex_ );
    
    tracking_->setEl( elevation );
    driveState_->setMode( DriveState::AZ_EL );
    acquisition_->reacquire( ); // Force reacquire to return cached sequence #
}

void
DriveEngine::setRawDriveVoltages( const float azVoltage, 
                                  const float elVoltage )
{
    ScopeLock scopelock( mutex_ );

    if ( limits_->isAtSwLimit( ) ) { // Refuse if we're at any software limit.
        throw CARMA_EXCEPTION( IllegalStateException, 
            "At sw limit - not changing to raw voltage (open loop) mode." );
    } else {
        engineering_->rawAzVoltage_ = azVoltage;
        engineering_->rawElVoltage_ = elVoltage;

        driveState_->setMode( DriveState::OPEN_VOLTAGE );
    }
}
        
void 
DriveEngine::setRawDriveRates( const float azRateInDegPerMin, 
                               const float elRateInDegPerMin )
{
    ScopeLock scopelock( mutex_ );

    if ( limits_->isAtSwLimit( ) ) { 
        throw CARMA_EXCEPTION( IllegalStateException, 
            "At sw limit - not changing to raw rate (open loop) mode." );
    } else {
        engineering_->rawAzRateInRadPerSec_ = 
            azRateInDegPerMin * RAD_PER_DEG / 60.0;
        engineering_->rawElRateInRadPerSec_ = 
            elRateInDegPerMin * RAD_PER_DEG / 60.0;

        driveState_->setMode( DriveState::OPEN_RATE );
    }
}

void
DriveEngine::setEngineeringMode( const bool enable ) 
{
    ScopeLock scopelock( mutex_ );
    
    drive_.setEngineeringMode( enable );
}

void
DriveEngine::setAzOffset( const Angle & offset )
{
    ScopeLock scopelock( mutex_ );

    tracking_->setAzOffset( offset );
    acquisition_->reacquire( ); // Force reacquire to return cached sequence #
}

void
DriveEngine::setElOffset( const Angle & offset )
{
    ScopeLock scopelock( mutex_ );

    tracking_->setElOffset( offset );
    acquisition_->reacquire( ); // Force reacquire to return cached sequence #
}

void
DriveEngine::setOffset( const Angle & azOffset,
                        const Angle & elOffset )
{
    ScopeLock scopelock( mutex_ );

    tracking_->setAzOffset( azOffset );
    tracking_->setElOffset( elOffset );
    acquisition_->reacquire( ); // Force reacquire to return cached sequence #
}

void
DriveEngine::setSource( const std::string source )
{
    ScopeLock scopelock( mutex_ );

    sourcePosition_->setSourceName( source );
}

void
DriveEngine::setAzMaxRate( const double azMaxRateInDegPerSec )
{
    if ( azMaxRateInDegPerSec < 0.0 ) {
        ostringstream errmsg;
        errmsg << "Az max rate must be positive.";
        throw CARMA_EXCEPTION( IllegalArgumentException, errmsg.str( ) );
    }
    
    ScopeLock scopelock( mutex_ );
        
    azServo_->setMaxEncoderRate( azMaxRateInDegPerSec * RAD_PER_DEG );
}

void
DriveEngine::setElMaxRate( const double elMaxRateInDegPerSec )
{
    if ( elMaxRateInDegPerSec < 0.0 ) {
        ostringstream errmsg;
        errmsg << "El max rate must be positive.";
        throw CARMA_EXCEPTION( IllegalArgumentException, errmsg.str( ) );
    }
    
    ScopeLock scopelock( mutex_ );
        
    elServo_->setMaxEncoderRate( elMaxRateInDegPerSec * RAD_PER_DEG );
}

void
DriveEngine::setRaDec( const RaDecTriplet & raDecTriplet,
                       const WrapType wrap )
{
    bool discontinuity = true; // 1st value is discontinuous, 2nd and 3rd aren't

    ScopeLock scopelock( mutex_ );

    const RaDecTripletConstIter pBegin = raDecTriplet.begin();
    const RaDecTripletConstIter pEnd = raDecTriplet.end();
    for ( RaDecTripletConstIter p = pBegin; p != pEnd; ++p ) {
        sourcePosition_->setRaDec(  p->mjd, 
                                    p->raRadians, 
                                    p->decRadians, 
                                    discontinuity, 
                                    wrap );
        discontinuity = false;
    }
    driveState_->setMode( DriveState::CELESTIAL );
    acquisition_->reacquire( ); // Force reacquire to return cached sequence #
}

void
DriveEngine::updateRaDec( const RaDecEpoch & raDecEpoch,
                          const WrapType wrap )
{
    const bool discontinuity = false; // Always false for updates.

    ScopeLock scopelock( mutex_ );

    sourcePosition_->setRaDec( raDecEpoch.mjd, 
                               raDecEpoch.raRadians, 
                               raDecEpoch.decRadians, 
                               discontinuity, 
                               wrap );
}

void 
DriveEngine::setWeather( const double ambientTemp, 
                         const double barometricPressure,
                         const double relativeHumidity,
                         const double dewpointTemp,
                         const double windSpeed, 
                         const double windDirection )
{
    ScopeLock scopelock( mutex_ );

    sourcePosition_->updateWeather( ambientTemp, 
                                    barometricPressure, 
                                    relativeHumidity,
                                    dewpointTemp,
                                    windSpeed,
                                    windDirection );
}

void
DriveEngine::setLocation( const Location & location )
{
    ScopeLock scopelock( mutex_ );

    sourcePosition_->setLocation( location );
}

void
DriveEngine::selectAperture( const ApertureType aperture )
{
    ScopeLock scopelock( mutex_ );

    switch ( aperture ) {
        case OPTICAL:
            sourcePosition_->setRefractionModel( 
                DriveEngine::SourcePosition::OPTICAL );
            break;
        case RADIO1MM: 
        case RADIO3MM:
        case RADIO1CM:
            sourcePosition_->setRefractionModel( 
                DriveEngine::SourcePosition::RADIO );
            break;
        default:
            throw CARMA_EXCEPTION( IllegalArgumentException, "Bad aperture." );
    }

    pointing_->selectAperture( aperture );
}

void
DriveEngine::setAperturePointingConstants(
    const ApertureType aperture,
    const Angle & azOffset,
    const Angle & elOffset,
    const Angle & sag )
{
    ScopeLock scopelock( mutex_ );

    pointing_->setAperturePointingConstants( aperture,
                                             azOffset,
                                             elOffset,
                                             sag );
}

void
DriveEngine::setMountPointingConstants( 
    const carma::services::Angle & azEncoderZero,
    const carma::services::Angle & elEncoderZero,
    const carma::services::Angle & nonOrthogonalityOfAxes,
    const carma::services::Angle & azimuthVerticalityNorthSouth,
    const carma::services::Angle & azimuthVerticalityEastWest )
{
    ScopeLock scopelock( mutex_ );

    pointing_->setAzEncoderOffset( azEncoderZero );
    pointing_->setElEncoderOffset( elEncoderZero );
    pointing_->setMountPointingConstants(
        nonOrthogonalityOfAxes,
        azimuthVerticalityNorthSouth,
        azimuthVerticalityEastWest );
}

void
DriveEngine::setTiltmeterZeros(
    const carma::services::Angle & aftForwardZero,
    const carma::services::Angle & leftRightZero )
{
    ScopeLock scopelock( mutex_ );

    pointing_->setTiltmeterZeros( aftForwardZero, leftRightZero );
}

void
DriveEngine::setAzEncoderZero( const carma::services::Angle & azEncoderZero )
{
    ScopeLock scopelock( mutex_ );

    pointing_->setAzEncoderOffset( azEncoderZero );
}

void
DriveEngine::setElEncoderZero( const carma::services::Angle & elEncoderZero )
{
    ScopeLock scopelock( mutex_ );

    pointing_->setElEncoderOffset( elEncoderZero );
}

void
DriveEngine::setMountOffsets( const carma::services::Angle & azMountOffset,
                              const carma::services::Angle & elMountOffset )
{
    ScopeLock scopelock( mutex_ );
    pointing_->setAzMountOffset( azMountOffset );
    pointing_->setElMountOffset( elMountOffset );
    acquisition_->reacquire( ); // Force reacquire to return cached sequence #
}

void
DriveEngine::setAzMountOffset( const carma::services::Angle & azMountOffset )
{
    ScopeLock scopelock( mutex_ );

    pointing_->setAzMountOffset( azMountOffset );
    acquisition_->reacquire( ); // Force reacquire to return cached sequence #
}

void
DriveEngine::setElMountOffset( const carma::services::Angle & elMountOffset )
{
    ScopeLock scopelock( mutex_ );

    pointing_->setElMountOffset( elMountOffset );
    acquisition_->reacquire( ); // Force reacquire to return cached sequence #
}

void
DriveEngine::snow( )
{
    ScopeLock scopelock( mutex_ );

    driveState_->setMode( DriveState::SNOW );
}

void
DriveEngine::stop( )
{
    ScopeLock scopelock( mutex_ );
    driveState_->setMode(DriveState::STOP);
}

void
DriveEngine::stowZenith( )
{
    ScopeLock scopelock( mutex_ );

    driveState_->setMode( DriveState::STOW_ZENITH );
    acquisition_->reacquire( ); // Force reacquire to return cached sequence #
}

void 
DriveEngine::setSafeRange( const Angle & azLow,
                           const Angle & azHigh,
                           const Angle & elLow,
                           const Angle & elHigh ) 
{
    ScopeLock scopelock( mutex_ );

    limits_->setSafeRange( azLow, azHigh, elLow, elHigh );
}

void
DriveEngine::stowSafe( )
{
    ScopeLock scopelock( mutex_ );
            
    if ( !limits_->safeRangeSet( ) ) {
        throw CARMA_EXCEPTION( IllegalStateException, "Safe range not set." );
    } 

    driveState_->setMode( DriveState::STOW_SAFE );
    acquisition_->reacquire( ); // Force reacquire to return cached sequence #
}

void
DriveEngine::setSequenceNumber( const unsigned long seqNo )
{
    ScopeLock scopelock( mutex_ );

    acquisition_->cacheNextSequenceNumberPendingReacquisition( seqNo );
}

void
DriveEngine::setTolerance( const Angle & tolerance )
{
    ScopeLock scopelock( mutex_ );

    acquisition_->setToleranceAndReacquire( tolerance );
}

void
DriveEngine::updateConfigurationData( )
{
    ScopeLock scopelock( mutex_ );
    drive_.updateConfDataFromFile( );
}

void
DriveEngine::freezeTilt( )
{
    ScopeLock scopelock( mutex_ );
    pointing_->updateTilts( false );
}

void
DriveEngine::thawTilt( )
{
    ScopeLock scopelock( mutex_ );
    pointing_->updateTilts( true );
}

void
DriveEngine::toggleBacklashCorrection( const bool enable )
{
    ScopeLock scopelock( mutex_ );
    // There doesn't seem to be any implementation here...
}

void
DriveEngine::writeMonitorData( )
{
    acquisition_->writeMonitorData( );
    limits_->writeMonitorData( );
    pointing_->writeMonitorData( );
    tracking_->writeMonitorData( );
    driveState_->writeMonitorData( );
    sourcePosition_->writeMonitorData( );
    timing_->writeMonitorData( );
    drive_.writeMonitorData( );
}

void
DriveEngine::driveLoopThread( )
{
    FrameAlignedTimer timer( TIMER_OFFSET_NANOS, 1 );
    timer.ResetNextFireTime( 0 ); 
    unsigned long int frame = 0;
        
    bool impossibleRate     = false;
    bool forbiddenAz        = false;
    bool azSwitchFail       = false;
    bool controllerOvertemp = false;
    
    while ( true ) {
        timer.WaitForNextFireTime( );
        timing_->driveLoopWake( );
        ++frame;
        
        ScopeLock scopelock( mutex_ ); // Lock EVERYTHING out each update cycle.

        const enum DriveState::ModeType mode = driveState_->getDriveMode( );
                
        const bool applyFineCorrections = ( mode == DriveState::AZ_EL || 
                                            mode == DriveState::CELESTIAL );
            
        // Successful retrieval of the actual position is critical - if this 
        // fails, set drive state to error, stop the drives and write data.
        // In practice this can fail if there's a timing bug, the module
        // is yanked offline or it otherwise begins to malfunction.
        if ( (!pointing_->updatePosition(applyFineCorrections)) &&
             (driveState_->getDriveState() != OvroDriveState::FATAL) ) {
            driveState_->setDriveStateToError();
            drive_.stop( );
            timing_->driveLoopDone( );
            writeMonitorData( );
            continue;
        }

        const AzElPair actualPosition = pointing_->getPosition();
        const AzElPair rawPosition    = pointing_->getRawPosition();

        driveState_->updateDriveState(actualPosition, rawPosition);
  
        // Done this way so that we only log once on transition into failure      
        if (!impossibleRate) {
            impossibleRate = tracking_->impossibleRateDetected();
        }
        if (!forbiddenAz) {
            forbiddenAz = limits_->isAzForbidden(actualPosition);
        }
        if (!azSwitchFail) {
            azSwitchFail = pointing_->checkForAzOverlapSwitchFail();
        }
        if (!controllerOvertemp) {
            controllerOvertemp = pointing_->getControllerOvertemp();
        }
        
        if (azSwitchFail)       driveState_->setAzOverlapFailure();
        if (impossibleRate)     driveState_->setImpossibleRate();
        if (forbiddenAz)        driveState_->setForbiddenAz();
        if (controllerOvertemp) driveState_->setControllerOvertemp();
        if (driveState_->getDriveState() == OvroDriveState::FATAL) {
            //programLogWarnIfPossible("FATAL detected in drive loop");
            drive_.stop();
            timing_->driveLoopDone();
            writeMonitorData();
            continue;
        }

        switch ( mode ) {
            case DriveState::STOP:
                drive_.stop( ); // Disable continuously.
                break;
            case DriveState::STOW_ZENITH:
            case DriveState::STOW_SAFE:
                if ( acquisition_->isSourceAcquired( ) ) { 
                    drive_.stop( ); // Stop the drives
                    driveState_->setMode( DriveState::STOWED ); 
                } else {
                    tracking_->setNewAzElDriveRates( actualPosition );
                }
                break;
            case DriveState::STOWED:
                break;
            case DriveState::SNOW:
                tracking_->setAzEl( sourcePosition_->
                    calculateSnowTrackPosition( actualPosition ) );
                tracking_->setNewAzElDriveRates( actualPosition );
                break;
            case DriveState::AZ_EL:
                tracking_->setNewAzElDriveRates( actualPosition );
                break;
            case DriveState::CELESTIAL:
                tracking_->setAzEl( sourcePosition_->
                    interpolateSourcePosition( UPDATE_PERIOD_IN_FRAMES ) );
                tracking_->setNewAzElDriveRates( actualPosition );
                break;
            case DriveState::OPEN_VOLTAGE:
                if ( limits_->isAtSwLimit( ) ) { // Only open to a point
                    drive_.stop( );
                } else { // Must continuously update like everything else.
                    drive_.setRawDriveVoltages( engineering_->rawAzVoltage_, 
                                                engineering_->rawElVoltage_ );
                }
                break;
            case DriveState::OPEN_RATE:
                if ( limits_->isAtSwLimit( ) ) { // Only open to a point
                    drive_.stop( );
                } else { // Must continuously update like everything else.
                    drive_.setAzElDriveRates( 
                        engineering_->rawAzRateInRadPerSec_, 
                        engineering_->rawElRateInRadPerSec_,
                        actualPosition.elevation );
                }
                break;
            default:
                break;
        }
    
        timing_->driveLoopDone( );
        writeMonitorData( );
    }
}

void 
DriveEngine::driveLoopThreadEntry( DriveEngine & This )
try {

    This.driveLoopThread( );

} catch ( ... ) {

    if ( CaughtExceptionIsThreadQuitRequestedError( ) ) {

        CARMA_CPTRACE( TRACE_THREADING, "DriveEngine::driveLoop - "
                       "Exiting nicely via a thread quit request." );

        throw;
    } else {
        logCaughtAsError( );
    }
}
