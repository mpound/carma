/**
 * @file
 * Contains class definition for ovro::DriveControl CORBA implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.43 $
 * $Date: 2013/05/23 18:26:06 $
 * $Id: DriveControlImpl.cc,v 1.43 2013/05/23 18:26:06 scott Exp $
 */

#include "carma/antenna/ovro/control/DriveControlImpl.h"

#include "carma/antenna/ovro/control/DriveEngine.h"
#include "carma/corba/Client.h"
#include "carma/corba/Server.h"
#include "carma/services/Angle.h"
#include "carma/services/Length.h"
#include "carma/services/Location.h"
#include "carma/util/BaseException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/Trace.h"

#include <string>
#include <iostream>

using namespace carma;
using namespace carma::antenna;
using namespace carma::antenna::common;
using namespace carma::antenna::ovro;
using namespace carma::services;
using namespace carma::util;
using namespace std;

namespace {

    const carma::util::Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE3;

    string
    apertureAsString( const common::DriveControl::Aperture aperture )
    {
        switch ( aperture ) {
            case common::DriveControl::OPTICAL: return "OPTICAL";
            case common::DriveControl::RADIO1MM: return "RADIO1MM";
            case common::DriveControl::RADIO3MM: return "RADIO3MM";
            case common::DriveControl::RADIO1CM: return "RADIO1CM";
            default: return "< error >";
        }
    }

    string
    positionAsString( const common::DriveControl::Position position )
    {
        switch ( position ) {
            case common::DriveControl::ZENITH:  return "ZENITH";
            case common::DriveControl::SERVICE: return "SERVICE";
            case common::DriveControl::SAFE:    return "SAFE";
            default: return "< error >";
        }
    }

    string
    azWrapModeAsString( const common::DriveControl::AzWrapMode azWrapMode )
    {
        switch ( azWrapMode ) {
            case common::DriveControl::ZERO: return "ZERO";
            case common::DriveControl::ADD: return "ADD";
            case common::DriveControl::SUB: return "SUB";
            default: return "< error >";
        }
    }

    DriveEngine::ApertureType
    commonApertureAsDriveEngineAperture(
        const common::DriveControl::Aperture aperture )
    {
        switch ( aperture ) {
            case common::DriveControl::OPTICAL:
                return DriveEngine::OPTICAL;
            case common::DriveControl::RADIO1MM:
                return DriveEngine::RADIO1MM;
            case common::DriveControl::RADIO3MM:
                return DriveEngine::RADIO3MM;
            case common::DriveControl::RADIO1CM:
                return DriveEngine::RADIO1CM;
            default:
                throw CARMA_EXCEPTION( IllegalArgumentException, "Bad enum." );
        }
    }

    DriveEngine::WrapType
    commonWrapModeAsDriveEngineWrapType(
        const carma::antenna::common::DriveControl::AzWrapMode azWrapMode )
    {
        switch ( azWrapMode ) {
            case common::DriveControl::ZERO: return DriveEngine::NO_TURN;
            case common::DriveControl::ADD: return DriveEngine::ADD_TURN;
            case common::DriveControl::SUB: return DriveEngine::SUBTRACT_TURN;
            default:
                throw CARMA_EXCEPTION( IllegalArgumentException, "Bad enum." );
        }
    }

    void
    rebindOpticalTelRef( OpticalTelControl_var & opticalTelRef,
                         const string & opticalTelName )
    {
        if ( CORBA::is_nil( opticalTelRef ) ) {
            try {
                CARMA_CPTRACE(Trace::TRACE3, "OpticalTelControl reference is "
                        "nil, attempting to re-resolve.");

                corba::Client & client = 
                    Program::getProgram().getCorbaClient();

                opticalTelRef = client.resolveName< OpticalTelControl >(
                        opticalTelName );
            } catch (...) {
                // Stifle and wait till next go around.
                opticalTelRef = OpticalTelControl::_nil();
            }
        }
    }

    string
    raDecEpochAsString( const ovro::RaDecEpoch & raDecEpoch )
    {
        ostringstream oss;
        oss << "( mjd=" << raDecEpoch.mjd << ", ra=" << raDecEpoch.raRadians
            << ", dec=" << raDecEpoch.decRadians << " )";
        return oss.str();
    }

    string
    raDecTripletAsString( const ovro::RaDecTriplet & triplet )
    {
        string ans = "[ ";
        const ovro::RaDecTripletConstIter tBegin = triplet.begin();
        const ovro::RaDecTripletConstIter tEnd = triplet.end();
        for ( ovro::RaDecTripletConstIter t = tBegin; t != tEnd; ++t )
            ans += raDecEpochAsString( *t ) + " ";
        ans += "]";
        return ans;
    }

    ovro::RaDecEpoch
    corbaToOvroRaDecEpoch( const DriveControl::RaDecEpoch & corbaRaDecEpoch )
    {
        ovro::RaDecEpoch ovroRaDecEpoch;
        ovroRaDecEpoch.mjd = corbaRaDecEpoch.mjd;
        ovroRaDecEpoch.raRadians = corbaRaDecEpoch.ra;
        ovroRaDecEpoch.decRadians = corbaRaDecEpoch.dec;
        return ovroRaDecEpoch;
    }

    ovro::RaDecTriplet
    corbaToOvroRaDecTriplet( const DriveControl::RaDecTriplet & corbaRaDecTriplet )
    {
        ovro::RaDecTriplet ovroRaDecTriplet;

        const CORBA::Long count = corbaRaDecTriplet.length( );

        for ( CORBA::Long i = 0; i < count; ++i )
            ovroRaDecTriplet.push_back(
                corbaToOvroRaDecEpoch( corbaRaDecTriplet[ i ] ) );

        return ovroRaDecTriplet;
    }


} // namespace < unnamed >

DriveControlImpl::DriveControlImpl( DriveEngine & driveEngine,
                                    const string & opticalTelControlDOName,
                                    const bool simulate )
    : driveEngine_( driveEngine ),
      simulate_( simulate ),
      opticalTelControlDOName_( opticalTelControlDOName ),
      opticalTelControl_( OpticalTelControl::_nil() )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "DriveControlImpl - Constructor." );
}

DriveControlImpl::~DriveControlImpl( )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "DriveControlImpl - Destructor." );
}

void
DriveControlImpl::stow( const common::DriveControl::Position position,
                        const CORBA::ULong seq )
try {
    ostringstream log;
    log << "DriveControlImpl::stow"
        << "( position=" << positionAsString( position )
        << ", seq=" << seq << " ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.setSequenceNumber( seq );

    switch ( position ) {
        case common::DriveControl::SERVICE:
        case common::DriveControl::ZENITH:
            driveEngine_.stowZenith( );
            break;
        case common::DriveControl::SAFE:
            driveEngine_.stowSafe( );
            break;
        default:
            throw CARMA_EXCEPTION( UserException, "Illegal pos enum." );
    }
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::stop( )
try {
    programLogInfoIfPossible( "DriveControlImpl::stop( )." );

    driveEngine_.stop( );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::trackSnow( )
try {
    programLogInfoIfPossible( "DriveControlImpl::trackSnow( )." );

    driveEngine_.snow( );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::trackWind( )
try {
    programLogErrorIfPossible("DriveControlImpl::trackWind().");

    driveEngine_.stowZenith( );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::setAntLocation( const CORBA::Double longitude,
                                  const CORBA::Double latitude,
                                  const CORBA::Double altitude )
try {
    ostringstream log;
    log << "DriveControlImpl::setAntLocation( longitude=" << longitude
        << ", latitude=" << latitude << ", altitude=" << altitude << " ).";
    programLogInfoIfPossible( log.str( ) );

    const Location location( Angle( longitude, "radians" ),
                             Angle( latitude, "radians" ),
                             Length( altitude, "meters" ) );

    driveEngine_.setLocation( location );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::setAzel( const CORBA::Double az, const CORBA::Double el,
                           const CORBA::ULong seq )
try {
    ostringstream log;
    log << "DriveControlImpl::setAzEl"
        << "( az=" << az << ", el=" << el << ", seq=" << seq << " ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.setSequenceNumber( seq );
    driveEngine_.setAzEl( Angle( az, Angle::DEGREES_STR ),
                          Angle( el, Angle::DEGREES_STR ) );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::setAz( const CORBA::Double az, const CORBA::ULong seq )
try {
    ostringstream log;
    log << "DriveControlImpl::setAz( az=" << az << " seq=" << seq << " ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.setSequenceNumber( seq );
    driveEngine_.setAz( Angle( az, Angle::DEGREES_STR ) );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::setEl( const CORBA::Double el, const CORBA::ULong seq )
try {
    ostringstream log;
    log << "DriveControlImpl::setEl( el=" << el << ", seq=" << seq << " ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.setSequenceNumber( seq );
    driveEngine_.setEl( Angle( el, Angle::DEGREES_STR ) );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::setMaxRate( const CORBA::Float azRate,
                              const CORBA::Float elRate )
try {
    ostringstream log;
    log << "DriveControlImpl::setMaxRate( azRate=" << azRate
        << ", elRate=" << elRate << " ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.setAzMaxRate( azRate );
    driveEngine_.setElMaxRate( elRate );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::setAzMaxRate( const CORBA::Float azRate )
try {
    ostringstream log;
    log << "DriveControlImpl::setAzMaxRate( azRate=" << azRate << " ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.setAzMaxRate( azRate );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::setElMaxRate( const CORBA::Float elRate )
try {
    ostringstream log;
    log << "DriveControlImpl::setElMaxRate( elRate=" << elRate << " ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.setElMaxRate( elRate );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

/// @TODO Implement azWrap and overTheTop
void
DriveControlImpl::track(
    const char * source,
    const common::DriveControl::RaDecTriplet & positionTriplet,
    const common::DriveControl::AzWrapMode azWrapMode,
    const CORBA::Boolean overTheTop,
    const CORBA::ULong seq )
try {
    const ovro::RaDecTriplet triplet =
        corbaToOvroRaDecTriplet( positionTriplet );
    ostringstream log;
    log << "DriveControlImpl::track"
        << "( source=" << source
        << ", positionTriplet=<unlogged>" // << raDecTripletAsString( triplet )
        << ", azWrapMode=" << azWrapModeAsString( azWrapMode )
        << ", overTheTop=" << overTheTop
        << ", seq=" << seq << " ).";
    programLogInfoIfPossible( log.str( ) );

    // Set offsets to zero whenever we go to a new source    
    driveEngine_.setOffset( Angle(0.0, Angle::ARCMIN_STR),
                            Angle(0.0, Angle::ARCMIN_STR));

    driveEngine_.setSequenceNumber( seq );
    driveEngine_.setSource( string( source ) );
    driveEngine_.setRaDec( triplet,
                           commonWrapModeAsDriveEngineWrapType( azWrapMode ) );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::updateRaDec(
        const common::DriveControl::RaDecEpoch & position,
        carma::antenna::common::DriveControl::AzWrapMode azWrapMode )
try {
    const ovro::RaDecEpoch raDecPos = corbaToOvroRaDecEpoch( position );
    ostringstream log;
    log << "DriveControlImpl::updateRaDec"
        << "( position=" << raDecEpochAsString( raDecPos ) << " ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.updateRaDec(
        raDecPos,
        commonWrapModeAsDriveEngineWrapType( azWrapMode ) );

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::updateWeather( const CORBA::Float ambientTemp,
                                 const CORBA::Float barometricPressure,
                                 const CORBA::Float relativeHumidity,
                                 const CORBA::Float dewpointTemp,
                                 const CORBA::Float windSpeed,
                                 const CORBA::Float windDirection )
try {
    ostringstream log;
    log << "DriveControlImpl::updateWeather( ambientTemp=" << ambientTemp
        << ", barometricPressure=" << barometricPressure << ", "
        << "relativeHumidity=" << relativeHumidity << ", dewpointTemp="
        << dewpointTemp << ", windSpeed=" << windSpeed << ", windDirection="
        << windDirection << " ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.setWeather(
        ambientTemp,
        barometricPressure,
        relativeHumidity,
        dewpointTemp,
        windSpeed,
        windDirection );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::setMountOffset( const CORBA::Double az,
                                  const CORBA::Double el,
                                  const CORBA::ULong seq )
try {
    ostringstream log;
    log << "DriveControlImpl::setMountOffset( az=" << az << ", el="
        << el << ", seq=" << seq << " ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.setSequenceNumber( seq );
    driveEngine_.setMountOffsets( Angle( az, Angle::ARCMIN_STR ),
                                  Angle( el, Angle::ARCMIN_STR ) );

    if ( simulate_ ) {

        rebindOpticalTelRef( opticalTelControl_, opticalTelControlDOName_ );

        if ( ! CORBA::is_nil( opticalTelControl_ ) ) {
            opticalTelControl_->zeroTestOffset( );
        }
    }

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::setAzMountOffset( const CORBA::Double az,
                                    const CORBA::ULong seq )
try {
    ostringstream log;
    log << "DriveControlImpl::setAzMountOffset( az=" << az
        << ", seq=" << seq << " ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.setSequenceNumber( seq );
    driveEngine_.setAzMountOffset( Angle( az, Angle::ARCMIN_STR ) );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::setElMountOffset( const CORBA::Double el,
                                    const CORBA::ULong seq )
try {
    ostringstream log;
    log << "DriveControlImpl::setElMountOffset( el=" << el
        << ", seq=" << seq << " ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.setSequenceNumber( seq );
    driveEngine_.setElMountOffset( Angle( el, Angle::ARCMIN_STR ) );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::setOffset( const CORBA::Double az,
                             const CORBA::Double el,
                             const CORBA::ULong seq)
try {
    ostringstream log;
    log << "DriveControlImpl::setOffset( az=" << fixed << setprecision(3)
        << az << ", el="
        << el << ", seq=" << seq << " ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.setSequenceNumber( seq );
    driveEngine_.setOffset( Angle( az, Angle::ARCMIN_STR ),
                            Angle( el, Angle::ARCMIN_STR ) );

    // If we're simulating, try to get the optical tel DO and
    // set offsets for fake stars.
    if ( simulate_ ) {

        rebindOpticalTelRef( opticalTelControl_, opticalTelControlDOName_ );

        if ( ! CORBA::is_nil( opticalTelControl_ ) ) {
            opticalTelControl_->applyTestOffset( az, el );
        }
    }
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::setAzOffset( const CORBA::Double az,
                               const CORBA::ULong seq )
try {
    ostringstream log;
    log << "DriveControlImpl::setAzOffset( az=" << fixed << setprecision(3)
        << az << ", seq=" << seq << " ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.setSequenceNumber( seq );
    driveEngine_.setAzOffset( Angle( az, Angle::ARCMIN_STR ) );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::setElOffset( const CORBA::Double el,
                               const CORBA::ULong seq )
try {
    ostringstream log;
    log << "DriveControlImpl::setElOffset( el=" << fixed << setprecision(3)
        << el << ", seq=" << seq << " ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.setSequenceNumber( seq );
    driveEngine_.setElOffset( Angle( el, Angle::ARCMIN_STR ) );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::setAperturePointingConstants(
    const common::DriveControl::Aperture aperture,
    const CORBA::Float azOffset,
    const CORBA::Float elOffset,
    const CORBA::Float sag )
try {
    ostringstream log;
    log << "DriveControlImpl::setAperturePointingConstants( aperture="
        << apertureAsString( aperture ) << ", azOffset=" << azOffset
        << ", elOffset=" << elOffset << ", sag=" << sag << ").";
    programLogInfoIfPossible( log.str( ) );

    const DriveEngine::ApertureType ap =
        commonApertureAsDriveEngineAperture( aperture );
    driveEngine_.setAperturePointingConstants(
        ap,
        Angle( azOffset, Angle::ARCMIN_STR ),
        Angle( elOffset, Angle::ARCMIN_STR ),
        Angle( sag, Angle::ARCMIN_STR ) );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::setTiltmeterZero( const CORBA::Float aftForward,
                                    const CORBA::Float leftRight )
try {
    ostringstream log;
    log << "DriveControlImpl::setTiltmeterZero( aftForward=" << aftForward
        << ", leftRight=" << leftRight << " ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.setTiltmeterZeros(
        Angle( aftForward, Angle::ARCMIN_STR ),
        Angle( leftRight, Angle::ARCMIN_STR ) );

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::selectAperture(
    const common::DriveControl::Aperture aperture )
try {
    ostringstream log;
    log << "DriveControlImpl::selectAperture( aperture="
        << apertureAsString( aperture ) << " ).";
    programLogInfoIfPossible( log.str( ) );

    const DriveEngine::ApertureType ap =
        commonApertureAsDriveEngineAperture( aperture );
    driveEngine_.selectAperture( ap );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::setTolerance( const CORBA::Float tolerance )
try {
    ostringstream log;
    log << "DriveControlImpl::setTolerance( tolerance=" << tolerance << " ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.setTolerance( Angle( tolerance, Angle::ARCSEC_STR ) );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::setMountPointingConstants( const CORBA::Double m1,
                                             const CORBA::Double m2,
                                             const CORBA::Double m3,
                                             const CORBA::Double m4,
                                             const CORBA::Double m5 )
try {
    ostringstream log;
    log << "DriveControlImpl::setMountPointingconstants( m1=" << m1 <<
                                                         ", m2=" << m2 <<
                                                         ", m3=" << m3 <<
                                                         ", m4=" << m4 <<
                                                         ", m5=" << m5 << " ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.setMountPointingConstants(
        Angle( m1, Angle::ARCMIN_STR ),
        Angle( m2, Angle::ARCMIN_STR ),
        Angle( m3, Angle::ARCMIN_STR ),
        Angle( m4, Angle::ARCMIN_STR ),
        Angle( m5, Angle::ARCMIN_STR ) );

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::setSafeRange( CORBA::Float azLow, CORBA::Float azHigh,
			  CORBA::Float elLow, CORBA::Float elHigh)
try {
    ostringstream log;
    log << "DriveControlImpl::setSafeRange( azLow=" << azLow << ", azHigh="
        << azHigh << ", elLow=" << elLow << ", elHigh=" << elHigh << " ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.setSafeRange( Angle( azLow, Angle::DEGREES_STR ),
                               Angle( azHigh, Angle::DEGREES_STR ),
                               Angle( elLow, Angle::DEGREES_STR ),
                               Angle( elHigh, Angle::DEGREES_STR ) );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::setRawDriveVoltages( const CORBA::Float azVoltage,
                                       const CORBA::Float elVoltage )
try {
    ostringstream log;
    log << "DriveControlImpl::setRawDriveVoltages( "
        << "azVoltage=" << azVoltage << ", "
        << "elVoltage=" << elVoltage << " ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.setRawDriveVoltages( static_cast< float >( azVoltage ),
                                      static_cast< float >( elVoltage ) );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::setRawDriveRates( const CORBA::Float azRateInDegPerMin,
                                    const CORBA::Float elRateInDegPerMin )
try {
    ostringstream log;
    log << "DriveControlImpl::setRawDriveRates( "
        << "azRateInDegPerMin=" << azRateInDegPerMin << ", "
        << "elRateInDegPerMin=" << elRateInDegPerMin << " ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.setRawDriveRates( static_cast< float >( azRateInDegPerMin ),
                                   static_cast< float >( elRateInDegPerMin ) );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::setEngineeringMode( const CORBA::Boolean enable )
try {
    ostringstream log;
    log << "DriveControlImpl::setEngineeringMode( enable=" << enable << ").";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.setEngineeringMode( static_cast< bool >( enable ) );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::updateConfigurationData( )
try {
    ostringstream log;
    log << "DriveControlImpl::updateConfigurationData( ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.updateConfigurationData( );

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::freezeTilt( )
try {
    ostringstream log;
    log << "DriveControlImpl::freezeTilt( ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.freezeTilt( );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::thawTilt( )
try {
    ostringstream log;
    log << "DriveControlImpl::thawTilt( ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.thawTilt( );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
DriveControlImpl::toggleBacklashCorrection( const CORBA::Boolean enable )
try {
    ostringstream log;
    log << "DriveControlImpl::toggleBacklashCorrection( ).";
    programLogInfoIfPossible( log.str( ) );

    driveEngine_.toggleBacklashCorrection( enable );
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
}
