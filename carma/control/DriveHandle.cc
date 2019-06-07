// $Id: DriveHandle.cc,v 1.135 2014/04/02 23:10:53 iws Exp $
// Implementation of control interface to Drive object
//
// @author: Amar Amarnath
// @author: Marc Pound
//
// $CarmaCopyright$
//

#include <string>
#include <stdexcept>
#include <iomanip>
#include <sstream>
#include <iostream>
#include <cstdlib>
#include <math.h>

#include "carma/corba/corba.h"
#include "carma/antenna/common/DriveControl.h"
#include "carma/antenna/common/driveControlUtils.h"
#include "carma/antenna/ovro/control/ovroDriveControl.h"
#include "carma/antenna/bima/control/bimaDriveControl.h"
#include "carma/antenna/sza/control/szaDriveControl.h"
#include "carma/control/DriveHandle.h"
#include "carma/control/antennaHandleUtils.h"
#include "carma/monitor/MonitorSubsystem.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/AntennaCommon.h"
#include "carma/monitor/BimaSubsystem.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/services/Angle.h"
#include "carma/services/Distance.h"
#include "carma/services/EphemerisException.h"
#include "carma/services/Physical.h"
#include "carma/services/Pressure.h"
#include "carma/services/Source.h"
#include "carma/services/SourceNotFoundException.h"
#include "carma/services/stringConstants.h"
#include "carma/services/Temperature.h"
#include "carma/services/Velocity.h"
#include "carma/util/corbaSequenceUtils.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Time.h"
#include "carma/util/TimedBenchmark.h"
#include "carma/util/Trace.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/PthreadCond.h"
#include "carma/util/ScopedLock.h"


using namespace ::std;
using namespace ::log4cpp;
using namespace ::carma;
using namespace ::carma::antenna;
using namespace ::carma::antenna::common;
using namespace ::carma::antenna::bima;
using namespace ::carma::antenna::bima::control;
using namespace ::carma::antenna::ovro;
using namespace ::carma::control;
using namespace ::carma::monitor;
using namespace ::carma::services;
using namespace ::carma::util;


namespace {

typedef ControlSubsystemBase::TrackModeMonitorPointEnum TrackModeEnum;

const Trace::TraceLevel kTraceLevel = Trace::TRACE4;

::pthread_mutex_t gEphemGuard = PTHREAD_MUTEX_INITIALIZER;

typedef ScopedLock< ::pthread_mutex_t > EphemLock;

const Angle BIMA_POSITIVE_AZ_LIMIT(450.0, carma::services::DEGREES);
const Angle BIMA_NEGATIVE_AZ_LIMIT(-90.0, carma::services::DEGREES);
const Angle BIMA_LOWER_ELEV_LIMIT(   1.5, carma::services::DEGREES);
const Angle BIMA_UPPER_ELEV_LIMIT(85, carma::services::DEGREES);

const Angle OVRO_POSITIVE_AZ_LIMIT(353.0, carma::services::DEGREES);
const Angle OVRO_NEGATIVE_AZ_LIMIT(-88.0, carma::services::DEGREES);
const Angle OVRO_LOWER_ELEV_LIMIT(   6.0, carma::services::DEGREES);
const Angle OVRO_UPPER_ELEV_LIMIT(87.5, carma::services::DEGREES);

const Angle SZA_POSITIVE_AZ_LIMIT( 326.0, carma::services::DEGREES);
const Angle SZA_NEGATIVE_AZ_LIMIT(-146.0, carma::services::DEGREES);
const Angle SZA_LOWER_ELEV_LIMIT(   17.0, carma::services::DEGREES);
const Angle SZA_UPPER_ELEV_LIMIT(   86.0, carma::services::DEGREES);

const Angle HW_UPPER_ELEV_LIMIT(88.5, carma::services::DEGREES);

const Angle    ZENITH_EL(87.5, carma::services::DEGREES );
const Velocity ZERO_VEL( 0.0, carma::services::KMS );
const Distance ZERO_DIST( 0.0, "km" );


}  // namespace < anonymous >


DriveHandle::DriveHandle( const unsigned short   carmaAntNo, 
                          const Location &       location,
                          MonitorSystem &        monitorSystem,
                          ControlSubsystemBase::Antenna & antenna ) :
DriveControlRemoteObjHandle(
    makeAntennaDoName( carmaAntNo, DRIVE_NAME ),
    &(antenna.antennaReachable( ).drive( )),
    &(getAntennaSubsystem( carmaAntNo, monitorSystem )),
    &monitorSystem,
    false, // log if not reachable
    true  // log sent commands
    ),
carmaAntNo_( carmaAntNo ),
antType_( computeAntennaType( carmaAntNo ) ),
antennaSubsystem_( getAntennaSubsystem( carmaAntNo, monitorSystem ) ),
monitorSystem_( monitorSystem ),
antenna_( antenna ),
sourceMode_( SOURCE_MODE_INVALID ),
trackToleranceFreq_(0.0),
trackTolerance_(0.08), 
raTrackingOffset_( 0.0 ),
decTrackingOffset_( 0.0 ),
setSafeRangeCompleted_( false ),
timeToTrack_(25.0),
lastControlWrapMode_(carma::control::ZERO),
lastAntWrapMode_(carma::antenna::common::DriveControl::ZERO)
{
    setSourceModeToIdle( );
    setLocation( location );
    nextSequenceNo_ = -10;
    consecutiveErrors_ = 0;
    errLimit_ = 0;
    // set a default MJD for the ephemeris to stifle
    // warnings before track command is called.
    localEphemeris_.setMJD();
    checker_.setLocation( location );
    checker_.setMJD();
    defaultWrapLimits();
}


DriveHandle::~DriveHandle( )
try {
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}

void
DriveHandle::updateRaDec( const string & source,
                          const double mjd, const double ra, const double dec, 
                          control::AzWrapMode mode ) 
{
    const ScopedLogNdc ndc( "DriveHandle::updateRaDec" );
    
    setSourceModeToRaDec( );
    
    if ( isObjReachable( ) ) {
        
        // Actual sent mode may be different from requested
        // if request was "TIME" or was illegal (e.g. ADD for an OVRO
        // antenna).  So log the sent mode as well.
        const common::DriveControl::AzWrapMode sentMode = 
            controlWrapToDriveWrap(mode);

        string remoteCallString;
        {
            ostringstream oss;
            
            oss << std::fixed
                << "DriveControl::updateRaDec"
                << "(source=" << source
                << ", mjd=" << setprecision(9) << mjd
                << ", RA(rad)=" << setprecision(7) << ra
                << ", Dec(rad)=" << setprecision(7) << dec
                << ", mode=" << azWrapModeToString( mode ) 
                << " (converted to " << azWrapModeToString( sentMode ) << " )"
                << ")";
                
            remoteCallString = oss.str( );
        }
        
        try {
            const double sendTime = Time::MJD();

            common::DriveControl::RaDecEpoch raDecEpoch;
            raDecEpoch.mjd = mjd;
            raDecEpoch.ra = ra;
            raDecEpoch.dec = dec;

            remoteObj( )->updateRaDec( raDecEpoch, sentMode );

            logSentCommandIfNeeded( remoteCallString, sendTime );

            // set the monitor points after successful completion.
            antenna_.pointRa().setValue( ra );
            antenna_.pointDec().setValue( dec );

            lastAntWrapMode_ = sentMode;
        } catch ( const CORBA::Exception & ex ) {
            processException( remoteCallString, ex );
        }
    } // if reachable
}

void
DriveHandle::track( const string & source,
                    const MjdTriplet & triplet,
                    const bool resetTrackOffsets,
                    const string & userCatalog,
                    const control::AzWrapMode mode,
                    const int preferredSequenceNo )
{
    // DEBUG VARIABLES
    const bool debug = false;
    double startLoop;
    
    if (debug) Program::getLogger() << Priority::INFO
            << "Entering DriveHandle::track() for " + doName()
            << " at " << Time::getTimeString(3);
    string ndcString;
    {
        ostringstream oss;
        oss << "DriveHandle::track" 
            << "(source=" << source
            << ", resetTrackOffsets=" << boolalpha << resetTrackOffsets 
            << ", ucat=\"" << userCatalog << "\""
            << ", mode=" << azWrapModeToString(mode) 
            << ", seqNo=" << preferredSequenceNo << ")";
        ndcString = oss.str();
    }
    const ScopedLogNdc ndc(ndcString);
        
    string remoteCallString;

    try {
        // set source mode to ensure that equatOffset call does not
        // throw an exception.   User may have started from
        // another source mode, which is ok since we are about to track.
        setSourceModeToRaDec();

        common::DriveControl::RaDecTriplet raDecTriplet;
        raDecTriplet.length( 3 );

        if (debug) {
            startLoop = Time::MJD();
            Program::getLogger() << Priority::INFO
            << "Begin DriveHandle::track() compute triplet for " + doName()
            << " at " << Time::getTimeString(3);
        }

        // call setSource outside of loop to save computational load.
        // no need to call it 6 times.
        {
            CARMA_CPTRACE( kTraceLevel, 
                    "setting localEphemeris_ source" );
            TimedBenchmark tb;
            tb.reset();
            const EphemLock lock( gEphemGuard );
            tb.start();
            // This will throw SourceNotFoundException if the source 
            // is not a planet and is not in the user or standard catalogs.
            // Note we can always pass in userCatalog here even
            // if it was not specified in the call, since the default
            // value is "" which is also the default for the Ephemeris
            // (making it ignore user catalog and go straight to system
            // catalog).
            programLogInfoIfPossible("Calling setSource on Ephemeris and SourceChecker. This involves a disk read. (2 actually)");
            localEphemeris_.setSource(source, userCatalog);
            checker_.setSource(source, userCatalog);
            tb.stop();

            if ( tb.milliseconds() > 1000.0 ) {
                ostringstream oss;
                oss << "DriveHandle::track() - localEphemeris_ and checker_ "
                    << "setSource calls took " << tb.milliseconds() << "ms.";
                programLogErrorIfPossible( oss.str() );
            }
        }

        for ( CORBA::ULong i = 0; i < 3; ++i ) {

            CARMA_CPTRACE( kTraceLevel, 
                    "setting localEphemeris_ mjd from triplet times" );

            {
                const EphemLock lock( gEphemGuard );

                localEphemeris_.setMJD(triplet.mjd[i]);
            }

            // If asked, clear the tracking offsets. Use the DriveHandle 
            // method so that the Ephemeris offsets also get cleared.
            if ( resetTrackOffsets ) {
                CARMA_CPTRACE( kTraceLevel, "resetting equat offset" );
                setEquatOffset( 0.0, 0.0 );
            } else {
                CARMA_CPTRACE( kTraceLevel, 
                        "resetting localEphemeris_ ra/dec offsets" );

                const EphemLock lock( gEphemGuard );

                // ephemeris::setSource resets all offsets to zero,
                // so reset them here.
                localEphemeris_.setRaDecOffsets(
                        raTrackingOffset_,
                        decTrackingOffset_ );
                // NB: we are ignoring tracking offsets for the SourceChecker.
                // Values w/o them are good enough for wrap decisions.
            }

            CARMA_CPTRACE( kTraceLevel, 
                    "computing localEphemeris_ ra and dec" );

            raDecTriplet[i].mjd = triplet.mjd[i];
            {
                const EphemLock lock( gEphemGuard );

                // getRa/Dec can throw EphemerisException.
                // both recomputed topocentric RA, radians
                raDecTriplet[i].ra  = localEphemeris_.getRa();  
                raDecTriplet[i].dec = localEphemeris_.getDec();
            }

        } // End loop over mjd triplet values
        if (debug) {
            double endLoop = Time::MJD();
            int delta   = static_cast<int>((endLoop-startLoop)*86400*1000);
            Program::getLogger() << Priority::INFO
            << "End DriveHandle::track() compute triplet for " + doName()
            << " at " << Time::getTimeString(3)
            << " DELTA=" << delta << " ms ";
        }
        
        if (!isObjReachable( )) return;

        const double sendTime = Time::MJD();

        // Actual sent wrap mode may be different from requested
        // if request was "TIME" or was illegal (e.g. ADD for an OVRO
        // antenna).  So log the sent mode as well.
        const common::DriveControl::AzWrapMode sentMode = 
            controlWrapToDriveWrap(mode);

        // No antennas should be looking up the source in an Ephemeris 
        // via the source name.  To do so violates the agreed upon design. 
        // Thus the ra and dec are sent remote object.
        const bool overTheTop = false;

        setNextSequenceNo( preferredSequenceNo );

        {
            ostringstream oss;
            oss << std::fixed
                << "DriveControl::track"
                << "(source=" << source
                << ", positionTriplet=" <<  raDecTripletToString(raDecTriplet )
                << ", azWrapMode=" << azWrapModeToString(sentMode)
                << ", overTheTop=" << boolalpha << overTheTop  
                << ", preferredSeqNo=" << preferredSequenceNo 
                << "(nextSequenceNo=" << nextSequenceNo_ << "))";

            remoteCallString = oss.str();
        }

        CARMA_CPTRACE( kTraceLevel, "calling track" );
        remoteObj( )->track( source.c_str(), 
                raDecTriplet, 
                sentMode,
                overTheTop,
                nextSequenceNo_ );

        logSentCommandIfNeeded( remoteCallString, sendTime );

        // set the monitor points after successful completion.
        antenna_.pointRa().setValue( raDecTriplet[1].ra );
        antenna_.pointDec().setValue( raDecTriplet[1].dec );
    } catch ( const SourceNotFoundException& snfe ) {
        logException( remoteCallString, snfe.getMessage( ) );
        throw; // will be caught by SubarrayControl::track()
    } catch ( const EphemerisException& ee ) {
        logException( remoteCallString, ee.getMessage( ) );
        programLogError( "EphemerisException stifled." );
    } catch ( const CORBA::Exception & ex ) {
        processException( remoteCallString, ex );
    }

    CARMA_CPTRACE( kTraceLevel, "end" );
}

void
DriveHandle::track( const Source & source,
                    const MjdTriplet & triplet,
                    const bool resetTrackOffsets,
                    const control::AzWrapMode mode,
                    const int preferredSequenceNo )
{
    // DEBUG VARIABLES
    const bool debug = false;
    double startLoop;
    
    if (debug) Program::getLogger() << Priority::INFO
            << "Entering DriveHandle::track() for " + doName()
            << " at " << Time::getTimeString(3);
    string ndcString;
    {
        ostringstream oss;
        oss << "DriveHandle::track" 
            << "(source=" << source.getName()
            << ", resetTrackOffsets=" << boolalpha << resetTrackOffsets 
            << ", mode=" << azWrapModeToString(mode) 
           << ", seqNo=" << preferredSequenceNo << ")";
        ndcString = oss.str();
    }
    const ScopedLogNdc ndc(ndcString);
        
    string remoteCallString;

    try {
        // set source mode to ensure that equatOffset call does not
        // throw an exception.   User may have started from
        // another source mode, which is ok since we are about to track.
        setSourceModeToRaDec();

        common::DriveControl::RaDecTriplet raDecTriplet;
        raDecTriplet.length( 3 );

        if (debug) {
            startLoop = Time::MJD();
            Program::getLogger() << Priority::INFO
            << "Begin DriveHandle::track() compute triplet for " + doName()
            << " at " << Time::getTimeString(3);
        }

        // call setSource outside of loop to save computational load.
        // no need to call it 6 times.
        {
            CARMA_CPTRACE( kTraceLevel, 
                    "setting localEphemeris_ source" );
            TimedBenchmark tb;
            tb.reset();
            const EphemLock lock( gEphemGuard );
            tb.start();
            localEphemeris_.setSource( source );
            checker_.setSource( source );
            tb.stop();

            if ( tb.milliseconds() > 1000.0 ) {
                ostringstream oss;
                oss << "DriveHandle::track() - localEphemeris_ and checker_ "
                    << "setSource calls took " << tb.milliseconds() << "ms.";
                programLogErrorIfPossible( oss.str() );
            }
        }

        for ( CORBA::ULong i = 0; i < 3; ++i ) {

            CARMA_CPTRACE( kTraceLevel, 
                    "setting localEphemeris_ mjd from triplet times" );

            {
                const EphemLock lock( gEphemGuard );

                localEphemeris_.setMJD(triplet.mjd[i]);
            }

            // If asked, clear the tracking offsets. Use the DriveHandle 
            // method so that the Ephemeris offsets also get cleared.
            if ( resetTrackOffsets ) {
                CARMA_CPTRACE( kTraceLevel, "resetting equat offset" );
                setEquatOffset( 0.0, 0.0 );
            } else {
                CARMA_CPTRACE( kTraceLevel, 
                        "resetting localEphemeris_ ra/dec offsets" );

                const EphemLock lock( gEphemGuard );

                // ephemeris::setSource resets all offsets to zero,
                // so reset them here.
                localEphemeris_.setRaDecOffsets(
                        raTrackingOffset_,
                        decTrackingOffset_ );
                // NB: we are ignoring tracking offsets for the SourceChecker.
                // Values w/o them are good enough for wrap decisions.
            }

            CARMA_CPTRACE( kTraceLevel, 
                    "computing localEphemeris_ ra and dec" );

            raDecTriplet[i].mjd = triplet.mjd[i];
            {
                const EphemLock lock( gEphemGuard );

                // getRa/Dec can throw EphemerisException.
                // both recomputed topocentric RA, radians
                raDecTriplet[i].ra  = localEphemeris_.getRa();  
                raDecTriplet[i].dec = localEphemeris_.getDec();
            }

        } // End loop over mjd triplet values
        if (debug) {
            double endLoop = Time::MJD();
            int delta   = static_cast<int>((endLoop-startLoop)*86400*1000);
            Program::getLogger() << Priority::INFO
            << "End DriveHandle::track() compute triplet for " + doName()
            << " at " << Time::getTimeString(3)
            << " DELTA=" << delta << " ms ";
        }
        
        if (!isObjReachable( )) return;

        const double sendTime = Time::MJD();

        // Actual sent wrap mode may be different from requested
        // if request was "TIME" or was illegal (e.g. ADD for an OVRO
        // antenna).  So log the sent mode as well.
        const common::DriveControl::AzWrapMode sentMode = 
            controlWrapToDriveWrap(mode);

        // No antennas should be looking up the source in an Ephemeris 
        // via the source name.  To do so violates the agreed upon design. 
        // Thus the ra and dec are sent remote object.
        const bool overTheTop = false;

        setNextSequenceNo( preferredSequenceNo );

        {
            ostringstream oss;
            oss << std::fixed
                << "DriveControl::track"
                << "(source=" << source.getName()
                << ", positionTriplet=" <<  raDecTripletToString(raDecTriplet )
                << ", azWrapMode=" << azWrapModeToString(sentMode)
                << ", overTheTop=" << boolalpha << overTheTop  
                << ", preferredSeqNo=" << preferredSequenceNo 
                << "(nextSequenceNo=" << nextSequenceNo_ << "))";

            remoteCallString = oss.str();
        }

        CARMA_CPTRACE( kTraceLevel, "calling track" );
        remoteObj( )->track( source.getName().c_str(), 
                raDecTriplet, 
                sentMode,
                overTheTop,
                nextSequenceNo_ );

        logSentCommandIfNeeded( remoteCallString, sendTime );

        // set the monitor points after successful completion.
        antenna_.pointRa().setValue( raDecTriplet[1].ra );
        antenna_.pointDec().setValue( raDecTriplet[1].dec );
    } catch ( const SourceNotFoundException& snfe ) {
        logException( remoteCallString, snfe.getMessage( ) );
        throw; // will be caught by SubarrayControl::track()
    } catch ( const EphemerisException& ee ) {
        logException( remoteCallString, ee.getMessage( ) );
        programLogError( "EphemerisException stifled." );
    } catch ( const CORBA::Exception & ex ) {
        processException( remoteCallString, ex );
    }

    CARMA_CPTRACE( kTraceLevel, "end" );
}

void
DriveHandle::startTrack( const string source,
                         const MjdTriplet mjdTriplet,
                         const string userCatalog ,
                         const control::AzWrapMode mode,
                         const double time,
                         const int preferredSequenceNo )
{
    const ScopedLogNdc ndc( "DriveHandle::startTrack" );

    timeToTrack_ = time;
    const bool resetTrackOffsets = true;

    bool fixed = false;
    {
        const EphemLock lock( gEphemGuard );
        fixed = localEphemeris_.isFixed( source );
    }

    if ( fixed ) {
        CARMA_CPTRACE(kTraceLevel, "calling trackFixed ant="<<carmaAntNo_);
        trackFixed( source, preferredSequenceNo );
    } else  {
        trackTriplet( source, mjdTriplet, resetTrackOffsets, userCatalog, mode,
                      preferredSequenceNo );
    }
}

void
DriveHandle::startTrackX( const services::Source source,
                         const MjdTriplet mjdTriplet,
                         const control::AzWrapMode mode,
                         const double time,
                         const int preferredSequenceNo )
{
    const ScopedLogNdc ndc( "DriveHandle::startTrackX" );

    timeToTrack_ = time;
    const bool resetTrackOffsets = true;

    bool fixed = false;
    {
        const EphemLock lock( gEphemGuard );
        fixed = localEphemeris_.isFixed( source.getName() );
    }

    if ( fixed ) {
        CARMA_CPTRACE(kTraceLevel, "calling trackFixed ant="<<carmaAntNo_);
        trackFixed( source.getName(), preferredSequenceNo );
    } else  {
        trackTriplet( source, mjdTriplet, resetTrackOffsets, mode,
                      preferredSequenceNo );
    }
}

void 
DriveHandle::trackFixed( const string & source, const int preferredSequenceNo ) 
{
    const ScopedLogNdc ndc( "DriveHandle::trackFixed" );

    double az = 0.0;
    double el = 0.0;
    double azRad = 0.0;
    double elRad = 0.0;
    CARMA_CPTRACE(kTraceLevel, 
            "trackFixed antNo= "<< carmaAntNo_ 
            <<": localEphem::setSource("
            << source << ")" );
    {
        //Note: scoping this lock over the entire method
        //results in deadlock b/c of EphemLock in setAzel.
        //Keep this scope to a minimum.
        const EphemLock lock( gEphemGuard );
        localEphemeris_.setSource( source );
        //localEphemeris_.setMJD();
        azRad = localEphemeris_.getAz();
        elRad = localEphemeris_.getEl();
    }
    Angle azAngle(azRad, services::RADIANS);
    Angle elAngle(elRad, services::RADIANS);
    az=azAngle.degrees();
    el=elAngle.degrees();
    CARMA_CPTRACE(kTraceLevel, 
            "trackFixed antNo= "<< carmaAntNo_ <<": setAzel("
            << az << "," <<el<< ")" );
    setAzel( az, el, preferredSequenceNo );
    CARMA_CPTRACE(kTraceLevel, "trackFixed antNo= "<< carmaAntNo_ <<": end");
}

void
DriveHandle::trackCurrentSourceWithOffsets( 
                         const MjdTriplet mjdTriplet,
                         const string userCatalog ,
                         const control::AzWrapMode mode,
                         const int preferredSequenceNo ) 
{
    const string source = getSourceName();
    const bool resetTrackOffsets = false;
    trackTriplet( source, mjdTriplet, resetTrackOffsets, userCatalog, mode, 
                  preferredSequenceNo );
}

void
DriveHandle::trackCurrentSourceWithOffsets( 
                         const MjdTriplet mjdTriplet,
                         const control::AzWrapMode mode,
                         const int preferredSequenceNo ) 
{
    const carma::services::Source source = getSource();
    const bool resetTrackOffsets = false;
    trackTriplet( source, mjdTriplet, resetTrackOffsets, mode, 
                  preferredSequenceNo );
}

void
DriveHandle::trackTriplet( const Source & source,
                           const MjdTriplet & mjdTriplet,
                           const bool resetTrackOffsets, 
                           control::AzWrapMode mode,
                           const int preferredSequenceNo )
{
    const ScopedLogNdc ndc( "DriveHandle::trackTriplet" );

    // Note input mjd vector is
    //
    // mjd[0] = (now - antInterval/2)
    // mjd[1] = (now + antInterval/2)
    // mjd[2] = (now + 1.5 * antInterval)
    //
    // None of these is "now" so to fulfill the timeToTrack_
    // contract in the case of mode==TIME, we must compute
    // the optimal mode "now".   Assume the time we entered
    // this method is close enough to when track() was invoked
    // at the python level.
    
    double now = optimumWrapTime_ = Time::MJD();

    // There is a chance that the optimum wrap value will
    // change between now and the next invocation of 
    // setRaDec via the SAT.  This can happen for a northern
    // source crossing az=0 on 6m antennas or az=-7 on 10m
    // antennas, from a positive azimuth direction.
    // Therefore compute the optimum wrap also at the next
    // period.  If they differ send the "future" wrap.
    // This may mean that the wrap directive will be
    // not do-able by the antenna right now because,
    // but the directive is only to "resolve azimuth
    // ambiguities" and not a command to add or subtract
    // wraps.
    // Note, we only need check the first interval because 
    // if mode==TIME, each invocation of setRaDec via
    // SAT will recompute the optimum value.
   
    if ( mode == control::TIME )
    {
        const antenna::common::DriveControl::AzWrapMode nowWrap 
            = computeOptimumWrapValue();
        optimumWrapTime_    = mjdTriplet.mjd[1];
        double svTT         = timeToTrack_;
        double antInterval  = mjdTriplet.mjd[1] - mjdTriplet.mjd[0] ;
        timeToTrack_       -= antInterval;
        if ( timeToTrack_ <= 0 ) timeToTrack_ = antInterval;
        const antenna::common::DriveControl::AzWrapMode nextWrap 
            = computeOptimumWrapValue();
        if ( nextWrap != nowWrap )  {
            ostringstream os;
            os << "Re-optimizing wrap from "
                << azWrapModeToString( nowWrap )
                << " to "
                << azWrapModeToString( nextWrap )
                << ".";
            mode = driveWrapToControlWrap( nextWrap );
            programLogNoticeIfPossible( os.str() );
        } else  {
            optimumWrapTime_ = now;
            timeToTrack_     = svTT;
        }
    }

    track( source, mjdTriplet, resetTrackOffsets, mode, preferredSequenceNo  );
}

void
DriveHandle::trackTriplet( const string & source,
                           const MjdTriplet & mjdTriplet,
                           const bool resetTrackOffsets, 
                           const string & userCatalog,
                           control::AzWrapMode mode,
                           const int preferredSequenceNo )
{
    const ScopedLogNdc ndc( "DriveHandle::trackTriplet" );

    // Note input mjd vector is
    //
    // mjd[0] = (now - antInterval/2)
    // mjd[1] = (now + antInterval/2)
    // mjd[2] = (now + 1.5 * antInterval)
    //
    // None of these is "now" so to fulfill the timeToTrack_
    // contract in the case of mode==TIME, we must compute
    // the optimal mode "now".   Assume the time we entered
    // this method is close enough to when track() was invoked
    // at the python level.
    
    double now = optimumWrapTime_ = Time::MJD();

    // There is a chance that the optimum wrap value will
    // change between now and the next invocation of 
    // setRaDec via the SAT.  This can happen for a northern
    // source crossing az=0 on 6m antennas or az=-7 on 10m
    // antennas, from a positive azimuth direction.
    // Therefore compute the optimum wrap also at the next
    // period.  If they differ send the "future" wrap.
    // This may mean that the wrap directive will be
    // not do-able by the antenna right now because,
    // but the directive is only to "resolve azimuth
    // ambiguities" and not a command to add or subtract
    // wraps.
    // Note, we only need check the first interval because 
    // if mode==TIME, each invocation of setRaDec via
    // SAT will recompute the optimum value.
   
    if ( mode == control::TIME )
    {
        const antenna::common::DriveControl::AzWrapMode nowWrap 
            = computeOptimumWrapValue();
        optimumWrapTime_    = mjdTriplet.mjd[1];
        double svTT         = timeToTrack_;
        double antInterval  = mjdTriplet.mjd[1] - mjdTriplet.mjd[0] ;
        timeToTrack_       -= antInterval;
        if ( timeToTrack_ <= 0 ) timeToTrack_ = antInterval;
        const antenna::common::DriveControl::AzWrapMode nextWrap 
            = computeOptimumWrapValue();
        if ( nextWrap != nowWrap )  {
            ostringstream os;
            os << "Re-optimizing wrap from "
                << azWrapModeToString( nowWrap )
                << " to "
                << azWrapModeToString( nextWrap )
                << ".";
            mode = driveWrapToControlWrap( nextWrap );
            programLogNoticeIfPossible( os.str() );
        } else  {
            optimumWrapTime_ = now;
            timeToTrack_     = svTT;
        }
    }

    track( source, mjdTriplet, resetTrackOffsets, userCatalog, mode,
           preferredSequenceNo  );
}


void
DriveHandle::setLocation( const Location & location ) {
    {
        const EphemLock lock( gEphemGuard );

        localEphemeris_.setLocation(location);
    }
    
    if ( isObjReachable( ) ) {
        const double longitude = location.getLongitude().radians();
        const double latitude = location.getLatitude().radians();
        const double altitude = location.getAltitude().meters();
        
        // Coordinates are Geocentric (ECEF) in the catalog and in the control
        // system for interferometry, but sent to the antennas
        // as geodetic WGS84. The difference is 11.1148 arcmin; we neglect any
        // change in altitude. The difference is accurate to within 0.1 asec
        // over the latitude range of CARMA.
        double  WGS84latitude = latitude + 11.1148*M_PI/(180*60);

        string remoteCallString;
        {
            ostringstream oss;
            
            oss << std::fixed
                << "DriveControl::setAntLocation("
                << "longitude(rad)=" << setprecision(12) << longitude
                << " latitude(rad)=" << setprecision(12) << WGS84latitude
                << " altitude(m)="   << setprecision(5)  << altitude
                << ") WGS84";
            
            remoteCallString = oss.str( );
        }
    
        try  {
            const double sendTime = Time::MJD( );
            
            remoteObj( )->setAntLocation(longitude, WGS84latitude, altitude);
                    
            logSentCommandIfNeeded( remoteCallString, sendTime );
        }  catch ( const ErrorException & ex ) {
            logException( remoteCallString, ex.getMessage( ) );

            programLogError(
                "ErrorException was stifled within DriveHandle::setLocation."
            );
        } catch ( ... ) {
            programLogErrorIfPossible(
                "Exception stifled within DriveHandle::setLocation: " +
                getStringForCaught()
            );
        }
    }
}


Location
DriveHandle::getLocation( ) const {
    Location theLocation;
    {
        const EphemLock lock( gEphemGuard );

        theLocation = localEphemeris_.getLocation();
    }
    
    ostringstream oss;

    oss << " DriveHandle::getLocation(): "
        << " Location returned from localEphemeris_ is: "
        << theLocation;
        
    programLogInfo( oss.str( ) );

    return theLocation;
}


Ephemeris
DriveHandle::getEphemeris( ) const {
    const EphemLock lock( gEphemGuard );

    return localEphemeris_;
}


void
DriveHandle::setOffset( const double azArcmin,
                        const double elArcmin,
                        const int preferredSequenceNo ) {

    setNextSequenceNo( preferredSequenceNo );

    if ( isObjReachable( ) )  {
        string remoteCallString;
        {
            ostringstream oss;
    
            oss << std::fixed
                << "DriveControl::setOffset("
                << "azArcmin = "   << setprecision(2) << azArcmin
                << ", elArcmin = " << setprecision(2) << elArcmin
                << ", preferredSequenceNo = " << preferredSequenceNo
                << " (nextSequenceNo = " << nextSequenceNo_ << "))";
    
            remoteCallString = oss.str( );
        }
        
        try  {
            const double sendTime = Time::MJD( );
            
            remoteObj( )->setOffset( azArcmin, elArcmin, nextSequenceNo_ );
          
            logSentCommandIfNeeded( remoteCallString, sendTime );
        }  catch ( const CORBA::Exception & ex )  {
            processException( remoteCallString, ex );
        }
    }
}


void
DriveHandle::setAzOffset ( const double azArcmin,
                           const int preferredSequenceNo ) {

    setNextSequenceNo( preferredSequenceNo );

    if ( isObjReachable( ) )  {
        string remoteCallString;
        {
            ostringstream oss;
    
            oss << "DriveControl::setAzOffset( azArcmin = " << azArcmin
                << ", preferredSequenceNo = " << preferredSequenceNo
                << " ( nextSequenceNo = " << nextSequenceNo_ << " ) )";
    
            remoteCallString = oss.str( );
        }
        
        try {
            const double sendTime = Time::MJD();
            
            remoteObj( )->setAzOffset( azArcmin, nextSequenceNo_ );
          
            logSentCommandIfNeeded( remoteCallString, sendTime );
        }  catch ( const CORBA::Exception & ex ) {
            processException( remoteCallString, ex );
        }
    }
}


void
DriveHandle::setElOffset ( const double elArcmin,
                           const int preferredSequenceNo ) {

    setNextSequenceNo( preferredSequenceNo );

    if ( isObjReachable( ) )  {
        string remoteCallString;
        {
            ostringstream oss;
    
            oss << "DriveControl::setElOffset(elArcmin = " << elArcmin 
                << ", preferredSequenceNo = " << preferredSequenceNo
                << " (nextSequenceNo = " << nextSequenceNo_ << "))";
    
            remoteCallString = oss.str( );
        }
        
        try  {
            const double sendTime = Time::MJD();
                
            remoteObj( )->setElOffset( elArcmin, nextSequenceNo_ );
          
            logSentCommandIfNeeded( remoteCallString, sendTime );
        } catch ( const CORBA::Exception & ex) {
            processException( remoteCallString, ex );
        }
    }
}


void
DriveHandle::setAzel( const double azDegrees,
                      const double elDegrees,
                      const int preferredSequenceNo )
try {
    setNextSequenceNo( preferredSequenceNo );
    checkAgainstLimits(azDegrees, elDegrees, false);
    setSourceModeToAzEl( );
    
    if ( isObjReachable( ) )  {
        string remoteCallString;
        {
            ostringstream oss;
    
            oss << std::fixed 
                << "DriveControl::setAzel"
                << "(azDegrees = " << setprecision(3) << azDegrees
                << ", elDegrees = " << setprecision(3) << elDegrees
                << ", preferredSeqNo=" << preferredSequenceNo 
                << " (nextSeqNo =" << nextSequenceNo_ << "))";
    
            remoteCallString = oss.str( );
        }
        CARMA_CPTRACE(kTraceLevel, remoteCallString);
        
        try {
            const double sendTime = Time::MJD( );
            
            CARMA_CPTRACE(kTraceLevel, "trying to invoke setAzel..");
            remoteObj( )->setAzel( azDegrees, elDegrees, nextSequenceNo_ );
            CARMA_CPTRACE(kTraceLevel, "invoke setAzel success");
            
            logSentCommandIfNeeded( remoteCallString, sendTime );

            const EphemLock lock( gEphemGuard );

            localEphemeris_.setSource( Source( "AzelSource",
                                        Angle( azDegrees, services::DEGREES ),
                                        Angle( elDegrees, services::DEGREES ),
                                        ZERO_VEL,
                                        ZERO_DIST,
                                        services::COORDSYS_AZEL ) );
        } catch ( const CORBA::Exception & ex ) {
            programLogError(
                "Handling a CORBA::Exception within DriveHandle::setAzel."
            );

            processException( remoteCallString, ex );

            programLogError(
                "CORBA::Exception was stifled  within DriveHandle::setAzel."
            );
        } catch ( const BaseException & be ) {
            logException( remoteCallString, be.getMessage( ) );
            
            programLogError(
                "BaseException was stifled within DriveHandle::setAzel."
            );
        } catch ( ... ) {
            logException( remoteCallString, "< Unknown exception >" );
            
            programLogError(
                "Propagating an unknown exception within DriveHandle::setAzel."
            );

            throw;
        }
    }
} catch ( ... ) {
    programLogError( "Coming out of DriveHandle::setAzel on an exception." );
           
    throw;
}


void
DriveHandle::setAz( const double azDegrees, const int preferredSequenceNo ) {
    
    setNextSequenceNo( preferredSequenceNo );

    setSourceModeToAzEl( );
    
    if ( isObjReachable( ) )  {

        string remoteCallString;
        {
            ostringstream oss;
    
            oss << "DriveControl::setAz"
                << "(azDegrees=" << azDegrees 
                << ", preferredSequenceNo=" << preferredSequenceNo 
                << "(nextSequenceNo=" << nextSequenceNo_ << "))";
    
            remoteCallString = oss.str( );
        }
        
        try {
            const double sendTime = Time::MJD( );
            
            remoteObj( )->setAz( azDegrees, nextSequenceNo_ );
          
            logSentCommandIfNeeded( remoteCallString, sendTime );
            
            const EphemLock lock( gEphemGuard );
            const double elevation = localEphemeris_.getEl();

            localEphemeris_.setSource( Source( "AzelSource",
                                        Angle( azDegrees, services::DEGREES ),
                                        Angle( elevation, services::RADIANS ),
                                        ZERO_VEL,
                                        ZERO_DIST,
                                        services::COORDSYS_AZEL ) );
        } catch ( const CORBA::Exception & ex ) {
            processException( remoteCallString, ex );
        }
    }
}


void
DriveHandle::setEl( const double elDegrees, const int preferredSequenceNo ) {
    
    setNextSequenceNo( preferredSequenceNo );
    // This setSourceMode should be done even for non-reachable ants-
    // otherwise the DE is not updated, etc
    setSourceModeToAzEl( );
    
    if ( isObjReachable( ) )  {
        string remoteCallString;
        {
            ostringstream oss;
    
            oss << "DriveControl::setEl"
                << "(elDegrees=" << elDegrees 
                << ", preferredSequenceNo=" << preferredSequenceNo
                << "(nextSequenceNo=" << nextSequenceNo_ << " ))";
    
            remoteCallString = oss.str( );
        }
        
        try  {
            const double sendTime = Time::MJD();
            
            remoteObj( )->setEl( elDegrees, nextSequenceNo_ );
          
            logSentCommandIfNeeded( remoteCallString, sendTime );
            
            const double azimuth = localEphemeris_.getAz();
            const Angle az(azimuth, services::RADIANS);
            const Angle el(elDegrees, services::DEGREES);
            const Source source( "AzelSource", az, el, 
                                 ZERO_VEL, ZERO_DIST, services::COORDSYS_AZEL);

            const EphemLock lock( gEphemGuard );

            localEphemeris_.setSource(source);
        }  catch (const CORBA::Exception& ex)  {
            processException( remoteCallString, ex );
        }
    }
}

void 
DriveHandle::setMountOffset( const double azArcmin,
                             const double elArcmin,
                             const int preferredSequenceNo )
{
    setNextSequenceNo( preferredSequenceNo );
    
    if ( isObjReachable( ) )  {

        string remoteCallString;
        {
            ostringstream oss;
            oss << std::fixed
                << "DriveControl::setMountOffset"
                << "(azArcmin=" << setprecision(2) << azArcmin 
                << ", elArcmin=" << setprecision(2) << elArcmin 
                << ", seq=" << nextSequenceNo_ << ")";

            remoteCallString = oss.str();
        }
        
        try {
            const double sendTime = Time::MJD( );

            remoteObj( )->setMountOffset( azArcmin, elArcmin, nextSequenceNo_ );

            logSentCommandIfNeeded( remoteCallString, sendTime );
        }  catch (const CORBA::Exception& ex)  {
            processException( remoteCallString, ex );
        }
    }
}

void
DriveHandle::stow( antenna::common::DriveControl::Position position,
                   const int preferredSequenceNo )
{
    setNextSequenceNo( preferredSequenceNo );

    setSourceModeToAzEl( );

    if ( isObjReachable( ) ) {

        string remoteCallString;
        { 
            ostringstream oss;
            oss << "DriveControl::stow( position=";

            switch ( position ) {
                case antenna::common::DriveControl::ZENITH :
                    oss << "ZENITH";
                    break;
                case antenna::common::DriveControl::SERVICE :
                    oss << "SERVICE";
                    break;
                case antenna::common::DriveControl::SAFE :
                    if ( !setSafeRangeCompleted_ ) {
                        ostringstream os;
                        os << "Cannot stow Antenna C"<< carmaAntNo_ 
                            << " to safe position because safe azimuth and "
                            << "elevation ranges have not been defined. "
                            << "DriveControl::setSafeRange() must be called "
                            << "before moving to safe position.";

                        throw CARMA_ERROR( os.str() );
                    }
                    remoteCallString = "SAFE";
                    break;
                default :
                    remoteCallString = "< UNKNOWN!>";
                    break;
            }
            oss << ", preferredSequenceNo=" << preferredSequenceNo
                << " (nextSequenceNo=" << nextSequenceNo_ << ") )";

            remoteCallString = oss.str();
        }   

        try {
            const double sendTime = Time::MJD( );

            remoteObj( )->stow( position, nextSequenceNo_ );

            logSentCommandIfNeeded( remoteCallString, sendTime );

            const EphemLock lock( gEphemGuard );

            const double elevation = localEphemeris_.getEl();
            if ( position == antenna::common::DriveControl::ZENITH ) {
                localEphemeris_.setSource( Source( "STOW",
                            ZENITH_EL,
                            Angle( elevation, services::RADIANS ),
                            ZERO_VEL,
                            ZERO_DIST,
                            services::COORDSYS_AZEL ) );
            } else {
                const double azimuth = localEphemeris_.getEl();
                localEphemeris_.setSource( Source( "STOW",
                            Angle( azimuth, services::RADIANS ),
                            Angle( elevation, services::RADIANS ),
                            ZERO_VEL,
                            ZERO_DIST,
                            services::COORDSYS_AZEL ) );
            }
        } catch ( const CORBA::Exception & ex ) {
            processException( remoteCallString, ex );
        }
    }
}

void
DriveHandle::stop( ) {
    // This setSourceMode should be done even for non-reachable ants.
    setSourceModeToAzEl( );
    
    if ( isObjReachable( ) ) {
        const string remoteCallString = "DriveControl::stop()";

        try {
            const double sendTime = Time::MJD( );
            
            remoteObj( )->stop( );
          
            logSentCommandIfNeeded( remoteCallString, sendTime );

            const EphemLock lock( gEphemGuard );

            const double elevation = localEphemeris_.getEl();
            const double azimuth   = localEphemeris_.getAz();
            localEphemeris_.setSource ( Source( "STOP",
                                        Angle( azimuth, services::RADIANS ),
                                        Angle( elevation, services::RADIANS ),
                                        ZERO_VEL,
                                        ZERO_DIST,
                                        services::COORDSYS_AZEL ) );
        } catch ( const CORBA::Exception & ex ) {
            processException( remoteCallString, ex );
        }
    }
}


void 
DriveHandle::setSafeRange( float azLow, float azHigh, 
                           float elLow, float elHigh)
{
    // These calls will throw if the request safe range is
    // outside the physical limits, warn if outside
    // the software limits.
    updateWrapLimits();
    checkAgainstLimits( azLow,  elLow, true );
    checkAgainstLimits( azHigh, elHigh, true );

    if ( isObjReachable( ) ) {
        const string remoteCallString = "DriveControl::setSafeRange()";


        try {
            const double sendTime = Time::MJD( );
            remoteObj( )->setSafeRange( azLow, azHigh, elLow, elHigh );
            logSentCommandIfNeeded( remoteCallString, sendTime );
            {
                // not really an ephemeris lock, but we want a mutex
                // around this boolean because it conditionally tested
                // in stow(SAFE) above.
                const EphemLock lock( gEphemGuard );
                setSafeRangeCompleted_ = true;
            }
        } catch ( const CORBA::Exception & ex ) {
            // do not change setSafeRangeCompleted_ to false here.
            // because there may have been a previous successful
            // invocation.
            processException( remoteCallString, ex );
        }
    }
}

void
DriveHandle::setOvroMountPointingConstants( const double m1,
                                            const double m2,
                                            const double m3,
                                            const double m4,
                                            const double m5 ) {
    using ovro::DriveControl;
    
    if ( isObjReachable( ) ) {
        DriveControl::_var_type ovroRemoteObj =
            narrowedRemoteObj< DriveControl >( );

        if ( CORBA::is_nil( ovroRemoteObj ) == false ) {
            string remoteCallString;
            {
                ostringstream oss;
                
                oss << "ovro::DriveControl::setMountPointingConstants("
                    << m1 << ", "
                    << m2 << ", "
                    << m3 << ", "
                    << m4 << ", "
                    << m5 << ")";
                
                remoteCallString = oss.str( );
            }
            
            try {
                const double sendTime = Time::MJD( );
    
                ovroRemoteObj->setMountPointingConstants( m1, m2, m3, m4, m5 );
              
                logSentCommandIfNeeded( remoteCallString, sendTime );
            } catch ( const CORBA::Exception & ex ) {
                processException( remoteCallString, ex );
            }
        }
    }
}


void
DriveHandle::setBimaMountPointingConstants(
    const vector< double > dazCoefsVec,
    const vector< double > delCoefsVec )
{
    using bima::control::DriveControl;
    
    if ( isObjReachable( ) ) {
        DriveControl::_var_type bimaRemoteObj =
            narrowedRemoteObj< DriveControl >( );

        if ( CORBA::is_nil( bimaRemoteObj ) == false ) {
            typedef DriveControl::sequence_double SeqDouble;
            
            const SeqDouble dazCoefSeq =
                convertVectorToSequence< SeqDouble >( dazCoefsVec );
            
            const SeqDouble delCoefSeq =
                convertVectorToSequence< SeqDouble >( delCoefsVec );

            string remoteCallString;
            {
                ostringstream oss;
                
                oss << "bima::control::DriveControl::setPointingModelCoefs(< "
                    << delCoefSeq.length() << " elements >)";
                    
                remoteCallString = oss.str();
            }

            try {
                const double sendTime = Time::MJD( );
    
                bimaRemoteObj->setPointingModelCoefs( dazCoefSeq, delCoefSeq );
              
                logSentCommandIfNeeded( remoteCallString, sendTime );
            } catch ( const CORBA::Exception & ex ) {
                processException( remoteCallString, ex );
            }
        }
    }
}

void 
DriveHandle::setSzaMountPointingConstants(
    double azEncoderCountsPerTurn,     double elEncoderCountsPerTurn,
    double azMinEncoderCount, double azMaxEncoderCount,
    double elMinEncoderCount, double elMaxEncoderCount,
    double azEncoderZeroDegrees, double elEncoderZeroDegrees,
    double haTiltDegrees, double latTiltDegrees, double elTiltDegrees,
    double opticalXCollimationDegrees, double opticalYCollimationDegrees, 
    double opticalFlexureSinDegrees,   double opticalFlexureCosDegrees,
    double radioXCollimationDegrees,   double radioYCollimationDegrees, 
    double radioFlexureSinDegrees,     double radioFlexureCosDegrees)
{
  using sza::control::DriveControl;
  
  if ( isObjReachable( ) ) {
    DriveControl::_var_type szaRemoteObj =
      narrowedRemoteObj< DriveControl >( );
    
    if ( CORBA::is_nil( szaRemoteObj ) == false ) {
      string remoteCallString;
      {
        ostringstream oss;
        
        oss << "sza::control::DriveControl::setMountPointingConstants("
            << (unsigned long)azEncoderCountsPerTurn     << ", " <<  (unsigned long)elEncoderCountsPerTurn    << ", "
            << (unsigned long)azMinEncoderCount       << ", " <<  (unsigned long)azMaxEncoderCount   << ", "
            << (unsigned long)elMinEncoderCount       << ", " <<  (unsigned long)elMaxEncoderCount   << ", "
            << azEncoderZeroDegrees<< ", " <<  elEncoderZeroDegrees      << ", "
            << haTiltDegrees << ", " << latTiltDegrees << ", " << elTiltDegrees << ", "
            << opticalXCollimationDegrees << ", " << opticalYCollimationDegrees << ", "
            << opticalFlexureSinDegrees << ", " << opticalFlexureCosDegrees   << ", "
            << radioXCollimationDegrees << ", " << radioYCollimationDegrees   << ", "
            << radioFlexureSinDegrees << ", " << radioFlexureCosDegrees
            << ")";
        
        remoteCallString = oss.str( );
      }
      
      try {
        const double sendTime = Time::MJD( );
        
        szaRemoteObj->setMountPointingConstants(
           (unsigned long)azEncoderCountsPerTurn, (unsigned long)elEncoderCountsPerTurn,
           (unsigned long)azMinEncoderCount, (unsigned long)azMaxEncoderCount,
           (unsigned long)elMinEncoderCount, (unsigned long)elMaxEncoderCount,
           azEncoderZeroDegrees, elEncoderZeroDegrees,
           haTiltDegrees, latTiltDegrees, elTiltDegrees,
           opticalXCollimationDegrees, opticalYCollimationDegrees,
           opticalFlexureSinDegrees,   opticalFlexureCosDegrees,
           radioXCollimationDegrees,   radioYCollimationDegrees,
           radioFlexureSinDegrees, radioFlexureCosDegrees);
        
        logSentCommandIfNeeded( remoteCallString, sendTime );
      } catch ( const CORBA::Exception & ex ) {
        processException( remoteCallString, ex );
      }
    }
  }
}

/**.......................................................................
 * Set Encoder limits for the SZA antennas
 */
void 
DriveHandle::setSzaEncoderLimits(double azMinEncoderCount,    double azMaxEncoderCount,
    double elMinEncoderCount, double elMaxEncoderCount)
{
  using sza::control::DriveControl;
  
  if ( isObjReachable( ) ) {
    DriveControl::_var_type szaRemoteObj =
      narrowedRemoteObj< DriveControl >( );
    
    if ( CORBA::is_nil( szaRemoteObj ) == false ) {
      string remoteCallString;
      {
        ostringstream oss;
        
        oss << "sza::control::DriveControl::setEncoderLimits("
            << (unsigned long)azMinEncoderCount       << ", " <<  (unsigned long)azMaxEncoderCount    << ", "
            << (unsigned long)elMinEncoderCount       << ", " <<  (unsigned long)elMaxEncoderCount
            << ")";
        
        remoteCallString = oss.str( );
      }
      
      try {
        const double sendTime = Time::MJD( );
        szaRemoteObj->setEncoderLimits((unsigned long)azMinEncoderCount,  (unsigned long)azMaxEncoderCount,
                           (unsigned long)elMinEncoderCount,  (unsigned long)elMaxEncoderCount);
        logSentCommandIfNeeded( remoteCallString, sendTime );
      } catch ( const CORBA::Exception & ex ) {
        processException( remoteCallString, ex );
      }
    }
  }
}

/**.......................................................................
 * Set Encoder zero points for the SZA antennas
 */
void DriveHandle::setSzaEncoderZeros(double azEncoderZeroDegrees, double elEncoderZeroDegrees)
{
  using sza::control::DriveControl;
  
  if ( isObjReachable( ) ) {
    DriveControl::_var_type szaRemoteObj =
      narrowedRemoteObj< DriveControl >( );
    
    if ( CORBA::is_nil( szaRemoteObj ) == false ) {
      string remoteCallString;
      {
        ostringstream oss;
        oss << "sza::control::DriveControl::setEncoderZeros(" << azEncoderZeroDegrees << ", " << elEncoderZeroDegrees << ")";
        remoteCallString = oss.str( );
      }
      
      try {
        const double sendTime = Time::MJD( );
        szaRemoteObj->setEncoderZeros(azEncoderZeroDegrees, elEncoderZeroDegrees);
        logSentCommandIfNeeded( remoteCallString, sendTime );
      } catch ( const CORBA::Exception & ex ) {
        processException( remoteCallString, ex );
      }
    }
  }
}

/**.......................................................................
 * Set tilts for the SZA antennas
 */
void DriveHandle::setSzaTilts(double haTiltDegrees, double latTiltDegrees, double elTiltDegrees)
{
  using sza::control::DriveControl;
  
  if ( isObjReachable( ) ) {
    DriveControl::_var_type szaRemoteObj =
      narrowedRemoteObj< DriveControl >( );
    
    if ( CORBA::is_nil( szaRemoteObj ) == false ) {
      string remoteCallString;
      {
        ostringstream oss;
        oss << "sza::control::DriveControl::setTilts(" << haTiltDegrees << ", " <<  latTiltDegrees << ", " << elTiltDegrees << ")";
        remoteCallString = oss.str( );
      }
      
      try {
        const double sendTime = Time::MJD( );
        szaRemoteObj->setTilts(haTiltDegrees, latTiltDegrees, elTiltDegrees);
        logSentCommandIfNeeded( remoteCallString, sendTime );
      } catch ( const CORBA::Exception & ex ) {
        processException( remoteCallString, ex );
      }
    }
  }
}

unsigned short
DriveHandle::getCarmaAntennaNo( ) const {
    return carmaAntNo_ ;
}


string
DriveHandle::getCarmaAntennaName( ) const {
    return computeCarmaAntennaName( carmaAntNo_ );
}


string
DriveHandle::getTypedAntennaName( ) const {
    return computeTypedAntennaName( carmaAntNo_ );
}


void
DriveHandle::updateAntennaSourceRaDec( const double mjd ) 
{
    const string source = getSourceName();
    
    string params;
    {
        ostringstream oss;
        oss << "source= " << source << " mjd=" 
            << fixed << setprecision(9) << mjd;        
        params = oss.str();
    }
    
    const string callString =
        "DriveHandle::updateAntennaSourceRaDec(" + params + ")";

    try {
        double ra, dec;
        {
            const EphemLock lock( gEphemGuard );

            localEphemeris_.setMJD (mjd);
            // getRa/Dec can throw EphemerisException
            // recomputed topocentric RA, radians
            ra  = localEphemeris_.getRa();  
            // recomputed topocentric Dec, radians
            dec = localEphemeris_.getDec(); 
        }
        
        // Reset beginning of wrap interval here so that
        // we don't have big slews when crossing
        // into a forbidden zone from a positive, decreasing
        // azimuth.  This will keep the antenna tracking
        // into a negative azimuth instead of wrapping back
        // to 360 (if and only if lastControlWrapMode_ == control::TIME).
        // @see bug 571
        optimumWrapTime_ = Time::MJD();

        updateRaDec( source, mjd, ra, dec, lastControlWrapMode_ );

        CARMA_CPTRACE(Trace::TRACE7, "DriveHandle::updateAntennaSourceRaDec"
          << " Finished updating antenna of type "
          << computeAntennaTypeName( carmaAntNo_ )
          << " with carma antenna number of " << carmaAntNo_
          << " with parameters " << params );

    } catch (const EphemerisException& ee ) {
        logException( callString, ee.getMessage( ) );

        programLogError(
            "EphemerisException was stifled in DriveHandle::updateAntennaSourceRaDec"
        );
    }
}


void
DriveHandle::updateTracking(double mjdAnt)
{

    const ScopedLogNdc ndc( "DriveHandle::updateTracking" );
    switch ( sourceMode_ )  {
        case SOURCE_MODE_RA_DEC :  
            updateAntennaSourceRaDec(mjdAnt);
            break;

        case SOURCE_MODE_AZ_EL :  
            programLogInfoIfPossible("Azel mode - no update needed") ;
            break;

        case SOURCE_MODE_IDLE : 
            programLogInfoIfPossible("Idle mode - no update needed") ;
            break;

        case SOURCE_MODE_INVALID : 
        default :  
            /* we shouldn't be here in any circumstance */
            programLogErrorIfPossible( "Bad source mode" );
            break;
    }
}


void
DriveHandle::updateWeather(
    const double      mjd,
    const Temperature & atmTemp,
    const Pressure    & atmPressure,
    const double      relHumid,
    const Temperature & dewpointTemp, 
    const Velocity    & windSpeed,
    const Angle       & windDirection,
    const bool        logGoodSend )
{
    if ( isObjReachable( ) ) {
        const float ambientTempCelsius = atmTemp.celsius();
        const float barPressure = atmPressure.millibar();
        const float relHumidFloat = relHumid;
        const float dewpointCelsius = dewpointTemp.celsius();
        const float windSpeedMph = windSpeed.mph();
        const float windDirDeg = windDirection.degrees();
        
        string remoteCallString;
        {
            ostringstream oss;

            oss  << " ambientTemp (C) = " << ambientTempCelsius
                 << " barPressure (mbars) = " << atmPressure.millibar()
                 << " humidity (%) = " << relHumidFloat
                 << " dewpointTemp (C) = " << dewpointCelsius
                 << " windSpeed (mph) = " << windSpeedMph
                 << " windDirection (d) = " << windDirDeg
                 << ")";
                 
            remoteCallString = oss.str( );
        }
             
        try {
            const double sendTime = Time::MJD();
            
           remoteObj( )->updateWeather( ambientTempCelsius,
                                        barPressure,
                                        relHumidFloat,
                                        dewpointCelsius,
                                        windSpeedMph,
                                        windDirDeg );

            if ( logGoodSend )
                logSentCommand( remoteCallString, sendTime );
        } catch ( const CORBA::Exception & ex ) {
            processException( remoteCallString, ex );
        }
    }
}
        

string
DriveHandle::getSourceName( ) 
{
    const EphemLock lock( gEphemGuard );
    return localEphemeris_.getSource().getName();
}

Source 
DriveHandle::getSource( ) 
{
    const EphemLock lock( gEphemGuard );
    return localEphemeris_.getSource();
}


DriveHandle::SourceMode
DriveHandle::getSourceMode( ) const 
{
    return sourceMode_;
}


void
DriveHandle::setSourceModeToIdle( ) {
    sourceMode_ = SOURCE_MODE_IDLE;
    antenna_.trackMode().setValue(TrackModeEnum::IDLE);
}


void DriveHandle::setSourceModeToRaDec( ) {
    sourceMode_ = SOURCE_MODE_RA_DEC;
    antenna_.trackMode().setValue(TrackModeEnum::SOURCE);
}


void
DriveHandle::setSourceModeToAzEl( ) {
    sourceMode_ = SOURCE_MODE_AZ_EL;
    antenna_.trackMode( ).setValue(TrackModeEnum::AZEL);
}


void DriveHandle::checkModeForEquatOffset( void )
{
    ostringstream prefixOs;
    prefixOs << "Antenna " << getCarmaAntennaNo();
    const string prefix = prefixOs.str();

    const string sourceName = getSourceName();

    if ( StringUtils::equalsIgnoreCase( sourceName, "NONE") ) {
        ostringstream os;
        os << prefix
           << " has no current source. You must call track on it"
           << " before setting equatorial offsets.";
        throw CARMA_ERROR( os.str() );
    }

    if (    sourceMode_ == SOURCE_MODE_AZ_EL 
         || sourceMode_ == SOURCE_MODE_INVALID ) {
        ostringstream os;
        os << prefix
           << " is not in RA/DEC mode so cannot set equatorial offsets."
           << " You need to track a source with this antenna first.";
        throw CARMA_ERROR( os.str() );
    }

}

void
DriveHandle::setEquatOffset( const double ra,
                             const double dec ) 
{

    const string methodName = "DriveHandle::setEquatOffset";
    const ScopedLogNdc ndc( methodName );
    
    checkModeForEquatOffset();

    ostringstream prefixOs;
    prefixOs << "Antenna " << getCarmaAntennaNo();
    const string prefix = prefixOs.str();

    if ( sourceMode_ == SOURCE_MODE_IDLE ) {
        ostringstream os;
        os << prefix
           << " is in IDLE mode. Therefore, equatorial offsets will get"
           << " zeroed out when track() is next issued. ";
        programLogWarnIfPossible( os.str() );
    }

    Angle raOffAngle(ra,"arcmin");
    Angle decOffAngle(dec,"arcmin");
    // use false here so that negative angles stay negative.
    raTrackingOffset_  = raOffAngle.radians(false);
    decTrackingOffset_ = decOffAngle.radians(false);

    // change the tracking offsets
    const EphemLock lock( gEphemGuard );

    localEphemeris_.setRaDecOffsets(raTrackingOffset_,decTrackingOffset_);
}


void
DriveHandle::conditionallyUpdateTrackTolerance( const double freq )
{
    // Fractional freq change required to force an update
    const double freqThreshold = 0.03;  
    if (freq < 1.0) return; // Just in case
    if (abs(freq - trackToleranceFreq_)/freq < freqThreshold) return;
    updateTrackTolerance(trackTolerance_, freq);
}


void
DriveHandle::updateTrackTolerance( const float  tolerance, 
                                   const double freq )
{    
    trackTolerance_ = tolerance;
    
    if ( freq < 1.0 )
        return; // Just in case
        
    trackToleranceFreq_ = freq;
    
    if ( isObjReachable( ) ) {
        const float dia = antenna_.diameter( ).getValue( );
        
        const Angle hpbwAngle( (constants::Physical::C / (freq * 1e9) / dia),
                               services::RADIANS );
        
        const float toleranceArcsec = tolerance * hpbwAngle.arcSeconds( );
    
        string remoteCallString;
        {
            ostringstream oss;
    
            oss << "DriveControl::setTolerance(" << toleranceArcsec << ")";
    
            remoteCallString = oss.str( );
        }
    
        try {
            const double sendTime = Time::MJD();

            remoteObj( )->setTolerance( toleranceArcsec );

            logSentCommandIfNeeded( remoteCallString, sendTime );
        } catch ( const CORBA::Exception & ex ) {
            processException( remoteCallString, ex );
        }
    }
}

void DriveHandle::setNextSequenceNo( const int preferredSequenceNo ) 
{
    monitorSystem_.readNewest(); // Get to the top of the queue

    const int currentSeqNo = getAntennaCommon( carmaAntNo_, monitorSystem_ ).
                    drive().driveSeqNum().getValue();

    if ( currentSeqNo == preferredSequenceNo ) {
        nextSequenceNo_ = preferredSequenceNo + 10;
    } else {
        nextSequenceNo_ = preferredSequenceNo;
    }
}

bool
DriveHandle::isActionComplete( const MonitorSystem & monsys,
                               const int             monErrLimit )
{
    const bool debug = true;
    const MonitorPointInt & completionMP =
        getAntennaCommon(carmaAntNo_, monsys).drive().driveSeqNum();
    
    const int oldErrLimit = errLimit_;
    const int oldConsecErrors = consecutiveErrors_;

    // Non-zero limit triggers resetting
    if (monErrLimit > 0) {
        errLimit_ = monErrLimit;
        consecutiveErrors_ = 0;
    }
    
    if (!completionMP.isValid()) {
        consecutiveErrors_++;
        if (consecutiveErrors_ >= errLimit_) {
            ostringstream o;

            o << "isActionComplete: " 
              << "number of consecutive invalid monitor frames "
              << "equals limit of "
              << errLimit_;
              
            o << " ("
              << "monErrLimit=" << monErrLimit << ", "
              << "oldErrLimit=" << oldErrLimit << ", "
              << "errLimit_ =" << errLimit_ << ", "
              << "oldConsecErrors=" << oldConsecErrors << ", "
              << "consecutiveErrors_=" << consecutiveErrors_ << ")";

            throw CARMA_ERROR(o);
        }
                   
        return false;
    }
    
    // Valid monitor point
    if (debug && (consecutiveErrors_ > 0)) {
        ostringstream o;
        o << "isActionComplete: ant#" << carmaAntNo_ 
          << " had "<< consecutiveErrors_
          << " consecutive invalid monitor frames";
        Category& log = Program::getLogger();
        log << Priority::INFO << o.str();
    }
    consecutiveErrors_ = 0;
    
    return (completionMP.getValue() == nextSequenceNo_);
}


namespace {


struct ::timespec
getFutureTime( const struct ::timeval & absBase,
               const double             seconds )
{
    const long long kNanosPerSec = 1000LL * 1000LL * 1000LL;
    const long long kNanosPerMicro = 1000LL;
    
    const long long absNanos =
        absBase.tv_sec * kNanosPerSec +
        absBase.tv_usec * kNanosPerMicro +
        static_cast< long long >( floor( seconds * kNanosPerSec ) );
                               
    const long long absSecs = (absNanos / kNanosPerSec);

    struct ::timespec absResult;

    absResult.tv_sec = absSecs;
    absResult.tv_nsec = absNanos - (absSecs * kNanosPerSec);

    return absResult;
}


}  // namespace < anonymous >


void
DriveHandle::test( const double aSeconds,
                   const double bSeconds,
                   const long   whichTest )
{
    struct ::timeval absBase;

    ::gettimeofday( &absBase, 0 );

    const double delaySeconds = aSeconds * getCarmaAntennaNo() + bSeconds;

    {
        ostringstream oss;
        
        oss << "Delay is " << delaySeconds << " seconds";
            
        programLogInfoIfPossible( oss.str() );
    }

    const struct ::timespec absTimeout =
        getFutureTime( absBase, delaySeconds );
    
    {
        PthreadMutex mutex;
        PthreadCond cond;
        
        const ScopedLock< PthreadMutex > lock( mutex );
        
        while ( true ) {
            if ( cond.TimedWait( mutex, absTimeout ) == false )
                break;
        }
    }

    if ( whichTest == 1 )
        throw CARMA_ERROR( "Deliberately requested ErrorException" );

    {
        ostringstream oss;
        
        oss << "Completed at " << Time::getTimeString( 3 )
            << " after delaying " << delaySeconds << " seconds";
            
        programLogInfoIfPossible( oss.str() );
    }
}


const string 
DriveHandle::azWrapModeToString( control::AzWrapMode mode ) const
{
    switch( mode ) {
        default:
        case control::ZERO:
            return string("ZERO");
        case control::ADD:
            return string("ADD");
        case control::SUB:
            return string("SUB");
        case control::TIME :
            return string("TIME");
    }
    
}


const string 
DriveHandle::azWrapModeToString( antenna::common::DriveControl::AzWrapMode mode ) const
{
    switch( mode ) {
        default:
        case antenna::common::DriveControl::ZERO:
            return string("ZERO");
        case antenna::common::DriveControl::ADD:
            return string("ADD");
        case antenna::common::DriveControl::SUB:
            return string("SUB");
    }
}


antenna::common::DriveControl::AzWrapMode
DriveHandle::computeOptimumWrapValue()
{
    ScopedLogNdc ndc("DriveHandle::computeOptimumWrapValue");

    // Keep this line before the lock below, 
    // as updateWrapLimits also grabs and releases the lock!
    updateWrapLimits();

    const EphemLock lock( gEphemGuard );

    // Determine where the antenna is currently pointed.
    // a number between negative and positive az wrap limits.
    // If the validity is bad on this read
    MonitorPointDouble & azMp = getAntennaCommon( carmaAntNo_, monitorSystem_ )
                           .drive().track().actualAzimuth();
    MonitorPoint::VALIDITY validity = azMp.getValidity();
    // Check the validity of the read azimuth.  If it is not at least
    // VALID_NOT_CHECKED, return the current wrap mode unchanged.
    if (    validity == MonitorPoint::INVALID_HW_BAD
         || validity == MonitorPoint::INVALID_NO_DATA
         || validity == MonitorPoint::INVALID_NO_HW
       ) 
    {
        ostringstream os;
        os << "Azimuth monitor point was not valid. "
           << "Validity value was " << azMp.validityToString(validity)
           << ". Returning current wrap mode: "
           << azWrapModeToString( lastAntWrapMode_ );
        programLogWarnIfPossible( os.str() );
        return lastAntWrapMode_;
    }

    double antAzDegrees = azMp.getValue();
    checker_.setMJD( optimumWrapTime_ );
    AzWrapType wrap = checker_.computeOptimumWrapValue( 
                       controlAntTypeToServicesAntType( antType_ ),
                       antAzDegrees,
                       timeToTrack_ );

    return servicesWrapToDriveWrap( wrap );
}


antenna::common::DriveControl::AzWrapMode
DriveHandle::controlWrapToDriveWrap( control::AzWrapMode mode ) 
{
    switch ( mode ) {
        default:
        case control::ZERO :
            lastControlWrapMode_ = control::ZERO;
            return antenna::common::DriveControl::ZERO;
        case control::ADD :
            // Never send ADD to OVRO antennas because
            // those antenna types do not have enough
            // physical wrap to support an add of a full turn.
            // So use TIME instead
            switch( antType_ ) {
                default:
                case ANTENNA_TYPE_BIMA :
                case ANTENNA_TYPE_SZA :
                    lastControlWrapMode_ = control::ADD;
                    return antenna::common::DriveControl::ADD;
                case ANTENNA_TYPE_OVRO :
                    // don't set lastControlWrapMode_ here as recursive
                    // call will set it.
                    return controlWrapToDriveWrap( control::TIME );
            }
        case control::SUB :
            lastControlWrapMode_ = control::SUB;
            return antenna::common::DriveControl::SUB ;
        case control::TIME :
            antenna::common::DriveControl::AzWrapMode optMode =
                computeOptimumWrapValue();

            // Set lastControlWrapMode_ to TIME here so that
            // invocations of updateTracking-->setRaDec
            // from the SubarrayTrackerThread re-evaluate
            // the optimum wrap regardless of whether
            // we have surpassed the original optimumWrapTime+trackTime.
            // This is required so that we don't do unwanted
            // long slews at azimuth crossings, between python level calls to 
            // track().
            lastControlWrapMode_ = control::TIME;
            return optMode;
    }
}


carma::control::AzWrapMode
DriveHandle::driveWrapToControlWrap( antenna::common::DriveControl::AzWrapMode mode ) const
{
    switch ( mode ) {
        default:
        case antenna::common::DriveControl::ZERO:
            return control::ZERO;
        case antenna::common::DriveControl::ADD:
            return control::ADD;
        case antenna::common::DriveControl::SUB:
            return control::SUB;
    }
}


void
DriveHandle::updateWrapLimits()
{
    // Note: if any one of the limits queries fail, we get the defaults 
    // for all limits. This is easier than a separate try-catch-default for
    // each value.  Besides, chances are if any of them are unavailable, 
    // then all will be unavailable to due say subsystem being unavailable.
    try {
        // Keep lock inside try scope; because defaultWrapLimits() in catch
        // scope also tries to grab this lock.
        const EphemLock lock( gEphemGuard );

        AntennaCommon::Limit & limit = 
            getAntennaCommon( carmaAntNo_, monitorSystem_ ).drive().limit();

        float value = limit.azHighSwLimitVal().getValue();
        checker_.setAzPositiveWrapLimit( Angle( value, services::DEGREES ) );

        value = limit.azLowSwLimitVal().getValue();
        checker_.setAzNegativeWrapLimit( Angle( value, services::DEGREES ) );

        value = limit.elLowSwLimitVal().getValue();
        checker_.setElevLimit( Angle( value, services::DEGREES ) );

        value = limit.elHighSwLimitVal().getValue();
        checker_.setElevUpperLimit( Angle( value, services::DEGREES ) );

    } catch ( ... ) {
        ostringstream os;
        os << "Unable to get wrap limits for antenna " << carmaAntNo_ << ". Using default values.";
        programLogWarnIfPossible( os.str() );
        defaultWrapLimits();
    }
}

void
DriveHandle::checkAgainstLimits(const double azDegrees, 
                                const double elDegrees, 
                                const bool warnOnly )
{    
    // Throw Exception if we will go past hw limits.
    // Warn if we will go past sw limits.
    const AntennaCommon::Limit & limit = 
            getAntennaCommon( carmaAntNo_, monitorSystem_ ).drive().limit();

    float azMaxHw = limit.azHighHwLimitVal().getValue();
    float azMinHw = limit.azLowHwLimitVal().getValue();
    float azMaxSw = limit.azHighSwLimitVal().getValue();
    float azMinSw = limit.azLowSwLimitVal().getValue();
    float elMaxHw = limit.elHighHwLimitVal().getValue();
    float elMinHw = limit.elLowHwLimitVal().getValue();
    float elMaxSw = limit.elHighSwLimitVal().getValue();
    float elMinSw = limit.elLowSwLimitVal().getValue();
    bool  valid = true;
    valid = valid && limit.azHighHwLimitVal().isValid();
    valid = valid && limit.azLowHwLimitVal().isValid();
    valid = valid && limit.azHighSwLimitVal().isValid();
    valid = valid && limit.azLowSwLimitVal().isValid();
    valid = valid && limit.elHighHwLimitVal().isValid();
    valid = valid && limit.elLowHwLimitVal().isValid();
    valid = valid && limit.elHighSwLimitVal().isValid();
    valid = valid && limit.elLowSwLimitVal().isValid();
    
    // If not valid put in some default values
    if (!valid) {
        if (carmaAntNo_ < 7) {
            azMinHw = OVRO_NEGATIVE_AZ_LIMIT.degrees();
            azMinSw = OVRO_NEGATIVE_AZ_LIMIT.degrees();
            azMaxHw = OVRO_POSITIVE_AZ_LIMIT.degrees();
            azMaxSw = OVRO_POSITIVE_AZ_LIMIT.degrees();
            elMinHw = OVRO_LOWER_ELEV_LIMIT.degrees();
            elMinSw = OVRO_LOWER_ELEV_LIMIT.degrees();
            elMaxSw = OVRO_UPPER_ELEV_LIMIT.degrees();
            elMaxHw = HW_UPPER_ELEV_LIMIT.degrees();
        }    
        else if (carmaAntNo_ < 16) {
            azMinHw = BIMA_NEGATIVE_AZ_LIMIT.degrees();
            azMinSw = BIMA_NEGATIVE_AZ_LIMIT.degrees();
            azMaxHw = BIMA_POSITIVE_AZ_LIMIT.degrees();
            azMaxSw = BIMA_POSITIVE_AZ_LIMIT.degrees();
            elMinHw = BIMA_LOWER_ELEV_LIMIT.degrees();
            elMinSw = BIMA_LOWER_ELEV_LIMIT.degrees();
            elMaxSw = BIMA_UPPER_ELEV_LIMIT.degrees();
            elMaxHw = HW_UPPER_ELEV_LIMIT.degrees();
        }    
        else {
            azMinHw = SZA_NEGATIVE_AZ_LIMIT.degrees();
            azMinSw = SZA_NEGATIVE_AZ_LIMIT.degrees();
            azMaxHw = SZA_POSITIVE_AZ_LIMIT.degrees();
            azMaxSw = SZA_POSITIVE_AZ_LIMIT.degrees();
            elMinHw = SZA_LOWER_ELEV_LIMIT.degrees();
            elMinSw = SZA_LOWER_ELEV_LIMIT.degrees();
            elMaxSw = SZA_UPPER_ELEV_LIMIT.degrees();
            elMaxHw = SZA_UPPER_ELEV_LIMIT.degrees();
        }    
    }    
    {
        if ( (azDegrees < azMinHw) || (azDegrees > azMaxHw) ) {
            ostringstream os;
            os  << fixed << setprecision(1)
                << "Requested azimuth " << azDegrees 
                << " for C" << carmaAntNo_
                << " outside hardware limit range ["
                << azMinHw << "," << azMaxHw << "]";
            if (warnOnly) {
                programLogWarnIfPossible( os.str() );
            }
            else  {
                throw CARMA_ERROR(os);
            }
        }

        if ( elDegrees < elMinHw || elDegrees > elMaxHw ) {
            ostringstream os;
            os  << fixed << setprecision(1)
                << "Requested elevation " << elDegrees 
                << " for C" << carmaAntNo_
                << " outside hardware limit range ["
                << elMinHw << "," << elMaxHw << "]";
            if (warnOnly) programLogWarnIfPossible( os.str() );
            else throw CARMA_ERROR(os);
        }
    }

    {
        if ( azDegrees < azMinSw || azDegrees > azMaxSw ) {
            ostringstream os;
            os  << fixed << setprecision(1)
                << "Requested azimuth " << azDegrees 
                << " for C" << carmaAntNo_
                << " outside software limit range ["
                << azMinSw << "," << azMaxSw << "]";
            programLogWarnIfPossible( os.str() );
        }
        if ( elDegrees < elMinSw || elDegrees > elMaxSw ) {
            ostringstream os;
            os  << fixed << setprecision(1)
                << "Requested elevation " << elDegrees 
                << " for C" << carmaAntNo_
                << " outside software limit range ["
                << elMinSw << "," << elMaxSw << "]";
            programLogWarnIfPossible( os.str() );
        }
    }
}


void 
DriveHandle::defaultWrapLimits()
{
    const EphemLock lock( gEphemGuard );


    switch( antType_ ) {
        default:
        case ANTENNA_TYPE_SZA :
            checker_.setElevUpperLimit( SZA_UPPER_ELEV_LIMIT );
            checker_.setAzPositiveWrapLimit(SZA_POSITIVE_AZ_LIMIT);
            checker_.setAzNegativeWrapLimit(SZA_NEGATIVE_AZ_LIMIT);
            checker_.setElevLimit(SZA_LOWER_ELEV_LIMIT);
            break;
            break;
        case ANTENNA_TYPE_BIMA :
            checker_.setElevUpperLimit( BIMA_UPPER_ELEV_LIMIT );
            checker_.setAzPositiveWrapLimit( BIMA_POSITIVE_AZ_LIMIT );
            checker_.setAzNegativeWrapLimit( BIMA_NEGATIVE_AZ_LIMIT );
            checker_.setElevLimit( BIMA_LOWER_ELEV_LIMIT );
            break;
        case ANTENNA_TYPE_OVRO :
            checker_.setElevUpperLimit( OVRO_UPPER_ELEV_LIMIT );
            checker_.setAzPositiveWrapLimit( OVRO_POSITIVE_AZ_LIMIT );
            checker_.setAzNegativeWrapLimit( OVRO_NEGATIVE_AZ_LIMIT );
            checker_.setElevLimit( OVRO_LOWER_ELEV_LIMIT );
            break;
    }
}


services::AntennaType
DriveHandle::controlAntTypeToServicesAntType( control::AntennaType antType ) const
{
    switch( antType ) {
        case ANTENNA_TYPE_SZA :
            return services::ANT_TYPE_SZA;
        case ANTENNA_TYPE_BIMA :
            return services::ANT_TYPE_BIMA;
        default:
        case ANTENNA_TYPE_OVRO :
            return services::ANT_TYPE_OVRO;
    }
}


antenna::common::DriveControl::AzWrapMode
DriveHandle::servicesWrapToDriveWrap( services::AzWrapType wrap ) const
{
    switch( wrap ) {
        case services::AZWRAP_ADD :
            return antenna::common::DriveControl::ADD;
        case services::AZWRAP_SUB :
            return antenna::common::DriveControl::SUB;
        default:
        case services::AZWRAP_ZERO:
            return antenna::common::DriveControl::ZERO;
    }
}
