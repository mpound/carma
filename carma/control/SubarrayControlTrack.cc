/**
 *
 * Carma control interface server implementation for the track command,
 * and equatorial offsets for the antennas and the phase center.
 *
 * @author: Marc Pound
 *
 * $Id: SubarrayControlTrack.cc,v 1.190 2014/05/05 21:40:42 scott Exp $
 *
 * $CarmaCopyright$
 *
 */

// NOTE: If you get compile time errors on makeHandleMethodFunctorGroup
// calls, check the number of parameters in your invocation.
// HandleMethodFunctorGroup.h only defines templates for
// up to 9 parameters and you'll get a error message:
// "no matching function call to to makeHandleMethodFunctorGroup(...) ...".
// Note also you can get a similar error if you try to use references
// anywhere in the method signature which you are dispatching.


#include "carma/control/SubarrayControlImpl.h"

#include <iomanip>
#include <sstream>
#include <string>

#include "carma/corba/corba.h"
#include "carma/control/AntennaControls.h"
#include "carma/control/DriveHandle.h"
#include "carma/control/WorkerPool.h"
#include "carma/control/FunctorWorkRequest.h"
#include "carma/control/HandleMethodFunctorGroup.h"
#include "carma/control/SatThreadSync.h"
#include "carma/control/SubarrayTrackerThread.h"
#include "carma/monitor/SlPipelineSubsystem.h"
#include "carma/services/Astro.h"
#include "carma/services/Ephemeris.h"
#include "carma/services/Frequency.h"
#include "carma/services/Physical.h"
#include "carma/services/stringConstants.h"
#include "carma/services/Source.h"
#include "carma/services/Types.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/FileUtils.h"
#include "carma/util/FileNotFoundException.h"
#include "carma/util/Logger.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedQILockManager.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"
#include "carma/util/WorkResult.h"


using namespace ::std;
using namespace log4cpp;
using namespace CORBA;
using namespace carma;
using namespace carma::antenna;
using namespace carma::antenna::common;
using namespace carma::control;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::services::constants;
using namespace carma::util;


namespace {

const Trace::TraceLevel kTraceLevel = Trace::TRACE7;


const string kConciseMjdStringDateFormat = "%d%b%y";


// getConciseStringForMjd will only put in the date if the absolute
// difference between mjd and refMjd is greater than the given tolerance
// The tolerance defaults to approximately 1 hour.
string
getConciseMjdString( const double mjd,
                     const double refMjd,
                     const int    precision = 1,
                     const double dateMjdTolerance = 0.04166667 )
{
    if ( fabs( mjd - refMjd ) > dateMjdTolerance ) {
        return Time::getDateTimeString( mjd,
                                        precision,
                                        kConciseMjdStringDateFormat );
    } else {
        return Time::getTimeString( mjd,
                                    precision );
    }
}


}  // namespace < anonymous >


void
SubarrayControlImpl::trackDriveGroup(
            const string &     sourceName,
            const DriveGroup & driveGroup,
            const bool         affectPhaseCenter,
            const carma::control::AzWrapMode azWrapMode,
            const double       time,
            const bool         overTheTop,
            const bool         failIfIntegrating )
{
    ++nextDriveSeqNo_; // Preferred seq no

    const string prefix = "SubarrayControlImpl::trackDriveGroup ";
    ScopedLogNdc ndc(prefix);

    // Don't allow track if an integration is in progress
    {
        if ( subarrayIsIntegrating() && failIfIntegrating ) {
            ostringstream oss;

            oss << "Track command not allowed while an integration is "
                << "still running;\n  issue a cancel to stop integration";

            throw CARMA_ERROR(oss);
        }
    }

    // Don't allow track if subarray is not initialized
    if ( !initializationFlag_ ) {
       ostringstream oss;
        oss << "Subarray has not been initialized; issue an "
            << "initialize() command in the sac";
        throw CARMA_ERROR(oss);
    }


    CARMA_CPTRACE( kTraceLevel, "source = " << sourceName);
    CARMA_CPTRACE( kTraceLevel, "calling getAlignedAntUpdateTimes()" );


    const double now = Time::MJD() ;
    const MjdTriplet mjdTriplet = getAlignedAntUpdateTimes( now );

    const bool fixed = arrayRefEphem_->isFixed( sourceName );

    // if we got here via trackSingle() then don't update
    // any subarray-wide monitor points, and don't monkey
    // with the Ephemeris instance used for such purposes.
    if ( affectPhaseCenter ) {
        const double newLastAntUpdateMjd = computeTruncatedAntUpdateTime(now);

        trackerThread_->setLastAntUpdate( newLastAntUpdateMjd );

        // going to a fixed source, e.g. transmitter, turn off
        // doppler tracking.  But not for a trackSingle command.
        if ( fixed ) {
            CARMA_CPTRACE( kTraceLevel, "fixed source: turning off doppler" );
            doppler("none");
        }
        // Update the doppler tracking frequencies, even
        // if this observed source is not the doppler source,
        // because the interpolator time-range must stay
        // current.  BUT, only do it if we got here through
        // a track call and not a trackSingle call.
        // Note if isDopplerTracking_ is false, the method call
        // is a no-op.
        //
        resetDopplerInterpolator(mjdTriplet);

        // only set this if we got past any throws above.
        setSourceName( sourceName );
        // Note Ephemeris::setSource will clear any Ra/Dec/Az/El source
        // offsets in the arrayRefEphem_, but does not affect the
        // localEphemeris_ in any DriveHandle.  So call it here, but
        // NOT in updateShortTermMonitorPoints()
        programLogInfoIfPossible("Setting array reference ephemeris source. This involves a disk read");
        arrayRefEphem_->setSource( sourceName, userCatalog_ );
        setPhsOffsetMonitorPoints( 0.0, 0.0 );
        updateShortTermMonitorPoints( now );

        programLogInfoIfPossible(
            "SAT last ant update was set to " +
            Time::getTimeString( newLastAntUpdateMjd, 1 ) );
    }

    {   // Scope for the TrackerThreadSync
        CARMA_CPTRACE( kTraceLevel, "syncing to SAT thread" );

        const TrackerThreadSync sync( *this );

        if (affectPhaseCenter && arrayRefEphem_->useSource()) {
            const string callString =
                string( "DriveHandle::startTrackX(" ) + sourceName + ")";
            CARMA_CPTRACE(kTraceLevel, " calling: " << callString);

            WorkResultSet wrs( callString + " result set" );

	        const services::Source theSource = arrayRefEphem_->getSource();
            queueFunctorWorkRequestGroup(
                callString,
                makeHandleMethodFunctorGroup(
                    driveGroup,
                    &DriveHandle::startTrackX,
                    theSource,
                    mjdTriplet,
                    azWrapMode,
                    time,
                    nextDriveSeqNo_),
                wrs,
                *workerPool_ );

            // The DriveHandle computes triplet positions as well as doing
            // the IO to the antenna. The computation adds an additional
            // 650 msec for 15 ants.
            const unsigned long computeTripletWait = 650;
            unsigned long waitMillis = getDefaultLateAfterMillis() + computeTripletWait;
            waitForAllNormal(wrs, waitMillis);
        } else {
            // if a trackSingle commmand, we can't use the arrayRefEphem
            // source
            const string callString =
                string( "DriveHandle::startTrack(" ) + sourceName + ")";
            CARMA_CPTRACE(kTraceLevel, " calling: " << callString);

            WorkResultSet wrs( callString + " result set" );

            const string ucat = userCatalog_;
            queueFunctorWorkRequestGroup(
                callString,
                makeHandleMethodFunctorGroup(
                    driveGroup,
                    &DriveHandle::startTrack,
                    sourceName,
                    mjdTriplet,
                    ucat,
                    azWrapMode,
                    time,
                    nextDriveSeqNo_),
                wrs,
                *workerPool_ );


            // The DriveHandle computes triplet positions as well as doing
            // the IO to the antenna. The computation adds an additional
            // 650 msec for 15 ants.
            const unsigned long computeTripletWait = 650;
            unsigned long waitMillis =
                    getDefaultLateAfterMillis() + computeTripletWait;
            waitForAllNormal(wrs, waitMillis);
        }

        CARMA_CPTRACE( kTraceLevel, "Sending setAntennaRaDec to DelayEngine" );

        // do this in any case, even if trackSingle
        renewDelays( driveGroup , now );
    } // End TrackerThreadSync block
}

void
SubarrayControlImpl::track(
           const char * const sourceParam,
           const SeqShort &   carmaAntNoSeq,
           const bool         affectPhaseCenter,
           const control::AzWrapMode   azWrapMode,
           const double       time,
           const bool         overTheTop )
try {
    ScopedLogNdc ndc("SubarrayControlImpl::track");
    // Note: MIRIAD prefers (requires?) all uppercase, this was the
    // BIMA convention. Not sure if internal miriad routines
    // like planet flux cal will barf at lower or mixed case.
    // Best to pick a convention and stick with it.
    const string sourceName(
            StringUtils::lowASCIIAlphaNumericToUpper( sourceParam )
            );

    cmdlog() << "track(" << sourceName << ", "
             << getStringForCarmaAntNoSeq( carmaAntNoSeq ) << ", "
             << "affectPhaseCenter=" << affectPhaseCenter << ")";

    {
        // Check for valid source name in catalog before
        // DriveHandle fanout. (Bug 350).
        // Ephemeris will throw SourceNotFoundException if
        // invalid source.
        programLogInfoIfPossible("Checking for valid source. This involves a disk read.");
        Ephemeris e;
        e.setSource( sourceName, userCatalog_ );
    }

    // @todo add a compare of carmaAntNoSeq to all current subarray antNo's,
    // e.g.[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15], which we want to
    // behave the same as [0].
    // @see Marc's email Fri, 31 Mar 2006 17:26:48
    // @see deviant observer behavior

    const bool useAllAntennas = (carmaAntNoSeq.length() == 1)
                             && (carmaAntNoSeq[ 0 ] == 0);

    const bool failIfIntegrating = useAllAntennas; // Only fail if using all

    if ( (affectPhaseCenter == false) && useAllAntennas ) {
        const string msg =
            "You must affect phase center for antenna sequence of [0]";

        programLogErrorIfPossible( msg );

        throw CARMA_ERROR( msg );
    }

    /*
    if ( affectPhaseCenter )
        programLogInfoIfPossible( "track will affect phase center" );
    else
        programLogInfoIfPossible( "track will not affect phase center" );
    */
    
    
    // The antenna code track() will also zero the offsets()
    const DriveGroup driveGroup =
        getDriveGroupForCarmaAntNoSeq( "track",
                                       carmaAntNoSeq,
                                       true,     // allow zero
                                       false,    // don't allow dupes
                                       false );  // don't skip unowned ants

   // Reset all equatorial pointing offset monitor points to zero
   // for the given antennas.  If carmaAntSeq=0 also reset
   // the phaseCenter offset.
   // The startTrack call sets the internal values for the
   // Drivehandles to zero.  This should happen regardless
   // of whether this is track or trackSingle, since the policy
   // is any kind of track zeroes the offsets.
   // Note we could just call equatOffset(0,0,0) here, but then
   // we'll make extraneous calls to the remote drive objects.
    setEqOffsetMonitorPoints( 0.0, 0.0, carmaAntNoSeq );

    // Dispatch track command to antennas
    // affectPhaseCenter = true means all antennas tracking the same
    // source.
    trackDriveGroup( sourceName, driveGroup, affectPhaseCenter,
        azWrapMode, time, overTheTop, failIfIntegrating );

    // If we got here via trackSingle, then set the TrackSingle
    // command monitor points per antenna.
    const double now = Time::MJD();
    if ( ! useAllAntennas ) {
        AntMonPtGroup ampGroup
            = getAntMonPtGroupForCarmaAntNoSeq("trackSingle", carmaAntNoSeq);

        AntMonPtGroup::const_iterator i = ampGroup.begin();
        const AntMonPtGroup::const_iterator iEnd = ampGroup.end();

        for ( ; i != iEnd; ++i ) {
            const ControlSubsystemBase::Antenna * const ant = *i;
            if ( ant == 0 )
                continue;
            ControlSubsystemBase::TrackSingle& command =
                           ant->antCommands().trackSingle();
            command.timestamp().setValue( now );
            command.sourcename().setValue( sourceName );
        }
        markStateChange();
    } else {
        ControlSubsystemBase::Track & trackcmd =
            subarrayContainer_.commands().track();

        trackcmd.timestamp().setValue( now );
        trackcmd.sourcename().setValue( sourceName );
        markStateChange();
    }

} catch ( ... ) {
    rethrowCaughtAsUser();
}


void
SubarrayControlImpl::equatOffset( const double     dra,
                                  const double     ddec,
                                  const SeqShort&  carmaAntNoSeq,
                                  const bool       affectPhaseCenter,
                                  const bool       whileIntegrating)
try {
    cmdlog() << "equatOffset("
             << "dra(arcmin)=" << setprecision(3) << dra << ", "
             << "ddec(arcmin)=" << ddec << ", "
             << getStringForCarmaAntNoSeq( carmaAntNoSeq ) << ", "
             << "affectPhaseCenter=" << affectPhaseCenter << ", "
             << "whileIntegrating=" << whileIntegrating 
             << ")";

    // Don't allow change to tracking if an integration is in progress
    // unless parameter is set.
    {
        if (!whileIntegrating && subarrayIsIntegrating()) {
            ostringstream oss;

            oss << "equatOffset command not allowed while an integration is "
                << "still running;\n  issue a cancel to stop integration";

            throw CARMA_ERROR( oss.str() );
        }
    }
    // @todo add a compare of carmaAntNoSeq to
    // [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15], which we want to
    // behave the same as [0].
    // @see Marc's email Fri, 31 Mar 2006 17:26:48
    // @see deviant observer behavior

    if ( (affectPhaseCenter == false) &&
         (carmaAntNoSeq.length() == 1) && (carmaAntNoSeq[ 0 ] == 0) ) {
        const string msg =
            "You must affect phase center for antenna sequence of [0]";

        programLogErrorIfPossible( msg );

        throw CARMA_ERROR( msg );
    }

    if ( affectPhaseCenter )
        programLogInfoIfPossible( "equatOffset will affect phase center" );
    else
        programLogInfoIfPossible( "equatOffset will not affect phase center" );

    const DriveGroup driveGroup =
        getDriveGroupForCarmaAntNoSeq( "equatOffset",
                                       carmaAntNoSeq,
                                       true,     // allow zero
                                       false,    // don't allow dupes
                                       false );  // don't skip unowned ants


    // this will throw if Drive mode is inappropriate.
    checkModeForEquatOffset( driveGroup );

    ++nextDriveSeqNo_; // Preferred seq no

    {
        const string callString = "DriveHandle::setEquatoffset";
        WorkResultSet wrs( callString + " result set" );

        // This call does not do any IO; using threads is probably very inefficient
        // compared to just looping over antennas and calling the method on the driveHandle.
        queueFunctorWorkRequestGroup(
            callString,
            makeHandleMethodFunctorGroup(
                driveGroup,
                &DriveHandle::setEquatOffset,
                dra,
                ddec
            ),
            wrs,
	       *workerPool_);

        // NOTE: There is no IO here so this should be very fast
        waitForAllNormal(wrs);
    }

    setEqOffsetMonitorPoints( dra, ddec, carmaAntNoSeq );

    retrack( driveGroup );

    // If all antenna sky offsets modified, then change the
    // phase center offset as well.  This is the correct behavior
    // for mosaicking, which is the common use case of
    // calling this method with carmaAntNoSeq = [0].
    // If we chose not to change the phase center, then phase coherence is
    // lost as the tracking center moves away further and further away
    // from the phase center.  At about 30 primary beams away, the loss
    // is a few percent.
    if ( affectPhaseCenter )
        phaseCenterOffsetInternal( dra, ddec );
} catch ( ... ) {
    rethrowCaughtAsUser();
}


void
SubarrayControlImpl::setEqOffsetMonitorPoints(
    const double       dra,
    const double       ddec,
    const CarmaAntNoSeq & carmaAntNoSeq )
{
    const double now = Time::MJD();
    const bool useAllAntennas = (carmaAntNoSeq.length() == 1) &&
                                (carmaAntNoSeq[ 0 ] == 0);
    if ( useAllAntennas ) {
        carma::monitor::EquatOffset & eqoffcmd =
            subarrayContainer_.commands().equatOffset();

        eqoffcmd.timestamp().setValue( now );
        eqoffcmd.ra().setValue( dra );
        eqoffcmd.dec().setValue( ddec );

    } else {
        // Set the command monitor point values per antenna.
        AntMonPtGroup ampGroup
            = getAntMonPtGroupForCarmaAntNoSeq("equatOffset", carmaAntNoSeq);

        AntMonPtGroup::const_iterator i = ampGroup.begin();
        const AntMonPtGroup::const_iterator iEnd = ampGroup.end();

        for ( ; i != iEnd; ++i ) {
            const ControlSubsystemBase::Antenna * const ant = *i;
            if ( ant == 0 )
                continue;
            carma::monitor::EquatOffset & command =
                ant->antCommands().equatOffset();
            command.timestamp().setValue( now );
            command.ra().setValue( dra );
            command.dec().setValue( ddec );
        }
    }
    markStateChange();
}


void
SubarrayControlImpl::phaseCenterOffset( const double dra,
                                        const double ddec )
try {
    ostringstream oss;

    oss << "phaseCenterOffset(dra(arcmin)="
        << std::fixed << setprecision(2)
        << dra
        << " ddec(arcmin)="
        << ddec << ")";

    cmdlog() << oss.str() ;

    phaseCenterOffsetInternal(dra, ddec);

} catch ( ... ) {
    rethrowCaughtAsUser();
}


void
SubarrayControlImpl::phaseCenterOffsetInternal( const double dra,
                                        const double ddec )
{
    setPhsOffsetMonitorPoints( dra, ddec );
    // now send a new set of triplets to the delay engine for all antennas
    const DriveGroup driveGroup = getDriveGroup( "phaseCenterOffset" );
    renewDelays( driveGroup );
}

void
SubarrayControlImpl::setPhsOffsetMonitorPoints( const double dra,
                                                const double ddec)
{
    const double now = Time::MJD();
    // Set the command monitor point values. Note this is not
    // a per antenna command.
    ControlSubsystemBase::PhaseCenterOffset& command
            = subarrayContainer_.commands().phaseCenterOffset();

    command.timestamp().setValue( now );
    command.ra().setValue( dra );
    command.dec().setValue( ddec );

    Angle raOffset(  dra,"arcmin");
    Angle decOffset(ddec,"arcmin");
    // use false here so that negative angles stay negative.
    double raOffsetRadians = raOffset.radians(false);
    double decOffsetRadians = decOffset.radians(false);

    arrayRefEphem_->setRaDecOffsets(raOffsetRadians, decOffsetRadians);
    arrayRefEphem_->setMJD( now );
    subarrayContainer_.phaseCenterAppRa().setValue(arrayRefEphem_->getRa());
    subarrayContainer_.phaseCenterAppDec().setValue(arrayRefEphem_->getDec());
}


double
SubarrayControlImpl::computeTruncatedMjd( const double mjd,
                                          const double interval )
{
    return (interval * floor( mjd / interval ));
}


double
SubarrayControlImpl::computeTruncatedInterfUpdateTime(double now) const
{
    if ( trackerThread_.get() == 0 ) {
        throw CARMA_ERROR(
                "NULL SAT thread pointer in computeTruncateAntUpdateTime()" );
    }

    // Truncate to last interferometer interpInterval
    return computeTruncatedMjd( now,
                                trackerThread_->getInterpolationInterval() );
}

double
SubarrayControlImpl::computeTruncatedAntUpdateTime( double now ) const
{
    if ( trackerThread_.get() == 0 ) {
        throw CARMA_ERROR(
                "NULL SAT thread pointer in computeTruncateAntUpdateTime()" );
    }

    // Truncate to last interferometer interpInterval
    return computeTruncatedMjd( now,
                                trackerThread_->getAntInterpolationInterval() );
}


MjdTriplet
SubarrayControlImpl::getAlignedAntUpdateTimes( const double now )
{
    /*
     * The timestamps for the three samples in the triplet *must* be
     * aligned with the SAT ant update cycle. If this is not done, the SAT will
     * come in with a new sample and the oldest of samples in the
     * interpolator will be pushed out, potentially leaving the oldest time
     * in the interpolator in the future.
     * So we must start out filling the interpolator with a sample that
     * corresponds to the oldest sample in the interpolator. The SAT always
     * sends a timestamp that is 1.5 interpIntervals in the future,
     * so the interpolator then contains -0.5, +0.5, +1.5.
     */

    // Truncate to last interferometer interpInterval
    // this method throws if trackerThread_ is null.
    const double updateTime = computeTruncatedAntUpdateTime( now );

    // Truncate to last antenna interpInterval
    const double antInterpInterval =
        trackerThread_->getAntInterpolationInterval();

    const double mjd0 = updateTime - antInterpInterval / 2;

    MjdTriplet mjdTriplet;

    mjdTriplet.mjd[0] = mjd0;
    mjdTriplet.mjd[1] = mjd0 + antInterpInterval;
    mjdTriplet.mjd[2] = mjd0 + 2.0 * antInterpInterval;

    return mjdTriplet;
}


MjdTriplet
SubarrayControlImpl::getAlignedInterfUpdateTimes( const double now )
{
    // Truncate to last interferometer interpInterval
    // this method throws if trackerThread_ is null.
    const double updateTime = computeTruncatedInterfUpdateTime( now );

    const double interpInterval = trackerThread_->getInterpolationInterval();

    const double mjd0 = updateTime - interpInterval / 2;

    MjdTriplet mjdTriplet;

    mjdTriplet.mjd[0] = mjd0;
    mjdTriplet.mjd[1] = mjd0 + interpInterval;
    mjdTriplet.mjd[2] = mjd0 + 2.0 * interpInterval;

    return mjdTriplet;
}


void
SubarrayControlImpl::checkModeForEquatOffset( const DriveGroup & driveGroup )
{
      DriveGroup::const_iterator i = driveGroup.begin( );
      const DriveGroup::const_iterator iEnd = driveGroup.end( );

      // This is not too expensive since the call stays inside
      // the drive handle, checking only a private member variable,
      // and never goes to the DO.
      //
      // @todo An exception will be thrown on the first antenna
      // that is in the wrong mode and the exception message seen
      // by the observer will reflect that. So a clueless observer
      // might try 15 times before catching on. Ideally, we should
      // loop over all and create a message that lists all
      // antennas that are in the wrong mode.
      for ( ; i != iEnd; ++i ) {
        DriveHandle * const driveHandle = *i;

        if ( driveHandle == 0 )
	      continue;

        driveHandle->checkModeForEquatOffset();
      }
}

void
SubarrayControlImpl::retrack( const DriveGroup & driveGroup )
{

    if ( trackerThread_.get( ) == 0 )
        throw CARMA_ERROR( "NULL SAT thread pointer" );

    const MjdTriplet mjdTriplet = getAlignedAntUpdateTimes( );
    // Don't set the last update time here with trackerThread_.setLastAntUpdate
    // because it will cause the update to other antennas to be delayed.
    // Therefore this antenna will get updated in sync with the rest.

    {   // Scope for the TrackerThreadSync
        const TrackerThreadSync sync( *this );

        CARMA_CPTRACE(kTraceLevel,
                        "SubarrayControlImpl::retrack synced to SAT thread");
        const string callString = "DriveHandle::trackCurrentSourceWithOffsets";

        WorkResultSet wrs( callString + " result set" );

        //const string ucat = userCatalog_;
        queueFunctorWorkRequestGroup(
            callString,
            makeHandleMethodFunctorGroup(
        	   driveGroup,
        	   &DriveHandle::trackCurrentSourceWithOffsets,
        	   mjdTriplet,
               control::TIME,
               nextDriveSeqNo_
            ),
            wrs,
            *workerPool_ );

        unsigned long waitMillis = getDefaultLateAfterMillis() ;
        waitForAllNormal(wrs, waitMillis);
    }

    CARMA_CPTRACE ( kTraceLevel, "SubarrayControlImpl::retrack - Exiting");
}

// Corba command
double
SubarrayControlImpl::doppler( const char * sourceParam )
try {
    const string sourceName( sourceParam );
    cmdlog() << "doppler(" << sourceName << ")";

    const double now = Time::MJD();
    const double lo =
        computeDopplerFrequencyWithInterpReset( sourceName, loRestFreq_ );
    updateFrequency( lo );
    updateDopplerMonitorPoints( now );

    // force recomputation of delays since frequency change
    // may be larger than spec to keep 1 deg phase error
    // ( dfreq/freq < 5E-10 ).  This also ensures that
    // the refractive delay in the delay engine uses updated
    // frequency.
    // If sacI not initialized yet, don't attempt set delays.
    if ( initializationFlag_ ) {
        renewDelays( getDriveGroup("renewDelays"), now );
    }
    return lo;

} catch ( ... ) {
    rethrowCaughtAsUser();
    // This makes the compiler happy, stopping the warnings. We should never
    // get to here because the statement above is effectively a throw.
    return 0.0;
}

void
SubarrayControlImpl::resetDopplerInterpolator(
    const MjdTriplet & mjdTriplet )
{
    // do nothing if we are not doppler tracking.
    // Note we check here instead of in the caller so that SAT can
    // call this method w/o knowing the SACI's doppler tracking state.
    if ( !isDopplerTracking_ ) return;

    ostringstream params;
    const double conciseMjdRef = Time::MJD();
    params << "resetDopplerInterpolator: "
           << "Full reset of doppler frequency interpolator with MJD[] = "
           << setprecision(12)
           << mjdTriplet.mjd[0] << " ("
           << getConciseMjdString( mjdTriplet.mjd[0], conciseMjdRef ) << "), "
           << mjdTriplet.mjd[1] << " ("
           << getConciseMjdString( mjdTriplet.mjd[1], conciseMjdRef ) << "), "
           << mjdTriplet.mjd[2] << " ("
           << getConciseMjdString( mjdTriplet.mjd[2], conciseMjdRef ) << ")";
    CARMA_CPTRACE( kTraceLevel, params.str() );
    programLogInfoIfPossible( params.str() );

    ScopedQILockManager lockManager( dopplerInterp_, true );

    lockManager.lockQI();
    dopplerInterp_.empty();
    for ( unsigned int i = 0; i < 3; i++ ) {
        dopplerEphem_->setMJD( mjdTriplet.mjd[i] );
        const double velocity =
            dopplerEphem_->getDoppler( FRAME_TOPOGRAPHIC );
        // Higher velocity means redshifted, which means
        // the frequency is LOWER.
        const double mjdDopplerFactor = 1.0 - velocity / Physical::C;

        dopplerInterp_.extend( mjdTriplet.mjd[i], mjdDopplerFactor );
    }
    lockManager.unlockQI();
}


void
SubarrayControlImpl::extendDopplerInterpolator( const double mjd )
{
    // do nothing if we are not doppler tracking.
    // Note we check here instead of in the caller so that SAT can
    // call this method w/o knowing the SACI's doppler tracking state.
    if ( !isDopplerTracking_ )
        return;

    const double conciseMjdRef = Time::MJD();

    ostringstream params;
    params << "extendDopplerInterpolator: "
           << "Extending doppler frequency interpolator to MJD "
           << setprecision(12)
           << mjd << " (" << getConciseMjdString( mjd, conciseMjdRef ) << ")";
    CARMA_CPTRACE( kTraceLevel, params.str() );
    programLogInfoIfPossible( params.str() );

    dopplerEphem_->setMJD( mjd );
    // Note that Ephemeris::getDoppler(FRAME) returns 
    // Vsource corrected for earth and frame motions.
    // For non-solar system objects, Vsource is
    // VLSR(OPTICAL) or VLSR(RADIO) from the user catalog.
    // Therefore we have to use the proper optical or radio 
    // conversion to frequency.  The definitions are 
    //
    // V(RADIO)   = C*(restfreq - shiftedfreq)/shiftedfreq
    //
    // V(OPTICAL) = C*(restfreq - shiftedfreq)/restfreq
    // 
    // So V(RADIO) = V(OPTICAL) * shiftedfreq/restfreq.
    //
    // Note V(OPTICAL) is identical to C*Z.
    //
    const double velocity = dopplerEphem_->getDoppler( FRAME_TOPOGRAPHIC );

    velocityDefType veldef = 
        dopplerEphem_->getSource().getVelocity().getDefinition();
    // Doppler factor is (shifted frequency/rest frequency)
    // Higher velocity means redshifted, which means the frequency is LOWER.
    double dopfac = 1.0;
    if ( veldef == VEL_RADIO ) {
        dopfac = 1.0 - velocity / Physical::C;
    } else if ( veldef == VEL_OPTICAL ) {
        dopfac = 1.0 / (1.0 +  velocity / Physical::C );
    }

    ScopedQILockManager lockManager( dopplerInterp_, true );

    lockManager.lockQI();

    double qiMjdsBefore[3];
    unsigned qiNumMjdsBefore = 0;
    dopplerInterp_.getXvals( qiMjdsBefore, &qiNumMjdsBefore );

    dopplerInterp_.extend( mjd, dopfac );

    double qiMjdsAfter[3];
    unsigned qiNumMjdsAfter = 0;
    dopplerInterp_.getXvals( qiMjdsAfter, &qiNumMjdsAfter );

    lockManager.unlockQI();

    ostringstream oss;

    oss << "extendDopplerInterpolator: "
        << "Extended doppler frequency interpolator from {";

    for ( unsigned i = 0; i < qiNumMjdsBefore; ++i ) {
        if ( i != 0 )
            oss << ", ";

        oss << getConciseMjdString( qiMjdsBefore[i], conciseMjdRef );
    }

    oss << "} to {";

    for ( unsigned i = 0; i < qiNumMjdsAfter; ++i ) {
        if ( i != 0 )
            oss << ", ";

        oss << getConciseMjdString( qiMjdsAfter[i], conciseMjdRef );
    }

    oss << "}";

    programLogInfoIfPossible( oss.str() );
}

//@TODO
//   Deal correctly with alternate velocity reference frames and definitions
//   especially with planets.
double
SubarrayControlImpl::computeDopplerFrequencyWithInterpReset(
    const string & sourceName,
    const double   restFreq )
{
    ScopedLogNdc ndc("SubarrayControlImpl::computeDopplerFrequencyWithInterpReset");
    CARMA_CPTRACE( kTraceLevel,
        "<Entering> sourceName = " << sourceName);
    // No doppler for dopsource="none"
    if ( StringUtils::equalsIgnoreCase(sourceName,"none") ) {
        isDopplerTracking_ = false;
        dopplerSource_ = StringUtils::lowASCIIAlphaNumericToUpper(sourceName);
        // "zero out" the monitor points
        subarrayContainer_.dopplerSource().setValue(dopplerSource_);
        subarrayContainer_.velObservatory().setValue(0.0);
        subarrayContainer_.velocity().setValue(0.0);
        markStateChange();
        return restFreq;
    }

    bool change = !(StringUtils::equalsIgnoreCase(sourceName,"dontChange"));

    // only make this call in this method, nowhere else, to
    // prevent unintended setting of the doppler source.
    if ( change ) {
        programLogInfoIfPossible("Setting doppler ephemeris source. This involves a disk read");
        dopplerEphem_->setSource( sourceName , userCatalog_ );
        const Velocity dopvel = dopplerEphem_->getSource().getVelocity();
        velocityDefType veldef = dopvel.getDefinition();
        if ( veldef != VEL_RADIO )
        {
            ostringstream os;
            os << "Doppler source " << sourceName
               << " does not have a RADIO definition velocity."
               << " OPTICAL doppler calculation is not yet implemented!";
            throw CARMA_EXCEPTION( util::UserException, os.str().c_str() );
        }

        velocityFrameType velframe = dopvel.getFrame();
        if ( velframe != FRAME_LSR )
        {
            ostringstream os;
            os << "Doppler source " << sourceName
               << " does not use LSR velocity frame. "
               << " Alternate frames are not yet implemented!";
            throw CARMA_EXCEPTION( util::UserException, os.str().c_str() );
        }

        // set these AFTER the call to setSource, which will throw
        // if the source is unrecognized.
        // The policy is that we do not change the doppler tracking state
        // if there was a bad command.
        isDopplerTracking_ = true;
        dopplerSource_ = StringUtils::lowASCIIAlphaNumericToUpper(sourceName);
    }
    //CARMA_CPTRACE( kTraceLevel,
    //    "computeDopplerFrequencyWithInterpReset(" << sourceName <<
    //    "): change=" << change);

    // Update the LO frequency, if possible.
    const double now = Time::MJD() ;
    const MjdTriplet mjdTriplet = getAlignedAntUpdateTimes(now);

    resetDopplerInterpolator( mjdTriplet );

    return computeDopplerFrequency( now, restFreq );
}

double
SubarrayControlImpl::computeDopplerFrequency( const double mjd,
                                              const double restFreq )
{
    CARMA_CPTRACE( kTraceLevel, "Entering computeDopplerFrequency(mjd) " );
    // if we aren't doppler tracking, do nothing.
    if ( !isDopplerTracking_ ) {
        CARMA_CPTRACE( kTraceLevel, "NOT doppler tracking" );
        return restFreq;
    }

    CARMA_CPTRACE( kTraceLevel, "doppler tracking " );

    ScopedQILockManager lockManager( dopplerInterp_, true );

    lockManager.lockQI();
    if ( dopplerInterp_.canBracket(mjd) ) {
        const double lo = restFreq*dopplerInterp_.evaluate(mjd);
        const string localDopplerSource = dopplerSource_;
        const string localSourceName = sourceName();

        lockManager.unlockQI();

        const double conciseMjdRef = Time::MJD();

        ostringstream os;
        os << "SaCI::computeDopplerFrequency: "
           << "Successfully interpolated doppler frequency for MJD="
           << setprecision(12)
           << mjd << " (" << getConciseMjdString( mjd, conciseMjdRef ) << ")"
           << " Doppler source: "
           << localDopplerSource
           << " Tracking source: "
           << localSourceName;

        programLogInfoIfPossible( os.str() );

        return lo;
    } else {
        const double tmin = dopplerInterp_.getXmin();
        const double tmax = dopplerInterp_.getXmax();
        const string localDopplerSource = dopplerSource_;
        const string localSourceName = sourceName();

        double qiMjds[3];
        unsigned qiNumMjds = 0;
        dopplerInterp_.getXvals( qiMjds, &qiNumMjds );

        lockManager.unlockQI();

        // Compute how far out we are.
        // If mjd is below interpolator mininum, then the difference
        // should be reported as negative.
        const double deltaSec =
            ((mjd < tmin ) ? (mjd - tmin) : (mjd - tmax)) * 86400.0;

        const double conciseMjdRef = Time::MJD();

        ostringstream oss;

        oss << "SaCI::computeDopplerFrequency: "
            << "Could not update doppler frequency because the MJD "
            << setprecision(12) << mjd << " ("
            << getConciseMjdString( mjd, conciseMjdRef )
            << ") is out of range of the doppler frequency interpolator {";

        for ( unsigned i = 0; i < qiNumMjds; ++i ) {
            if ( i != 0 )
                oss << ", ";

            oss << getConciseMjdString( qiMjds[i], conciseMjdRef );
        }

        oss << "}. Difference = " << setprecision(6) << deltaSec
            << " seconds. Doppler source: " << localDopplerSource
            << " Tracking source: " << localSourceName;

        throw CARMA_ERROR( oss );
    }
}

// For use by the subarray tracker thread; just logs, doesn't throw
void SubarrayControlImpl::updateLo1DopplerFrequency(const double mjd)
{
    if ( !isDopplerTracking_ ) return;
    CARMA_CPTRACE( kTraceLevel, "Entering updateLo1DopplerFrequency " );
    try {
        double frequencyGHz = computeDopplerFrequency( mjd, loRestFreq_ );
        double dopfac;
        // No Doppler shift of LO at 1cm
        if (loRestFreq_ < 0.1) dopfac = 1.0;
        else                    dopfac = frequencyGHz/loRestFreq_;
        if (dopfac < 0.001) dopfac = 1.0;
        ifFreq_  = dopfac*ifRestFreq_;
        skyFreq_ = dopfac*restFreq_;
        // No Doppler shift of LO at 1cm
        if (frequencyGHz < 50.0) frequencyGHz = loRestFreq_;
        updateFrequency(frequencyGHz); // Set refLO and mon pts
    } catch (const BaseException& ex) {
        // log it as an ERROR so it shows up in WWWW but don't exit.
        programLogErrorIfPossible( ex.getMessage() );
    }
}

void
SubarrayControlImpl::ucat( const char * const catalog )
try {
    cmdlog() << "ucat(" << catalog << ")";

    const string userCatalogName( catalog );

    // if input is none, then we remove the user catalog
    // from all ephemerides.
    if ( StringUtils::equalsIgnoreCase( userCatalogName, "none" ) ) {
         // This string MUST MUST MUST be set to "" if there
         // is no user catalog being used.  Ephemeris and
         // SourceChecker depend on it being "" to indicate
         // no user catalog.

        userCatalog_ = string();
        subarrayContainer_.userCatalog().setValue(services::NO_CATALOG);

        programLogNotice( "Unloaded user catalog" );

        return;
    }

    // standard place for catalogs
    const string prefix = "/array/rt/catalogs/";
    // the catalog to try and open.
    string tryUserCatalog;

    // Check if it is an absolute path. If so don't prepend prefix.
    if ( userCatalogName[0] == '/' ) {
        tryUserCatalog = userCatalogName;
    } else {
        tryUserCatalog = prefix + userCatalogName;
    }

    if ( FileUtils::exists( tryUserCatalog ) ) {
        programLogNotice( "Got user catalog: " + userCatalog_ );
    } else {
        // Note: leave userCatalog_ unchanged.
        string msg;
        ostringstream oss;
        oss << "User catalog " << tryUserCatalog << " not found.";
        msg = oss.str();
        throw CARMA_EXCEPTION( FileNotFoundException, msg );
    }

    // Do a basic sanity check without actually reading
    // the contents of the file.  This will throw
    // an exception if catalog is not a regular fule.
    try {
        Table testCatalog;
        testCatalog.status( tryUserCatalog );
    } catch( ... ) {
        throw;
    }

    // passed tests, set the real variables.
    userCatalog_ = tryUserCatalog;
    subarrayContainer_.userCatalog().setValue( userCatalogName );

} catch ( ... ) {
    rethrowCaughtAsUser();
}




