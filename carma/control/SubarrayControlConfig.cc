/**
 *
 * Carma control interface server implementation for various config
 * commands, such as signal path mapping, antenna locations and pads
 *
 * @author: Steve Scott
 *
 * $Id: SubarrayControlConfig.cc,v 1.87 2013/04/12 20:54:44 mpound Exp $
 *
 * $CarmaCopyright$
 *
 */


#include "carma/control/SubarrayControlImpl.h"

#include <iomanip>
#include <sstream>

#include "carma/corba/corba.h"
#include "carma/control/AntennaControls.h"
#include "carma/control/errorMsgs.h"
#include "carma/control/LOrefHandle.h"
#include "carma/control/PadOffsets.h"
#include "carma/control/SatThreadSync.h"
#include "carma/control/Subarray.h"
#include "carma/services/Length.h"
#include "carma/services/Pad.h"
#include "carma/services/padUtils.h"
#include "carma/monitor/AntennaCommon.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"
#include "carma/util/UserException.h"

#include <iomanip>

using namespace ::std;
using namespace carma;
using namespace carma::antenna;
using namespace carma::antenna::common;
using namespace carma::control;
using namespace carma::interferometry;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::util;


void
SubarrayControlImpl::antennaOffset( const float        east,
                                    const float        north,
                                    const float        up,
                                    const CORBA::Short carmaAntNo)
try
{
    const string methodName("SubarrayControlImpl::antennaOffset");
    ScopedLogNdc ndc( methodName );
    cmdlog() << "antennaOffset("
             << "east=" << setprecision(2) << east << ", "
             << "north=" << setprecision(2) << north << ", "
             << "up="    << setprecision(2) << up << ", "
             << carmaAntNo 
             << ")";

    if ( carmaAntNo == 0 ) 
        throw CARMA_ERROR( ZERO_DISALLOWED );

    // Empty results are already allowed internally in this call
    // if initializationFlag_ == false
    AntControlsGroup antControlsGroup =
        getAntControlsGroupForCarmaAntNo( methodName, carmaAntNo );

    if ( ! checkAntControls(antControlsGroup, carmaAntNo, methodName ) ) return;

    AntennaControls * const antControls = *(antControlsGroup.begin( ));

    if ( antControls == 0 ) {
        throw CARMA_ERROR( NULL_ANTENNA );
    }

    // convert to CQs and send to antenna control who will in turn
    // tell DriveControl.
    const Length lEast(east,"mm");
    const Length lNorth(north,"mm");
    const Length lUp(up,"mm");

    antControls->setAntennaOffsets(lEast, lNorth, lUp);

    // Do not broadcast delays to correlator and loberotator if
    // this method is being called during subarray initialization or
    // during antenna initialization.
    bool broadcastDelays = (initializationFlag_ && isAntennaInitialized( carmaAntNo ) );

    try {
        updateLocation( *antControls, broadcastDelays );
    } catch (...) {
        programLogError( "Caught and rethrew unknown exception" );
        throw;
    }

    // Only do this if we didn't error out previously.
    // Write received values into corresponding control monitor points
    // and update timestamp for data we just wrote
   
    const double timestamp = Time::MJD(); // current timestamp for monitor data
    ControlSubsystem::AntennaOffset & ao =
        controlSubsystem_.antenna(carmaAntNo - 1).antennaOffset();

    ao.east().setValue( east );
    ao.north().setValue( north );
    ao.up().setValue( up );
    subarrayContainer_.timestamp().setValue( timestamp );
    markStateChange();

} catch ( ... ) {
    rethrowCaughtAsUser( );
}

void
SubarrayControlImpl::axisNonintersection( const float        offset,
                                          const CORBA::Short carmaAntNo )
try
{
    const string methodName("SubarrayControlImpl::axisNonintersection");
    ScopedLogNdc ndc( methodName );
    cmdlog() << "axisNonintersection("
             << "offset=" << setprecision(2) << offset << ", "
             << carmaAntNo << ")";

    if ( carmaAntNo == 0 ) 
        throw CARMA_ERROR( ZERO_DISALLOWED );

    // Empty results are already allowed internally in this call
    // if initializationFlag_ == false
    AntControlsGroup antControlsGroup =
        getAntControlsGroupForCarmaAntNo( methodName, carmaAntNo );

    if ( ! checkAntControls(antControlsGroup, carmaAntNo, methodName ) ) return;

    AntennaControls * const antControls = *(antControlsGroup.begin( ));

    if ( antControls == 0 ) {
        throw CARMA_ERROR( NULL_ANTENNA );
    }

    // Do not broadcast delays to correlator and loberotator if
    // this method is being called during subarray initialization or
    // during antenna initialization.
    bool broadcastDelays = (initializationFlag_ && isAntennaInitialized( carmaAntNo ) );

    try {
        // this does a little unnecessary work of recomputing
        // location, but it gets us to the correct delay engine
        // call.
        updateLocation( *antControls, broadcastDelays );
    } catch (...) {
        programLogError( "Caught and rethrew unknown exception" );
        throw;
    }

    antControls->setAxisMisalignment(Length(offset, "mm"));
    controlSubsystem_.antenna(carmaAntNo - 1).
            axisNonintersection().setValue(offset);
    markStateChange();


} catch ( ... ) {
    rethrowCaughtAsUser( );
}


void
SubarrayControlImpl::pad( const short        padNo,
                          const CORBA::Short carmaAntNo)
try {
    const string methodName("SubarrayControlImpl::pad");
    ScopedLogNdc ndc( methodName );
    cmdlog() << "pad("
             << "padNo=" << padNo << ", "
             << carmaAntNo 
             << ")";

    if ( carmaAntNo == 0 ) 
        throw CARMA_ERROR( ZERO_DISALLOWED );

    // Empty results are already allowed internally in this call
    // if initializationFlag_ == false
    AntControlsGroup antControlsGroup =
        getAntControlsGroupForCarmaAntNo( methodName, carmaAntNo );

    if ( ! checkAntControls(antControlsGroup, carmaAntNo, methodName ) ) return;

    AntennaControls * const antControls = *(antControlsGroup.begin( ));

    if ( antControls == 0 ) {
        throw CARMA_ERROR( NULL_ANTENNA );
    }

    // Do not broadcast delays to correlator and loberotator if
    // this method is being called during subarray initialization or
    // during antenna initialization.
    bool broadcastDelays = (initializationFlag_ && isAntennaInitialized( carmaAntNo ) );

    const Pad thePad = obs_->getPad( defaultPadName( padNo ) );
    antControls->setPad(thePad);
    // set the monitor point value
    controlSubsystem_.antenna(carmaAntNo - 1).padNumber().setValue ( padNo );
    delayEngine_->setPadDelay( carmaAntNo, padDelay_[padNo] );
    markStateChange();

    try {
        updateLocation( *antControls, broadcastDelays );
    } catch (...) {
        programLogError( "Caught and rethrew unknown exception" );
        throw;
    }

} catch ( ... ) {
    rethrowCaughtAsUser( );
}

// Entry point for padOffset python command
void
SubarrayControlImpl::padOffset( const float        east,
                                const float        north,
                                const float        up,
                                const CORBA::Short carmaAntNo)
try {
    const string methodName("SubarrayControlImpl::padOffset");
    ScopedLogNdc ndc( methodName );
    cmdlog() << "padOffset("
             << "east=" << east << ", "
             << "north=" << north << ", "
             << "up=" << up << ", "
             << carmaAntNo 
             << ")";
             
    if ( carmaAntNo == 0 ) 
        throw CARMA_ERROR( ZERO_DISALLOWED );

    // Empty results are already allowed internally in this call
    // if initializationFlag_ == false
    AntControlsGroup antControlsGroup =
        getAntControlsGroupForCarmaAntNo( methodName, carmaAntNo );

    if ( ! checkAntControls(antControlsGroup, carmaAntNo, methodName ) ) return;

    AntennaControls * const antControls = *(antControlsGroup.begin( ));

    if ( antControls == 0 ) {
        throw CARMA_ERROR( NULL_ANTENNA );
    }

    // Do not broadcast delays to correlator and loberotator if
    // this method is being called during subarray initialization or
    // during antenna initialization.
    bool broadcastDelays = (initializationFlag_ && isAntennaInitialized( carmaAntNo ) );

    padOffset( east, north, up, *antControls, broadcastDelays );

    // Only do this if we didn't error out previously.
    // Write received values into corresponding control monitor points
    // and update timestamp for data we just wrote

    const double timestamp = Time::MJD(); // current timestamp for monitor data
    ControlSubsystem::PadOffset & po =
        controlSubsystem_.antenna(carmaAntNo - 1).padOffset();

    po.east().setValue( east );
    po.north().setValue( north );
    po.up().setValue( up );
    subarrayContainer_.timestamp().setValue( timestamp );
    markStateChange();

} catch ( ... ) {
    rethrowCaughtAsUser( );
}


// pad offset helper function
// No boolean override here because once you are at this
// point you have the handle to all the requested antennas
// via the AntennaControls object.
void
SubarrayControlImpl::padOffset( const float       east,
                                const float       north,
                                const float       up,
                                AntennaControls & antControls,
                                bool              broadcastDelays )
{
    ScopedLogNdc ndc("SubarrayControlImpl::padOffset(AntControls)");

    // convert to CQs and send to antenna control who will in turn
    // tell DriveControl.
    const Length lEast(east,"mm");
    const Length lNorth(north,"mm");
    const Length lUp(up,"mm");

    antControls.setPadOffsets(lEast, lNorth, lUp);

    try {
        updateLocation( antControls, broadcastDelays );
    } catch (...) {
        programLogError( "Caught and rethrew unknown exception" );
        throw;
    }
}

void
SubarrayControlImpl::updateLocation( AntennaControls & antControls,
       bool broadcastDelays )
{
    ScopedLogNdc ndc("SubarrayControlImpl::updateLocation");

    // get the newly computed location from the just-sent-in pad offsets
    // so that we can tell the Delay Engine the new antenna location.
    const unsigned short carmaAntNo = antControls.getCarmaAntennaNo();
    const Location updatedLocation  = antControls.getAbsoluteLocation();
    const Length   axisMis          = antControls.getAxisMisalignment();

    try {
        TrackerThreadSync sync( *this );

        delayEngine_->setAntennaCoordinates(carmaAntNo,
               updatedLocation, axisMis);
        const DelayFrameVec delayFrameVec = delayEngine_->computeDelays();
        if ( broadcastDelays ) {
            broadcastDelayData( delayFrameVec );
        } else {
            // For uninitialized antennas, copy the XYZ location info
            // from the local delay frame to the master.
            const ::size_t numDelayFrames = delayFrameVec.size();
            // this angle doesn't matter for uninitialized antennas
            const double theta = 0.0; 
            const DelayEngineSubsystem * const delayFrame =
                delayFrameVec.at(  numDelayFrames - 1 );
            copyAntDelayData( delaySubsystem_.delayData( carmaAntNo - 1 ),
                              delayFrame->delayData( carmaAntNo - 1 ),
                              theta );
        }
    } catch (...) {
        programLogError( "Caught and rethrew unknown exception" );
        throw;
    }

    // update the display ENU monitor points
    // if this command succeeded.
    ControlSubsystemBase::TotalENU & antEnu
        = controlSubsystem_.antenna(carmaAntNo - 1).totalENU();
    vector<Length> tenu = antControls.getTotalEnu();
    antEnu.east().setValue( tenu[0].meters() );
    antEnu.north().setValue( tenu[1].meters() );
    antEnu.up().setValue( tenu[2].meters() );
    markStateChange();
    
    // Set lat/long/alt monitor points - these are only used for display
    ControlSubsystemBase::Location& antLoc
        = controlSubsystem_.antenna(carmaAntNo-1).location();
    antLoc.longitude().setValue(updatedLocation.getLongitude().radians());
    antLoc.latitude().setValue(updatedLocation.getLatitude().radians());
    antLoc.altitude().setValue(updatedLocation.getAltitude().meters());
    
}

void
SubarrayControlImpl::assignLO( const CORBA::Long loRefNo )
try {
    cmdlog() << "assignLO(" << loRefNo << ")";

    if ( loRef_.get() == 0 ) {
        programLogWarnIfPossible(
            "Skipping assigning LO since loRef_ is null" );
    } else {
        subarrayContainer_.refLoNumber().setValue(loRefNo);
        loRef_->assignSynth( loRefNo );
    }
} catch ( ... ) {
    rethrowCaughtAsUser( );
}


void
SubarrayControlImpl::jyperk( const float gain,
                             const CORBA::Short carmaAntNo)
try {
    CarmaAntNoSeq antSeq;
    antSeq.length(1);
    antSeq[0] = carmaAntNo;

   // Set the monitor point value per antenna.
    AntMonPtGroup ampGroup
            = getAntMonPtGroupForCarmaAntNoSeq("jyperk", antSeq);
    if ( ampGroup.size() != 1 ) {
        if ( (initializationFlag_ == false) && ampGroup.empty() ) {
            // Silently ignore this command during initialization
            // if we don't own this antenna. Happens if you reinitialize
            // a subarray and another subarray is already holding this
            // antenna

            return;
        }
        throw CARMA_ERROR( ZERO_DISALLOWED );
    }

    ControlSubsystemBase::Antenna * const ant = * ( ampGroup.begin() );
    if ( ant == 0 ) {
        throw CARMA_ERROR( NULL_MONSYS );
    }
    ant->jyperk().setValue( gain );
    markStateChange();
} catch ( ... ) {
    rethrowCaughtAsUser( );
}

void
SubarrayControlImpl::sbratio (const float ratio,
                              const CarmaAntNoSeq & carmaAntNoSeq )
try {
    const bool allowZero = true;
    const bool ignoreDupes = true;
    const bool skipAntsNotOwnedByMe = false; // Throws if ant not in subarray.

    AntMonPtGroup ampGroup =
        getAntMonPtGroupForCarmaAntNoSeq( "sbratio",
                                          carmaAntNoSeq,
                                          allowZero,
                                          ignoreDupes,
                                          skipAntsNotOwnedByMe );

    AntMonPtGroup::iterator amp = ampGroup.begin();
    const AntMonPtGroup::iterator ampEnd = ampGroup.end();
    for ( ; amp != ampEnd; ++amp ) {

        const int antNo = (*amp)->carmaAntennaNumber().getValue();
        const int antIdx = antNo - 1;

        controlSubsystem_.antenna( antIdx ).sidebandRatio().setValue( ratio );
    }
    markStateChange();
} catch ( ... ) {
    rethrowCaughtAsUser( );
}

bool
SubarrayControlImpl::checkAntControls(
        AntControlsGroup & antControlsGroup,
        const unsigned carmaAntNo,
        const string & methodName )
try {
    ScopedLogNdc ndc ("SubarrayControlImpl::checkAntControls");

    if ( antControlsGroup.empty() ) {

        // If, during subarray initialization,  we don't own this antenna, 
        // set the pad info anyway so that the DelayEngine can publish the
        // XYZ position info that MIRIAD needs for all antennas whether they
        // are part of this subarray or not.
        //
        // If the subarray is already initialized, then the user will get an
        // error telling them they don't own this antenna.
        //
        if (initializationFlag_ == false) {

        // If the antenna is already initialized, then another subarray owns
        // or has owned this antenna and it's XYZ info should already be published.
        // In which case, we don't need this step.
            if ( isAntennaInitialized( carmaAntNo ) == false )   {
                const AntennaControls::PersistentInfo persistentInfo =
                  retrieveAntPersistentInfo(carmaAntNo, carmaMonitor_, *obs_);

                auto_ptr<AntennaControls> ap(
                        new AntennaControls(carmaAntNo, persistentInfo)
                             );
                antControlsGroup.insert( ap.get() );
                ap.release();
            } else {
                cmdlog() << methodName << " skipped during initialization because antenna C"
                         << carmaAntNo << " is owned by another subarray"
                         << " and is already initialized";
                return false;
            }
        } else {
            cmdlog() << methodName<< " skipped because antenna C"
                 << carmaAntNo << " is owned by another subarray.";
            return false;
        } // if initializationFlag_ == false
    }// if antcontrolsGroup.empty()

    return true;

} catch ( ... ) {
    rethrowCaughtAsUser( );
    return false; // shut the compiler up
}
