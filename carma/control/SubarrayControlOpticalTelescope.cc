#include "carma/control/SubarrayControlImpl.h"

#include "carma/antenna/common/OpticalTelCommon.h"
#include "carma/control/errorMsgs.h"
#include "carma/control/AntennaControls.h"
#include "carma/control/OpticalTelHandle.h"
#include "carma/control/FunctorWorkRequest.h"
#include "carma/control/HandleMethodFunctorGroup.h"
#include "carma/control/RemoteObjMethodFunctorGroup.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/WorkResult.h"

#include <iomanip>

using namespace std;
using namespace carma::antenna::common;
using namespace carma::control;
using namespace carma::monitor;
using namespace carma::util;

SubarrayControlImpl::OpticalTelGroup
SubarrayControlImpl::getOpticalTelGroupForAntControlsGroup(
    const string &           commandName,
    const AntControlsGroup & antControlsGroup )
{
    OpticalTelGroup result;

    AntControlsGroup::const_iterator i = antControlsGroup.begin();
    const AntControlsGroup::const_iterator iEnd = antControlsGroup.end();

    for ( ; i != iEnd; ++i ) {
        OpticalTelHandle * const othp = (*i)->opticalTelHandle( );

        if ( othp != 0 )
            result.insert( othp );
    }

    return result;
}


SubarrayControlImpl::OpticalTelGroup
SubarrayControlImpl::getOpticalTelGroup( const string & commandName )
{
    const AntControlsGroup antControlsGroup = getAntControlsGroup();

    return getOpticalTelGroupForAntControlsGroup( commandName,
                                                  antControlsGroup );
}


SubarrayControlImpl::OpticalTelGroup
SubarrayControlImpl::getOpticalTelGroupForCarmaAntNoSeq(
    const string &        commandName,
    const CarmaAntNoSeq & carmaAntNoSeq,
    const bool            allowZero,
    const bool            ignoreDupes,
    const bool            skipUnownedAnts )
{
    const AntControlsGroup antControlsGroup =
        getAntControlsGroupForCarmaAntNoSeq( commandName,
                                             carmaAntNoSeq,
                                             allowZero,
                                             ignoreDupes,
                                             skipUnownedAnts );

    return getOpticalTelGroupForAntControlsGroup( commandName,
                                                  antControlsGroup );
}

SubarrayControlImpl::OpticalTelGroup
SubarrayControlImpl::getOpticalTelGroupForCarmaAntNo(
    const ::std::string & commandName,
    const CORBA::Short    carmaAntNo ) {

    return getOpticalTelGroupForAntControlsGroup(
            commandName,
            getAntControlsGroupForCarmaAntNo( commandName, carmaAntNo ) );
}

void
SubarrayControlImpl::camera( const SwitchState switchState,
                             const SeqShort &  carmaAntNoSeq )
try {
    string switchStateName = "< Unknown >";
    bool opticalAperture = false;

    switch ( switchState ) {
        case ON:
            switchStateName = "ON";
            opticalAperture = true;
            break;

        case OFF:
            switchStateName = "OFF";
            opticalAperture = false;
            break;
    }

    cmdlog() << "camera("
             << "state=" << switchStateName << ", "
             << getStringForCarmaAntNoSeq( carmaAntNoSeq ) << ")";

    const OpticalTelGroup opticalTelGroup =
        getOpticalTelGroupForCarmaAntNoSeq( "camera",
                                            carmaAntNoSeq,
                                            true,
                                            false );

    string remoteCallString;
    {
        ostringstream oss;

        oss << "OpticalTelControl::camera(" << switchStateName << ")";

        remoteCallString = oss.str( );
    }

    WorkResultSet wrs( remoteCallString + " result set" );

    queueFunctorWorkRequestGroup(
        remoteCallString,
        makeRemoteObjMethodFunctorGroup(
            opticalTelGroup,
            remoteCallString,
            "",
            &OpticalTelControl::turn,
            switchState ),
        wrs,
        *workerPool_ );

    waitForAllNormal( wrs );

    // Now the select aperture command
    selectAperture( opticalAperture, carmaAntNoSeq );
} catch ( ... ) {
    rethrowCaughtAsUser();
}


void SubarrayControlImpl::setFrameDimensions( const CORBA::Short x,
                                              const CORBA::Short y,
                                              const CORBA::Short x0,
                                              const CORBA::Short y0,
                                              const SeqShort &  carmaAntNoSeq )
try {

    cmdlog() << "setFrameDimensions("
             << "x=" << x << ", y=" << y
             << ", x0=" << x0 << ", y0=" << y0 << ", "
             << getStringForCarmaAntNoSeq( carmaAntNoSeq ) << ")";

    const OpticalTelGroup opticalTelGroup =
        getOpticalTelGroupForCarmaAntNoSeq( "setFrameDimensions",
                                            carmaAntNoSeq,
                                            true,
                                            false );

    string remoteCallString;
    {
        ostringstream oss;

        oss << "OpticalTelControl::setFrameDimensions("
             << "x=" << x << ", y=" << y
             << ", x0=" << x0 << ", y0=" << y0 << ")";

        remoteCallString = oss.str( );
    }

    WorkResultSet wrs( remoteCallString + " result set" );

    queueFunctorWorkRequestGroup(
        remoteCallString,
        makeRemoteObjMethodFunctorGroup(
            opticalTelGroup,
            remoteCallString,
            "",
            &OpticalTelControl::setFrameDimensions,
            x, y, x0, y0 ),
        wrs,
        *workerPool_ );

    waitForAllNormal( wrs );

} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
SubarrayControlImpl::setFrameBrightness( const CORBA::Float brightness,
                                         const SeqShort & carmaAntNoSeq )
try {
    cmdlog() << "setFrameBrightness("
             << "brightness=" << brightness << ", "
             << getStringForCarmaAntNoSeq( carmaAntNoSeq ) << ")";

    const OpticalTelGroup opticalTelGroup =
        getOpticalTelGroupForCarmaAntNoSeq( "setFrameBrightness",
                                            carmaAntNoSeq,
                                            true,
                                            false );

    string remoteCallString;
    {
        ostringstream oss;

        oss << "OpticalTelControl::setBrightness("
            << "brightness=" << brightness << ")";

        remoteCallString = oss.str( );
    }

    WorkResultSet wrs( remoteCallString + " result set" );

    queueFunctorWorkRequestGroup(
        remoteCallString,
        makeRemoteObjMethodFunctorGroup(
            opticalTelGroup,
            remoteCallString,
            "",
            &OpticalTelControl::setBrightness,
            brightness ),
        wrs,
        *workerPool_ );

    waitForAllNormal( wrs );

} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
SubarrayControlImpl::setFrameContrast( const CORBA::Float contrast,
                                       const SeqShort & carmaAntNoSeq )
try {

    cmdlog() << "setFrameContrast("
             << "contrast=" << contrast << ", "
             << getStringForCarmaAntNoSeq( carmaAntNoSeq ) << ")";

    const OpticalTelGroup opticalTelGroup =
        getOpticalTelGroupForCarmaAntNoSeq( "setFrameContrast",
                                            carmaAntNoSeq,
                                            true,
                                            false );

    string remoteCallString;
    {
        ostringstream oss;

        oss << "OpticalTelControl::setContrast("
            << "contrast=" << contrast << ")";

        remoteCallString = oss.str( );
    }

    WorkResultSet wrs( remoteCallString + " result set" );

    queueFunctorWorkRequestGroup(
        remoteCallString,
        makeRemoteObjMethodFunctorGroup(
            opticalTelGroup,
            remoteCallString,
            "",
            &OpticalTelControl::setContrast,
            contrast ),
        wrs,
        *workerPool_ );

    waitForAllNormal( wrs );

} catch ( ... ) {
    rethrowCaughtAsUser();
}
void
SubarrayControlImpl::setFramegrabberResolution(
    const carma::control::Resolution fgResolution,
    const SeqShort & carmaAntNoSeq )
try {

    cmdlog() << "setFramegrabberResolution(fgResolution="
             << OpticalTelCommon::getResolutionAsString( fgResolution ) << ", "
             << getStringForCarmaAntNoSeq( carmaAntNoSeq ) << ")";

    const OpticalTelGroup opticalTelGroup =
        getOpticalTelGroupForCarmaAntNoSeq( "setFramegrabberResolution",
                                            carmaAntNoSeq,
                                            true,
                                            false );

    string remoteCallString;
    {
        ostringstream oss;

        oss << "OpticalTelControl::setFramegrabberResolution("
             << OpticalTelCommon::getResolutionAsString( fgResolution ) << ")";

        remoteCallString = oss.str( );
    }

    WorkResultSet wrs( remoteCallString + " result set" );

    queueFunctorWorkRequestGroup(
        remoteCallString,
        makeRemoteObjMethodFunctorGroup(
            opticalTelGroup,
            remoteCallString,
            "",
            &OpticalTelControl::setFramegrabberResolution,
            fgResolution ),
        wrs,
        *workerPool_ );

    // The OVRO antenna take a long time for this; don't know why
    const unsigned long lateAfterMillis = 4000;
    waitForAllNormal(wrs, lateAfterMillis);

} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
SubarrayControlImpl::setRotationAndFieldsOfView(
    const CORBA::Float rotationInDegrees,
    const CORBA::Float azFOVInArcminutes,
    const CORBA::Float elFOVInArcminutes,
    const CORBA::Short carmaAntNo )
try {

    { // Set control points first in the event the antenna is offline.
        ControlSubsystemBase::Antenna & antMp =
            getAntMonPtForCarmaAntNo( carmaAntNo );

        antMp.cameraRotation().setValue( rotationInDegrees );
        antMp.cameraAzFOV().setValue( azFOVInArcminutes );
        antMp.cameraElFOV().setValue( elFOVInArcminutes );

        markStateChange();
    }

    string parameterString;
    {
        ostringstream oss;
        oss << "rotationInDegrees=" << rotationInDegrees << ", "
            << "azFOVInArcminutes=" << azFOVInArcminutes << ", "
            << "elFOVInArcminutes=" << elFOVInArcminutes;

        parameterString = oss.str( );
    }

    ostringstream oss;
    oss << "setRotationAndFieldsOfView("
        << parameterString << ", "
        << carmaAntNo << ")";

    const OpticalTelGroup opticalTelGroup =
        getOpticalTelGroupForCarmaAntNo("setRotationAndFieldsOfView",
                                        carmaAntNo);

    if (opticalTelGroup.size() != 1 ) {
        if ( (initializationFlag_ == false) && opticalTelGroup.empty() ) {
            // Silently ignore this command during initialization
            // if we don't own this antenna. Happens if you reinitialize
            // a subarray and another subarray is already holding this
            // antenna
            oss << "; skipped beause antenna not owned by subarray";
            cmdlog() << oss.str();
            return;
        }
        cmdlog() << oss.str();
        throw CARMA_ERROR( ZERO_DISALLOWED );
    }
    cmdlog() << oss.str();

    string remoteCallString;
    {
        ostringstream oss;

        oss << "OpticalTelControl::setRotationAndFieldsOfView("
            << parameterString << ")";

        remoteCallString = oss.str( );
    }

    WorkResultSet wrs( remoteCallString + " result set" );

    queueFunctorWorkRequestGroup(
        remoteCallString,
        makeRemoteObjMethodFunctorGroup(
            opticalTelGroup,
            remoteCallString,
            "",
            &OpticalTelControl::setRotationAndFieldsOfView,
            rotationInDegrees,
            azFOVInArcminutes,
            elFOVInArcminutes ),
        wrs,
        *workerPool_ );

    waitForAllNormal( wrs );

} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
SubarrayControlImpl::takeBackgroundImage(
    const CORBA::UShort numFrames,
    const SeqShort & carmaAntNoSeq )
try {
    cmdlog() << "takeBackgroundImage("
             << "numFrames=" << numFrames << ", "
             << getStringForCarmaAntNoSeq( carmaAntNoSeq ) << ")";

    const OpticalTelGroup opticalTelGroup =
        getOpticalTelGroupForCarmaAntNoSeq( "takeBackgroundImage",
                                            carmaAntNoSeq,
                                            true,
                                            false );
    ++nextCentroidSeqNo_;

    string remoteCallString;
    {
        ostringstream oss;

        oss << "OpticalTelControl::takeBackgroundImageWithSeqNo("
            << "numFrames=" << numFrames << ", "
            << "seq=" << nextCentroidSeqNo_ << " )";

        remoteCallString = oss.str( );
    }

    WorkResultSet wrs( remoteCallString + " result set" );

    queueFunctorWorkRequestGroup(
        remoteCallString,
        makeHandleMethodFunctorGroup(
            opticalTelGroup,
            &OpticalTelHandle::takeBackgroundWithSeqNo,
            static_cast< carma::monitor::MonitorSystem * >( &carmaMonitor_ ),
            numFrames,
            nextCentroidSeqNo_ ),
        wrs,
        *workerPool_ );

    waitForAllNormal( wrs );

} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
SubarrayControlImpl::findCentroid( const CORBA::UShort numFramesPerImage,
                                   const CORBA::UShort minValidCentroids,
                                   const CORBA::UShort maxCentroidAttempts,
                                   const CORBA::UShort numEdgePixels,
                                   const CORBA::UShort apertureRadiusPixels,
                                   const CORBA::Float pixelThresholdSigma,
                                   const CORBA::Boolean subBackground,
                                   const CORBA::Boolean normalizeMedian,
                                   const SeqShort & carmaAntNoSeq )
try {

    string parameterString;
    {
        ostringstream oss;
        oss << "numFramesPerImage=" << numFramesPerImage << ", "
            << "minValidCentroids=" << minValidCentroids << ", "
            << "maxCentroidAttempts=" << maxCentroidAttempts << ", "
            << "numEdgePixels=" << numEdgePixels << ", "
            << "apertureRadiusPixels=" << apertureRadiusPixels << ", "
            << "pixelThresholdSigma=" << pixelThresholdSigma << ", "
            << "subBackground=" << boolalpha << subBackground << ", "
            << "normalizeMedian=" << normalizeMedian << noboolalpha;

        parameterString = oss.str( );
    }

    cmdlog() << "findCentroid("  << parameterString << ", "
             << getStringForCarmaAntNoSeq( carmaAntNoSeq ) << ")";

    const OpticalTelGroup opticalTelGroup =
        getOpticalTelGroupForCarmaAntNoSeq( "findCentroid",
                                            carmaAntNoSeq,
                                            true,
                                            false );

    ++nextCentroidSeqNo_;

    string remoteCallString;
    {
        ostringstream oss;

        oss << "OpticalTelControl::findCentroid("
            << parameterString << ", "
            << "seq=" << nextCentroidSeqNo_ << ")";

        remoteCallString = oss.str( );
    }

    WorkResultSet wrs( remoteCallString + " result set" );

    queueFunctorWorkRequestGroup(
        remoteCallString,
        makeHandleMethodFunctorGroup(
            opticalTelGroup,
            &OpticalTelHandle::findCentroidWithSeqNo,
            static_cast< carma::monitor::MonitorSystem * >( &carmaMonitor_ ),
            numFramesPerImage,
            minValidCentroids,
            maxCentroidAttempts,
            numEdgePixels,
            apertureRadiusPixels,
            pixelThresholdSigma,
            subBackground,
            normalizeMedian,
            nextCentroidSeqNo_ ),
        wrs,
        *workerPool_ );

    waitForAllNormal( wrs );

} catch ( ... ) {
    rethrowCaughtAsUser();
}

carma::control::CentroidResults*
SubarrayControlImpl::getCentroidResults( const CORBA::Short carmaAntNo )
try {
    cmdlog() << "getCentroidResults(carmaAntNo="  << carmaAntNo << ")";

    // This needs to point to a real object to avoid a segfault when the
    // antenna is not in the array or if it does not have an opticalTel DO
    CentroidResults* res = new CentroidResults();

    OpticalTelGroup otGroup = getOpticalTelGroupForCarmaAntNo(
        "getCentroidResults",
        carmaAntNo );

    if ( otGroup.size( ) == 1 ) {
        OpticalTelGroup::iterator i = otGroup.begin( );

        const bool logIfNotReachable = true;
        if ( (*i)->isObjReachable(logIfNotReachable) ) {
           res = (*i)->remoteObj()->getCentroidResults();
        }
    }
    
    return res;

} catch ( ... ) {
    rethrowCaughtAsUser();
    return 0; // Pacify compiler warnings
}

carma::antenna::common::flattenedOpticalImage*
SubarrayControlImpl::getImage( const CORBA::UShort numFrames,
                               const CORBA::Boolean subBackground,
                               const CORBA::Boolean normalizeMedian,
                               const CORBA::Boolean normalizeImage,
                               const CORBA::Short carmaAntNo )
try {
    cmdlog() << "getImage("
        << " numFrames=" << numFrames << ","
        << " subBackground=" << subBackground << ","
        << " normalizeMedian=" << normalizeMedian << ","
        << " normalizeImage=" << normalizeImage << ","
        << " carmaAntNo="  << carmaAntNo << " )";

    OpticalTelGroup otGroup = getOpticalTelGroupForCarmaAntNo(
        "getImage",
        carmaAntNo );

    // This needs to point to a real object to avoid a segfault when the
    // antenna is not in the array or if it does not have an opticalTel DO
    flattenedOpticalImage* res = new  flattenedOpticalImage();

    if ( otGroup.size( ) == 1 ) {
        OpticalTelGroup::iterator i = otGroup.begin();

        const bool logIfNotReachable = true;
        if ( (*i)->isObjReachable(logIfNotReachable) ) {
            res = (*i)->remoteObj()->getImage( numFrames,
                                               subBackground,
                                               normalizeMedian,
                                               normalizeImage );
        }
    }
    return res;

} catch ( ... ) {
    rethrowCaughtAsUser();
    return 0; // We won't get here but pacifies compiler warnings
}
