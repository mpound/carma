/**
 *
 * Carma control interface server implementation for miscellaneous commands.
 *
 * @author: Steve Scott
 *
 * $Id: SubarrayControlMisc.cc,v 1.184 2014/06/04 17:09:16 mpound Exp $
 *
 * $CarmaCopyright$
 *
 */


#include "carma/control/SubarrayControlImpl.h"

#include <algorithm>
#include <iomanip>
#include <fstream>
#include <sstream>

#include "carma/corba/corba.h"
#include "carma/control/AlarmHandle.h"
#include "carma/control/AntennaHandle.h"
#include "carma/control/AntennaControls.h"
#include "carma/control/antennaHandleUtils.h"
#include "carma/control/CalibratorHandle.h"
#include "carma/control/CryoHandle.h"
#include "carma/control/DriveHandle.h"
#include "carma/control/FaultHandle.h"
#include "carma/control/FocusHandle.h"
#include "carma/control/FunctorWorkRequest.h"
#include "carma/control/HandleMethodFunctorGroup.h"
#include "carma/control/OpticalTelHandle.h"
#include "carma/control/PipelineHandle.h"
#include "carma/control/RemoteObjMethodFunctorGroup.h"
#include "carma/control/SatThreadSync.h"
#include "carma/control/stringUtils.h"
#include "carma/control/WorkerPool.h"
#include "carma/util/corrUtils.h"
#include "carma/util/CorrelatorSet.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/pipeline/pipelineControl.h"
#include "carma/services/SpectralLineCatalog.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/FileUtils.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/StringUtils.h"
#include "carma/util/WorkResult.h"

#include "carma/szautil/Percent.h"
#include "carma/szautil/String.h"

using namespace ::std;
using namespace carma;
using namespace carma::antenna;
using namespace carma::antenna::common;
using namespace carma::control;
using namespace carma::fault;
using namespace carma::monitor;
using namespace carma::pipeline;
using namespace carma::services;
using namespace carma::util;
using namespace log4cpp;

namespace {


    ControlSubsystemBase::PolarizationMonitorPointEnum::POLARIZATION
    monitorPointPolarization( const PolarizationControl::State state )
    {
        // Alias ridiculously qualified enums
        typedef carma::antenna::common::PolarizationControl PC;

        typedef
        ControlSubsystemBase::PolarizationMonitorPointEnum PME;

        switch (state) {
            case PC::POLH: return PME::HORIZONTAL;
            case PC::POLV: return PME::VERTICAL;
            case PC::POLRCP: return PME::RIGHT_CIRCULAR;
            case PC::POLLCP: return PME::LEFT_CIRCULAR;
            default: throw CARMA_EXCEPTION( ErrorException, "Invalid enum" );
        }
    }

} // namespace < unnamed >

SubarrayControlImpl::AntennaGroup
SubarrayControlImpl::getAntennaGroupForAntControlsGroup(
    const string &           commandName,
    const AntControlsGroup & antControlsGroup ) {
    AntennaGroup result;

    AntControlsGroup::const_iterator i = antControlsGroup.begin( );
    const AntControlsGroup::const_iterator iEnd = antControlsGroup.end( );

    while ( i != iEnd ) {
        AntennaHandle * const ahp = (*i)->antennaHandle( );

        if ( ahp != 0 )
            result.insert( ahp );

        ++i;
    }

    return result;
}


SubarrayControlImpl::AntennaGroup
SubarrayControlImpl::getAntennaGroupForCarmaAntNoSeq(
    const string &        commandName,
    const CarmaAntNoSeq & carmaAntNoSeq,
    const bool            allowZero,
    const bool            ignoreDupes,
    bool                  skipAntsNotOwnedByMe )
{
    const AntControlsGroup antControlsGroup =
        getAntControlsGroupForCarmaAntNoSeq( commandName,
                                             carmaAntNoSeq,
                                             allowZero,
                                             ignoreDupes,
                                             skipAntsNotOwnedByMe );

    return getAntennaGroupForAntControlsGroup( commandName, antControlsGroup );
}


SubarrayControlImpl::AntennaGroup
SubarrayControlImpl::getAntennaGroupForCarmaAntNo(
    const string &     commandName,
    const CORBA::Short carmaAntNo ) {
    return getAntennaGroupForAntControlsGroup(
        commandName,
        getAntControlsGroupForCarmaAntNo( commandName, carmaAntNo ) );
}


SubarrayControlImpl::FocusGroup
SubarrayControlImpl::getFocusGroupForAntControlsGroup(
    const string &           commandName,
    const AntControlsGroup & antControlsGroup ) {
    FocusGroup result;

    AntControlsGroup::const_iterator i = antControlsGroup.begin( );
    const AntControlsGroup::const_iterator iEnd = antControlsGroup.end( );

    while ( i != iEnd ) {
        FocusHandle * const fhp = (*i)->focusHandle( );

        if ( fhp != 0 )
            result.insert( fhp );

        ++i;
    }

    return result;
}


SubarrayControlImpl::FocusGroup
SubarrayControlImpl::getFocusGroupForCarmaAntNoSeq(
    const string &        commandName,
    const CarmaAntNoSeq & carmaAntNoSeq,
    const bool            allowZero,
    const bool            ignoreDupes )
{
    const AntControlsGroup antControlsGroup =
        getAntControlsGroupForCarmaAntNoSeq( commandName,
                                             carmaAntNoSeq,
                                             allowZero,
                                             ignoreDupes,
                                             false );

    return getFocusGroupForAntControlsGroup( commandName, antControlsGroup );
}


SubarrayControlImpl::FocusGroup
SubarrayControlImpl::getFocusGroupForCarmaAntNo(
    const string &     commandName,
    const CORBA::Short carmaAntNo ) {
    return getFocusGroupForAntControlsGroup(
        commandName,
        getAntControlsGroupForCarmaAntNo( commandName, carmaAntNo ) );
}


SubarrayControlImpl::CalibratorGroup
SubarrayControlImpl::getCalibratorGroupForCarmaAntNoSeq(
    const string &        commandName,
    const CarmaAntNoSeq & carmaAntNoSeq,
    const bool            allowZero,
    const bool            ignoreDupes )
{
    const AntControlsGroup antControlsGroup =
        getAntControlsGroupForCarmaAntNoSeq( commandName,
                                             carmaAntNoSeq,
                                             allowZero,
                                             ignoreDupes,
                                             false );

    CalibratorGroup result;

    AntControlsGroup::const_iterator i = antControlsGroup.begin( );
    const AntControlsGroup::const_iterator iEnd = antControlsGroup.end( );

    while ( i != iEnd ) {
        CalibratorHandle * const chp = (*i)->calibratorHandle( );

        if ( chp != 0 )
            result.insert( chp );

        ++i;
    }

    return result;
}


SubarrayControlImpl::CalibratorGroup
SubarrayControlImpl::getCalibratorGroupForCarmaAntNo(
    const string&      commandName,
    CORBA::Short       carmaAntNo)
{
    SeqShort ants;
    ants.length(1);
    ants[0] = carmaAntNo;
    return getCalibratorGroupForCarmaAntNoSeq(
                commandName, ants, true, false);
}


SubarrayControlImpl::CryoGroup
SubarrayControlImpl::getCryoGroupForCarmaAntNoSeq(
    const string &        commandName,
    const CarmaAntNoSeq & carmaAntNoSeq,
    const bool            allowZero,
    const bool            ignoreDupes )
{
    const AntControlsGroup antControlsGroup =
        getAntControlsGroupForCarmaAntNoSeq( commandName,
                                             carmaAntNoSeq,
                                             allowZero,
                                             ignoreDupes,
                                             false );

    CryoGroup result;

    AntControlsGroup::const_iterator i = antControlsGroup.begin( );
    const AntControlsGroup::const_iterator iEnd = antControlsGroup.end( );

    while ( i != iEnd ) {
        CryoHandle * const chp = (*i)->cryoHandle( );

        if ( chp != 0 )
            result.insert( chp );

        ++i;
    }

    return result;
}


void
SubarrayControlImpl::recordPoint( const CORBA::Short carmaAntNo )
try {
    const string filename("pointingData.txt");
    ofstream datafile(filename.c_str(), ios::out|ios::app);
    datafile << setiosflags(ios::fixed);

    ostringstream msg;
    msg << "recordPoint: Couldn't open file " << filename ;
    if (!datafile) {
        throw CARMA_EXCEPTION( carma::util::UserException, msg.str().c_str() );
    }

    const string typedAntName = computeTypedAntennaName( carmaAntNo );

    // Allow input antenna name to be any case, we make the first char UC,
    // with the rest LC
    string casedAntennaName =
        StringUtils::lowASCIIAlphaNumericToLower(typedAntName);
    casedAntennaName[0] = toupper(casedAntennaName[0]);
    string mpRoot =
        "Carma." + string(casedAntennaName) + ".AntennaCommon.Drives.";

    carmaMonitor_.readNewestConditionalCopy();
    const char* cstr   = (mpRoot + "Azimuth.requestedAzimuth").c_str();
    double az    = queryDouble(cstr);
    double el    = queryDouble((mpRoot + "Elevation.requestedElevation").c_str());
    double offAz = queryDouble((mpRoot + "offsetAz").c_str());
    double offEl = queryDouble((mpRoot + "offsetEl").c_str());

    datafile
        << setw(6) << typedAntName
        << " " << Time::getDateTimeString(0)
        << " " << setw(11) << setprecision(5) << Time::MJD()
        << " " << setw(9)  << setprecision(4) << az
        << " " << setw(9)  << setprecision(4) << el
        << " " << setw(6)  << setprecision(2) << offAz
        << " " << setw(6)  << setprecision(2) << offEl
        << endl;
} catch ( ... ) {
    rethrowCaughtAsUser();
}


void
SubarrayControlImpl::focus( const float        position,
                            const FocusGroup & focusGroup ) {
    // issue the command out to the group of antennas
    ++nextOpticsSeqNo_;
    {
        string requestIdCallString;
        {
            ostringstream oss;
            oss << "FocusHandle::setZ( position=" << position
                << ", seqNo=" << nextOpticsSeqNo_ << " )";
            requestIdCallString = oss.str( );
        }

        WorkResultSet wrs( "FocusHandle::setZ result set" );

        queueFunctorWorkRequestGroup(
            requestIdCallString,
            makeHandleMethodFunctorGroup(
                focusGroup,
                &FocusHandle::setZ,
                position, nextOpticsSeqNo_ ),
            wrs,
            *workerPool_ );

        waitForAllNormal( wrs );
    }
}


void
SubarrayControlImpl::focusZ( const float        position,
                             const CORBA::Short carmaAntNo )
try {
    CARMA_CPTRACE( util::Trace::TRACE7,
                   "SubarrayControlImpl::focusZ starts ("
                   << Time::getTimeString( 3 ) << ")." );

    { // Set control points First in the event that an antenna is offline

        ControlSubsystemBase::Antenna & ant =
            getAntMonPtForCarmaAntNo( carmaAntNo );

        ant.focusZ().setValue( position );
        markStateChange();
    }

    cmdlog() << "focusZ("
             << "position=" << position << ", "
             << carmaAntNo << ")";

    const FocusGroup focusGroup =
        getFocusGroupForCarmaAntNo( "focusZ", carmaAntNo );

    focus( position, focusGroup );

    CARMA_CPTRACE( util::Trace::TRACE7,
                   "SubarrayControlImpl::focusZ ends ("
                   << Time::getTimeString( 3 ) << ")." );
} catch ( ... ) {
    rethrowCaughtAsUser();
}


void
SubarrayControlImpl::focusX( const float        position,
                             const CORBA::Short carmaAntNo )
try {
    CARMA_CPTRACE( util::Trace::TRACE7,
                   "SubarrayControlImpl::focusX starts ("
                   << Time::getTimeString( 3 ) << ")." );

    { // Set control points First in the event that an antenna is offline

        ControlSubsystemBase::Antenna & ant =
            getAntMonPtForCarmaAntNo( carmaAntNo );

        ant.focusX().setValue( position );
        markStateChange();
    }

    cmdlog() << "focusX("
             << "position=" << position << ", "
             << carmaAntNo << ")";

    const FocusGroup focusGroup =
        getFocusGroupForCarmaAntNo( "focusX", carmaAntNo );

    // issue the command out to the group of antennas
    ++nextOpticsSeqNo_;
    {
        string requestIdCallString;
        {
            ostringstream oss;
            oss << "FocusHandle::setX( position=" << position
                << ", seqNo=" << nextOpticsSeqNo_ << " )";
            requestIdCallString = oss.str( );
        }

        WorkResultSet wrs( "FocusHandle::setX result set" );

        queueFunctorWorkRequestGroup(
            requestIdCallString,
            makeHandleMethodFunctorGroup(
                focusGroup,
                &FocusHandle::setX,
                position, nextOpticsSeqNo_ ),
            wrs,
            *workerPool_ );

        waitForAllNormal( wrs );
    }

    CARMA_CPTRACE( util::Trace::TRACE7,
                   "SubarrayControlImpl::focusX ends ("
                   << Time::getTimeString( 3 ) << ")." );
} catch ( ... ) {
    rethrowCaughtAsUser();
}


void
SubarrayControlImpl::focusY( const float        position,
                             const CORBA::Short carmaAntNo )
try {
    CARMA_CPTRACE( util::Trace::TRACE7,
                   "SubarrayControlImpl::focusY starts ("
                   << Time::getTimeString( 3 ) << ")." );

    { // Set control points First in the event that an antenna is offline

        ControlSubsystemBase::Antenna & ant =
            getAntMonPtForCarmaAntNo( carmaAntNo );

        ant.focusY().setValue( position );
        markStateChange();
    }

    cmdlog() << "focusY("
             << "position=" << position << ", "
             << carmaAntNo << ")";

    const FocusGroup focusGroup =
        getFocusGroupForCarmaAntNo( "focusY", carmaAntNo );

    // issue the command out to the group of antennas
    ++nextOpticsSeqNo_;
    {
        string requestIdCallString;
        {
            ostringstream oss;
            oss << "FocusHandle::setY( position=" << position
                << ", seqNo=" << nextOpticsSeqNo_ << " )";
            requestIdCallString = oss.str( );
        }

        WorkResultSet wrs( "FocusHandle::setY result set" );

        queueFunctorWorkRequestGroup(
            requestIdCallString,
            makeHandleMethodFunctorGroup(
                focusGroup,
                &FocusHandle::setY,
                position, nextOpticsSeqNo_ ),
            wrs,
            *workerPool_ );

        waitForAllNormal( wrs );
    }

    CARMA_CPTRACE( util::Trace::TRACE7,
                   "SubarrayControlImpl::focusY ends ("
                   << Time::getTimeString( 3 ) << ")." );
} catch ( ... ) {
    rethrowCaughtAsUser();
}


void
SubarrayControlImpl::passiveLO(const double freq)
try {
    return;
} catch ( ... ) {
    rethrowCaughtAsUser();
}


void
SubarrayControlImpl::polarization(
    const PolarizationControl::State state,
    const SeqShort &                 carmaAntNoSeq )
try {

    { // Set the monitor point value per antenna.

        AntMonPtGroup ampGroup
            = getAntMonPtGroupForCarmaAntNoSeq("polarization", carmaAntNoSeq);

        AntMonPtGroup::const_iterator i = ampGroup.begin();
        const AntMonPtGroup::const_iterator iEnd = ampGroup.end();

        for ( ; i != iEnd; ++i ) {
            const ControlSubsystemBase::Antenna * const ant = *i;
            if ( ant == 0 )
                continue;

            ant->polarization().setValue( monitorPointPolarization( state ) );
        }
        markStateChange();
    }

    return;

} catch ( ... ) {
    rethrowCaughtAsUser();
}


void
SubarrayControlImpl::radioAperture( const bool       useRadio,
                                    const SeqShort & carmaAntNoSeq )
try {
    cmdlog() << "radioAperture("
             << "useRadio=" << useRadio << ", "
             << getStringForCarmaAntNoSeq( carmaAntNoSeq ) << ")";

    const bool opticalAperture = ! useRadio;
    selectAperture( opticalAperture, carmaAntNoSeq );
} catch ( ... ) {
    rethrowCaughtAsUser();
}


void
SubarrayControlImpl::selectAperture( const bool       opticalAperture,
                                     const SeqShort & carmaAntNoSeq )
try {
    DriveControl::Aperture aperture = DriveControl::RADIO1CM;
    string apertureName = "RADIO1CM";

    if ( opticalAperture ) {
        aperture = DriveControl::OPTICAL;
        apertureName = "OPTICAL";
    } else {
        const double lo1 = subarrayContainer_.loFreq().getValue();

        if ( lo1 < 50 ) {
            aperture = DriveControl::RADIO1CM;
            apertureName = "RADIO1CM";
        } else if ( lo1 < 200 ) {
            aperture = DriveControl::RADIO3MM;
            apertureName = "RADIO3MM";
        } else {
            aperture = DriveControl::RADIO1MM;
            apertureName = "RADIO1MM";
        }
    }

    const DriveGroup driveGroup =
        getDriveGroupForCarmaAntNoSeq( "selectAperture",
                                       carmaAntNoSeq,
                                       true,
                                       false,
                                       false );

    string remoteCallString;
    {
        ostringstream oss;
        oss << "DriveControl::selectAperture(" << apertureName << ")";
        remoteCallString = oss.str( );
    }

    WorkResultSet wrs( remoteCallString + " command" );

    queueFunctorWorkRequestGroup(
        remoteCallString,
        makeRemoteObjMethodFunctorGroup(
            driveGroup,
            remoteCallString,
            "",
            &DriveControl::selectAperture,
            aperture ),
        wrs,
        *workerPool_);
    waitForAllNormal(wrs);
} catch ( ... ) {
    rethrowCaughtAsUser();
}


void
SubarrayControlImpl::cal( const CalibratorControl::Position calPos,
                          const SeqShort &                  carmaAntNoSeq )
try {
    string calPosName = "< unknown >";

    switch ( calPos ) {
        case CalibratorControl::SKY:
            calPosName = "SKY"; break;
        case CalibratorControl::AMBIENT:
            calPosName = "AMBIENT"; break;
        case CalibratorControl::FIXEDTEMP:
            calPosName = "FIXEDTEMP"; break;
        case CalibratorControl::PARTIAL:
            calPosName = "PARTIAL"; break;
    }

    cmdlog() << "cal(" << calPosName << ", "
             << getStringForCarmaAntNoSeq( carmaAntNoSeq ) << ")";

    nextCalibratorSeqNo_++; // Preferred seq no

    // Get to the top of the queue
    carmaMonitor_.readNewest();

    const CalibratorGroup calibratorGroup =
        getCalibratorGroupForCarmaAntNoSeq(
            "cal",carmaAntNoSeq, true, false );

    ostringstream request;
    request << "CalibratorHandle::cal("
        << "pos=" << calPosName
        << ", seqNo=" << nextCalibratorSeqNo_
        << ")";

    WorkResultSet wrs( request.str() + " cmd" );

     queueFunctorWorkRequestGroup(
         request.str(),
         makeHandleMethodFunctorGroup(calibratorGroup,
              &CalibratorHandle::setCalibrator,
              static_cast<MonitorSystem*>(&carmaMonitor_),
              calPos, nextCalibratorSeqNo_),
            wrs,
            *workerPool_);

     waitForAllNormal( wrs );
} catch ( ... ) {
    rethrowCaughtAsUser();
}


AntennaReady*
SubarrayControlImpl::bogus( const float numSeconds )
try {
    // test updateRepTasks
    updateRepTasks(carmaMonitor_.control(), controlSubsystem_);

    AntennaReady_var a( new AntennaReady );
    int secs = static_cast<int>(round(numSeconds));
    if (secs > 0)sleep(secs);

    CarmaMonitorSystem mon; // Check mem allocation...

    int numComplete = 3;
    int numNotComplete = 10;
    a->ready.length(numComplete);
    for (int i=0; i<numComplete; i++) a->ready[i]=numComplete+1-i;
    a->notready.length(numNotComplete);
    for (int i=0; i<numNotComplete; i++) a->notready[i]=i+10;
    return a._retn();
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


double
SubarrayControlImpl::lineFreq( const char * const line )
try {
    SpectralLineCatalog lineCat;
    const string filename =
        Program::getConfFile("catalogs/SpectralLine.cat");
    lineCat.open(filename);
    SpectralLine l = lineCat.lookup(line, line);
    Frequency f = l.getFrequency();
    return f.gigahertz();
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


double
SubarrayControlImpl::transitionFreq( const char * const molecule,
                                     const char * const transition)
try {
    SpectralLineCatalog lineCat;
    const string filename =
        Program::getConfFile("catalogs/SpectralLine.cat");
    lineCat.open(filename);
    SpectralLine l = lineCat.lookup(molecule, transition);
    Frequency f = l.getFrequency();
    return f.gigahertz();
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


void
SubarrayControlImpl::alarm( const bool         state,
                            const char * const alarmName )
try {
    cmdlog() << "alarm(" << boolalpha << state << ", \""
           << alarmName << "\")";
    string aName = "observerInitiated";
    if (alarmName != 0) {
        if (strlen(alarmName) != 0) aName = alarmName;
    }
    typedef ControlSubsystemBase::AlarmMonitorPointEnum ALARM;
    ALARM& a = subarrayContainer_.alarm();
    if (state) {
        a.setValue(ALARM::ON);
        //alarm_->turnOn(aName.c_str(), "", "", true);
    }
    else {
        a.setValue(ALARM::OFF);
        //alarm_->turnOff();
    }
} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
SubarrayControlImpl::alarm1mm( const bool state )
try {
    cmdlog() << "alarm1mm(" << boolalpha << state << ")";
    ostringstream os;
    typedef ControlSubsystemBase::Alarm1mmMonitorPointEnum ALARM;
    ALARM& a = subarrayContainer_.alarm1mm();
    if (state) {
	try {
	    ostringstream ss ;
	    ss << "Control.Subarray" << subarrayNo_ << ".loFreq";
	    double freq = queryDouble( ss.str().c_str() );
	    if ( freq > 120 )
		os << "Weather is degrading; consider switch to 3mm observing";
	    else
		os << "Weather is improving; consider switch to 1mm observing";
	} catch (...) {
	    os << "Weather has changed; check observing waveband." ;
	}
        a.setValue(ALARM::ON);

        comment( os.str().c_str() );
    } else {
        a.setValue(ALARM::OFF);
        comment(" 1mm alarm is OFF" );
    }
} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
SubarrayControlImpl::alarmEnable( const bool state )
try {
    cmdlog() << "alarmEnable(" << boolalpha << state << ")";
    alarm_->enableAlarm(state);
} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
SubarrayControlImpl::comment( const char * const obsComment )
try {
    // We probably don't want to log the change in the RTD
    //cmdlog() << "comment(" << obsComment << ")";
    const string strComment( obsComment );
    subarrayContainer_.comment().setValue( strComment );
    // Set the persistent, non-archived version of this monitor
    // point as well.  This one is used for, e.g. RTD.
    subarrayContainer_.notice().setValue( strComment );

} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
SubarrayControlImpl::lastCommand( const string & command )
try {

    subarrayContainer_.lastCommand().setValue( command );

} catch ( ... ) {
    // don't throw here since this is not an archived monitor point.
    // just log that there is a problem for someone to investigate.
    ostringstream o;
    o << "lastCommand("<<command<<")"
      << " - Unknown exception when setting lastCommand monitor point.";
    programLogErrorIfPossible( o.str() );
}


void
SubarrayControlImpl::log( const char * const entry )
try {
    Category& l = Program::getLogger();
    l << Priority::INFO << entry;
} catch ( ... ) {
    rethrowCaughtAsUser();
}


void
SubarrayControlImpl::logError( const char * const entry )
try {
    Category& l = Program::getLogger();
    l << Priority::ERROR << entry;
} catch ( ... ) {
    rethrowCaughtAsUser();
}


static void checkScriptIndex(const short index, const int max, const bool allowZero)
{
    if (index > max) {
        std::ostringstream oss;
        oss << "Index(" << index << ") exceeds maximum(" << max << ")";
        throw CARMA_EXCEPTION(carma::util::UserException, oss.str().c_str());
    }

    const short lim = (allowZero) ? 0 : 1;
    if (index < lim) {
        std::ostringstream oss;
        oss << "Index(" << index << ") too low";
        throw CARMA_EXCEPTION(carma::util::UserException, oss.str().c_str());
    }
}

bool
SubarrayControlImpl::getScriptBool( const CORBA::Short index )
try {
    ControlSubsystemBase::Script &script = subarrayContainer_.script();
    checkScriptIndex(index, script.getNumScriptBool(), false);

    return script.scriptBool(index - 1).getValue();
} catch (...) {
    rethrowCaughtAsUser();
    // Stifle compiler warning
    return false;
}

void
SubarrayControlImpl::setScriptBool( const CORBA::Short index,
                                    const bool         value )
try {
    ControlSubsystemBase::Script &script = subarrayContainer_.script();
    checkScriptIndex(index, script.getNumScriptBool(), true);

    if (index == 0) {
        for (int i = 0; i < script.getNumScriptBool(); i++)
            script.scriptBool(i).setValue(value);
    } else {
        script.scriptBool(index - 1).setValue(value);
    }

    markStateChange();
} catch (...) {
    rethrowCaughtAsUser();
}

CORBA::Double
SubarrayControlImpl::getScriptDouble( const CORBA::Short index )
try {
    ControlSubsystemBase::Script &script = subarrayContainer_.script();
    checkScriptIndex(index, script.getNumScriptDouble(), false);

    return script.scriptDouble(index - 1).getValue();
} catch (...) {
    rethrowCaughtAsUser();

    // stifle compiler warning
    return 2.0;
}

void
SubarrayControlImpl::setScriptDouble( const CORBA::Short  index,
                                      const CORBA::Double value )
try {
    ControlSubsystemBase::Script &script = subarrayContainer_.script();
    checkScriptIndex(index, script.getNumScriptDouble(), true);

    if (index == 0) {
        for (int i = 0; i < script.getNumScriptDouble(); i++)
            script.scriptDouble(i).setValue(value);
    } else {
        script.scriptDouble(index - 1).setValue(value);
    }

    markStateChange();
} catch (...) {
    rethrowCaughtAsUser();
}

CORBA::Long
SubarrayControlImpl::getScriptInt( const CORBA::Short index )
try {
    ControlSubsystemBase::Script &script = subarrayContainer_.script();
    checkScriptIndex(index, script.getNumScriptInt(), false);

    return script.scriptInt(index - 1).getValue();
} catch (...) {
    rethrowCaughtAsUser();
    // Stifle compiler warning
    return 0;
}

void
SubarrayControlImpl::setScriptInt( const CORBA::Short index,
                                   const CORBA::Long value )
try {
    ControlSubsystemBase::Script &script = subarrayContainer_.script();
    checkScriptIndex(index, script.getNumScriptInt(), true);

    if (index == 0) {
        for (int i = 0; i < script.getNumScriptInt(); i++)
            script.scriptInt(i).setValue(value);
    } else {
        script.scriptInt(index - 1).setValue(value);
    }

    markStateChange();
} catch (...) {
    rethrowCaughtAsUser();
}


// Open a scriptState file, creating it if necessary
// Caller is responsible for closing the file 
string SubarrayControlImpl::scriptStringFilename(int index)
{
    string fn = "scriptLog";
    if (index == 0) {
        fn = "scriptLog";
    }
    else {
        ostringstream o;
        o << "scriptString" << index;
        fn = o.str();
    }
    string fname = scriptStateDir_ + "/" + fn;
    return fname;
}


const size_t STRING_BUFF_LEN = 500;

char*
SubarrayControlImpl::getScriptString( const CORBA::Short index )
try {
    checkScriptIndex(index, nScriptStrings_, false);
    string fn  = scriptStringFilename(index);
    const char*  cfn = fn.c_str();
    // Create file if does not exist
    if (!FileUtils::exists(fn)) {
        ofstream f(cfn);
    }
    // Read contents of file
    ifstream f(cfn);
    char buff[STRING_BUFF_LEN+1];
    buff[STRING_BUFF_LEN] = '\0';
    f.read(buff, STRING_BUFF_LEN);
    buff[f.gcount()] = '\0';
    return CORBA::string_dup(buff);
} catch (...) {
    rethrowCaughtAsUser();
    // Stifle compiler warning
    return (char*)"";
}

void
SubarrayControlImpl::setScriptString( const CORBA::Short index,
                                      const char * const value )
try {

    // truncate string to BUFF_LEN characters
    string s(value);
    if (s.length() > STRING_BUFF_LEN) s.erase(STRING_BUFF_LEN);

    checkScriptIndex(index, nScriptStrings_, true);

    if (index == 0) {
        for (int i = 0; i < nScriptStrings_; i++) {
            string fn  = scriptStringFilename(i+1).c_str();
            ofstream f(fn.c_str());
            f.write(s.c_str(), s.size());
        }
    } else {
        string fn  = scriptStringFilename(index).c_str();
        ofstream f(fn.c_str());
        f.write(s.c_str(), s.size());
    }

} catch (...) {
    rethrowCaughtAsUser();
}

void
SubarrayControlImpl::appendScriptString(const CORBA::Short index,
                                        const char* const value)
try {
    checkScriptIndex(index, nScriptStrings_, true);

    // retrieve original string and append, truncating to STRING_BUFF_LEN chars
    string fn  = scriptStringFilename(index).c_str();
    const char*  cfn = fn.c_str();
    if (!FileUtils::exists(fn)) {
        ofstream f(cfn);
    }
    ifstream f(cfn);
    char buff[STRING_BUFF_LEN+1];
    buff[STRING_BUFF_LEN] = '\0';
    f.read(buff, STRING_BUFF_LEN);
    buff[f.gcount()] = '\0';
    string s(value);
    size_t totlen = s.length() + strlen(buff);
    if (totlen > STRING_BUFF_LEN) {
        s.erase(STRING_BUFF_LEN - strlen(buff));
    }
    ofstream fout(cfn, ios::app);
    fout.write(s.c_str(), s.size());

} catch (...) {
    rethrowCaughtAsUser();
}

const size_t HISTORY_BUFF_LEN = 4000000;
const int    HISTORY_INDEX = 0;// History has a special scriptString index
void
SubarrayControlImpl::addScriptHistory(const char* const value)
try {
    // Don't exceed maximum size. Silently throw away messages to avoid
    // crashing scripts when the history gets full.
    // Retrieve original contents and append, truncating to HISTORY_BUFF_LEN 
    // chars
    string fn  = scriptStringFilename(HISTORY_INDEX).c_str();
    const char*  cfn = fn.c_str();
    if (!FileUtils::exists(fn)) {
        ofstream f(cfn);
    }
    ifstream f(cfn);
    char buff[HISTORY_BUFF_LEN+1];
    f.read(buff, HISTORY_BUFF_LEN);
    buff[f.gcount()] = '\0';
    string s(value);
    size_t totlen = s.length() + strlen(buff);
    if (totlen > HISTORY_BUFF_LEN) {
        s.erase(HISTORY_BUFF_LEN - strlen(buff));
        scriptHistoryIsFull_ = true;
    }
    ofstream fout(cfn, ios::app);
    fout.write(s.c_str(), s.size());

} catch (...) {
    rethrowCaughtAsUser();
}

char* SubarrayControlImpl::getScriptHistory()
try {
    string fn  = scriptStringFilename(HISTORY_INDEX);
    const char*  cfn = fn.c_str();
    // Create file if does not exist
    if (!FileUtils::exists(fn)) {
        ofstream f(cfn);
    }
    // Read contents of file
    ifstream f(cfn);
    char buff[HISTORY_BUFF_LEN+1];
    f.read(buff, HISTORY_BUFF_LEN);
    buff[f.gcount()] = '\0';
    scriptHistoryIsFull_ = (strlen(buff) >= HISTORY_BUFF_LEN);
    return CORBA::string_dup(buff);
} catch (...) {
    rethrowCaughtAsUser();
    // Stifle compiler warning
    return (char*)"";
}

// A better name would be getScriptHistoryIsFull
bool
SubarrayControlImpl::getScriptHistoryFull()
try {
    return this->scriptHistoryIsFull_;
} catch (...) {
    rethrowCaughtAsUser();
    // Stifle compiler warning
    return false;
}

void
SubarrayControlImpl::clearScriptAll( )
try {

    setScriptBool(0, false);
    setScriptDouble(0, 0.0);
    setScriptInt(0, 0);
    setScriptString(0, "");    

    int historyIndex = 0; // History has a special scriptString index
    string fn  = scriptStringFilename(historyIndex).c_str();
    ofstream f(fn.c_str());
    f.write("", 0);
    scriptHistoryIsFull_ = false;

    markStateChange();
} catch (...) {
    rethrowCaughtAsUser();
}


void
SubarrayControlImpl::setScriptName( const char * const name )
try {
    subarrayContainer_.scriptName().setValue(name);
} catch ( ... ) {
    rethrowCaughtAsUser();
}


char *
SubarrayControlImpl::getScriptName( )
try {
    const string name = subarrayContainer_.scriptName().getValue();
    return CORBA::string_dup( name.c_str() );
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


void
SubarrayControlImpl::setScriptState( const ScriptStateType state )
try {
    typedef ControlSubsystemBase::ScriptStateMonitorPointEnum STATE;
    STATE& s = subarrayContainer_.scriptState();
    switch (state) {
        case RUNNING:   s.setValue(STATE::RUNNING);   break;
        case COMPLETED: s.setValue(STATE::COMPLETED); break;
        case CRASHED:   s.setValue(STATE::CRASHED);   break;
        case CANCELED:  s.setValue(STATE::CANCELED);  break;
    }
} catch ( ... ) {
    rethrowCaughtAsUser();
}


ScriptStateType
SubarrayControlImpl::getScriptState( )
try {
    typedef ControlSubsystemBase CS;
    typedef ControlSubsystemBase::ScriptStateMonitorPointEnum STATE;
    STATE::SCRIPTSTATE s = subarrayContainer_.scriptState().getValue();
    switch (s) {
        case STATE::RUNNING:    return(RUNNING);   break;
        case STATE::COMPLETED:  return(COMPLETED); break;
        case STATE::CRASHED:    return(CRASHED);   break;
        case STATE::CANCELED:   return(CANCELED);  break;
        // Return crashed for internal err as well...
        case STATE::SCRIPTSTATE_COUNT:
        default:                return(CRASHED);   break;
    }
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


void
SubarrayControlImpl::setConfigName( const char * const name )
try {
    subarrayContainer_.configName().setValue(name);
    markStateChange( );
} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
SubarrayControlImpl::pointStatus( const PointStatusType status,
	                          const SeqShort & carmaAntNoSeq )
try {

    const PointStatusMonitorPointEnum::POINTSTATUS value
        = ( status == ON_SOURCE
              ? PointStatusMonitorPointEnum::ONSRC
              : PointStatusMonitorPointEnum::OFFSRC
          );

    AntMonPtGroup ampGroup
        = getAntMonPtGroupForCarmaAntNoSeq("pointStatus", carmaAntNoSeq);

    AntMonPtGroup::const_iterator i = ampGroup.begin();
    const AntMonPtGroup::const_iterator iEnd = ampGroup.end();
    for ( ; i != iEnd; ++i ) {
        const ControlSubsystemBase::Antenna * const ant = *i;
    if ( ant == 0 )
        continue;
        ant->pointStatus().setValue( value );
    }

    // now set for subarray as a whole. (see bug 933)
    subarrayContainer_.pointStatus().setValue( value );
    markStateChange();

} catch ( ... ) {
    rethrowCaughtAsUser();
}


SubarrayControlImpl::FaultSysGroup
SubarrayControlImpl::getFaultSysGroup( )
{
    FaultSysGroup result;

    FaultHandle * const fhp = faultSys_.get();

    if ( fhp != 0 )
        result.insert( fhp );

    return result;
}


static std::string pref2str(const enum carma::fault::EffectPreference pref)
{
    switch (pref) {
    case PREF_NONE:
        return "PREF_NONE";
    case PREF_BLANK:
        return "PREF_BLANK";
    case PREF_FLAG:
        return "PREF_FLAG";
    default:
        return "UNKNOWN";
    }
}


void
SubarrayControlImpl::setFaultSystemDriveErrorPreference(const enum carma::fault::EffectPreference inPref)
try {
    const std::string stateParamText = pref2str(inPref);

    cmdlog() << "setFaultSystemDriveBlankingState(" << stateParamText << ")";

    const FaultSysGroup faultSysGroup = getFaultSysGroup();

    if ( faultSysGroup.empty() )
        return;  // Silently ignore for subarrays without fault system access

    const string requestIdCallString =
        "FaultControl::setDriveErrorPreference(" + stateParamText + ")";

    WorkResultSet wrs( requestIdCallString );

    queueFunctorWorkRequestGroup(
        requestIdCallString,
        makeRemoteObjMethodFunctorGroup(
            faultSysGroup,
            "setDriveErrorPreference",
            stateParamText,
            &FaultControl::setDriveErrorPreference,
            static_cast<CORBA::UShort>(subarrayNo_),
            inPref ),
        wrs,
        *workerPool_ );

    waitForAllNormal( wrs );
} catch ( ... ) {
    rethrowCaughtAsUser();
}


namespace {


void
convertSeqStringToUniqueStringVec(
    const control::SeqString & seqString,
    vector< string > &         uniqueStringVec )
{
    const int seqLen = seqString.length();

    uniqueStringVec.clear();
    uniqueStringVec.reserve( seqLen );

    for ( int i = 0; i < seqLen; ++i )
        uniqueStringVec.push_back( static_cast<const char *>( seqString[i] ) );

    stable_sort( uniqueStringVec.begin(), uniqueStringVec.end() );

    const vector< string >::iterator eraseBegin =
        unique( uniqueStringVec.begin(), uniqueStringVec.end() );

    uniqueStringVec.erase( eraseBegin, uniqueStringVec.end() );
}


string
getMpNamesParamText( const vector< string > & mpNames )
{
    if ( mpNames.size() == 1 )
        return mpNames[0];

    ostringstream oss;
    oss << "<" << mpNames.size() << " monitor points>";
    return oss.str();
}


}  // namespace < anonymous >


void
SubarrayControlImpl::disableFaultSystemAlarms(
    const SeqString & inMonitorPointNames )
try {
    vector< string > mpNames;
    convertSeqStringToUniqueStringVec( inMonitorPointNames, mpNames );

    const string mpNamesParamText = getMpNamesParamText( mpNames );

    cmdlog() << "disableFaultSystemAlarms(" << mpNamesParamText << ")";

    const FaultSysGroup faultSysGroup = getFaultSysGroup();

    if ( faultSysGroup.empty() )
        return;  // Silently ignore for subarrays without fault system access

    const string handleMethod =
        "FaultHandle::disableAlarms(" + mpNamesParamText + ")";

    WorkResultSet wrs( handleMethod );

    queueFunctorWorkRequestGroup(
        handleMethod,
        makeHandleMethodFunctorGroup(
            faultSysGroup,
            &FaultHandle::disableAlarms,
            mpNames ),
        wrs,
        *workerPool_ );

    waitForAllNormal( wrs );
} catch ( ... ) {
    rethrowCaughtAsUser();
}


void
SubarrayControlImpl::restoreFaultSystemAlarms(
    const SeqString & inMonitorPointNames )
try {
    vector< string > mpNames;
    convertSeqStringToUniqueStringVec( inMonitorPointNames, mpNames );

    const string mpNamesParamText = getMpNamesParamText( mpNames );

    cmdlog() << "restoreFaultSystemAlarms(" << mpNamesParamText << ")";

    const FaultSysGroup faultSysGroup = getFaultSysGroup();

    if ( faultSysGroup.empty() )
        return;  // Silently ignore for subarrays without fault system access

    const string handleMethod =
        "FaultHandle::restoreAlarms(" + mpNamesParamText + ")";

    WorkResultSet wrs( handleMethod );

    queueFunctorWorkRequestGroup(
        handleMethod,
        makeHandleMethodFunctorGroup(
            faultSysGroup,
            &FaultHandle::restoreAlarms,
            mpNames ),
        wrs,
        *workerPool_ );

    waitForAllNormal( wrs );
} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
SubarrayControlImpl::setFaultSystemAlarmEnableState(const bool inStateIsOn)
try {
    const std::string stateParamText = (inStateIsOn) ? "state=ON" : "state=OFF";
    cmdlog() << "setFaultSystemAlarmEnableState(" << stateParamText << ")";

    // silently ignore for subarrays without fault system access
    const FaultSysGroup faultSysGroup = getFaultSysGroup();
    if (faultSysGroup.empty())
        return;

    const std::string requestIdCallString = "FaultControl::setAlarmEnable(" + stateParamText + ")";

    WorkResultSet wrs(requestIdCallString);

    queueFunctorWorkRequestGroup(
        requestIdCallString,
        makeRemoteObjMethodFunctorGroup(
            faultSysGroup,
            "setAlarmEnable",
            stateParamText,
            &FaultControl::setAlarmEnable,
            static_cast<CORBA::UShort>(subarrayNo_),
            inStateIsOn),
        wrs,
        *workerPool_);

    waitForAllNormal(wrs);
} catch (...) {
    rethrowCaughtAsUser();
}


void SubarrayControlImpl::resetTimeSinceLastIntegration()
try {
    cmdlog() << "resetTimeSinceLastIntegration( )";

    WorkResultSet wrs( "PipelineControl::resetTimeSinceLastIntegration cmd" );

    queueFunctorWorkRequestGroup(
        "PipelineControl::resetTimeSinceLastIntegration()",
        makeRemoteObjMethodFunctorGroup(
            getPipelineGroup( ),
            "resetTimeSinceLastIntegration",
            "",
            &PipelineControl::resetTimeSinceLastIntegration ),
        wrs,
        *workerPool_ );

    waitForAllNormal( wrs );
} catch ( ... ) {
    rethrowCaughtAsUser( );
}

void
SubarrayControlImpl::antennaInitialized(const CORBA::Boolean state,
                                        const SeqShort & carmaAntNoSeq )
try {
    const bool allowZero = true;
    const bool ignoreDupes = true;
    const bool skipAntsNotOwnedByMe = true;

    const AntennaGroup antGroup = getAntennaGroupForCarmaAntNoSeq(
        "antennaInitialized",
        carmaAntNoSeq,
        allowZero,
        ignoreDupes,
        skipAntsNotOwnedByMe );

    ostringstream request;
    request << "AntennaHandle::setInitialization("
        << "state=" << ( state ? "true" : "false" )
        << " ).";

    WorkResultSet wrs( request.str() + " cmd" );


    queueFunctorWorkRequestGroup(
        request.str(),
        makeHandleMethodFunctorGroup(
            antGroup,
            &AntennaHandle::setInitialization,
            state),
        wrs,
        *workerPool_);

    waitForAllNormal( wrs );

} catch ( ... ) {
    rethrowCaughtAsUser();
}

void SubarrayControlImpl::startTrack()
try {
    lastStartTrackMJD_ = util::Time::MJD();
} catch ( ... ) {
    rethrowCaughtAsUser();
}

unsigned SubarrayControlImpl::getMaxNumBands(const carma::util::CorrelatorType cType)
{
  CorrelatorSet corrSet(cType);
  unsigned nBands = 0;
  if(corrSet.includesSpectral())
    nBands += util::NUM_SLC_BANDS;
  if(corrSet.includesWideband())
    nBands += util::NUM_WBC_BANDS;
  if(corrSet.includesC3gMax8())
    nBands += util::NUM_C3G_BANDS;
  if(corrSet.includesC3gMax23())
    nBands += util::NUM_C3G_BANDS;

  return nBands;
}

bool SubarrayControlImpl::
getBlockDownconverterEnabled(const carma::util::CorrelatorType cType, const unsigned corrBandNo ) const
{
    switch ( cType ) {
        case util::CORR_SPECTRAL :
            return slBdcEnabled_.at(corrBandNo - 1);
        case util::CORR_WIDEBAND :
            return wbBdcEnabled_.at(corrBandNo - 1);
        case util::CORR_C3GMAX8:
        case util::CORR_C3GMAX23:
            return false;
        default:
        case util::CORR_NONE:
        {
            ostringstream os;
            os << "Bad correlator type enum : " << getStringForCorrType( cType )
               << ". Must be one of CORR_SPECTRAL, CORR_WIDEBAND, CORR_C3GMAX8, CORR_C3GMAX23.";
            throw CARMA_EXCEPTION(util::IllegalArgumentException, os.str().c_str() );
        }
    }
}

void SubarrayControlImpl::setBlockDownconverterEnabled( 
                      const carma::util::CorrelatorType cType, 
                      const unsigned corrBandNo, const bool enabled)
{
    switch ( cType ) {
        case util::CORR_SPECTRAL :
            slBdcEnabled_.at(corrBandNo - 1) = enabled;
        case util::CORR_WIDEBAND :
            wbBdcEnabled_.at(corrBandNo - 1) = enabled;
        case util::CORR_C3GMAX8:
        case util::CORR_C3GMAX23:
          return;
        case util::CORR_NONE:
            return;
        default:
        {
            ostringstream os;
            os << "Bad correlator type enum : " << getStringForCorrType( cType )
               << ". Must be one of CORR_SPECTRAL, CORR_WIDEBAND,CORR_C3GMAX8,CORR_C3GMAX23, or CORR_NONE.";
            throw CARMA_EXCEPTION(util::IllegalArgumentException, os.str().c_str() );
        }
    }
}

bool
SubarrayControlImpl::isAntennaInitialized( const short carmaAntNo )
try {
    ScopedLogNdc ndc("SubarrayControlImpl::isAntennaInitialized");
    const string typedAntName = computeTypedAntennaName( carmaAntNo );

    // Allow input antenna name to be any case, we make the first char UC,
    // with the rest LC
    string casedAntennaName =
        StringUtils::lowASCIIAlphaNumericToLower(typedAntName);
    casedAntennaName[0] = toupper(casedAntennaName[0]);
    const string mpRoot =
        "Carma." + string(casedAntennaName) + ".AntennaCommon.initialized";
    carmaMonitor_.readNewestConditionalCopy();
    const char* cstr    = mpRoot.c_str();
    bool antInitialized = false;
    try {
        antInitialized = queryBool(cstr);
    } catch ( ... ) {
        ostringstream os;
        os << "Couldn't get monsys for antenna C"<<carmaAntNo
           << ". Returning false." ;
        programLogNoticeIfPossible(os.str());
    }

    return antInitialized;
} catch ( ... ) {
    rethrowCaughtAsUser();
    throw CARMA_EXCEPTION( UserException, "< unknown exception >" );
}

/**.......................................................................
 * Return True if the antenna is shadowed, false if not
 */
bool SubarrayControlImpl::isShadowedNow(const short carmaAntNo, ShadowingType type, double diameterFraction)
{
  sza::util::Percent percent;
  percent.setPercentMax1(diameterFraction);

  shadowingCalculator_.setInternalShadowingDiameterPercentage(percent);
  shadowingCalculator_.setSweptVolumeShadowingDiameterPercentage(percent);
  shadowingCalculator_.update();

  std::vector<bool> shadowFlags;

  switch (type) {
  case SHADOW_INTERNAL:
    shadowFlags = shadowingCalculator_.getInternalShadowing();
    break;
  default:
    shadowFlags = shadowingCalculator_.getSweptVolumeShadowing();
    break;
  }

  return shadowFlags[carmaAntNo-1];
}

/**.......................................................................
 * Return True if the antenna is shadowed for the requested HA/DEC
 * position, false if not
 */
bool SubarrayControlImpl::isShadowedHaDec(const short carmaAntNo, double hourAngleHours, double decDegrees, ShadowingType type, double diameterFraction)
{
  sza::util::Percent percent;
  percent.setPercentMax1(diameterFraction);

  shadowingCalculator_.setInternalShadowingDiameterPercentage(percent);
  shadowingCalculator_.setSweptVolumeShadowingDiameterPercentage(percent);

  sza::util::HourAngle ha;
  ha.setHours(hourAngleHours);

  sza::util::Declination dec;
  dec.setDegrees(decDegrees);

  shadowingCalculator_.updateConfigurationInformation();
  shadowingCalculator_.setHaDec(ha, dec);
  shadowingCalculator_.updateShadowFlags();

  std::vector<bool> shadowFlags;

  switch (type) {
  case SHADOW_INTERNAL:
    shadowFlags = shadowingCalculator_.getInternalShadowing();
    break;
  default:
    shadowFlags = shadowingCalculator_.getSweptVolumeShadowing();
    break;
  }

  return shadowFlags[carmaAntNo-1];
}

/**.......................................................................
 * Return True if the antenna is shadowed for the requested Source,
 * false if not
 */
bool SubarrayControlImpl::isShadowedSource(const short carmaAntNo, std::string sourceName, double lstHours, ShadowingType type, double diameterFraction)
{
  try {
    carma::services::Source src = findSource(sourceName);

    double raHours = src.getXCoordinate().hours();
    
    // If the passed LST is negative, take the current LST, else use the passed LST
    
    lstHours = lstHours < 0.0 ? lst() : lstHours;
    
    double haHours = lstHours - raHours;
    double decDegrees = src.getYCoordinate().degrees();

    //    ThrowCarmaUserException("LST = " << lstHours << " RA = " << raHours << " HA = " << haHours << " DEC = " << decDegrees);

    return isShadowedHaDec(carmaAntNo, haHours, decDegrees, type, diameterFraction);

  } catch (...) {
    rethrowCaughtAsUser();
  }

  return false;
}

/**.......................................................................
 * Method to return a source, either from the default catalog, or the
 * user catalog
 */
carma::services::Source SubarrayControlImpl::findSource(
            const std::string & sourceName)
{
  carma::services::Source src;

  //------------------------------------------------------------
  // First look for the source in the default source catalog
  //------------------------------------------------------------

  try {
    std::string srcNameCaps = sza::util::String::firstToUpper(sourceName);
    
    const std::map<std::string, carma::services::Source>& srcMap = sourceCatalog_.getSourceMap();
    
    if(srcMap.find(srcNameCaps) == srcMap.end()) {
      ThrowCarmaUserException("No source named: " << srcNameCaps << " found in the catalog");
    }

    carma::services::Source src = sourceCatalog_.getSourceMap().find(srcNameCaps)->second;

    std::ostringstream os;
    os << "Found source " << sourceName << " in the catalog";
    COUT("Found source " << sourceName << " in the catalog");
    programLogInfo(os.str());

    COUT("Source = " << src);

  } catch(...) {

    // Not found in default catalog -- check user catalog
    // Call info() first so it can throw if something's wrong

    std::ostringstream os;
    os << "Source " << sourceName << " not found in the catalog -- trying info";
    COUT("Source " << sourceName << " not found in the catalog -- trying info");
    programLogInfo(os.str());

    (void) info(sourceName.c_str());

    os.str("");
    os << "Info didn't throw -- guess it's there";
    COUT("Info didn't throw -- guess it's there");
    programLogInfo(os.str());

    Ephemeris ephem;
    ephem.setMJD();
    ephem.setSource(sourceName, userCatalog_);

    carma::services::Angle ra, dec;

    ra.setRadians(ephem.getRa());
    dec.setRadians(ephem.getDec());

    COUT("Source = " << src);

    src.setXCoordinate(ra);
    src.setYCoordinate(dec);

  }

  return src;
}
//=================================================================== 
//      RepTask methods   
// Internal routine used by other repTask methods
ControlSubsystem::RepTask& getRepTask(ControlSubsystem& cs, int taskIndex)
try {
    if(taskIndex < 0) {
        ostringstream m; 
        m << "Task index (" << taskIndex << ") must be >= 0";
        ThrowCarmaUserException(m.str());
    }
    int l = cs.repTaskCount();
    if(taskIndex >= l) {
        ostringstream m; 
        m << "Task index (" << taskIndex << ") must be less than " << l;
        ThrowCarmaUserException(m.str());
    }
    return cs.repTask(taskIndex);
} catch ( ... ) {
    rethrowCaughtAsUser();
    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}
void SubarrayControlImpl::setRepTaskName(int taskIndex, const char* taskName)
try {
    ControlSubsystem::RepTask& rti = getRepTask(controlSubsystem_,taskIndex);
    rti.taskName().setValue(taskName);
    rti.timeLastDone().setValue(Time::MJD());
    rti.since().setValue(0);
    rti.autoReady().setValue(false);
    rti.reminderReady().setValue(false);
    
} catch (const util::UserException&) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();
    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}
void SubarrayControlImpl::setRepTaskInterval(int taskIndex,  double interval)   
try {
    getRepTask(controlSubsystem_, taskIndex).repeatInterval().setValue(interval);
} catch (const util::UserException&) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();
    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}
void SubarrayControlImpl::setRepTaskRemind(int taskIndex, double interval)
try {
    getRepTask(controlSubsystem_, taskIndex).reminderInterval().setValue(interval);
} catch (const util::UserException&) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();
    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}
void SubarrayControlImpl::setRepTaskAuto(int taskIndex, double interval)
try {
    getRepTask(controlSubsystem_, taskIndex).autoInterval().setValue(interval);
} catch (const util::UserException&) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();
    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}
void SubarrayControlImpl::setRepTaskCompleted(int taskIndex, double mjd)         
try {
    getRepTask(controlSubsystem_, taskIndex).timeLastDone().setValue(mjd);
} catch (const util::UserException&) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();
    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}
// Update since value and ready flags based on current time
// Does not throw exceptions, just increments a counter
void SubarrayControlImpl::updateRepTasks(
        ControlSubsystem& inControlSubsys, ControlSubsystem& outControlSubsys)
    const        
try {
    // Update is only done in subarray1
    if (subarrayNo_ != 1) return;
    double now = Time::MJD();
    int l = inControlSubsys.repTaskCount();
    for (int i=0; i<l; i++) {    
        ControlSubsystem::RepTask& rtIn  = inControlSubsys.repTask(i);
        ControlSubsystem::RepTask& rtOut = outControlSubsys.repTask(i);
        double last = rtIn.timeLastDone().getValue();
        if ((last < 1000) || (last > 100000)) continue;
        double since = now - last;
        rtOut.since().setValue(since);
        double reminderInterval = rtIn.reminderInterval().getValue();
        double autoInterval     = rtIn.autoInterval().getValue();
        rtOut.reminderReady().setValue(since>reminderInterval);
        rtOut.autoReady().setValue(since>autoInterval);
    }
} catch (...) {
    int e = outControlSubsys.repTaskErrors().getValue();
    outControlSubsys.repTaskErrors().setValue(e+1);
}
//===================================================================    
// Testing
char* SubarrayControlImpl::testMessageSize(CORBA::ULong size)
try {
    const string b = "abcedfghijklmnopqrstuvwxyz1234567890";
    const unsigned int sz = b.size();
    string s = "";
    for (unsigned int i=0; i<size/sz; i++) {
        s += b;
    }
    s += b.substr(0, size%sz);
    return CORBA::string_dup(s.c_str());
} catch ( const util::UserException & ) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}



//==========================================================================

