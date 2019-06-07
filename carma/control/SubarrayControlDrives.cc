/**
 *
 * Carma control interface server implementation for drive system commands.
 *
 * @author: Steve Scott
 *
 * $Id: SubarrayControlDrives.cc,v 1.118 2011/09/26 17:42:46 iws Exp $
 *
 * $CarmaCopyright$
 *
 */


#include "carma/control/SubarrayControlImpl.h"

#include <set>
#include <string>
#include <sstream>
#include <iostream>

#include "carma/corba/corba.h"
#include "carma/control/AntennaControls.h"
#include "carma/control/antennaHandleUtils.h"
#include "carma/control/DriveHandle.h"
#include "carma/control/WorkerPool.h"
#include "carma/control/FunctorWorkRequest.h"
#include "carma/control/HandleMethodFunctorGroup.h"
#include "carma/control/RemoteObjMethodFunctorGroup.h"
#include "carma/control/SatThreadSync.h"
#include "carma/control/Subarray.h"
#include "carma/monitor/AntennaCommon.h"
#include "carma/util/corbaSequenceUtils.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/WorkResult.h"


using namespace ::std;
using namespace log4cpp;
using namespace CORBA;
using namespace carma;
using namespace carma::antenna;
using namespace carma::antenna::common;
using namespace carma::control;
using namespace carma::monitor;
using namespace carma::util;


namespace {


set< DriveHandle * >
generateAntennaTypeSubset( const set< DriveHandle * > & driveGroup,
                           const AntennaType            antType ) {
    typedef set< DriveHandle * > LocalDriveGroupType;

    LocalDriveGroupType result;

    LocalDriveGroupType::const_iterator i = driveGroup.begin( );
    const LocalDriveGroupType::const_iterator iEnd = driveGroup.end( );

    for ( ; i != iEnd; ++i ) {
        DriveHandle * const driveHandle = *i;

        if ( driveHandle == 0 )
            continue;

        const unsigned short carmaAntNo = driveHandle->getCarmaAntennaNo( );

        if ( computeAntennaType( carmaAntNo ) == antType )
            result.insert( driveHandle );
    }

    return result;
}


DriveControl::Aperture
convertApertureEnum( const Aperture ap ) {
    switch ( ap ) {
        case APERTURE_OPTICAL:
            return DriveControl::OPTICAL;

        case APERTURE_RADIO1MM:
            return DriveControl::RADIO1MM;

        case APERTURE_RADIO3MM:
            return DriveControl::RADIO3MM;

        case APERTURE_RADIO1CM:
            return DriveControl::RADIO1CM;

        default:
            throw CARMA_EXCEPTION( util::UserException, "bad aperature value" );
    }
}


}  // namespace < anonymous >

// Deprecated
unsigned long
SubarrayControlImpl::getDriveCommandLateAfterMillis(
    const DriveGroup & driveGroup,
    const size_t       maxVaxOpsPerOvroAnt )
{
    return getDefaultLateAfterMillis() ;
}

// Deprecated
unsigned long
SubarrayControlImpl::getDriveCommandLateAfterMillis(
    const DriveGroup & driveGroup )
{
    return getDriveCommandLateAfterMillis( driveGroup, 1 );
}


SubarrayControlImpl::DriveGroup
SubarrayControlImpl::getDriveGroupForAntControlsGroup(
    const string &           commandName,
    const AntControlsGroup & antControlsGroup ) {
    DriveGroup result;

    AntControlsGroup::const_iterator i = antControlsGroup.begin( );
    const AntControlsGroup::const_iterator iEnd = antControlsGroup.end( );

    while ( i != iEnd ) {
        DriveHandle * const dhp = (*i)->driveHandle( );

        if ( dhp != 0 )
            result.insert( dhp );

        ++i;
    }

    return result;
}


SubarrayControlImpl::DriveGroup
SubarrayControlImpl::getDriveGroup( const string & commandName ) {
    return getDriveGroupForAntControlsGroup( commandName,
                                             getAntControlsGroup( ) );
}


SubarrayControlImpl::DriveGroup
SubarrayControlImpl::getDriveGroupForCarmaAntNoSeq(
    const string &        commandName,
    const CarmaAntNoSeq & carmaAntNoSeq,
    const bool            allowZero,
    const bool            ignoreDupes,
    const bool            skipUnownedAnts )
{
    return getDriveGroupForAntControlsGroup(
        commandName,
        getAntControlsGroupForCarmaAntNoSeq( commandName,
                                             carmaAntNoSeq,
                                             allowZero,
                                             ignoreDupes,
                                             skipUnownedAnts ) );
}


SubarrayControlImpl::DriveGroup
SubarrayControlImpl::getDriveGroupForCarmaAntNo(
    const string &     commandName,
    const CORBA::Short carmaAntNo ) {
    return getDriveGroupForAntControlsGroup(
        commandName,
        getAntControlsGroupForCarmaAntNo( commandName, carmaAntNo ) );
}


void
SubarrayControlImpl::mountOffset( const double       az,
                                  const double       el,
                                  const CORBA::Short carmaAntNo )
try {
    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::mountOffset starts" );

    cmdlog() << "mountOffset("
             << "az=" << az << ", "
             << "el=" << el << ", "
             << getStringForCarmaAntNo( carmaAntNo ) << ")";

    const DriveGroup driveGroup =
        getDriveGroupForCarmaAntNo( "mountOffset", carmaAntNo );

    ++nextDriveSeqNo_;

    // issue the command out to the group of antennas
    {
        WorkResultSet wrs( "DriveHandle::setMountOffset result set" );

        queueFunctorWorkRequestGroup(
            "DriveHandle::setMountOffset()",
            makeHandleMethodFunctorGroup(
                driveGroup,
                &DriveHandle::setMountOffset,
                az,
                el,
                nextDriveSeqNo_ ),
            wrs,
            *workerPool_ );
        waitForAllNormal(wrs);
    }

    { // Set control points
        ControlSubsystemBase::Antenna & antMp =
            getAntMonPtForCarmaAntNo( carmaAntNo );

        antMp.azimuthMountOffset().setValue( az );
        antMp.elevationMountOffset().setValue( el );
        markStateChange();
    }

    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::mountOffset ends" );
} catch ( ... ) {
    rethrowCaughtAsUser( );
}


void
SubarrayControlImpl::stow( const DriveControl::Position position,
                           const SeqShort & carmaAntNoSeq )
try {
    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::stow starts" );

    ostringstream cmdos;
    carma::monitor::Stow::PositionMonitorPointEnum::POSITION
        stowPosition = carma::monitor::Stow::PositionMonitorPointEnum::SAFE;
    cmdos << "stow(" ;
    switch ( position ) {
	case DriveControl::ZENITH  :
	    cmdos << "ZENITH, ";
	    stowPosition = carma::monitor::Stow::PositionMonitorPointEnum::ZENITH;
	    break;
	case DriveControl::SERVICE :
	    cmdos << "SERVICE , ";
	    stowPosition = carma::monitor::Stow::PositionMonitorPointEnum::SERVICE;
	    break;
	case DriveControl::SAFE    :
	    cmdos << "SAFE , ";
	    stowPosition = carma::monitor::Stow::PositionMonitorPointEnum::SAFE;
	    break;
	default:
	// that's a problem!
	    cmdos << "UNKNOWN, ";
	    break;
    }

    cmdlog() << cmdos.str()
	     << getStringForCarmaAntNoSeq( carmaAntNoSeq ) << ")";

    const DriveGroup driveGroup =
        getDriveGroupForCarmaAntNoSeq( "stow",
                                       carmaAntNoSeq,
                                       true,
                                       false,
                                       false );

    ++nextDriveSeqNo_;

    // issue the command out to the group of antennas
    {
        WorkResultSet wrs( "DriveHandle::stow result set" );

        queueFunctorWorkRequestGroup(
            "DriveHandle::stow()",
            makeHandleMethodFunctorGroup(
                driveGroup,
                &DriveHandle::stow ,
                position,
                nextDriveSeqNo_ ),
            wrs,
            *workerPool_ );
        waitForAllNormal(wrs);
    }

    const bool useAllAntennas = (carmaAntNoSeq.length() == 1) &&
                                (carmaAntNoSeq[ 0 ] == 0);

    const double now = Time::MJD();

    if ( useAllAntennas ) {
        carma::monitor::Stow & stowcmd = subarrayContainer_.commands().stow();

        stowcmd.timestamp().setValue( now );
        stowcmd.position().setValue( stowPosition );

    } else {
        // Set the monitor point value per antenna.
        AntMonPtGroup ampGroup
            = getAntMonPtGroupForCarmaAntNoSeq("stow", carmaAntNoSeq);

        AntMonPtGroup::const_iterator i = ampGroup.begin();
        const AntMonPtGroup::const_iterator iEnd = ampGroup.end();

        for ( ; i != iEnd; ++i ) {
            const ControlSubsystemBase::Antenna * const ant = *i;
            if ( ant == 0 )
                continue;
            ant->antCommands().stow().timestamp().setValue( now );
            ant->antCommands().stow().position().setValue( stowPosition );
        }
    }
    markStateChange();

    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::stow ends" );
} catch ( ... ) {
    rethrowCaughtAsUser( );
}


void
SubarrayControlImpl::stop( const SeqShort & carmaAntNoSeq )
try {
    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::stop starts" );

    cmdlog() << "stop(" << getStringForCarmaAntNoSeq( carmaAntNoSeq ) << ")";

    const DriveGroup driveGroup =
        getDriveGroupForCarmaAntNoSeq( "stop",
                                       carmaAntNoSeq,
                                       true,
                                       false,
                                       false );

    // issue the command out to the group of antennas
    {
        WorkResultSet wrs( "DriveHandle::stop result set" );

        queueFunctorWorkRequestGroup(
            "DriveHandle::stop()",
            makeHandleMethodFunctorGroup(
                driveGroup,
                &DriveHandle::stop ),
            wrs,
            *workerPool_ );
        waitForAllNormal(wrs);
    }

    const bool useAllAntennas = (carmaAntNoSeq.length() == 1) &&
                                (carmaAntNoSeq[ 0 ] == 0);

    const double now = Time::MJD();

    if ( useAllAntennas ) {
        carma::monitor::Stop & stopcmd = subarrayContainer_.commands().stop();

        stopcmd.timestamp().setValue( now );

    } else {
        // Set the monitor point value per antenna.

        AntMonPtGroup ampGroup
            = getAntMonPtGroupForCarmaAntNoSeq("stop", carmaAntNoSeq);

        AntMonPtGroup::const_iterator i = ampGroup.begin();
        const AntMonPtGroup::const_iterator iEnd = ampGroup.end();

        for ( ; i != iEnd; ++i ) {
            const ControlSubsystemBase::Antenna * const ant = *i;
            if ( ant == 0 )
                continue;
            ant->antCommands().stop().timestamp().setValue( now );
        }
    }
    markStateChange();

    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::stop ends" );
} catch ( ... ) {
    rethrowCaughtAsUser( );
}


void
SubarrayControlImpl::offset( const double     azArcmin,
                             const double     elArcmin,
                             const SeqShort & carmaAntNoSeq )
try {
    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::offset starts" );

    cmdlog() << "offset("
             << "azArcmin=" << azArcmin << ", "
             << "elArcmin=" << elArcmin << ", "
             << getStringForCarmaAntNoSeq( carmaAntNoSeq ) << ")";

    const bool skipUnownedAnts = (initializationFlag_ == false);

    const DriveGroup driveGroup =
        getDriveGroupForCarmaAntNoSeq( "offset",
                                       carmaAntNoSeq,
                                       true,
                                       false,
                                       skipUnownedAnts );

    ++nextDriveSeqNo_;

    // issue the command out to the group of antennas
    {
        string methodCallString;
        {
            ostringstream oss;
            oss << std::fixed
                << "DriveHandle::setOffset("
                << "azArcmin=" << azArcmin << ", elArcmin=" << elArcmin
                << ", seq=" << nextDriveSeqNo_ << ")";

            methodCallString = oss.str( );
        }

        WorkResultSet wrs( "DriveHandle::setOffset result set" );

        queueFunctorWorkRequestGroup(
            methodCallString,
            makeHandleMethodFunctorGroup(
                driveGroup,
                &DriveHandle::setOffset,
                azArcmin,
                elArcmin,
                nextDriveSeqNo_ ),
            wrs,
            *workerPool_ );
        waitForAllNormal(wrs);
    }

    // Set the monitor point values per antenna.
    {
        AntMonPtGroup ampGroup
            = getAntMonPtGroupForCarmaAntNoSeq("offset", carmaAntNoSeq);

        AntMonPtGroup::const_iterator i = ampGroup.begin();
        const AntMonPtGroup::const_iterator iEnd = ampGroup.end();

        for ( ; i != iEnd; ++i ) {
            const ControlSubsystemBase::Antenna * const ant = *i;
            if ( ant == 0 )
                continue;
            ant->azimuthOffset().setValue( azArcmin );
            ant->elevationOffset().setValue( elArcmin );
        }
        markStateChange();
    }

    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::offset ends" );
} catch ( ... ) {
    rethrowCaughtAsUser( );
}


void
SubarrayControlImpl::offsetAz( const double     azArcmin,
                               const SeqShort & carmaAntNoSeq )
try {
    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::offsetAz starts" );

    cmdlog() << "offsetAz("
             << "azArcmin=" << azArcmin << ", "
             << getStringForCarmaAntNoSeq( carmaAntNoSeq ) << ")";

    const bool skipUnownedAnts = (initializationFlag_ == false);

    const DriveGroup driveGroup =
        getDriveGroupForCarmaAntNoSeq( "offsetAz",
                                       carmaAntNoSeq,
                                       true,
                                       false,
                                       skipUnownedAnts );

    ++nextDriveSeqNo_;

    // issue the command out to the group of antennas
    {
        string methodCallString;
        {
            ostringstream oss;
            oss << std::fixed
                << "DriveHandle::setAzOffset(azArcmin=" << azArcmin
                << ", seq=" << nextDriveSeqNo_ << ")";
            methodCallString = oss.str( );
        }

        WorkResultSet wrs( "DriveHandle::setAzOffset result set" );

        queueFunctorWorkRequestGroup(
            methodCallString,
            makeHandleMethodFunctorGroup(
                driveGroup,
                &DriveHandle::setAzOffset,
                azArcmin,
                nextDriveSeqNo_ ),
            wrs,
            *workerPool_ );

        waitForAllNormal(wrs);
    }

    // Set the monitor point value per antenna.
    {
        AntMonPtGroup ampGroup
            = getAntMonPtGroupForCarmaAntNoSeq("offsetAz", carmaAntNoSeq);

        AntMonPtGroup::const_iterator i = ampGroup.begin();
        const AntMonPtGroup::const_iterator iEnd = ampGroup.end();

        for ( ; i != iEnd; ++i ) {
            const ControlSubsystemBase::Antenna * const ant = *i;
            if ( ant == 0 )
                continue;
            ant->azimuthOffset().setValue( azArcmin );
        }
    }


    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::offsetAz ends" );
} catch ( ... ) {
    rethrowCaughtAsUser( );
}


void
SubarrayControlImpl::offsetEl( const double     elArcmin,
                               const SeqShort & carmaAntNoSeq )
try {
    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::offsetEl starts" );

    cmdlog() << "offsetEl("
             << "elArcmin=" << elArcmin << ", "
             << getStringForCarmaAntNoSeq( carmaAntNoSeq ) << ")";

    const bool skipUnownedAnts = (initializationFlag_ == false);

    const DriveGroup driveGroup =
        getDriveGroupForCarmaAntNoSeq( "offsetEl",
                                       carmaAntNoSeq,
                                       true,
                                       false,
                                       skipUnownedAnts );

    ++nextDriveSeqNo_;

    // issue the command out to the group of antennas
    {
        string methodCallString;
        {
            ostringstream oss;
            oss << "DriveHandle::setElOffset(elArcmin=" << elArcmin
                << ", seq=" << nextDriveSeqNo_ << ")";
            methodCallString = oss.str( );
        }

        WorkResultSet wrs( "DriveHandle::setElOffset result set" );

        queueFunctorWorkRequestGroup(
            methodCallString,
            makeHandleMethodFunctorGroup(
                driveGroup,
                &DriveHandle::setElOffset,
                elArcmin,
                nextDriveSeqNo_ ),
            wrs,
            *workerPool_ );
        waitForAllNormal(wrs);
    }

    // Set the monitor point value per antenna.
    {
        AntMonPtGroup ampGroup
            = getAntMonPtGroupForCarmaAntNoSeq("offsetEl", carmaAntNoSeq);

        AntMonPtGroup::const_iterator i = ampGroup.begin();
        const AntMonPtGroup::const_iterator iEnd = ampGroup.end();

        for ( ; i != iEnd; ++i ) {
            const ControlSubsystemBase::Antenna * const ant = *i;
            if ( ant == 0 )
                continue;
            ant->elevationOffset().setValue( elArcmin );
        }
    }

    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::offsetEl ends" );
} catch ( ... ) {
    rethrowCaughtAsUser( );
}

/*
float
SubarrayControlImpl::getAzPositiveWrapLimit( unsigned short carmaAntNo )
{
    return getAntennaCommon( carmaAntNo, carmaMonitor_ ).drive().limit().azHighSwLimitVal().getValue();
}

float
SubarrayControlImpl::getAzNegativeWrapLimit( unsigned short carmaAntNo )
{
    return getAntennaCommon( carmaAntNo, carmaMonitor_ ).drive().limit().azLowSwLimitVal().getValue();
}

float
SubarrayControlImpl::getElevUpperLimit( unsigned short carmaAntNo )
{
    return getAntennaCommon( carmaAntNo, carmaMonitor_ ).drive().limit().azHighSwLimitVal().getValue();
}

float
SubarrayControlImpl::getElevLowerLimit( unsigned short carmaAntNo )
{
    return getAntennaCommon( carmaAntNo, carmaMonitor_ ).drive().limit().elLowSwLimitVal().getValue();
}
*/

void
SubarrayControlImpl::move( const double     azDegrees,
                           const double     elDegrees,
                           const SeqShort & carmaAntNoSeq )
try {
    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::move starts" );
    cmdlog() << "move("
             << "azDegrees=" << setprecision(4) << azDegrees << ", "
             << "elDegrees=" << setprecision(4) << elDegrees << ", "
             << getStringForCarmaAntNoSeq( carmaAntNoSeq ) << ")";

    const DriveGroup driveGroup =
        getDriveGroupForCarmaAntNoSeq( "move", carmaAntNoSeq, true, false, false );

    ++nextDriveSeqNo_;

    // issue the command out to the group of antennas
    {
        string methodCallString;
        {
            ostringstream oss;
            oss << std::fixed
                << "DriveHandle::setAzel("
                << "azDegrees=" << azDegrees << ", elDegrees=" << elDegrees
                << ")";
            methodCallString = oss.str( );
        }

        WorkResultSet wrs( "DriveHandle::setAzel result set" );

        queueFunctorWorkRequestGroup(
            methodCallString,
            makeHandleMethodFunctorGroup(
                driveGroup,
                &DriveHandle::setAzel,
                azDegrees,
                elDegrees,
                nextDriveSeqNo_ ),
            wrs,
            *workerPool_ );
        waitForAllNormal(wrs);
    }

    // don't update delays until all Drives are finished
    const double now = Time::MJD();
    renewDelays( driveGroup, now );

    const bool useAllAntennas = (carmaAntNoSeq.length() == 1) &&
                                (carmaAntNoSeq[ 0 ] == 0);
    if ( useAllAntennas ) {
        carma::monitor::Move & movecmd = subarrayContainer_.commands().move();

        movecmd.timestamp().setValue( now );
        movecmd.azimuth().setValue( azDegrees );
        movecmd.elevation().setValue( elDegrees );

    } else {
        // Set the command monitor point value per antenna.
        AntMonPtGroup ampGroup
            = getAntMonPtGroupForCarmaAntNoSeq("move", carmaAntNoSeq);

        AntMonPtGroup::const_iterator i = ampGroup.begin();
        const AntMonPtGroup::const_iterator iEnd = ampGroup.end();

        for ( ; i != iEnd; ++i ) {
            const ControlSubsystemBase::Antenna * const ant = *i;
            if ( ant == 0 )
                continue;
            carma::monitor::Move & command =
                           ant->antCommands().move();
            command.timestamp().setValue( now );
            command.azimuth().setValue( azDegrees );
            command.elevation().setValue( elDegrees );
        }
    }
    markStateChange();

    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::move ends" );
} catch ( ... ) {
    rethrowCaughtAsUser( );
}


void
SubarrayControlImpl::moveAz( const double     azDegrees,
                             const SeqShort & carmaAntNoSeq )
try {
    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::moveAz starts" );


    cmdlog() << "moveAz("
             << "azDegrees=" << setprecision(4) << azDegrees << ", "
             << getStringForCarmaAntNoSeq( carmaAntNoSeq ) << ")";

    const DriveGroup driveGroup =
        getDriveGroupForCarmaAntNoSeq( "moveAz", carmaAntNoSeq, true, false, false );

    ++nextDriveSeqNo_;

    // issue the command out to the group of antennas
    {
        string methodCallString;
        {
            ostringstream oss;
            oss << std::fixed
                << "DriveHandle::setAz(azDegrees=" << azDegrees
                << ", seq=" << nextDriveSeqNo_ << " )";
            methodCallString = oss.str( );
        }

        WorkResultSet wrs( "DriveHandle::setAz result set" );

        queueFunctorWorkRequestGroup(
            methodCallString,
            makeHandleMethodFunctorGroup(
                driveGroup,
                &DriveHandle::setAz,
                azDegrees,
                nextDriveSeqNo_ ),
            wrs,
            *workerPool_ );
        waitForAllNormal(wrs);
    }

    // don't update delays until all Drives are finished
    const double now = Time::MJD();
    renewDelays( driveGroup, now );

    const bool useAllAntennas = (carmaAntNoSeq.length() == 1) &&
                                (carmaAntNoSeq[ 0 ] == 0);

    if ( useAllAntennas ) {
        carma::monitor::Move & movecmd = subarrayContainer_.commands().move();

        movecmd.timestamp().setValue( now );
        movecmd.azimuth().setValue( azDegrees );
        // Ignore elevation assuming it's already at the prior stored value.
        // If it has never been set then client code should check for the error.

    } else {
        // Set the command monitor point value per antenna.
        // Note we use the Move container for this and hand it the
        // current elevation for that parameter.
        AntMonPtGroup ampGroup
            = getAntMonPtGroupForCarmaAntNoSeq("moveAz", carmaAntNoSeq);

        AntMonPtGroup::const_iterator i = ampGroup.begin();
        const AntMonPtGroup::const_iterator iEnd = ampGroup.end();

        carmaMonitor_.readNewestConditionalCopy();

        for ( ; i != iEnd; ++i ) {
            const ControlSubsystemBase::Antenna * const ant = *i;
            if ( ant == 0 )
                continue;
            carma::monitor::Move & command =
                           ant->antCommands().move();
            command.timestamp().setValue( now );
            command.azimuth().setValue( azDegrees );
            // Readback the current elevation from the
            // antenna::common::drive monitor system.
            const unsigned short antNo = ant->carmaAntennaNumber().getValue();
            //const unsigned short antIndex = antNo - 1;
            double curElev = getAntennaCommon( antNo, carmaMonitor_ )
                             .drive().track().actualAzimuth().getValue();
            command.elevation().setValue( curElev );
        }
    }
    markStateChange();

    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::moveAz ends" );
} catch ( ... ) {
    rethrowCaughtAsUser( );
}


void
SubarrayControlImpl::moveEl( const double     elDegrees,
                             const SeqShort & carmaAntNoSeq )
try {
    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::moveEl starts" );

    cmdlog() << "moveEl("
             << "elDegrees=" << setprecision(4) << elDegrees << ", "
             << getStringForCarmaAntNoSeq( carmaAntNoSeq ) << ")";

    const DriveGroup driveGroup =
        getDriveGroupForCarmaAntNoSeq( "moveEl", carmaAntNoSeq,
            true, false, false );

    ++nextDriveSeqNo_;

    // issue the command out to the group of antennas
    {
        string methodCallString;
        {
            ostringstream oss;
            oss << std::fixed
                << "DriveHandle::setEl(elDegrees=" << elDegrees
                << ", seq=" << nextDriveSeqNo_ << ")";
            methodCallString = oss.str( );
        }

        WorkResultSet wrs( "DriveHandle::setEl result set" );

        queueFunctorWorkRequestGroup(
            methodCallString,
            makeHandleMethodFunctorGroup(
                driveGroup,
                &DriveHandle::setEl,
                elDegrees,
                nextDriveSeqNo_ ),
            wrs,
            *workerPool_ );
        waitForAllNormal(wrs);
    }

    // don't update delays until all Drives are finished
    const double now = Time::MJD();
    renewDelays( driveGroup, now );

    const bool useAllAntennas = (carmaAntNoSeq.length() == 1) &&
                                (carmaAntNoSeq[ 0 ] == 0);

    if ( useAllAntennas ) {
        carma::monitor::Move & movecmd = subarrayContainer_.commands().move();

        movecmd.timestamp().setValue( now );
        movecmd.elevation().setValue( elDegrees );
        // Ignore azimuth assuming it's already at the prior stored value.
        // If it has never been set then client code should check for the error.

    } else {
        // Set the command monitor point value per antenna.
        // Note we use the Move container for this and hand it the
        // current azimuth for that parameter.
        AntMonPtGroup ampGroup
            = getAntMonPtGroupForCarmaAntNoSeq("moveEl", carmaAntNoSeq);

        AntMonPtGroup::const_iterator i = ampGroup.begin();
        const AntMonPtGroup::const_iterator iEnd = ampGroup.end();

        const double now = Time::MJD();
        carmaMonitor_.readNewestConditionalCopy();
        for ( ; i != iEnd; ++i ) {
            const ControlSubsystemBase::Antenna * const ant = *i;
            if ( ant == 0 ) continue;
            carma::monitor::Move& command =
                           ant->antCommands().move();
            command.timestamp().setValue( now );
            command.elevation().setValue( elDegrees );
            // Readback the current azimuth from the
            // antenna::common::drive monitor system.
            const unsigned short antNo = ant->carmaAntennaNumber().getValue();
            //const unsigned short antIndex = antNo - 1;
            double curAz = getAntennaCommon( antNo, carmaMonitor_ )
                             .drive().track().actualAzimuth().getValue();
            command.azimuth().setValue( curAz );
        }
    }
    markStateChange();

    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::moveEl ends" );
} catch ( ... ) {
    rethrowCaughtAsUser( );
}


void
SubarrayControlImpl::tiltZeros( const float        aftForward,
                                const float        leftRight,
                                const CORBA::Short carmaAntNo )
try {
    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::tiltZeros starts" );

    cmdlog() << "tiltZeros("
             << "aftForward=" << aftForward << ", "
             << "leftRight=" << leftRight << ", "
             << getStringForCarmaAntNo( carmaAntNo ) << ")";

    const DriveGroup driveGroup =
        getDriveGroupForCarmaAntNo( "tiltZeros", carmaAntNo );

    // issue the command out to the group of antennas
    {
        string methodCallString;
        {
            ostringstream oss;
            oss << std::fixed
                << "DriveControl::setTiltmeterZero("
                << "aftForward=" << aftForward << ", leftRight=" << leftRight
                << ")";
            methodCallString = oss.str( );
        }

        string paramString;
        {
            ostringstream oss;

            oss <<  "aftForward = " << aftForward
                << " leftRight = " << leftRight;

            paramString = oss.str( );
        }

        WorkResultSet wrs( "DriveControl::setTiltmeterZero result set" );

        queueFunctorWorkRequestGroup(
            methodCallString,
            makeRemoteObjMethodFunctorGroup(
                driveGroup,
                "setTiltmeterZero",
                paramString,
                &DriveControl::setTiltmeterZero,
                aftForward,
                leftRight ),
            wrs,
            *workerPool_ );
        waitForAllNormal(wrs);
    }

    { // Set control points

        ControlSubsystemBase::Antenna & antMp =
            getAntMonPtForCarmaAntNo( carmaAntNo );

        antMp.aftForwardTiltZero().setValue( aftForward );
        antMp.leftRightTiltZero().setValue( leftRight );
        markStateChange();
    }

    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::tiltZeros ends" );
} catch ( ... ) {
    rethrowCaughtAsUser( );
}


void
SubarrayControlImpl::aperturePointingConstants( const Aperture     ap,
                                                const float        az,
                                                const float        el,
                                                const float        sag,
                                                const CORBA::Short carmaAntNo )
try {
    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::aperturePointingConstants starts" );

    cmdlog() << "aperturePointingConstants("
             << "ap=" << ap << ", "
             << "az=" << az << ", "
             << "el=" << el << ", "
             << "sag=" << sag << ", "
             << getStringForCarmaAntNo( carmaAntNo ) << ")";

    const DriveControl::Aperture dcAp = convertApertureEnum( ap );

    const DriveGroup driveGroup =
        getDriveGroupForCarmaAntNo( "aperturePointingConstants", carmaAntNo );

    { // Set control points First in the event that an antenna is offline

        ControlSubsystemBase::Antenna & ant =
            getAntMonPtForCarmaAntNo( carmaAntNo );

        switch ( ap ) {
        case APERTURE_OPTICAL:
            ant.apertureOptical().pointingConstants().azOffset().setValue( az );
            ant.apertureOptical().pointingConstants().elOffset().setValue( el );
            ant.apertureOptical().pointingConstants().sag().setValue( sag );
            break;
        case APERTURE_RADIO1MM:
            ant.aperture1mm().pointingConstants().azOffset().setValue( az );
            ant.aperture1mm().pointingConstants().elOffset().setValue( el );
            ant.aperture1mm().pointingConstants().sag().setValue( sag );
            break;
        case APERTURE_RADIO3MM:
            ant.aperture3mm().pointingConstants().azOffset().setValue( az );
            ant.aperture3mm().pointingConstants().elOffset().setValue( el );
            ant.aperture3mm().pointingConstants().sag().setValue( sag );
            break;
        case APERTURE_RADIO1CM:
            ant.aperture1cm().pointingConstants().azOffset().setValue( az );
            ant.aperture1cm().pointingConstants().elOffset().setValue( el );
            ant.aperture1cm().pointingConstants().sag().setValue( sag );
            break;
        default:
            throw CARMA_EXCEPTION( util::UserException, "bad aperature value" );
        }

        markStateChange();
    }

    // issue the command out to the group of antennas
    {
        string methodCallString;
        {
            ostringstream oss;
            oss << std::fixed
                << "DriveControl::setAperturePointingConstants("
                << "ap=" << dcAp
                << ", az=" << az
                << ", el=" << el
                << ", sag=" << sag
                << ")";

            methodCallString = oss.str( );
        }

        string paramString;
        {
            ostringstream oss;

            oss <<  "ap = " << dcAp
                << " az = " << az
                << " el = " << el
                << " sag = " << sag;

            paramString = oss.str( );
        }

        WorkResultSet wrs( "DriveControl::setAperturePointingConstants result set" );

        queueFunctorWorkRequestGroup(
            methodCallString,
            makeRemoteObjMethodFunctorGroup(
                driveGroup,
                "setAperturePointingConstants",
                paramString,
                &DriveControl::setAperturePointingConstants,
                dcAp,
                az,
                el,
                sag ),
            wrs,
            *workerPool_ );
        waitForAllNormal(wrs);
    }

    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::aperturePointingConstants ends" );
} catch ( ... ) {
    rethrowCaughtAsUser( );
}


void
SubarrayControlImpl::azPointingConstant( const double       m1,
                                         const CORBA::Short carmaAntNo )
try {
    const string errorMsg( "SaCI::azPointingConstant unimplemented" );
    programLogError( errorMsg );
    throw CARMA_EXCEPTION( util::UserException, errorMsg.c_str() );
} catch ( ... ) {
    rethrowCaughtAsUser( );
}


void
SubarrayControlImpl::setOvroMountPointingConstants(
    const double       m1,
    const double       m2,
    const double       m3,
    const double       m4,
    const double       m5,
    const CORBA::Short carmaAntNo )
try {
    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::setOvroMountPointingConstants starts" );

    { // Set control points first in the event an antenna is offline
        ControlSubsystemBase::Ovro & antMp =
            getOvroMonPtForCarmaAntNo( carmaAntNo );

        antMp.azEncoderOffset().setValue( m1 );
        antMp.elEncoderOffset().setValue( m2 );
        antMp.axisNonOrthogonality().setValue( m3 );
        antMp.northSouthAzAxisVerticality().setValue( m4 );
        antMp.eastWestAzAxisVerticality().setValue( m5 );

        markStateChange();
    }

    cmdlog() << "setOvroMountPointingConstants("
             << "m1=" << m1 << ", "
             << "m2=" << m2 << ", "
             << "m3=" << m3 << ", "
             << "m4=" << m4 << ", "
             << "m5=" << m5 << ", "
             << getStringForCarmaAntNo( carmaAntNo ) << ")";

    DriveGroup ovroDriveGroup;
    {
        const DriveGroup driveGroup =
            getDriveGroupForCarmaAntNo( "setOvroMountPointingConstants",
                                        carmaAntNo );

        ovroDriveGroup =
            generateAntennaTypeSubset( driveGroup, ANTENNA_TYPE_OVRO );
    }

    // issue the command out to the group of antennas
    {
        string methodCallString;
        {
            ostringstream oss;

            oss << "DriveHandle::setOvroMountPointingConstants("
                << m1 << ", " << m2 << ", " << m3 << ", " << m4 << ", " << m5
                << ")";

            methodCallString = oss.str( );
        }

        WorkResultSet wrs( methodCallString + " result set" );

        queueFunctorWorkRequestGroup(
            methodCallString,
            makeHandleMethodFunctorGroup(
                ovroDriveGroup,
                &DriveHandle::setOvroMountPointingConstants,
                m1,
                m2,
                m3,
                m4,
                m5 ),
            wrs,
            *workerPool_ );
        waitForAllNormal(wrs);
    }

    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::setOvroMountPointingConstants ends" );
} catch ( ... ) {
    rethrowCaughtAsUser( );
}


void
SubarrayControlImpl::setBimaMountPointingConstants(
    const SeqDouble &  dazCoefSeq,
    const SeqDouble &  delCoefSeq,
    const CORBA::Short carmaAntNo )
try {
    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::setBimaMountPointingConstants starts" );

    string dazCoefSeqString;
    {
        ostringstream oss;

        oss << "< " << dazCoefSeq.length() << " elements >";

        dazCoefSeqString = oss.str();
    }

    string delCoefSeqString;
    {
        ostringstream oss;

        oss << "< " << delCoefSeq.length() << " elements >";

        delCoefSeqString = oss.str();
    }

    cmdlog() << "setBimaMountPointingConstants("
             << "dazCoefSeq=" << dazCoefSeqString << ", "
             << "delCoefSeq=" << delCoefSeqString << ", "
             << getStringForCarmaAntNo( carmaAntNo ) << ")";

    { // Set control points FIRST in case this antenna is offline.
        ControlSubsystemBase::Bima & antMp =
            getBimaMonPtForCarmaAntNo( carmaAntNo );

        if ( static_cast< int >( dazCoefSeq.length() ) > antMp.getNumApc( ) ||
             static_cast< int >( delCoefSeq.length() ) > antMp.getNumEpc( ) ) {

            throw CARMA_EXCEPTION( util::UserException, "Coefficient sequence "
                                   "length exceeds monitor container size." );
        }

        for ( unsigned i = 0; i < dazCoefSeq.length(); ++i )
            antMp.apc( i ).setValue( dazCoefSeq[i]  );

        for ( unsigned i = 0; i < delCoefSeq.length(); ++i )
            antMp.epc( i ).setValue( delCoefSeq[i] );

        markStateChange();
    }

    DriveGroup bimaDriveGroup;
    {
        const DriveGroup driveGroup =
            getDriveGroupForCarmaAntNo( "setBimaMountPointingConstants",
                                        carmaAntNo );

        bimaDriveGroup =
            generateAntennaTypeSubset( driveGroup, ANTENNA_TYPE_BIMA );
    }

    // issue the command out to the group of antennas
    {
        const vector< double > dazCoefs =
            convertSequenceToVector< double >( dazCoefSeq );

        const vector< double > delCoefs =
            convertSequenceToVector< double >( delCoefSeq );

        string methodCallString;
        {
            ostringstream oss;

            oss << "DriveHandle::setBimaMountPointingConstants("
                << "[ ??? ], [ ??? ]"
                << ")";

            methodCallString = oss.str( );
        }

        WorkResultSet wrs( methodCallString + " result set" );

        queueFunctorWorkRequestGroup(
            methodCallString,
            makeHandleMethodFunctorGroup(
                bimaDriveGroup,
                &DriveHandle::setBimaMountPointingConstants,
                dazCoefs,
                delCoefs ),
            wrs,
            *workerPool_ );
        waitForAllNormal(wrs);
    }

    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::setBimaMountPointingConstants ends" );
} catch ( ... ) {
    rethrowCaughtAsUser( );
}


/**.......................................................................
 * Set all SZA mount pointing constants
 */
void
SubarrayControlImpl::setSzaMountPointingConstants(CORBA::ULong  azEncoderCountsPerTurn,     CORBA::ULong  elEncoderCountsPerTurn,
						  CORBA::ULong  azMinEncoderCount,          CORBA::ULong  azMaxEncoderCount,
						  CORBA::ULong  elMinEncoderCount,          CORBA::ULong  elMaxEncoderCount,
						  CORBA::Double azEncoderZeroDegrees,       CORBA::Double elEncoderZeroDegrees,
						  CORBA::Double haTiltDegrees,              CORBA::Double latTiltDegrees,    CORBA::Double elTiltDegrees,
						  CORBA::Double opticalXCollimationDegrees, CORBA::Double opticalYCollimationDegrees,
						  CORBA::Double opticalFlexureSinDegrees,   CORBA::Double opticalFlexureCosDegrees,
						  CORBA::Double radioXCollimationDegrees,   CORBA::Double radioYCollimationDegrees,
						  CORBA::Double radioFlexureSinDegrees,     CORBA::Double radioFlexureCosDegrees,
						  CORBA::Short  carmaAntNo )
  try {
    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::setSzaMountPointingConstants starts" );

    { // Set control points first in the event an antenna is offline
      ControlSubsystemBase::Sza & antMp =
      getSzaMonPtForCarmaAntNo( carmaAntNo );

      antMp.azEncoderCountsPerTurn().setValue( azEncoderCountsPerTurn );
      antMp.elEncoderCountsPerTurn().setValue( elEncoderCountsPerTurn );
      antMp.azMinEncoderCount().setValue( azMinEncoderCount );
      antMp.azMaxEncoderCount().setValue( azMaxEncoderCount );
      antMp.elMinEncoderCount().setValue( elMinEncoderCount );
      antMp.elMaxEncoderCount().setValue( elMaxEncoderCount );
      antMp.azEncoderZero().setValue( azEncoderZeroDegrees );
      antMp.elEncoderZero().setValue( elEncoderZeroDegrees );
      antMp.opticalXCollimation().setValue( opticalXCollimationDegrees );
      antMp.opticalYCollimation().setValue( opticalYCollimationDegrees );
      antMp.opticalFlexureSin().setValue( opticalFlexureSinDegrees );
      antMp.opticalFlexureCos().setValue( opticalFlexureCosDegrees );
      antMp.radioXCollimation().setValue( radioXCollimationDegrees );
      antMp.radioYCollimation().setValue( radioYCollimationDegrees );
      antMp.radioFlexureSin().setValue( radioFlexureSinDegrees );
      antMp.radioFlexureCos().setValue( radioFlexureCosDegrees );
      antMp.haTilt().setValue( haTiltDegrees);
      antMp.latTilt().setValue( latTiltDegrees);
      antMp.elTilt().setValue( elTiltDegrees);

      markStateChange();
    }

    cmdlog() << "setSzaMountPointingConstants("
	     << "azEncoderCountsPerTurn     = " << azEncoderCountsPerTurn     << " elEncoderCountsPerTurn     = " << elEncoderCountsPerTurn
	     << "azMinEncoderCount          = " << azMinEncoderCount          << " azMaxEncoderCount          = " << azMaxEncoderCount
	     << "elMinEncoderCount          = " << elMinEncoderCount          << " elMaxEncoderCount          = " << elMaxEncoderCount
	     << "azEncoderZeroDegrees       = " << azEncoderZeroDegrees       << " elEncoderZeroDegrees       = " <<  elEncoderZeroDegrees
	     << "haTiltDegrees = " << haTiltDegrees  << " latTiltDegrees = " << latTiltDegrees << "elTiltDegrees = " << elTiltDegrees
	     << "opticalXCollimationDegrees = " << opticalXCollimationDegrees << " opticalYCollimationDegrees = " << opticalYCollimationDegrees
	     << "opticalFlexureSinDegrees   = " << opticalFlexureSinDegrees   << " opticalFlexureCosDegrees   = " << opticalFlexureCosDegrees
	     << "radioXCollimationDegrees   = " << radioXCollimationDegrees   << " radioYCollimationDegrees   = " << radioYCollimationDegrees
	     << "radioFlexureSinDegrees     = " << radioFlexureSinDegrees     << " radioFlexureCosDegrees     = " << radioFlexureCosDegrees
             << getStringForCarmaAntNo( carmaAntNo ) << ")";

    DriveGroup szaDriveGroup;
    {
        const DriveGroup driveGroup =
            getDriveGroupForCarmaAntNo( "setSzaMountPointingConstants",
                                        carmaAntNo );

        szaDriveGroup =
	  generateAntennaTypeSubset( driveGroup, ANTENNA_TYPE_SZA );
    }

    // issue the command out to the group of antennas
    {
      string methodCallString;
      {
	ostringstream oss;

            oss << "DriveHandle::setSzaMountPointingConstants("
		<< azEncoderCountsPerTurn     << ", " << elEncoderCountsPerTurn     << ", "
		<< azMinEncoderCount          << ", " << azMaxEncoderCount          << ", "
	        << elMinEncoderCount          << ", " << elMaxEncoderCount          << ", "
		<< azEncoderZeroDegrees       << ", " << elEncoderZeroDegrees       << ", "
		<< haTiltDegrees              << ", " << latTiltDegrees             << ", " << elTiltDegrees << ", "
		<< opticalXCollimationDegrees << ", " << opticalYCollimationDegrees << ", "
		<< opticalFlexureSinDegrees   << ", " << opticalFlexureCosDegrees   << ", "
		<< radioXCollimationDegrees   << ", " << radioYCollimationDegrees   << ", "
		<< radioFlexureSinDegrees     << ", " << radioFlexureCosDegrees
                << ")";

            methodCallString = oss.str( );
        }

        WorkResultSet wrs( methodCallString + " result set" );

        queueFunctorWorkRequestGroup(
            methodCallString,
            makeHandleMethodFunctorGroup(
					 szaDriveGroup,
					 &DriveHandle::setSzaMountPointingConstants,
					 static_cast<double>( azEncoderCountsPerTurn ),
					 static_cast<double>( elEncoderCountsPerTurn ),
					 static_cast<double>( azMinEncoderCount ),
					 static_cast<double>( azMaxEncoderCount ),
					 static_cast<double>( elMinEncoderCount ),
					 static_cast<double>( elMaxEncoderCount ),
					 static_cast<double>( azEncoderZeroDegrees ),
					 static_cast<double>( elEncoderZeroDegrees ),
					 static_cast<double>( haTiltDegrees ),
					 static_cast<double>( latTiltDegrees ),
					 static_cast<double>( elTiltDegrees ),
					 static_cast<double>( opticalXCollimationDegrees ),
					 static_cast<double>( opticalYCollimationDegrees ),
					 static_cast<double>( opticalFlexureSinDegrees ),
					 static_cast<double>( opticalFlexureCosDegrees ),
					 static_cast<double>( radioXCollimationDegrees ),
					 static_cast<double>( radioYCollimationDegrees ),
					 static_cast<double>( radioFlexureSinDegrees ),
					 static_cast<double>( radioFlexureCosDegrees ) ),
            wrs,
            *workerPool_ );
        waitForAllNormal(wrs);
    }

    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::setSzaMountPointingConstants ends" );
} catch ( ... ) {
    rethrowCaughtAsUser( );
}

/**.......................................................................
 * Set SZA encoder limits
 */
void SubarrayControlImpl::setSzaEncoderLimits(CORBA::ULong  azMinEncoderCount,          CORBA::ULong  azMaxEncoderCount,
					      CORBA::ULong  elMinEncoderCount,          CORBA::ULong  elMaxEncoderCount,
					      CORBA::Short  carmaAntNo )
  try {

    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::setSzaEncoderLimits starts" );
    
    { // Set control points first in the event an antenna is offline
      ControlSubsystemBase::Sza & antMp =
	getSzaMonPtForCarmaAntNo( carmaAntNo );
      
      antMp.azMinEncoderCount().setValue( azMinEncoderCount );
      antMp.azMaxEncoderCount().setValue( azMaxEncoderCount );
      antMp.elMinEncoderCount().setValue( elMinEncoderCount );
      antMp.elMaxEncoderCount().setValue( elMaxEncoderCount );
      
      markStateChange();
    }
    
    cmdlog() << "setSzaEncoderLimits("
	     << "azMinEncoderCount          = " << azMinEncoderCount          << " azMaxEncoderCount          = " << azMaxEncoderCount
	     << "elMinEncoderCount          = " << elMinEncoderCount          << " elMaxEncoderCount          = " << elMaxEncoderCount
             << getStringForCarmaAntNo( carmaAntNo ) << ")";
    
    DriveGroup szaDriveGroup;
    {
      const DriveGroup driveGroup =
	getDriveGroupForCarmaAntNo( "setSzaEncoderLimits",
				    carmaAntNo );
      
      szaDriveGroup =
	generateAntennaTypeSubset( driveGroup, ANTENNA_TYPE_SZA );
    }
    
    // issue the command out to the group of antennas
    {
      string methodCallString;
      {
	ostringstream oss;
	
	oss << "DriveHandle::setSzaEncoderLimits("
	    << azMinEncoderCount      << ", " <<  azMaxEncoderCount      << ", "
	    << elMinEncoderCount      << ", " <<  elMaxEncoderCount
	    << ")";
	
	methodCallString = oss.str( );
      }
      
      WorkResultSet wrs( methodCallString + " result set" );
      
      queueFunctorWorkRequestGroup(
				   methodCallString,
				   makeHandleMethodFunctorGroup(
								szaDriveGroup,
								&DriveHandle::setSzaEncoderLimits,
								static_cast<double>( azMinEncoderCount ),
								static_cast<double>( azMaxEncoderCount ),
								static_cast<double>( elMinEncoderCount ),
								static_cast<double>( elMaxEncoderCount ) ),
				   wrs,
				   *workerPool_ );
      waitForAllNormal(wrs);
    }
    
    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::setSzaEncoderLimits ends" );
  } catch ( ... ) {
    rethrowCaughtAsUser( );
  }

/**.......................................................................
 * Set SZA encoder zeros
 */
void SubarrayControlImpl::setSzaEncoderZeros(CORBA::Double azEncoderZeroDegrees, CORBA::Double elEncoderZeroDegrees,
					     CORBA::Short  carmaAntNo )
  try {

    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::setSzaEncoderZeros starts" );
    
    { // Set control points first in the event an antenna is offline
      ControlSubsystemBase::Sza & antMp =
	getSzaMonPtForCarmaAntNo( carmaAntNo );
      
      antMp.azEncoderZero().setValue( azEncoderZeroDegrees );
      antMp.elEncoderZero().setValue( elEncoderZeroDegrees );
      
      markStateChange();
    }
    
    cmdlog() << "setSzaEncoderZeros("
	     << "azEncoderZeroDegrees = " << azEncoderZeroDegrees << " elEncoderZeroDegrees = " << elEncoderZeroDegrees
             << getStringForCarmaAntNo( carmaAntNo ) << ")";
    
    DriveGroup szaDriveGroup;
    {
      const DriveGroup driveGroup =
	getDriveGroupForCarmaAntNo( "setSzaEncoderZeros",
				    carmaAntNo );
      
      szaDriveGroup =
	generateAntennaTypeSubset( driveGroup, ANTENNA_TYPE_SZA );
    }
    
    // issue the command out to the group of antennas
    {
      string methodCallString;
      {
	ostringstream oss;
	oss << "DriveHandle::setSzaEncoderZeros(" << azEncoderZeroDegrees << ", " <<  elEncoderZeroDegrees << ")";
	methodCallString = oss.str( );
      }
      
      WorkResultSet wrs( methodCallString + " result set" );
      
      queueFunctorWorkRequestGroup(
				   methodCallString,
				   makeHandleMethodFunctorGroup(
								szaDriveGroup,
								&DriveHandle::setSzaEncoderZeros,
								static_cast<double>( azEncoderZeroDegrees ),
								static_cast<double>( elEncoderZeroDegrees ) ),
				   wrs,
				   *workerPool_ );
      waitForAllNormal(wrs);
    }
    
    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::setSzaEncoderZeros ends" );
  } catch ( ... ) {
    rethrowCaughtAsUser( );
  }

/**.......................................................................
 * Set SZA tilts
 */
void SubarrayControlImpl::setSzaTilts(CORBA::Double haTiltDegrees, CORBA::Double latTiltDegrees, CORBA::Double elTiltDegrees,
				      CORBA::Short  carmaAntNo )
  try {

    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::setSzaTilts starts" );
    
    { // Set control points first in the event an antenna is offline
      ControlSubsystemBase::Sza & antMp =
	getSzaMonPtForCarmaAntNo( carmaAntNo );
      
      antMp.haTilt().setValue(  haTiltDegrees);
      antMp.latTilt().setValue( latTiltDegrees);
      antMp.elTilt().setValue(  elTiltDegrees);
      
      markStateChange();
    }
    
    cmdlog() << "setSzaTilts("
	     << "haTiltDegrees  = " << haTiltDegrees  << ", "
	     << "latTiltDegrees = " << latTiltDegrees << ", "
	     << "elTiltDegrees  = " << elTiltDegrees  << ","
             << getStringForCarmaAntNo( carmaAntNo )  << ")";
    
    DriveGroup szaDriveGroup;
    {
      const DriveGroup driveGroup =
	getDriveGroupForCarmaAntNo( "setSzaTilts",
				    carmaAntNo );
      
      szaDriveGroup =
	generateAntennaTypeSubset( driveGroup, ANTENNA_TYPE_SZA );
    }
    
    // issue the command out to the group of antennas
    {
      string methodCallString;
      {
	ostringstream oss;
	oss << "DriveHandle::setSzaTilts("
	    << haTiltDegrees << ", " <<  latTiltDegrees << ", " << elTiltDegrees << ")";
	methodCallString = oss.str( );
      }
      
      WorkResultSet wrs( methodCallString + " result set" );
      
      queueFunctorWorkRequestGroup(
				   methodCallString,
				   makeHandleMethodFunctorGroup(
								szaDriveGroup,
								&DriveHandle::setSzaTilts,
								static_cast<double>( haTiltDegrees ),
								static_cast<double>( latTiltDegrees ),
								static_cast<double>( elTiltDegrees ) ),
				   wrs,
				   *workerPool_ );
      waitForAllNormal(wrs);
    }
    
    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::setSzaEncoderLimits ends" );
  } catch ( ... ) {
    rethrowCaughtAsUser( );
  }

void
SubarrayControlImpl::trackThreshold(float threshold,
                                    const SeqShort& carmaAntNoSeq )
try {
    const string cmdName = "trackThreshold";

    string paramString;
    {
        ostringstream oss;
        oss << std::fixed
            << "threshold=" << setprecision( 2 ) << threshold << ", "
            << getStringForCarmaAntNoSeq( carmaAntNoSeq );
        paramString = oss.str( );
    }

    cmdlog() << cmdName << "(" << paramString << ")";

    // issue the command out to the group of antennas
    {
        const DriveGroup driveGroup =
            getDriveGroupForCarmaAntNoSeq( cmdName,
                                           carmaAntNoSeq,
                                           true,
                                           false,
                                           false );


        ostringstream oss;
        oss << "DriveControl::" << cmdName << "(" << paramString << ")";
        string methodCallString = oss.str( );

        const double lofreq = subarrayContainer_.loFreq().getValue();

        WorkResultSet wrs( methodCallString + "result set" );

        // issue the command out to the group of drives
        queueFunctorWorkRequestGroup(
            methodCallString,
            makeHandleMethodFunctorGroup(
                driveGroup,
                &DriveHandle::updateTrackTolerance,
                threshold,
                lofreq ),
            wrs,
            *workerPool_ );
        waitForAllNormal(wrs);
    }

    // Set the command monitor point value per antenna.
    {

        const bool useAllAntennas = (carmaAntNoSeq.length() == 1) &&
                                    (carmaAntNoSeq[ 0 ] == 0);

        if ( useAllAntennas ) // Set MP for subarray container variant.
            subarrayContainer_.trackTolerance().setValue( threshold );

        const AntMonPtGroup ampGroup =
            getAntMonPtGroupForCarmaAntNoSeq( "trackThreshold",
                                              carmaAntNoSeq );

        AntMonPtGroup::const_iterator i = ampGroup.begin();
        const AntMonPtGroup::const_iterator iEnd = ampGroup.end();

        for ( ; i != iEnd; ++i ) {
            const ControlSubsystemBase::Antenna * const ant = *i;
            if ( ant == 0 ) continue;
            ant->trackTolerance().setValue( threshold );
        }
    }
    markStateChange();

} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
SubarrayControlImpl::snowTrack( const SeqShort & carmaAntNoSeq )
try {
    const string cmdName( "snowTrack" );

    const string paramString( getStringForCarmaAntNoSeq( carmaAntNoSeq ) );

    cmdlog() << cmdName << "(" << paramString << ")";

    // issue the command out to the group of antennas
    {
        const DriveGroup driveGroup =
            getDriveGroupForCarmaAntNoSeq( cmdName,
                                           carmaAntNoSeq,
                                           true,
                                           false,
                                           false );

        string methodCallString;
        {
            ostringstream oss;

            oss << "DriveControl::" << cmdName << "(" << paramString << ")";

            methodCallString = oss.str( );
        }

        WorkResultSet wrs( methodCallString + "result set" );

        // issue the command out to the group of drives
        queueFunctorWorkRequestGroup(
            methodCallString,
            makeRemoteObjMethodFunctorGroup(
                driveGroup,
                cmdName,
                paramString,
                &DriveControl::trackSnow ),
            wrs,
            *workerPool_ );
    }

    // Set the command monitor point
    const bool useAllAntennas = (carmaAntNoSeq.length() == 1) &&
                                (carmaAntNoSeq[ 0 ] == 0);

    if ( useAllAntennas ) {
        ControlSubsystemBase::SnowTrack & snowcmd =
            subarrayContainer_.commands().snowTrack();

        snowcmd.timestamp().setValue( Time::MJD( ) );
        markStateChange();
    }

} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
SubarrayControlImpl::windTrack( const SeqShort & carmaAntNoSeq )
try {
    const string cmdName( "windTrack" );

    const string paramString( getStringForCarmaAntNoSeq( carmaAntNoSeq ) );

    cmdlog() << cmdName << "(" << paramString << ")";

    // issue the command out to the group of antennas
    {
        const DriveGroup driveGroup =
            getDriveGroupForCarmaAntNoSeq( cmdName,
                                           carmaAntNoSeq,
                                           true,
                                           false,
                                           false );

        string methodCallString;
        {
            ostringstream oss;

            oss << "DriveControl::" << cmdName << "(" << paramString << ")";

            methodCallString = oss.str( );
        }

        WorkResultSet wrs( methodCallString + "result set" );

        // issue the command out to the group of drives
        queueFunctorWorkRequestGroup(
            methodCallString,
            makeRemoteObjMethodFunctorGroup(
                driveGroup,
                cmdName,
                paramString,
                &DriveControl::trackWind ),
            wrs,
            *workerPool_ );
    }

    // Set the command monitor point
    const bool useAllAntennas = (carmaAntNoSeq.length() == 1) &&
                                (carmaAntNoSeq[ 0 ] == 0);

    if ( useAllAntennas ) {
        ControlSubsystemBase::WindTrack & windcmd =
            subarrayContainer_.commands().windTrack();

        windcmd.timestamp().setValue( Time::MJD( ) );
        markStateChange();
    }

} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
SubarrayControlImpl::testAntHandles( const CORBA::Double aSeconds,
                                     const CORBA::Double bSeconds,
                                     const CORBA::Long   whichTest,
                                     const CORBA::Double lateAfterSeconds,
                                     const SeqShort &    carmaAntNoSeq )
try {
    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::testAntHandles starts" );

    cmdlog() << "testAntHandles("
             << "aSeconds=" << aSeconds << ", "
             << "bSeconds=" << bSeconds << ", "
             << "whichTest=" << whichTest << ", "
             << "lateAfterSeconds=" << lateAfterSeconds << ", "
             << getStringForCarmaAntNoSeq( carmaAntNoSeq ) << ")";

    const unsigned long lateAfterMillis =
        static_cast< unsigned long >( floor( lateAfterSeconds * 1000.0 ) );

    const DriveGroup driveGroup =
        getDriveGroupForCarmaAntNoSeq( "testAntHandles",
                                       carmaAntNoSeq,
                                       true,
                                       false,
                                       false );

    // issue the command out to the group of antennas
    {
        WorkResultSet wrs( "DriveHandle::test result set" );

        queueFunctorWorkRequestGroup(
            "DriveHandle::test()",
            makeHandleMethodFunctorGroup(
                driveGroup,
                &DriveHandle::test,
                static_cast< double >( aSeconds ),
                static_cast< double >( bSeconds ),
                static_cast< long >( whichTest ) ),
            wrs,
            *workerPool_ );

        waitForAllNormal( wrs, lateAfterMillis );
    }

    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::testAntHandles ends" );
} catch ( ... ) {
    rethrowCaughtAsUser( );
}

void
SubarrayControlImpl::setSafeRange( const CORBA::Float azLow,
                                   const CORBA::Float azHigh,
                                   const CORBA::Float elLow,
                                   const CORBA::Float elHigh,
                                   const SeqShort &   carmaAntNoSeq )
try {

    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::setSafeRange starts" );
    cmdlog() << "setSafeRange( azLow=" << azLow << ", azHigh=" << azHigh
	     << ", elLow=" << elLow << ", elHigh=" << elHigh
	     << "," << getStringForCarmaAntNoSeq( carmaAntNoSeq )
	     << ")";

    // skip antennas not owned by this subarray.
    const DriveGroup driveGroup =
        getDriveGroupForCarmaAntNoSeq( "setSafeRange",
                                       carmaAntNoSeq,
                                       true,
                                       true,
                                       true );

    // issue the command out to the group of antennas
    {
        WorkResultSet wrs( "DriveHandle::setSafeRange result set" );

        queueFunctorWorkRequestGroup(
            "DriveHandle::setSafeRange()",
            makeHandleMethodFunctorGroup(
                driveGroup,
                &DriveHandle::setSafeRange,
                azLow,
                azHigh,
                elLow,
                elHigh
                ),
            wrs,
            *workerPool_ );
        waitForAllNormal(wrs);
    }

    { // Set monitor system control points
        AntMonPtGroup ampGroup
            = getAntMonPtGroupForCarmaAntNoSeq("setSafeRange", carmaAntNoSeq);

        AntMonPtGroup::const_iterator i = ampGroup.begin();
        const AntMonPtGroup::const_iterator iEnd = ampGroup.end();

        for ( ; i != iEnd; ++i ) {
            const ControlSubsystemBase::Antenna * const ant = *i;
            if ( ant == 0 )
                continue;
            ant->azSafeLow().setValue( azLow );
            ant->azSafeHigh().setValue( azHigh );
            ant->elSafeLow().setValue( elLow );
            ant->elSafeHigh().setValue( elHigh );
        }
        markStateChange();
    }

    CARMA_CPTRACE( util::Trace::TRACE7, "SaCI::setSafeRange ends" );
} catch ( ... ) {
    rethrowCaughtAsUser( );
}
