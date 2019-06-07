/**
 *
 * Carma control interface server implementation for
 * debugging routines.
 *
 * @author: Steve Scott
 *
 * $Id: SubarrayControlDebug.cc,v 1.29 2013/04/12 20:54:44 mpound Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <sstream>

#include "carma/control/AntennaControls.h"
#include "carma/control/FunctorWorkRequest.h"
#include "carma/control/HandleMethodFunctorGroup.h"
#include "carma/control/LoberotatorHandle.h"
#include "carma/control/RemoteObjMethodFunctorGroup.h"
#include "carma/control/SubarrayControlImpl.h"
#include "carma/control/WorkerPool.h"
#include "carma/loberotator/LoberotatorControl.h"
#include "carma/interferometry/DelayEngine.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/WorkResult.h"

using namespace ::std;
using namespace carma;
using namespace carma::control;
using namespace carma::interferometry;
using namespace carma::loberotator;
using namespace carma::monitor;
using namespace carma::util;


void SubarrayControlImpl::fringeTracking(bool on)
try
{
    const CORBA::Long channels = 0; // Do all channels

    cmdlog() << "fringeTracking(" << on << ")";

    WorkResultSet wrs( "LoberotatorControl::enableFringeTracking result set" );

    queueFunctorWorkRequestGroup(
        "LoberotatorControl::enableFringeTracking()",
        makeRemoteObjMethodFunctorGroup(
            getLoberotatorGroup(),
            "enableFringeTracking",
            "",
            &LoberotatorControl::enableFringeTracking,
            channels,
            on ),
        wrs,
        *workerPool_ );

    waitForAllNormal( wrs );
} catch ( ... ) {
    rethrowCaughtAsUser();
}


void SubarrayControlImpl::phaseSwitching(const bool on, const short chanNo)
try
{
    cmdlog() << "phaseSwitching(state=" << on << ", chanNo=" << chanNo << ")";

    CORBA::Long chan = chanNo; // Change to correct type

    WorkResultSet wrs( "LoberotatorControl::enablePhaseSwitching result set" );

    queueFunctorWorkRequestGroup(
        "LoberotatorControl::enablePhaseSwitching()",
        makeRemoteObjMethodFunctorGroup(
            getLoberotatorGroup(),
            "enablePhaseSwitching",
            "",
            &LoberotatorControl::enablePhaseSwitching,
            chan,
            on ),
        wrs,
        *workerPool_ );

    waitForAllNormal( wrs );
} catch ( ... ) {
    rethrowCaughtAsUser();
}


void SubarrayControlImpl::lrPhaseOffsetMode(const bool on)
try
{
    const CORBA::Long channels = 0; // Do all channels

    cmdlog() << "lrPhaseOffsetMode(" << on << ")";

    WorkResultSet wrs( "LoberotatorControl::setOffsetControl result set" );

    queueFunctorWorkRequestGroup(
        "LoberotatorControl::setOffsetControl()",
        makeRemoteObjMethodFunctorGroup(
            getLoberotatorGroup(),
            "setOffsetControl",
            "",
            &LoberotatorControl::setOffsetControl,
            channels,
            on ),
        wrs,
        *workerPool_ );

    waitForAllNormal( wrs );
} catch ( ... ) {
    rethrowCaughtAsUser();
}

// FIXME: antenna# assumed to be channel#
void SubarrayControlImpl::lrPhase( const float        phaseOffset,
                                   const CORBA::Short carmaAntNo )
try
{
    // no logging
    WorkResultSet wrs( "LoberotatorControl::setOffsetPhase result set" );

    queueFunctorWorkRequestGroup(
        "LoberotatorControl::setOffsetPhase()",
        makeRemoteObjMethodFunctorGroup(
            getLoberotatorGroup(),
            "setOffsetPhase",
            "",
            &LoberotatorControl::setOffsetPhase,
            static_cast< CORBA::Long >( carmaAntNo ),
            static_cast< CORBA::Double >( phaseOffset ) ),
        wrs,
        *workerPool_ );

    waitForAllNormal( wrs );
} catch ( ... ) {
    rethrowCaughtAsUser();
}

// FIXME: antenna# assumed to be channel#
void SubarrayControlImpl::lrRate( const float        phaseRate,
                                  const CORBA::Short carmaAntNo )
try
{
    // no logging
    WorkResultSet wrs( "LoberotatorControl::setOffsetRate result set" );

    queueFunctorWorkRequestGroup(
        "LoberotatorControl::setOffsetRate()",
        makeRemoteObjMethodFunctorGroup(
            getLoberotatorGroup(),
            "setOffsetRate",
            "",
            &LoberotatorControl::setOffsetRate,
            static_cast< CORBA::Long >( carmaAntNo ),
            static_cast< CORBA::Double >( phaseRate ) ),
        wrs,
        *workerPool_ );

    waitForAllNormal( wrs );
} catch ( ... ) {
    rethrowCaughtAsUser();
}


void
SubarrayControlImpl::useAdjustableDelay( const bool         useIt ,
                                         const SeqShort &   carmaAntNoSeq)
try {
    const AntControlsGroup antControlsGroup =
        getAntControlsGroupForCarmaAntNoSeq(
		"useAdjustableDelay", carmaAntNoSeq, true, true, true );

    AntControlsGroup::const_iterator i = antControlsGroup.begin();
    const AntControlsGroup::const_iterator iEnd = antControlsGroup.end();
    for ( ; i != iEnd; ++i ) {
	AntennaControls * const antControls = *i;

	if ( antControls == 0 ) {
	    programLogErrorIfPossible(
		"CARMA antenna number mapped to a NULL AntennaControls" );
	}
	unsigned short carmaAntNo = antControls->getCarmaAntennaNo();
	delayEngine_->useAdjustableDelay(carmaAntNo,useIt);
    }

    // recompute & broadcast delays if subarray is already initialized
    if ( initializationFlag_ ) {
        const DelayFrameVec dfv = delayEngine_->computeDelays();
        broadcastDelayData( dfv );
    }
} catch ( ... ) {
    rethrowCaughtAsUser();
}


void
SubarrayControlImpl::useGeometricDelay( const bool         useIt ,
                                        const SeqShort &   carmaAntNoSeq)
try {
    const AntControlsGroup antControlsGroup =
        getAntControlsGroupForCarmaAntNoSeq(
		"useGeometricDelay", carmaAntNoSeq, true, true, true );

    AntControlsGroup::const_iterator i = antControlsGroup.begin();
    const AntControlsGroup::const_iterator iEnd = antControlsGroup.end();
    for ( ; i != iEnd; ++i ) {
	AntennaControls * const antControls = *i;

	if ( antControls == 0 ) {
	    programLogErrorIfPossible(
		"CARMA antenna number mapped to a NULL AntennaControls" );
	}
	unsigned short carmaAntNo = antControls->getCarmaAntennaNo();
        delayEngine_->useGeometricDelay(carmaAntNo,useIt);
    }

    // recompute & broadcast delays if subarray is already initialized
    if ( initializationFlag_ ) {
        const DelayFrameVec dfv = delayEngine_->computeDelays();
        broadcastDelayData( dfv );
    }
} catch ( ... ) {
    rethrowCaughtAsUser();
}


void
SubarrayControlImpl::useHeightDelay( const bool         useIt ,
                                     const SeqShort &   carmaAntNoSeq)
try {
    const AntControlsGroup antControlsGroup =
            getAntControlsGroupForCarmaAntNoSeq(
		    "useHeightDelay", carmaAntNoSeq, true, true, true );
    AntControlsGroup::const_iterator i = antControlsGroup.begin();
    const AntControlsGroup::const_iterator iEnd = antControlsGroup.end();
    for ( ; i != iEnd; ++i ) {
	AntennaControls * const antControls = *i;

	if ( antControls == 0 ) {
	    programLogErrorIfPossible(
		"CARMA antenna number mapped to a NULL AntennaControls" );
	}
	unsigned short carmaAntNo = antControls->getCarmaAntennaNo();
	delayEngine_->useHeightDelay(carmaAntNo,useIt) ;
    }

    // recompute & broadcast delays if subarray is already initialized
    if ( initializationFlag_ ) {
        const DelayFrameVec dfv = delayEngine_->computeDelays();
        broadcastDelayData( dfv );
    }
} catch ( ... ) {
    rethrowCaughtAsUser();
}


void
SubarrayControlImpl::useIonosphericDelay( const bool         useIt ,
                                          const SeqShort &   carmaAntNoSeq)
try {
    const AntControlsGroup antControlsGroup =
            getAntControlsGroupForCarmaAntNoSeq(
		"useIonosphericDelay", carmaAntNoSeq, true, true, true );

    AntControlsGroup::const_iterator i = antControlsGroup.begin();
    const AntControlsGroup::const_iterator iEnd = antControlsGroup.end();
    for ( ; i != iEnd; ++i ) {
	AntennaControls * const antControls = *i;

	if ( antControls == 0 ) {
	    programLogErrorIfPossible(
		"CARMA antenna number mapped to a NULL AntennaControls" );
	}
	unsigned short carmaAntNo = antControls->getCarmaAntennaNo();
	delayEngine_->useIonosphericDelay(carmaAntNo,useIt);
    }

    // recompute & broadcast delays if subarray is already initialized
    if ( initializationFlag_ ) {
        const DelayFrameVec dfv = delayEngine_->computeDelays();
        broadcastDelayData( dfv );
    }
} catch ( ... ) {
    rethrowCaughtAsUser();
}


void
SubarrayControlImpl::useTroposphericDelay( const bool         useIt ,
                                           const SeqShort &   carmaAntNoSeq)
try {
    const AntControlsGroup antControlsGroup =
            getAntControlsGroupForCarmaAntNoSeq(
		    "useTroposphericDelay", carmaAntNoSeq, true, true, true );
    AntControlsGroup::const_iterator i = antControlsGroup.begin();
    const AntControlsGroup::const_iterator iEnd = antControlsGroup.end();
    for ( ; i != iEnd; ++i ) {
	AntennaControls * const antControls = *i;

	if ( antControls == 0 ) {
	    programLogErrorIfPossible(
		"CARMA antenna number mapped to a NULL AntennaControls" );
	}
	unsigned short carmaAntNo = antControls->getCarmaAntennaNo();
	delayEngine_->useTroposphericDelay(carmaAntNo,useIt);
    }

    // recompute & broadcast delays if subarray is already initialized
    if ( initializationFlag_ ) {
        const DelayFrameVec dfv = delayEngine_->computeDelays();
        broadcastDelayData( dfv );
    }
} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
SubarrayControlImpl::useThermalDelay( const bool         useIt ,
                                      const SeqShort &   carmaAntNoSeq)
try {
    const AntControlsGroup antControlsGroup =
            getAntControlsGroupForCarmaAntNoSeq(
		    "useThermalDelay", carmaAntNoSeq, true, true, true );
    AntControlsGroup::const_iterator i = antControlsGroup.begin();
    const AntControlsGroup::const_iterator iEnd = antControlsGroup.end();
    for ( ; i != iEnd; ++i ) {
	AntennaControls * const antControls = *i;

	if ( antControls == 0 ) {
	    programLogErrorIfPossible(
		"CARMA antenna number mapped to a NULL AntennaControls" );
	}
	unsigned short carmaAntNo = antControls->getCarmaAntennaNo();
	delayEngine_->useThermalDelay(carmaAntNo,useIt) ;
    }

    // recompute & broadcast delays if subarray is already initialized
    if ( initializationFlag_ ) {
        const DelayFrameVec dfv = delayEngine_->computeDelays();
        broadcastDelayData( dfv );
    }
} catch ( ... ) {
    rethrowCaughtAsUser();
}
