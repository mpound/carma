/**
 *
 * Carma control interface server implementation for the integrate procedure,
 * and control of the pipeline.
 *
 * @author: Steve Scott
 *
 * $Id: SubarrayControlInteg.cc,v 1.62 2014/06/04 17:09:16 mpound Exp $
 *
 * $CarmaCopyright$
 *
 */


#include <sstream>

#include "carma/control/SubarrayControlImpl.h"

#include "carma/corba/corba.h"
#include "carma/control/PipelineHandle.h"
#include "carma/control/FunctorWorkRequest.h"
#include "carma/control/RemoteObjMethodFunctorGroup.h"
#include "carma/control/HandleMethodFunctorGroup.h"
#include "carma/util/CorrelatorSet.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/WorkResult.h"


using namespace ::std;
using namespace carma;
using namespace carma::control;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::pipeline;
using namespace log4cpp;


SubarrayControlImpl::PipelineGroup
SubarrayControlImpl::getPipelineGroup( ) {
    PipelineGroup result;

    // Pipeline group can contain more than one pipeline, now
    // that subarrays can control more than one correlator.  Need to
    // check and return group appropriate for the correlators we are
    // controlling.

  carma::util::CorrelatorType corr_ = getCorrelatorDesignation();
  CorrelatorSet corrset(corr_);

  if (corrset.includesSpectral()) {
    PipelineHandle * const php = slPipeline_.get( );
    if(php != 0) result.insert(php);
  }
  if (corrset.includesWideband()) {
    PipelineHandle * const php = wbPipeline_.get( );
    if(php != 0) result.insert(php);
  }
  if (corrset.includesC3gMax8()) {
    PipelineHandle* php = c3gMax8Pipeline_.get( );
    if(php != 0) result.insert(php);

  }
  if (corrset.includesC3gMax23()) {
    PipelineHandle* php = c3gMax23Pipeline_.get( );
    if(php != 0) result.insert(php);
  }

    return result;
}


// Integration time will be rounded to nearest number of frames
void
SubarrayControlImpl::integrate(const double         intTime,
                               const CORBA::Long    numInts,
                               const double         gap, 
                               const CORBA::Boolean science)
try {


    carma::util::CorrelatorSet corrset( getCorrelatorDesignation() );
    if ( corrset.isEmpty() ) {
        ostringstream os;
        os << "There is no correlator assigned to this subarray, "
           << "hence no integration is possible. "
           << "You can assign a correlator with the addCorrelator command.";
        throw CARMA_ERROR( os.str() );
    }

    const double minIntegrationTime = 0.5;

    if (intTime < minIntegrationTime) {
        ostringstream o;
        o << "Integration time (" 
          << fixed << setprecision(1) << intTime << ") must be at least "
          << minIntegrationTime << " seconds";
        throw CARMA_EXCEPTION(UserException, o.str().c_str());
    }

    // Don't allow integrate if subarray is not initialized
    if (!initializationFlag_) {
        ostringstream oss;
        oss << "Subarray has not been initialized; issue an "
            << "initialize() command in the sac";
        throw CARMA_ERROR(oss.str());
    }

    const string localSourceName = sourceName();
    if ( isThrowawayObsblock() || (!science)) {
      comment( "Integrations will not be processed by AHW and sdpFiller." );
    } else {
      comment( "" );
    }

    if (science) {
        // Check that the current source has intent set.
        // If not set, check if a default intent is available
        // for the project and use that.  Otherwise, throw.
        if ( (!currentSourceHasIntent())) {
            std::string project_ = getProject( getCorrelatorDesignation() );
            if ( hasDefaultIntent( project_ ) && science) {
                const IntentInfo iinfo = getDefaultIntent( project_ );
                ostringstream os;
                os << " Adding default intent for project "
                   << project_
                   << " = ["
                   << iinfo.purpose
                   << ","
                   << boolalpha
                   << iinfo.selfcal
                   << ","
                   << iinfo.fastSwitch
                   << "] Source = " << localSourceName;
                programLogNoticeIfPossible( os.str() );
                setDefaultIntent( iinfo );
            } 
            else {
                ostringstream o;
                o << "The source "
                  << localSourceName
                  << " has no observing intent set.  Fix this by calling "
                  << "intent( '"<<localSourceName <<"', ... )";
                throw CARMA_EXCEPTION(UserException, o.str().c_str());
            }
        }
    }

    setObsblockCurrentObsObject();

    string paramString;
    {
        ostringstream oss;
        oss << fixed
            << "intTime=" << setprecision(1) << intTime << ", "
            << "numInts=" << numInts << ", " 
            << "gap="  << setprecision(1) << gap << ", "   
            << "science=" << boolalpha << science;
        paramString = oss.str();
    }

    cmdlog() << "integrate(" << paramString << ")";

    cancelFlag_ = false;
    WorkResultSet wrs( "PipelineControl::startIntegration result set" );

    ++nextIntegSeqNo_;

    string callString;
    {
        ostringstream oss;

        oss << "PipelineHandle::integrate(" << paramString << ", "
            << "preferredSeqNo=" << nextIntegSeqNo_ << ")";
        callString = oss.str();
    }

    queueFunctorWorkRequestGroup(
        callString,
        makeHandleMethodFunctorGroup(
            getPipelineGroup( ), 
            &PipelineHandle::integrate,
            intTime,
            numInts,
            gap,
            science,
            nextIntegSeqNo_ ),
        wrs,
        *workerPool_ );

    logSentCommand("integrate", "pipeline");

    waitForAllNormal( wrs );
} catch ( ... ) {
    rethrowCaughtAsUser( );
}

void
SubarrayControlImpl::phaseOffset( const float phase,
                                  const short bandNo,
                                  const short antenna1No,
                                  const short antenna2No )
try {
    return;
} catch ( ... ) {
    rethrowCaughtAsUser( );
}


void
SubarrayControlImpl::applyTsys( const bool on )
try {

    ostringstream paramStream;
    paramStream << boolalpha << on;
    string paramString = paramStream.str();

    cmdlog() << "applyTsys(" << paramString << ")";

    WorkResultSet wrs( "PipelineControl::applyTsys cmd" );

    queueFunctorWorkRequestGroup(
        "PipelineControl::applyTsys()",
        makeRemoteObjMethodFunctorGroup(
            getPipelineGroup( ),
            "applyTsysCalibration",
            paramString,
            &PipelineControl::applyTsysCalibration,
            on ),
        wrs,
        *workerPool_ );

    waitForAllNormal( wrs );
} catch ( ... ) {
    rethrowCaughtAsUser( );
}


void
SubarrayControlImpl::applyFlux(const bool on)
try {

    ostringstream paramStream;
    paramStream << boolalpha << on;
    string paramString = paramStream.str();

    cmdlog() << "applyFlux(" << paramString << ")";

    WorkResultSet wrs( "PipelineControl::applyFlux cmd" );

    queueFunctorWorkRequestGroup(
        "PipelineControl::applyFluxCalibration()",
        makeRemoteObjMethodFunctorGroup(
            getPipelineGroup( ),
            "applyFlux",
            paramString,
            &PipelineControl::applyFluxCalibration,
            on ),
        wrs,
        *workerPool_ );

    waitForAllNormal( wrs );

    if (!on) return; // If you turn off flux, don't mess with Tsys
    // Make sure that if Flux is turned on, so is Tsys...
    WorkResultSet wrs2( "PipelineControl::applyTsysCalibration cmd" );

    queueFunctorWorkRequestGroup(
        "PipelineControl::applyTsysCalibration()",
        makeRemoteObjMethodFunctorGroup(
            getPipelineGroup( ),
            "applyTsys",
            paramString,
            &PipelineControl::applyTsysCalibration,
            on ),
        wrs2,
        *workerPool_ );

    waitForAllNormal(wrs2);

} catch ( ... ) {
    rethrowCaughtAsUser( );
}


void
SubarrayControlImpl::applyFocus( const bool on )
try {
    return;
} catch ( ... ) {
    rethrowCaughtAsUser( );
}


void
SubarrayControlImpl::setDecimationMode(const DecimationMode decimationMode,
                                       const unsigned short astrobandNo)
try {
    string paramString = "Undefined";
    switch(decimationMode) {
        case DECIMATION_OFF_KEEP_END_CHANNELS:
            paramString = "DECIMATION_OFF_KEEP_END_CHANNELS"; break;
        case DECIMATION_OFF_DROP_END_CHANNELS:
            paramString = "DECIMATION_OFF_DROP_END_CHANNELS"; break;
        case DECIMATION_ON:
            paramString = "DECIMATION_ON"; break;
    }
    cmdlog() << "decimate(" << paramString 
             << ", astrobandNo=" << astrobandNo << ")";
    
    if (astrobandNo > 40) {
        ostringstream o;
        o << "setDecimationMode(mode=" << paramString 
          << ", stroband=" << astrobandNo << ") "
          << "has astroband out of range (0-40)";
        throw CARMA_EXCEPTION(UserException, o.str().c_str());
    }
    if (astrobandNo == 0) {
        WorkResultSet wrs( "PipelineHandle::setDecimationMode() result set" );
        queueFunctorWorkRequestGroup(
            "PipelineHandle::setDecimationMode()",
            makeHandleMethodFunctorGroup(
            getPipelineGroup(),
            &PipelineHandle::setDecimationMode,
            decimationMode, astrobandNo),
            wrs,
            *workerPool_ );

        waitForAllNormal(wrs);
    }
    // A specific astroband
    else {
        if (astrobandNo < 9) {
            slPipeline_->setDecimationMode(decimationMode, astrobandNo);
        }
        else if (astrobandNo < 25) {
            wbPipeline_->setDecimationMode(decimationMode, astrobandNo);
        }
        else if (astrobandNo < 33) {
            c3gMax23Pipeline_->setDecimationMode(decimationMode, astrobandNo);
        }
        else {
            c3gMax8Pipeline_->setDecimationMode(decimationMode, astrobandNo);
        }
    }
} catch ( ... ) {
    rethrowCaughtAsUser( );
}

