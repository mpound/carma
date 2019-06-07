// $Id: PipelineControlImpl.cc,v 1.6 2014/05/15 17:11:13 scott Exp $

#include "carma/pipeline/PipelineControlImpl.h"

#include "carma/pipeline/BlankFlagStage.h"
#include "carma/pipeline/CoherenceStage.h"
#include "carma/pipeline/DecimatorStage.h"
#include "carma/pipeline/IntegratorStage.h"
#include "carma/pipeline/PublisherStage.h"
#include "carma/pipeline/SelfCalStage.h"
#include "carma/pipeline/TsysStage.h"
#include "carma/pipeline/VisBrickWriterStage.h"
#include "carma/util/corbaSequenceUtils.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"

using namespace carma::monitor;
using namespace carma::pipeline;
using namespace carma::util;
using namespace std;

PipelineControlImpl::PipelineControlImpl(
        carma::pipeline::CoherenceStage & coherence,
        carma::pipeline::BlankFlag & blankFlag,
        carma::pipeline::Integrator & integrator,
        carma::pipeline::VisBrickWriter & visbrick,
        carma::pipeline::Decimator & decimator,
        carma::pipeline::SelfCalStage & selfCal,
        carma::pipeline::TsysStage & tsys,
        carma::pipeline::Publisher & publisher ) :
    coherence_( coherence ),
    blankFlag_( blankFlag ),
    integrator_( integrator ),
    visbrick_( visbrick ),
    decimator_( decimator ),
    selfCal_( selfCal ),
    tsys_( tsys ),
    publisher_( publisher )
{

}

PipelineControlImpl::~PipelineControlImpl( )
{
    // Nothing
}

void
PipelineControlImpl::resetTsys( const ::carma::util::SeqShort & carmaAntNoSeq )
try {
    programLogInfoIfPossible( __PRETTY_FUNCTION__ );
    vector< int > carmaAntNoVec;
    assignSequenceToVector( carmaAntNoSeq, carmaAntNoVec );
    tsys_.resetTsys( carmaAntNoVec );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
PipelineControlImpl::startIntegration( const double         intTime,
                                       const CORBA::Long    numRec,
                                       const double         gap,
                                       const CORBA::Boolean science,
                                       const CORBA::Long    seqNo )
try {
    ostringstream msg;

    msg << fixed << "PipelineControlImpl::startIntegration("
        << "integTime= "<< setprecision(1) << intTime 
        << ", numRec=" << numRec 
        << ", gap=" << setprecision(1) << gap
        << ", science=" << science
        << ", seqNo=" << seqNo << ")";

    programLogInfoIfPossible( msg.str( ) );

    if ( intTime < 0.0 ) {
        throw CARMA_EXCEPTION( carma::util::UserException,
                               "Integration Time must be positive");
    }

    integrator_.startIntegration(intTime, numRec, gap,
            static_cast<int>(seqNo));
    visbrick_.setIsScienceData(science);
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
PipelineControlImpl::stopIntegration( )
try {
    programLogInfoIfPossible( "Stop integration." );

    integrator_.stopIntegration();
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
PipelineControlImpl::resetTimeSinceLastIntegration( )
try {
    programLogInfoIfPossible( "Resetting time since last integration" );

    integrator_.resetTimeSinceLastIntegration();
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
PipelineControlImpl::applyTsysCalibration( CORBA::Boolean apply )
try {
    ostringstream msg;

    msg << "PipelineControlImpl::applyTsysCalibration( apply="
        << std::boolalpha << apply << " ).";

    programLogInfoIfPossible( msg.str( ) );

    tsys_.applyTsysToData( apply );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
PipelineControlImpl::applyFluxCalibration( CORBA::Boolean apply )
try {
    ostringstream msg;

    msg << "PipelineControlImpl::applyFluxCalibration( apply="
        << std::boolalpha << apply << " )";

    programLogInfoIfPossible( msg.str( ) );

    tsys_.applyFluxCalibrationToData( apply );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
PipelineControlImpl::activateCoherenceMonitor( )
try {
    programLogInfoIfPossible( "PipelineControlImpl::activateCoherenceMonitor( )" );

    coherence_.activate();
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
PipelineControlImpl::deactivateCoherenceMonitor( )
try {
    programLogInfoIfPossible( "PipelineControlImpl::deactivateCoherenceMonitor( )" );

    coherence_.deactivate();
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
PipelineControlImpl::activateDecimator( )
try {
    programLogInfoIfPossible( "PipelineControlImpl::activateDecimator( )" );

    decimator_.activate();
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
PipelineControlImpl::deactivateDecimator( )
try {
    programLogInfoIfPossible( "PipelineControlImpl::deactivateDecimator( )" );

    decimator_.deactivate();
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
PipelineControlImpl::activateTsys( )
try {
    programLogInfoIfPossible( "PipelineControlImpl::activateTsys( )" );

    tsys_.activate();
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
PipelineControlImpl::deactivateTsys( )
try {
    programLogInfoIfPossible( "PipelineControlImpl::deactivateTsys( )" );

    tsys_.deactivate();
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
PipelineControlImpl::activateBlankFlag( )
try {
    programLogInfoIfPossible( "PipelineControlImpl::activateBlankFlag( )" );

    blankFlag_.activate();
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
PipelineControlImpl::deactivateBlankFlag( )
try {
    programLogInfoIfPossible( "PipelineControlImpl::deactivateBlankFlag( )" );

    blankFlag_.deactivate();
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
PipelineControlImpl::activateSelfCal( )
try {
    programLogInfoIfPossible( "PipelineControlImpl::activateSelfCal( )" );

    selfCal_.activate();
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
PipelineControlImpl::deactivateSelfCal( )
try {
    programLogInfoIfPossible( "PipelineControlImpl::deactivateSelfCal( )" );

    selfCal_.deactivate();
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
PipelineControlImpl::activatePublisher( )
try {
    programLogInfoIfPossible( "PipelineControlImpl::activatePublisher( )" );

    publisher_.activate();
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
PipelineControlImpl::deactivatePublisher( )
try {
    programLogInfoIfPossible( "PipelineControlImpl::activatePublisher( )" );

    publisher_.deactivate();
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
PipelineControlImpl::keepEndChannels( const bool keep,
                                      const CORBA::UShort astroBandNo )
try {
    ostringstream msg;

    msg << "PipelineControlImpl::keepEndChannels(keep=" << boolalpha
        << keep << ", astroBandNo=" << astroBandNo << ")";
    programLogInfoIfPossible( msg.str( ) );

    if ( 0 == astroBandNo ) {
        decimator_.keepEndChannels(keep);
    } else {
        decimator_.keepEndChannels(keep, astroBandNo);
    }
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
PipelineControlImpl::decimate( const bool dec,
                               const CORBA::UShort astroBandNo )
try {
    ostringstream msg;

    msg << "PipelineControlImpl::decimate(dec=" << boolalpha
        << dec << ", bandNo=" << astroBandNo << ")";
    programLogInfoIfPossible( msg.str( ) );

    if ( 0 == astroBandNo ) {
        decimator_.decimation(dec);
    } else {
        decimator_.decimation(dec, astroBandNo);
    }
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
PipelineControlImpl::logCalibrationOnce( const CORBA::Short astroband )
try {

    tsys_.logCalibration( astroband );

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

void
PipelineControlImpl::setReferenceAnt( const CORBA::Short antNo )
try {
   selfCal_.setReferenceAntNo( antNo );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}
