// $Id: Stage.cc,v 1.2 2012/08/30 21:11:50 abeard Exp $

#include "carma/pipeline/Stage.h"

#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/monitor/PipelineCommon.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

using namespace carma::util;
using namespace carma::correlator::lib;
using namespace carma::monitor;
using namespace carma::pipeline;
using namespace std;

namespace {
    
    typedef carma::monitor::StageStats::ProcStatMonitorPointEnum ProcStatMPE;
    
    const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE3;
    const Trace::TraceLevel TRACE_STAGE_PROCESSING = Trace::TRACE5;

} // namespace <unnamed>

struct Stage::Pimpl { 
    
    explicit Pimpl( carma::monitor::StageStats & stats, 
                    const std::string & name );

    void writeStageStatsWhenFinishedHoldingLock( );

    PthreadMutex mutex_; // Catch all mutex  

    bool isActivated_;
    double accumulatedRuntime_; // Per stage per frame
    double maxProcTimeMs_;
    carma::util::frameType dataFC_;
    carma::util::frameType lastDataFC_; // Last fully processed frame 
    enum ProcStatMPE::PROCSTAT procStat_; // One of PENDING, GOOD, BAD or INPROGRESS
    const string name_;
    carma::monitor::StageStats & stageStats_;
};

Stage::Pimpl::Pimpl( carma::monitor::StageStats & stats, 
                     const std::string & name ) :
    isActivated_( true ), 
    accumulatedRuntime_( 0.0 ),
    maxProcTimeMs_( 0.0 ),
    dataFC_( 0 ),
    lastDataFC_( 0 ),
    procStat_( ProcStatMPE::BAD ),
    name_( name ),
    stageStats_( stats )
{
    // Nothing
}

void
Stage::Pimpl::writeStageStatsWhenFinishedHoldingLock( )
{
    // Note this is called twice - once by the process thread, and once
    // by the updateMonitorData thread.  
    
    // The amount of accumulated proc time up until this routine was called.
    // This is necessary because we independently write stage stats and
    // call fillStageMonitorData which means that when we kick off the monitor
    // system write (or the autowriter does it for us), some stages may not
    // be done processing.  Thus we display only stage stats up until we call
    const double localProcTimeMs = 
        accumulatedRuntime_ * Time::MILLISECONDS_PER_DAY;
    
    if ( localProcTimeMs > maxProcTimeMs_ ) maxProcTimeMs_ = localProcTimeMs;

    // Always write the current stats - regardless of when this is called.
    stageStats_.state( ).setValue( isActivated_ );
    stageStats_.dataFrameCount( ).setValue( dataFC_ );
    stageStats_.procTime( ).setValue( localProcTimeMs );
    stageStats_.maxProcTime( ).setValue( maxProcTimeMs_ );
    stageStats_.procStat( ).setValue( procStat_ );

    // If we are done with stage processing for this frame, reset things.
    if ( lastDataFC_ == dataFC_ ) { 
        accumulatedRuntime_ = 0.0;
        procStat_ = ProcStatMPE::PENDING;
    }
}

Stage::Stage( StageStats & stageStats,
              const string & name ) : 
    pimpl_( new Pimpl( stageStats, name ) )
{
    // Nothing
}

Stage::~Stage( ) 
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "Stage::~Stage() - D'tor." );
}

void Stage::activate( ) 
{
    ScopedPthreadMutexLock scopelock( pimpl_->mutex_ );
    pimpl_->isActivated_ = true;
}

void Stage::deactivate( ) 
{
    ScopedPthreadMutexLock scopelock( pimpl_->mutex_ );
    pimpl_->isActivated_ = false;
}

bool Stage::isStageActive( ) const
{
    ScopedPthreadMutexLock scopelock( pimpl_->mutex_ );
    return pimpl_->isActivated_;
}

string Stage::getName( ) const
{
    return pimpl_->name_;
}

carma::util::frameType Stage::getDataFrameCount( ) const
{
    ScopedPthreadMutexLock scopelock( pimpl_->mutex_ );
    return pimpl_->dataFC_;
}

void
Stage::preprocessCorrelatorData( 
    const carma::correlator::lib::CorrelatorDataPtr cd )
{
    const double startMJD = Time::MJD( );

    ScopedLogNdc ndc( pimpl_->name_ + " Stage Preprocess" );

    CARMA_CPTRACE( TRACE_STAGE_PROCESSING, "Stage::preprocessCorrelatorData - "
        "Calling for stage " << pimpl_->name_ << "." );

    bool isActive;
    {  // Block monitor thread out for as little time as possible.
        ScopedPthreadMutexLock scopelock( pimpl_->mutex_ );

        if ( pimpl_->lastDataFC_ == pimpl_->dataFC_ ) 
            pimpl_->accumulatedRuntime_ = 0.0;

        pimpl_->procStat_ = ProcStatMPE::INPROGRESS;
        isActive = pimpl_->isActivated_;

        if ( isActive && cd.get() != 0 ) 
            pimpl_->dataFC_ = 
                Time::computeClosestFrame( cd->getHeader().getMJD() );

    }
    
    if ( isActive && cd.get() != 0 ) {

        try {

            preprocess( cd );

        } catch (...) {
            ScopedPthreadMutexLock scopelock( pimpl_->mutex_ );
            pimpl_->procStat_ = ProcStatMPE::BAD;
            logCaughtAsError( );
        }
    }

    {
        ScopedPthreadMutexLock scopelock( pimpl_->mutex_ );
        pimpl_->accumulatedRuntime_ += ( Time::MJD() - startMJD );
    }

}

void
Stage::processCorrelatorBand( carma::correlator::lib::CorrelatorBand * cb )
{
    const double startMJD = Time::MJD( );

    ScopedLogNdc ndc( pimpl_->name_ + " Stage Process Band" );

    CARMA_CPTRACE( TRACE_STAGE_PROCESSING, "Stage::processCorrelatorBand - "
        "Calling for stage " << pimpl_->name_ << "." );
    
    bool isActive;
    {  
        ScopedPthreadMutexLock scopelock( pimpl_->mutex_ ); // THRASHER
        isActive = pimpl_->isActivated_;
    }

    if ( isActive && cb != 0 ) {
        
        try { 

            processBand( cb );

        } catch (...) {
            logCaughtAsError( );
            ScopedPthreadMutexLock scopelock( pimpl_->mutex_ );
            pimpl_->procStat_ = ProcStatMPE::BAD;
        }
    }
    
    {
        ScopedPthreadMutexLock scopelock( pimpl_->mutex_ ); // THRASHER
        pimpl_->accumulatedRuntime_ += ( Time::MJD() - startMJD );
    }

}

carma::correlator::lib::CorrelatorDataPtr
Stage::postprocessCorrelatorData( carma::correlator::lib::CorrelatorDataPtr cd )
{
    const double startMJD = Time::MJD( );

    ScopedLogNdc ndc( pimpl_->name_ + " Stage Postprocess Band" );

    CARMA_CPTRACE( TRACE_STAGE_PROCESSING, "Stage::postprocessCorrelatorBand - "
        "Calling for stage " << pimpl_->name_ << "." );

    bool isActive;
    {  
        ScopedPthreadMutexLock scopelock( pimpl_->mutex_ );
        isActive = pimpl_->isActivated_;
        if ( isActive && cd.get() != 0 ) 
            pimpl_->dataFC_ = 
                Time::computeClosestFrame( cd->getHeader().getMJD() );
    }

    CorrelatorDataPtr returnCd = cd;

    if ( isActive && cd.get() != 0 ) {

        try {

            returnCd = postprocess( cd );

        } catch (...) {
            ScopedPthreadMutexLock scopelock( pimpl_->mutex_ );
            pimpl_->procStat_ = ProcStatMPE::BAD;
            logCaughtAsError( );
        }
    }
    
    { 
        ScopedPthreadMutexLock scopelock( pimpl_->mutex_ );
        pimpl_->accumulatedRuntime_ += ( Time::MJD() - startMJD );
        if ( pimpl_->procStat_ != ProcStatMPE::BAD ) 
            pimpl_->procStat_ = ProcStatMPE::GOOD; 
        pimpl_->writeStageStatsWhenFinishedHoldingLock( ); 
        pimpl_->lastDataFC_ = pimpl_->dataFC_;
    }

    return returnCd;
}
                
void 
Stage::fillStageMonitorData( )
{
    ScopedLogNdc ndc( pimpl_->name_ + " Stage Fill Stage Monitor Data" );
    try {
        fillMonitorData( );
    } catch (...) {
        programLogErrorIfPossible( "Stage:fillStageMonitorData() - Exception "
            "caught, NOT rethrowing (thread will continue to run).  Message: "
            + getStringForCaught() );
    }
}

void
Stage::fillMonitorStageStats( )
{
    ScopedLogNdc ndc( pimpl_->name_ + " Stage Fill Monitor Stage Stats" );

    ScopedPthreadMutexLock scopelock( pimpl_->mutex_ );
        
    pimpl_->writeStageStatsWhenFinishedHoldingLock( ); 

}
