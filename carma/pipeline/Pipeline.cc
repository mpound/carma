// $Id: Pipeline.cc,v 1.1 2011/08/18 23:25:52 abeard Exp $

#include "carma/pipeline/Pipeline.h"

#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/monitor/AstroSubsystemExt.h"
#include "carma/monitor/PipelineCommon.h"
#include "carma/monitor/PipelineSubsystem.h"
#include "carma/pipeline/Stage.h"
#include "carma/util/AutoPthreadQuitAndJoinGroup.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/FrameAlignedTimer.h"
#include "carma/util/MultiShotBarrier.h"
#include "carma/util/programLogging.h"
#include "carma/util/PthreadCond.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/PthreadAttr.h"
#include "carma/util/PthreadRWLock.h"
#include "carma/util/ScopedExclusiveLock.h"
#include "carma/util/ScopedExclusiveLockManager.h"
#include "carma/util/ScopedSharedLock.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/StartPthread.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/ScopedPthreadMutexLockManager.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"
#include "carma/services/Global.h"

#include <boost/foreach.hpp>
#include <deque>
#include <set>
#include <vector>

using namespace carma;
using namespace carma::correlator::lib;
using namespace carma::monitor;
using namespace carma::pipeline;
using namespace carma::services;
using namespace carma::util;
using namespace std;

namespace {

    
    typedef vector<Stage *> StageVector;
    
    typedef ScopedSharedLock<PthreadRWLock> ScopedReadLock;

    typedef ScopedExclusiveLock<PthreadRWLock> ScopedWriteLock;

    const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE3;
    const Trace::TraceLevel TRACE_THREADING = Trace::TRACE3;
    const Trace::TraceLevel TRACE_BAND_THREADING = Trace::TRACE7;
    const Trace::TraceLevel TRACE_THREAD_STARTUP = Trace::TRACE3;
    const Trace::TraceLevel TRACE_THREAD_TEARDOWN = Trace::TRACE7;
    const Trace::TraceLevel TRACE_DATA_PROCESSING = Trace::TRACE5;
    const Trace::TraceLevel TRACE_MONITOR_UPDATE = Trace::TRACE4;

    const int MAX_BAND_THREADS = 16;
    
    typedef set< int > BandNumbers;

    BandNumbers
    getCorrelatorDataBandNumbers( CorrelatorDataPtr cd )
    {
        BandNumbers answer;
        const vector< CorrelatorBand > cbands = cd->getBands();
        const vector< CorrelatorBand >::const_iterator cbBegin = cbands.begin();
        const vector< CorrelatorBand >::const_iterator cbEnd = cbands.end();
        for ( vector< CorrelatorBand >::const_iterator cb = cbBegin;
              cb != cbEnd; ++cb ) 
            answer.insert( cb->getBandNumber() );

        return answer;
    }


} // namespace <unnamed>

class Pipeline::Pimpl {
public:
    
    explicit Pimpl( PipelineSubsystem & monitorData,
                    AstroSubsystem & astroMonitor,
                    long monitorWriteDelayMs,
                    bool autowrite ); 

    /* virtual */ ~Pimpl( );

    void addCorrelatorDataToFifo( CorrelatorDataPtr cd );

    void clear( );
    
    void pushStageBack( Stage & stage );

    void incrementMissedMonitorInfoCount( int by );

private:

    static void processDataThread( Pipeline::Pimpl & This );

    static void processBandThread( Pipeline::Pimpl & This );

    static void updateMonitorDataThread( Pipeline::Pimpl & This );

    void internalProcessData( CorrelatorDataPtr cd );
    
    void dispatchCorrelatorBandsToThreadsHoldingReadLock( 
        CorrelatorDataPtr cd );

    // 'Pipeline' Stats as opposed to 'Stage' stats as each stage has their own.
    void updatePipelineTimingStats( double dataMJD, 
                                    double startMJD, 
                                    double endMJD );
    void writePipelineTimingStats( );

    struct TimingStats {
        
        TimingStats();

        PthreadMutex mutex;

        int startOffsetMs;
        int maxStartOffsetMs;
        int stopOffsetMs;
        int maxStopOffsetMs;
        float totalProcTimeMs;
        float maxTotalProcTimeMs;
        frameType dataFrame;
        int missedFrames; 
        int missedMonitorInfo;
    };
         
    TimingStats timingStats_;
     
    struct StageProcessingInfo {

        bool preprocessed;
        bool postprocessed;
        PthreadMutex mutex;
        MultiShotBarrier barrier;
        Stage * stage;

        StageProcessingInfo( Stage * s ) :
            preprocessed( false ),
            postprocessed( false ),
            stage( s ) { };

        void reset( CorrelatorDataPtr corrData ) 
        {
            ScopedPthreadMutexLock scopelock( mutex );
            preprocessed = false;
            postprocessed = false;
            if ( corrData.get() != 0 )
                barrier.reset( corrData->getNumberOfBands( ) );
        };
            
    };
        
    // Use shared_ptr to get around container copy semantics.
    typedef boost::shared_ptr< StageProcessingInfo > StageProcessingInfoPtr;
    typedef vector< StageProcessingInfoPtr > StageProcessingInfoVec;
    StageProcessingInfoVec stages_;
    PthreadRWLock stageRWLock_; // Allow multiple stages_ readers
                
    AstroSubsystem & astroMonitor_;
    PipelineSubsystem & monitorData_;

    struct CorrDataFifo { 
        deque< CorrelatorDataPtr > fifo; // Predicate: fifo is not empty 
        PthreadCond cond;                // Waits for insertion into fifo
        PthreadMutex mutex;
    } sharedFifo_;

    struct CorrBandsPendingSet {
        BandNumbers pendingBands;  // Predicate: Set size is > 1
        CorrelatorDataPtr pendingCorrData;
        PthreadCond cond;
        PthreadMutex mutex;
    } bandsPending_;
        
    struct CorrBandsFinishedSet {
        BandNumbers finishedBands; // Predicate: Lifo empty.
        PthreadCond cond;
        PthreadMutex mutex;
    } bandsFinished_;

    ::pthread_t processThreadId_;
    vector< ::pthread_t > bandThreadIds_;
    ::pthread_t monitorThreadId_; 

    FrameAlignedTimer timer_;
    const bool autowrite_;

    // Needed to bust out of threads upon quit.
    class PipelineTQRH; 
    class PipelineBandTQRH; 
    class MonitorTQRH; 

};

Pipeline::Pimpl::TimingStats::TimingStats() : 
    startOffsetMs( 0 ),
    maxStartOffsetMs( 0 ),
    stopOffsetMs( 0 ),
    maxStopOffsetMs( 0 ),
    totalProcTimeMs( 0.0 ),
    maxTotalProcTimeMs( 0.0 ),
    dataFrame( 0 ),
    missedFrames( 0 ),
    missedMonitorInfo( 0 )
{ 

}

class Pipeline::Pimpl::PipelineTQRH : public ThreadQuitRequestHandler {
public:

    explicit PipelineTQRH( Pipeline::Pimpl & parent );

    virtual ~PipelineTQRH( );

    void HandleQuitRequest( pthread_t thread );

private:

    Pipeline::Pimpl & parent_;
};

Pipeline::Pimpl::PipelineTQRH::PipelineTQRH( Pipeline::Pimpl & parent )
    : parent_( parent )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, 
                   "Pipeline::Pimpl::PipelineTQRH - Constructor." );
}

Pipeline::Pimpl::PipelineTQRH::~PipelineTQRH( )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, 
                   "Pipeline::Pimpl::PipelineTQRH - Destructor." );
}

void
Pipeline::Pimpl::PipelineTQRH::HandleQuitRequest( ::pthread_t thread ) 
{
    CARMA_CPTRACE( TRACE_THREADING, 
                   "PipelineTQRH::HandleQuitRequest - invoked." );

    // Push a null pointer onto the fifo - this will bust 
    // Pimpl::processDataThread out of a block on cond
    parent_.addCorrelatorDataToFifo( CorrelatorDataPtr( ) );

    // It's also possible we're blocked waiting for other bands to finish
    // so broadcast to that waiter.
    ScopedPthreadMutexLock scopelock( parent_.bandsFinished_.mutex ); 

    parent_.bandsFinished_.cond.Broadcast( );
}

class Pipeline::Pimpl::PipelineBandTQRH : public ThreadQuitRequestHandler {
public:

    explicit PipelineBandTQRH( Pipeline::Pimpl & parent );

    virtual ~PipelineBandTQRH( );

    void HandleQuitRequest( pthread_t thread );

private:

    Pipeline::Pimpl & parent_;
};

Pipeline::Pimpl::PipelineBandTQRH::PipelineBandTQRH( Pipeline::Pimpl & parent )
    : parent_( parent )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, 
                   "Pipeline::Pimpl::PipelineBandTQRH - Constructor." );
}

Pipeline::Pimpl::PipelineBandTQRH::~PipelineBandTQRH( )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, 
                   "Pipeline::Pimpl::PipelineBandTQRH - Destructor." );
}

void
Pipeline::Pimpl::PipelineBandTQRH::HandleQuitRequest( ::pthread_t thread ) 
{
    CARMA_CPTRACE( TRACE_THREADING, 
                   "PipelineBandTQRH::HandleQuitRequest - invoked." );

    // First acquire the mutex - should assure that band threads
    // are blocked on condition variable.
    ScopedPthreadMutexLock lock( parent_.bandsPending_.mutex );

    // Broadcast to any waiters.
    parent_.bandsPending_.cond.Broadcast();
}

class Pipeline::Pimpl::MonitorTQRH : public ThreadQuitRequestHandler {
public:

    explicit MonitorTQRH( Pipeline::Pimpl & parent );

    virtual ~MonitorTQRH( );

    void HandleQuitRequest( pthread_t thread );

private:

    Pipeline::Pimpl & parent_;
};

Pipeline::Pimpl::MonitorTQRH::MonitorTQRH( Pipeline::Pimpl & parent )
    : parent_( parent )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, 
                   "Pipeline::Pimpl::MonitorTQRH - Constructor." );
}

Pipeline::Pimpl::MonitorTQRH::~MonitorTQRH( )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, 
                   "Pipeline::Pimpl::MonitorTQRH - Destructor." );
}

void
Pipeline::Pimpl::MonitorTQRH::HandleQuitRequest( ::pthread_t thread ) 
{
    CARMA_CPTRACE( TRACE_THREADING, 
                   "MonitorTQRH::HandleQuitRequest - invoked." );

    // Interrupt block in timer or future block in timer for clean thread exit.
    parent_.timer_.InterruptPresentOrNextWait( );
}

Pipeline::Pimpl::Pimpl( PipelineSubsystem & monitorData,
                        AstroSubsystem & astroMonitor,
                        const long monitorWriteDelayMs,
                        const bool autowrite ) : 
    timingStats_( ),
    stages_( ), 
    stageRWLock_( ), 
    astroMonitor_( astroMonitor ),
    monitorData_( monitorData ),
    processThreadId_( 0 ),
    monitorThreadId_( 0 ),
    timer_( monitorWriteDelayMs * 1000 * 1000 ),
    autowrite_( autowrite )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "Pipeline::Pimpl - Constructor." );

    // NOTE: thread is started with cancellation DISABLED. 
    processThreadId_ = StartPthreadWithRef( 
        Pimpl::processDataThread,
        ( *this ),
        "Pipeline::processDataThread" );

    for ( unsigned short band = 0; band < MAX_BAND_THREADS; ++band )
    {
        ostringstream threadName;
        threadName << "Pipeline::processBandThread ( thread # " 
                   << (band + 1) << "/" << MAX_BAND_THREADS << " )";
        // NOTE: threads are started with cancellation DISABLED. 
        bandThreadIds_.push_back( 
            StartPthreadWithRef( Pimpl::processBandThread, 
                                 ( *this ), 
                                 threadName.str() ) );

    }
    
    monitorThreadId_ = StartPthreadWithRef( 
        Pimpl::updateMonitorDataThread,
        ( *this ),
        "Pipeline::updateMonitorDataThread" );

}

Pipeline::Pimpl::~Pimpl( ) 
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "Pipeline::Pimpl - Destructor." );

    // WARNING: The order in which we tear down threads is delicate.
    // The main goal here is to not interrupt current processing of 
    // correlator data (data being processed in the pipeline already).
    // In order to shutdown in a timely manner, we do not attempt to 
    // finish processing a whole integration (which might not be 
    // possible anyways if the system is being shutdown). This is 
    // especially tricky because different stages might hold mutexes
    // across Pipeline threads (see Decimator).  Since it is not 
    // practical to add TQRH implementations to all stages, I instead
    // assume here that this is happening and stage thread shutdown 
    // only after data currently being processed has made it through the
    // pipeline.  Consequence of this on the Stage implementation are
    // outlined in that classes documentation.


    try {

        // Make sure no other thread is currently processing corr or mon data.
        ScopedExclusiveLockManager<PthreadRWLock> scopelockManager( stageRWLock_ ); 
        scopelockManager.lock( );

        // Make thread quit request on all threads.
        const vector<pthread_t>::const_iterator tBegin = bandThreadIds_.begin();
        const vector<pthread_t>::const_iterator tEnd = bandThreadIds_.end();
        for ( vector<pthread_t>::const_iterator t = tBegin; t != tEnd; ++t ) 
            if ( *t != 0 ) RequestThreadQuit( *t );
        RequestThreadQuit( monitorThreadId_ );
        RequestThreadQuit( processThreadId_ );

        // Add them to an auto joiner.
        typedef AutoPthreadQuitAndJoinGroup AutoJoinGroup;
        AutoJoinGroup autoJoinGroup;  
        const AutoJoinGroup::Action JOIN_ONLY = AutoJoinGroup::JOIN_ONLY_ACTION;
        for ( vector<pthread_t>::const_iterator t = tBegin; t != tEnd; ++t ) 
            if ( *t != 0 ) autoJoinGroup.insert( *t, JOIN_ONLY );
        autoJoinGroup.insert( monitorThreadId_, JOIN_ONLY );
        autoJoinGroup.insert( processThreadId_, JOIN_ONLY );

        // At this stage, all threads will have exited except those that 
        // are blocked on stageRWLock_.  Release the lock will cause
        // them to unblock and subsequently exit on thread quit tests.
        scopelockManager.unlock( );

        // The autoJoinGroup instance reaps our dead threads
        // when we exit scope here.

    } catch ( ... ) {
        // Stifle
    }

    CARMA_CPTRACE( TRACE_CTOR_DTOR, "Pipeline::Pimpl - Exiting destructor." );
}

void 
Pipeline::Pimpl::processDataThread( Pipeline::Pimpl & This )
try {

    PipelineTQRH tqrh( This );
    ScopedThreadQuitRequestHandlerSelf scopedQuit( tqrh );

    while ( true ) {

        ThreadQuitTestSelf( );

        ScopedPthreadMutexLockManager lockManager( This.sharedFifo_.mutex );

        lockManager.LockMutex( );

        CARMA_CPTRACE( TRACE_THREADING, "Pimpl::processDataThread - "
            "Waiting on predicate (sharedFifo size > 0)." );
        
        while ( This.sharedFifo_.fifo.size( ) == 0 ) { // predicate 
            This.sharedFifo_.cond.Wait( This.sharedFifo_.mutex ); 
        }

        ThreadQuitTestSelf( ); 
        
        CARMA_CPTRACE( TRACE_THREADING, "Pimpl::processDataThread - "
            "Signaled and tested predicate (sharedFifo size > 0)." );

        // Pop CorrelatorDataPtr from the deque 
        CorrelatorDataPtr cd = This.sharedFifo_.fifo.front( );
        This.sharedFifo_.fifo.pop_front( );
        
        lockManager.UnlockMutex( );
        
        CARMA_CPTRACE( TRACE_DATA_PROCESSING, "Pipeline::Pimpl::"
                       "processDataThread - Processing correlator data for "
                       "MJD=" << cd->getHeader( ).getMJD( ) << "." );

        try { 
            if ( cd.get( ) == 0 ) {
                ThreadQuitTestSelf( );
                throw ::std::logic_error( "Pipeline::Pimpl::processDataThread"
                    " - CorrelatorDataPtr is 0 but no request has been made "
                    "to quit this thread!" );
            } 
            
            This.internalProcessData( cd );

        } catch ( ... ) {
            RethrowCaughtExceptionIfThreadQuitRequestedError( );
            logCaughtAsError( );
        }
    }

} catch ( ... ) {

    if ( CaughtExceptionIsThreadQuitRequestedError( ) ) {

        CARMA_CPTRACE( TRACE_THREADING, "Pipeline::Pimpl::processDataThread - "
            "Exiting via a thread quit request." );

        throw;
    } else {
        logCaughtAsError( );
    }
}

void
Pipeline::Pimpl::processBandThread( Pipeline::Pimpl & This )
try {

    PipelineBandTQRH btqrh( This );
    ScopedThreadQuitRequestHandlerSelf scopedQuit( btqrh ); 

    CARMA_CPTRACE( TRACE_THREAD_STARTUP, 
                   "Pipeline::Pimple::processBandThread( ) - Starting!" );

    while ( true ) {

        ThreadQuitTestSelf( );

        ScopedPthreadMutexLockManager lockManager( This.bandsPending_.mutex );

        lockManager.LockMutex( );
        
        CARMA_CPTRACE( TRACE_BAND_THREADING, "Testing for bands pending.");

        while ( This.bandsPending_.pendingBands.empty() ) {
            ThreadQuitTestSelf( );
            This.bandsPending_.cond.Wait( This.bandsPending_.mutex );
            ThreadQuitTestSelf( );
        }
        
        CARMA_CPTRACE( TRACE_BAND_THREADING, "There are pending bands!");

        // Pop band and make copy of corr data ptr.
        int bandNo;
        {
            const BandNumbers::iterator bandIt = 
                This.bandsPending_.pendingBands.begin();
            bandNo = *bandIt;
            This.bandsPending_.pendingBands.erase( bandIt );
        }
    
        string bandString;
        {
            ostringstream bandoss;
            bandoss << "band=" << bandNo;
            bandString = bandoss.str();
        }

        ScopedLogNdc logContext( bandString ); 
        
        lockManager.UnlockMutex( ); // Let others in to access this info
    
        BOOST_FOREACH( const StageProcessingInfoPtr & si, This.stages_ ) {

            ScopedLogNdc logContext( si->stage->getName() ); 
            
            CARMA_CPTRACE( TRACE_BAND_THREADING, "Begin" );

            CorrelatorDataPtr cd;
            
            { 
                { 
                    ScopedPthreadMutexLock scopelock(This.bandsPending_.mutex);
                    cd = This.bandsPending_.pendingCorrData;
                }

                ScopedPthreadMutexLock scopelock( si->mutex );

                if ( !si->preprocessed ) { // First in wins.
                    CARMA_CPTRACE( TRACE_BAND_THREADING, "Preprocessing." );
                    si->stage->preprocessCorrelatorData( cd );
                    si->preprocessed = true;
                }
            }
            
            CorrelatorBand * cb = 0;
            try {
                if ( cd.get() != 0 ) 
                    cb = &(cd->getBand( bandNo ));
            } catch (...) {
                // Stifle
            }
                    
            CARMA_CPTRACE( TRACE_BAND_THREADING, "Band processing." );
            si->stage->processCorrelatorBand( cb );

            si->barrier.wait(); // Wait for all bands in this stage to finish.

            {
                ScopedPthreadMutexLock scopelock( si->mutex );

                if ( !si->postprocessed ) { // First in wins
                    CARMA_CPTRACE( TRACE_BAND_THREADING, "Postprocessing." );

                    // Warning: Chain lock
                    ScopedPthreadMutexLock scopelock(This.bandsPending_.mutex);

                    This.bandsPending_.pendingCorrData = 
                        si->stage->postprocessCorrelatorData( cd );

                    si->postprocessed = true;
                }
            }

            CARMA_CPTRACE( TRACE_BAND_THREADING, "Done." );
        }

        // Now signal that we are done.
        {
            ScopedPthreadMutexLock scopelock( This.bandsFinished_.mutex );

            This.bandsFinished_.finishedBands.insert( bandNo );
        
            // Signal to single waiter.
            This.bandsFinished_.cond.Signal();
        }

        CARMA_CPTRACE( TRACE_BAND_THREADING, 
                       "Pipeline::Pimpl::processBandThread - Finished " 
                       << "processing band " << bandNo << "." );

    } // while ( true ) 
    
} catch ( ... ) {

    if ( CaughtExceptionIsThreadQuitRequestedError( ) ) {

        CARMA_CPTRACE( TRACE_THREAD_TEARDOWN, 
                       "Pipeline::Pimpl::processBandThread - "
                       "Exiting cleanly via a thread quit request." );

        throw;
    } else {
        logCaughtAsError( );
    }
}

void
Pipeline::Pimpl::updateMonitorDataThread( Pipeline::Pimpl & This )
try {

    MonitorTQRH mtqrh( This );
    ScopedThreadQuitRequestHandlerSelf scopedQuit( mtqrh ); 

    while ( true ) {
        
        ThreadQuitTestSelf();
        This.timer_.ResetNextFireTimeAndWait();
        ThreadQuitTestSelf();

        {
            ScopedReadLock scopelock( This.stageRWLock_ );
            ThreadQuitTestSelf();

            BOOST_FOREACH( const StageProcessingInfoPtr & si, This.stages_ ) {
                si->stage->fillMonitorStageStats( );
            }
        }

        This.writePipelineTimingStats( );
        if ( ! This.autowrite_ ) {
            This.monitorData_.write( );
            This.astroMonitor_.writeWithoutResettingValidities();
        }

    } // Loop until thread quit 

} catch (...) {
    
    if ( CaughtExceptionIsThreadQuitRequestedError( ) ) {

        CARMA_CPTRACE( TRACE_THREAD_TEARDOWN, 
                       "Pipeline::Pimpl::updateMonitorDataThread - "
                       "Exiting cleanly via a thread quit request." );

        throw;
    } else {
        logCaughtAsError( );
    }
}

void 
Pipeline::Pimpl::addCorrelatorDataToFifo( CorrelatorDataPtr cd ) 
{
    CARMA_CPTRACE( TRACE_THREADING, "Pimpl::addCorrelatorDataToFifo()"
            " Blocking on sharedFifo mutex." );
    ScopedPthreadMutexLock scopelock( sharedFifo_.mutex );

    CARMA_CPTRACE( TRACE_THREADING, "Pimpl::addCorrelatorDataToFifo." );

    if ( sharedFifo_.fifo.size( ) > 0 ) {
        ostringstream msg;
        msg << "Pipeline::processCorrelatorData( ) - Correlator Data FIFO "
            << "contains " << sharedFifo_.fifo.size( ) << " elements!";

        programLogWarnIfPossible( msg.str( ) );
    }

    sharedFifo_.fifo.push_back( cd );

    CARMA_CPTRACE( TRACE_THREADING, "Pimpl::addCorrelatorDataToFifo - "
            "Signalling waiters." );
    sharedFifo_.cond.Signal( ); // Signal to single waiter
}

void
Pipeline::Pimpl::dispatchCorrelatorBandsToThreadsHoldingReadLock( 
    CorrelatorDataPtr cd )
{
    CARMA_CPTRACE( TRACE_BAND_THREADING, "Pipeline::Pimpl::"
        "dispatchCorrelatorBandsToThreadsHoldingReadLock()." );

    if ( cd.get() == 0 ) return;

    // Process band data in separate threads
    // Populate band set.
    BandNumbers bands = getCorrelatorDataBandNumbers( cd );

    // Push bands onto pending band queue 
    if ( ! bands.empty() ) {
    
        CARMA_CPTRACE( TRACE_BAND_THREADING, 
                       "Pushing bands on per-band queue.");

        { // Define scope for locking mutex
            ScopedPthreadMutexLock scopelock( bandsPending_.mutex );

            if ( ! bandsPending_.pendingBands.empty() )
                throw ::std::logic_error( 
                        "Pipeline::Pimpl::processDataThread There are bands "
                        "pending processing but I'm starting a new set! "
                        "Something's seriously amiss." );

            // Push bands and data onto the processing queue.
            bandsPending_.pendingBands = bands;  
            
            bandsPending_.pendingCorrData = cd;
        
            CARMA_CPTRACE( TRACE_BAND_THREADING, "Broadcasting to waiters.");

            // Broadcast to waiters while holding lock.
            bandsPending_.cond.Broadcast();

        } // Unlock mutex

        // Wait on bands completed condition (e.g. bandsCompleted == bands )
        {
            ScopedPthreadMutexLock scopelock( bandsFinished_.mutex );
        
            CARMA_CPTRACE( TRACE_BAND_THREADING, "Waiting for finish.");

            // TODO: Add a timed wait which can help catch any threading or
            // deadlocking issues.  We know we need to be done in 1/2 a sec
            // so anything longer likely indicates a serious problem.
            while ( bandsFinished_.finishedBands != bands ) { 
                ThreadQuitTestSelf();
                bandsFinished_.cond.Wait( bandsFinished_.mutex );
                ThreadQuitTestSelf();
            }

            bandsFinished_.finishedBands.clear();
        } // Unlock mutex
            
        CARMA_CPTRACE( TRACE_BAND_THREADING, "Voila.");

    } // if !bands.empty()

}

void
Pipeline::Pimpl::internalProcessData( CorrelatorDataPtr cd )
{
    ThreadQuitTestSelf( );

    CARMA_CPTRACE( TRACE_BAND_THREADING, "Pipeline::Pimpl::internalProcessData"
        " About to lock stageRWLock." );

    ScopedReadLock scopelock( stageRWLock_ ); // Allow multiple readers
    
    CARMA_CPTRACE( TRACE_BAND_THREADING, "Pipeline::Pimpl::internalProcessData"
        " stageRWLock locked." );

    if ( cd.get( ) == 0 ) { // If correlator data is null the pipeline is empty
        CARMA_CPTRACE( TRACE_BAND_THREADING, "CorrelatorData shared ptr is 0.");
        return;
    } 
    
    ThreadQuitTestSelf( );

    const double start = Time::MJD( );

    BOOST_FOREACH( const StageProcessingInfoPtr & si, stages_ ) {
        si->reset( cd );
    }

    if ( cd->getNumberOfBands() > 0 ) {

        dispatchCorrelatorBandsToThreadsHoldingReadLock( cd );

    } else {
        // No bands to process in corrdata so just pre and post process.
        CorrelatorDataPtr nextStageCd( cd ); 
        BOOST_FOREACH( const StageProcessingInfoPtr & si, stages_ ) {
            si->stage->preprocessCorrelatorData( nextStageCd );
            nextStageCd = si->stage->postprocessCorrelatorData( nextStageCd );
            if ( 0 == nextStageCd.get() ) 
                break;
        }
    }

    BOOST_FOREACH( const StageProcessingInfoPtr & si, stages_ ) {
        if ( si->stage->isStageActive() )  {
            si->stage->fillStageMonitorData( );
        }
    }

    const double end = Time::MJD( );

    updatePipelineTimingStats( cd->getHeader( ).getMJD( ), start, end );

    ThreadQuitTestSelf( );
}

void
Pipeline::Pimpl::updatePipelineTimingStats( const double dataMJD,
                                            const double startMJD,
                                            const double endMJD )
{
    ScopedPthreadMutexLock scopelock( timingStats_.mutex );

    // Make sure everything is relative to the data frame received!
    const frameType dataFrame = Time::computeClosestFrame( dataMJD );

    const frameDiffType frameDiff = dataFrame - timingStats_.dataFrame;

    if ( frameDiff > 1 && timingStats_.dataFrame != 0 ) 
        timingStats_.missedFrames += frameDiff; 

    const double endDataMJD = Time::MJD( dataFrame + 1 );

    timingStats_.startOffsetMs = static_cast<int>(
        ( startMJD - endDataMJD ) * Time::MILLISECONDS_PER_DAY );
    
    timingStats_.stopOffsetMs = static_cast<int>( 
        ( endMJD - endDataMJD ) * Time::MILLISECONDS_PER_DAY );

    timingStats_.totalProcTimeMs = 
        ( endMJD - startMJD ) * Time::MILLISECONDS_PER_DAY;

    if ( timingStats_.startOffsetMs > timingStats_.maxStartOffsetMs )
        timingStats_.maxStartOffsetMs = timingStats_.startOffsetMs;

    if ( timingStats_.stopOffsetMs > timingStats_.maxStopOffsetMs ) 
        timingStats_.maxStopOffsetMs = timingStats_.stopOffsetMs;

    if ( timingStats_.totalProcTimeMs > timingStats_.maxTotalProcTimeMs ) 
        timingStats_.maxTotalProcTimeMs = timingStats_.totalProcTimeMs;

    timingStats_.dataFrame = dataFrame;
}

void
Pipeline::Pimpl::writePipelineTimingStats( ) 
{
    ScopedPthreadMutexLock scopelock( timingStats_.mutex );

    monitor::PipelineStatus & status = monitorData_.getPipelineStatus();
    status.totalProcTime( ).setValue( timingStats_.totalProcTimeMs );
    status.maxTotalProcTime( ).setValue( timingStats_.maxTotalProcTimeMs );
    status.startOffset( ).setValue( timingStats_.startOffsetMs );
    status.maxStartOffset( ).setValue( timingStats_.maxStartOffsetMs );
    status.stopOffset( ).setValue( timingStats_.stopOffsetMs );
    status.maxStopOffset( ).setValue( timingStats_.maxStopOffsetMs );
    status.dataFrame( ).setValue( timingStats_.dataFrame );
    status.missedFrames( ).setValue( timingStats_.missedFrames );
    status.missedMonitorFrames( ).setValue( timingStats_.missedMonitorInfo );
}

Pipeline::Pipeline( PipelineSubsystem & monitorData,
                    AstroSubsystem & astroMonitor,
                    const long monitorWriteDelayMs,
                    const bool autowrite )
    : pimpl_( new Pimpl( monitorData, astroMonitor, 
                         monitorWriteDelayMs, autowrite ) )
{
    // Nothing
}

Pipeline::~Pipeline( )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "Pipeline::~Pipeline( ) - D'tor." );
}

void
Pipeline::pushStageBack( Stage & stage )
{
    pimpl_->pushStageBack( stage );
}

void
Pipeline::Pimpl::pushStageBack( Stage & stage )
{
    ScopedWriteLock scopelock( stageRWLock_ ); // Only a single writer

    StageProcessingInfoPtr stageProcInfo( new StageProcessingInfo( &stage ) );
    stages_.push_back( stageProcInfo );
}

void 
Pipeline::Pimpl::incrementMissedMonitorInfoCount( const int by )
{
    ScopedPthreadMutexLock scopelock( timingStats_.mutex );
    timingStats_.missedMonitorInfo += by;
}

void
Pipeline::clear( )
{
    pimpl_->clear( );
}

void
Pipeline::Pimpl::clear( )
{
    ScopedWriteLock scopelock( stageRWLock_ ); // Only a single writer

    stages_.clear( );
}

void 
Pipeline::processCorrelatorData( CorrelatorDataPtr data ) const
{
    pimpl_->addCorrelatorDataToFifo( data );
}

void
Pipeline::incrementMissedMonitorInfo( const int by )
{
    pimpl_->incrementMissedMonitorInfoCount( by );
}
