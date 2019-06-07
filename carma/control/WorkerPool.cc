#include "carma/control/WorkerPool.h"

#include <deque>
#include <vector>
#include <sstream>

#include <pthread.h>
#include <sys/time.h>

#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/PthreadCond.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedLockManager.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/StartPthread.h"
#include "carma/util/Trace.h"
#include "carma/util/WorkRequest.h"
#include "carma/util/SimpleStatisticsAccumulators.h"

#include <boost/shared_ptr.hpp>


using namespace ::std;
using namespace carma;
using namespace carma::control;
using namespace carma::util;


namespace {


const Trace::TraceLevel kDefTraceLev = Trace::TRACE5;

long
latencyMicroseconds( const struct ::timeval & beginTime,
                     const struct ::timeval & endTime )
{
    long deltaSecs = endTime.tv_sec - beginTime.tv_sec;
    long deltaMicros;

    if ( endTime.tv_usec >= beginTime.tv_usec )
        deltaMicros = endTime.tv_usec - beginTime.tv_usec;
    else {
        deltaSecs -= 1;
        deltaMicros = (1000 * 1000) - (beginTime.tv_usec - endTime.tv_usec);
    }

    const long totalMicros = (deltaSecs * 1000 * 1000) + deltaMicros;
    if (totalMicros < 0)
        return 0;

    return totalMicros;
}


// BULLSHIT_TWC - Unify this with the version in the fault system
string
latencyString( const struct ::timeval & beginTime,
               const struct ::timeval & endTime )
{
    long deltaSecs = endTime.tv_sec - beginTime.tv_sec;
    long deltaMicros;

    if ( endTime.tv_usec >= beginTime.tv_usec )
        deltaMicros = endTime.tv_usec - beginTime.tv_usec;
    else {
        deltaSecs -= 1;
        deltaMicros = (1000 * 1000) - (beginTime.tv_usec - endTime.tv_usec);
    }

    ostringstream oss;

    if ( deltaSecs > 0 ) {
        const long deltaMillis =
            (deltaSecs * 1000) + ((deltaMicros + 500) / 1000);

        oss << deltaMillis << " ms";
    } else if ( deltaMicros >= (10 * 1000) ) {
        const long deltaMillis = ((deltaMicros + 500) / 1000);

        oss << deltaMillis << " ms";
    } else if ( deltaMicros >= 1000 ) {
        const long deltaHundredMicros = ((deltaMicros + 50) / 100);
        const long highDigit = (deltaHundredMicros / 10);
        const long lowDigit = deltaHundredMicros - 10 * highDigit;

        oss << highDigit << "." << lowDigit << " ms";
    } else if ( deltaMicros >= 100 ) {
        const long deltaTenMicros = ((deltaMicros + 5) / 10);
        const long highDigit = (deltaTenMicros / 10);
        const long lowDigit = deltaTenMicros - 10 * highDigit;

        oss << "0." << highDigit <<  lowDigit << " ms";
    } else if ( deltaMicros >= 10 ) {
        oss << "0.0" << deltaMicros << " ms";
    } else {
        oss << "0.00" << deltaMicros << " ms";
    }

    return oss.str();
}


class PthreadWorkerPool {
    public:
        explicit PthreadWorkerPool( const string & id,
                                    ::size_t       numWorkers,
                                    const bool     logStatsByDefault );

        virtual ~PthreadWorkerPool( );

        void queueRequestGroup( const set< WorkRequest > & group,
                                const bool *               logStats,
                                struct ::timeval *         queueTime );

        void getStatistics( struct WorkerPoolStats &stats );

    private:
        // No copying
        PthreadWorkerPool( const PthreadWorkerPool & );
        PthreadWorkerPool & operator=( const PthreadWorkerPool & );

        struct ThreadArgs {
            ThreadArgs( PthreadWorkerPool & inPool );

            PthreadWorkerPool & pool;
        };

        struct DequeEntry {
            DequeEntry( const WorkRequest & rhs );

            WorkRequest      wr;
            struct ::timeval queueTime;
            bool             logStats;
        };

        typedef deque< DequeEntry > DequeType;

        void cleanUpAllThreads( );

        static void workerEntryPoint( const ThreadArgs & pool );

        // statistics gathering points (NOTE: MUST lock around these!)
        void stats_execute_start(const std::string &id, const long ms_queue);
        void stats_execute_complete(const std::string &id, const long ms_execute);
        void stats_execute_exception(const std::string &id, const long ms_execute);

        const string          id_;
        const bool            logStatsByDefault_;

        ::size_t              numWorkers_;
        vector< ::pthread_t > workersStorage_;

        PthreadMutex          requestGuard_;
        PthreadCond           requestCond_;
        bool                  quitRequested_;
        DequeType             requestDeque_;

        // statistics, all protected by requestGuard_ mutex
        unsigned int instNumExecuting_;
        unsigned int frameNumExceptions_;
        unsigned int totalNumExceptions_;
        UnsignedIntStatAccumulator frameQueueTimeAccum_;
        UnsignedIntStatAccumulator totalQueueTimeAccum_;
        UnsignedIntStatAccumulator frameExecuteTimeAccum_;
        UnsignedIntStatAccumulator totalExecuteTimeAccum_;

        std::string frameQueueTimeMaxId_;
        std::string totalQueueTimeMaxId_;
        std::string frameExecuteTimeMaxId_;
        std::string totalExecuteTimeMaxId_;
};


PthreadWorkerPool::ThreadArgs::ThreadArgs( PthreadWorkerPool & inPool ) :
pool( inPool )
{
}


PthreadWorkerPool::DequeEntry::DequeEntry( const WorkRequest & rhs ) :
wr( rhs )
{
}


PthreadWorkerPool::PthreadWorkerPool( const string & id,
                                      const ::size_t numWorkers,
                                      const bool     logStatsByDefault )
    : id_( id )
    , logStatsByDefault_( logStatsByDefault )
    , numWorkers_( 0 )
    , quitRequested_( false )
    , instNumExecuting_( 0 )
    , frameNumExceptions_( 0 )
    , totalNumExceptions_( 0 )
{
    try {
        workersStorage_.resize( numWorkers );

        while ( numWorkers_ < numWorkers ) {
            string workerNdc;

            {
                ostringstream oss;

                if ( id.empty() == false )
                    oss << id_ << " ";

                oss << "worker ";

                if ( numWorkers_ < 26 )
                    oss << static_cast< char >( 'A' + numWorkers_ );
                else
                    oss << "#" << (1 + numWorkers_);

                workerNdc = oss.str();
            }

            const ThreadArgs workerArgs( *this );

            workersStorage_[ numWorkers_ ] =
                StartPthreadWithCopy( workerEntryPoint,
                                      workerArgs,
                                      workerNdc );

            ++numWorkers_;
        }
    } catch ( ... ) {
        cleanUpAllThreads();

        throw;
    }
}


PthreadWorkerPool::~PthreadWorkerPool( )
try {
    cleanUpAllThreads();
} catch ( ... ) {
    // just stifle the exception

    return;
}


void
PthreadWorkerPool::cleanUpAllThreads( )
{
    {
        const ScopedLock< PthreadMutex > lock( requestGuard_ );

        quitRequested_ = true;
    }

    requestCond_.Broadcast();

    while ( numWorkers_ > 0 ) {
        --numWorkers_;

        if ( numWorkers_ < workersStorage_.size() ) {
            void * result = 0;

            ::pthread_join( workersStorage_[ numWorkers_ ], &result );
        }
    }
}


void
PthreadWorkerPool::workerEntryPoint( const ThreadArgs & args )
try {
    PthreadWorkerPool &pool = args.pool;
    CARMA_CPTRACE( kDefTraceLev, "started" );

    while ( true ) {
        ScopedLockManager< PthreadMutex > guardManager( pool.requestGuard_ );

        guardManager.lock();

        while ( (pool.quitRequested_ == false) && pool.requestDeque_.empty() ) {
            pool.requestCond_.Wait( pool.requestGuard_ );
        }

        if ( pool.quitRequested_ )
            break;

        struct ::timeval pickUpTime;
        ::gettimeofday( &pickUpTime, 0 );

        DequeEntry dequeEntry = pool.requestDeque_.front();
        pool.requestDeque_.pop_front();

        pool.stats_execute_start(dequeEntry.wr.getId(), latencyMicroseconds(dequeEntry.queueTime, pickUpTime));
        guardManager.unlock();

        const ScopedLogNdc ndc( dequeEntry.wr.getId() );

        const bool doTrace = (Trace::getProgramTraceIfWillWrite( kDefTraceLev ) != 0 );

        if ( dequeEntry.logStats || doTrace ) {
            const string msg =
                "Picked up " +
                latencyString( dequeEntry.queueTime, pickUpTime ) +
                " after queue time";

            if ( dequeEntry.logStats )
                programLogInfo( msg );

            if ( doTrace ) {
                CARMA_CPTRACE( kDefTraceLev, msg );
                CARMA_CPTRACE( kDefTraceLev, "service begins" );
            }
        }

        struct ::timeval beginTime;
        ::gettimeofday( &beginTime, 0 );

        try {
            // service the request
            dequeEntry.wr.service();

            struct ::timeval endTime;
            ::gettimeofday( &endTime, 0 );

            // execution completed successfully
            {
                ScopedLock< PthreadMutex > scopelock( pool.requestGuard_ );
                pool.stats_execute_complete( dequeEntry.wr.getId(),  
                                             latencyMicroseconds( beginTime, 
                                                                  endTime ) );
            } 
            if ( dequeEntry.logStats || doTrace ) {

                const string msg =
                    "Servicing began " +
                    latencyString( dequeEntry.queueTime, beginTime ) +
                    " after queue time, ended " +
                    latencyString( dequeEntry.queueTime, endTime ) +
                    " after queue time, and took " +
                    latencyString( beginTime, endTime );

                if ( dequeEntry.logStats )
                    programLogInfo( msg );

                if ( doTrace ) { CARMA_CPTRACE( kDefTraceLev, msg ); }
            }
        } catch ( ... ) {

            struct ::timeval exceptionTime;
            ::gettimeofday( &exceptionTime, 0 );

            // execution completed with exception
            guardManager.lock();
            pool.stats_execute_exception(dequeEntry.wr.getId(), latencyMicroseconds(beginTime, exceptionTime));
            guardManager.unlock();

            const string msg =
                "Servicing began " +
                latencyString( dequeEntry.queueTime, beginTime ) +
                " after queue time and generated an exception " +
                latencyString( dequeEntry.queueTime, exceptionTime ) +
                " after queue time: " + getStringForCaught();

            programLogError( msg );

            CARMA_CPTRACE( Trace::TRACE1, msg );

            // just stifle the exception
        }
    }

    CARMA_CPTRACE( kDefTraceLev, "exiting normally" );
} catch ( ... ) {
    programLogErrorIfPossible(
        "Coming out of PthreadWorkerPool::workerEntryPoint"
        " on an exception - " + getStringForCaught() );

    throw;
}


void
PthreadWorkerPool::queueRequestGroup( const set< WorkRequest > & group,
                                      const bool * const         pLogStats,
                                      struct ::timeval * const   pQueueTime )
{
    const bool doTrace =
        (Trace::getProgramTraceIfWillWrite( kDefTraceLev ) != 0 );

    if ( doTrace ) { CARMA_CPTRACE( kDefTraceLev, "queueing request group" ); }

    const bool logStats =
        ((pLogStats == 0) ? logStatsByDefault_ : (*pLogStats));

    const ::size_t groupSize = group.size();

    if ( groupSize == 0 )
        return;

    ::size_t j = 0;

    ScopedLockManager< PthreadMutex > guardManager( requestGuard_ );

    guardManager.lock();

    // NOTE: I use a ScopedLockManager< T > instead of a ScopedLock< T > so
    //       that I can avoid DequeType::~DequeType being called
    //       on temp until after all use of the mutex and condition variable
    //       is done (i.e. I minimize the latency for getting the worker
    //       threads going if they are ready to handle these requests).
    DequeType newDeque( requestDeque_ );

    newDeque.insert( newDeque.end(), group.begin(), group.end() );

    requestDeque_.swap( newDeque );

    DequeType::reverse_iterator i = requestDeque_.rbegin();
    const DequeType::reverse_iterator iEnd = requestDeque_.rend();

    struct ::timeval queueTime;

    ::gettimeofday( &queueTime, 0 );

    while ( (i != iEnd) && (j < groupSize) ) {
        i->queueTime = queueTime;
        i->logStats = logStats;

        ++i;
        ++j;
    }

    if ( doTrace ) { CARMA_CPTRACE( kDefTraceLev, "queued request group" ); }

    guardManager.unlock();

    // NOTE: newDeque is now, in fact, holding the old requestDeque_
    if ( newDeque.empty() && (groupSize == 1) ) {
        requestCond_.Signal();

        if ( doTrace ) {
            CARMA_CPTRACE(
                kDefTraceLev,
                "signalled one waiter (at most) about the available request" );
        }
    } else {
        requestCond_.Broadcast();

        if ( doTrace ) {
            CARMA_CPTRACE(
                kDefTraceLev,
                "broadcast to all waiters about the available requests" );
        }
    }

    if ( pQueueTime != 0 )
        *pQueueTime = queueTime;
}

void
PthreadWorkerPool::stats_execute_start(const std::string &id, const long ms_queue)
{
    using namespace boost::accumulators;

    instNumExecuting_++;

    frameQueueTimeAccum_( static_cast< unsigned int >( ms_queue ) );
    totalQueueTimeAccum_( static_cast< unsigned int >( ms_queue ) );

    if (extract_result<tag::max>(frameQueueTimeAccum_) == static_cast<unsigned int>(ms_queue))
        frameQueueTimeMaxId_ = std::string(id, 0, 80);

    if (extract_result<tag::max>(totalQueueTimeAccum_) == static_cast<unsigned int>(ms_queue))
        totalQueueTimeMaxId_ = std::string(id, 0, 80);
}

void
PthreadWorkerPool::stats_execute_complete(const std::string &id, const long ms_execute)
{
    using namespace boost::accumulators;

    instNumExecuting_--;

    frameExecuteTimeAccum_( static_cast< unsigned int >( ms_execute ) );
    totalExecuteTimeAccum_( static_cast< unsigned int >( ms_execute ) );

    if (extract_result<tag::max>(frameExecuteTimeAccum_) == static_cast<unsigned int>(ms_execute))
        frameExecuteTimeMaxId_ = std::string(id, 0, 80);

    if (extract_result<tag::max>(totalExecuteTimeAccum_) == static_cast<unsigned int>(ms_execute))
        totalExecuteTimeMaxId_ = std::string(id, 0, 80);
}

void
PthreadWorkerPool::stats_execute_exception(const std::string &id, const long ms_execute)
{
    frameNumExceptions_++;
    totalNumExceptions_++;

    stats_execute_complete(id, ms_execute);
}

void
PthreadWorkerPool::getStatistics(struct WorkerPoolStats &stats)
{
    using namespace boost::accumulators;
    const ScopedLock< PthreadMutex > lock( requestGuard_ );

    stats.frameExceptions = frameNumExceptions_;
    stats.totalExceptions = totalNumExceptions_;

    stats.instExecuteCount = instNumExecuting_;
    stats.instQueueCount = requestDeque_.size();

    stats.frameQueueTimeMaxId = frameQueueTimeMaxId_;
    stats.frameQueueCount = extract_result<tag::count>(frameQueueTimeAccum_);
    stats.frameQueueTimeMax = extract_result<tag::max>(frameQueueTimeAccum_);
    stats.frameQueueTimeMin = extract_result<tag::min>(frameQueueTimeAccum_);
    stats.frameQueueTimeAvg = static_cast< unsigned int >( extract_result<tag::mean>(frameQueueTimeAccum_) );

    stats.totalQueueTimeMaxId = totalQueueTimeMaxId_;
    stats.totalQueueCount = extract_result<tag::count>(totalQueueTimeAccum_);
    stats.totalQueueTimeMax = extract_result<tag::max>(totalQueueTimeAccum_);
    stats.totalQueueTimeMin = extract_result<tag::min>(totalQueueTimeAccum_);
    stats.totalQueueTimeAvg = static_cast< unsigned int >( extract_result<tag::mean>(totalQueueTimeAccum_) );

    stats.frameExecuteTimeMaxId = frameExecuteTimeMaxId_;
    stats.frameExecuteCount = extract_result<tag::count>(frameExecuteTimeAccum_);
    stats.frameExecuteTimeMax = extract_result<tag::max>(frameExecuteTimeAccum_);
    stats.frameExecuteTimeMin = extract_result<tag::min>(frameExecuteTimeAccum_);
    stats.frameExecuteTimeAvg = static_cast< unsigned int >( extract_result<tag::mean>(frameExecuteTimeAccum_) );

    stats.totalExecuteTimeMaxId = totalExecuteTimeMaxId_;
    stats.totalExecuteCount = extract_result<tag::count>(totalExecuteTimeAccum_);
    stats.totalExecuteTimeMax = extract_result<tag::max>(totalExecuteTimeAccum_);
    stats.totalExecuteTimeMin = extract_result<tag::min>(totalExecuteTimeAccum_);
    stats.totalExecuteTimeAvg = static_cast< unsigned int >( extract_result<tag::mean>(totalExecuteTimeAccum_) );

    // clear per-frame values
    frameNumExceptions_ = 0;
    frameQueueTimeMaxId_ = std::string();
    frameExecuteTimeMaxId_ = std::string();

    {
        UnsignedIntStatAccumulator newAccum;
        frameQueueTimeAccum_ = newAccum;
    }

    {
        UnsignedIntStatAccumulator newAccum;
        frameExecuteTimeAccum_ = newAccum;
    }
}

}  // namespace < anonymous >


class WorkerPool::Impl {
    public:
        explicit Impl( const string & id,
                       ::size_t       numWorkers,
                       bool           logStatsByDefault );

        virtual ~Impl( );

        void queueRequestGroup( const set< WorkRequest > & group,
                                const bool *               logStats,
                                struct ::timeval *         queueTime );

        void getStatistics( struct WorkerPoolStats &stats );

    private:
        // No copying
        Impl( const Impl & );
        Impl & operator=( const Impl & );

        PthreadWorkerPool pthreadWorkerPool_;
};


WorkerPool::Impl::Impl( const string & id,
                        const ::size_t numWorkers,
                        const bool     logStatsByDefault ) :
pthreadWorkerPool_( id, numWorkers, logStatsByDefault )
{
}


WorkerPool::Impl::~Impl( )
try {
} catch ( ... ) {
    // Just stifle any exception

    return;
}


void
WorkerPool::Impl::queueRequestGroup( const set< WorkRequest > & group,
                                     const bool * const         logStats,
                                     struct ::timeval * const   queueTime )
{
    pthreadWorkerPool_.queueRequestGroup( group, logStats, queueTime );
}


void
WorkerPool::Impl::getStatistics( struct WorkerPoolStats &stats )
{
    pthreadWorkerPool_.getStatistics( stats );
}


WorkerPool::WorkerPool( const string & id,
                        const ::size_t numWorkers,
                        const bool     logStatsByDefault )
{
    impl_ = new Impl( id, numWorkers, logStatsByDefault );
}


WorkerPool::~WorkerPool( )
try {
    delete impl_;
} catch ( ... ) {
    // Just stifle any exception

    return;
}


void
WorkerPool::queueRequestGroup( const set< WorkRequest > & group )
{
    impl_->queueRequestGroup( group, 0, 0 );
}


void
WorkerPool::queueRequestGroup( const set< WorkRequest > & group,
                               const bool                 logStats )
{
    impl_->queueRequestGroup( group, &logStats, 0 );
}


void
WorkerPool::queueRequestGroup( const set< WorkRequest > & group,
                               struct ::timeval &         queueTime )
{
    impl_->queueRequestGroup( group, 0, &queueTime );
}


void
WorkerPool::queueRequestGroup( const set< WorkRequest > & group,
                               const bool                 logStats,
                               struct ::timeval &         queueTime )
{
    impl_->queueRequestGroup( group, &logStats, &queueTime );
}


void
WorkerPool::queueRequestGroup( const set< WorkRequest > & group,
                               struct ::timespec &        queueTime )
{
    struct ::timeval queueTimeVal;

    impl_->queueRequestGroup( group, 0, &queueTimeVal );

    queueTime.tv_sec = queueTimeVal.tv_sec;
    queueTime.tv_nsec = 1000 * queueTimeVal.tv_usec;
}


void
WorkerPool::queueRequestGroup( const set< WorkRequest > & group,
                               const bool                 logStats,
                               struct ::timespec &        queueTime )
{
    struct ::timeval queueTimeVal;

    impl_->queueRequestGroup( group, &logStats, &queueTimeVal );

    queueTime.tv_sec = queueTimeVal.tv_sec;
    queueTime.tv_nsec = 1000 * queueTimeVal.tv_usec;
}


void
WorkerPool::getStatistics( struct WorkerPoolStats &stats )
{
    impl_->getStatistics( stats );
}
