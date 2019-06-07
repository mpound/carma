#ifndef CARMA_CONTROL_WORKERPOOL_H
#define CARMA_CONTROL_WORKERPOOL_H


//! @file
//! @brief Interface file for the carma::control::WorkerPool class


#include <set>
#include <string>


struct timespec;
struct timeval;


namespace carma {
namespace util {
    class WorkRequest;
}  // namespace carma::util

namespace control {

//! @brief A structure which contains statistics about the WorkerPool
struct WorkerPoolStats {
    unsigned int frameExceptions;
    unsigned int totalExceptions;

    unsigned int instExecuteCount;
    unsigned int instQueueCount;

    std::string  frameQueueTimeMaxId;
    unsigned int frameQueueCount;
    unsigned int frameQueueTimeMax;
    unsigned int frameQueueTimeMin;
    unsigned int frameQueueTimeAvg;

    std::string  totalQueueTimeMaxId;
    unsigned int totalQueueCount;
    unsigned int totalQueueTimeMax;
    unsigned int totalQueueTimeMin;
    unsigned int totalQueueTimeAvg;

    std::string  frameExecuteTimeMaxId;
    unsigned int frameExecuteCount;
    unsigned int frameExecuteTimeMax;
    unsigned int frameExecuteTimeMin;
    unsigned int frameExecuteTimeAvg;

    std::string  totalExecuteTimeMaxId;
    unsigned int totalExecuteCount;
    unsigned int totalExecuteTimeMax;
    unsigned int totalExecuteTimeMin;
    unsigned int totalExecuteTimeAvg;
};


//! @brief A pool of workers that can have work requests queued to it for
//!        servicing
//!
//! @note At present there is no guarantee about the order in which queued work
//!       requests will be serviced.
class WorkerPool {
    public:
        //! @brief Construct a pool of workers
        WorkerPool( const ::std::string & id,
                    ::size_t              numWorkers,
                    bool                  logStatsByDefault );
        
        //! @brief Destruct a pool of workers
        virtual ~WorkerPool( );
        
        //! @brief Queue a group of work requests for servicing
        //!
        //! This operation is atomic with respect to exceptions. Either all
        //! the work requests in the group are queued for servicing or none of
        //! them are queued.
        void queueRequestGroup(
            const ::std::set< util::WorkRequest > & group );
        
        //! @brief Queue a group of work requests for servicing
        //!
        //! This operation is atomic with respect to exceptions. Either all
        //! the work requests in the group are queued for servicing or none of
        //! them are queued.
        void queueRequestGroup(
            const ::std::set< util::WorkRequest > & group,
            bool                                    logStats );
        
        //! @brief Queue a group of work requests for servicing
        //!
        //! This operation is atomic with respect to exceptions. Either all
        //! the work requests in the group are queued for servicing or none of
        //! them are queued.
        void queueRequestGroup(
            const ::std::set< util::WorkRequest > & group,
            struct ::timeval &                      queueTime );

        //! @brief Queue a group of work requests for servicing
        //!
        //! This operation is atomic with respect to exceptions. Either all
        //! the work requests in the group are queued for servicing or none of
        //! them are queued.
        void queueRequestGroup(
            const ::std::set< util::WorkRequest > & group,
            bool                                    logStats,
            struct ::timeval &                      queueTime );

        //! @brief Queue a group of work requests for servicing
        //!
        //! This operation is atomic with respect to exceptions. Either all
        //! the work requests in the group are queued for servicing or none of
        //! them are queued.
        void queueRequestGroup(
            const ::std::set< util::WorkRequest > & group,
            struct ::timespec &                     queueTime );

        //! @brief Queue a group of work requests for servicing
        //!
        //! This operation is atomic with respect to exceptions. Either all
        //! the work requests in the group are queued for servicing or none of
        //! them are queued.
        void queueRequestGroup(
            const ::std::set< util::WorkRequest > & group,
            bool                                    logStats,
            struct ::timespec &                     queueTime );

        //! @brief Get statistics about this WorkerPool object
        //!
        //! This MUST be called exactly once every 500ms frame, no more, no less.
        //! The "frame" values are cleared each time this is called.
        void getStatistics(struct WorkerPoolStats &stats);

    private:
        // No copying
        WorkerPool( const WorkerPool & rhs );
        WorkerPool & operator=( const WorkerPool & rhs );
        
        class Impl;
    
        Impl * impl_;
};


}  // namespace carma::control
}  // namespace carma


#endif
