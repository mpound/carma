#ifndef CARMA_UTIL_SCHEDULED_TIMER_H
#define CARMA_UTIL_SCHEDULED_TIMER_H

#include <ctime>

#include "carma/util/PthreadMutex.h"
#include "carma/util/PthreadCond.h"

namespace carma {
namespace util {

//! Abstract base class for PthreadCond::TimedWait based timers.
//! ScheduledTimer provides an abstract base class for PthreadCond::TimedWait
//! based timers. Instances of ScheduledTimer (and of derived classes, unless
//! their documentation says otherwise) are only intended to be used from a
//! single thread. The behavior is undefined if multiple threads attempt to
//! wait on the same ScheduledTimer instance. It is however, safe to have
//! multiple threads waiting on distinct ScheduledTimer instances.

class ScheduledTimer {
    public:

        //! Block (the thread of the caller) until the next fire time for the
        //! timer arrives. That fire time is then popped from the queue of fire
        //! times. If the next fire time has already passed then this method
        //! will pop that fire time and return immediately without blocking.
        //! If multiple fire times have already passed then subsequent calls
        //! to this method will also return immediately until all fire times
        //! that have already passed have been popped from the queue.
        //! Any internal errors will throw an exception.
        void WaitForNextFireTime( );

        //! Interrupt the wait that is presently occurring on this timer object
        //! or if no such wait is presently occurring then preemptively
        //! interrupt the next wait that occurs. This is the one method that
        //! can be called by any thread other than the owning thread of the
        //! timer object.
        void InterruptPresentOrNextWait( );

    protected:
        typedef struct ::timespec Timespec;

        static void sanityCheckTimespec( const Timespec & ts );

        explicit ScheduledTimer( bool preflight );

        virtual ~ScheduledTimer( );

        void SetNextFireTime( const Timespec & nextFireAbsTime );

        void SetNextFireTimeAndWait( const Timespec & nextFireAbsTime );

        void refreshNextFireAbsTimeIfNeeded( Timespec * nextFireAbsTime );

        virtual Timespec
        CalculateNextFireAbsoluteTime( const Timespec & fireAbsTime ) = 0;

    private:
        typedef enum {
            FIRE_ABS_TIME_STATE_INVALID,
            FIRE_ABS_TIME_STATE_NEXT,
            FIRE_ABS_TIME_STATE_LAST
        } FireAbsTimeState;

        FireAbsTimeState fireAbsTimeState_;
        Timespec         fireAbsTime_;
        PthreadCond      cond_;
        PthreadMutex     mutex_;
        bool             interruptRequested_;
};


}  // namespace util
}  // namespace carma

#endif
