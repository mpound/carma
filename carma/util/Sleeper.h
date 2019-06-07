#ifndef CARMA_UTIL_SLEEPER_H
#define CARMA_UTIL_SLEEPER_H

#include <sys/time.h>

#include "carma/util/PthreadMutex.h"
#include "carma/util/PthreadCond.h"


namespace carma {
namespace util {


//! @brief Abstract base class for PthreadCond::TimedWait based sleeping with
//!        quit/interuption support.
//!
//! Instances of Sleeper are only intended to be used from a single thread.
//! The Sleeper is undefined if multiple threads attempt to wait on the same
//! ScheduledTimer instance. It is however, safe to have multiple threads
//! waiting on distinct Sleeper instances.
//! Insert your favourite Woody Allen joke here.
//!     "If you want to make God laugh, tell him your future plans."
class Sleeper {
    public:
        explicit Sleeper( );
        
        virtual ~Sleeper( );

        void waitUntilAbsTime( const struct ::timespec & absTime );
        void waitUntilAbsTime( const struct ::timeval & absTime );

        void waitForWholeSecDuration( const int duration );

        //! Interrupt the wait that is presently occurring on this instance
        //! or if no such wait is presently occurring then preemptively
        //! interrupt the next wait that occurs. This is the one method that
        //! can be called by any thread other than the owning thread of the
        //! instance.
        void interruptPresentOrNextWait( );

    private:
        void internalWaitUntilAbsTime( const struct ::timespec & absTime );

        PthreadCond  cond_;
        PthreadMutex mutex_;
        bool         interruptRequested_;
};


}  // namespace util
}  // namespace carma

#endif
