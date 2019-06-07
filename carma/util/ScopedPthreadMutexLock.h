#ifndef CARMA_UTIL_SCOPED_PTHREAD_MUTEX_LOCK_H
#define CARMA_UTIL_SCOPED_PTHREAD_MUTEX_LOCK_H


#include "carma/util/PthreadMutex.h"
#include "carma/util/posixErrors.h"


namespace carma {
namespace util {


//! @brief A scope class (i.e. a class that does or manages something for the
//! lifetime of the instance) that makes simple PthreadMutex lock management
//! easier in a C++ world.
//!
//! If your locking needs are not so simple then ScopedPthreadMutexLockManager
//! might be what you need. Usage might look something like this:
//! @code
//! static long long    gSharedValue;
//! static PthreadMutex gSharedValueGuard;
//!
//! void
//! UpdateSharedValue( const long long newValue ) {
//!     ScopedPthreadMutexLock scopeLock( gSharedValueGuard );
//!
//!     gSharedValue = newValue;
//! }
//! @endcode

class ScopedPthreadMutexLock {
    public:

        //! Obtains a lock on the given mutex for the caller's thread
        //! (possibly waiting an indeterminate amount of time along the way).
        //!
        //! @param mutex
        //!        The mutex to to wait for a lock on

        explicit ScopedPthreadMutexLock( PthreadMutex & mutex );


        //! Releases the lock on the mutex (that was given to the constructor)
        //! held by the caller's thread.

        /* virtual */ ~ScopedPthreadMutexLock( );

    private:
        // no copying
        ScopedPthreadMutexLock( const ScopedPthreadMutexLock & );
        ScopedPthreadMutexLock & operator=( const ScopedPthreadMutexLock & );

        PthreadMutex & mutex_;
};


}  // namespace util
}  // namespace carma


// ******* Below here is simply implementation *******

inline
carma::util::ScopedPthreadMutexLock::ScopedPthreadMutexLock( PthreadMutex & m ) :
mutex_( m )
{
    mutex_.Lock( );
}


inline
carma::util::ScopedPthreadMutexLock::~ScopedPthreadMutexLock( )
try {
    logIfPosixError( mutex_.UnlockNoThrow( ) );
} catch ( ... ) {
    // just stifle any exceptions
    
    return;
}


#endif
