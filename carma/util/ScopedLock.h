#ifndef CARMA_UTIL_SCOPED_LOCK_H
#define CARMA_UTIL_SCOPED_LOCK_H


#include "carma/util/posixErrors.h"


namespace carma {
namespace util {


//! @brief A templated scope class (i.e. a class that does or manages something
//!        for the lifetime of the instance) that makes simple mutex lock
//!        management easier in a C++ world.
//!
//! @note
//! Please note that use of this template requires that the expressions
//! @code
//! lockMutex( m );
//! unlockMutexNoThrow( m );
//! @endcode
//! are valid and will lock and unlock a mutex object if m is of type M &.
//! In particular, to use the types ScopedLock< PthreadMutex > and/or
//! ScopedLock< pthread_mutex_t > you will need to include the file
//! "carma/util/PthreadMutex.h" to get prototypes for the appropriate functions.
//!
//! Typical usage would look something like this:
//! @code
//! #include "MutexType.h"
//!
//! static long long gSharedValue;
//! static MutexType gSharedValueGuard;
//!
//! void
//! UpdateSharedValue( const long long newValue ) {
//!     ScopedLock< MutexType > scopeLock( gSharedValueGuard );
//!
//!     gSharedValue = newValue;
//! }
//! @endcode
//!
//! If your locking needs are not so simple then ScopedLockManager< M >
//! might be what you need.

template< typename M >
class ScopedLock {
    public:

        //! Obtains a lock on the given mutex for the caller's thread
        //! (possibly waiting an indeterminate amount of time along the way).
        //!
        //! @param m The mutex to to wait for a lock on

        explicit ScopedLock( M & m );


        //! Releases the lock on the mutex (that was given to the constructor)
        //! held by the caller's thread.

        /* virtual */ ~ScopedLock( );

    private:
        // no copying
        ScopedLock( const ScopedLock & );
        ScopedLock & operator=( const ScopedLock & );

        M & m_;
};


}  // namespace carma::util
}  // namespace carma


// ******* Below here is simply implementation *******

template< typename M >
inline
carma::util::ScopedLock< M >::ScopedLock( M & m ) :
m_( m )
{
    lockMutex( m );
}


template< typename M >
inline
carma::util::ScopedLock< M >::~ScopedLock( )
try {
    logIfPosixError( unlockMutexNoThrow( m_ ) );
} catch ( ... ) {
    // just stifle any exceptions
    
    return;
}


#endif
