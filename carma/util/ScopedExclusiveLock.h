#ifndef CARMA_UTIL_SCOPED_EXCLUSIVE_LOCK_H
#define CARMA_UTIL_SCOPED_EXCLUSIVE_LOCK_H


namespace carma {
namespace util {


//! @brief A templated scope class (i.e. a class that does or manages something
//!        for the lifetime of the instance) that makes simple lock
//!        management easier in a C++ world.
//!
//! @note
//! Please note that use of this template requires that the expressions
//! @code
//! exclusiveLock( l );
//! exclusiveUnlock( l );
//! @endcode
//! are valid and will exclusive lock and exclusive unlock a lock object if l
//! is of type L &. In particular, to use the types
//! ScopedExclusiveLock< PthreadRWLock > and/or
//! ScopedExclusiveLock< pthread_rwlock_t > you will need to include the file
//! "carma/util/PthreadRWLock.h" to get prototypes for the appropriate
//! functions.
//!
//! Typical usage would look something like this:
//! @code
//! #include "LockType.h"
//!
//! static long long gSharedValue;
//! static LockType gSharedValueGuard;
//!
//! void
//! updateSharedValue( const long long newValue ) {
//!     ScopedExclusiveLock< LockType > lock( gSharedValueGuard );
//!
//!     gSharedValue = newValue;
//! }
//! @endcode
//!
//! If your locking needs are not so simple then
//! ScopedExclusiveLockManager< L > might be what you need.

template< typename L >
class ScopedExclusiveLock {
    public:

        //! Obtains an exclusive lock on the given lock for the caller's
        //! thread (possibly waiting an indeterminate amount of time along the
        //! way).
        //!
        //! @param l The lock to to wait for an exclusive lock on

        explicit ScopedExclusiveLock( L & l );


        //! Releases the exclusive lock on the lock (that was given to the
        //! constructor) held by the caller's thread.

        /* virtual */ ~ScopedExclusiveLock( );

    private:
        // no copying
        ScopedExclusiveLock( const ScopedExclusiveLock & rhs );
        ScopedExclusiveLock & operator=( const ScopedExclusiveLock & rhs );

        L & l_;
};


}  // namespace carma::util
}  // namespace carma


// ******* Below here is simply implementation *******

template< typename L >
inline
carma::util::ScopedExclusiveLock< L >::ScopedExclusiveLock( L & l ) :
l_( l ) {
    exclusiveLock( l );
}


template< typename L >
inline
carma::util::ScopedExclusiveLock< L >::~ScopedExclusiveLock( )
try {
    exclusiveUnlock( l_ );
} catch ( ... ) {
    // just stifle any exceptions
    
    return;
}


#endif
