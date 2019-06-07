#ifndef CARMA_UTIL_SCOPED_SHARED_LOCK_H
#define CARMA_UTIL_SCOPED_SHARED_LOCK_H


namespace carma {
namespace util {


//! @brief A templated scope class (i.e. a class that does or manages something
//!        for the lifetime of the instance) that makes simple lock
//!        management easier in a C++ world.
//!
//! @note
//! Please note that use of this template requires that the expressions
//! @code
//! sharedLock( l );
//! sharedUnlock( l );
//! @endcode
//! are valid and will shared lock and shared unlock a lock object if l is of
//! type L &. In particular, to use the types ScopedSharedLock< PthreadRWLock >
//! and/or ScopedSharedLock< pthread_rwlock_t > you will need to include the
//! file "carma/util/PthreadRWLock.h" to get prototypes for the appropriate
//! functions.
//!
//! Typical usage would look something like this:
//! @code
//! #include "LockType.h"
//!
//! static long long gSharedValue;
//! static LockType gSharedValueGuard;
//!
//! long long
//! fetchSharedValue( ) {
//!     long long result;
//!
//!     {
//!         ScopedSharedLock< LockType > lock( gSharedValueGuard );
//!
//!         result = gSharedValue;
//!     }
//!
//!     return result;
//! }
//! @endcode
//!
//! If your locking needs are not so simple then
//! ScopedSharedLockManager< L > might be what you need.

template< typename L >
class ScopedSharedLock {
    public:

        //! Obtains an shared lock on the given lock for the caller's
        //! thread (possibly waiting an indeterminate amount of time along the
        //! way).
        //!
        //! @param l The lock to to wait for an shared lock on

        explicit ScopedSharedLock( L & l );


        //! Releases the shared lock on the lock (that was given to the
        //! constructor) held by the caller's thread.

        /* virtual */ ~ScopedSharedLock( );

    private:
        // no copying
        ScopedSharedLock( const ScopedSharedLock & );
        ScopedSharedLock & operator=( const ScopedSharedLock & );

        L & l_;
};


}  // namespace carma::util
}  // namespace carma


// ******* Below here is simply implementation *******

template< typename L >
inline
carma::util::ScopedSharedLock< L >::ScopedSharedLock( L & l ) :
l_( l )
{
    sharedLock( l );
}


template< typename L >
inline
carma::util::ScopedSharedLock< L >::~ScopedSharedLock( )
try {
    sharedUnlock( l_ );
} catch ( ... ) {
    // just stifle any exceptions
    
    return;
}


#endif
