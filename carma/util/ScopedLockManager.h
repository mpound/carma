#ifndef CARMA_UTIL_SCOPED_LOCK_MANAGER_H
#define CARMA_UTIL_SCOPED_LOCK_MANAGER_H


#include "carma/util/checking.h"
#include "carma/util/posixErrors.h"


namespace carma {
namespace util {


//! @brief A scope class (i.e. a class that does or manages something for the
//!        lifetime of the instance) that makes non-trivial mutex lock
//!        management easier in a C++ world.
//!
//! @note
//! Please note that use of this template requires that the expressions
//! @code
//! lockMutex( m );
//! tryLockMutex( m );
//! unlockMutex( m );
//! unlockMutexNoThrow( m );
//! @endcode
//! are valid and will lock, try to lock non-blocking, and unlock a mutex
//! object if m is of type M &. In particular, to use the types
//! ScopedLock< PthreadMutex > and/or ScopedLock< pthread_mutex_t > you will
//! need to include the file "carma/util/PthreadMutex.h" to get prototypes for 
//! the appropriate functions.
//!
//! Lock chaining usage might look something like this:
//! @code
//! #include "MutexType.h"
//!
//! static long long gSharedValue1;
//! static MutexType gSharedValue1Guard;
//!
//! static short     gSharedValue2;
//! static MutexType gSharedValue2Guard;
//!
//! void
//! UpdateSharedValues( const long long newValue1,
//!                     const short     newValue2 ) {
//!     ScopedLockManager< MutexType > lock1Manager( gSharedValue1Guard );
//!     ScopedLockManager< MutexType > lock2Manager( gSharedValue2Guard );
//!
//!     lock1Manager.lock( );
//!     gSharedValue1 = newValue1;
//!     lock2Manager.lock( );
//!     lock1Manager.unlock( );
//!     gSharedValue2 = newValue2;
//! }
//! @endcode
//! Another usage might look something like this:
//! @code
//! #include "MutexType.h"
//!
//! static long long gSharedValue;
//! static MutexType gSharedValueGuard;
//!
//! void
//! UpdateSharedValueIfConvenient( const long long newValue ) {
//!     ScopedLockManager< MutexType > lockManager( gSharedValueGuard );
//!
//!     if ( lockManager.tryLock( ) )
//!         gSharedValue = newValue;
//! }
//! @endcode
//! If your locking needs are simple then ScopedLock might be all you need.

template< typename M >
class ScopedLockManager {
    public:
        explicit ScopedLockManager( M & m );

        /* virtual */ ~ScopedLockManager( );

        void lock( );

        bool tryLock( );

        void unlock( );

    private:
        // no copying
        ScopedLockManager( const ScopedLockManager & );
        ScopedLockManager & operator=( const ScopedLockManager & );
        
        M &  m_;
        bool isLocked_;
};


}  // namespace carma::util
}  // namespace carma


// ******* Below here is simply implementation *******

template< typename M >
inline
carma::util::ScopedLockManager< M >::ScopedLockManager( M & m ) :
m_( m ),
isLocked_( false )
{
}


template< typename M >
inline
carma::util::ScopedLockManager< M >::~ScopedLockManager( )
try {
    if ( isLocked_ )
        logIfPosixError( unlockMutexNoThrow( m_ ) );
} catch ( ... ) {
    // just stifle any exceptions
    
    return;
}


template< typename M >
inline void
carma::util::ScopedLockManager< M >::lock( )
{
    CARMA_CHECK( isLocked_ == false );

    lockMutex( m_ );

    isLocked_ = true;
}


template< typename M >
inline bool
carma::util::ScopedLockManager< M >::tryLock( )
{
    CARMA_CHECK( isLocked_ == false );

    const bool result = tryLockMutex( m_ );

    if ( result )
        isLocked_ = true;

    return result;
}


template< typename M >
inline void
carma::util::ScopedLockManager< M >::unlock( )
{
    CARMA_CHECK( isLocked_ );

    unlockMutex( m_ );

    isLocked_ = false;
}


#endif
