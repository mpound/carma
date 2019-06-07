#ifndef CARMA_UTIL_SCOPED_PTHREAD_MUTEX_LOCK_MANAGER_H
#define CARMA_UTIL_SCOPED_PTHREAD_MUTEX_LOCK_MANAGER_H


#include "carma/util/checking.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/posixErrors.h"


namespace carma {
namespace util {


//! @brief A scope class (i.e. a class that does or manages something for the
//! lifetime of the instance) that makes non-trivial PthreadMutex lock
//! management easier in a C++ world.
//!
//! If your locking needs are simple then ScopedPthreadMutexLock might be all
//! you need. Lock chaining usage might look something like this:
//! @code
//! static long long    gSharedValue1;
//! static PthreadMutex gSharedValue1Guard;
//!
//! static short        gSharedValue2;
//! static PthreadMutex gSharedValue2Guard;
//!
//! void
//! UpdateSharedValues( const long long newValue1,
//!                     const short     newValue2 ) {
//!     ScopedPthreadMutexLockManager lock1Manager( gSharedValue1Guard );
//!     ScopedPthreadMutexLockManager lock2Manager( gSharedValue2Guard );
//!
//!     lock1Manager.LockMutex( );
//!     gSharedValue1 = newValue1;
//!     lock2Manager.LockMutex( );
//!     lock1Manager.UnlockMutex( );
//!     gSharedValue2 = newValue2;
//! }
//! @endcode
//! Another usage might look something like this:
//! @code
//! static long long    gSharedValue;
//! static PthreadMutex gSharedValueGuard;
//!
//! void
//! UpdateSharedValueIfConvenient( const long long newValue ) {
//!     ScopedPthreadMutexLockManager lockManager( gSharedValueGuard );
//!
//!     if ( lockManager.TryLockMutex( ) )
//!         gSharedValue = newValue;
//! }
//! @endcode

class ScopedPthreadMutexLockManager {
    public:
        explicit ScopedPthreadMutexLockManager( PthreadMutex & mutex );

        /* virtual */ ~ScopedPthreadMutexLockManager( );

        void LockMutex( );

        bool TryLockMutex( );

        void UnlockMutex( );

    private:
        // no copying
        ScopedPthreadMutexLockManager( const ScopedPthreadMutexLockManager & );
        ScopedPthreadMutexLockManager & operator=( const ScopedPthreadMutexLockManager & );
        
        PthreadMutex & mutex_;
        bool           mutexIsLocked_;
};


}  // namespace carma::util
}  // namespace carma


// ******* Below here is simply implementation *******

inline
carma::util::ScopedPthreadMutexLockManager::ScopedPthreadMutexLockManager( PthreadMutex & mutex ) :
mutex_( mutex ),
mutexIsLocked_( false )
{
}


inline
carma::util::ScopedPthreadMutexLockManager::~ScopedPthreadMutexLockManager( )
try {
    if ( mutexIsLocked_ )
        logIfPosixError( mutex_.UnlockNoThrow( ) );
} catch ( ... ) {
    // just stifle any exceptions
    
    return;
}


inline void
carma::util::ScopedPthreadMutexLockManager::LockMutex( )
{
    CARMA_CHECK( mutexIsLocked_ == false );

    mutex_.Lock( );

    mutexIsLocked_ = true;
}


inline bool
carma::util::ScopedPthreadMutexLockManager::TryLockMutex( )
{
    CARMA_CHECK( mutexIsLocked_ == false );

    bool result = mutex_.TryLock( );

    if ( result )
        mutexIsLocked_ = true;

    return result;
}


inline void
carma::util::ScopedPthreadMutexLockManager::UnlockMutex( )
{
    CARMA_CHECK( mutexIsLocked_ );

    mutex_.Unlock( );

    mutexIsLocked_ = false;
}


#endif
