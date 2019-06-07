#ifndef CARMA_UTIL_SCOPEDSHAREDLOCKMANAGER_H
#define CARMA_UTIL_SCOPEDSHAREDLOCKMANAGER_H

#include "carma/util/checking.h"
#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"

namespace carma {
namespace util {

/**
 * Scoped manager class for shared PthreadRWLock locks.
 *
 * Can be used for any type lock provided the following functions are defined
 * for those locks.
 * void sharedLock( LockType & );
 * void trySharedLock( LockType & );
 * void sharedUnlock( LockType & );
 *
 * Copied entirely from other Scoped Managers by Tom Costa.
 * @see carma::util::PthreadRWLock
 * @see carma::util::ScopedSharedLock
 */
template < typename L > 
class ScopedSharedLockManager {
public:

    explicit ScopedSharedLockManager( L & l );

    /* virtual */ ~ScopedSharedLockManager( );

    void lock( );

    bool tryLock( );

    void unlock( );

private:

    // prevent copying and assignment
    ScopedSharedLockManager( const ScopedSharedLockManager & );
    ScopedSharedLockManager & operator=( const ScopedSharedLockManager & );

    bool locked_;
    L & lock_;
};

}  // namespace util
} // namespace carma

template< typename L >
inline
carma::util::ScopedSharedLockManager< L >::ScopedSharedLockManager( L & l ) : 
    locked_( false ),
    lock_( l ) 
{
    // Nothing
}

template< typename L >
inline
carma::util::ScopedSharedLockManager< L >::~ScopedSharedLockManager( )
{
    try {
        if ( locked_ ) {
            sharedUnlock( lock_ );
            locked_ = false;
        }
    } catch ( const carma::util::ErrorException & err ) {
        programLogErrorIfPossible( err.getLogString( ) ); // No throw
    } catch (...) {
        programLogErrorIfPossible( "~ScopedSharedLockManger() - Unknown" );
    }
}

template< typename L >
inline void
carma::util::ScopedSharedLockManager< L >::lock( )
{
    CARMA_CHECK( locked_ == false );

    sharedLock( lock_ );
    
    locked_ = true;
}

template< typename L >
inline bool
carma::util::ScopedSharedLockManager< L >::tryLock( )
{
    CARMA_CHECK( locked_ == false );

    const bool result = trySharedLock( lock_ );

    if ( result ) 
        locked_ = true;

    return result;
}

template< typename L >
inline void
carma::util::ScopedSharedLockManager< L >::unlock( )
{
    CARMA_CHECK( locked_ == true );

    sharedUnlock( lock_ );

    locked_ = false;
}

#endif
