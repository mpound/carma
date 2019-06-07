#ifndef CARMA_UTIL_SCOPEDEXCLUSIVELOCKMANAGER_H
#define CARMA_UTIL_SCOPEDEXCLUSIVELOCKMANAGER_H

#include "carma/util/checking.h"
#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"

namespace carma {
namespace util {

/**
 * Scoped manager class for exclusive PthreadRWLock locks.
 *
 * Can be used for any type lock provided the following functions are defined
 * for those locks.
 * void exclusiveLock( LockType & );
 * void tryExclusiveLock( LockType & );
 * void exclusiveUnlock( LockType & );
 *
 * Copied entirely from Andy Beard's ScopedSharedLockManager.
 * @see carma::util::PthreadRWLock
 * @see carma::util::ScopedExclusiveLock
 */
template < typename L > 
class ScopedExclusiveLockManager {
public:

    explicit ScopedExclusiveLockManager( L & l );

    /* virtual */ ~ScopedExclusiveLockManager( );

    void lock( );

    bool tryLock( );

    void unlock( );

private:

    // prevent copying and assignment
    ScopedExclusiveLockManager( const ScopedExclusiveLockManager & );
    ScopedExclusiveLockManager & operator=( const ScopedExclusiveLockManager & );

    bool locked_;
    L &  lock_;
};

}  // namespace util
} // namespace carma

template< typename L >
inline
carma::util::ScopedExclusiveLockManager< L >::ScopedExclusiveLockManager( L & l ) : 
locked_( false ),
lock_( l ) 
{
    // Nothing
}

template< typename L >
inline
carma::util::ScopedExclusiveLockManager< L >::~ScopedExclusiveLockManager( )
{
    try {
        if ( locked_ ) {
            exclusiveUnlock( lock_ );
            locked_ = false;
        }
    } catch ( const carma::util::ErrorException & err ) {
        programLogErrorIfPossible( err.getLogString() ); // No throw
    } catch ( ... ) {
        programLogErrorIfPossible( "~ScopedExclusiveLockManger() - Unknown" );
    }
}

template< typename L >
inline void
carma::util::ScopedExclusiveLockManager< L >::lock( )
{
    CARMA_CHECK( locked_ == false );

    exclusiveLock( lock_ );
    
    locked_ = true;
}

template< typename L >
inline bool
carma::util::ScopedExclusiveLockManager< L >::tryLock( )
{
    CARMA_CHECK( locked_ == false );

    const bool result = tryExclusiveLock( lock_ );

    if ( result ) 
        locked_ = true;

    return result;
}

template< typename L >
inline void
carma::util::ScopedExclusiveLockManager< L >::unlock( )
{
    CARMA_CHECK( locked_ == true );

    exclusiveUnlock( lock_ );

    locked_ = false;
}

#endif
