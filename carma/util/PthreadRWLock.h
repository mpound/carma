#ifndef CARMA_UTIL_PTHREAD_RW_LOCK_H
#define CARMA_UTIL_PTHREAD_RW_LOCK_H

#include <pthread.h>
#include <cerrno>

#include "carma/util/posixErrors.h"


namespace carma {
namespace util {


class PthreadRWLockAttr;
class PthreadRWLock;


void exclusiveLock( ::pthread_rwlock_t & );
void exclusiveLock( PthreadRWLock & );

bool tryExclusiveLock( ::pthread_rwlock_t & );
bool tryExclusiveLock( PthreadRWLock & );

void exclusiveUnlock( ::pthread_rwlock_t & );
void exclusiveUnlock( PthreadRWLock & );

void sharedLock( ::pthread_rwlock_t & );
void sharedLock( PthreadRWLock & );

bool trySharedLock( ::pthread_rwlock_t & );
bool trySharedLock( PthreadRWLock & );

void sharedUnlock( ::pthread_rwlock_t & );
void sharedUnlock( PthreadRWLock & );


//! @brief A simple wrapper class that makes use of ::pthread_rwlock_t easier
//! in a C++ world.
//!
//! If you have no idea what a read/write lock is then here is the general idea:
//! It is used to serialize manipulation/access to some shared
//! resource from multiple threads. Hence, methods of PthreadRWLock are safe to
//! call concurrently on a single instance. They wouldn't be terribly useful
//! if this wasn't the case.

class PthreadRWLock {
    public:

        //! @brief Construct a read/write lock with the CARMA defaults
        //!
        //! Any internal errors will throw an exception.
        //!
        //! @post No locks are held on the instance by @b any thread.

        explicit PthreadRWLock( );


        //! @brief Construct a read/write lock with the given attributes
        //!
        //! Any internal errors will throw an exception.
        //!
        //! @post No locks are held on the instance by @b any thread.

        explicit PthreadRWLock( const PthreadRWLockAttr & attr );


        //! @brief Construct a read/write lock with the given attributes
        //!
        //! Any internal errors will throw an exception.
        //!
        //! @post No locks are held on the instance by @b any thread.

        explicit PthreadRWLock( const ::pthread_rwlockattr_t & attr );


        //! @brief Destruct a read/write lock
        //!
        //! No exceptions will be thrown and hence internal errors will be
        //! ignored as far as clients are concerned (though they may be
        //! logged).
        //!
        //! @pre No locks are held on the instance by @b any thread.

        virtual ~PthreadRWLock( );


        //! @brief Obtain an exclusive lock on the instance for the caller's
        //!        thread

        void ExclusiveLock( );


        //! @brief Try to obtain an exclusive lock on the instance for the
        //!        caller's thread

        bool TryExclusiveLock( );


        //! @brief Release an exclusive lock on the instance for the
        //!        caller's thread

        void ExclusiveUnlock( );


        //! @brief Obtain an shared lock on the instance for the caller's
        //!        thread

        void SharedLock( );


        //! @brief Try to obtain a shared lock on the instance for the
        //!        caller's thread

        bool TrySharedLock( );


        //! @brief Release a shared lock on the instance for the
        //!        caller's thread

        void SharedUnlock( );


        //! @brief Obtain a reference to the internal ::pthread_rwlock_t
        //!
        //! @warning Use this with care. Certainly do not go off and call
        //!          something like ::pthread_rwlock_destroy on the return
        //!          value. If you don't know what you are doing with a POSIX
        //!          read/write lock then think twice before using this method.
        //!
        //! @return a reference to the internal ::pthread_rwlock_t

        const ::pthread_rwlock_t & InternalPthreadRWLock( ) const;


        //! @brief Obtain a reference to the internal ::pthread_rwlock_t
        //!
        //! @warning Use this with care. Certainly do not go off and call
        //!          something like ::pthread_rwlock_destroy on the return
        //!          value. If you don't know what you are doing with a POSIX
        //!          read/write lock then think twice before using this method.
        //!
        //! @return a reference to the internal ::pthread_rwlock_t

        ::pthread_rwlock_t & InternalPthreadRWLock( );


    private:
        // no copying
        PthreadRWLock( const PthreadRWLock & rhs );
        PthreadRWLock & operator=( const PthreadRWLock & rhs );

        ::pthread_rwlock_t rwlock_;
};


}  // namespace carma::util
}  // namespace carma


inline void
carma::util::exclusiveLock( ::pthread_rwlock_t & l )
{
    failIfPosixError( ::pthread_rwlock_wrlock( &l ),
                      "::pthread_rwlock_wrlock error" );
}


inline bool
carma::util::tryExclusiveLock( ::pthread_rwlock_t & l )
{
    bool result = false;

    const int posixRetVal = ::pthread_rwlock_trywrlock( &l );

    switch ( posixRetVal ) {
        case 0:
            result = true;
            break;

        case EBUSY:
            result = false;
            break;

        default:
            throwPosixError( posixRetVal, "::pthread_rwlock_trywrlock error" );
    }

    return result;
}


inline void
carma::util::exclusiveUnlock( ::pthread_rwlock_t & l )
{
    failIfPosixError( ::pthread_rwlock_unlock( &l ),
                      "::pthread_rwlock_unlock error" );
}


inline void
carma::util::sharedLock( ::pthread_rwlock_t & l )
{
    failIfPosixError( ::pthread_rwlock_rdlock( &l ),
                      "::pthread_rwlock_rdlock error" );
}


inline bool
carma::util::trySharedLock( ::pthread_rwlock_t & l )
{
    bool result = false;

    const int posixRetVal = ::pthread_rwlock_tryrdlock( &l );

    switch ( posixRetVal ) {
        case 0:
            result = true;
            break;

        case EBUSY:
            result = false;
            break;

        default:
            throwPosixError( posixRetVal, "::pthread_rwlock_tryrdlock error" );
    }

    return result;
}


inline void
carma::util::sharedUnlock( ::pthread_rwlock_t & l )
{
    failIfPosixError( ::pthread_rwlock_unlock( &l ),
                      "::pthread_rwlock_unlock error" );
}


inline void
carma::util::PthreadRWLock::ExclusiveLock( )
{
    exclusiveLock( rwlock_ );
}


inline bool
carma::util::PthreadRWLock::TryExclusiveLock( )
{
    return tryExclusiveLock( rwlock_ );
}


inline void
carma::util::PthreadRWLock::ExclusiveUnlock( )
{
    exclusiveUnlock( rwlock_ );
}


inline void
carma::util::PthreadRWLock::SharedLock( )
{
    sharedLock( rwlock_ );
}


inline bool
carma::util::PthreadRWLock::TrySharedLock( )
{
    return trySharedLock( rwlock_ );
}


inline void
carma::util::PthreadRWLock::SharedUnlock( )
{
    sharedUnlock( rwlock_ );
}


inline const ::pthread_rwlock_t &
carma::util::PthreadRWLock::InternalPthreadRWLock( ) const
{
    return rwlock_;
}


inline ::pthread_rwlock_t &
carma::util::PthreadRWLock::InternalPthreadRWLock( )
{
    return rwlock_;
}


inline void
carma::util::exclusiveLock( PthreadRWLock & l )
{
    l.ExclusiveLock();
}


inline bool
carma::util::tryExclusiveLock( PthreadRWLock & l )
{
    return l.TryExclusiveLock();
}


inline void
carma::util::exclusiveUnlock( PthreadRWLock & l )
{
    l.ExclusiveUnlock();
}


inline void
carma::util::sharedLock( PthreadRWLock & l )
{
    l.SharedLock();
}


inline bool
carma::util::trySharedLock( PthreadRWLock & l )
{
    return l.TrySharedLock();
}


inline void
carma::util::sharedUnlock( PthreadRWLock & l )
{
    l.SharedUnlock();
}


#endif
