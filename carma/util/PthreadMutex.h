#ifndef CARMA_UTIL_PTHREAD_MUTEX_H
#define CARMA_UTIL_PTHREAD_MUTEX_H

#include <pthread.h>
#include <cerrno>

#include "carma/util/posixErrors.h"


namespace carma {
namespace util {


class PthreadMutexAttr;
class PthreadMutex;


void lockMutex( ::pthread_mutex_t & );
void lockMutex( PthreadMutex & );

bool tryLockMutex( ::pthread_mutex_t & );
bool tryLockMutex( PthreadMutex & );

void unlockMutex( ::pthread_mutex_t & );
void unlockMutex( PthreadMutex & );

int unlockMutexNoThrow( ::pthread_mutex_t & );
int unlockMutexNoThrow( PthreadMutex & );


//! @brief A simple wrapper class that makes use of ::pthread_mutex_t easier
//! in a C++ world.
//!
//! If you have no idea what a mutex is then here is the general idea:
//! A mutex is an token/object that only one thread can being holding a lock
//! on at a time. It is used to serialize manipulation/access to some shared
//! resource from multiple threads. Hence, methods of PthreadMutex are safe to
//! call concurrently on a single instance. They wouldn't be terribly useful
//! if this wasn't the case.

class PthreadMutex {
    public:

        //! @brief Construct a mutex with the CARMA defaults
        //!
        //! Any internal errors will throw an exception.
        //!
        //! @post A lock is @b not held on the instance by @b any thread.

        explicit PthreadMutex( );


        //! @brief Construct a mutex with the given type
        //!
        //! Any internal errors will throw an exception.
        //!
        //! @post A lock is @b not held on the instance by @b any thread.

        explicit PthreadMutex( int type );


        //! @brief Construct a mutex with the given attributes
        //!
        //! Any internal errors will throw an exception.
        //!
        //! @post A lock is @b not held on the instance by @b any thread.

        explicit PthreadMutex( const PthreadMutexAttr & attr );


        //! @brief Construct a mutex with the given attributes
        //!
        //! Any internal errors will throw an exception.
        //!
        //! @post A lock is @b not held on the instance by @b any thread.

        explicit PthreadMutex( const ::pthread_mutexattr_t & attr );


        //! @brief Destruct a mutex
        //!
        //! No exceptions will be thrown and hence internal errors will be
        //! ignored as far as clients are concerned (though they may be
        //! logged).
        //!
        //! @pre A lock is @b not held on the instance by @b any thread.

        virtual ~PthreadMutex( );


        //! @brief Obtain a lock on the instance for the caller's thread
        //!
        //! May possibly block or deadlock for an indeterminate amount of time
        //! along the way. Detected errors will throw an exception. The
        //! behavior if a lock on the instance is already held by the caller's
        //! thread at the time of the call is defined (or undefined) by POSIX
        //! according to the type of mutex that was constructed.
        //!
        //! @pre A lock is @b not held on the instance by the caller's thread.
        //!
        //! @post Either a lock is held on the instance by the caller's thread
        //!       or an exception was thrown (in which case a lock is @b not
        //!       held on the instance by the caller's thread if it was @b not
        //!       held by the caller's thread at the time of the call).

        void Lock( );


        //! Obtain a lock on the instance for the caller's thread and return
        //! @c true if it is possible to do so immediately without blocking and
        //! return @c false if obtaining a lock for the caller's thread would
        //! require blocking. Detected errors will throw an exception. The
        //! behavior is undefined if a lock on the instance is already held by
        //! the caller's thread at the time of the call.
        //!
        //! @return @c true if a lock was obtained and @c false if it was not.
        //!
        //! @pre A lock is @b not held on the instance by the caller's thread.
        //!
        //! @post One of the following states: a lock is held on the instance
        //!       by the caller's thread and @c true was returned, a lock is
        //!       @b not held on the instance by the caller's thread and
        //!       @c false was returned, or an exception was thrown (in which
        //!       case a lock is @b not held on the on the instance by the
        //!       caller's thread if it was @b not held by the caller's thread
        //!        at the time of the call).

        bool TryLock( );


        //! Release a lock that is held on the instance by the caller's thread.
        //! Detected errors will throw an exception. The behavior is undefined
        //! if a lock is @b not held on the instance by the caller's thread at
        //! the time of the call.
        //!
        //! @pre A lock is held on the instance by the caller's thread.
        //!
        //! @post Either a lock is @b not held on the instance by the caller's
        //!       thread or an exception was thrown (in which case the caller's
        //!       lock holding state is undefined).

        void Unlock( );


        int UnlockNoThrow( );


        //! @brief Obtain a reference to the internal ::pthread_mutex_t
        //!
        //! @warning Use this with care. Certainly do not go off and call
        //!          something like ::pthread_mutex_destroy on the return
        //!          value. If you don't know what you are doing with POSIX
        //!          mutexes then think twice before using this method.
        //!
        //! @return a reference to the internal ::pthread_mutex_t

        const ::pthread_mutex_t & InternalPthreadMutex( ) const;


        //! @brief Obtain a reference to the internal ::pthread_mutex_t
        //!
        //! @warning Use this with care. Certainly do not go off and call
        //!          something like ::pthread_mutex_destroy on the return
        //!          value. If you don't know what you are doing with POSIX
        //!          mutexes then think twice before using this method.
        //!
        //! @return a reference to the internal ::pthread_mutex_t

        ::pthread_mutex_t & InternalPthreadMutex( );


    private:
        // no copying
        PthreadMutex( const PthreadMutex & rhs );
        PthreadMutex & operator=( const PthreadMutex & rhs );

        ::pthread_mutex_t mutex_;
};


}  // namespace carma::util
}  // namespace carma


inline void
carma::util::lockMutex( ::pthread_mutex_t & m )
{
    failIfPosixError( ::pthread_mutex_lock( &m ),
                      "::pthread_mutex_lock error" );
}


inline bool
carma::util::tryLockMutex( ::pthread_mutex_t & m )
{
    bool result = false;

    const int posixRetVal = ::pthread_mutex_trylock( &m );

    switch ( posixRetVal ) {
        case 0:
            result = true;
            break;

        case EBUSY:
            result = false;
            break;

        default:
            throwPosixError( posixRetVal, "::pthread_mutex_lock error" );
    }

    return result;
}


inline void
carma::util::unlockMutex( ::pthread_mutex_t & m )
{
    failIfPosixError( ::pthread_mutex_unlock( &m ),
                      "::pthread_mutex_unlock error" );
}


inline int
carma::util::unlockMutexNoThrow( ::pthread_mutex_t & m )
{
    return ::pthread_mutex_unlock( &m );
}


inline void
carma::util::PthreadMutex::Lock( )
{
    lockMutex( mutex_ );
}


inline bool
carma::util::PthreadMutex::TryLock( )
{
    return tryLockMutex( mutex_ );
}


inline void
carma::util::PthreadMutex::Unlock( )
{
    unlockMutex( mutex_ );
}


inline int
carma::util::PthreadMutex::UnlockNoThrow( )
{
    return unlockMutexNoThrow( mutex_ );
}


inline const ::pthread_mutex_t &
carma::util::PthreadMutex::InternalPthreadMutex( ) const
{
    return mutex_;
}


inline ::pthread_mutex_t &
carma::util::PthreadMutex::InternalPthreadMutex( )
{
    return mutex_;
}


inline void
carma::util::lockMutex( PthreadMutex & m )
{
    m.Lock();
}


inline bool
carma::util::tryLockMutex( PthreadMutex & m )
{
    return m.TryLock();
}


inline void
carma::util::unlockMutex( PthreadMutex & m )
{
    m.Unlock();
}


inline int
carma::util::unlockMutexNoThrow( PthreadMutex & m )
{
    return m.UnlockNoThrow();
}


#endif
