#ifndef CARMA_UTIL_PTHREAD_COND_H
#define CARMA_UTIL_PTHREAD_COND_H

#include <pthread.h>

#include "carma/util/PthreadMutex.h"

namespace carma {
namespace util {


void broadcastCond( ::pthread_cond_t & c );


void signalCond( ::pthread_cond_t & c );


void waitCond( ::pthread_cond_t &  c,
               ::pthread_mutex_t & m );

void waitCond( ::pthread_cond_t & c,
               PthreadMutex &     m );


bool timedWaitCond( ::pthread_cond_t &        c,
                    ::pthread_mutex_t &       m,
                    const struct ::timespec & abstime );

bool timedWaitCond( ::pthread_cond_t &        c,
                    PthreadMutex &            m,
                    const struct ::timespec & abstime );


class PthreadCondAttr;


//! A simple wrapper class that makes use of ::pthread_cond_t easier in a
//! C++ world. Condition variables (and ::pthread_cond_t, the POSIX
//! realization of them) are a bit too complicated and tricky to explain
//! here. Look them up in a book on POSIX/Pthread programming if you need
//! more information.

class PthreadCond {
    public:
        //! @brief Default construct a cond with the CARMA defaults
        //!
        //! Any internal errors will throw an exception.
        explicit PthreadCond( );


        //! @brief Construct a cond with the given attributes
        //!
        //! Any internal errors will throw an exception.
        explicit PthreadCond( const PthreadCondAttr & attr );


        //! @brief Construct a cond with the given attributes
        //!
        //! Any internal errors will throw an exception.
        explicit PthreadCond( const ::pthread_condattr_t & attr );


        virtual ~PthreadCond( );


        //! @brief Wake any waiters
        //!
        //! Wake all threads waiting on this instance at the time of the call.
        //! 
        //! @warning While it is legal to Broadcast without holding the 
        //! associated Mutex, it is a mistake to assume that all waiting 
        //! threads are blocked on the condition, even if it seems this is
        //! the only possible option. In reality, when the lock is not held
        //! by the broadcaster, you are at the mercy of the scheduler which
        //! may have scheduled your waiting thread out prior to the atomic
        //! blocking on the condition. In this case, your waiter will never
        //! receive the broadcast. This can be especially problematic when
        //! tearing down threads where the assumption can easily lead to 
        //! deadlock. If you need predictable scheduling, you should lock
        //! the associated mutex before broadcasting.
        void Broadcast( );


        //! @brief Wake one of any waiters
        //!
        //! If any threads are waiting on this instance at the time of the
        //! call then wake one of them.
        //! 
        //! @warning While it is legal to Signal without holding the 
        //! associated Mutex, it is a mistake to assume that any waiting 
        //! thread is blocked on the condition, even if it seems this is
        //! the only possible option. In reality, when the lock is not held
        //! by the signaler, you are at the mercy of the scheduler which
        //! may have scheduled your waiting thread out prior to the atomic
        //! blocking on the condition. In this case, your waiter will never
        //! receive the signal. This can be especially problematic when
        //! tearing down threads where the assumption can easily lead to 
        //! deadlock. If you need predictable scheduling, you should lock 
        //! the associated mutex before signalling.
        void Signal( );


        //! @brief Wait until waken
        //!
        //! Atomically release the given mutex and wait the caller's thread
        //! on this instance until waked and the mutex has been reacquired
        //! (unless the given mutex is invalid/errors in some way).
        //! Spurious wakes may occur and hence the condition being waited for
        //! should be rechecked and rewaited if needed.
        void Wait( ::pthread_mutex_t & m );

        //! @brief Wait until waked
        //!
        //! Atomically release the given mutex and wait the caller's thread
        //! on this instance until waked and the mutex has been reacquired
        //! (unless the given mutex is invalid/errors in some way).
        //! Spurious wakes may occur and hence the condition being waited for
        //! should be rechecked and rewaited if needed.
        void Wait( PthreadMutex & m );


        //! @brief Wait until waken or timed out
        //!
        //! Atomically release the given mutex and wait the caller's thread
        //! on this instance until either waked or @p abstime has arrived in
        //! absolute time. In all cases the mutex will be reacquired (unless
        //! the given mutex is invalid/errors in some way).
        //! Spurious wakes may occur and hence the condition being waited for
        //! should be rechecked and rewaited if needed.
        //!
        //! @return true if waked and false if timed out
        bool TimedWait( ::pthread_mutex_t &       m,
                        const struct ::timespec & abstime );

        //! @brief Wait until waken or timed out
        //!
        //! Atomically release the given mutex and wait the caller's thread
        //! on this instance until either waked or @p abstime has arrived in
        //! absolute time. In all cases the mutex will be reacquired (unless
        //! the given mutex is invalid/errors in some way).
        //! Spurious wakes may occur and hence the condition being waited for
        //! should be rechecked and rewaited if needed.
        //!
        //! @return true if waked and false if timed out
        bool TimedWait( PthreadMutex &            m,
                        const struct ::timespec & abstime );


        //! @brief Obtain a reference to the internal ::pthread_cond_t
        //!
        //! @warning Use this with care. Certainly do not go off and call
        //!          something like ::pthread_cond_destroy on the return
        //!          value. If you don't know what you are doing with POSIX
        //!          condition variables then think twice before using this
        //!          method.
        //!
        //! @return a reference to the internal ::pthread_cond_t
        const ::pthread_cond_t & InternalPthreadCond( ) const;


        //! @brief Obtain a reference to the internal ::pthread_cond_t
        //!
        //! @warning Use this with care. Certainly do not go off and call
        //!          something like ::pthread_cond_destroy on the return
        //!          value. If you don't know what you are doing with POSIX
        //!          condition variables then think twice before using this
        //!          method.
        //!
        //! @return a reference to the internal ::pthread_cond_t
        ::pthread_cond_t & InternalPthreadCond( );


    private:
        // no copying
        PthreadCond( const PthreadCond & rhs );
        PthreadCond & operator=( const PthreadCond & rhs );

        ::pthread_cond_t cond_;
};


}  // namespace carma::util
}  // namespace carma


inline void
carma::util::PthreadCond::Wait( PthreadMutex & m )
{
    Wait( m.InternalPthreadMutex() );
}


inline bool
carma::util::PthreadCond::TimedWait( PthreadMutex &            m,
                                     const struct ::timespec & abstime )
{
    return TimedWait( m.InternalPthreadMutex(), abstime );
}


inline const ::pthread_cond_t &
carma::util::PthreadCond::InternalPthreadCond( ) const
{
    return cond_;
}


inline ::pthread_cond_t &
carma::util::PthreadCond::InternalPthreadCond( )
{
    return cond_;
}


inline void
carma::util::waitCond( ::pthread_cond_t & c,
                       PthreadMutex &     m )
{
    waitCond( c, m.InternalPthreadMutex() );
}


inline bool
carma::util::timedWaitCond( ::pthread_cond_t &        c,
                            PthreadMutex &            m,
                            const struct ::timespec & abstime )
{
    return timedWaitCond( c, m.InternalPthreadMutex(), abstime );
}


#endif
