#include "carma/util/ThreadQuit.h"


// ***************************************************************************
// *****************************     WARNING     *****************************
// ***************************************************************************
// This file is extremely subtle and hard to understand.
// It took me a very long time to write it and get it working mostly correctly.
// I don't think this is because the design or implementation are bad relative
// to other options. I think C++ safe thread quitting is just plain hard to do
// correctly and efficiently.
// This design/implementation was done under these guidlines:
//   1. For an individual thread: starts, stops, quit requests, registration
//      changes, etc. are relatively rare.
//   2. Checking very frequently for quit requests from many threads should
//      be as cheap as possible and should avoid any cross-talk/contention
//      between those threads as much as possible.
//   3. The TQRH mechanism is needed to allow breaking a target thread out of
//      various waiting states but it makes life difficult because the code in
//      here has no control over what client locks the client TQRH instance
//      will decide to go lock and block on in the course of its execution
//      and hence it is quite hard to avoid deadlocking because:
//      The target thread can call xxxSelf() at any point (and while holding
//      client locks) and the TQRH is invoked while holding the global lock
//      and the target thread's tqrhAndDeferralGuard_ lock.
// ***************************************************************************
// ***************************************************************************


#include <map>
#include <vector>
#include <stdexcept>

#include "carma/util/PthreadMutex.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedLockManager.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


class ThreadQuitInfo;

typedef ThreadQuitInfo * ThreadQuitInfoPtr;
typedef map< ::pthread_t, ThreadQuitInfoPtr > ThreadInfoMap;


__thread ThreadQuitInfoPtr gTlsThreadQuitInfo = 0;

::pthread_mutex_t gThreadInfoMapGuard = PTHREAD_MUTEX_INITIALIZER;
ThreadInfoMap *   gThreadInfoMap = 0;


class ThreadQuitRequestedError : public ::std::runtime_error {
    public:
        explicit ThreadQuitRequestedError( );

        ~ThreadQuitRequestedError( ) throw( );

        void markOkayToDestruct( );

    private:
        bool okayToDestruct_;
};


ThreadQuitRequestedError::ThreadQuitRequestedError( ) :
::std::runtime_error( "thread quit requested" ),
okayToDestruct_( false )
{
}


ThreadQuitRequestedError::~ThreadQuitRequestedError( ) throw ( )
try {
    if ( okayToDestruct_ == false )
        programLogErrorIfPossible( "okayToDestruct_ is false" );
} catch ( ... ) {
    // Just stifle the exception

    return;
}


void
ThreadQuitRequestedError::markOkayToDestruct( )
{
    okayToDestruct_ = true;
}


class ThreadQuitInfo {
    public:
        static void testSelf( );

        static void incrementRegistrationCountSelf( );
        static void decrementRegistrationCountSelf( );

        static void incrementDeferralLevelSelf( );
        static void decrementDeferralLevelSelf( );

        static ::size_t
        registerRequestHandlerSelf( ThreadQuitRequestHandler & tqrh );

        static void unregisterRequestHandlerSelf( ::size_t handlerCookie );

        static void request( ::pthread_t thread );

    private:
        // Disallow copying
        ThreadQuitInfo( const ThreadQuitInfo & rhs );
        ThreadQuitInfo & operator=( const ThreadQuitInfo & rhs );

        explicit ThreadQuitInfo( );

        /* virtual */ ~ThreadQuitInfo( );

        // IncrementalLevelType is ::size_t type because we probably can't have
        // more scoped increments on the call stack than available memory for
        // the stack itself.
        typedef ::size_t IncrementalLevelType;

        typedef vector< ThreadQuitRequestHandler * > TqrhPtrVector;

        PthreadMutex         guard_;
        IncrementalLevelType registrationCount_;
        bool                 requested_;

        PthreadMutex         tqrhAndDeferralGuard_;
        IncrementalLevelType deferralLevel_;
        TqrhPtrVector        tqrhPtrVec_;
};


void
ThreadQuitInfo::testSelf( )
{
    ThreadQuitInfo * const info = gTlsThreadQuitInfo;

    // It's okay to read/compare deferralLevel_ in here without locking
    // tqrhAndDeferralGuard_ because the only routines that modify the
    // value of deferralLevel_ are xxxSelf() calls from this same thread
    // that is presently calling this routine.
    if ( (info != 0) && (info->deferralLevel_ == 0) ) {
        const ScopedLock< PthreadMutex > lock( info->guard_ );

        if ( info->requested_ )
            throw ThreadQuitRequestedError();
    }
}


void
ThreadQuitInfo::incrementRegistrationCountSelf( )
{
    ThreadQuitInfo * const info = gTlsThreadQuitInfo;

    if ( info != 0 ) {
        // just need to increment the existing registration's count
        const ScopedLock< PthreadMutex > guardLock( info->guard_ );

        // It's okay to read/compare deferralLevel_ in here without locking
        // tqrhAndDeferralGuard_ because the only routines that modify the
        // value of deferralLevel_ are xxxSelf() calls from this same thread
        // that is presently calling this routine.
        if ( (info->deferralLevel_ == 0) && info->requested_ )
            throw ThreadQuitRequestedError();

        IncrementalLevelType newRegCount = info->registrationCount_;

        ++newRegCount;

        if ( newRegCount <= info->registrationCount_ )
            throw logic_error( "registration count would have overflowed" );

        info->registrationCount_ = newRegCount;
    } else {
        // need to add the registration to the global set
        ThreadQuitInfo * const newInfo = new ThreadQuitInfo;

        try {
            gTlsThreadQuitInfo = newInfo;

            try {
                const ThreadInfoMap::value_type myNewEntry =
                    make_pair( ::pthread_self(), newInfo );

                // Notice that I am NOT holding a lock on guard_ when I lock
                // gThreadInfoMapGuard.

                const ScopedLock< ::pthread_mutex_t >
                    mapGuardLock( gThreadInfoMapGuard );

                if ( gThreadInfoMap == 0 )
                    gThreadInfoMap = new ThreadInfoMap;

                gThreadInfoMap->insert( myNewEntry );
            } catch ( ... ) {
                gTlsThreadQuitInfo = 0;

                throw;
            }
        } catch ( ... ) {
            delete newInfo;

            throw;
        }
    }
}


void
ThreadQuitInfo::decrementRegistrationCountSelf( )
{
    ThreadQuitInfo * const info = gTlsThreadQuitInfo;

    if ( info == 0 )
        throw logic_error( "thread is not registered" );

    {
        ScopedLockManager< PthreadMutex > guardManager( info->guard_ );

        guardManager.lock();

        bool regCountWasBad = false;

        IncrementalLevelType newRegCount = info->registrationCount_;

        if ( newRegCount <= 0 )
            regCountWasBad = true;
        else
            --newRegCount;

        if ( newRegCount > 0 ) {
            // just need to decrement the existing registration's count
            info->registrationCount_ = newRegCount;

            guardManager.unlock();
        } else {
            // need to remove the registration from the global set

            // I need to be careful here with the locks here to avoid
            // deadlocks and still be correct. Remember that a thread trying
            // to request a quit of this thread will lock gThreadInfoMapGuard
            // and then lock this thread's guard_ and then release guard_
            // and then lock this thread's xxx_ and then release xxx_ and then
            // release gThreadInfoMapGuard.
            // Hence, first I release my lock on this thread's
            // guard_ so that I will not deadlock against a thread that trying
            // to request a quit of this thread.
            // Then, I lock gThreadInfoMapGuard and remove my instance from the
            // global set.
            // Finally, I do the delete. I know no other thread can be
            // holding/using a pointer to my info instance at this point
            // because to be holding/using such a pointer they would have to be
            // holding at least gThreadInfoMapGuard and have done a lookup in
            // the global set to get that pointer but I just insured that I
            // removed it from the global set while holding a lock on
            // gThreadInfoMapGuard.

            guardManager.unlock();

            const ::pthread_t myKey = ::pthread_self();

            {
                const ScopedLock< ::pthread_mutex_t >
                    mapGuardLock( gThreadInfoMapGuard );

                if ( gThreadInfoMap != 0 )
                    gThreadInfoMap->erase( myKey );
            }

            gTlsThreadQuitInfo = 0;

            delete info;
        }

        if ( regCountWasBad )
            throw logic_error( "registration count was already 0 or less" );
    }
}


void
ThreadQuitInfo::incrementDeferralLevelSelf( )
{
    ThreadQuitInfo * const info = gTlsThreadQuitInfo;

    if ( info != 0 ) {
        // It's okay to read/compare deferralLevel_ in here without locking
        // tqrhAndDeferralGuard_ because the only routines that modify the
        // value of deferralLevel_ are xxxSelf() calls from this same thread
        // that is presently calling this routine.
        if ( info->deferralLevel_ == 0 ) {
            const ScopedLock< PthreadMutex > guardLock( info->guard_ );

            if ( info->requested_ )
                throw ThreadQuitRequestedError();
        }

        IncrementalLevelType newDeferralLevel = info->deferralLevel_;

        ++newDeferralLevel;

        if ( newDeferralLevel <= info->deferralLevel_ )
            throw logic_error( "deferral level would have overflowed" );

        // Now I lock tqrhAndDeferralGuard_ to actually modify the value
        // of deferralLevel_
        {
            const ScopedLock< PthreadMutex >
                lock( info->tqrhAndDeferralGuard_ );

            info->deferralLevel_ = newDeferralLevel;
        }
    }
}


void
ThreadQuitInfo::decrementDeferralLevelSelf( )
{
    ThreadQuitInfo * const info = gTlsThreadQuitInfo;

    if ( info != 0 ) {
        // It's okay to read/compare deferralLevel_ in here without locking
        // tqrhAndDeferralGuard_ because the only routines that modify the
        // value of deferralLevel_ are xxxSelf() calls from this same thread
        // that is presently calling this routine.
        if ( info->deferralLevel_ <= 0 )
            throw logic_error( "deferral level was already 0 or less" );

        // Now I lock tqrhAndDeferralGuard_ to actually modify the value
        // of deferralLevel_
        {
            const ScopedLock< PthreadMutex >
                lock( info->tqrhAndDeferralGuard_ );

            --(info->deferralLevel_);
        }
    }
}


::size_t
ThreadQuitInfo::registerRequestHandlerSelf( ThreadQuitRequestHandler & tqrh )
{
    ThreadQuitInfo * const info = gTlsThreadQuitInfo;

    ::size_t handlerCookie = 0;

    if ( info != 0 ) {
        // It's okay to read/compare deferralLevel_ in here without locking
        // tqrhAndDeferralGuard_ because the only routines that modify the
        // value of deferralLevel_ are xxxSelf() calls from this same thread
        // that is presently calling this routine.
        if ( info->deferralLevel_ == 0 ) {
            const ScopedLock< PthreadMutex > lock( info->guard_ );

            if ( info->requested_ )
                throw ThreadQuitRequestedError();
        }

        {
            const ScopedLock< PthreadMutex >
                lock( info->tqrhAndDeferralGuard_ );

            info->tqrhPtrVec_.push_back( &tqrh );

            handlerCookie = info->tqrhPtrVec_.size();
        }
    }

    return handlerCookie;
}


void
ThreadQuitInfo::unregisterRequestHandlerSelf( const ::size_t handlerCookie )
{
    ThreadQuitInfo * const info = gTlsThreadQuitInfo;

    if ( info == 0 ) {
        if ( handlerCookie != 0 )
            throw logic_error( "thread is unregistered and handler cookie is not 0" );
    } else {
        const ScopedLock< PthreadMutex > lock( info->tqrhAndDeferralGuard_ );

        if ( handlerCookie == 0 )
            throw logic_error( "thread is registered and handler cookie is 0" );

        if ( handlerCookie != info->tqrhPtrVec_.size() )
            throw logic_error( "handler cookie does not point to last handler list entry" );

        info->tqrhPtrVec_.pop_back();
    }
}


void
ThreadQuitInfo::request( const ::pthread_t thread )
{
    if ( ::pthread_self() == thread )
        programLogWarnIfPossible( "Thread is trying to suicide" );

    // We hold gThreadInfoMapGuard for the duration to avoid the thread in
    // question from deleting it's ThreadQuitInfo instance out from under us.

    const ScopedLock< ::pthread_mutex_t > mapGuardLock( gThreadInfoMapGuard );

    if ( gThreadInfoMap == 0 )
        throw runtime_error( "thread is not registered" );

    ThreadQuitInfo * info = 0;

    {
        ThreadInfoMap::const_iterator i = gThreadInfoMap->find( thread );

        if ( i == gThreadInfoMap->end() )
            throw runtime_error( "thread is not registered" );

        info = i->second;
    }

    {
        // lock nesting gThreadInfoMapGuard -> info->guard_
        const ScopedLock< PthreadMutex > guardLock( info->guard_ );

        info->requested_ = true;
    }

    {
        // lock nesting gThreadInfoMapGuard -> info->tqrhAndDeferralGuard_
        const ScopedLock< PthreadMutex > lock( info->tqrhAndDeferralGuard_ );

        if ( info->deferralLevel_ == 0 ) {
            TqrhPtrVector::const_iterator j = info->tqrhPtrVec_.begin();
            const TqrhPtrVector::const_iterator jEnd = info->tqrhPtrVec_.end();

            for ( ; j != jEnd; ++j )
                (*j)->HandleQuitRequest( thread );
        }
    }
}


ThreadQuitInfo::ThreadQuitInfo( ) :
guard_(),
registrationCount_( 1 ),
requested_( false ),
tqrhAndDeferralGuard_(),
deferralLevel_( 0 ),
tqrhPtrVec_()
{
}


ThreadQuitInfo::~ThreadQuitInfo( )
try {
    if ( registrationCount_ != 1 )
        programLogErrorIfPossible( "registrationCount_ is not 1" );
} catch ( ... ) {
    // Just stfile the exception

    return;
}


}  // namespace < anonymous >


ScopedThreadQuitRegisterSelf::ScopedThreadQuitRegisterSelf( )
{
    ThreadQuitInfo::incrementRegistrationCountSelf();
}


ScopedThreadQuitRegisterSelf::~ScopedThreadQuitRegisterSelf( )
try {
    try {
        ThreadQuitInfo::decrementRegistrationCountSelf();
    } catch ( ... ) {
        programLogErrorIfPossible(
            "ThreadQuitInfo::decrementRegistrationCountSelf failure" );
    }
} catch ( ... ) {
    // Just stifle the exception

    return;
}


ScopedThreadQuitDeferSelf::ScopedThreadQuitDeferSelf( )
{
    ThreadQuitInfo::incrementDeferralLevelSelf();
}


ScopedThreadQuitDeferSelf::~ScopedThreadQuitDeferSelf( )
try {
    try {
        ThreadQuitInfo::decrementDeferralLevelSelf();
    } catch ( ... ) {
        programLogErrorIfPossible(
            "ThreadQuitInfo::decrementDeferralLevelSelf failure" );
    }
} catch ( ... ) {
    // Just stifle the exception

    return;
}


ScopedThreadQuitRequestHandlerSelf::ScopedThreadQuitRequestHandlerSelf(
    ThreadQuitRequestHandler & tqrh ) :
handlerCookie_( ThreadQuitInfo::registerRequestHandlerSelf( tqrh ) )
{
}


ScopedThreadQuitRequestHandlerSelf::~ScopedThreadQuitRequestHandlerSelf( )
try {
    ThreadQuitInfo::unregisterRequestHandlerSelf( handlerCookie_ );
} catch ( ... ) {
    // Just stifle the exception

    return;
}


void
carma::util::ThreadQuitTestSelf( )
{
    ThreadQuitInfo::testSelf();
}


void
carma::util::RequestThreadQuit( ::pthread_t thread )
{
    ThreadQuitInfo::request( thread );
}


bool
carma::util::CaughtExceptionIsThreadQuitRequestedError( )
{
    bool isQuit = false;

    try {
        throw;
    } catch ( const ThreadQuitRequestedError & ) {
        isQuit = true;
        // stifle our little throw
    } catch ( ... ) {
        // stifle our little throw
    }

    return isQuit;
}


void
carma::util::RethrowCaughtExceptionIfThreadQuitRequestedError( )
{
    if ( CaughtExceptionIsThreadQuitRequestedError() )
        throw;
}


bool
carma::util::MarkCaughtExceptionOkayToDestructIfThreadQuitRequestedError( )
{
    bool isQuit = false;

    try {
        throw;
    } catch ( ThreadQuitRequestedError & e ) {
        isQuit = true;

        try {
            e.markOkayToDestruct();
        } catch ( ... ) {
            // stifle any spurious exception
        }

        // stifle our little throw
    } catch ( ... ) {
        // stifle our little throw
    }

    return isQuit;
}
