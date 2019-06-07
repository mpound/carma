#include <iostream>
#include <stdexcept>
#include <vector>
#include <cerrno>

#include <pthread.h>
#include <unistd.h>
#include <sys/time.h>
#include <limits.h>

#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/PthreadRWLock.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedExclusiveLockManager.h"
#include "carma/util/ScopedSharedLock.h"
#include "carma/util/ThreadQuit.h"
#include "carma/util/Test/utils.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


char gProblemDetected = 0;


struct TqrhHellArgs {
    PthreadMutex clientGuard;

    explicit TqrhHellArgs( );
};


TqrhHellArgs::TqrhHellArgs( ) :
clientGuard( PTHREAD_MUTEX_ERRORCHECK )
{
}


class TqrhHell : public ThreadQuitRequestHandler {
    public:
        explicit TqrhHell( TqrhHellArgs & tqrhHellArgs );

        virtual void HandleQuitRequest( ::pthread_t thread );

    private:
        TqrhHellArgs & tqrhHellArgs_;
};


TqrhHell::TqrhHell( TqrhHellArgs & tqrhHellArgs ) :
tqrhHellArgs_( tqrhHellArgs )
{
}


void
TqrhHell::HandleQuitRequest( const ::pthread_t )
{
    {
        // This tests a particularly nasty potential livelock/deadlock in
        // the way the thread quit mechanism works.

        cout << "Locking client guard in quit request handler..." << endl;

        const ScopedLock< PthreadMutex > lock( tqrhHellArgs_.clientGuard );

        cout << "Client guard locked in quit request handler." << endl;
    }

    cout << "Quit request handler done" << endl;
}


void *
TqrhHellEntryPoint( void * arg )
{
    try {
        const ScopedThreadQuitRegisterSelf quitRegistration;

        TqrhHellArgs * const tqrhHellArgs =
            static_cast< TqrhHellArgs * >( arg );

        if ( tqrhHellArgs == 0 )
            throw runtime_error( "tqrhHellArgs was NULL" );

        TqrhHell tqrh1( *tqrhHellArgs );
        const ScopedThreadQuitRequestHandlerSelf tqrh1Install( tqrh1 );

        const ScopedLock< PthreadMutex > lock( tqrhHellArgs->clientGuard );

        TqrhHell tqrh2( *tqrhHellArgs );
        const ScopedThreadQuitRequestHandlerSelf tqrh2Install( tqrh2 );

        while ( true ) {
            ThreadQuitTestSelf();
        }
    } catch ( ... ) {
        if ( MarkCaughtExceptionOkayToDestructIfThreadQuitRequestedError() != true ) {
            gProblemDetected = -1;

            cerr << "ERROR: Stifling unexpected exception"
                 << " in TqrhHellEntryPoint - " << getStringForCaught()
                 << endl;
        }
    }

    return 0;
}


void
testTqrhHell( )
{
    cout << "Testing thread quit request handler hell..." << endl;

    TqrhHellArgs tqrhHellArgs;

    pthread_t thread;

    if ( ::pthread_create( &thread,
                           0,
                           TqrhHellEntryPoint,
                           &tqrhHellArgs ) != 0 )
        throw runtime_error( "::pthread_create error" );

    ::sleep( 2 );

    try {
        RequestThreadQuit( thread );
    } catch ( ... ) {
        gProblemDetected = -1;

        cerr << "ERROR: Exception thrown trying to request thread quit - "
             << getStringForCaught()  << endl;
    }

    void * threadResult = 0;

    if ( ::pthread_join( thread, &threadResult ) != 0 ) {
        const int savedErrno = errno;

        gProblemDetected = -1;

        cerr << "ERROR: pthread_join error " << savedErrno << " - "
             << strerror( savedErrno ) << endl;
    }

    cout << "Done testing thread quit request handler hell" << endl;
}


::pthread_rwlock_t gStartBarrier = PTHREAD_RWLOCK_INITIALIZER;


void
WorkLoop( ::size_t & quitTests )
{
    const ::size_t loopUnrollingSafetyMargin = 128;
    const ::size_t loopTermCount = (UINT_MAX - loopUnrollingSafetyMargin);

    quitTests = 0;

    ::size_t localCounter = 0;

    try {
        const ScopedSharedLock< ::pthread_rwlock_t >
            startSync( gStartBarrier );

        do {
            // Loop body was unrolled by hand to reduce the loop overhead

            ThreadQuitTestSelf();
            ++localCounter;

            ThreadQuitTestSelf();
            ++localCounter;

            ThreadQuitTestSelf();
            ++localCounter;

            ThreadQuitTestSelf();
            ++localCounter;

            ThreadQuitTestSelf();
            ++localCounter;

            ThreadQuitTestSelf();
            ++localCounter;

            ThreadQuitTestSelf();
            ++localCounter;

            ThreadQuitTestSelf();
            ++localCounter;
        } while ( localCounter < loopTermCount );
    } catch ( ... ) {
        quitTests = localCounter;

        throw;
    }

    quitTests = localCounter;
}


void *
RegisteredThreadEntryPoint( void * arg )
{
    ::size_t quitTests = 0;

    try {
        const ScopedThreadQuitRegisterSelf quitRegistration;

        WorkLoop( quitTests );
    } catch ( ... ) {
        if ( MarkCaughtExceptionOkayToDestructIfThreadQuitRequestedError() != true ) {
            gProblemDetected = -1;

            cerr << "ERROR: Stifling unexpected exception"
                 << " in RegisteredThreadEntryPoint - " << getStringForCaught()
                 << endl;
        }
    }

    return test::mankyConvertToVoidPtr( quitTests );
}


void *
UnregisteredThreadEntryPoint( void * arg )
{
    ::size_t quitTests = 0;

    try {
        WorkLoop( quitTests );
    } catch ( ... ) {
        gProblemDetected = -1;

        cerr << "ERROR: Stifling unexpected exception"
             << " in UnregisteredThreadEntryPoint - " << getStringForCaught()
             << endl;
    }

    return test::mankyConvertToVoidPtr( quitTests );
}


}  // namespace < anonymous >


//
// @key threads 5 int
//      Number of threads
//
//
// @key duration 10 int
//      Number of seconds to run
//
// @key tqrhHell false bool
//      Whether or not to try to cause a livelock/deadlock using the
//      ThreadQuitRequestHandler support.
//      At present this will livelock/deadlock the program in a most
//      unpleasant way.
//
// @logger TEST_FACILITY carma.test.util.tThreadQuit
//

int
Program::main( )
{
    const int threadsParam = getIntParameter( "threads" );
    const int durationParam = getIntParameter( "duration" );
    const bool tqrhHell = getBoolParameter( "tqrhHell" );

    const long numThreads = ::std::max( 0, threadsParam );
    const int durationSecs = ::std::max( 0, durationParam );
    const int stallSecs = 5;

    if ( tqrhHell )
        testTqrhHell();

    ScopedExclusiveLockManager< ::pthread_rwlock_t >
        startSyncManager( gStartBarrier );

    startSyncManager.lock();

    cout << "Creating " << numThreads << " threads..." << endl;

    vector< pthread_t > threads;
    threads.reserve( numThreads );

    for ( long i = 0; i < numThreads; ++i ) {
        pthread_t thread;

        if ( ::pthread_create( &thread,
                               0,
                               RegisteredThreadEntryPoint,
                               test::mankyConvertToVoidPtr( i ) ) != 0 )
            throw runtime_error( "::pthread_create error" );

        threads.push_back( thread );
    }

    vector< ::size_t > threadQuitTests;
    threadQuitTests.reserve( threads.size() );

    cout << "Stalling " << stallSecs
         << " seconds to let the threads get ready" << endl;

    ::sleep( stallSecs );

    cout << "Proceeding with " << threads.size() << " threads for about "
         << durationSecs << " seconds..." << endl;

    struct ::timeval startTime;

    ::gettimeofday( &startTime, 0 );

    startSyncManager.unlock();

    if ( durationSecs > 0 )
        ::sleep( durationSecs );

    {
        vector< pthread_t >::const_iterator i = threads.begin();
        const vector< pthread_t >::const_iterator iEnd = threads.end();

        for ( ; i != iEnd; ++i ) {
            try {
                RequestThreadQuit( *i );
            } catch ( ... ) {
                gProblemDetected = -1;

                cerr << "ERROR: Exception thrown requesting thread quit - "
                     << getStringForCaught() << endl;
            }
        }
    }

    {
        vector< pthread_t >::const_iterator i = threads.begin();
        const vector< pthread_t >::const_iterator iEnd = threads.end();

        for ( ; i != iEnd; ++i ) {
            void * threadResult = 0;

            if ( ::pthread_join( *i, &threadResult ) != 0 ) {
                const int savedErrno = errno;

                gProblemDetected = -1;

                cerr << "ERROR: pthread_join error " << savedErrno << " - "
                     << strerror( savedErrno ) << endl;
            }

            const ::size_t quitTests =
                test::mankyConvertFromVoidPtr< ::size_t >( threadResult );

            threadQuitTests.push_back( quitTests );
        }
    }

    struct ::timeval endTime;

    ::gettimeofday( &endTime, 0 );

    unsigned long long totalQuitTests = 0;

    {
        vector< ::size_t >::const_iterator i = threadQuitTests.begin();
        const vector< ::size_t >::const_iterator iEnd = threadQuitTests.end();

        for ( size_t index = 1; i != iEnd; ++i, ++index ) {
            const ::size_t quitTests = *i;

            cout << "Thread #" << index << " performed " << quitTests
                 << " quit tests." << endl;

            if ( (UINT_MAX - quitTests) <= 1000000 ) {
                gProblemDetected = -1;

                cerr << "ERROR: Thread #" << index
                     << " is bumping up against the UINT_MAX limit and"
                     << " this test code probably needs to be extended"
                     << " to use 64 bit counters." << endl;
            }

            totalQuitTests += quitTests;
        }
    }

    int32_t deltaMicros = endTime.tv_usec - startTime.tv_usec;
    int32_t deltaSecs = endTime.tv_sec - startTime.tv_sec;

    if ( deltaMicros < 0 ) {
        deltaSecs -= 1;
        deltaMicros += 1000000;
    }

    cout << totalQuitTests << " total quit tests in "
         << deltaSecs << " seconds and " << deltaMicros << " microseconds";

    if ( totalQuitTests != 0 ) {
        const double nanosPerTest =
            ((static_cast< double >( deltaSecs ) * 1.0e+9 +
              static_cast< double >( deltaMicros ) * 1.0e+3) /
             static_cast< double >( totalQuitTests ));

         cout << " (" << nanosPerTest << " nanoseconds/test)";
    }

    cout << "." << endl;

    if ( gProblemDetected != 0 ) {
        cerr << "ERROR: Problem(s) detected." << endl;

        return -1;
    } else {
        cout << "No problems detected." << endl;

        return 0;
    }
}
