#include "carma/util/Sleeper.h"

#include <cerrno>
#include <sstream>


#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ThreadQuit.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


const int kNanosPerSec = 1000 * 1000 * 1000;
const int kNanosPerMicro = 1000;

const int kMicrosPerSec = 1000 * 1000;


void
sanitizeAbsTimeval( struct ::timeval & absTv )
{
    if ( absTv.tv_usec >= kMicrosPerSec ) {
        const int wholeSecs = (absTv.tv_usec / kMicrosPerSec);

        absTv.tv_sec += wholeSecs;
        absTv.tv_usec -= (wholeSecs * kMicrosPerSec);
    } else if ( absTv.tv_usec < 0 ) {
        const long long absMicros =
            static_cast< long long >( absTv.tv_sec ) * kMicrosPerSec +
            static_cast< long long >( absTv.tv_usec );

        if ( absMicros < 0 )
            throw CARMA_ERROR( "absMicros is negative" );

        absTv.tv_sec = static_cast< ::time_t >( absMicros / kMicrosPerSec );

        absTv.tv_usec =
            static_cast< long >(
                absMicros -
                (static_cast< long long >( absTv.tv_sec ) * kMicrosPerSec) );
    }
}


class SleeperTQRH : public ThreadQuitRequestHandler {
    public:
        SleeperTQRH( Sleeper & sleeper );

        void HandleQuitRequest( ::pthread_t thread );

    private:
        Sleeper & sleeper_;
};


SleeperTQRH::SleeperTQRH( Sleeper & sleeper ) :
sleeper_( sleeper )
{
}


void
SleeperTQRH::HandleQuitRequest( ::pthread_t thread )
{
    sleeper_.interruptPresentOrNextWait();
}


}  // namespace < anonymous >


Sleeper::Sleeper( ) :
cond_(),
mutex_(),
interruptRequested_( false )
{
}


Sleeper::~Sleeper( )
try {
} catch ( ... ) {
    // Just stifle any exception

    return;
}


void
Sleeper::interruptPresentOrNextWait( )
{
    {
        const ScopedLock< PthreadMutex > lock( mutex_ );

        interruptRequested_ = true;
    }
    
    // At most one thread should be waiting so
    // Signal will work instead of Broadcast.
    cond_.Signal();
}


void
Sleeper::internalWaitUntilAbsTime( const struct ::timespec & absTime )
{
    bool interrupted = false;

    {
        SleeperTQRH sleeperTQRH( *this );
        const ScopedThreadQuitRequestHandlerSelf tqrhInstall( sleeperTQRH );

        const ScopedLock< PthreadMutex > lock( mutex_ );

        bool timedOut = false;

        while ( (interruptRequested_ == false) && (timedOut == false) )
            timedOut = (cond_.TimedWait( mutex_, absTime ) == false);

        if ( interruptRequested_ ) {
            interrupted = true;
            interruptRequested_ = false;
        } else if ( timedOut != true )
            programLogError( "timedOut is not true" );
    }

    if ( interrupted ) {
        ThreadQuitTestSelf();

        throw CARMA_ERROR( "sleeper wait was interrupted" );
    }
}


void
Sleeper::waitUntilAbsTime( const struct ::timespec & absTime )
{
    if ( (absTime.tv_nsec >= 0) && (absTime.tv_nsec < kNanosPerSec) )
        internalWaitUntilAbsTime( absTime );
    else {
        const long long absNanos =
            static_cast< long long >( absTime.tv_sec ) * kNanosPerSec +
            static_cast< long long >( absTime.tv_nsec );

        if ( absNanos < 0 )
            throw CARMA_ERROR( "absNanos is negative" );
    
        struct ::timespec sanitizedAbsTime;

        sanitizedAbsTime.tv_sec =
            static_cast< ::time_t >( absNanos / kNanosPerSec );

        sanitizedAbsTime.tv_nsec =
            static_cast< long >(
                absNanos -
                (static_cast< long long >( sanitizedAbsTime.tv_sec ) *
                 kNanosPerSec) );
    
        internalWaitUntilAbsTime( sanitizedAbsTime );
    }
}


void
Sleeper::waitUntilAbsTime( const struct ::timeval & absTv )
{
    struct ::timeval sanitizedAbsTv = absTv;
    sanitizeAbsTimeval( sanitizedAbsTv );
    
    struct ::timespec absTime;
    
    absTime.tv_sec = sanitizedAbsTv.tv_sec;
    absTime.tv_nsec = kNanosPerMicro * sanitizedAbsTv.tv_usec;
    
    internalWaitUntilAbsTime( absTime );
}


void
Sleeper::waitForWholeSecDuration( const int durationSecs )
{
    struct ::timeval sanitizedNow;

    ::gettimeofday( &sanitizedNow, 0 );
    sanitizeAbsTimeval( sanitizedNow );

    const long long bigSecs =
        static_cast< long long >( sanitizedNow.tv_sec ) + durationSecs;

    if ( bigSecs < 0 )
        throw CARMA_ERROR( "bigSecs is negative" );

    const ::time_t clippedSecs = static_cast< ::time_t >( bigSecs );

    if ( static_cast< long long >( clippedSecs ) != bigSecs )
        throw CARMA_ERROR( "bigSecs not representable in the time_t type" );
        
    struct ::timespec absTime;
    
    absTime.tv_sec = clippedSecs;
    absTime.tv_nsec = kNanosPerMicro * sanitizedNow.tv_usec;
    
    internalWaitUntilAbsTime( absTime );
}
