#include "carma/util/ScheduledTimer.h"

#include <cerrno>
#include <sstream>

#include <sys/time.h>

#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ThreadQuit.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


typedef long long BigSignedNanos;


const long kNanosPerSec = 1000L * 1000L * 1000L;


class InterruptQuitRequestHandler : public ThreadQuitRequestHandler {
    public:
        InterruptQuitRequestHandler( ScheduledTimer & timer );
        
        void HandleQuitRequest( ::pthread_t thread );
        
    private:
        ScheduledTimer & timer_;
};


InterruptQuitRequestHandler::InterruptQuitRequestHandler( ScheduledTimer & timer ) :
timer_( timer )
{
}


void
InterruptQuitRequestHandler::HandleQuitRequest( ::pthread_t thread )
{
    timer_.InterruptPresentOrNextWait();
}


}  // namespace < anonymous >


void
ScheduledTimer::sanityCheckTimespec( const Timespec & ts )
{
    if ( (ts.tv_nsec < 0) || (ts.tv_nsec >= kNanosPerSec) ) {
        ostringstream oss;
        
        oss << "timespec tv_nsec field value " << ts.tv_nsec
            << " is outside of nice range [0, "
            << kNanosPerSec << ")";
            
        programLogErrorIfPossible( oss.str() );
    }
}


ScheduledTimer::ScheduledTimer( const bool preflight ) :
fireAbsTimeState_( FIRE_ABS_TIME_STATE_INVALID ),
interruptRequested_( false )
{
    if ( preflight ) {
        struct ::timeval now;
        
        ::gettimeofday( &now, 0 );
        
        const BigSignedNanos absBigSignedNanos =
            static_cast< BigSignedNanos >( now.tv_sec ) * kNanosPerSec + 
            static_cast< BigSignedNanos >( now.tv_usec ) * 1000LL +
            1000LL * 1000LL;
            
        Timespec ts;

        ts.tv_sec =
            static_cast< ::time_t >( absBigSignedNanos / kNanosPerSec );
            
        ts.tv_nsec = 
            static_cast< long >(
                absBigSignedNanos -
                static_cast< BigSignedNanos >( ts.tv_sec ) * kNanosPerSec );

        sanityCheckTimespec( ts );

        {
            const ScopedLock< PthreadMutex > lock( mutex_ );
        
            cond_.TimedWait( mutex_, ts );
        }
    }
}


ScheduledTimer::~ScheduledTimer( )
try {
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


void
ScheduledTimer::SetNextFireTime( const Timespec & nextFireAbsTime )
{
    sanityCheckTimespec( nextFireAbsTime );

    fireAbsTime_ = nextFireAbsTime;
    fireAbsTimeState_ = FIRE_ABS_TIME_STATE_NEXT;
}


void
ScheduledTimer::SetNextFireTimeAndWait( const Timespec & nextFireAbsTime )
{
    SetNextFireTime( nextFireAbsTime );
    WaitForNextFireTime();
}


void
ScheduledTimer::refreshNextFireAbsTimeIfNeeded(
    Timespec * const nextFireAbsTime )
{
    if ( fireAbsTimeState_ != FIRE_ABS_TIME_STATE_NEXT ) {
        if ( fireAbsTimeState_ != FIRE_ABS_TIME_STATE_LAST )
            throw CARMA_ERROR( "scheduled timer fire absolute time state is not last" );
            
        sanityCheckTimespec( fireAbsTime_ );

        SetNextFireTime( CalculateNextFireAbsoluteTime( fireAbsTime_ ) );

        if ( fireAbsTimeState_ != FIRE_ABS_TIME_STATE_NEXT )
            programLogError( "fireAbsTimeState_ is not FIRE_ABS_TIME_STATE_NEXT" );
    }
    
    if ( nextFireAbsTime != 0 )
        *nextFireAbsTime = fireAbsTime_;
}


void
ScheduledTimer::WaitForNextFireTime( )
{
    refreshNextFireAbsTimeIfNeeded( 0 );

    sanityCheckTimespec( fireAbsTime_ );

    bool interrupted = false;
    
    {
        InterruptQuitRequestHandler handler( *this );
        const ScopedThreadQuitRequestHandlerSelf handlerInstall( handler );
        
        const ScopedLock< PthreadMutex > lock( mutex_ );
        
        bool timedOut = false;
        
        while ( (interruptRequested_ == false) && (timedOut == false) )
            timedOut = (cond_.TimedWait( mutex_, fireAbsTime_ ) == false);
            
        if ( interruptRequested_ ) {
            interrupted = true;
            interruptRequested_ = false;
        } else if ( timedOut != true )
            programLogError( "timedOut is not true" );
    }
    
    if ( interrupted ) {
        ThreadQuitTestSelf();

        throw CARMA_ERROR( "timer wait was interrupted" );
    }
    
    fireAbsTimeState_ = FIRE_ABS_TIME_STATE_LAST;
}


void
ScheduledTimer::InterruptPresentOrNextWait( )
{
    {
        const ScopedLock< PthreadMutex > lock( mutex_ );

        interruptRequested_ = true;
    }
    
    // At most one thread should be waiting so
    // Signal will work instead of Broadcast.
    cond_.Signal();
}
