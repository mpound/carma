#include "carma/util/PeriodicTimer.h"

#include "carma/util/programLogging.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


typedef struct ::timeval Timeval;


const long kNanosPerSecond = 1000L * 1000L * 1000L;


struct ::timespec
convertTimevalToTimespec( const Timeval & tv )
{
    struct ::timespec result;
    
    result.tv_sec = tv.tv_sec;
    result.tv_nsec = tv.tv_usec * 1000L; 
    
    return result;
}


}  // namespace < anonymous >


PeriodicTimer::PeriodicTimer( const Timespec & period,
                              const bool       preflight ) :
ScheduledTimer( preflight ),
period_( period )
{
}


PeriodicTimer::PeriodicTimer( const Timeval & period,
                              const bool      preflight ) :
ScheduledTimer( preflight ),
period_( convertTimevalToTimespec( period ) )
{
}


void
PeriodicTimer::ResetNextFireAbsoluteTime( const Timespec & nextFireAbsTime )
{
    SetNextFireTime( nextFireAbsTime );
}


void
PeriodicTimer::ResetNextFireAbsoluteTimeAndWait( const Timespec & nextFireAbsTime )
{
    SetNextFireTimeAndWait( nextFireAbsTime );
}


void
PeriodicTimer::ResetNextFireAbsoluteTime( const Timeval & nextFireAbsTime )
{
    SetNextFireTime( convertTimevalToTimespec( nextFireAbsTime ) );
}


void
PeriodicTimer::ResetNextFireAbsoluteTimeAndWait( const Timeval & nextFireAbsTime )
{
    SetNextFireTimeAndWait( convertTimevalToTimespec( nextFireAbsTime ) );
}


PeriodicTimer::Timespec
PeriodicTimer::CalculateNextFireAbsoluteTime( const Timespec & fireAbsTime )
{
    sanityCheckTimespec( fireAbsTime );
    sanityCheckTimespec( period_ );

    Timespec result;

    result.tv_sec = fireAbsTime.tv_sec + period_.tv_sec;
    result.tv_nsec = fireAbsTime.tv_nsec + period_.tv_nsec;

    if ( result.tv_nsec >= kNanosPerSecond ) {
        result.tv_nsec -= kNanosPerSecond;
        ++(result.tv_sec);      
    }
    
    sanityCheckTimespec( result );

    if ( result.tv_sec < fireAbsTime.tv_sec )
        programLogErrorIfPossible( "result.tv_sec < fireAbsTime.tv_sec" );

    return result;
}
