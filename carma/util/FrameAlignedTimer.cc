#include "carma/util/FrameAlignedTimer.h"

#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


const long kNanosPerHalfSecond = 500L * 1000L * 1000L;
const long kNanosPerSecond = 2L * kNanosPerHalfSecond;


struct ::timespec
convertFramesToPeriod( const long periodFrames )
{
    struct ::timespec result;

    if ( periodFrames < 1 )
        throw CARMA_ERROR( "frame aligned timer period is less than 1" );

    result.tv_sec = (periodFrames / 2);
    result.tv_nsec = (periodFrames % 2) * kNanosPerHalfSecond;

    return result;
}


void
checkTimespecAlignment( const struct ::timespec & ts,
                        const long                offsetNanos )
{
    if ( (ts.tv_nsec != offsetNanos) &&
         (ts.tv_nsec != (kNanosPerHalfSecond + offsetNanos)) )
        programLogErrorIfPossible( "Timespec is not properly aligned" );
}


}  // namespace < anonymous >


FrameAlignedTimer::FrameAlignedTimer( const long offsetNanos,
                                      const long periodFrames,
                                      const bool preflight ) :
ScheduledTimer( preflight ),
period_( convertFramesToPeriod( periodFrames ) ),
offsetNanos_( offsetNanos )
{
    if ( period_.tv_sec < 0 )
        programLogErrorIfPossible( "period_ is negative" );
    else if ( (period_.tv_sec == 0) && (period_.tv_nsec == 0) )
        programLogErrorIfPossible( "period_ is zero" );

    sanityCheckTimespec( period_ );
    checkTimespecAlignment( period_, 0 );

    if ( offsetNanos_ < 0 )
        throw CARMA_ERROR( "frame aligned timer offset is negative" );
    else if ( offsetNanos_ >= kNanosPerHalfSecond )
        throw CARMA_ERROR( "frame aligned timer offset is greater than or equal to a half second" );

}


FrameAlignedTimer::Timespec
FrameAlignedTimer::InternalResetNextFireTime( const long delayFrames,
                                              const bool wait )
{
    if ( delayFrames < 0 )
        throw CARMA_ERROR( "delay frames is negative" );

    struct ::timeval actualAbsTime;

    ::gettimeofday( &actualAbsTime, 0 );

    const long actualNanos = (actualAbsTime.tv_usec * 1000L) + 999L;

    if ( (actualNanos < 0) || (actualNanos >= kNanosPerSecond) )
        programLogErrorIfPossible( "bad actualNanos" );

    Timespec nextFireAbsTime;

    if ( actualNanos < offsetNanos_ ) {
        nextFireAbsTime.tv_sec = actualAbsTime.tv_sec;
        nextFireAbsTime.tv_nsec = offsetNanos_;
    } else if ( actualNanos < (kNanosPerHalfSecond + offsetNanos_) ) {
        nextFireAbsTime.tv_sec = actualAbsTime.tv_sec;
        nextFireAbsTime.tv_nsec = kNanosPerHalfSecond + offsetNanos_;
    } else {
        nextFireAbsTime.tv_sec = actualAbsTime.tv_sec + 1;
        nextFireAbsTime.tv_nsec = offsetNanos_;
    }

    checkTimespecAlignment( nextFireAbsTime, offsetNanos_ );

    nextFireAbsTime.tv_sec += (delayFrames / 2);
    nextFireAbsTime.tv_nsec += ((delayFrames % 2) * kNanosPerHalfSecond);
    
    if ( nextFireAbsTime.tv_nsec >= kNanosPerSecond ) {
        ++(nextFireAbsTime.tv_sec);
        nextFireAbsTime.tv_nsec -= kNanosPerSecond;
    }

    checkTimespecAlignment( nextFireAbsTime, offsetNanos_ );

    if ( wait )
        SetNextFireTimeAndWait( nextFireAbsTime );
    else
        SetNextFireTime( nextFireAbsTime );

    return nextFireAbsTime;
}


FrameAlignedTimer::Timespec
FrameAlignedTimer::ResetNextFireTime( const long delayFrames )
{
    return InternalResetNextFireTime( delayFrames, false );
}


FrameAlignedTimer::Timespec
FrameAlignedTimer::ResetNextFireTimeAndWait( const long delayFrames )
{
    return InternalResetNextFireTime( delayFrames, true );
}


FrameAlignedTimer::Timespec
FrameAlignedTimer::CalculateNextFireAbsoluteTime( const Timespec & fireAbsTime )
{
    sanityCheckTimespec( fireAbsTime );
    checkTimespecAlignment( fireAbsTime, offsetNanos_ );

    Timespec result;

    result.tv_sec = fireAbsTime.tv_sec + period_.tv_sec;
    result.tv_nsec = fireAbsTime.tv_nsec + period_.tv_nsec;

    if ( result.tv_nsec >= kNanosPerSecond ) {
        ++(result.tv_sec);
        result.tv_nsec -= kNanosPerSecond;
    }

    if ( result.tv_sec < fireAbsTime.tv_sec )
        programLogErrorIfPossible( "result.tv_sec < fireAbsTime.tv_sec" );

    sanityCheckTimespec( result );
    checkTimespecAlignment( result, offsetNanos_ );

    return result;
}


struct ::timespec
FrameAlignedTimer::getNextFireTime( )
{
    struct ::timespec nextFireAbsTime;
    
    refreshNextFireAbsTimeIfNeeded( &nextFireAbsTime );
    
    return nextFireAbsTime;
}
