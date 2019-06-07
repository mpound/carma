#ifndef CARMA_UTIL_PERIODIC_TIMER_H
#define CARMA_UTIL_PERIODIC_TIMER_H

#include <ctime>

#include <sys/time.h>

#include "carma/util/ScheduledTimer.h"

namespace carma {
namespace util {

//! Timer object that fires at a fixed rate.
//!
//! @warning An instance is NOT ready to wait on simply by being constructed.
//!          You MUST reset an instance's next fire time before (or at the
//!          same time as) the first wait on an instance.
//!
//! @note If you want a timer that fires at a fixed offset after every
//!       <I>N<SUP>th</SUP></I> CARMA frame time then consider using an
//!       instance of FrameAlignedTimer instead.

class PeriodicTimer : public ScheduledTimer {
    public:

        //! Construct an instance with the given period.
        //! If any parameters are invalid or an internal error occurs then an
        //! exception will be thrown.
        //!
        //! @param period
        //!        How often in absolute time this instance will fire. The
        //!        value must be greater than 0.
        //!
        //! @param preflight
        //!        If true then the instance will be preflighted as part of
        //!        construction. Preflighting involves actually performing
        //!        a very quick wait and/or test on the internal primitives
        //!        used to implement the instance. This MAY reduce any
        //!        increased one-time latency and/or drift in the first wait
        //!        on the instance (on some operating systems).
        //!
        //! @pre @p period is greater than 0.
        
        explicit PeriodicTimer( const struct ::timespec & period,
                                bool                      preflight = true );
                                

        //! Same as #PeriodicTimer( const struct ::timespec &, bool ) except
        //! this constructor takes a struct ::timeval for the @p period
        //! parameter.

        explicit PeriodicTimer( const struct ::timeval & period,
                                bool                     preflight = true );
        

        //! Reset the next fire time (and by association the entire queue of
        //! fire times) for this instance. All old fire times for this
        //! instance are removed from the queue as though they never existed.
        //!
        //! @param nextFireAbsoluteTime
        //!        absolute time for the new next fire time for this instance.

        void ResetNextFireAbsoluteTime( const struct ::timespec & nextFireAbsoluteTime );


        //! Same as ResetNextFireAbsoluteTime( const struct ::timespec & )
        //! except that this method takes a struct ::timeval for the 
        //! @p nextFireAbsoluteTime parameter.

        void ResetNextFireAbsoluteTime( const struct ::timeval & nextFireAbsoluteTime );
        
        
        //! Same as ResetNextFireAbsoluteTime( const struct ::timespec & )
        //! except that this method also waits for the new next fire time to
        //! arrive before returning the new next fire time that it has just
        //! waited for.
        //!
        //! @post Either absolute time is greater than or equal to the fire
        //!       time returned or an exception has been thrown.

        void ResetNextFireAbsoluteTimeAndWait( const struct ::timespec & nextFireAbsoluteTime );


        //! Same as ResetNextFireAbsoluteTimeAndWait( const struct ::timespec & )
        //! except that this method takes a struct ::timeval for the
        //! @p nextFireAbsoluteTime parameter.

        void ResetNextFireAbsoluteTimeAndWait( const struct ::timeval & nextFireAbsoluteTime );
        
    protected:
        virtual struct ::timespec CalculateNextFireAbsoluteTime( const struct ::timespec & fireAbsoluteTime );
        
    private:
        const struct ::timespec period_;
};

}  // namespace util
}  // namespace carma

#endif
