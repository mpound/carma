#ifndef CARMA_UTIL_FRAME_ALIGNED_TIMER_H
#define CARMA_UTIL_FRAME_ALIGNED_TIMER_H

#include <ctime>

#include <sys/time.h>

#include "carma/util/ScheduledTimer.h"

namespace carma {
namespace util {

//! Timer object that fires at a fixed offset after every <I>N<SUP>th</SUP></I>
//!       CARMA frame time.
//!
//! @warning An instance is NOT ready to wait on simply by being constructed.
//!          You MUST reset an instance's next fire time before (or at the
//!          same time as) the first wait on an instance.

class FrameAlignedTimer : public ScheduledTimer {
    public:

        //! Construct an instance with the given offset and period.
        //! If any parameters are invalid or an internal error occurs then an
        //! exception will be thrown.
        //!
        //! @param offsetNanos
        //!        Offset in nanoseconds from the CARMA frame time to the fire
        //!        time for the instance. The value must be greater than 0 and
        //!        less than the CARMA frame period in nanoseconds.
        //!
        //! @param periodFrames
        //!        How often (in integral CARMA frame periods) the instance
        //!        will fire. The value must be greater than 0.
        //!
        //! @param preflight
        //!        If true then the instance will be preflighted as part of
        //!        construction. Preflighting involves actually performing
        //!        a very quick wait and/or test on the internal primitives
        //!        used to implement the instance. This MAY reduce any
        //!        increased one-time latency and/or drift in the first wait
        //!        on the instance (on some operating systems).
        //!
        //! @pre @p offsetNanos is greater than 0 and less than the
        //!         CARMA frame period in nanoseconds.
        //!
        //! @pre @p periodFrames is greater than 0.

        explicit FrameAlignedTimer( long offsetNanos = 0,
                                    long periodFrames = 1,
                                    bool preflight = true );


        //! Reset the next fire time (and by association the entire queue of
        //! fire times) to the soonest absolute time that is the correct offset
        //! from a CARMA frame time and also is at least @p delayFrames CARMA
        //! frames after the time of the call and then return that new next fire
        //! absolute time.
        //! All old fire times are removed from the queue as though they never
        //! existed.
        //!
        //! @param delayFrames
        //!        minimum number of CARMA frames to delay the next fire time
        //!        after the time of the call.
        //!
        //! @pre @p delayFrames is greater than or equal to 0.
        //!
        //! @return new next absolute fire time.

        struct ::timespec ResetNextFireTime( long delayFrames = 0 );


        //! Same as ResetNextFireTime( ) except that this method also waits for
        //! the new next fire time to arrive before returning the new next fire
        //! time that it has just waited for.
        //!
        //! @post Either absolute time is greater than or equal to the fire
        //!       time returned or an exception has been thrown.

        struct ::timespec ResetNextFireTimeAndWait( long delayFrames = 0 );

        struct ::timespec getNextFireTime( );

    protected:
        virtual Timespec
        CalculateNextFireAbsoluteTime( const Timespec & fireAbsTime );

        Timespec InternalResetNextFireTime( long delayFrames,
                                            bool wait );

    private:
        const Timespec period_;
        const long     offsetNanos_;
};

}  // namespace util
}  // namespace carma

#endif
