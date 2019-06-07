#ifndef CARMA_UTIL_SCOPEDSTOPWATCHTIMER_H
#define CARMA_UTIL_SCOPEDSTOPWATCHTIMER_H

namespace carma {
namespace util {

class StopWatch;

/** 
 * Starts and stops attached stopwatch via ctor & dtor respectively.
 */
class ScopedStopWatchTimer {
public:

    explicit ScopedStopWatchTimer( carma::util::StopWatch & stopwatch );

    virtual ~ScopedStopWatchTimer( );

private:

    carma::util::StopWatch & stopWatch_;

}; // class ScopedStopWatchTimer

}} // namespace carma::util;
#endif
