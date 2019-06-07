#ifndef CARMA_UTIL_STOPWATCH_H
#define CARMA_UTIL_STOPWATCH_H

/**
 * @file StopWatch.h
 * @author Dave Mehringer
 * @version $id$
 * $CarmaCopyright$
 */

#include <string>
#include <time.h>

namespace carma {
namespace util {

/**
 * Class for determining the elapsed wall-clock or CPU time between two 
 * events. Precision of a few clock ticks for a single start()/stop() 
 * event pair on a lightly loaded machine
 */
class StopWatch {
public:
     /** 
      * enumeration for clock type
      */
    enum ClockType {
      WALL_CLOCK = 0,
      CPU_TIME   = 1
    };

    /**
     * constructor
     */
    explicit StopWatch( );

    /**
     * constructor
     */
    explicit StopWatch( ClockType type );

    /**
     * constructor
     */
    explicit StopWatch( const ::std::string & name );

    /**
     * constructor
     */
    StopWatch( ClockType             type,
               const ::std::string & name );

    /**
     * destructor, does nothing
     */
    ~StopWatch();

    /**
     * start the watch
     * @throws IllegalStateException if the watch is already running
     */
    void start();

    /**
     * stop the watch
     * @throws IllegalStateException if the watch is not running
     */
    void stop();
    
    /**
     * is the watch running?
     */
    bool isRunning() const;

    /**
     * get the time interval in seconds between the last start() and stop()
     */
    double getElapsedTime() const;

    /**
     * get the the sum of the time intervals between each start and stop pair
     */
    double getCumulativeElapsedTime(bool reset=false);

protected:
    const ClockType     type_;
    const ::std::string name_;
    
    double startTime_;
    double elapsedTime_;
    double cumulativeElapsedTime_;
    bool running_;
    struct timespec scratchTimeSpec_;
};

} // end namespace util
} // end namespace carma

#endif //CARMA_UTIL_STOPWATCH_H
