#ifndef TIMEVAL_H
#define TIMEVAL_H

/**
 * @file TimeVal.h
 * 
 * Tagged: Fri Nov 14 12:39:38 UTC 2003
 * 
 * @author Erik Leitch
 */
#include <time.h>
#include <sys/time.h>
#include <iostream>

namespace sza {
  namespace util {
    
    /**
     * A class for managing timeval/timespec structs.  
     *
     * These are defined in sys/time.h as:
     *
     *    struct timeval {                    
     *       long tv_sec; 
     *       long tv_usec;
     *    }               
     *                      and
     *                      
     *    struct timespec {                    
     *       long tv_sec; 
     *       long tv_nsec;
     *    }               
     *
     * struct timeval (with microsecond resolution) is used by
     * functions like select() as a timeout specifier, while
     * struct timespec (with nanosecond resolution) is used by
     * functions like clock_gettime().  Many system functions
     * which require a time specification user one or the other
     * in an irritatingly inconsistent fashion.
     *
     * This class is intended as a meta-time specifier, which
     * stores a time with nanosecond granularity, and can present
     * either a timeval or timespec face to the world, via the
     * timeVal() and timeSpec() methods below.
     *
     * Note that functions like select() can modify their timeval
     * arguments, while most uses of timespec are as a static
     * container.  The ***Elapsed() methods below reflect the
     * mutability of a timeval struct when used as a timeout
     * argument to select(), which decrements the timeval struct
     * to reflect elapsed time.
     */
    class TimeVal {
    public:
      
      /**
       * Constructors with no initialization.
       */
      TimeVal();
      
      /**
       * Constructors with initialization.
       *
       * We make this one have three arguments, otherwise there's
       * no way of distinguishing a constructor with seconds and
       * microseconds from a constructor with seconds and
       * nanoseconds.
       *
       * Although the internal time representation is always kept
       * either in seconds and microseconds (struct timeval) or
       * seconds and nanoseconds (struct timespec), the time set
       * in the constructor will be the addition of all three
       * arguments.
       */
      TimeVal(unsigned long seconds, unsigned long microSeconds, 
	      unsigned long nanoSeconds);
      
      /**
       * Constructor with seconds and nanoseconds.
       */
      TimeVal(unsigned long seconds, unsigned long nanoSeconds);
      
      /**
       * Initialize from a timeval struct.
       */
      TimeVal(const struct timeval& tVal);
      
      /**
       * Initialize from a timespec struct.
       */
      TimeVal(const struct timespec& timeSpec);
      
      /**
       * Initialize from a double MJD
       */
      TimeVal(double mjd);

      //------------------------------------------------------------
      // Methods for seting the time.
      //------------------------------------------------------------
      
#define HAVE_RT 1

#if HAVE_RT
#else
      enum clockid_t {
	CLOCK_REALTIME,
	CLOCK_HIGHRES
      };
#endif

      /**
       * Fill this structure with the current time from the
       * specified clock.  Supported clocks include at least:
       * 
       * CLOCK_REALTIME -- the realtime clock for the system,
       *                   relative to an Epoch (specified where?)
       * 
       * CLOCK_HIGHRES  -- the high-resolution non-adjustable clock, 
       *                   relative to some arbitrary time in the past.
       */
      void setToCurrentTime(clockid_t clock = CLOCK_REALTIME);
      
      /**
       * Set the time, as an MJD.
       */
      void setMjd(unsigned long days, unsigned long seconds, 
		  unsigned long nanoSeconds);
      
      /**
       * Set the time, as an MJD.
       */
      void setMjd(unsigned long days, unsigned long milliSeconds);
      
      /**
       * Set the time, as a double MJD.
       */
      void setMjd(double mjd);
      
      /**
       * Set the time.  For the MJD representation, passed times
       * will be interpreted as time since the Epoch.
       */
      void setTime(unsigned long seconds, unsigned long microSeconds, 
		   unsigned long nanoSeconds);
      
      /**
       * Set the time.
       */
      void setTime(unsigned long seconds, unsigned long nanoSeconds);
      
      /**
       * Set the time with a timespec struct.
       */
      void setTime(const struct timespec& timeSpec);
      
      /**
       * Set the time with a timeval struct.
       */
      void setTime(const struct timeval& tVal);
      
      //------------------------------------------------------------
      // Methods to modify the time.
      //------------------------------------------------------------
      
      /**
       * Increment the time by fractional seconds.
       */	
      void incrementSeconds(double seconds);

      /**
       * Increment the time by nanoseconds.
       */	
      void incrementNanoSeconds(unsigned nanoSeconds);
      
      /**
       * Reset our internal timeval struct to stored values.
       */
      void reset();
      
      /**
       * Return a pointer to our internal timeval struct.
       */
      struct timeval* timeVal();
      
      /**
       * Return a pointer to our internal timespec struct.
       */
      struct timespec* timeSpec();
      
      /**
       * Add two TimeVal objects
       */
      const TimeVal operator+(const TimeVal& tVal);
      
      /**
       * Subtract two TimeVal objects
       */
      const TimeVal operator-(const TimeVal& tVal);
      
      //------------------------------------------------------------
      // Methods returning the time.
      //------------------------------------------------------------
      
      /**
       * Return the total time in MJD days.
       */
      double getTimeInMjdDays();
      
      /**
       * Return the time in our timeval struct as fractional seconds.
       */
      double getTimeInSeconds();
      
      /**
       * Return the total time in milliseconds.
       */
      double getTimeInMilliSeconds();

      /**
       * Return the total time in microseconds.
       */
      double getTimeInMicroSeconds();
      
      /**
       * Return the total time as integer nanoseconds.
       */
      unsigned long getTimeInNanoSeconds();
      
      //------------------------------------------------------------
      // Methods returning parts of the time.
      //------------------------------------------------------------
      
      /**
       * Return just the fractional seconds, as a double
       */
      double getFractionalTimeInSeconds();
      
      /**
       * Return just the integer seconds
       */
      unsigned long getSeconds();
      
      /**
       * Return just the integer micro seconds
       */
      unsigned long getMicroSeconds();
      
      /**
       * Return just the integer nano seconds
       */
      unsigned long getNanoSeconds();
      
      /**
       * Return the mjd day number corresponding to this time.
       */
      unsigned long getMjdDays();
      
      /**
       * Return the mjd seconds corresponding to this time.
       */
      unsigned long getMjdSeconds();
      
      /**
       * Return the mjd milliseconds corresponding to this time.
       */
      unsigned long getMjdMilliSeconds();
      
      /**
       * Return the mjd nanoseconds corresponding to this time.
       */
      unsigned long getMjdNanoSeconds();
      
      /**
       * Return the complete mjd day as a double 
       */
      double getMjd();

      /**
       * Return the feactional part only of the mjd day as a double 
       */
      double getFractionalMjdDay();

      /**
       * Get a unique identifier based on the MJD.  This will return
       * the integral number of nanoSecondIntervals corresponding to
       * this MJD.
       */
      unsigned int getMjdId(unsigned nanoSecondInterval);

      //------------------------------------------------------------
      // When our timespec struct is used as a countdown timer, it
      // can be modified to reflect the elapsed time.  The following
      // methods return the elapsed time in different
      // representations.
      //------------------------------------------------------------
      
      /**
       * Return the time elapsed as fractional seconds.  Note that
       * the ***Elapsed() methods only apply to our timeval struct,
       * since a timespec struct is not used as a countdown timer.
       */
      double getElapsedTimeInSeconds();
      
      /**
       * Return the time in our timeVal struct as integer microseconds.
       */
      double getElapsedTimeInMicroSeconds();
      
      /**
       * Return the time in our timeVal struct as integer nanoseconds.
       */
      unsigned long getElapsedTimeInNanoSeconds();
      

      //------------------------------------------------------------
      // Print methods
      //------------------------------------------------------------      

      std::string getUtcString();

      std::string dateString();

      /**
       * Allows cout << timeVal
       */
      friend std::ostream& operator<<(std::ostream& os, TimeVal& tVal);

    private:
      
      // The following are all equivalent representations of a
      // single time stamp.
      
      // timeval stores a time as seconds and microseconds.
      
      struct timeval timeVal_;
      
      // timespec stores a time as seconds and nanoseconds.
      
      struct timespec timeSpec_;
      
      // A generic representation of time as seconds and nanoseconds.
      
      unsigned long seconds_;
      unsigned long nanoSeconds_;
      
      // An equivalent representation of time as MJD day number,
      // seconds and nanoseconds.
      
      unsigned long mjdDays_;
      unsigned long mjdSeconds_;
      unsigned long mjdNanoSeconds_;
      
      //------------------------------------------------------------
      // Methods to update all represenations of time.
      //------------------------------------------------------------

    public:

      /**
       * Set the seconds in this object.  
       */
      void setSeconds(unsigned long seconds);
      
      /**
       * Set the microseconds in this struct.  Does not increment
       * the seconds portion of the time kept by this class.
       *
       * @throws Error if microSeconds is greater than 1 second.
       */

      void setMicroSeconds(unsigned long microSeconds);

    private:
      
      /**
       * Set the nanoseconds in this struct.  Does not increment
       * the seconds portion of the time kept by this class.
       *
       * @throws Error if nanoSeconds is greater than 1 second.
       */
      void setNanoSeconds(unsigned long nanoSeconds);
      
    }; // End class TimeVal
  }; // End namespace util
}; // End namespace sza

#endif // End #ifndef 
