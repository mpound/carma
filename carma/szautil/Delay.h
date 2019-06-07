#ifndef SZA_UTIL_DELAY_H
#define SZA_UTIL_DELAY_H

/**
 * @file Delay.h
 * 
 * Tagged: Tue Aug 10 13:16:39 PDT 2004
 * 
 * @author Erik Leitch
 */
#include <iostream>

#include "carma/szautil/Length.h"

namespace sza {
  namespace util {
    
    /**
     * Trivial class for handling delays
     */
    class Delay {
    public:
      
      /**
       * Constructor.
       */
      Delay();
      
      /**
       * Destructor.
       */
      virtual ~Delay();
      
      /**
       * Set the delay 
       */
      void setDelay(Length length) {
	setDelayInMeters(length.meters());
      }

      /**
       * Set the delay, in meters
       */
      void setDelayInMeters(double delayInMeters) {
	nanoSeconds_ = delayInMeters / lightSpeed_ * nanoSecPerSec_;
      }

      /**
       * Set the delay rate, in meters/second
       */
      void setDelayRateInMeters(double delayRateInMeters) {
	dNanoSeconds_ = delayRateInMeters / lightSpeed_ * nanoSecPerSec_;
      }

      /**
       * Set the delay, in nanoSeconds
       */
      void setDelayInNanoSeconds(double delayInNanoSec) {
	nanoSeconds_ = delayInNanoSec;
      }

      /**
       * Set the delay rate, in nanoSeconds/second
       */
      void setDelayRateInNanoSeconds(double delayRateInNanoSec) {
	dNanoSeconds_ = delayRateInNanoSec;
      }

      /**
       * Get the delay, in meters
       */
      inline double meters() {
	return nanoSeconds_ / nanoSecPerSec_ * lightSpeed_;
      }

      /**
       * Get the delay rate, in meters/sec
       */
      inline double metersPerSecond() {
	return dNanoSeconds_ / nanoSecPerSec_ * lightSpeed_;
      }

      /**
       * Get the delay, in seconds
       */
      inline double seconds() {
	return nanoSeconds_ / nanoSecPerSec_;
      }

      /**
       * Get the delay, in nanoseconds
       */
      inline double nanoSeconds() {
	return nanoSeconds_;
      }

      /**
       * Get the delay rate, in nanoseconds/sec
       */
      inline double nanoSecondsPerSecond() {
	return dNanoSeconds_;
      }

      /**
       * Get the delay rate, in seconds/sec
       */
      inline double secondsPerSecond() {
	return dNanoSeconds_ / nanoSecPerSec_;
      }

      /** 
       * Add two Delays
       */
      Delay  operator+(Delay& delay);
      Delay& operator+=(Delay& delay);
      Delay& operator+=(Delay delay);
      
      /** 
       * Subtract two Delays
       */
      Delay  operator-(Delay& delay);
      Delay& operator-=(Delay& delay);
      Delay& operator-=(Delay delay);

      /**
       * Allows cout << Delay
       */

      friend std::ostream& operator<<(std::ostream& os, Delay delay);

      inline void flipDelay() {
	nanoSeconds_ *= -1;
      }

      inline void flipRate() {
	dNanoSeconds_ *= -1;
      }

    private:

      /**
       * Speed of light, in meters per second
       */
      static const double lightSpeed_;
      static const double nanoSecPerSec_;

      double nanoSeconds_;
      double dNanoSeconds_;

    }; // End class Delay

    std::ostream& operator<<(std::ostream& os, Delay delay);
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_DELAY_H
