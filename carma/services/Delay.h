#ifndef CARMA_UTIL_DELAY_H
#define CARMA_UTIL_DELAY_H

/**
 * @file Delay.h
 * 
 * @author Peter Teuben 
 * Derived from SZA 
 */
#include <iostream>

namespace carma {
  namespace services {
    
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
      inline double meters() const {
	return nanoSeconds_ / nanoSecPerSec_ * lightSpeed_;
      }

      /**
       * Get the delay rate, in meters/sec
       */
      inline double metersPerSecond() const {
	return dNanoSeconds_ / nanoSecPerSec_ * lightSpeed_;
      }

      /**
       * Get the delay, in nanoseconds
       */
      inline double nanoSeconds() const {
	return nanoSeconds_;
      }

      /**
       * Get the delay, in nanoseconds/sec
       */
      inline double nanoSecondsPerSecond() const {
	return dNanoSeconds_;
      }

      /**
       * Get the delay rate, in seconds/sec
       */
      inline double secondsPerSecond() const {
	return dNanoSeconds_ / nanoSecPerSec_;
      }

      /** 
       * Add two Delays
       */
      const Delay  operator+(const Delay& delay) const;
      Delay& operator+=(const Delay& delay);
      Delay& operator+=(const Delay delay);
      
      /** 
       * Subtract two Delays
       */
      const Delay  operator-(const Delay& delay) const;
      Delay& operator-=(const Delay& delay);
      Delay& operator-=(const Delay delay);

      /**
       * Allows cout << Delay
       */
      //      friend std::ostream& operator<<(std::ostream& os, Delay& delay);
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
      static const double lightSpeed_    = 2.99792458e8;
      static const double nanoSecPerSec_ = 1e9;

      double nanoSeconds_;
      double dNanoSeconds_;

    }; // End class Delay
    std::ostream& operator<<(std::ostream& os, Delay delay);
  } // End namespace util
} // End namespace carma



#endif // End #ifndef CARMA_UTIL_DELAY_H
