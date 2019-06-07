#ifndef SZA_UTIL_HOURANGLE_H
#define SZA_UTIL_HOURANGLE_H

/**
 * @file HourAngle.h
 * 
 * Tagged: Tue Aug 10 14:05:46 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Angle.h"
#include "carma/szautil/Debug.h"

namespace sza {
  namespace util {
    
    class HourAngle : public Angle {
    public:
      
      /**
       * Public constructor intializes to angle modulo 2*PI
       */
      HourAngle() : Angle(true) {};

      /**
       * Constructor.
       */
      void setHours(double hours);
      void setHours(std::string hours);
      void setHours(double hour, double min, double sec);

      /**
       * Destructor.
       */
      virtual ~HourAngle();

      /**
       * Return the contents of this object
       */
      inline double hours() {
	return radians_ * hourPerRad_;
      }

      /**
       * Return the contents of this object
       */
      inline double seconds() {
	return radians_ * arcSecPerRad_;
      }

      inline unsigned getIntegerHours() {
	unsigned hours = (unsigned)(radians_ * hourPerRad_);
	return hours;
      }

      inline unsigned getIntegerMinutes() {
	unsigned hours = (unsigned)(radians_ * hourPerRad_);
	unsigned mins  = (unsigned)((radians_ * hourPerRad_ - hours)*60);
	return mins;
      }


      inline unsigned getIntegerSeconds() {
	unsigned hours = (unsigned)(radians_ * hourPerRad_);
	unsigned mins  = (unsigned)((radians_ * hourPerRad_ - hours)*60);
	unsigned secs  = (unsigned)(((radians_ * hourPerRad_ - hours)*60 - mins)*60);
	return secs;
      }

      inline unsigned getIntegerMilliSeconds() {
	unsigned hours = (unsigned)(radians_ * hourPerRad_);
	unsigned mins  = (unsigned)((radians_ * hourPerRad_ - hours)*60);
	unsigned secs  = (unsigned)(((radians_ * hourPerRad_ - hours)*60 - mins)*60);
	return (unsigned)((((radians_ * hourPerRad_ - hours)*60 - mins)*60 - secs) * 1000);
      }

      /**
       * Allows cout << HourAngle
       */
      friend std::ostream& operator<<(std::ostream& os, HourAngle& hour);

      /** 
       * Add two HourAngles
       */
      HourAngle operator+(HourAngle& angle);
      
      /** 
       * Subtract two HourAngles
       */
      HourAngle operator-(HourAngle& angle);

      static const double hourPerRad_;
      static const double arcSecPerRad_;
      static const double arcSecPerSec_;

    }; // End class HourAngle
    
  } // End namespace util
} // End namespace sza

#endif // End #ifndef SZA_UTIL_HOURANGLE_H
