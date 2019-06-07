#ifndef SZA_UTIL_DECANGLE_H
#define SZA_UTIL_DECANGLE_H

/**
 * @file DecAngle.h
 * 
 * Tagged: Tue Aug 10 14:03:21 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Angle.h"

namespace sza {
  namespace util {
    
    class DecAngle : public Angle {
    public:
      
      /**
       * Constructor.
       */
      DecAngle() {
	initialize();
      };
      
      /**
       * Destructor.
       */
      virtual ~DecAngle() {};

      inline double arcMinutes() {
	return radians_ * arcMinPerRad_;
      }

      inline double arcSeconds() {
	return radians_ * arcSecPerRad_;
      }

      /** 
       * Add two DecAngles
       */
      DecAngle operator+(DecAngle& angle) {
	DecAngle sum;
	sum.setRadians(radians_);
	sum.addRadians(angle.radians());
	return sum;
      }
      
      /** 
       * Subtract two DecAngles
       */
      DecAngle operator-(DecAngle& angle) {
	DecAngle diff;
	diff.setRadians(radians_);
	diff.addRadians(-angle.radians());
	return diff;
      }

      inline int getIntegerDegrees() {
	bool neg = radians_ < 0.0;
	double arad = fabs(radians_);
	unsigned degs = (unsigned)(arad * degPerRad_);

	return (neg ? -1 : 1)*degs;
      }

      inline unsigned getIntegerArcMinutes() {
	double arad = fabs(radians_);
	unsigned degs  = (unsigned)(arad * degPerRad_);
	unsigned mins  = (unsigned)((arad * degPerRad_ - degs)*60);
	return mins;
      }

      inline unsigned getIntegerArcSeconds() {
	double arad = fabs(radians_);
	unsigned degs  = (unsigned)(arad   * degPerRad_);
	unsigned mins  = (unsigned)((arad  * degPerRad_ - degs)*60);
	unsigned secs  = (unsigned)(((arad * degPerRad_ - degs)*60 - mins)*60);
	return secs;
      }

      inline unsigned getIntegerMilliArcSeconds() {
	double arad = fabs(radians_);
	unsigned degs  = (unsigned)(arad   * degPerRad_);
	unsigned mins  = (unsigned)((arad  * degPerRad_ - degs)*60);
	unsigned secs  = (unsigned)(((arad * degPerRad_ - degs)*60 - mins)*60);
	return (unsigned)((((arad * degPerRad_ - degs)*60 - mins)*60 - secs) * 1000);
      }

    private:

      static const double arcSecPerRad_ = 206265;
      static const double arcMinPerRad_ = 206265.0 / 60;

    }; // End class DecAngle
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_DECANGLE_H
