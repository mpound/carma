#ifndef SZA_UTIL_ANGLE_H
#define SZA_UTIL_ANGLE_H

/**
 * @file Angle.h
 * 
 * Tagged: Tue Aug 10 13:49:00 PDT 2004
 * 
 * @author Erik Leitch
 */
#include <cmath>
#include <string>
#include <iostream>

#include "carma/szautil/ConformableQuantity.h"

namespace sza {
  namespace util {
    
    class Angle : public ConformableQuantity {
    public:
      
      class Radians {};
      class Degrees {};
      class ArcSec {};
      class ArcMinutes {};
      class MilliArcSec {};

      /**
       * Public constructor with no value initialization
       */
      Angle(bool modulo=false);

      /**
       * Unambiguous contructor for degree strings
       */
      Angle(std::string degrees, bool modulo);

      /**
       * Constructors with unit specifications
       */
      Angle(const Radians& units,     double radians, bool modulo=false);
      Angle(const Degrees& units,     double degrees, bool modulo=false);
      Angle(const MilliArcSec& units, double mas,     bool modulo=false);
      Angle(const ArcSec& units,      double as,      bool modulo=false);
      Angle(const ArcMinutes& units,  double am,      bool modulo=false);
      Angle(const Angle& angle);
      Angle(Angle& angle);

      /**
       * Destructor.
       */
      virtual ~Angle();
      
      /**
       * Set the angle
       */
      void setRadians(double radians);
      void setDegrees(double degrees);
      void setDegrees(double degrees, double arcmin, double arcsec);
      void setArcMinutes(double am);
      void setMas(double mas);
      void setArcSec(double as);
      void setDegrees(std::string degrees);

      /**
       * Add to the angle
       */
      virtual void addRadians(double radians);
      virtual void addDegrees(double degrees);
      virtual void addDegrees(std::string degrees);

      /**
       * Get the angle
       */
      inline double radians() {
	return radians_;
      }

      inline double degrees() {
	return radians_ * degPerRad_;
      }

      inline double arcsec() {
	return radians_ * arcSecPerRad_;
      }

      inline double arcmin() {
	return radians_ * arcMinPerRad_;
      }

      inline double mas() {
	return radians_ * masPerRad_;
      }

      inline short tiltmeterUnits() {
	return (short)(arcmin() * 1000);
      }

      inline short caltertUnits() {
	return (short)(degrees() * 100);
      }

      std::string strDegrees();

      void operator=(Angle& angle);
      void operator=(const Angle& angle);

      /**
       * Allows cout << Angle
       */
      friend std::ostream& operator<<(std::ostream& os, Angle& angle);

      /** 
       * Add two Angles
       */
      Angle operator+(Angle& angle);
      
      /** 
       * Add two Angles
       */
      void operator+=(Angle& angle);
      void operator+=(const Angle& angle);

      /** 
       * Subtract two Angles
       */
      Angle operator-(Angle& angle);

      /** 
       * Divide an angle by an integer
       */
      void operator/=(unsigned uval);

      /** 
       * Divide an angle by an integer
       */
      Angle operator/(unsigned uval);

      /** 
       * Compare two Angles
       */
      bool operator>(Angle& angle);
      bool operator>(const Angle& angle);
      bool operator>=(Angle& angle);
      bool operator<(Angle& angle);
      bool operator<(const Angle& angle);
      bool operator<=(Angle& angle);

      //------------------------------------------------------------
      // Public utility methods

      /**
       * Convert a sexagesimal string to a double representation
       */
      static double sexagesimalToDouble(std::string valStr);

      /**
       * Convert a double to a sexagesimal string
       */
      static std::string doubleToSexagesimal(double val);

      static double radiansToPiMinusPi(double radians);
      static double radiansToZeroTwoPi(double radians);

      static const double pi_;
      static const double twoPi_;

      static const double degPerRad_;
      static const double arcSecPerDegree_;
      static const double masPerDegree_;
      static const double arcSecPerRad_;
      static const double arcMinPerRad_;
      static const double masPerRad_;

      void initialize();

    protected:

      Angle(double radians, bool modulo);

      double radians_; // The radian representation of this angle
      bool modulo_;    // True if this angle should be manipulated
		       // modulo twoPi_

    }; // End class Angle
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_ANGLE_H
