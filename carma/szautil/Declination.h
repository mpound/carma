// $Id: Declination.h,v 1.1 2010/12/13 21:06:29 eml Exp $

#ifndef SZA_UTIL_DECLINATION_H
#define SZA_UTIL_DECLINATION_H

/**
 * @file Declination.h
 * 
 * Tagged: Fri Jun 15 16:44:12 PDT 2007
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:29 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/DecAngle.h"

namespace sza {
  namespace util {

    class Declination : public DecAngle {
    public:

      /**
       * Constructor.
       */
      Declination();

      /**
       * Destructor.
       */
      virtual ~Declination();

      void addRadians(double radians);

      /** 
       * Add an angle to this declination
       */
      Declination operator+(Angle& angle) {
	Declination sum;
	sum.setRadians(radians_);
	sum.addRadians(angle.radians());
	return sum;
      }

      /** 
       * Subtract an angle from this declination
       */
      Declination operator-(Angle& angle) {
	Declination sum;
	sum.setRadians(radians_);
	sum.addRadians(-angle.radians());
	return sum;
      }

    private:
    }; // End class Declination

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_DECLINATION_H
