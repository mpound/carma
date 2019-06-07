// $Id: SolidAngle.h,v 1.1 2010/12/13 21:06:32 eml Exp $

#ifndef SZA_UTIL_SOLIDANGLE_H
#define SZA_UTIL_SOLIDANGLE_H

/**
 * @file SolidAngle.h
 * 
 * Tagged: Wed Sep 14 17:52:22 PDT 2005
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:32 $
 * 
 * @author Erik Leitch
 */
#include <iostream>

#include "carma/szautil/ConformableQuantity.h"
#include "carma/szautil/Angle.h"

namespace sza {
  namespace util {

    class SolidAngle : public ConformableQuantity {
    public:

      class Steradians {};
      class SqArcMinutes {};

      /**
       * Constructor.
       */
      SolidAngle();
      SolidAngle(const Steradians& units,   double sr);
      SolidAngle(const SqArcMinutes& units, double sqarcmin);
      SolidAngle(Angle& fwhm);
      SolidAngle(Angle& fwhma, Angle& fwhmb);

      /**
       * Destructor.
       */
      virtual ~SolidAngle();

      void initialize();

      void setSr(double sr);
      void setSqArcMin(double sqarcmin);

      inline double sqArcMin() {
	return sr_ * Angle::arcMinPerRad_ * Angle::arcMinPerRad_;
      }

      inline double sqDegrees() {
	return sr_ * Angle::degPerRad_ * Angle::degPerRad_;
      }

      inline double sr() {
	return sr_;
      }

    private:

      double sr_;

    }; // End class SolidAngle

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_SOLIDANGLE_H
