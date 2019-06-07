// $Id: Flux.h,v 1.2 2012/02/22 18:44:43 iws Exp $

#ifndef SZA_UTIL_FLUX_H
#define SZA_UTIL_FLUX_H

/**
 * @file Flux.h
 * 
 * Tagged: Wed Sep 14 17:14:39 PDT 2005
 * 
 * @version: $Revision: 1.2 $, $Date: 2012/02/22 18:44:43 $
 * 
 * @author Erik Leitch
 */
#include <iostream>

#include "carma/szautil/ConformableQuantity.h"
#include "carma/szautil/Frequency.h"
#include "carma/szautil/Temperature.h"
#include "carma/szautil/SolidAngle.h"

namespace sza {
  namespace util {

    class Flux : public ConformableQuantity {
    public:

      class Jansky {};
      class MilliJansky {};
      class MegaJansky {};

      /**
       * Constructor.
       */
      Flux();
      Flux(const Jansky& units, double Jy);
      Flux(const MilliJansky& units, double mJy);
      Flux(const MegaJansky& units, double MJy);
      Flux(Frequency& freq, Temperature& temp, SolidAngle& omega);

      /**
       * Destructor.
       */
      virtual ~Flux();

      void setJy(double Jy);
      void setMilliJy(double mJy);
      void setMegaJy(double MJy);

      // Return the flux, in Jy

      inline double Jy() {
	return Jy_;
      }

      // Return the flux, in mJy

      inline double mJy() {
	return Jy_ * mJyPerJy_;
      }

      static const double mJyPerJy_;
      static const double JyPerMJy_;

      void initialize();

      friend std::ostream& operator<<(std::ostream& os, Flux& flux);

      bool operator>=(Flux& flux);
      bool operator<=(Flux& flux);

    private:

      double Jy_;

    }; // End class Flux

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_FLUX_H
