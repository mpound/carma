// $Id: SzCalculator.h,v 1.1 2010/12/13 21:06:32 eml Exp $

#ifndef SZA_UTIL_SZCALCULATOR_H
#define SZA_UTIL_SZCALCULATOR_H

/**
 * @file SzCalculator.h
 * 
 * Tagged: Fri Aug  1 22:12:12 PDT 2008
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:32 $
 * 
 * @author tcsh: username: Command not found.
 */
#include "carma/szautil/Flux.h"
#include "carma/szautil/Frequency.h"
#include "carma/szautil/Intensity.h"
#include "carma/szautil/Temperature.h"

namespace sza {
  namespace util {

    class SzCalculator {
    public:

      /**
       * Constructor.
       */
      SzCalculator();

      /**
       * Destructor.
       */
      virtual ~SzCalculator();

      // Calculate the factor by which comptonY should be multiplied
      // to convert to CMB temperature decrement/increment

      static Temperature comptonYToDeltaT(Frequency& freq);
      static Temperature comptonYToDeltaTDaisuke(Frequency& freq);

      // Calculate the factor by which comptonY should be multiplied
      // to convert to Flux/sr

      static Intensity comptonYToIntensity(Frequency& freq);

      // Calculate the dimensionless x factor that enters into the
      // Planck function

      static double planckX(Frequency& freq, Temperature& temp);

      // Evaluate the derivative of the Planck function wrt to T at
      // given T and freq.

      static Intensity dPlanck(Frequency& freq, Temperature& temp);

    private:
    }; // End class SzCalculator

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_SZCALCULATOR_H
