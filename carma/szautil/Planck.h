// $Id: Planck.h,v 1.1 2010/12/13 21:06:31 eml Exp $

#ifndef SZA_UTIL_PLANCK_H
#define SZA_UTIL_PLANCK_H

/**
 * @file Planck.h
 * 
 * Tagged: Thu Aug 21 11:15:49 PDT 2008
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:31 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/Frequency.h"
#include "carma/szautil/Intensity.h"
#include "carma/szautil/Temperature.h"

namespace sza {
  namespace util {

    class Planck {
    public:

      /**
       * Constructor.
       */
      Planck();

      /**
       * Destructor.
       */
      virtual ~Planck();

      // Return the Planck intensity

      static Intensity IPlanck(Frequency nu, Temperature T);
      
      // Return the dimensionless Planck x-factor

      static double xPlanck(Frequency nu, Temperature T);

      // Return Planck dI/dT, in units of (Jy/sr) / K

      static double JyPerSrPerKPlanck(Frequency nu, Temperature T);

    private:
    }; // End class Planck

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_PLANCK_H
