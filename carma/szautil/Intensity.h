// $Id: Intensity.h,v 1.1 2010/12/13 21:06:30 eml Exp $

#ifndef SZA_UTIL_INTENSITY_H
#define SZA_UTIL_INTENSITY_H

/**
 * @file Intensity.h
 * 
 * Tagged: Fri Aug  1 23:13:07 PDT 2008
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:30 $
 * 
 * @author tcsh: username: Command not found.
 */
#include "carma/szautil/ConformableQuantity.h"

namespace sza {
  namespace util {

    class Intensity : public ConformableQuantity {
    public:

      class JanskyPerSr {};
      class MegaJanskyPerSr {};

      /**
       * Constructor.
       */
      Intensity();
      Intensity(const JanskyPerSr& units, double JyPerSr);
      Intensity(const MegaJanskyPerSr& units, double MJyPerSr);

      /**
       * Destructor.
       */
      virtual ~Intensity();

      // Return the flux, in Jy

      inline double JyPerSr() {
	return JyPerSr_;
      }

      void setJyPerSr(double JyPerSr);

    private:

      double JyPerSr_;

      void initialize();

    }; // End class Intensity

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_INTENSITY_H
