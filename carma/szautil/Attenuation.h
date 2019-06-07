#ifndef SZA_UTIL_ATTENUATION_H
#define SZA_UTIL_ATTENUATION_H

/**
 * @file Attenuation.h
 * 
 * Tagged: Sun Mar 27 12:36:42 PST 2005
 * 
 * @author Erik Leitch
 */
#include <iostream>

#include "carma/szautil/ConformableQuantity.h"

namespace sza {
  namespace util {
    
    class Attenuation : public ConformableQuantity {
    public:
      
      class dBUnit {};

      /**
       * Constructor.
       */
      Attenuation();
      Attenuation(const dBUnit& units, double dB);
       
      /**
       * Destructor.
       */
      virtual ~Attenuation();
      
      // Set the attenuation, in dB

      void setdB(double dB);

      // Return the attenuation, in dB

      inline double dB() {
	return dB_;
      }

      inline unsigned char intModUnits() {
	return (unsigned char)dB();
      }

      void initialize();

    private:

      double dB_;

    }; // End class Attenuation
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_ATTENUATION_H
