#ifndef SZA_UTIL_POWER_H
#define SZA_UTIL_POWER_H

/**
 * @file Power.h
 * 
 * Tagged: Thu Oct 18 17:42:37 PDT 2007
 * 
 * @author SZA data acquisition
 */
#include "carma/szautil/ConformableQuantity.h"

namespace sza {
  namespace util {
    
    class Power : public ConformableQuantity {
    public:
      
      class dBm {};

      /**
       * Constructor.
       */
      Power();
      Power(const dBm& units, double dBmPow);

      /**
       * Destructor.
       */
      virtual ~Power();
      
      void setdBm(double dBmPow) {
	dBm_ = dBmPow;
      }

      double getdBm() {
	return dBm_;
      }

    private:

      void initialize();

      // The actual frequency, in dBm
      
      double dBm_;

    }; // End class Power
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_POWER_H
