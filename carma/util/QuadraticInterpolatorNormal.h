#ifndef CARMA_UTIL_QUADRATICINTERPOLATORNORMAL_H
#define CARMA_UTIL_QUADRATICINTERPOLATORNORMAL_H

/**
 * @file QuadraticInterpolatorNormal.h
 * 
 * Tagged: Tue Mar 16 17:16:24 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/util/QuadraticInterpolator.h"

namespace carma {
  namespace util {
    
    /**
     * Class for interpolating normal (non-angle) ordinates
     */
    class QuadraticInterpolatorNormal : public QuadraticInterpolator {
    public:
      
      /**
       * Constructor.
       */
      QuadraticInterpolatorNormal(double emptyValue=0.0);

    }; // End class QuadraticInterpolatorNormal
    
  } // End namespace util
} // End namespace carma



#endif // End #ifndef CARMA_UTIL_QUADRATICINTERPOLATORNORMAL_H
