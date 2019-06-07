#ifndef CARMA_UTIL_QUADRATICINTERPOLATORSIGNEDANGLE_H
#define CARMA_UTIL_QUADRATICINTERPOLATORSIGNEDANGLE_H

/**
 * @file QuadraticInterpolatorSignedAngle.h
 * 
 * Tagged: Tue Mar 16 16:59:18 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/util/QuadraticInterpolator.h"

namespace carma {
  namespace util {
    
    /**
     * Class for interpolating signed (-pi <= v < pi) angles
     */
    class QuadraticInterpolatorSignedAngle : public QuadraticInterpolator {
    public:

	    QuadraticInterpolatorSignedAngle(double emptyValue);

    private:

      double fixAngle(double angle);

    }; // End class QuadraticInterpolatorSignedAngle
    
  } // End namespace util
} // End namespace carma



#endif // End #ifndef CARMA_UTIL_QUADRATICINTERPOLATORSIGNEDANGLE_H
