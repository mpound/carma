#ifndef CARMA_UTIL_QUADRATICINTERPOLATORPOSITIVEANGLE_H
#define CARMA_UTIL_QUADRATICINTERPOLATORPOSITIVEANGLE_H

/**
 * @file QuadraticInterpolatorPositiveAngle.h
 * 
 * Tagged: Tue Mar 16 17:02:03 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/util/QuadraticInterpolator.h"

namespace carma {
  namespace util {
    
    /**
     * Class for interpolating signed (0 <= v < 2.pi) angles
     */
    class QuadraticInterpolatorPositiveAngle : public QuadraticInterpolator {
    public:

	    QuadraticInterpolatorPositiveAngle(double emptyValue=0.0);

    private:

      double fixAngle(double angle);

    }; // End class QuadraticInterpolatorPositiveAngle
    
  } // End namespace util
} // End namespace carma



#endif // End #ifndef CARMA_UTIL_QUADRATICINTERPOLATORPOSITIVEANGLE_H
