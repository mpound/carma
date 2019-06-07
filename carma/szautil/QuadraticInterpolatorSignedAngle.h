#ifndef SZA_UTIL_QUADRATICINTERPOLATORSIGNEDANGLE_H
#define SZA_UTIL_QUADRATICINTERPOLATORSIGNEDANGLE_H

/**
 * @file QuadraticInterpolatorSignedAngle.h
 * 
 * Tagged: Tue Mar 16 16:59:18 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/QuadraticInterpolator.h"

namespace sza {
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
} // End namespace sza



#endif // End #ifndef SZA_UTIL_QUADRATICINTERPOLATORSIGNEDANGLE_H
