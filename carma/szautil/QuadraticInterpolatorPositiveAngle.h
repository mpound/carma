#ifndef SZA_UTIL_QUADRATICINTERPOLATORPOSITIVEANGLE_H
#define SZA_UTIL_QUADRATICINTERPOLATORPOSITIVEANGLE_H

/**
 * @file QuadraticInterpolatorPositiveAngle.h
 * 
 * Tagged: Tue Mar 16 17:02:03 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/QuadraticInterpolator.h"

namespace sza {
  namespace util {
    
    /**
     * Class for interpolating signed (0 <= v < 2.pi) angles
     */
    class QuadraticInterpolatorPositiveAngle : public QuadraticInterpolator {
    public:

      QuadraticInterpolatorPositiveAngle(double emptyValue);

    private:

      double fixAngle(double angle);

    }; // End class QuadraticInterpolatorPositiveAngle
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_QUADRATICINTERPOLATORPOSITIVEANGLE_H
