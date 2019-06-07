#include <cmath>

#include "carma/szautil/QuadraticInterpolatorPositiveAngle.h"

using namespace sza::util;

/**.......................................................................
 * Constructor
 */
QuadraticInterpolatorPositiveAngle::
QuadraticInterpolatorPositiveAngle(double emptyValue) 
{
  type_ = QP_POSITIVE_ANGLE;
  setEmptyValue(emptyValue);
}

/*.......................................................................
 * Wrap an angle into the range 0 <= v < 2.pi_.
 *
 * Input:
 *
 *  angle   double  The angle to be wrapped (radians).
 *
 * Output:
 *
 *  return  double  The angle adjusted into the range 0 <= v < 2.pi_.
 */
double QuadraticInterpolatorPositiveAngle::fixAngle(double angle)
{
  return angle - twopi_ * floor(angle/twopi_);
}
