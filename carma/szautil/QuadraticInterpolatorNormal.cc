#include "carma/szautil/QuadraticInterpolatorNormal.h"

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
QuadraticInterpolatorNormal::
QuadraticInterpolatorNormal(double emptyValue) 
{
  type_ = QP_NORMAL;
  setEmptyValue(emptyValue);
}
