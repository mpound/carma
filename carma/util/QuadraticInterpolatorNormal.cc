#include "carma/util/QuadraticInterpolatorNormal.h"

using namespace carma::util;

/**.......................................................................
 * Constructors.
 */
QuadraticInterpolatorNormal::
QuadraticInterpolatorNormal(double emptyValue) 
{
  type_ = QP_NORMAL;
  setEmptyValue(emptyValue);
}

