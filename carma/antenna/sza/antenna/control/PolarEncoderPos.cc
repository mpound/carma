#include "carma/antenna/sza/antenna/control/PolarEncoderPos.h"

using namespace std;
using namespace sza::antenna::control;

/**.......................................................................
 * Constructor.
 */
PolarEncoderPos::PolarEncoderPos()
{
  // Initialize these to something impossible

  right_ = UNSET;
  left_  = UNSET;
}
