#include "carma/szautil/Exception.h"
#include "carma/antenna/sza/antenna/control/AxisPos.h"

using namespace sza::antenna::control;
using namespace sza::util;

/**.......................................................................
 * Constructor
 */
AxisPos::AxisPos(Axis::Type type) : axis_(type)
{
  reset();
}

/**.......................................................................
 * Reset internal members
 */
void AxisPos::reset()
{
  topo_  = 0.0;
  count_ = 0;

  // Complain if the axis does not represent a single axis

  if(!axis_.isValidSingleAxis())
    throw Error("AxisPos::reset: Invalid axis.\n");
}
