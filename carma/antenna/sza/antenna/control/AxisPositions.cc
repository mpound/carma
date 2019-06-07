#include "carma/szautil/Exception.h"

#include "carma/antenna/sza/antenna/control/AxisPositions.h"

#include "carma/szaarrayutils/szaconst.h"

using namespace sza::antenna::control;
using namespace sza::util;

/**.......................................................................
 * Constructor initializes the AxisPos members.
 */
AxisPositions::AxisPositions() : 
  az_(Axis::AZ), el_(Axis::EL), pa_(Axis::PA) {};

/**.......................................................................
 * Return a pointer to the requested axis position
 */
sza::antenna::control::AxisPos* 
AxisPositions::AxisPos(Axis::Type type)
{
  switch (type) {
  case Axis::AZ:
    return &az_;
    break;
  case Axis::EL:
    return &el_;
    break;
  case Axis::PA:
    return &pa_;
    break;
  default:
    throw Error("AxisPositions::AxisPos: Unrecognized axis.\n");
    break;
  }
}

/**.......................................................................
 * Pack the topocentric positions for archival in the register
 * database
 */
void AxisPositions::pack(signed* s_elements)
{
  s_elements[0] = static_cast<signed>(az_.topo_ * rtomas); // Milli-arcseconds 
  s_elements[1] = static_cast<signed>(el_.topo_ * rtomas); // Milli-arcseconds 
  s_elements[2] = static_cast<signed>(pa_.topo_ * rtomas); // Milli-arcseconds 
}
