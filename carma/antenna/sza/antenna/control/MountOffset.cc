#include <iostream>

#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"

#include "carma/antenna/sza/antenna/control/MountOffset.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::util;

/**.......................................................................
 * Constructor just intializes the offsets to zero
 */
MountOffset::MountOffset()
{
  reset();
}

/**.......................................................................
 * Initialize the offsets
 */
void MountOffset::reset()
{
  az_ = 0;
  el_ = 0;
  pa_ = 0;
}

/**.......................................................................
 * Increment the offsets, in radians
 */
void MountOffset::increment(double daz, double del)
{
  az_ = wrapPi(az_ + daz);
  el_ = wrapPi(el_ + del);
}

/**.......................................................................
 * Set the offsets
 */
void MountOffset::set(OffsetMsg msg)
{
  switch(msg.mode) {
  case OffsetMsg::ADD:
    if(msg.axes & OffsetMsg::AZ)
      az_ = wrapPi(az_ + msg.body.mount.az);
    if(msg.axes & OffsetMsg::EL)
      el_ = wrapPi(el_ + msg.body.mount.el);
    break;

    // Replace the existing offsets.

  case OffsetMsg::SET:
    if(msg.axes & OffsetMsg::AZ)
      az_ = wrapPi(msg.body.mount.az);
    if(msg.axes & OffsetMsg::EL)
      el_ = wrapPi(msg.body.mount.el);
    break;
  default:
    ErrorDef(err, "SkyOffset::setOffset: Unrecognized mode.\n");
    break;
  };
  DBPRINT(true, Debug::DEBUG3, "Mount offsets are (new): " << az_ << ", " << el_);
}

/**.......................................................................
 * Return the az offset
 */
double MountOffset::getAz()
{
  return az_;
}

/**.......................................................................
 * Return the elevation offset
 */
double MountOffset::getEl()
{
  return el_;
}

/**.......................................................................
 * Method to pack the mount offsets for archival in the register
 * database.
 */
void MountOffset::pack(signed* s_elements)
{
  DBPRINT(true, Debug::DEBUG3, "Mount offsets are: "
	  << az_ << ", " << el_);

  s_elements[0] = static_cast<signed>(az_ * rtomas);
  s_elements[1] = static_cast<signed>(el_ * rtomas);
  s_elements[2] = static_cast<signed>(pa_ * rtomas);
}

