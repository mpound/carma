#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/antenna/sza/antenna/control/Position.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::util;

/**.......................................................................
 * Constructor just calls reset() to intialize
 */
Position::Position()
{
  reset();
}

/**.......................................................................
 * Initialize data members to something sensible
 */
void Position::reset() 
{
  az_ = 0.0;
  el_ = 0.0;
  pa_ = 0.0;
}

/**.......................................................................
 * Set the data members
 */
void Position::set(Axis::Type axis, double val)
{
  switch (axis) {
  case Axis::AZ:
    az_ = val;
    break;
  case Axis::EL:
    el_ = val;
    break;
  case Axis::PA:
    pa_ = val;
    break;
  default:
    ErrorDef(err, "Position::set: Unrecognized axis.\n");
    break;
  };
}

/**.......................................................................
 * Set the data members
 */
void Position::set(double az, double el, double pa)
{
  az_ = az;
  el_ = el;
  pa_ = pa;
}

/**.......................................................................
 * Get the data members
 */
double Position::get(Axis::Type axis)
{
  switch (axis) {
  case Axis::AZ:
    return az_;
    break;
  case Axis::EL:
    return el_;
    break;
  case Axis::PA:
    return pa_;
    break;
  default:
    ErrorDef(err, "Position::get: Unrecognized axis.\n");
    break;
  };
}

/**.......................................................................
 * Increment the requested position with mount offsets
 */
void Position::increment(MountOffset* offset)
{
  DBPRINT(true, Debug::DEBUG3, "Incrementing mount position by: "
	  << offset->getAz() << ", " << offset->getEl());

  az_  = offset->wrap2pi(az_ + offset->getAz());
  el_ += offset->getEl();
}

/**.......................................................................
 * Pack this position for archival in the register database.
 */
void Position::pack(signed* s_elements)
{
  s_elements[0] = static_cast<signed>(az_ * rtomas);
  s_elements[1] = static_cast<signed>(el_ * rtomas);
  s_elements[2] = static_cast<signed>(pa_ * rtomas);
}
