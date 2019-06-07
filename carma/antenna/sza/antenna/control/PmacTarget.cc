#include "carma/szautil/Exception.h"

#include "carma/antenna/sza/antenna/control/PmacTarget.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::util;

using namespace sza::util;

/**.......................................................................
 * Return a pointer to the requested PmacAxis container
 *
 * @throws Exception
 */
sza::antenna::control::PmacAxis* 
PmacTarget::PmacAxis(Axis::Type axis)
{
  switch (axis) {
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
    throw Error("PmacTarget::PmacAxis: Received unrecognized axis type.\n");
    break;
  };
}

/**.......................................................................
 * Return the current mode
 */
PmacMode::Mode PmacTarget::getMode()
{
  return mode_;
}

/**.......................................................................
 * Set the current mode
 */
void PmacTarget::setMode(PmacMode::Mode mode)
{
  mode_ = mode;
}

/**.......................................................................
 * Pack the current mode for archival in the register database.
 */
void PmacTarget::packMode(unsigned* u_elements)
{
  u_elements[0] = mode_;
}

/**.......................................................................
 * Pack the encoder counts for archival in the register database.
 */
void PmacTarget::packCounts(signed* s_elements)
{
  s_elements[0] = static_cast<signed>(az_.getCount());
  s_elements[1] = static_cast<signed>(el_.getCount());
  s_elements[2] = static_cast<signed>(pa_.getCount());
}

/**.......................................................................
 * Pack the encoder rates for archival in the register database.
 */
void PmacTarget::packRates(signed* s_elements)
{
  s_elements[0] = static_cast<signed>(az_.getRate());
  s_elements[1] = static_cast<signed>(el_.getRate());
  s_elements[2] = static_cast<signed>(pa_.getRate());
}
