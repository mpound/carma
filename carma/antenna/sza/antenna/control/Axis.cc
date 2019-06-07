#include "carma/antenna/sza/antenna/control/Axis.h"

using namespace sza::antenna::control;

/**.......................................................................
 * Constructor
 */
Axis::Axis(Type type)
{
  type_ = type;
}

/**.......................................................................
 * Return true if the passed enumerator represents a valid single axis
 */
bool Axis::isValidSingleAxis()
{
  if(type_ == AZ || type_ == EL || type_ == PA)
    return true;
  return false;
}
