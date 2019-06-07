#include <cmath>
#include <iostream>

#include "carma/szautil/Debug.h"

#include "carma/antenna/sza/antenna/control/Collimation.h"

using namespace sza::antenna::control;
using namespace sza::util;

/**.......................................................................
 * Constructor trivially calls reset() method, below.
 */
Collimation::Collimation() : SkyOffset() 
{
  reset();
};

/**.......................................................................
 * Update the x offset associated with this collimation correction
 */
void Collimation::setXOffset(Angle x)
{
  setXInRadians(x.radians());
}

/**.......................................................................
 * Update the elevation offset associated with this collimation correction
 */
void Collimation::setYOffset(Angle y)
{
  setYInRadians(y.radians());
}

/**.......................................................................
 * Update the x offset associated with this collimation correction
 */
void Collimation::incrXOffset(Angle x)
{
  incrXInRadians(x.radians());
}

/**.......................................................................
 * Update the elevation offset associated with this collimation correction
 */
void Collimation::incrYOffset(Angle y)
{
  incrYInRadians(y.radians());
}

bool Collimation::isUsable()
{
  return usable_;
}

void Collimation::setUsable(bool usable)
{
  usable_ = usable;
}

void Collimation::reset()
{
  setUsable(false);
}
