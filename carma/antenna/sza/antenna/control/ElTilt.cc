#include <cmath>

#include "carma/szautil/Debug.h"
#include "carma/antenna/sza/antenna/control/ElTilt.h"
#include "carma/szaarrayutils/szaconst.h"

using namespace sza::antenna::control;
using namespace sza::util;

/**.......................................................................
 * Constructor trivially calls reset(), below.
 */
ElTilt::ElTilt()
{
  reset();
}

/**.......................................................................
 * Reset internal data members.
 */
void ElTilt::reset()
{
  tilt_     = 0.0;
  sin_tilt_ = 0.0;
  cos_tilt_ = 1.0;
}

/**.......................................................................
 * Install the tilt.
 */
void ElTilt::setTilt(double tilt)
{
  // Install the new tilt

  tilt_ = tilt;

  // And precompute sines and cosines

  sin_tilt_ = sin(tilt_);
  cos_tilt_ = cos(tilt_);
}

/**.......................................................................
 * Correct the misalignment of the elevation axis.
 *
 * Input/Output:
 *  f    PointingCorrections *  The az/el pointing to be corrected.
 */
void ElTilt::apply(PointingCorrections* f)
{
  DBPRINT(true, Debug::DEBUG3, "Applying tilts: elTilt_ = " << tilt_);

  // Correct the elevation.

  f->sin_el /= cos_tilt_;
  f->el = asin(f->sin_el);
  f->cos_el = cos(f->el);

  // Correct the azimuth.

  if(f->cos_el != 0.0) {
    f->az -= asin(f->sin_el/f->cos_el * sin_tilt_ / cos_tilt_);
    f->sin_az = sin(f->az);
    f->cos_az = cos(f->az);
  };

  // Correct the parallactic angle.

  if(f->cos_el != 0.0)
    f->pa -= asin(sin_tilt_ / f->cos_el);
}

/**.......................................................................
 * Pack a tilt for archival in the register database.
 */
void ElTilt::packTilt(signed* s_elements) 
{
  *s_elements = static_cast<signed>(tilt_ * rtomas);
}

