#include <cmath>

#include "carma/szautil/Debug.h"
#include "carma/antenna/sza/antenna/control/AzTilt.h"
#include "carma/szaarrayutils/szaconst.h"

using namespace sza::antenna::control;
using namespace sza::util;

/**.......................................................................
 * Constructor just resets internal members.
 */
AzTilt::AzTilt()
{
  reset();
}

/**.......................................................................
 * Reset internal data members
 */
void AzTilt::reset()
{
  haTilt_  = 0.0;
  latTilt_ = 0.0;
}

/**.......................................................................
 * Install the HA tilt.
 */
void AzTilt::setHaTilt(double haTilt)
{
  haTilt_ = haTilt;
}

/**.......................................................................
 * Install the latitude tilt.
 */
void AzTilt::setLatTilt(double latTilt)
{
  latTilt_ = latTilt;
}

/**.......................................................................
 * Correct the tilt of the azimuth axis.
 *
 * As of March 2001 using a new algorithm from MCS which actually
 * works in terms of of the magnitude and direction of the tilt.
 * However to avoid changing the names of variables etc. convert tilts
 * in the directions of hour angle and latitude to a magnitude and
 * direction in this function.
 *
 * DASI tilt meter has same orientation as SZA one.  x is
 * front-to-back with positive x corresponding to front tilted up. y
 * is 90 deg anticlock from x when looking down az axis (usual RH
 * coord frame).  So for scope at az=0 xtilt is latTilt, and ytilt is
 * haTilt.
 *
 * Input/Output:
 *  f    PointingCorrections *  The az/el pointing to be corrected.
 */
void AzTilt::apply(PointingCorrections* f)
{
  DBPRINT(true, Debug::DEBUG3, "Applying tilts: latTilt_ = " << latTilt_
	  << "haTilt_ = " << haTilt_);

  double x = latTilt_;
  double y = haTilt_;
  /*
   * Precompute trig terms.
   */
  double cos_x = cos(x);
  double sin_x = sin(x);
  double ycos_x = y * cos(x);
  double cos_ycosx = cos(ycos_x);
  double sin_ycosx = sin(ycos_x);
  /*
   * Compute the numerator and denominator of the atan2() that is used
   * to compute the azimuth of the tilt.
   */
  double top = sin_ycosx;
  double bot = cos_ycosx * sin_x;
  /*
   * Compute the azimuth of the tilt.
   * Using -halfpi here seems to be arbitrary
   */
  double tilt_az = 0 - pi - (top==0.0 && bot==0.0 ? 0.0 : atan2(top, bot));
  /*
   * Compute the magnitude of the tilt.
   */
  double tilt_mag = acos(cos_x * cos_ycosx);
  /*
   * Compute the direction between the azimuth of the source and the azimuth
   * of the axis around which the tilt is directed.
   */
  double w = tilt_az - halfpi - f->az;
  /*
   * Precompute trig terms.
   */
  double sin_w = sin(w);
  double cos_w = cos(w);
  double sin_mag = sin(tilt_mag);
  double cos_mag = cos(tilt_mag);
  /*
   * Compute the new target elevation.
   */
  f->sin_el = f->sin_el * cos_mag - f->cos_el * sin_mag * sin_w;
  f->el = asin(f->sin_el);
  f->cos_el = cos(f->el);
  /*
   * Compute the new target azimuth.
   */
  {
    double top = cos_w * f->cos_el;
    double bot = -(cos_mag * sin_w * f->cos_el + sin_mag * f->sin_el);
    f->az = tilt_az - (top==0.0 && bot==0.0 ? 0.0 : atan2(top,bot));
    f->sin_az = sin(f->az);
    f->cos_az = cos(f->az);
  };
  /*
   * Compute the new target parallactic angle.
   */
  {
    double top = cos_w * sin_mag;
    double bot = f->cos_el * cos_mag + f->sin_el * sin_mag * sin_w;
    f->pa -= (top==0.0 && bot==0.0 ? 0.0 : atan2(top, bot));
  };
}

/**.......................................................................
 * Pack an HA tilt for archival in the register database.
 */
void AzTilt::packHaTilt(signed* s_elements) 
{
  *s_elements = static_cast<signed>(haTilt_ * rtomas);
}

/**.......................................................................
 * Pack a latitude tilt for archival in the register database.
 */
void AzTilt::packLatTilt(signed* s_elements) 
{
  *s_elements = static_cast<signed>(latTilt_ * rtomas);
}

