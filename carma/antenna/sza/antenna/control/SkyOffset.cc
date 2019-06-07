#include <cmath>

#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"

#include "carma/antenna/sza/antenna/control/SkyOffset.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::util;


/**.......................................................................
 * Constructor just initializes the offsets
 */
SkyOffset::SkyOffset()
{
  reset();
}

/**.......................................................................
 * Reset the sky offsets
 */
void SkyOffset::reset()
{
  active_ = false;
  x_ = 0.0;
  y_ = 0.0;
  cos_theta_  = 1.0;
  sin_theta_  = 0.0;
  cos_radius_ = 1.0;
  sin_radius_ = 0.0;
}

void SkyOffset::setXInRadians(double x) 
{
  DBPRINT(true, Debug::DEBUG3, "Setting x offset to: " << x);
  x_ = wrapPi(x);
  cacheValues();
}

void SkyOffset::setYInRadians(double y) 
{
  DBPRINT(true, Debug::DEBUG3, "Setting y offset to: " << y);
  y_ = wrapPi(y);
  cacheValues();
}

void SkyOffset::incrXInRadians(double x) 
{
  DBPRINT(true, Debug::DEBUG3, "Incrting x offset to: " << x);
  setXInRadians(x_ + x);
}

void SkyOffset::incrYInRadians(double y) 
{
  DBPRINT(true, Debug::DEBUG3, "Incrting y offset to: " << y);
  setXInRadians(y_ + y);
}

/**.......................................................................
 * Update the values of the sky offset
 */
void SkyOffset::set(OffsetMsg msg)
{
  // Increment the existing offsets?

  switch(msg.mode) {
  case OffsetMsg::ADD:

    if(msg.axes & OffsetMsg::X)
      setXInRadians(x_ + msg.body.sky.x);
    if(msg.axes & OffsetMsg::Y)
      setYInRadians(y_ + msg.body.sky.y);
    break;

    // Replace the existing offsets.

  case OffsetMsg::SET:

    if(msg.axes & OffsetMsg::X)
      setXInRadians(msg.body.sky.x);
    if(msg.axes & OffsetMsg::Y)
      setYInRadians(msg.body.sky.y);
    break;

  default:
    ErrorDef(err, "SkyOffset::setOffset: Unrecognized mode.\n");
    break;
  };

  // And precompute sines and cosines

  cacheValues();
}

void SkyOffset::cacheValues()
{
  const double tiny = 1.0e-7; // A small angle 

  // Convert from cartesian to polar on the surface of the celestial
  // sphere, with the angle theta equalling zero when pointing towards
  // increasing y value, increasing anticlockwise on the sphere such that
  // theta is 90 degrees when the vector is pointed along x.
  //
  //  tan(theta) = sin(x) / tan(y)   [spherical equivalent of tan(t)=x/y]
  //  cos(radius) = cos(y) * cos(x)  [spherical equivalent of r=sqrt(x*x+y*y)]

  if(fabs(y_) < tiny) {  // theta = pi/2, radius = x 

    cos_theta_ = 0.0;
    sin_theta_ = 1.0;
    cos_radius_ = cos(x_);
    sin_radius_ = sin(x_);
    active_ = sin_radius_ != 0.0;

  } else {

    double sin_x = sin(x_);
    double tan_y = tan(y_);

    if(sin_x == 0.0 && tan_y == 0.0) { // theta = 0, radius = 0 

      cos_theta_ = 1.0;
      sin_theta_ = 0.0;
      cos_radius_ = 1.0;
      sin_radius_ = 0.0;
      active_ = false;

    } else {

      double theta = atan2(sin_x, tan_y);
      cos_theta_ = cos(theta);
      sin_theta_ = sin(theta);
      cos_radius_ = cos(y_) * cos(x_);
      sin_radius_ = sin(acos(cos_radius_));
      active_ = sin_radius_ != 0.0;

    };
  };
}

/**.......................................................................
 * Add in any position-independent sky offsets.
 *
 * Input:
 *  f     PointingCorrections *  The corrected az,el and latitude.
 */
void SkyOffset::apply(PointingCorrections *f)
{
  DBPRINT(true, Debug::DEBUG3, "Applying offsets");

  if(active_) {
    DBPRINT(true, Debug::DEBUG3, "Applying offsets: active");
    double sin_el = cos_radius_ * f->sin_el +
      sin_radius_ * f->cos_el * cos_theta_;

    // Compute the change in the azimuth (daz). Note that the fact that
    // trk_sky_offset() set sky->active true means that sky->sin_radius
    // is known to be non-zero, so we don't need to trap for that possibility
    // here.

    double top = sin_theta_ * sin_radius_;
    double bot = f->cos_el * cos_radius_ -
      f->sin_el * sin_radius_ * cos_theta_;
    double daz = (top != 0.0 && bot != 0.0) ? atan2(top, bot) : 0.0;

    // Record the tilted elevation and its trig terms.

    f->sin_el = sin_el;
    f->el = asin(sin_el);
    f->cos_el = cos(f->el);

    // Compute the tilted azimuth and its trig terms.

    f->az += daz;
    f->sin_az = sin(f->az);
    f->cos_az = cos(f->az);

    DBPRINT(true, Debug::DEBUG3, "Just applied offsets: x_ = " << x_
	    << ", y_ = " << y_);
  };
}

/**.......................................................................
 * Pack offsets for archival in the register database.
 */
void SkyOffset::pack(signed* s_elements)
{
  s_elements[0] = static_cast<signed>(x_ * rtomas);
  s_elements[1] = static_cast<signed>(y_ * rtomas);
}
