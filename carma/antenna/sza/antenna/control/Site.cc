#include <cmath>

#include "carma/szautil/Coordinates.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/Vector.h"

#include "carma/antenna/sza/antenna/control/Site.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::util;


/**.......................................................................
 * Constructor.
 */
Site::Site()
{
  reset();
}

/**.......................................................................
 * A public method to set fiducial site parameters
 */
void Site::setFiducial(Angle longitude, Angle latitude, double altitude)
{
  // Set the new fiducial values to those that were passed.

  if(set_Site(&fiducial_, longitude.radians(), latitude.radians(), altitude))
    throw Error("Site::reset: Error in set_Site().\n");

  DBPRINT(true, Debug::DEBUG13, "Got longitude = " << longitude
	  << ", latitude = " << latitude
	  << ", altitude = " << altitude);
	  
  // And calculate a new actual position for this antenna, based on the
  // passed arguments.

  Vector<double> llaVals = 
    Coordinates::llaAndUenToLla(longitude, latitude, altitude, up_, east_, north_);

  DBPRINT(true, Debug::DEBUG13, "Calc: up = " << up_
	  << ", east  = " << east_
	  << ", north = " << north_
	  << ", longitude = " << llaVals[0]
	  << ", latitude  = " << llaVals[1]
	  << ", altitude  = " << llaVals[2]);

  if(set_Site(&actual_, llaVals[0], llaVals[1], llaVals[2]))
    throw Error("Site::reset: Error in set_Site().\n");
}

/**.......................................................................
 * A public method to set offset site parameters
 */
void Site::setOffset(double up, double east, double north)
{
  // Set internal offsets to the passed values

  up_    = up;
  east_  = east;
  north_ = north;

  // And update the actual antenna position based on the new offsets

  Angle lon, lat;

  lon.setRadians(fiducial_.longitude);
  lat.setRadians(fiducial_.latitude);

  Vector<double> llaVals = 
    Coordinates::llaAndUenToLla(lon, lat, fiducial_.altitude, up, east, north);

  if(set_Site(&actual_, llaVals[0], llaVals[1], llaVals[2]))
    throw Error("Site::reset: Error in set_Site().\n");
}

/**.......................................................................
 * Reset site parameters to default values.
 */
void Site::reset()
{
  if(set_Site(&fiducial_, 0.0, 0.0, 0.0))
    throw Error("Site::reset: Error in set_Site().\n");

  if(set_Site(&actual_, 0.0, 0.0, 0.0))
    throw Error("Site::reset: Error in set_Site().\n");

  up_    = 0.0;
  east_  = 0.0;
  north_ = 0.0;
}

/**.......................................................................
 * Return the local sidereal time for a given site and UTC.
 *
 * This is a public wrapper around mjd_utc_to_lst so we never have to
 * expose actual_ to other classes.
 */
double Site::convertMjdUtcToLst(double utc, double ut1utc, double eqneqx)
{
  // Use the actual position of this antenna

  DBPRINT(false, Debug::DEBUG13, 
	  actual_.longitude << ", " 
	  << actual_.latitude << ", " 
	  << actual_.altitude);

  return sza::array::mjd_utc_to_lst(utc, &actual_, ut1utc, eqneqx);
}

/**.......................................................................
 * Correct the azimuth and elevation for horizontal parallax.
 *
 * Input:
 *  dist     double    The distance of the source (AU), or 0.0 if the
 *                     distance can be assumed to be infinite.
 * Input/Output:
 *  f   PointingCorrections *  The az/el pointing to be corrected.
 */
void Site::applyParallax(double dist, PointingCorrections* f)
{
  // Use the actual position of this antenna

  if(f->cos_el != 0.0 && dist > actual_.rcent) {
    f->el -= atan2(f->cos_el, dist/actual_.rcent - f->sin_el);
    f->sin_el = sin(f->el);
    f->cos_el = cos(f->el);
  }
}

/**.......................................................................
 * Correct the azimuth and elevation for diurnal aberration.
 *
 * Input/Output:
 *  f    PointingCorrections *  The az/el pointing to be corrected.
 */
void Site::applyDiurnalAberration(PointingCorrections *f)
{
  // Define a rectangular coordinate system with z towards zenith, x towards
  // the southern horizon and y towards the Eastern horizon. Then,

  double psi;                 // tan(psi) = x/z 
  double theta;               // cos(theta) = y 
  double x;                   // x = -cos(el)*cos(az) 
  double cos_el = f->cos_el;  // cos(el) 
  double sin_el = f->sin_el;  // z = sin(el) 
  double cos_az = f->cos_az;  // cos(az) 
  double sin_az = f->sin_az;  // sin(az) 
  double cos_theta;           // cos(theta) 
  double sin_theta;           // sin(theta) 
  double cos_psi;             // cos(psi) 
  double sin_psi;             // sin(psi) 
  const double tiny = 1.0e-7; // A small angle 

  // Compute theta as defined above.

  cos_theta = cos_el * sin_az;
  theta = acos(cos_theta);
  sin_theta = sin(theta);

  // If the source is along the direction of the tangential velocity
  // vector then the azimuth and elevation are unaffected.

  if(fabs(theta) < tiny)
    return;

  // Compute psi as defined above.

  x = -cos_el * cos_az;
  if(x != 0.0 || sin_el != 0.0)
    psi = atan2(x, sin_el);
  else
    psi = 0.0;
  cos_psi = cos(psi);
  sin_psi = sin(psi);

  // The aberration causes the source to appear to shift towards
  // the direction of the site's velocity. Since the tangential
  // velocity of the Earth is directed along the y axis, this
  // results in a reduction of theta.

  theta -= actual_.velocity/cvel * sin_theta; 
  sin_theta = sin(theta);
  cos_theta = cos(theta);

  // Compute the modified azimuth and elevation.

  f->sin_el = sin_theta * cos_psi;
  f->el = asin(f->sin_el);
  f->cos_el = cos(f->el);

  // Compute the new azimuth, unless pointing at the zenith. The
  // azimuth is irrelevant at the zenith.

  {
    double sin_theta_sin_psi = sin_theta * sin_psi;
    if(cos_theta != 0.0 || sin_theta_sin_psi != 0.0)
      f->az = wrap2pi(atan2(cos_theta, -sin_theta_sin_psi));
    else
      f->az = 0.0;
    f->sin_az = sin(f->az);
    f->cos_az = cos(f->az);
  };
}

/**.......................................................................
 * Install the latitude in the pointing corrections container.
 */
void Site::updateLatitude(PointingCorrections* f)
{
  // Use the actual position of this antenna

  f->lat     = actual_.latitude;
  f->sin_lat = actual_.sin_lat;
  f->cos_lat = actual_.cos_lat;
}

/**.......................................................................
 * Pack fiducial site-specific data for archival in the register database.
 */
void Site::packActual(signed* s_elements)
{
  // milli-arcsec 

  s_elements[0] = static_cast<signed>(actual_.longitude * rtomas);

  // milli-arcsec 

  s_elements[1] = static_cast<signed>(actual_.latitude  * rtomas);

  // mm 

  s_elements[2] = static_cast<signed>(actual_.altitude  * 1000.0);
}

/**.......................................................................
 * Pack fiducial site-specific data for archival in the register database.
 */
void Site::packFiducial(signed* s_elements)
{
  // milli-arcsec 

  s_elements[0] = static_cast<signed>(fiducial_.longitude * rtomas);

  // milli-arcsec 

  s_elements[1] = static_cast<signed>(fiducial_.latitude  * rtomas);

  // mm 

  s_elements[2] = static_cast<signed>(fiducial_.altitude  * 1000.0);
}

/**.......................................................................
 * Pack offset site-specific data for archival in the register
 * database.
 */
void Site::packOffset(signed* s_elements)
{
  // mm

  s_elements[0] = static_cast<signed>(up_* 1000.0);

  // mm

  s_elements[1] = static_cast<signed>(east_ * 1000.0);

  // mm 

  s_elements[2] = static_cast<signed>(north_ * 1000.0);
}

/**.......................................................................
 * Round an angle into the range 0-2.pi. 
 *
 * Input:
 *  angle    double   The angle to be rounded (radians).
 *
 * Output:
 *  return   double   An angle in the range:  0 <= angle < 2.pi.
 */
double Site::wrap2pi(double angle)
{
  return (angle >= 0.0) ? (angle - twopi * floor(angle/twopi)) :
			  (angle + twopi * ceil(-angle/twopi));
}
