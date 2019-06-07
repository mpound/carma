#include "carma/szautil/Ellipsoid.h"

#include <cmath>

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
Ellipsoid::Ellipsoid() 
{
  initialized_ = false;
}

/**.......................................................................
 * Constructor.
 */
Ellipsoid::Ellipsoid(Length majorAxis, Length minorAxis) 
{
  setMajorMinorAxis(majorAxis, minorAxis);
}

Ellipsoid::Ellipsoid(Length majorAxis, double flattening)
{
  setMajorAxisAndFlattening(majorAxis, flattening);
}

/**.......................................................................
 * Destructor.
 */
Ellipsoid::~Ellipsoid() {}

/**.......................................................................
 * Return the flattening parameter of an ellipsoid.
 *
 * Flattening is defined to be: f = (a - b)/a = 1 - b/a
 *
 */
double Ellipsoid::flattening()
{
  checkInitialization();
  return 1.0 - (b_/a_);
}

/**.......................................................................
 * Return the major axis of an ellipsoid
 */
Length Ellipsoid::majorAxis()
{
  checkInitialization();
  return a_;
}

/**.......................................................................
 * Return the minor axis of an ellipsoid
 */
Length Ellipsoid::minorAxis()
{
  checkInitialization();
  return b_;
}

/**.......................................................................
 * Return the first eccentricity e
 */
double Ellipsoid::firstEccentricity()
{
  return sqrt(firstEccentricitySquared());
}
 
double Ellipsoid::firstEccentricitySquared()
{
  double f = flattening();
  return f * (2.0 - f);
}

double Ellipsoid::secondEccentricity()
{
  return sqrt(secondEccentricitySquared());
}

double Ellipsoid::secondEccentricitySquared()
{
  double f = flattening();
  double fac = 1.0 - f;
  return f * (2.0 - f) / (fac*fac);
}

/**.......................................................................
 * Return the length of the radius vector at a given ellipsoidal
 * latitude.
 *
 * From the equation of an ellipse, we have:
 *
 *      x^2   y^2   r^2          /       a^2           \      r^2          /       tan^2(l)   \ 
 *  1 = --- + --- = --- cos^2(l) |  1 +  --- tan^2(l)  |  =   --- cos^2(l) |  1 +  ---------   | 
 *      a^2   b^2   a^2          \       b^2           /      a^2          \       (1 - f)^2  / 
 *
 */
Length Ellipsoid::radius(Angle latitude)
{
  double clat = cos(latitude.radians());
  double slat = sin(latitude.radians());
  double tlat = slat / clat;
  double f = flattening();

  double fac = 1.0/sqrt((clat * clat) * (1 + tlat * tlat / ((1.0 - f) * (1.0 - f))));

  return majorAxis() * fac;
}

void Ellipsoid::setMajorAxisAndFlattening(Length majorAxis, double flattening)
{
  Length minorAxis = majorAxis * (1.0 - flattening);
  setMajorMinorAxis(majorAxis, minorAxis);
}

void Ellipsoid::setMajorMinorAxis(Length majorAxis, Length minorAxis)
{
  a_ = majorAxis;
  b_ = minorAxis;
  initialized_ = true;
}

void Ellipsoid::checkInitialization()
{
  if(!initialized_) {
    ThrowError("Ellipse parameters have not been specified");
  }
}

