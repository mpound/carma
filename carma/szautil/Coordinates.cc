#include "carma/szautil/Coordinates.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/Matrix.h"

#include "carma/szaarrayutils/szaconst.h"

#include <cmath>

using namespace std;
using namespace sza::util;

#ifdef EPS
#undef EPS
#endif

#define EPS 1e-15

// Radius of the earth in meters, from Explanatory Supplement, p. 161

const double Coordinates::earthEqRadiusMeters_ = 6.378140e6;


// Flattening of the earth, from Explanatory Supplement, p. 161
// Flattening is defined as:
//
//        f =  (a - b)/a = 1 - b/a,
//
// where a and b are the semi-major and semi-minor axes, respectively
// of the ellipsoid.
 
const double Coordinates::earthFlattening_ = 1.0/298.257;

// Speed of light in meters/sec

const double Coordinates::lightSpeed_ = 2.99792458e8;

// Scale AU to meters, from Explanatory Supplement, p. 163


const double Coordinates::auToMeters_ = 1.49597870e11;

// Earth rotational velocity, in radians per second, from Explanatory
// Supplement, p. 162

const double Coordinates::earthRotVelRadPerSec_ = 7.2921151467e-5;

/**.......................................................................
 * Constructor.
 */
Coordinates::Coordinates() 
{
  longitude_.setRadians(0.0);
  latitude_.setRadians(0.0);
  altitude_ = 0.0;
}

/**.......................................................................
 * Constructor with initialization of lat long and alt
 */
Coordinates::Coordinates(Angle longitude, Angle latitude, double altitude) 
{
  setLongitude(longitude);
  setLatitude(latitude);
  setAltitude(altitude);
}

/**.......................................................................
 * Constructor with initialization of lat long and alt
 */
Coordinates::Coordinates(Coordinates& coords)
{
  setLongitude(coords.longitude());
  setLatitude(coords.latitude());
  setAltitude(coords.altitude());
}

/**.......................................................................
 * Destructor.
 */
Coordinates::~Coordinates() {}

/**.......................................................................
 * Set methods
 */
void Coordinates::setLla(Angle longitude, Angle latitude, double altitude)
{
  setLongitude(longitude);
  setLatitude(latitude);
  setAltitude(altitude);
}

void Coordinates::setLongitude(Angle longitude)
{
  longitude_ = longitude;
}

void Coordinates::setLatitude(Angle latitude)
{
  latitude_ = latitude;
}

void Coordinates::setAltitude(double altitude)
{
  altitude_ = altitude;
}

/**.......................................................................
 * Return the (X, Y, Z) interferometry coordinates of a point specified
 * in (E, N, U) coordinates, relative to the latitude and longitude of
 * this object.
 *
 * Optional arguments:
 * 
 *   east  (m)
 *   north (m)
 *   up    (m)
 */
Vector<double> Coordinates::getAbsXyz(double up, double east, double north)
{
  return llaAndUenToAbsXyz(longitude_, latitude_, altitude_, up, east, north);
}

/**.......................................................................
 * Return the XYZ interferometry coordinates of a point specified in
 * UEN coordinates, relative to the LLA of this object.
 *
 * Optional arguments:
 * 
 *   east  (m)
 *   north (m)
 *   up    (m)
 *   geocentric -- If true, return geocentric (earth-centered) coordinates, 
 *                 if false, return topocentric (surface-relative) coords.
2 */
Vector<double> Coordinates::getXyz(double up, double east, double north, 
				   bool geocentric)
{
  return laAndUenToXyz(latitude_, altitude_, up, east, north, geocentric);
}

/**.......................................................................
 * Return (E, N, U) coordinates of the specified LLA point, relative
 * to the coordinates stored in this object.
 */
Vector<double> Coordinates::getUen(Coordinates& coords)
{
  return getUen(coords.longitude(), coords.latitude(), coords.altitude());
}

/**.......................................................................
 * Return the (E, N, U) coordinates of a point specified in (L, L, A)
 * coordinates, relative to the latitude and longitude of this object.
 *
 * Optional arguments:
 * 
 *   longitude (rad)
 *   latitude  (rad)
 *   altitude  (m)
 */
Vector<double> Coordinates::getUen(Angle longitude, Angle latitude, double altitude)
{
  return llaAndLlaToUen(longitude_, latitude_, altitude_,
			longitude, latitude, altitude);
}

/**.......................................................................
 * Return the (U, V, W) coordinates of requested source position,
 * relative to the position in this object.
 *
 * Optional arguments:
 * 
 *   longitude (rad)
 *   latitude  (rad)
 *   altitude  (m)
 */
Vector<double> Coordinates::getUvw(HourAngle ha, DecAngle declination)
{
  Vector<double> xyzVals = getXyz();

  return haDecAndXyzToUvw(ha, declination, xyzVals[0], xyzVals[1], xyzVals[2]);
}

//-----------------------------------------------------------------------
// Conversions
//-----------------------------------------------------------------------

/**.......................................................................
 * Given absolute XYZ, convert to LLA
 */
Vector<double> Coordinates::absXyzToLla(double XA, double YA, double ZA)
{
  Vector<double> llaVals(3), xyzVals(3);

  xyzVals[0] = XA;
  xyzVals[1] = YA;
  xyzVals[2] = ZA;

  // Longitude is the angle of the projection onto the X-Y plane,
  
  llaVals[0] = atan2(YA, XA);
  
  // Latitude is the angle between the coordinate vector and the Z axis
  
  llaVals[1] = M_PI/2 - acos(ZA/xyzVals.magnitude());
  
  // Altitude is the magnitude of the coordinate vector, less the
  // radius of the earth
  
  llaVals[2] = xyzVals.magnitude() - earthEqRadiusMeters_;
  
  return llaVals;
}

/**.......................................................................
 * Given a fiducial LLA and an UEN offset relative to it, return the
 * latitude, longitude, and altitude.
 *
 *   latitude       (rad)
 *   east longitude (rad)
 *   altitude       (m)
 *
 * And optionally:
 * 
 *   east  (m)
 *   north (m)
 *   up    (m)
 */
Vector<double> Coordinates::llaAndUenToLla(Angle longitude, Angle latitude, 
					   double altitude, 
					   double up, double east, double north)
{
  // Convert from LLA and UEN to absolute XYZ coordinates

  Vector<double> xyzVals = 
    llaAndUenToAbsXyz(longitude, latitude, altitude, up, east, north);
  
  double XA,YA,ZA;
  
  XA = xyzVals[0];
  YA = xyzVals[1];
  ZA = xyzVals[2];
  
  return absXyzToLla(XA, YA, ZA);
}

/**.......................................................................
 * Given a fiducial LL and an AZ/EL offset relative to it, return the
 * latitude and longitude of the offset direction
 *
 *   latitude       (rad)
 *   east longitude (rad)
 *   az             (rad)
 *   el             (rad)
 */
Vector<double> Coordinates::
lngLatAndAzElToLngLat(Angle& lng,       Angle& lat,
		      Angle& az,        Angle& el)

{
  // Get the UEN coordinates corresponding to this az/el offset

  Vector<double> uen = azElToUen(az, el);

  // Convert from LL and UEN to absolute XYZ coordinates

  Vector<double> xyzVals = 
    llaAndUenToAbsXyz(lng, lat, 0.0, uen[0], uen[1], uen[2], false);
  
  double XA,YA,ZA;
  
  XA = xyzVals[0];
  YA = xyzVals[1];
  ZA = xyzVals[2];
  
  return absXyzToLla(XA, YA, ZA);
}

/**.......................................................................
 * Given a fiducial absolute XYZ and an UEN offset relative to it,
 * return the latitude, longitude, and altitude.
 *
 *   XA (m)
 *   YA (m)
 *   ZA (m)
 *
 * And optionally:
 * 
 *   east  (m)
 *   north (m)
 *   up    (m)
 */
Vector<double> Coordinates::absXyzAndUenToLla(double XA, double YA, double ZA,
					      double up, double east, double north)
{
  // Convert from absolute XYZ to LLA

  Vector<double> llaVals = absXyzToLla(XA, YA, ZA);

  // And add the UEN offset

  Angle lon, lat;
  lon.setRadians(llaVals[0]);
  lat.setRadians(llaVals[1]);

  return llaAndUenToLla(lon, lat, llaVals[2], up, east, north);
}

/**.......................................................................
 * Given N, E, U relative to the coordinates in this object, return
 * the latitude, longitude, and altitude.
 *
 * Optionally:
 * 
 *   east  (m)
 *   north (m)
 *   up    (m)
 */
Vector<double> Coordinates::getLla(double up, double east, double north)
{
  return llaAndUenToLla(longitude_, latitude_, altitude_, up, east, north);
}

/**.......................................................................
 * Given a (L, L, A) coordinate, return its Az, El relative to the
 * position of this object
 */
Vector<Angle> Coordinates::getAzEl(Coordinates& coords)
{
  return getAzEl(coords.longitude(), coords.latitude(), coords.altitude());
}


/**.......................................................................
 * Given a (L, L, A) coordinate, return its Az, El relative to the
 * position of this object
 */
Vector<Angle> Coordinates::getAzEl(Angle longitude, Angle latitude, 
				    double altitude)
{
  Vector<double> uenVals = getUen(longitude, latitude, altitude);

  double U = uenVals[0];
  double E = uenVals[1];
  double N = uenVals[2];

  return uenToAzEl(U, E, N);
}

/**.......................................................................
 * Given Azimuth and Elevation, return the UEN coordinates.  Note that
 * with r==1.0, this function returns the direction cosines of the
 * position specified by az/el.
 */
Vector<double> Coordinates::azElToUen(Angle az, Angle el, double r)
{
  Vector<double> uenVals(3);

  // cos(el) is the projection of the vector onto the E-N plane
  
  double cel = cos(el.radians());

  uenVals[0] = r * sin(el.radians());       // U component
  uenVals[1] = r * cel * sin(az.radians()); // E component
  uenVals[2] = r * cel * cos(az.radians()); // N component

  return uenVals;
}

/**.......................................................................
 * Given UEN, return the Azimuth and Elevation
 */
Vector<Angle> Coordinates::uenToAzEl(double U, double E, double N)
{
  Vector<Angle> azelVals(2);
  Vector<double> uenVals(3);

  uenVals[0] = U;
  uenVals[1] = E;
  uenVals[2] = N;

  // Azimuth is the angle of the projection onto the N-E plane
  
  azelVals[0].setRadians(atan2(E, N));
  
  // Elevation is 90 - the angle between the coordinate vector and the U axis
  
  azelVals[1].setRadians(asin(U/uenVals.magnitude()));
  
  return azelVals;
}

/**.......................................................................
 * Given UEN, return the Azimuth and Elevation
 */
Vector<Angle> Coordinates::uenToAzEl(Vector<double>& uen)
{
  return uenToAzEl(uen[0], uen[1], uen[2]);
}

/**.......................................................................
 * Given NEU and the LLA of this object, return the latitude,
 * longitude, and altitude.
 *
 *   east  (m)
 *   north (m)
 *   up    (m)
 */
void Coordinates::add(double up, double east, double north)
{
  Vector<double> vec = llaAndUenToLla(longitude_, latitude_, altitude_, 
				      up, east, north);

  Angle lon, lat;
  lon.setRadians(vec[0]);
  lat.setRadians(vec[1]);

  setLongitude(lon);
  setLatitude(lat);
  setAltitude(vec[2]);
}

/**.......................................................................
 * Return the longitude rotation matrix for transforming between (N,
 * E, U) and (X, Y, Z)
 */
sza::util::Matrix<double> Coordinates::getUenToXyzLonRot(Angle longitude)
{
  sza::util::Matrix<double> lonRot(3,3);
  
  // Precompute sines and cosines
  
  double clon = cos(longitude.radians());
  double slon = sin(longitude.radians());

  // Construct a rotation matrix for the longitude rotation
    
  //  lonRot[0][0] = -slon; lonRot[0][1] = -clon; lonRot[0][2] =  0;
  //  lonRot[1][0] =  clon; lonRot[1][1] = -slon; lonRot[1][2] =  0;
  //  lonRot[2][0] =     0; lonRot[2][1] =     0; lonRot[2][2] =  1;

  lonRot[0][0] =  clon; lonRot[0][1] = -slon; lonRot[0][2] =  0;
  lonRot[1][0] =  slon; lonRot[1][1] =  clon; lonRot[1][2] =  0;
  lonRot[2][0] =     0; lonRot[2][1] =     0; lonRot[2][2] =  1;
  
  return lonRot;  
}

/**.......................................................................
 * Return the latitude rotation matrix to convert from (U, E, N)
 * coordinates to (X, Y, Z)
 */
sza::util::Matrix<double> Coordinates::getUenToXyzLatRot(Angle latitude)
{
  sza::util::Matrix<double> latRot(3,3);
  
  // Precompute sines and cosines
  
  double clat = cos(latitude.radians());
  double slat = sin(latitude.radians());

  // Construct a rotation matrix for the latitude rotation

  latRot[0][0] =  clat; latRot[0][1] =     0; latRot[0][2] = -slat;
  latRot[1][0] =     0; latRot[1][1] =     1; latRot[1][2] =     0;
  latRot[2][0] =  slat; latRot[2][1] =     0; latRot[2][2] =  clat;
  
  return latRot;
}

/**.......................................................................
 * Return absolute X, Y, Z interferometry coordinates, given:
 *
 *   latitude (rad)
 *   east longitude (rad)
 *   altitude (m)
 *
 * And optionally:
 * 
 *   east (m)
 *   north (m)
 *   up (m)
 */
Vector<double> Coordinates::llaAndUenToAbsXyz(Angle longitude, Angle latitude, double altitude, 
					      double up, double east, double north, bool doTrans)
{
  Matrix<double> latRot, lonRot;
  Vector<double> translation;
  Vector<double> inputCoords;
  Vector<double> outputCoords;
  
  // Get rotation matrices

  latRot = getUenToXyzLatRot(latitude);
  lonRot = getUenToXyzLonRot(longitude);
  
  // Construct a translation vector to put us in earth-center relative
  // coordinates
  
  translation.resize(3);
  translation[0] = earthEqRadiusMeters_;
  translation[1] = 0.0;
  translation[2] = 0.0;
  
  // Construct a vector of input coordinates
  
  inputCoords.resize(3);
  inputCoords[0] = altitude + up;
  inputCoords[1] = east;
  inputCoords[2] = north;
  
  // Calculate the XYZ coordinates
  
  Vector<double> vec = (doTrans ? (inputCoords + translation) : (inputCoords));
  Matrix<double> rot = (lonRot * latRot);

  //  Matrix<double> rot = (lonRot * latRot);
  
  outputCoords = rot * vec;

  return outputCoords;
}

/**.......................................................................
 * Given a topocentric XYZ, return the UEN coordinates at a
 * given latitude
 */
Vector<double> Coordinates::topoXYZToUen(double X, double Y, double Z, 
					 Angle latitude)
{
  // Get the rotation matrix to take us from XYZ to UEN

  Matrix<double> uenToXyzLatRot = getUenToXyzLatRot(latitude);
  Matrix<double> xyzToUenLatRot = uenToXyzLatRot.inverse();

  Vector<double> xyzVals(X, Y, Z), uenVals;

  uenVals = (xyzToUenLatRot * xyzVals);
  
  return uenVals;
}

/**.......................................................................
 * Return TMS XYZ interferometry coordinates, given:
 *
 *   latitude       (rad)
 *   altitude       (m)
 *
 * And optionally:
 * 
 *   east  (m)
 *   north (m)     -- An offset relative to the fiducial LLA point.
 *   up    (m)
 *
 *   geocentric -- True if the coordinates returned should be relative to
 *                 the center of the earth (geocentric).  
 *                 Use 'false' if you want XYZ coordinates relative to the 
 *                 surface of the earth (topocentric)
 */
Vector<double> Coordinates::laAndUenToXyz(Angle latitude, double altitude, 
					  double up, double east, double north,
					  bool geocentric)
{
  Matrix<double> latRot;
  Vector<double> outputCoords;
  
  // Get rotation matrices

  latRot = getUenToXyzLatRot(latitude);

  // Construct a translation vector to put us in earth-center relative
  // coordinates
  
  Vector<double> translation(geocentric ? earthEqRadiusMeters_ : 0.0, 0.0, 0.0);
  
  // Construct a vector of input coordinates
  
  Vector<double> inputCoords(altitude + up, east, north);
  
  // Calculate the XYZ coordinates.  It is simply given by a latitude
  // rotation.
  
  Vector<double> vec = (inputCoords + translation);
  Matrix<double> rot = latRot;
  
  outputCoords = rot * vec;

  return outputCoords;
}

/**.......................................................................
 * Return the UEN coordinates of a LLA point, relative to a fiducial
 * LLA point
 */
Vector<double> Coordinates::
llaAndLlaToUen(Angle fiducialLongitude, Angle fiducialLatitude, double fiducialAltitude, 
	       Angle longitude, Angle latitude, double altitude)
{
  // Get the XYZ coordinates of the requested point

  Vector<double> xyzVals = 
    llaAndUenToAbsXyz(longitude, latitude, altitude, 0.0, 0.0, 0.0);

  // Get the rotation matrix to convert from XYZ to UEN

  Matrix<double> lonRot = getUenToXyzLonRot(fiducialLongitude);
  Matrix<double> latRot = getUenToXyzLatRot(fiducialLatitude);
  Matrix<double> rot    = (lonRot * latRot), inv = rot.inverse();
  
  // Construct a translation vector to put us in earth-center relative
  // coordinates
  
  Vector<double> translation(earthEqRadiusMeters_, 0.0, 0.0);
  
  // Now calculate the UEN coordinates
  
  Vector<double> uenVals(3), retVals(3);

  uenVals = (inv * xyzVals) - translation;

  // Subtract off the fiducial altitude to convert to U

  retVals[0] = uenVals[0] - fiducialAltitude; // U
  retVals[1] = uenVals[1]; // E
  retVals[2] = uenVals[2]; // N

  return retVals;
}

/**.......................................................................
 * Return the (U, V, W) coordinates of the specified (X, Y, Z) point.  
 *
 * Optional arguments:
 * 
 *   longitude (rad)
 *   latitude  (rad)
 *   altitude  (m)
 */
Vector<double> Coordinates::haDecAndXyzToUvw(HourAngle ha, DecAngle declination,
					     double X, double Y, double Z)
{
  Vector<double> xyzVals(3);

  xyzVals[0] = X;
  xyzVals[1] = Y;
  xyzVals[2] = Z;

  // Get the rotation matrix for the coordinates of this object

  Matrix<double> decRot = getXyzToUvwDecRot(declination);
  Matrix<double> haRot  = getXyzToUvwHaRot(ha);
  Matrix<double> rot    = (decRot * haRot);
  
  // Now calculate the UVW coordinates
  
  Vector<double> uvwVals(3);

  uvwVals = (rot * xyzVals);

  return uvwVals;
}

/**.......................................................................
 * Return the (X, Y, Z) coordinates of the point offset by the
 * specified dHa and dDec
 */
Vector<double> Coordinates::dHaDdecToXyz(HourAngle dHa, Angle dDec)
{
  double sdec = sin(dDec.radians());
  double cdec = cos(dDec.radians());
  double sha  = sin(dHa.radians());
  double cha  = cos(dHa.radians());

  // Now calculate the XYZ coordinates
  
  Vector<double> xyzVals(3);

  xyzVals[0] = sdec*cha;
  xyzVals[1] = sdec*sha;
  xyzVals[2] = cdec;

  return xyzVals;
}

/**.......................................................................
 * Return the dHa and dDec corresponding to the specified (X, Y, Z)
 * coordinate
 */
void Coordinates::xyzToDhaDdec(double X, double Y, double Z, HourAngle& dHa, Angle& dDec)
{
  Vector<double> xyzVals(3);

  xyzVals[0] = X;
  xyzVals[1] = Y;
  xyzVals[2] = Z;

  return xyzToDhaDdec(xyzVals, dHa, dDec);
}

/**.......................................................................
 * Return the dHa and dDec (from pi/2) corresponding to the specified
 * (X, Y, Z) coordinate
 */
void Coordinates::xyzToDhaDdec(Vector<double>& xyzVals, HourAngle& dHa, Angle& dDec)
{
  dHa.setRadians(atan2(xyzVals[1], xyzVals[0]));
  dDec.setRadians(-acos(xyzVals[2])/xyzVals.magnitude());
}

/**.......................................................................
 * Given fiducial position on the sky (assumed to be at 0h), and a
 * polar offset (theta, rho) from it (where theta is measured
 * clockwise looking out on the sky), return the ra, dec coordinates
 * of the new location.
 */
void Coordinates::raDecAndThetaRhoToRaDec(HourAngle& ra0, Declination& dec0, 
					  Angle& theta, Angle& rho, 
					  HourAngle& ra, Declination& dec)
{
  // Get the XYZ vals corresponding to this polar offset

  Vector<double> xyzVals = thetaRhoToXyz(dec0, theta, rho);

  // Now calculate the delta HA and DEC that correspond to this XYZ
  // coordinate

  HourAngle dHa;
  Angle dDec;

  xyzToDhaDdec(xyzVals, dHa, dDec);

  // Finally, increment the center ra dec location by the offsets

  ra  =  ra0 +  dHa;
  dec.setRadians(M_PI/2);
  dec = dec + dDec;
}

/**.......................................................................
 * Given polar offsets on the sky from a given dec position (assumed
 * to be at HA=0h), theta (azimuthal) and rho (radial), return the XYZ
 * direction cosines.
 */
Vector<double> Coordinates::thetaRhoToXyz(Declination& dec, Angle& theta, Angle& rho)
{
  // Construct the three rotation matrices we need to go from the
  // polar offset position to the XYZ position

  Matrix<double> rhoRot   = getThetaRhoToXyzRhoRot(rho);
  Matrix<double> thetaRot = getThetaRhoToXyzThetaRot(theta);
  Matrix<double> decRot   = getThetaRhoToXyzDecRot(dec);


  Matrix<double> rot1 = (thetaRot * rhoRot);


  Matrix<double> rot2 = (decRot * rot1);

  Vector<double> initVals(3);
  initVals[0] = 0.0;
  initVals[1] = 0.0;
  initVals[2] = 1.0;

  Vector<double> xyzVals(3);

  xyzVals = (rot2 * initVals);

  if(fabs(xyzVals[0]) < EPS)
    xyzVals[0] = 0.0;
  if(fabs(xyzVals[1]) < EPS)
    xyzVals[1] = 0.0;
  if(fabs(xyzVals[2]) < EPS)
    xyzVals[2] = 0.0;
    
  return xyzVals;
}

Matrix<double> Coordinates::getThetaRhoToXyzRhoRot(Angle& rho)
{
  double sr = sin(rho.radians());
  double cr = cos(rho.radians());

  Matrix<double> rot(3,3);

  // Construct a rotation matrix for the rho rotation
  
  rot[0][0] =  cr; rot[0][1] =     0; rot[0][2] =  sr;
  rot[1][0] =   0; rot[1][1] =     1; rot[1][2] =   0;
  rot[2][0] = -sr; rot[2][1] =     0; rot[2][2] =  cr;

  return rot;
}

Matrix<double> Coordinates::getThetaRhoToXyzThetaRot(Angle& theta)
{
  double st = sin(theta.radians());
  double ct = cos(theta.radians());

  Matrix<double> rot(3,3);

  // Construct a rotation matrix for the theta rotation
  
  rot[0][0] =  ct; rot[0][1] = -st; rot[0][2] = 0;
  rot[1][0] =  st; rot[1][1] =  ct; rot[1][2] = 0;
  rot[2][0] =   0; rot[2][1] =   0; rot[2][2] = 1;

  return rot;
}

Matrix<double> Coordinates::getThetaRhoToXyzDecRot(Declination& dec)
{
  double sd = sin(dec.radians());
  double cd = cos(dec.radians());

  Matrix<double> rot(3,3);

  // Construct a rotation matrix for the theta rotation
  
  rot[0][0] =  sd; rot[0][1] =  0; rot[0][2] = cd;
  rot[1][0] =   0; rot[1][1] =  1; rot[1][2] =  0;
  rot[2][0] = -cd; rot[2][1] =  0; rot[2][2] = sd;

  return rot;
}


/**.......................................................................
 * Return the (X, Y, Z) coordinates of the specified (R, Ha, Dec)
 * coordinate.
 */
Vector<double> Coordinates::haDecToXyz(HourAngle ha, DecAngle declination,
				       bool geocentric, SrcDist type, double r)
{
  // Construct the input source coordinate in UVW coordinates

  Vector<double> uvwVals(3);

  uvwVals[0] = 0;
  uvwVals[1] = 0;

  if(type==ACTUAL)
    uvwVals[2] = (r * auToMeters_) - (geocentric ? 0.0 : earthEqRadiusMeters_);
  else
    uvwVals[2] = 1.0;

  // Get the rotation matrix to convert from UVW to XYZ

  Matrix<double> xyzToUvwDecRot = getXyzToUvwDecRot(declination);
  Matrix<double> xyzToUvwHaRot  = getXyzToUvwHaRot(ha);
  Matrix<double> xyzToUvwRot    = (xyzToUvwDecRot * xyzToUvwHaRot);
  Matrix<double> uvwToXyzRot    = xyzToUvwRot.inverse();
  
  // Now calculate the XYZ coordinates
  
  Vector<double> xyzVals(3);

  xyzVals = (uvwToXyzRot * uvwVals);

  return xyzVals;
}

/**.......................................................................
 * Return the (Az, El) coordinates of the specified (HA, Dec) point.  
 */
Vector<Angle> 
Coordinates::laAndHaDecToAzEl(Angle latitude, double altitude, 
			      HourAngle ha, DecAngle declination, 
			      bool geocentric, 
			      SrcDist type, double r)
{
  // Get the geocentric XYZ direction of the source vector.  Passing
  // SrcType INF will return the source direction only.

  Vector<double> geoXyzSrcDir = haDecToXyz(ha, declination, true, INF);

  // Get the geocentric direction of the point of observation

  Vector<double> geoXyzObs = laAndUenToXyz(latitude, altitude, 0.0, 0.0, 0.0,
					   true);

  Vector<double> geoXyzObsDir = geoXyzObs/geoXyzObs.magnitude();

  // Factor of R_obs/R_src, or 0 if source is infinitely far away

  double fac = ((geocentric || type==INF) ? 0.0 : (geoXyzObs.magnitude()/ (r * auToMeters_)));

  // Get the topocentric source direction.  In the case of type==INF,
  // this will be the same as the geocentric

  Vector<double> topoXyzSrcDir = (geoXyzSrcDir - geoXyzObsDir * fac);
  topoXyzSrcDir = topoXyzSrcDir/sqrt(1 - 2*fac*(geoXyzSrcDir*geoXyzObsDir) + fac*fac);

  // Get the rotation matrix to convert from XYZ to UEN

  Matrix<double> uenToXyzRot = getUenToXyzLatRot(latitude);
  Matrix<double> xyzToUenRot = uenToXyzRot.inverse();
  
  // Now convert from XYZ direction to UEN direction
  
  Vector<double> uenSrcDir = (xyzToUenRot * topoXyzSrcDir);

  // And calculate the Az/El

  Vector<Angle> srcAzEl = uenToAzEl(uenSrcDir);

  // Trap Az or El = nan.  This can happen if geoXyzSrc == geoXyzObs,
  // e.g., if the user inputs a source location which exactly
  // coincides with the XYZ of the observation. Although this is an
  // unrealistic situation, it should be noted.

  if(!isfinite(srcAzEl[0].radians()) || !isfinite(srcAzEl[1].radians())) {
    LogStream errStr;
    errStr.initMessage(true);
    errStr << "Az/El position is not defined for Ha = " << ha
	   << ", Dec = " << declination
	   << " at latitude = " << latitude << endl;
    throw Error(errStr);
  } 

  return srcAzEl;
}

/**.......................................................................
 * Return the first derivative of the (U, V, W) coordinates of the
 * specified XYZ point.
 *
 * Optional arguments:
 * 
 *   longitude (rad)
 *   latitude  (rad)
 *   altitude  (m)
 */
Vector<double> Coordinates::getdUvw(HourAngle ha, DecAngle declination, 
				    double X, double Y, double Z)
{
  Vector<double> xyzVals(3);

  xyzVals[0] = X;
  xyzVals[1] = Y;
  xyzVals[2] = Z;

  // Get the rotation matrix for the coordinates of this object

  Matrix<double> decRot = getXyzToUvwDecRot(declination);
  Matrix<double> haRot  = getdXyzToUvwHaRot(ha);
  Matrix<double> rot    = (decRot * haRot);
  
  // Now calculate the Uvw coordinates
  
  Vector<double> uvwVals(3);

  uvwVals = (rot * xyzVals);

  return uvwVals;
}

/**.......................................................................
 * Return the second derivative of the (U, V, W) coordinates of the
 * specified XYZ point.
 *
 * Optional arguments:
 * 
 *   longitude (rad)
 *   latitude  (rad)
 *   altitude  (m)
 */
Vector<double> Coordinates::getd2Uvw(HourAngle ha, DecAngle declination, 
				     double X, double Y, double Z)
{
  Vector<double> xyzVals(3);

  xyzVals[0] = X;
  xyzVals[1] = Y;
  xyzVals[2] = Z;

  // Get the rotation matrix for the coordinates of this object

  Matrix<double> decRot = getXyzToUvwDecRot(declination);
  Matrix<double> haRot  = getd2XyzToUvwHaRot(ha);
  Matrix<double> rot    = (decRot * haRot);
  
  // Now calculate the UVW coordinates
  
  Vector<double> uvwVals(3);

  uvwVals = (rot * xyzVals);

  return uvwVals;
}

/**.......................................................................
 * Calculate the physical delay, in meters, for the specified XYZ
 * point, for a source direction given by az/el.
 */
Delay Coordinates::getGeometricDelay(Angle latitude, 
				     Angle az, Angle el,
				     double X, double Y, double Z)
{
  // Calculate the UEN of the source direcion

  Vector<double> uenDir  = azElToUen(az, el);

  // Get the XYZ of the UEN point

  Vector<double> srcXyzDir = laAndUenToXyz(latitude, 0.0, 
					   uenDir[0], uenDir[1], uenDir[2], 
					   false);

  // Construct a vector of the input baseline coordinates

  Vector<double> xyzVals(3);

  xyzVals[0] = X;
  xyzVals[1] = Y;
  xyzVals[2] = Z;

  // Delay is just the projection of the baseline vector onto the
  // source direction

  // Convention is that positive w == negative delay.  That is, light
  // reaching the tip of the baseline vector has a negative delay
  // relative to the origin of the vector.

  Delay delay;
  delay.setDelayInMeters(-(xyzVals * srcXyzDir));

  return delay;
}

/**.......................................................................
 * Calculate the physical delay between light striking the origin of a
 * baseline (X0, Y0, Z0) and light striking the end of the baseline
 * (X, Y, Z).  If you are not applying the motion correction
 * (retarded baseline correction, in VLBI jargon), your coordinates
 * can be either surface-relative or earth-centered.  If applying the
 * motion correction, coordinates MUST be earth-centered, as it is
 * these coordinates, and not some relative XYZ, which are needed to
 * correctly calculate the derivative of the w coordinate of the
 * baseline endpoint.
 */
Delay Coordinates::getGeometricDelay(HourAngle ha, DecAngle declination, 
				     double X, double Y, double Z,
				     double X0, double Y0, double Z0,
				     bool doMotionCorrection)
{
  // Get the instantaneous dw between the two positions

  Vector<double> uvwVals = haDecAndXyzToUvw(ha, declination, 
					    X-X0, Y-Y0, Z-Z0);
  double w = uvwVals[2], wVel=0;

  // Get the rate of change too

  uvwVals = getdUvw(ha, declination, X-X0, Y-Y0, Z-Z0);
  double dw = uvwVals[2];

  // If we are including the correction for the motion of the earth,
  // we need the instantaneous velocity of the endpoint of the
  // baseline in the w-direction.  
  //
  // Apparently this correction is what VLBI types call the "retarded
  // baseline," discussed by TMS on pg. 315-316 of the 2nd edition.

  if(doMotionCorrection) {
    uvwVals = getdUvw(ha, declination, X, Y, Z);
    wVel = uvwVals[2];
  }

  // The delay is properly given by the speed of light times the time
  // interval which solves the following equation:
  //
  // w = w(0) + wVel(0) * dt + wAccel(0)/2 * dt^2 = c*t
  //
  // or:
  //
  // wAccel(0)/2 * dt^2 + (wVel(0) - c) * dt + w(0) = 0
  //
  // This can be solved using the usual formula for the roots of a
  // quadratic equation; however, without a lot of care, the
  // acceleration term is so small compared to the speed of light that
  // the relevant root almost always ends up 0 to the accuracy of the
  // calculation.  Because the acceleration is always small compared
  // to the velocity term, I shall instead ignore it, which leads to:
  //
  // (wVel(0) - c) * dt + w(0) = 0, or
  //
  // dt = -w(0)/(wVel(0) - c) = w(0)/(c - wVel(0))
  //

  // Convention is that positive w == negative delay.  That is, light
  // reaching the tip of the baseline vector has a negative delay
  // relative to the origin of the vector.  Hence the -ve prefactor
  // before the w.

  Delay delay;
  delay.setDelayInNanoSeconds(-w/(lightSpeed_ - wVel) * 1e9);

  // Set the rate as well

  delay.setDelayRateInNanoSeconds(-dw/(lightSpeed_ - wVel)* 1e9);

  return delay;
}

/**.......................................................................
 * Write the contents of this object to an ostream
 */
ostream& 
sza::util::operator<<(ostream& os, Coordinates& coords)
{
  os << coords.longitude() << " ";
  os << coords.latitude()  << " ";
  os << coords.altitude()  << " (m)";
  return os;
}

/**.......................................................................
 * Return the declination rotation matrix for transforming between XYZ
 * and UVW
 */
sza::util::Matrix<double> Coordinates::getXyzToUvwDecRot(DecAngle declination)
{
  sza::util::Matrix<double> decRot(3,3);
  
  // Precompute sines and cosines
  
  double cdec = cos(declination.radians());
  double sdec = sin(declination.radians());

  // Construct a rotation matrix for the declination rotation
  
  decRot[0][0] =  1; decRot[0][1] =     0; decRot[0][2] =     0;
  decRot[1][0] =  0; decRot[1][1] =  sdec; decRot[1][2] =  cdec;
  decRot[2][0] =  0; decRot[2][1] = -cdec; decRot[2][2] =  sdec;
  
  return decRot;  
}

/**.......................................................................
 * Return the HA rotation matrix for transforming between XYZ and UVW
 */
sza::util::Matrix<double> Coordinates::getXyzToUvwHaRot(HourAngle ha)
{
  sza::util::Matrix<double> haRot(3,3);
  
  // Precompute sines and cosines
  
  double cha = cos(ha.radians());
  double sha = sin(ha.radians());

  // Construct a rotation matrix for the ha rotation
  
  haRot[0][0] =  sha; haRot[0][1] =  cha; haRot[0][2] =  0;
  haRot[1][0] = -cha; haRot[1][1] =  sha; haRot[1][2] =  0;
  haRot[2][0] =    0; haRot[2][1] =    0; haRot[2][2] =  1;
  
  return haRot;  
}

/**.......................................................................
 * Return the first derivative of the HA rotation matrix for
 * transforming between XYZ and UVW
 */
sza::util::Matrix<double> Coordinates::getdXyzToUvwHaRot(HourAngle ha)
{
  sza::util::Matrix<double> haRot(3,3), result;
  
  // Precompute sines and cosines
  
  double cha = cos(ha.radians());
  double sha = sin(ha.radians());

  // Construct a rotation matrix for the ha rotation
  
  haRot[0][0] =  cha; haRot[0][1] = -sha; haRot[0][2] =  0;
  haRot[1][0] =  sha; haRot[1][1] =  cha; haRot[1][2] =  0;
  haRot[2][0] =    0; haRot[2][1] =    0; haRot[2][2] =  0;

  result = haRot * (2*M_PI/daysec);

  return result;  
}

/**.......................................................................
 * Return the secondd derivative of the HA rotation matrix for
 * transforming between XYZ and UVW
 */
sza::util::Matrix<double> Coordinates::getd2XyzToUvwHaRot(HourAngle ha)
{
  sza::util::Matrix<double> haRot(3,3), result;
  
  // Precompute sines and cosines
  
  double cha = cos(ha.radians());
  double sha = sin(ha.radians());

  // Construct a rotation matrix for the ha rotation
  
  haRot[0][0] = -sha; haRot[0][1] = -cha; haRot[0][2] =  0;
  haRot[1][0] =  cha; haRot[1][1] = -sha; haRot[1][2] =  0;
  haRot[2][0] =    0; haRot[2][1] =    0; haRot[2][2] =  0;
  
  result = haRot * (2*M_PI/daysec)*(2*M_PI/daysec);

  return result;  
}

