#include "carma/services/AntennaCoordinates.h"
#include "carma/services/Astro.h"
#include "carma/services/Distance.h"
#include "carma/services/Delay.h"
#include "carma/services/HourAngle.h"
#include "carma/services/Matrix.h"
#include "carma/services/Physical.h"
#include "carma/services/stringConstants.h"
#include "carma/util/IllegalStateException.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"
#include <cmath>

// just some #define's ; we should be using CARMA constants though
//#include "sza/array/code/share/include/szaconst.h"


using namespace std;
using namespace carma::services;
using namespace carma::services::constants;
using namespace carma::util;

/**.......................................................................
 * Constructor.
 * The default constructor uses the CARMA array center
 */
AntennaCoordinates::AntennaCoordinates() 
  : longitude_(0.0,"radians"),
    latitude_(0.0,"radians"),
    altitude_(0.0,"meters")
{
    // reset to array center
    Location carmaRef( CARMA_OBSERVATORY , REFERENCE );
    longitude_ = carmaRef.getLongitude();
    latitude_  = carmaRef.getLatitude();
    altitude_  = carmaRef.getAltitude();
}

AntennaCoordinates::AntennaCoordinates(const Location& location) 
  : longitude_(location.getLongitude()),
    latitude_(location.getLatitude()),
    altitude_(location.getAltitude())
{
    // nothing else needed here
}


/**.......................................................................
 * Constructor with initialization of lat long and alt
 */
AntennaCoordinates::AntennaCoordinates(Angle longitude, 
	                               Angle latitude, 
				       Length altitude) 
    : longitude_(longitude),
      latitude_(latitude),
      altitude_(altitude)
{
    // nothing else needed here
}


/**.......................................................................
 * Copy constructor with initialization of lat long and alt
 */
AntennaCoordinates::AntennaCoordinates( const AntennaCoordinates & rhs ) :
longitude_( rhs.longitude( ) ),
latitude_( rhs.latitude( ) ),
altitude_( rhs.altitude( ) ) {
}

/**.......................................................................
 * Destructor.
 */
AntennaCoordinates::~AntennaCoordinates() {}

/**.......................................................................
 * Assignment operator.
 */
AntennaCoordinates &
AntennaCoordinates::operator=( const AntennaCoordinates & rhs ) {
    longitude_ = rhs.longitude( );
    latitude_  = rhs.latitude( );
    altitude_  = rhs.altitude( );
    
    return *this;
}

/**.......................................................................
 * Set methods
 */
void AntennaCoordinates::setLla(Angle longitude, 
	                        Angle latitude, 
				Length altitude)
{
  setLongitude(longitude);
  setLatitude(latitude);
  setAltitude(altitude);
}

void AntennaCoordinates::setLongitude(Angle longitude)
{
  longitude_ = longitude;
}

void AntennaCoordinates::setLatitude(Angle latitude)
{
  latitude_ = latitude;
}

void AntennaCoordinates::setAltitude(Length altitude)
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
 *   up    (m)
 *   east  (m)
 *   north (m)
 */
Vector<double> AntennaCoordinates::getAbsXyz(double up, 
	                                     double east, 
					     double north)
{
  return llaAndUenToAbsXyz(longitude_, latitude_, altitude_, up, east, north);
}

/**.......................................................................
 * Return the XYZ interferometry coordinates of a point specified in
 * UEN coordinates, relative to the LLA of this object.
 *
 * Optional arguments:
 * 
 *   up    (m)
 *   east  (m)
 *   north (m)
 *   geocentric -- If true, return geocentric (earth-centered) coordinates, 
 *                 if false, return topocentric (surface-relative) coords.
 */
Vector<double> AntennaCoordinates::getXyz(double up, 
	                                  double east, 
					  double north, 
					  bool geocentric)
{
  return laAndUenToXyz(latitude_, altitude_, up, east, north, geocentric);
}

/**.......................................................................
 * Return (U, E, N) coordinates of the specified LLA point, relative
 * to the coordinates stored in this object.
 */
Vector<double> AntennaCoordinates::getUen(AntennaCoordinates& coords)
{
  return getUen(coords.longitude(), coords.latitude(), coords.altitude());
}

/**.......................................................................
 * Return the (U, E, N) coordinates of a point specified in (L, L, A)
 * coordinates, relative to the latitude and longitude of this object.
 *
 * Optional arguments:
 * 
 *   longitude 
 *   latitude
 *   altitude 
 */
Vector<double> AntennaCoordinates::getUen(Angle longitude, 
	                                  Angle latitude, 
					  Length altitude)
{
  return llaAndLlaToUen(longitude_, latitude_, altitude_,
			longitude, latitude, altitude);
}

/**.......................................................................
 * Return the (U, V, W) coordinates of requested source position,
 * relative to the position in this object.
 */
Vector<double> 
AntennaCoordinates::getUvw(HourAngle ha, Angle declination)
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
Vector<double> 
AntennaCoordinates::absXyzToLla(double XA, double YA, double ZA)
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
  
  llaVals[2] = xyzVals.magnitude() - Astro::EARTH.radius;
  
  return llaVals;
}

/**.......................................................................
 * Given a fiducial LLA and an UEN offset relative to it, return the
 * latitude, longitude, and altitude.
 *
 *   latitude       
 *   east longitude 
 *   altitude 
 *
 * And optionally:
 * 
 *   up    (m)
 *   east  (m)
 *   north (m)
 */
Vector<double> 
AntennaCoordinates::llaAndUenToLla(Angle longitude, 
	                           Angle latitude, 
				   Length altitude, 
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
 * Given a fiducial absolute XYZ and an UEN offset relative to it,
 * return the latitude, longitude, and altitude.
 *
 *   XA (m)
 *   YA (m)
 *   ZA (m)
 *
 * And optionally:
 * 
 *   north (m)
 *   up    (m)
 *   east  (m)
 */
Vector<double> AntennaCoordinates::absXyzAndUenToLla(
	double XA, double YA, double ZA,
	double up, double east, double north)
{
  // Convert from absolute XYZ to LLA

  Vector<double> llaVals = absXyzToLla(XA, YA, ZA);
  Angle longitude(llaVals[0], "radians");
  Angle latitude(llaVals[1], "radians");
  Length altitude(llaVals[2], "meters");

  // And add the UEN offset
  return llaAndUenToLla(longitude, latitude, altitude, up, east, north);
}

/**.......................................................................
 * Given U, E, N relative to the coordinates in this object, return
 * the latitude, longitude, and altitude.
 *
 * Optionally:
 * 
 *   north (m)
 *   up    (m)
 *   east  (m)
 */
Vector<double> 
AntennaCoordinates::getLla(double up, double east, double north)
{
  return llaAndUenToLla(longitude_, latitude_, altitude_, up, east, north);
}

/**.......................................................................
 * Given a (L, L, A) coordinate, return its Az, El relative to the
 * position of this object
 */
Vector<Angle> 
AntennaCoordinates::getAzEl(AntennaCoordinates& coords)
{
  return getAzEl(coords.longitude(), coords.latitude(), coords.altitude());
}


/**.......................................................................
 * Given a (L, L, A) coordinate, return its Az, El relative to the
 * position of this object
 */
Vector<Angle> 
AntennaCoordinates::getAzEl(Angle longitude, Angle latitude, Length altitude)
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
Vector<double> 
AntennaCoordinates::azElToUen(Angle az, Angle el, double r)
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
Vector<Angle> 
AntennaCoordinates::uenToAzEl(double U, double E, double N)
{
  Vector<double> uenVals(3);

  uenVals[0] = U;
  uenVals[1] = E;
  uenVals[2] = N;

  // initialize Vector with temporary Angle objects
  Vector<Angle> azelVals(2, Angle(0.0, "radians"));
  
  // Azimuth is the angle of the projection onto the N-E plane
  azelVals[0].setRadians(atan2(E, N));
  
  // Elevation is 90 - the angle between the coordinate vector and the U axis
  
  azelVals[1].setRadians(asin(U/uenVals.magnitude()));
  
  return azelVals;
}

/**.......................................................................
 * Given UEN, return the Azimuth and Elevation
 */
Vector<Angle> 
AntennaCoordinates::uenToAzEl(Vector<double>& uen)
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
void 
AntennaCoordinates::add(double up, double east, double north)
{
  Vector<double> vec = llaAndUenToLla(longitude_, latitude_, altitude_, 
				      up, east, north);

  Angle longitude(vec[0], "radians");
  Angle latitude(vec[1], "radians");
  Length altitude(vec[2], "meters");

  setLongitude(longitude);
  setLatitude(latitude);
  setAltitude(altitude);
}

/**.......................................................................
 * Return the longitude rotation matrix for transforming between (N,
 * E, U) and (X, Y, Z)
 */
carma::services::Matrix<double> 
AntennaCoordinates::getUenToXyzLonRot(Angle longitude)
{
  Matrix<double> lonRot(3,3);
  
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
carma::services::Matrix<double> 
AntennaCoordinates::getUenToXyzLatRot(Angle latitude)
{
  Matrix<double> latRot(3,3);
  
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
Vector<double> 
AntennaCoordinates::llaAndUenToAbsXyz(
	Angle longitude, Angle latitude, Length altitude, 
	double up, double east, double north)
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
  translation[0] = Astro::EARTH.radius;
  translation[1] = 0.0;
  translation[2] = 0.0;

  // Construct a vector of input coordinates
  
  inputCoords.resize(3);
  double altMeters = altitude.convert("meters");
  inputCoords[0] = altMeters + up;
  inputCoords[1] = east;
  inputCoords[2] = north;

  // Calculate the XYZ coordinates
  
  Vector<double> vec = (inputCoords + translation);
  Matrix<double> rot = (lonRot * latRot);

  //  Matrix<double> rot = (lonRot * latRot);

  outputCoords = rot * vec;

  return outputCoords;
}

/**.......................................................................
 * Given a topocentric XYZ, return the UEN coordinates at a
 * given latitude
 */
Vector<double> 
AntennaCoordinates::topoXYZToUen(double X, double Y, double Z, Angle latitude)
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
 *   latitude    
 *   altitude   
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
Vector<double> 
AntennaCoordinates::laAndUenToXyz(Angle latitude, Length altitude, 
				  double up, double east, double north,
				  bool geocentric)
{
  Matrix<double> latRot;
  Vector<double> outputCoords;
  
  // Get rotation matrices

  latRot = getUenToXyzLatRot(latitude);

  // Construct a translation vector to put us in earth-center relative
  // coordinates
  
  double rtrans = ( geocentric ? Astro::EARTH.radius: 0.0 );
  Vector<double> translation(rtrans, 0.0, 0.0);
  
  // Construct a vector of input coordinates
  
  double altMeters = altitude.convert("meters");
  Vector<double> inputCoords(altMeters + up, east, north);
  
  // Calculate the XYZ coordinates.  It is simply given by a latitude
  // rotation.
  
  Vector<double> vec = (inputCoords + translation);
  
  outputCoords = latRot * vec;

  return outputCoords;
}

/**.......................................................................
 * Return the UEN coordinates of a LLA point, relative to a fiducial
 * LLA point
 */
Vector<double> 
AntennaCoordinates::llaAndLlaToUen(Angle fiducialLongitude, 
	                           Angle fiducialLatitude,
				   Length fiducialAltitude, 
				   Angle longitude, 
				   Angle latitude, 
				   Length altitude)
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
  
  Vector<double> translation(Astro::EARTH.radius, 0.0, 0.0);
  
  // Now calculate the UEN coordinates
  
  Vector<double> uenVals(3), retVals(3);

  uenVals = (inv * xyzVals) - translation;

  // Subtract off the fiducial altitude to convert to U

  retVals[0] = uenVals[0] - fiducialAltitude.convert("meters"); // U
  retVals[1] = uenVals[1]; // E
  retVals[2] = uenVals[2]; // 

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
Vector<double> 
AntennaCoordinates::haDecAndXyzToUvw(HourAngle ha, Angle declination, 
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
  /*
  int degxx = static_cast<int>(declination.degrees());
  if ( degxx != 0 ) {
      double hr = ha.hours();
      if ( hr >  6.0 )  hr -= 24.0;
      if ( hr < -6.0 )  hr += 24.0;
  cout << "############ XYZ-UVW ROTATION MATRIX #################" << endl;
  cout  << ::std::setw(15) << ::std::setprecision(8) << "FOR ["<<hr << " , "<<declination.degrees()<<" , "
       << X <<"," << Y <<"," << Z << "]"<<endl;
  cout << rot << endl;
  cout << "######################################################" << endl;
  }
  */
  
  // Now calculate the UVW coordinates
  
  Vector<double> uvwVals(3);

  uvwVals = (rot * xyzVals);

  return uvwVals;
}

/**.......................................................................
 * Return the (X, Y, Z) coordinates of the specified (R, Ha, Dec)
 * coordinate.
 */
Vector<double> 
AntennaCoordinates::haDecToXyz(HourAngle ha, Angle declination, 
	                       Distance distance, bool geocentric)
{
  // Construct the input source coordinate in UVW coordinates

  Vector<double> uvwVals(3);

  uvwVals[0] = 0;
  uvwVals[1] = 0;

  double r = distance.convert("meters");

  if(distance.isInfinite())
    uvwVals[2] = 1.0;
  else
    uvwVals[2] = r  - (geocentric ? 0.0 : Astro::EARTH.radius);

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
AntennaCoordinates::laAndHaDecToAzEl(Angle latitude, Length altitude, 
				     HourAngle ha, Angle declination, 
				     Distance distance,
				     bool geocentric) 
{
  // Get the geocentric XYZ direction of the source vector.  Passing
  // SrcType INF will return the source direction only.

  Vector<double> geoXyzSrcDir = haDecToXyz(ha, declination, distance, true);

  // Get the geocentric direction of the point of observation

  Vector<double> geoXyzObs = laAndUenToXyz(latitude, altitude, 0.0, 0.0, 0.0,
					   true);

  Vector<double> geoXyzObsDir = geoXyzObs/geoXyzObs.magnitude();

  // Factor of R_obs/R_src, or 0 if source is infinitely far away

  double r = distance.convert("meters");
  double fac = ((geocentric || distance.isInfinite()) ? 0.0 : 
	  (geoXyzObs.magnitude()/ r ));

  // Get the topocentric source direction.  In the case of type==INF,
  // this will be the same as the geocentric

  Vector<double> topoXyzSrcDir = (geoXyzSrcDir - geoXyzObsDir * fac);
  topoXyzSrcDir = topoXyzSrcDir /
                  sqrt(1 - 2*fac*(geoXyzSrcDir*geoXyzObsDir) + fac*fac);

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

  if( !isfinite(srcAzEl[0].radians()) || !isfinite(srcAzEl[1].radians()) ) 
  {
      ostringstream os;
      os << "Az/El position is not defined for Ha = " << ha
	 << ", Dec = " << declination
	 << " at latitude = " << latitude;
    throw CARMA_EXCEPTION(carma::util::IllegalStateException, os);
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
Vector<double> 
AntennaCoordinates::getdUvw(HourAngle ha, Angle declination, 
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
 */
Vector<double> 
AntennaCoordinates::getd2Uvw(HourAngle ha, Angle declination, 
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
Delay AntennaCoordinates::getGeometricDelay(Angle latitude, 
	                                    Length altitude,
					    Angle az, Angle el,
					    double X, double Y, double Z)
{
  // Calculate the UEN of the source direcion
  ScopedLogNdc("AntennaCoordinates::getGeometricDelay(lat,alt,az,el,x,y,z)");


  Vector<double> uenDir  = azElToUen(az, el);


  // Get the XYZ of the UEN point
  Vector<double> srcXyzDir = laAndUenToXyz(latitude, altitude, 
					   uenDir[0], uenDir[1], uenDir[2], 
					   false);

  // must subtract off reference position, 
  // @see bug #149
  // technically should be refLatitude, refAltitude, no?
  Vector<double> refXyzDir = laAndUenToXyz(latitude, altitude, 
					   0.0, 0.0, 0.0,
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
  delay.setDelayInMeters( -( xyzVals * (srcXyzDir-refXyzDir) ) );

  ostringstream os;
  os << setiosflags(ios::fixed)
     << setprecision(4)
     << "Inputs: [" 
     << " Lat=" << latitude.degrees()
     << " Alt=" << altitude.meters()
     << " Az="  << az.degrees()
     << " El="  << el.degrees()
     << " X="  << X << " m"
     << " Y="  << Y << " m"
     << " Z="  << Z << " m ] / "
     << " Outputs: [ "
     << " UEN = "    << uenDir
     << " SrcDir = " << srcXyzDir
     << " delay = "  << delay.nanoSeconds()
     << " ]";
  CARMA_CPTRACE( Trace::TRACE3, os.str() );

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
Delay AntennaCoordinates::getGeometricDelay(HourAngle ha, Angle declination, 
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
  // relative to the origin of the vector. Hence the -ve prefactor
  // before the w.

  Delay delay;
  delay.setDelayInNanoSeconds(-w/(Physical::C - wVel) * 1e9);
 
  // Set the rate as well

  delay.setDelayRateInNanoSeconds(-dw/(Physical::C - wVel)* 1e9);

  return delay;
}

/**.......................................................................
 * Write the contents of this object to an ostream
 */
ostream& 
carma::services::operator<<(ostream& os, const AntennaCoordinates& coords)
{
  os << coords.longitude() << " ";
  os << coords.latitude()  << " ";
  os << coords.altitude()  << " ";
  return os;
}

/**.......................................................................
 * Return the declination rotation matrix for transforming between XYZ
 * and UVW
 */
carma::services::Matrix<double> 
AntennaCoordinates::getXyzToUvwDecRot(Angle declination)
{
  Matrix<double> decRot(3,3);
  
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
carma::services::Matrix<double> 
AntennaCoordinates::getXyzToUvwHaRot(HourAngle ha)
{
  Matrix<double> haRot(3,3);
  
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
carma::services::Matrix<double> 
AntennaCoordinates::getdXyzToUvwHaRot(HourAngle ha)
{
  Matrix<double> haRot(3,3), result;
  
  // Precompute sines and cosines
  
  double cha = cos(ha.radians());
  double sha = sin(ha.radians());

  // Construct a rotation matrix for the ha rotation
  
  haRot[0][0] =  cha; haRot[0][1] = -sha; haRot[0][2] =  0;
  haRot[1][0] =  sha; haRot[1][1] =  cha; haRot[1][2] =  0;
  haRot[2][0] =    0; haRot[2][1] =    0; haRot[2][2] =  0;

  result = haRot * (2*M_PI/Time::SECONDS_PER_DAY);

  return result;  
}

/**.......................................................................
 * Return the secondd derivative of the HA rotation matrix for
 * transforming between XYZ and UVW
 */
carma::services::Matrix<double> 
AntennaCoordinates::getd2XyzToUvwHaRot(HourAngle ha)
{
  Matrix<double> haRot(3,3), result;
  
  // Precompute sines and cosines
  
  double cha = cos(ha.radians());
  double sha = sin(ha.radians());

  // Construct a rotation matrix for the ha rotation
  
  haRot[0][0] = -sha; haRot[0][1] = -cha; haRot[0][2] =  0;
  haRot[1][0] =  cha; haRot[1][1] = -sha; haRot[1][2] =  0;
  haRot[2][0] =    0; haRot[2][1] =    0; haRot[2][2] =  0;
  
  double fact = (2*M_PI/Time::SECONDS_PER_DAY);
  result = haRot * fact * fact;

  return result;  
}

