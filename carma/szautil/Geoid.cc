#include "carma/szautil/Geoid.h"
#include "carma/szautil/Matrix.h"

#include <iomanip>

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor with default datum
 */
Geoid::Geoid() : Ellipsoid()
{
  changeDatum(DATUM_GPS);
}

/**.......................................................................
 * Constructor with datum specified
 */
Geoid::Geoid(GeoDatum datum) : Ellipsoid()
{
  changeDatum(datum);
}

/**.......................................................................
 * Destructor.
 */
Geoid::~Geoid() {}

/**.......................................................................
 * Return the length of the radius vector from the center of the
 * earth to the surface at a given geocentric latitude.
 */
Length Geoid::geocentricRadius(Angle geocentricLatitude)
{
  return radius(geocentricLatitude);
}

/**.......................................................................
 * Return the length of the radius vector normal to the surface
 * at a given geodetic latitude.
 *
 * From:
 *
 *     geodR * sin(geodLat) = geocR * sin(geocLat),
 *
 * we have 
 *
 *     geodR  = geocR * sin(geocLat)/sin(geodLat)
 *
 */
Length Geoid::geodeticRadius(Angle geodLatitude)
{
  Angle geocLatitude = geocentricLatitude(geodLatitude);
  return geocentricRadius(geocLatitude) * (sin(geocLatitude.radians()) / sin(geodLatitude.radians()));
}

/**.......................................................................
 * Return the geocentric latitude corresponding to a given
 * geodetic latitude
 *
 * Geocntric latitude and Geodetic latitude are related by:
 *
 *     tan(geoc) = (b/a)^2 * tan(geod)
 *
 *  Flattening is defined as: f = 1.0 - (b/a), so (b/a) = (1 - f)
 */
Angle Geoid::geocentricLatitude(Angle geodeticLatitude)
{
  double df       = (1.0 - flattening());
  double df2      = df * df;
  double tGeocLat = df2 * tan(geodeticLatitude.radians());

  Angle geocLat;
  geocLat.setRadians(atan(tGeocLat));

  return geocLat;
}

/**.......................................................................
 * Return the geocentric latitude corresponding to a given
 * geodetic latitude
 *
 * Geocntric latitude and Geodetic latitude are related by:
 *
 *     tan(geoc) = (b/a)^2 * tan(geod)
 *
 *  Flattening is defined as: f = 1.0 - (b/a), so (b/a) = (1 - f)
 */
Angle Geoid::geodeticLatitude(Angle geocentricLatitude)
{
  double df       = (1.0 - flattening());
  double df2      = df * df;
  double tGeodLat = 1.0/df2 * tan(geocentricLatitude.radians());

  Angle geodLatitude;
  geodLatitude.setRadians(atan(tGeodLat));

  return geodLatitude;
}

void Geoid::setDatum(GeoDatum datum, Length sphericalRadius)
{
  Length majAxis;
  double flattening;

  switch (datum) {

  case DATUM_SPHERE:
    majAxis = sphericalRadius;
    flattening = 0.0;
    break;

  case DATUM_CLARKE66:
  case DATUM_NAD27:
    majAxis.setMeters(6378206.400);
    flattening = 1.0/294.978698214;
    break;
    
  case DATUM_ANS:
  case DATUM_AGD66:
  case DATUM_GDA84:
    majAxis.setMeters(6378160.000);
    flattening = 1.0/298.25;
    break;

  case DATUM_GRS80:
  case DATUM_GDA94:
  case DATUM_GDA2000:
  case DATUM_NAD83:
    majAxis.setMeters(6378137.000);
    flattening = 1.0/298.257222101;
    break;

  case DATUM_WGS84:
  case DATUM_GPS:
    majAxis.setMeters(6378137.000);
    flattening = 1.0/298.257223563;
    break;
  case DATUM_NONE:
    return; // Do nothing
    break;
  default:
    ThrowError("Unrecognized datum specified: " << datum);
    break;
  }

  setMajorAxisAndFlattening(majAxis, flattening);
  datum_ = datum;
}

/**.......................................................................
 * Set the default datum of this object, so that it can be restored
 * after a temporary change
 */
void Geoid::setDefaultDatum(GeoDatum datum, Length sphericalRadius)
{
  defaultDatum_ = datum;
  defaultSphericalRadius_ = sphericalRadius;
}

/**.......................................................................
 * Restore the default datum of this object, after a temporary change
 */
void Geoid::restoreDefaultDatum()
{
  setDatum(defaultDatum_, defaultSphericalRadius_);
}

/**.......................................................................
 * Change the datum of this object
 */
void Geoid::changeDatum(GeoDatum datum, Length sphericalRadius) 
{
  setDatum(datum, sphericalRadius);
  setDefaultDatum(datum, sphericalRadius);
}

/**.......................................................................
 * Return the Equatorial radius of the Earth, for the current datum
 */
Length Geoid::earthEquatorialRadius()
{
  return majorAxis();
}

/**.......................................................................
 * Return the Polar radius of the Earth, for the current datum
 */
Length Geoid::earthPolarRadius()
{
  return minorAxis();
}

/**.......................................................................
 * Convert from Geodetic coordinates in an arbitrary input datum back
 * to Geodetic coordinates in the current datum
 */
Lla Geoid::geodeticLlaAndEnuToGeodeticLla(Lla& geodeticLla, LengthTriplet enu)
{
  LengthTriplet ecf = geodeticLlaAndEnuToEcf(geodeticLla, enu);
  return ecfToGeodeticLla(ecf);
}

/**.......................................................................
 * Convert from Geodetic coordinates in an arbitrary input datum to
 * Geocentric coordinates in the current datum
 */
Lla Geoid::geodeticLlaAndEnuToGeocentricLla(Lla& geodeticLla, LengthTriplet enu)
{
  LengthTriplet ecf = geodeticLlaAndEnuToEcf(geodeticLla, enu);
  return ecfToGeocentricLla(ecf);
}

/**.......................................................................
 * Convert from Geocentric coordinates in an arbitrary input datum to
 * Geodetic coordinates in the current datum
 */
Lla Geoid::geocentricLlaAndEnhToGeodeticLla(Lla& geocentricLla, LengthTriplet enh)
{
  LengthTriplet ecf = geocentricLlaAndEnhToEcf(geocentricLla, enh);
  return ecfToGeodeticLla(ecf);
}

/**.......................................................................
 * Convert from Geocentric coordinates in an arbitrary input datum
 * back to Geocentric coordinates in the current datum
 */
Lla Geoid::geocentricLlaAndEnhToGeocentricLla(Lla& geocentricLla, LengthTriplet enh)
{
  LengthTriplet ecf = geocentricLlaAndEnhToEcf(geocentricLla, enh);
  return ecfToGeocentricLla(ecf);
}

/**.......................................................................
 * Convert from ECF coordinates to Geocentric coordinates in the
 * current datum
 */
Lla Geoid::ecfToGeocentricLla(LengthTriplet& ecf)
{
  Lla geocentricLla;

  // Longitude is the angle of the projection onto the X-Y plane,
  
  geocentricLla.longitude_.setRadians(atan2(ecf.Y_.meters(), ecf.X_.meters()));
  
  // Latitude is the angle between the coordinate vector and the Z axis
  
  geocentricLla.latitude_.setRadians(M_PI/2 - acos(ecf.Z_ / ecf.magnitude()));
  
  // Altitude is the magnitude of the coordinate vector, less the
  // radius of the earth at this geocentric latitude.
  
  geocentricLla.altitude_ = ecf.magnitude() - geocentricRadius(geocentricLla.latitude_);

  geocentricLla.setDatum(datum_, earthEquatorialRadius());
  geocentricLla.setCoordSystem(COORD_GEOCENTRIC);
  
  return geocentricLla;
}

/**.......................................................................
 * Convert from ECF coordinates to Geodetic coordinates, in the
 * reference frame of the current datum
 */
Lla Geoid::ecfToGeodeticLla(LengthTriplet& ecf)
{
  // Convenience aliases

  double X = ecf.X_.meters();
  double Y = ecf.Y_.meters();
  double Z = ecf.Z_.meters();

  // Now do the calculations

  double r2    = X * X + Y * Y;
  double r     = sqrt(r2);
  double a     = majorAxis().meters();
  double b     = minorAxis().meters();

  double a2    = a*a;
  double b2    = b*b;

  double E2    = a2 - b2;
  double Z2    = Z*Z;
  double F     = 54 * b2 * Z2;
  double e2    = firstEccentricity() * firstEccentricity();
  double G     = r2 + (1.0 - e2) * Z2 - e2 * E2;
  double C     = (e2 * e2 * F * r2) / (G*G*G);
  double SArg  = 1.0 + C + sqrt(C * (C + 2.0));
  double S     = pow(SArg, 1.0/3);
  double PArg  = S + 1.0 / S + 1.0;
  double P     = F / (3 * PArg * PArg * G * G);
  double Q     = sqrt(1.0 + 2 * e2 * e2 * P);

  double rArg0 = (P * e2 * r) / (1.0 + Q);
  double rArg1 = 0.5 * a2 * (1.0 + 1.0 / Q);
  double rArg2 = (P * (1.0 - e2) * Z2) / (Q * (1.0 + Q));
  double rArg3 = 0.5 * P * r2;
  
  double arg = rArg1 - rArg2 - rArg3;

  double r0    = -rArg0 + (arg > 0.0 ? sqrt(rArg1 - rArg2 - rArg3) : 0.0);

  double UVArg = r - e2 * r0;

  double U     = sqrt(UVArg * UVArg + Z2);
  double V     = sqrt(UVArg * UVArg + (1.0 - e2) * Z2);
  double Z0    = (b2 * Z) / (a * V);

  double ep2  = secondEccentricity() * secondEccentricity();

  // Return the Geodetic position

  Lla geodeticLla;

  geodeticLla.longitude_.setRadians(atan2(Y, X));
  geodeticLla.latitude_.setRadians(atan((Z + ep2 * Z0) / r));
  geodeticLla.altitude_.setMeters(U * (1.0 - b2 / (a * V)));
  
  geodeticLla.setDatum(datum_, earthEquatorialRadius());
  geodeticLla.setCoordSystem(COORD_GEODETIC);
 
  return geodeticLla;
}

/**.......................................................................
 * Convert from ENU coordinates relative to a reference geodetic
 * longitude and latitude, to ECF (absolute XYZ, where Z points along
 * the North pole, X passes through Greenwich meridian, and Y passes
 * through longitude +90.  (Note that East longitude is positive, and
 * West longitude is negative.)
 */
LengthTriplet Geoid::geodeticLlaAndEnuToEcf(Lla& geodeticLla, LengthTriplet enu)
{
  //------------------------------------------------------------
  // Set the datum of this object to the input datum of the LLA
  // coordinate (if any was specified)
  //------------------------------------------------------------

  setDatum(geodeticLla.datum_, geodeticLla.sphericalRadius_);

  //------------------------------------------------------------
  // Store the Geocentric latitude corresponding to this Geodetic
  // latitude on the surface of the earth
  //------------------------------------------------------------

  Angle geocLatitude = geocentricLatitude(geodeticLla.latitude_);

  //------------------------------------------------------------
  // First shift the ENU coordinate system by the altitude so that the
  // origin coincides with the local surface of the earth
  //------------------------------------------------------------

  enu.up_ += geodeticLla.altitude_;

  //------------------------------------------------------------
  // Geodetic ENU coordinate system is related to a Geocentric ENH
  // coordinate system by a rotation about the east vector by (geodLat
  // - geocLat)
  //------------------------------------------------------------

  Matrix<double> eastRot = getEnuToEnhRot(geodeticLla.latitude_ - geocLatitude);
  LengthTriplet enh = eastRot * enu;

  //------------------------------------------------------------
  // The origin of the Geocentric ENH coordinate system is
  // translated from the center of the earth along the radius vector.
  // Shift the coordinate system along the geocentric H axis so
  // that the origin of the shifted coordinate system is now at the
  // center of the earth
  //------------------------------------------------------------

  enh.height_ += geocentricRadius(geocLatitude);

  //------------------------------------------------------------
  // The XYZ coordinate system is tilted by a rotation of geocLat
  // about the ENH east axis
  //------------------------------------------------------------

  Matrix<double> latRot = getEnhToXyzLatRot(geocLatitude);
  LengthTriplet xyz = latRot * enh;

  //------------------------------------------------------------
  // The ECF coordinate system is rotated by the longitude about the Z
  // axis
  //------------------------------------------------------------

  Matrix<double> lngRot = getXyzToEcfLngRot(geodeticLla.longitude_);
  LengthTriplet ecf = lngRot * xyz;

  ecf.setDatum(DATUM_NONE, earthEquatorialRadius()); // Datum not relevant in ECF coordinates
  ecf.setCoordSystem(COORD_ECF);

  //------------------------------------------------------------
  // Restore the datum of this object
  //------------------------------------------------------------

  restoreDefaultDatum();

  return ecf;
}

/**.......................................................................
 * Convert from ENU coordinates relative to a reference Geodetic
 * longitude and latitude, to relative XYZ (where Z points along the
 * North pole, X passes through the longitude of the reference point,
 * Y passes through the meridian that is -6H from the longitude of
 * observation (Note that East longitude is positive, and West
 * longitude is negative.)
 *
 * Note that this transformation is the same as to ECF, without the
 * longitude rotation about the Z axis
 */
LengthTriplet Geoid::geodeticLlaAndEnuToXyz(Lla& geodeticLla, LengthTriplet enu)
{
  //------------------------------------------------------------
  // Set the datum of this object to the input datum of the LLA
  // coordinate (if any was specified)
  //------------------------------------------------------------

  setDatum(geodeticLla.datum_, geodeticLla.sphericalRadius_);

  //------------------------------------------------------------
  // Store the Geocentric latitude corresponding to this Geodetic
  // latitude on the surface of the earth
  //------------------------------------------------------------

  Angle geocLatitude = geocentricLatitude(geodeticLla.latitude_);

  //------------------------------------------------------------
  // First shift the ENU coordinate system by the altitude so that the
  // origin coincides with the local surface of the earth
  //------------------------------------------------------------

  enu.up_ += geodeticLla.altitude_;

  //------------------------------------------------------------
  // Geodetic ENU coordinate system is related to a Geocentric ENH
  // coordinate system by a rotation about the east vector by latDiff
  // = (geodLat - geocLat)
  //------------------------------------------------------------

  Matrix<double> eastRot = getEnuToEnhRot(geodeticLla.latitude_ - geocLatitude);
  LengthTriplet enh = eastRot * enu;

  //------------------------------------------------------------
  // The origin of the Geocentric ENH coordinate system is
  // translated from the center of the earth along the radius vector.
  // Shift the coordinate system along the geocentric 'up' axis so
  // that the origin of the shifted coordinate system is now at the
  // center of the earth
  //------------------------------------------------------------

  enh.height_ += geocentricRadius(geocLatitude);

  //------------------------------------------------------------
  // This shifted ENH coordinate system is just tilted by a rotation
  // of geocLat about the eastPrime axis
  //------------------------------------------------------------

  Matrix<double> latRot = getEnhToXyzLatRot(geocLatitude);
  LengthTriplet xyz = latRot * enh;

  // Although the datum is not technically relevant in the XYZ
  // coordinate system, store the datum that was used to convert from
  // ENU to ENH at the reference latitude

  xyz.setDatum(geodeticLla.datum_, geodeticLla.sphericalRadius_);
  xyz.setCoordSystem(COORD_XYZ);

  //------------------------------------------------------------
  // Restore the datum of this object
  //------------------------------------------------------------

  restoreDefaultDatum();

  return xyz;
}

/**.......................................................................
 * Convert from relative XYZ coordinates at a reference Geodetic
 * longitude and latitude, to relative ENU coordinates.
 */
LengthTriplet Geoid::geodeticLlaAndXyzToEnu(Lla& geodeticLla, LengthTriplet xyz)
{
  //------------------------------------------------------------
  // Set the datum of this object to the input datum of the LLA
  // coordinate (if any was specified)
  //------------------------------------------------------------

  setDatum(geodeticLla.datum_, geodeticLla.sphericalRadius_);

  //------------------------------------------------------------
  // Store the Geocentric latitude corresponding to this Geodetic
  // latitude on the surface of the earth
  //------------------------------------------------------------

  Angle geocLatitude = geocentricLatitude(geodeticLla.latitude_);

  //------------------------------------------------------------ 
  // The local XYZ coordinate system and earth-centered ENH coordinate
  // system are related by a rotation of the geocentric latitude about
  // the Y axis
  //------------------------------------------------------------

  Matrix<double> latRot = getEnhToXyzLatRot(geocLatitude).inverse();
  LengthTriplet enh = latRot * xyz;

  //------------------------------------------------------------
  // Subtract the geocentric Radius to shift to an ENH coordinate
  // system at the reference point on the surface of the earth
  //------------------------------------------------------------

  enh.height_ -= geocentricRadius(geocLatitude);

  //------------------------------------------------------------
  // Geodetic ENU coordinate system is related to a Geocentric ENH
  // coordinate system by a rotation about the east vector by latDiff
  // = (geodLat - geocLat)
  //------------------------------------------------------------

  Matrix<double> eastRot = getEnuToEnhRot(geodeticLla.latitude_ - geocLatitude).inverse();
  LengthTriplet enu = eastRot * enh;

  // Store the datum that was used to convert from ENH to ENU at the
  // reference latitude

  enu.setDatum(geodeticLla.datum_, geodeticLla.sphericalRadius_);
  enu.setCoordSystem(COORD_ENU);

  //------------------------------------------------------------
  // Restore the datum of this object
  //------------------------------------------------------------

  restoreDefaultDatum();

  return enu;
}

/**.......................................................................
 * Convert from ENH coordinates relative to a reference geocentric
 * longitude and latitude, to ECF (absolute XYZ, where Z points along
 * the North pole, X passes through Greenwich meridian, and Y passes
 * through longitude +90.  (Note that East longitude is positive, and
 * West longitude is negative.)
 */
LengthTriplet Geoid::geocentricLlaAndEnhToEcf(Lla& geocentricLla, LengthTriplet enh)
{
  //------------------------------------------------------------
  // Set the datum of this object to the input datum of the LLA
  // coordinate (if any was specified)
  //------------------------------------------------------------

  setDatum(geocentricLla.datum_, geocentricLla.sphericalRadius_); 

  //------------------------------------------------------------
  // First shift the ENH coordinate system so that the origin
  // coincides with the center of the earth
  //------------------------------------------------------------


  COUT("Geocentric Radius = " << geocentricRadius(geocentricLla.latitude_));

  enh.height_ += geocentricRadius(geocentricLla.latitude_) + geocentricLla.altitude_;

  //------------------------------------------------------------
  // The XYZ coordinate system is tilted by a rotation of geocLat
  // about the eastPrime axis
  //------------------------------------------------------------

  Matrix<double> latRot = getEnhToXyzLatRot(geocentricLla.latitude_);
  LengthTriplet xyz = latRot * enh;

  //------------------------------------------------------------
  // Finally, the ECF coordinate system is rotated by the
  // longitude about the Z axis
  //------------------------------------------------------------

  Matrix<double> lngRot = getXyzToEcfLngRot(geocentricLla.longitude_);
  LengthTriplet ecf = lngRot * xyz;

  ecf.setDatum(DATUM_NONE, earthEquatorialRadius());      // Datum not relevant in ECF
				 // coordinates
  ecf.setCoordSystem(COORD_ECF);

  //------------------------------------------------------------
  // Restore the datum of this object
  //------------------------------------------------------------

  restoreDefaultDatum();

  return ecf;
}

/**.......................................................................
 * Convert from ENH coordinates relative to a reference geocentric
 * longitude and latitude, to relative XYZ (where Z points along the
 * North pole, X passes through the reference longitude, and Y passes
 * through a meridian that is -6H from the reference longitude).
 * (Note that East longitude is positive, and West longitude is
 * negative.)
 */
LengthTriplet Geoid::geocentricLlaAndEnhToXyz(Lla& geocentricLla, LengthTriplet enh)
{
  //------------------------------------------------------------
  // Set the datum of this object to the input datum of the LLA
  // coordinate (if any was specified)
  //------------------------------------------------------------

  setDatum(geocentricLla.datum_, geocentricLla.sphericalRadius_);

  //------------------------------------------------------------
  // First shift the ENH coordinate system so that the origin
  // coincides with the center of the earth
  //------------------------------------------------------------

  enh.height_ += geocentricRadius(geocentricLla.latitude_) + geocentricLla.altitude_;

  //------------------------------------------------------------
  // The XYZ coordinate system is tilted by a rotation of geocLat
  // about the east ENH axis
  //------------------------------------------------------------

  Matrix<double> latRot = getEnhToXyzLatRot(geocentricLla.latitude_);
  LengthTriplet xyz = latRot * enh;

  xyz.setDatum(DATUM_NONE, earthEquatorialRadius()); // Datum not relevant in XYZ
				      // coordinates
  xyz.setCoordSystem(COORD_XYZ);

  //------------------------------------------------------------
  // Restore the datum of this object
  //------------------------------------------------------------

  restoreDefaultDatum();

  return xyz;
}

/**.......................................................................
 * For a Geodetic tangent point specified by referenceLla and a scale
 * factor k0, return the projected 2D coordinate of the position given
 * by geodeticLla
 */
LengthTriplet Geoid::polarStereographicProjection(Lla& geodeticLla, Lla& referenceLla, double k0)
{
  double lr     = geodeticLla.latitude_.radians();
  double dLngr  = geodeticLla.longitude_.radians() - referenceLla.longitude_.radians();
  double preFac = 1.0;

  // Are we projecting the north pole, or the south pole?

  LengthTriplet ups;

  if(referenceLla.latitude_.radians() < 0) {
    lr     *= -1;
    dLngr  *= -1;
    preFac *= -1;
    ups.hemisphere_ = HEMI_SOUTH;
  } else {
    ups.hemisphere_ = HEMI_NORTH;
  }

  double clr   = cos(lr);
  double slr   = sin(lr);
  double e     = firstEccentricity();
  double eslr  = e * slr;

  double t     = tan(M_PI/4 - lr/2) / pow(((1 - eslr)/(1 + eslr)), e/2);
  Length rho   = earthEquatorialRadius() * (2*k0*t / sqrt( pow((1+e),(1+e)) * pow((1-e),(1-e)) ) );
  double m     = clr / sqrt(1 - eslr * eslr);

  ups.x_ =   rho *  preFac *  sin(dLngr);
  ups.y_ =   rho *  preFac * -cos(dLngr);
  ups.k_ =   rho / (earthEquatorialRadius() * m);

  ups.setDatum(datum_, earthEquatorialRadius());
  ups.setCoordSystem(COORD_UPS);

  return ups;
}

Lla Geoid::inversePolarStereographicProjection(LengthTriplet& xyk, double k0)
{
  // Compute the latitude at which the scale factor is 1.0

  //  double lrc     = asin(k0);
  double lrc     = 83.7413 / 180 * M_PI;

  Length x       = xyk.x_;
  Length y       = xyk.y_;
  double refLngr = 0.0;
  double preFac  = 1.0;

  if(xyk.hemisphere_ == HEMI_SOUTH) {
    x       *= -1;
    y       *= -1;
    refLngr *= -1;
    lrc     *= -1;
    preFac  *= -1;
  }

  COUT("lrc = " << lrc/M_PI * 180);

  double clrc  = cos(lrc);
  double slrc  = sin(lrc);
  double e     = firstEccentricity();
  double eslrc = e * slrc;

  double tc    = tan(M_PI/4 - lrc/2) / pow(((1 - eslrc)/(1 + eslrc)), e/2);
  double mc    = clrc / sqrt(1 - eslrc * eslrc);

  // Ensure that the z coordinate of the LengthTriplet is zero before
  // computing a magnitude

  xyk.z_.setMeters(0.0);

  COUT("xyk.x_         = " << xyk.x_);
  COUT("xyk.coords_[0] = " << xyk.coords_[0]);

  Length rho   = xyk.magnitude();

  COUT("rho = " << rho);

  double t     = rho / (earthEquatorialRadius()) * (tc / mc);
  double chi   = M_PI/2 - 2 * atan(t);

  double e2    = e*e;
  double e4    = e2*e2;
  double e6    = e4*e2;
  double e8    = e6*e2;

  double s2chi = sin(2*chi);
  double s4chi = sin(4*chi);
  double s6chi = sin(6*chi);
  double s8chi = sin(8*chi);

  double latr = chi 
    + s2chi * (e2/2 + 5*e4/24 + e6/12 + 13*e8/360)
    + s4chi * (7*e4/48 + 29*e6/240 + 811*e8/11520)
    + s6chi * (7*e6/120 + 81*e8/1120)
    + s8chi * (4279*e8/161280);
  
  double lngr = refLngr + atan(x / (y * -1));

  Lla lla;
  lla.latitude_.setRadians(preFac * latr);
  lla.longitude_.setRadians(preFac * lngr);

  lla.setDatum(datum_, earthEquatorialRadius());
  lla.setCoordSystem(COORD_GEODETIC);

  return lla;
}

/**.......................................................................
 * Calculate the Easting/Northing coordinate point and scale factor
 * (for converting between distances on the cylinder and distances on
 * the ground) corresponding to the given Geodetic longitude and
 * latitude, for the projection of the earth ellipsoid onto a
 * transverse cylinder tangent to the earth at the given reference
 * point (for k0 = 1).  
 *
 * For k0 = 1, this reduces to a transverse Mercator Projection.  For
 * k0 < 1, this is a secant projection, where the cylinder is not
 * tangent to the earth, but intersects the earth at two lines which
 * are parallel to the reference meridian.
 */
LengthTriplet Geoid::transverseSecantProjection(Lla& geodeticLla, Lla& referenceLla, double k0)
{
  double e2;
  e2 = firstEccentricitySquared();
  
  Angle dLng = geodeticLla.longitude_ - referenceLla.longitude_;

  double lr   = geodeticLla.latitude_.radians();
  double clr  = cos(lr);
  double slr  = sin(lr);
  double tlr  = slr/clr;
  
  double ep2  = e2/(1-e2);
  Length N    = earthEquatorialRadius() / sqrt(1.0 - e2 * slr * slr);
  double T    = tlr * tlr;
  double C    = ep2 * clr * clr;
  double A    = dLng.radians() * clr;
  Length M    = earthEquatorialRadius() * utmSeriesExpansionForM(geodeticLla.latitude_);
  Length M0   = earthEquatorialRadius() * utmSeriesExpansionForM(referenceLla.latitude_);

  double C2 = C*C;
  double T2 = T*T;
  double A2 = A*A;
  double A3 = A2*A;
  double A4 = A3*A;
  double A5 = A4*A;
  double A6 = A5*A;

  LengthTriplet xyk;

  xyk.x_ = N * (A + (1 - T + C) * A3/6 + (5 - 18*T + T2 + 72*C - 58*ep2) * A5/120);

  xyk.y_ = (M - M0) + N * tlr * (A2/2 + (5 - T + 9*C + 4*C2) * A4/24 
				 + (61 - 58*T + T2 + 600*C - 330*ep2) * A6/720);
  
  xyk.k_ = 1 + (1 + C) * A2/2 + (5 - 4*T + 42*C + 13*C2 - 28*ep2) * A4/24 
    + (61 - 148*T + 16*T2) * A6/720;
  
  // Multiply by the scale factor that converts from Transverse
  // Mercator projection (tangent to the ellipsoid at one point) to a
  // secant projection (cut the ellipsoid at two constant lines)
  
  xyk.x_ *= k0;
  xyk.y_ *= k0;
  xyk.k_ *= k0;

  return xyk;
}

/**.......................................................................
 * Calculate the Geodetic LLA corresponding to the given cylindrical
 * Easting/Northing coordinate, for the projection of the earth
 * ellipsoid onto a transverse cylinder tangent to the earth at the
 * given reference point (for k0 = 1).
 *
 * For k0 = 1, this reduces to the inverse of the transverse Mercator
 * Projection.  For k0 < 1, this is a secant projection, where the
 * cylinder is not tangent to the earth, but intersects the earth at
 * two lines which are parallel to the reference meridian.
 */
Lla Geoid::inverseTransverseSecantProjection(LengthTriplet& xyk, Lla& referenceLla, double k0)
{
  Length x = xyk.x_;
  Length y = xyk.y_;

  double e2 = firstEccentricitySquared();
  double e4 = e2*e2;
  double e6 = e4*e2;
  double sq1me2 = sqrt(1.0 - e2);

  Length M0 = earthEquatorialRadius() * utmSeriesExpansionForM(referenceLla.latitude_);
  Length M  = M0 + y / k0;
  double mu = (M / earthEquatorialRadius()) * 1.0/(1 - e2/4 - 3*e4/64 - 5*e6/256);
  double e1 = (1 - sq1me2) / (1 + sq1me2);

  double e12 = e1*e1;
  double e13 = e12*e1;
  double e14 = e14*e1;

  double s2mu = sin(2*mu);
  double s4mu = sin(4*mu);
  double s6mu = sin(6*mu);
  double s8mu = sin(8*mu);

  double lr = mu 
    + s2mu * (3*e1/2 - 27*e13/32)
    + s4mu * (21*e12/16 - 55*e14/32)
    + s6mu * (151*e13/96)
    + s8mu * (1097*e14/512);

  double clr  = cos(lr);
  double slr  = sin(lr);
  double tlr  = slr/clr;
  double sq1me2slr2 = sqrt(1.0 - e2 * slr * slr);

  double ep2  = e2/(1-e2);
  double C    = ep2 * clr * clr;
  double T    = tlr * tlr;
  Length N    = earthEquatorialRadius() / sq1me2slr2;
  Length R    = earthEquatorialRadius() * (1 - e2) / (sq1me2slr2 * sq1me2slr2 * sq1me2slr2);

  double T2 = T*T;
  double C2 = C*C;

  double D  = x / (N*k0);
  double D2 = D*D;
  double D3 = D2*D;
  double D4 = D3*D;
  double D5 = D4*D;
  double D6 = D5*D2;
  
  double latr = lr - (N/R * tlr) * (
				    + D2/2 
				    - (5 + 3*T + 10*C - 4*C2 - 9*ep2) * D4/24
				    + (61 + 90*T + 298*C + 45*T2 - 252*ep2 - 3*C) * D6/720
				    );
  
  double lngr = referenceLla.longitude_.radians() + (D - (1 + 2*T + C) * D3/6 + 
						     (5 - 2*C + 28*T - 3*C2 + 8*ep2 + 24*T2) * D5/120) / clr;

  Lla geodeticLla;

  geodeticLla.longitude_.setRadians(lngr);
  geodeticLla.latitude_.setRadians(latr);

  geodeticLla.setDatum(datum_, earthEquatorialRadius());
  geodeticLla.setCoordSystem(COORD_GEODETIC);

  return geodeticLla;
}

/**.......................................................................
 * Convert from Geodetic coordinates to UPS (Universal Polar
 * Stereographic) coordinates
 */
LengthTriplet Geoid::geodeticLlaAndEnuToUps(Lla& geodeticLla, LengthTriplet enu)
{
  // Convert from Geodetic coordinate + ENU in arbitrary input datum
  // to geodetic coordinate in the current datum

  Lla newGeodeticLla = geodeticLlaAndEnuToGeodeticLla(geodeticLla, enu);

  // Now compute UPS coordinates.  UPS is a stereographic projection
  // tangent to the earth at reference points of Longitude=0,
  // Latitude= +-90, with a scale factor of 0.994

  Lla referenceLla;
  referenceLla.longitude_.setDegrees(0.0);

  if(newGeodeticLla.latitude_.degrees() > 0)
    referenceLla.latitude_.setDegrees(90.0);
  else
    referenceLla.latitude_.setDegrees(-90.0);

  LengthTriplet ups = polarStereographicProjection(newGeodeticLla, referenceLla, 0.994);

  // UPS avoids dealing with negative easting by defining the
  // reference point to be at a 'false easting' and 'false northing'
  // of 2,000,000 meters

  Length  eastingOffset(Length::Meters(), 2000000);
  Length northingOffset(Length::Meters(), 2000000);

  ups.easting_  += eastingOffset;
  ups.northing_ += northingOffset;

  if(newGeodeticLla.latitude_.radians() >= 0.0) {
    ups.hemisphere_ = HEMI_NORTH;
  } else {
    ups.hemisphere_ = HEMI_SOUTH;
  }

  ups.setDatum(datum_, earthEquatorialRadius());
  ups.setCoordSystem(COORD_UPS);

  return ups;
}

/**.......................................................................
 * Convert from UPS (Universal Polar Stereographic) coordinates in
 * arbitrary input datum to Geodetic coordinates in the current datum
 */
Lla Geoid::upsToGeodeticLla(LengthTriplet& ups)
{
  // If the input UPS coordinate is not in the same datum as the
  // current datum, deproject to Geodetic LLA in the input datum, and
  // convert the result to Geodetic LLA in the current datum

  if(ups.datum_ != DATUM_NONE && ups.datum_ != datum_) {

    setDatum(ups.datum_, ups.sphericalRadius_);
    Lla geodeticLla = upsToGeodeticLla(ups);
    restoreDefaultDatum();

    LengthTriplet enu;
    return geodeticLlaAndEnuToGeodeticLla(geodeticLla, enu);
  }

  // Now compute Geodetic coordinates.  UPS is a stereographic projection
  // tangent to the earth at reference points of Longitude=0,
  // Latitude= +-90, with a scale factor of 0.994

  Lla referenceLla;
  referenceLla.longitude_.setDegrees(0.0);

  if(ups.hemisphere_ == HEMI_NORTH)
    referenceLla.latitude_.setDegrees(90.0);
  else
    referenceLla.latitude_.setDegrees(-90.0);

  // Remove any UPS-specific offsets

  LengthTriplet xyk = ups;

  Length  eastingOffset(Length::Meters(), 2000000);
  Length northingOffset(Length::Meters(), 2000000);

  xyk.x_ -= eastingOffset;
  xyk.y_ -= northingOffset;

  return inversePolarStereographicProjection(xyk, 0.994);
}

/**.......................................................................
 * My attempt to convert from Geodetic coordinates to UTM coordinates
 */
LengthTriplet Geoid::geodeticLlaAndEnuToUtm(Lla& geodeticLla, LengthTriplet enu)
{
  // Convert from the reference geodetic position + ENU offset
  // relative to that reference in arbitrary input datum, to geodetic
  // position in the current datum

  Lla newGeodeticLla = geodeticLlaAndEnuToGeodeticLla(geodeticLla, enu);
  
  // Now compute UTM coordinates.  UTM is a series of transverse
  // secant projections, at reference longitudes distributed 6 degrees
  // around the globe, and latitude zero.  The UTM projection uses a
  // scale factor of 0.9996, unlike the transverse Mercator
  // projection, which uses scale factor 1.

  Lla referenceLla;
  referenceLla.longitude_ = getUtmReferenceLongitude(newGeodeticLla.longitude_);
  referenceLla.latitude_.setDegrees(0.0);

  LengthTriplet utm = transverseSecantProjection(newGeodeticLla, referenceLla, 0.9996);

  // UTM avoids dealing with negative easting by defining the
  // reference longitude to be at a 'false easting' of 500,000 meters

  Length eastingOffset(Length::Meters(), 500000);
  utm.easting_  += eastingOffset;

  // Also, the northing is defined to increase from zero from the
  // equator northwards, but to decrease from 10,000,000 m from the
  // equator southwards.

  Length northingOffset(Length::Meters(), 10000000);

  if(newGeodeticLla.latitude_.radians() >= 0.0) {
    utm.hemisphere_ = HEMI_NORTH;
  } else {
    utm.northing_   = northingOffset - utm.northing_;
    utm.hemisphere_ = HEMI_SOUTH;
  }

  utm.zone_ = longitudeToUtmZone(newGeodeticLla.longitude_);

  utm.setDatum(datum_, earthEquatorialRadius());
  utm.setCoordSystem(COORD_UTM);

  return utm;
}

/**.......................................................................
 * Convert from UTM to Geodetic
 */
Lla Geoid::utmToGeodeticLla(LengthTriplet& utm)
{
  // If the input UPS coordinate is not in the same datum as the
  // current datum, deproject to Geodetic LLA in the input datum, and
  // convert the result to Geodetic LLA in the current datum

  if(utm.datum_ != DATUM_NONE && utm.datum_ != datum_) {

    setDatum(utm.datum_, utm.sphericalRadius_);
    Lla geodeticLla = utmToGeodeticLla(utm);
    restoreDefaultDatum();

    LengthTriplet enu;
    return geodeticLlaAndEnuToGeodeticLla(geodeticLla, enu);
  }

  if(utm.hemisphere_ == HEMI_NONE) {
    ThrowError("You must specify a hemisphere in the input utm object");
  }

  // Remove any UTM-specific offsets

  LengthTriplet xyk;

  Length eastingOffset(Length::Meters(), 500000);
  xyk.x_ = utm.easting_ - eastingOffset;

  Length northingOffset(Length::Meters(), 10000000);
  
  // Subtract the false northing, if the point is in the southern
  // hemisphere

  if(utm.hemisphere_ == HEMI_SOUTH) 
    xyk.y_ = northingOffset - utm.northing_;
  else
    xyk.y_ = utm.northing_;
	
  // Now get the reference point (specified by zone) corresponding to
  // this coordinate

  Lla referenceLla;
  referenceLla.longitude_ = utmZoneToLongitude(utm.zone_);
  referenceLla.latitude_.setDegrees(0.0);

  // And deproject to geodetic LLA

  return inverseTransverseSecantProjection(xyk, referenceLla, 0.9996);
}

double Geoid::utmSeriesExpansionForM(Angle& lat)
{
  double   lr = lat.radians();
  double s2lr = sin(2*lr);
  double s4lr = sin(4*lr);
  double s6lr = sin(6*lr);

  double e2 = firstEccentricitySquared();
  double e4 = e2*e2;
  double e6 = e4*e2;

  return 
    + lr * (1 - e2/4 - 3*e4/64 - 5*e6/256)
    - s2lr * (3*e2/8 + 3*e4/32 + 45*e6/1024)
    + s4lr * (15*e4/256 + 45*e6/1024)
    - s6lr * (35*e6/3072);
}

/**.......................................................................
 * Return the UTM zone number corresponding to the given longitude
 */
unsigned Geoid::longitudeToUtmZone(Angle longitude)
{
  // Zones are 6 degrees of longitude, numbered 1-60, starting at Zone
  // 1, which is centered on 177 degrees W longitude.  Longitudes
  // which are right on the boundary are apparently correctly
  // considered to be in either zone

  double lngDeg = longitude.degrees();
  double lngMin = -180.0;

  unsigned zone = (unsigned)floor((lngDeg - lngMin)/6.0) + 1;

  return zone;
}

/**.......................................................................
 * Return the UTM zone number corresponding to the given longitude
 */
Angle Geoid::utmZoneToLongitude(unsigned zone)
{
  if(zone == 0 || zone > 60) {
    ThrowError("UTM zone must be 1-60");
  }

  // Zones are 6 degrees of longitude, numbered 1-60, starting at Zone
  // 1, which is centered on 177 degrees W longitude.  Longitudes
  // which are right on the boundary are apparently correctly
  // considered to be in either zone

  double lngMin = -180.0;

  Angle refLng;
  refLng.setDegrees((zone - 1) * 6 + lngMin + 3.0);
  return refLng;
}

/**.......................................................................
 * Return the UTM zone number corresponding to the given longitude
 */
Angle Geoid::getUtmReferenceLongitude(Angle longitude)
{
  unsigned zone = longitudeToUtmZone(longitude);
  double lngMin = -180;
  
  Angle refLng;

  refLng.setDegrees(lngMin + (zone-1)*6.0 + 3.0);
  return refLng;
}

/**.......................................................................
 * Convert from UTM coordinate in any input datum to UTM
 * coordinate in the current datum
 */
LengthTriplet Geoid::utmToUtm(LengthTriplet& utm)
{
  Lla geodeticLla = utmToGeodeticLla(utm);
  return geodeticLlaAndEnuToUtm(geodeticLla);
}

/**.......................................................................
 * Given a Geodetic reference LLA, return the Geodetic coordinates of
 * a point offset by deltaUtm from that reference.
 */
Lla Geoid::geodeticLlaAndDeltaUtmToGeodeticLla(Lla& geodeticLla, LengthTriplet& deltaUtm)
{
  // Ensure that the conversion to UTM is done in the native input
  // datum (same datum as the deltaUtm)

  setDatum(geodeticLla.datum_, geodeticLla.sphericalRadius_);
  LengthTriplet utm = geodeticLlaAndEnuToUtm(geodeticLla);
  restoreDefaultDatum();

  // Add the UTM offset in the input datum

  utm.easting_  += deltaUtm.easting_;
  utm.northing_ += deltaUtm.northing_;

  // And convert back to Geodetic coordinates in the current datum

  Lla newGeodeticLla = utmToGeodeticLla(utm);

  // How do we handle UTM 'up' offset?  What does it mean to survey a
  // location and get a vertical UTM offset?  It is ill-defined at
  // best.  
  //
  // For now, treat it like a vertical offset from the geodetic
  // altitude of the reference point

  newGeodeticLla.altitude_ = geodeticLla.altitude_ + deltaUtm.z_;

  newGeodeticLla.setDatum(datum_, earthEquatorialRadius());
  newGeodeticLla.setCoordSystem(COORD_GEODETIC);

  return newGeodeticLla;
}

/**.......................................................................
 * Return the XYZ coordinates of the specified (R, Ha, Dec)
 * coordinate.
 */
Vector<double> Geoid::haDecToXyzDir(HourAngle ha, Declination dec)
{
  Length distance;
  distance.setMeters(1.0);

  LengthTriplet xyz = haDecDistToXyz(ha, dec, distance);

  Vector<double> xyzDir = xyz / xyz.magnitude();

  return xyzDir;
}

/**.......................................................................
 * Return the XYZ coordinates of the specified (Ha, Dec, Distance)
 * coordinate.
 */
LengthTriplet Geoid::haDecDistToXyz(HourAngle ha, Declination dec, Length distance)
{
  // Construct the input source coordinate in UVW coordinates

  Vector<Length> uvw(3);
  uvw[2] = distance;

  // Get the rotation matrix to convert from UVW to XYZ

  Matrix<double> xyzToUvwDecRot = getXyzToUvwDecRot(dec);
  Matrix<double> xyzToUvwHaRot  = getXyzToUvwHaRot(ha);
  Matrix<double> xyzToUvwRot    = (xyzToUvwDecRot * xyzToUvwHaRot);
  Matrix<double> uvwToXyzRot    = xyzToUvwRot.inverse();
  
  // Now calculate the XYZ coordinates
  
  LengthTriplet xyz;

  xyz = (uvwToXyzRot * uvw);
  xyz.setCoordSystem(COORD_XYZ);

  return xyz;
}

//-----------------------------------------------------------------------
// Convenience operators for output of datatypes used by this class
//-----------------------------------------------------------------------

std::ostream& sza::util::operator<<(std::ostream& os, const Lla& lla)
{
  return operator<<(os, (Lla&) lla);
}

std::ostream& sza::util::operator<<(std::ostream& os, Lla& lla)
{
  os << "Longitude    = " << lla.longitude_   << std::endl;
  os << "Latitude     = " << lla.latitude_    << std::endl;
  os << "Altitude     = " << lla.altitude_    << std::endl;
  os << "Datum        = " << lla.datum_       << std::endl;
  os << "Coord system = " << lla.coordSystem_ << std::endl;

  return os;
}

std::ostream& sza::util::operator<<(std::ostream& os, const LengthTriplet& triplet)
{
  return operator<<(os, (LengthTriplet&)triplet);
}

std::ostream& sza::util::operator<<(std::ostream& os, LengthTriplet& triplet)
{
  switch (triplet.coordSystem_) {
  case COORD_UTM:
    os << "Easting      = " << fixed << std::setprecision(3) << triplet.easting_  << std::endl;
    os << "Northing     = " << fixed << std::setprecision(3) << triplet.northing_ << std::endl;
    os << "k            = " << std::right << triplet.k_        << std::endl;
    os << "Zone         = " << triplet.zone_ 
       << " (" << (triplet.hemisphere_ == HEMI_NORTH ? "N" : "S") << ")" << std::endl;
    os << "Datum        = " << triplet.datum_   << std::endl;
    break;
  case COORD_UPS:
    os << "Easting      = " << triplet.easting_  << std::endl;
    os << "Northing     = " << triplet.northing_ << std::endl;
    os << "k            = " << triplet.k_        << std::endl;
    os << "Zone         = " << "polar"
       << " (" << (triplet.hemisphere_ == HEMI_NORTH ? "N" : "S") << ")" << std::endl;
    os << "Datum        = " << triplet.datum_    << std::endl;
    break;
  case COORD_ECF:
    os << "X            = " << triplet.X_ << std::endl;
    os << "Y            = " << triplet.Y_ << std::endl;
    os << "Z            = " << triplet.Z_ << std::endl;
    break;
  case COORD_XYZ:
    os << "X            = " << triplet.X_ << std::endl;
    os << "Y            = " << triplet.Y_ << std::endl;
    os << "Z            = " << triplet.Z_ << std::endl;
    break;
  case COORD_ENU:
    os << "E            = " << triplet.east_  << std::endl;
    os << "N            = " << triplet.north_ << std::endl;
    os << "U            = " << triplet.up_    << std::endl;
    break;
  case COORD_ENH:
    os << "E            = " << triplet.east_  << std::endl;
    os << "N            = " << triplet.north_ << std::endl;
    os << "H            = " << triplet.up_    << std::endl;
    break;
  default:
    os << "x            = " << triplet.x_ << std::endl;
    os << "y            = " << triplet.y_ << std::endl;
    os << "z            = " << triplet.z_ << std::endl;
    break;
  }

  os <<   "Coord system = " << triplet.coordSystem_ << std::endl;

  return os;
}

std::ostream& sza::util::operator<<(std::ostream& os, GeoDatum datum)
{
  switch (datum) {
  case  DATUM_SPHERE:
    os << "SPHERE";
    break;
  case DATUM_CLARKE66:
    os << "CLARKE66";
    break;
  case DATUM_NAD27:
    os << "NAD27";
    break;
  case DATUM_ANS:
    os << "ANS";
    break;
  case DATUM_AGD66:
    os << "AGD66";
    break;
  case DATUM_GDA84:
    os << "GDA84";
    break;
  case DATUM_GRS80:
    os << "GRS80";
    break;
  case DATUM_GDA94:
    os << "GDA94";
    break;
  case DATUM_GDA2000:
    os << "GDA2000";
    break;
  case DATUM_NAD83:
    os << "NAD83";
    break;
  case DATUM_WGS84:
    os << "WGS84";
    break;

  case DATUM_GPS:
    os << "GPS";
    break;
  default:
    os << "UNKNOWN";
    break;
  }

  return os;
}

GeoDatum sza::util::stringToDatum(std::string str)
{
  if(str == "SPHERE") {
    return DATUM_SPHERE;
  } else if(str == "CLARKE66") {
    return DATUM_CLARKE66;
  } else if(str == "NAD27") {
    return DATUM_NAD27;
  } else if(str == "ANS") {
    return DATUM_ANS;
  } else if(str == "AGD66") {
    return DATUM_AGD66;
  } else if(str == "GDA84") {
    return DATUM_GDA84;
  } else if(str == "GRS80") {
    return DATUM_GRS80;
  } else if(str == "GDA94") {
    return DATUM_GDA94;
  } else if(str == "GDA2000") {
    return DATUM_GDA2000;
  } else if(str == "NAD83") {
    return DATUM_NAD83;
  } else if(str == "WGS84") {
    return DATUM_WGS84;
  } else if(str == "GPS") {
    return DATUM_GPS;
  } else {
    return DATUM_NONE;
  }
}

std::ostream& sza::util::operator<<(std::ostream& os, GeoCoordSystem coordSystem)
{
  switch (coordSystem) {
  case  COORD_UTM:
    os << "UTM";
    break;
  case  COORD_UPS:
    os << "UPS";
    break;
  case  COORD_ENH:
    os << "ENH";
    break;
  case  COORD_ENU:
    os << "ENU";
    break;
  case  COORD_ECF:
    os << "ECF";
    break;
  case  COORD_XYZ:
    os << "XYZ";
    break;
  case  COORD_GEODETIC:
    os << "Geodetic";
    break;
  case  COORD_GEOCENTRIC:
    os << "Geocentric";
    break;
  default:
    os << "UNKNOWN";
    break;
  }

  return os;
}

/**.......................................................................
 * Return the declination rotation matrix for transforming between XYZ
 * and UVW
 */
sza::util::Matrix<double> Geoid::getXyzToUvwDecRot(Declination dec)
{
  sza::util::Matrix<double> decRot(3,3);
  
  // Precompute sines and cosines
  
  double cdec = cos(dec.radians());
  double sdec = sin(dec.radians());

  // Construct a rotation matrix for the declination rotation
  
  decRot[0][0] =  1; decRot[0][1] =     0; decRot[0][2] =     0;
  decRot[1][0] =  0; decRot[1][1] =  sdec; decRot[1][2] =  cdec;
  decRot[2][0] =  0; decRot[2][1] = -cdec; decRot[2][2] =  sdec;
  
  return decRot;  
}

/**.......................................................................
 * Return the HA rotation matrix for transforming between XYZ and UVW
 */
sza::util::Matrix<double> Geoid::getXyzToUvwHaRot(HourAngle ha)
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
 * Return the latitude rotation matrix to convert from geocentric ENH
 * coordinates to XYZ
 */
sza::util::Matrix<double> Geoid::getEnhToXyzLatRot(Angle geocentricLatitude)
{
  sza::util::Matrix<double> latRot(3,3);
  
  // Precompute sines and cosines
  
  double clat = cos(geocentricLatitude.radians());
  double slat = sin(geocentricLatitude.radians());

  // Construct a rotation matrix for the latitude rotation

  latRot[0][0] = 0; latRot[0][1] = -slat; latRot[0][2] = clat;
  latRot[1][0] = 1; latRot[1][1] =     0; latRot[1][2] =    0;
  latRot[2][0] = 0; latRot[2][1] =  clat; latRot[2][2] = slat;

  return latRot;
}

/**.......................................................................
 * Return the longitude rotation matrix to convert from XYZ to ECF
 * coordinates
 */
sza::util::Matrix<double> Geoid::getXyzToEcfLngRot(Angle longitude)
{
  Matrix<double> lngRot(3,3);

  double clng = cos(longitude.radians());
  double slng = sin(longitude.radians());

  lngRot[0][0] = clng; lngRot[0][1] = -slng; lngRot[0][2] = 0;
  lngRot[1][0] = slng; lngRot[1][1] =  clng; lngRot[1][2] = 0;
  lngRot[2][0] =    0; lngRot[2][1] =     0; lngRot[2][2] = 1;

  return lngRot;
}

/**.......................................................................
 * Return the rotation about the east axis to convert from Geodetic
 * ENU to Geocentric ENH
 */
sza::util::Matrix<double> Geoid::getEnuToEnhRot(Angle latDiff)
{
  sza::util::Matrix<double> decRot(3,3);
  
  Matrix<double> eastRot(3,3);

  double cd = cos(latDiff.radians());
  double sd = sin(latDiff.radians());

  eastRot[0][0] = 1; eastRot[0][1] =   0; eastRot[0][2] =  0;
  eastRot[1][0] = 0; eastRot[1][1] =  cd; eastRot[1][2] = sd;
  eastRot[2][0] = 0; eastRot[2][1] = -sd; eastRot[2][2] = cd;

  return eastRot;  
}

/**.......................................................................
 * Return the UVW coordinates of the specified (HA, Dec) point.  
 */
LengthTriplet 
Geoid::geodeticLlaAndHaDecToUvw(Lla& geodeticLla, 
				HourAngle ha, Declination declination) 
{
  LengthTriplet xyz = geodeticLlaAndEnuToXyz(geodeticLla);

  // Get the rotation matrix for the coordinates of this object

  Matrix<double> decRot = getXyzToUvwDecRot(declination);
  Matrix<double> haRot  = getXyzToUvwHaRot(ha);
  Matrix<double> rot    = (decRot * haRot);
  
  // Now calculate the UVW coordinates
  
  LengthTriplet uvw = (rot * xyz);

  uvw.setCoordSystem(COORD_UVW);

  return uvw;
}

/**.......................................................................
 * Return the (Az, El) coordinates of the specified (HA, Dec) point.  
 */
PolarLengthVector
Geoid::geodeticLlaAndHaDecToAzEl(Lla& geodeticLla, 
				 HourAngle ha, Declination declination, 
				 SrcDist type, Length distance)
{
  //------------------------------------------------------------
  // Get the geocentric XYZ direction of the source vector.  Passing
  // SrcType INF will return the source direction only (XYZ vector of
  // unit length).
  //------------------------------------------------------------

  Vector<double> xyzSrcDir = haDecToXyzDir(ha, declination);

  //------------------------------------------------------------
  // Get the geocentric (XYZ) direction of this geodetic point of
  // observation
  //------------------------------------------------------------

  LengthTriplet xyzObs = geodeticLlaAndEnuToXyz(geodeticLla);
  Lla geocentricLla = geodeticLlaAndEnuToGeocentricLla(geodeticLla);

  //------------------------------------------------------------
  // Normalize to unit length
  //------------------------------------------------------------

  Vector<double> xyzObsDir = xyzObs / xyzObs.magnitude();

  //------------------------------------------------------------
  // Factor of R_obs/R_src, or 0 if source is infinitely far away
  //------------------------------------------------------------

  double fac = (type==DIST_INF ? 0.0 : xyzObs.magnitude() / distance);

  //------------------------------------------------------------
  // Get the topocentric source direction.  In the case of type==INF,
  // this will be the same as the geocentric
  //------------------------------------------------------------

  Vector<double> topoXyzSrcDir = (xyzSrcDir - xyzObsDir * fac);
  topoXyzSrcDir = topoXyzSrcDir/sqrt(1 - 2*fac*(xyzSrcDir*xyzObsDir) + fac*fac);

  //------------------------------------------------------------
  // Get the rotation matrix to convert from XYZ to Geocentric ENH
  //------------------------------------------------------------

  Matrix<double> enhToXyzRot = getEnhToXyzLatRot(geocentricLla.latitude_);
  Matrix<double> xyzToEnhRot = enhToXyzRot.inverse();
  Vector<double> enhSrcDir   = (xyzToEnhRot * topoXyzSrcDir);

  //------------------------------------------------------------
  // Finally, convert from Geocentric ENH to Geodetic ENU direction
  // _at the point of observation_.  
  //
  // Note that this differs from the ENU to ENH rotations we do in
  // geodeticLlaAndEnuToEcf() and geodeticLlaAndEnuToEcf() in that
  // there, we are rotating about a point on the surface of the earth.
  //
  // Here, we are rotating about a point above the surface of the
  // earth given by the intersection of Geodetic latitude and the
  // Geocentric latitude corresponding to that point, ie, not the
  // Geocentric latitude corresponding to the intersection of the
  // Geodetic latitude with the surface of the earth
  //------------------------------------------------------------

  Matrix<double> eastRot   = getEnuToEnhRot(geodeticLla.latitude_ - geocentricLla.latitude_).inverse();
  Vector<double> enuSrcDir = eastRot * enhSrcDir;

  //------------------------------------------------------------
  // Now calculate the Az/El from the Geodetic ENU coordinates
  //------------------------------------------------------------

  Length normalization(Length::Meters(), 1.0);
  LengthTriplet enu = enuSrcDir * normalization;

  PolarLengthVector srcAzEl = enuToAzEl(enu);

  //------------------------------------------------------------
  // Trap Az or El = nan.  This can happen if geoXyzSrc == geoXyzObs,
  // e.g., if the user inputs a source location which exactly
  // coincides with the XYZ of the observation. Although this is an
  // unrealistic situation, it should be noted.
  //------------------------------------------------------------

  if(!isfinite(srcAzEl.az_.radians()) || !isfinite(srcAzEl.el_.radians())) {
    ThrowError("Az/El position is not defined for Ha = " << ha
	       << ", Dec = " << declination
	       << " at geodetic latitude = " << geodeticLla.latitude_ << endl);
  } 

  return srcAzEl;
}

#if 0
/**.......................................................................
 * Return the (HA, Dec) coordinates of the specified (Az, El) point.  
 */
PolarLengthVector
Geoid::geodeticLlaAndAzElToHaDec(Lla& geodeticLla, 
				 Angle az, Angle el,
				 SrcDist type, Length distance)
{
  //------------------------------------------------------------
  // Get the ENU direction of the source vector
  //------------------------------------------------------------

  LengthTriplet enu = azElToEnu(az, el, Length(Length::Meters(), 1.0));
  Vector<double> enuSrcDir = enu / enu.magnitude();

  //------------------------------------------------------------
  // Get the geocentric XYZ direction of the source vector.  Passing
  // SrcType INF will return the source direction only (XYZ vector of
  // unit length).
  //------------------------------------------------------------

  Vector<double> xyzSrcDir = haDecToXyzDir(ha, declination);

  //------------------------------------------------------------
  // Get the geocentric (XYZ) direction of this geodetic point of
  // observation
  //------------------------------------------------------------

  LengthTriplet xyzObs = geodeticLlaAndEnuToXyz(geodeticLla);
  Lla geocentricLla = geodeticLlaAndEnuToGeocentricLla(geodeticLla);

  //------------------------------------------------------------
  // Normalize to unit length
  //------------------------------------------------------------

  Vector<double> xyzObsDir = xyzObs / xyzObs.magnitude();

  //------------------------------------------------------------
  // Factor of R_obs/R_src, or 0 if source is infinitely far away
  //------------------------------------------------------------

  double fac = (type==DIST_INF ? 0.0 : xyzObs.magnitude() / distance);

  //------------------------------------------------------------
  // Get the topocentric source direction.  In the case of type==INF,
  // this will be the same as the geocentric
  //------------------------------------------------------------

  Vector<double> topoXyzSrcDir = (xyzSrcDir - xyzObsDir * fac);
  topoXyzSrcDir = topoXyzSrcDir/sqrt(1 - 2*fac*(xyzSrcDir*xyzObsDir) + fac*fac);

  //------------------------------------------------------------
  // Get the rotation matrix to convert from XYZ to Geocentric ENH
  //------------------------------------------------------------

  Matrix<double> enhToXyzRot = getEnhToXyzLatRot(geocentricLla.latitude_);
  Matrix<double> xyzToEnhRot = enhToXyzRot.inverse();
  Vector<double> enhSrcDir   = (xyzToEnhRot * topoXyzSrcDir);

  //------------------------------------------------------------
  // Finally, convert from Geocentric ENH to Geodetic ENU direction
  // _at the point of observation_.  
  //
  // Note that this differs from the ENU to ENH rotations we do in
  // geodeticLlaAndEnuToEcf() and geodeticLlaAndEnuToEcf() in that
  // there, we are rotating about a point on the surface of the earth.
  //
  // Here, we are rotating about a point above the surface of the
  // earth given by the intersection of Geodetic latitude and the
  // Geocentric latitude corresponding to that point, ie, not the
  // Geocentric latitude corresponding to the intersection of the
  // Geodetic latitude with the surface of the earth
  //------------------------------------------------------------

  Matrix<double> eastRot   = getEnuToEnhRot(geodeticLla.latitude_ - geocentricLla.latitude_).inverse();
  Vector<double> enuSrcDir = eastRot * enhSrcDir;

  //------------------------------------------------------------
  // Now calculate the Az/El from the Geodetic ENU coordinates
  //------------------------------------------------------------

  Length normalization(Length::Meters(), 1.0);
  LengthTriplet enu = enuSrcDir * normalization;

  PolarLengthVector srcAzEl = enuToAzEl(enu);

  //------------------------------------------------------------
  // Trap Az or El = nan.  This can happen if geoXyzSrc == geoXyzObs,
  // e.g., if the user inputs a source location which exactly
  // coincides with the XYZ of the observation. Although this is an
  // unrealistic situation, it should be noted.
  //------------------------------------------------------------

  if(!isfinite(srcAzEl.az_.radians()) || !isfinite(srcAzEl.el_.radians())) {
    ThrowError("Az/El position is not defined for Ha = " << ha
	       << ", Dec = " << declination
	       << " at geodetic latitude = " << geodeticLla.latitude_ << endl);
  } 

  return srcAzEl;
}

#endif

/**.......................................................................
 * Given Geodetic ENU, return the Azimuth and Elevation
 */
PolarLengthVector Geoid::enuToAzEl(LengthTriplet& enu)
{
  PolarLengthVector azel;
  azel.setCoordSystem(COORD_AZEL);

  // Azimuth is the angle of the projection onto the N-E plane
  
  azel.az_.setRadians(atan2(enu.east_.meters(), enu.north_.meters()));
  
  // Elevation is 90 - the angle between the coordinate vector and the U axis
  
  azel.el_.setRadians(asin(enu.up_ / enu.magnitude()));

  // Set the length too

  azel.length_ = enu.magnitude();

  return azel;
}

/**.......................................................................
 * Given Az and El and distance to a source, return the geodetic ENU
 */
LengthTriplet Geoid::azElToEnu(Angle az, Angle el, Length distance)
{
  LengthTriplet enu;
  enu.setCoordSystem(COORD_ENU);

  // cos(el) is the projection of the vector onto the E-N plane
  
  double cel = cos(el.radians());

  enu.east_  = distance * cel * sin(az.radians()); // E component
  enu.north_ = distance * cel * cos(az.radians()); // N component
  enu.up_    = distance * sin(el.radians());       // U component

  return enu;
}

/**.......................................................................
 * Given XYZ, return the Ha and Dec
 */
PolarLengthVector Geoid::xyzToHaDec(LengthTriplet& xyz)
{
  PolarLengthVector hadec;
  hadec.setCoordSystem(COORD_HADEC);

  // 90-HA is the angle of the projection onto the Y-X plane
  
  hadec.ha_.setRadians(atan2(xyz.X_.meters(), xyz.Y_.meters()) - M_PI/2);
  
  // Declination is the angle between the coordinate vector and the Z axis
  
  hadec.dec_.setRadians(asin(xyz.Z_ / xyz.magnitude()));

  // And set the length

  hadec.length_ = xyz.magnitude();

  COUT("hadec.coordSystem = " << hadec.coordSystem_);

  return hadec;
}

/**.......................................................................
 * Given Ha and Dec and distance to a source, return the XYZ coordinates
 */
LengthTriplet Geoid::haDecDistToXyz2(HourAngle ha, Declination dec, Length distance)
{
  LengthTriplet xyz;
  xyz.setCoordSystem(COORD_XYZ);

  // cos(dec) is the projection of the vector onto the X-Y plane
  
  double cdec = cos(dec.radians());

  xyz.X_  =   distance * cdec *  cos(ha.radians()); // X component
  xyz.Y_  =   distance * cdec * -sin(ha.radians()); // Y component
  xyz.Z_  =   distance * sin(dec.radians());        // Z component

  return xyz;
}

/**.......................................................................
 * Print a polar length vector
 */
std::ostream& sza::util::operator<<(std::ostream& os, const PolarLengthVector& angles)
{
  return operator<<(os, (PolarLengthVector&) angles);
}

/**.......................................................................
 * Print a polar length vector
 */
std::ostream& sza::util::operator<<(std::ostream& os, PolarLengthVector& angles)
{
  switch (angles.coordSystem_) {
  case COORD_AZEL:
    os << "Az = " << angles.az_ << std::endl
       << "El = " << angles.el_;
    break;
  case COORD_HADEC:
    os << "HA = " << angles.ha_ << std::endl
       << "Dec = " << angles.dec_;
    break;
  default:
    os << "Unknown";
    break;
  }

  return os;
}

/**.......................................................................
 * Multiply a LengthTriplet by a double matrix
 */
Vector<Length> sza::util::operator*(Matrix<double>& mat, LengthTriplet& triplet)
{
  return operator*(mat, triplet.coords_);
}

LengthTriplet Geoid::geodeticLlaAndGeodeticLlaToEnu(Lla& geodeticLlaRef, Lla& geodeticLlaObs)
{
  LengthTriplet ecfRef = geodeticLlaAndEnuToEcf(geodeticLlaRef);
  LengthTriplet ecfObs = geodeticLlaAndEnuToEcf(geodeticLlaObs);

  COUT(ecfRef);
  COUT(ecfObs);

  LengthTriplet deltaEcf = ecfObs - ecfRef;

  COUT(deltaEcf);

  Matrix<double> latRot(3,3);

  double clat = cos(geodeticLlaRef.latitude_.radians());
  double slat = sin(geodeticLlaRef.latitude_.radians());

  // Construct a rotation matrix for the longitude rotation

  Matrix<double> lngRot(3,3);

  double clng = cos(geodeticLlaRef.longitude_.radians());
  double slng = sin(geodeticLlaRef.longitude_.radians());

  lngRot[0][0] =  clng; lngRot[0][1] = slng; lngRot[0][2] = 0;
  lngRot[1][0] = -slng; lngRot[1][1] = clng; lngRot[1][2] = 0;
  lngRot[2][0] =     0; lngRot[2][1] =    0; lngRot[2][2] = 1;

  LengthTriplet deltaXyz = lngRot * deltaEcf;

  // Construct a rotation matrix for the latitude rotation

  latRot[0][0] =     0; latRot[0][1] = 1; latRot[0][2] =    0;
  latRot[1][0] = -slat; latRot[1][1] = 0; latRot[1][2] = clat;
  latRot[2][0] =  clat; latRot[2][1] = 0; latRot[2][2] = slat;

  LengthTriplet deltaEnu = latRot * deltaXyz;

  deltaEnu.setCoordSystem(COORD_ENU);
  deltaEnu.setDatum(defaultDatum_, earthEquatorialRadius());

  return deltaEnu;
}
