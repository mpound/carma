#ifndef SZA_UTIL_COORDINATES_H
#define SZA_UTIL_COORDINATES_H

/**
 * @file Coordinates.h
 * 
 * Tagged: Tue May  4 15:49:11 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Angle.h"
#include "carma/szautil/DecAngle.h"
#include "carma/szautil/Declination.h"
#include "carma/szautil/HourAngle.h"
#include "carma/szautil/Delay.h"
#include "carma/szautil/Vector.h"

namespace sza {
  namespace util {
    
    /**
     * This class performs conversions between several different
     * coordinate systems.  These are described below.
     *
     * (L, L, A): The longiude/latitude/altitude coordinate system.  I
     * will refer to this in shorthand as LLA, and coordinates as (L,
     * L, A).  Longitude increases from 0 at Greenwich, England in an
     * easterly direction, and decreases in a westerly direction.
     * East longitudes are correctly described as positive sexagesimal
     * numbers, typically between 0 and 180 degrees.  West longitudes
     * are often confusingly specified as positive numbers between 0
     * and 180, with a 'W' appended, e.g., +XXX:XX:XX.XX W, or even
     * more confusingly, the W is simply dropped altogether, creating
     * an ambiguity between E and W longitudes.  West longitudes
     * should correctly be described as negative numbers between 0 and
     * -180, or equivalently, as positive numbers between 180 and 360.
     * In this document, no distinction is made between E and W
     * longitudes.  Where applicable, W longitudes should be passed as
     * negative numbers.
     * 
     * (X, Y, Z): Thompson, Moran and Swenson (Eq. 4.15, 1st ed.)
     * describe a coordinate system for specifying relative antenna
     * locations: they call it (X, Y, Z).  In their system, Z lies
     * along the North Celestial Pole, X lies in the plane passing
     * through an arbitrary longitude/latitude point (at 0 Hour
     * Angle), and Y lies at -6h Hour Angle.  Thus to convert from a
     * latitude, longitude point to (X, Y, Z), one has only to perform
     * a translation and a latitude rotation. I interpret their XYZ
     * coordinate system to have as its origin the center of the
     * Earth, but there are differing opinions about this.  Some think
     * it should be taken to be fixed to the surface at some fiducial
     * point.  The two interpretations are simply related by a
     * translation by the radius of the earth, and the difference is
     * irrelevant when dealing with baseline coordinates, since those
     * are always relative.
     *
     * (U, V, W) : Coordinates tied to the source reference frame
     * (e.g., TMS, section 4.2).  These coordinates are simply related
     * to (X, Y, Z) by a rotation of 90-dec about the X axis, followed
     * by a rotation by 90-H about the Z axis, where dec is the source
     * declination and H the hour angle of the source.
     *
     * (XA, YA, ZA): Same as the (X, Y, Z) system described by TMS,
     * but with X in the plane of the Greenwich meridian, i.e., it is
     * an absolute geocentric coordinate system.  getAbsXYZ() takes a
     * longitude, latitude, altitude, and an east, north, up offset
     * relative to that long/lat/alt point, and converts it to an
     * absolute (XA, YA, ZA) vector, in meters.  By comparison with
     * XYZ' XYZ coordinate system, to convert from a lat/long/alt
     * point to (XA, YA, ZA), one has to perform a translation,
     * followed by both a longitude rotation and a latitude rotation.
     *
     * (E, N, U): This coordinate system is useful for specifying
     * locations relative to a given LLA point.  Coordinates are in a
     * tangent plane at the specified LLA point, with N pointing due
     * north, E pointing due east, and U pointing straight up along a
     * radial vector from the center of the earth, and passing through
     * the given LLA point.  To perform a conversion from UEN to XYZ
     * for example, one has to perform a translation by the Earth's
     * radius, followed by a longitude rotation and a latitude
     * rotation.
     *
     * This container will natively store coordinates in LLA, hence
     * the constructors which initialize these parameters.
     */
    class Coordinates : public Vector<double> {
    public:
      
      /**
       * Enumerate types of distances we might pass to methods
       */
      enum SrcDist {
	ACTUAL, // Actual source distance
	INF     // Source at infinity
      };
      
      /**
       * Constructor.
       */
      Coordinates();
      
      /**
       * Constructor with initialization of LLA
       *
       * @param longitude Angle Longitude
       * @param latitude  Angle Latitude
       * @param altitude  Altitude, in meters
       */
      Coordinates(Angle longitude, Angle latitude, double altitude);
      
      /**
       * Copy constructor.
       */
      Coordinates(Coordinates& coord);
      
      /**
       * Destructor.
       */
      virtual ~Coordinates();
      
      /**
       * Set methods
       */
      void setLla(Angle longitude, Angle latitude, double altitude);
      
      /**
       * Set just the longitude
       */
      void setLongitude(Angle longitude);
      
      /**
       * Set just the latitude
       */
      void setLatitude(Angle latitude);
      
      /**
       * Set just the altitude
       */
      void setAltitude(double altitude);
      
      /**
       * Position return methods
       */
      inline Angle& longitude() {
	return longitude_;
      }
      
      inline Angle& latitude() {
	return latitude_;
      }
      
      inline double& altitude() {
	return altitude_;
      }
      
      /**
       * Return absolute XYZ coordinates of the given UEN point,
       * relative to the LLA point of this object.
       */
      Vector<double> getAbsXyz(double east=0.0, double north=0.0, double up=0.0);
      
      /**
       * Return XYZ coordinates of the given UEN point, relative to
       * the LLA point of this object.
       */
      Vector<double> getXyz(double east=0.0, double north=0.0, double up=0.0,
			    bool geocentric=true);
      
      /**
       * Return UEN coordinates of the specified LLA point, relative
       * to the coordinates stored in this object
       */
      Vector<double> getUen(Angle longitude, Angle latitude, double altitude);
      Vector<double> getUen(Coordinates& coords);
      
      /**
       * Return the UVW coordinates for the current LLA location, of a
       * source at the requested declination and hour angle.  Note
       * that the coordinates in this container refer to an absolute
       * position, or effectively, to a baseline stretching from the
       * origin of the XYZ coordinate system to the location stored in
       * this container, which is the center of the earth.
       */
      Vector<double> getUvw(HourAngle ha, DecAngle declination);
      
      /**
       * Return LLA of the passed UEN offset point, relative to the
       * LLA coordinates stored in this object.
       */
      Vector<double> getLla(double east=0.0, double north=0.0, double up=0.0);
      
      /**
       * Given a LLA coordinate, return its Az, El relative to the
       * position of this object.
       */
      Vector<Angle> getAzEl(Angle longitude, Angle latitude, double altitude);
      Vector<Angle> getAzEl(Coordinates& coords);
      
      /**
       * Increment the LLA of this object by the specified UEN offset
       */
      void add(double up, double east, double north);
      
      //------------------------------------------------------------
      // Static member functions
      //------------------------------------------------------------
      
      /**
       * Return the LL of the point given by an az/el offset relative
       * to a fiducial LL
       */
      static Vector<double> 
	lngLatAndAzElToLngLat(Angle& lng,       Angle& lat,
			      Angle& az,        Angle& el);

      /**
       * Return the LLA of the point given by an UEN offset relative
       * to a fiducial LLA.
       */
      static Vector<double> llaAndUenToLla(Angle longitude, Angle latitude, 
					   double altitude, 
					   double up, double east, double north);
      
      /**
       * Return the UEN of an LLA point relative to a fiducial LLA point.
       */
      static Vector<double> llaAndLlaToUen(Angle fiducialLongitude, 
					   Angle fiducialLatitude, 
					   double fiducialAltitude, 
					   Angle longitude, Angle latitude, 
					   double altitude);
      
      /**
       * Return absolute XYZ of the point offset by UEN from a given
       * LLA point.
       */
      static Vector<double> llaAndUenToAbsXyz(Angle longitude, Angle latitude,
					      double altitude,
					      double up, double east, double north, bool doTrans=true);
      
      /**
       * Given a topocentric XYZ, return the UEN coordinates at a
       * given latitude
       */
      static Vector<double> topoXYZToUen(double X, double Y, double Z, 
					 Angle latitude);
      
      /**
       * Given absolute XYZ, convert to LLA
       */
      static Vector<double> absXyzToLla(double XA, double YA, double ZA);
      
      /**
       * Return the LLA of the point given by the specified absolute
       * XYZ and UEN offset.
       */
      static Vector<double> absXyzAndUenToLla(double XA, double YA, double ZA,
					      double up=0.0, double east=0.0, 
					      double north=0.0);
      
      /**
       * Return the XYZ coordinates of an UEN point relative to a
       * fiducial LA. Note that no longitude is required, as the X-Z
       * plane of the XYZ coordinate system always lies in the local
       * meridian.
       *
       * If geocentric==true, the origin of the returned XYZ
       * coordinates will be the center of the earth.  If false, the
       * origin will be at the fiducial point on the surface.  Use
       * whichever definition pleases you -- you'd better get
       * identical results for a baseline specified in either system,
       * since those coordinates are always relative.
       */
      static Vector<double> laAndUenToXyz(Angle latitude, double altitude,
					  double up=0.0, double east=0.0, 
					  double north=0.0,
					  bool geocentric=true);
      
      /**
       * Given Azimuth and Elevation, return the UEN coordinates.
       * Note that with r==1.0, this function returns the direction
       * cosines of the position specified by az/el.
       */
      static Vector<double> azElToUen(Angle az, Angle el, double r=1.0);
      
      /**
       * Given UEN, return the Azimuth and Elevation
       */
      static Vector<Angle> uenToAzEl(double U, double E, double N);
      static Vector<Angle> uenToAzEl(Vector<double>& uen);
      
      /**
       * Return the UVW coordinates, relative to a given XYZ
       * coordinate, for the requested source position.  Note that if
       * the passed XYZ coordinates are the (relative) coordinates of
       * a baseline, you will get the UVW coordinates relative to the
       * origin of your baseline.  If the passed XYZ coordinates are
       * actual antenna coordinates, you will get the UVW coordinates
       * relative to the origin of the XYZ coordinate system.
       */
      static Vector<double> haDecAndXyzToUvw(HourAngle ha, DecAngle declination,
					     double X, double Y, double Z);
      /**
       * Return the XYZ coordinate of a vector at requested (Ha, Dec)
       * direction and of length r.
       *
       * @param geocentric If true, return geocentric (X, Y, Z).  If false,
       *                   returned coordinates will be topocentric.
       *
       * @param type       If ACTUAL, interpret r to mean an actual distance 
       *                   (in AU).  If INF, ignore r and return the source 
       *                   direction only.
       */
      static Vector<double> haDecToXyz(HourAngle ha, DecAngle declination,
				       bool geocentric=true, 
				       SrcDist type=INF, double r=0.0);
      
      //=======================================================================
      // Given a delta HA and delta DEC, return the XYZ (direction
      // cosines only) coordinate
      //=======================================================================

      /**
       * Return the (X, Y, Z) coordinates of the specified (Ha, Dec)
       * coordinate.  This function returns the direction cosines only.
       */
      static Vector<double> dHaDdecToXyz(HourAngle dHa, Angle dDec);
      
      /**
       * Return the dHa and dDec corresponding to the specified (X, Y, Z)
       * coordinate.
       */
      static void xyzToDhaDdec(double X, double Y, double Z, 
			       HourAngle& dHa, Angle& dDec);
      
      /**
       * Return the dHa and dDec corresponding to the specified (X, Y, Z)
       * coordinate.
       */
      static void xyzToDhaDdec(Vector<double>& xyzVals, 
			       HourAngle& dHa, Angle& dDec);
      
      //=======================================================================
      // Given polar offsets on the sky from a given dec position
      // (assumed to be at HA=0h), theta (azimuthal) and rho (radial),
      // return the XYZ direction cosines.
      //=======================================================================

      static Vector<double> thetaRhoToXyz(Declination& dec, Angle& theta, Angle& rho);

      static Matrix<double> getThetaRhoToXyzDecRot(Declination& dec);
      static Matrix<double> getThetaRhoToXyzThetaRot(Angle& theta);
      static Matrix<double> getThetaRhoToXyzRhoRot(Angle& rho);
      
      /**.......................................................................
       * Given fiducial position on the sky (assumed to be at 0h), and
       * a polar offset (theta, rho) from it (where theta is measured
       * clockwise looking out on the sky), return the ra, dec
       * coordinates of the new location.
       */
      static void raDecAndThetaRhoToRaDec(HourAngle& ra0, Declination& dec0, 
					  Angle& theta, Angle& rho, 
					  HourAngle& ra, Declination& dec);

      /**
       * Return the Az El of a source at a given HA and Dec.
       *
       * @param latitude    The latitude wrt to which the coordinates are 
       *                    desired
       *
       * @param altitude    The altitude wrt to which the coordinates are 
       *                    desired
       *
       * @param ha          The HA of the source, wrt the point of observation
       *
       * @param declination The declination of the source.
       *
       * @param geocentric  If true,  the Az, El returned will be geocentric.
       *                    If false, the Az, El returned will be topocentric.  
       *                    Note that if you request topocentric Az/El, you do not 
       *                    need to correct the returned values for horizontal parallax.
       *                    Horizontal parallax _is_ the correction between geocentric
       *                    and topocentric Az/El.
       *
       * @param type If ACTUAL, interpret r to mean an actual distance 
       *             (in AU).  If INF, ignore r and return the source 
       *             direction only.
       */
      static Vector<Angle> 
	laAndHaDecToAzEl(Angle latitude, double altitude,
			 HourAngle ha, DecAngle declination, 
			 bool geocentric=true, 
			 SrcDist type=INF, double dist=1.0);
      
      /**
       * Get the delay for the Ha and Dec of a source, given XYZ
       * coordinates of a baseline.
       *
       * Note input coordinates should be TMS XYZ, not absolute XYZ.
       *
       * Note also that the antenna positions should be earth-centered
       * if you want to apply the correction for the earth's motion.
       */
      static Delay getGeometricDelay(HourAngle ha, DecAngle declination,
				     double X, double Y, double Z,
				     double X0, double Y0, double Z0,
				     bool doMotionCorrection);
      
      /**
       * Calculate the delay, in NanoSeconds, for the specified XYZ
       * point, for a source direction given by az/el.
       *
       * Note: input coordinates should be TMS XYZ, not absolute XYZ
       */
      static Delay getGeometricDelay(Angle latitude, 
				     Angle az, Angle el,
				     double X, double Y, double Z);
      
      //------------------------------------------------------------
      // Some utility methods which it may be convenient to leave 
      // public
      //------------------------------------------------------------
      
      /**
       * First derivatives of the UVW coordinates wrt time. (X/s)
       */
      static Vector<double> getdUvw(HourAngle ha, DecAngle declination, 
				    double X, double Y, double Z);
      
      /**
       * Second derivatives of the UVW coordinates wrt time. (X/s^2)
       */
      static Vector<double> getd2Uvw(HourAngle ha, DecAngle declination, 
				     double X, double Y, double Z);
      /**
       * Allows cout << coords
       */
      friend std::ostream& operator<<(std::ostream& os, Coordinates& coords);
      
    public:
      
      /*
       * Radius of the earth (meters)
       */
      static const double earthEqRadiusMeters_;
      
      /**
       * Flattening of the earth
       */
      static const double earthFlattening_;
      
      /*o
       * Speed of light (meters/sec)
       */
      static const double lightSpeed_;
      
      /**
       * Conversion between AU and meters
       */
      static const double auToMeters_;
      
      /**
       * Rotational angular velocity of the earth, in radians per
       * second
       */
      static const double earthRotVelRadPerSec_;
      
    private:
      
      /**
       * Angle representations of the double angles
       */
      Angle longitude_;
      Angle latitude_;
      double altitude_;
      
      /**
       * Return the longitude rotation matrix
       */
      static Matrix<double> getUenToXyzLonRot(Angle longitude);
      
      /**
       * Return the latitude rotation matrix
       */
      static Matrix<double> getUenToXyzLatRot(Angle latitude);
      
      /**
       * Return the declination rotation matrix
       */
      static Matrix<double> getXyzToUvwDecRot(DecAngle declination);
      
      /**
       * Return the hour angle rotation matrix
       */
      static Matrix<double> getXyzToUvwHaRot(HourAngle ha);
      
      /**
       * Return the first derivative of the angle rotation matrix wrt
       * time
       */
      static Matrix<double> getdXyzToUvwHaRot(HourAngle ha);
      
      /**
       * Return the second derivative of the angle rotation matrix wrt
       * time
       */
      static Matrix<double> getd2XyzToUvwHaRot(HourAngle ha);
      
    }; // End class Coordinates
    
  } // End namespace util
} // End namespace sza


#endif // End #ifndef SZA_UTIL_COORDINATES_H
