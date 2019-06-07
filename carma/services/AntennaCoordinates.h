// $Id: AntennaCoordinates.h,v 1.17 2006/01/20 16:03:22 mpound Exp $
#ifndef CARMA_SERVICES_ANTENNA_COORDINATES_H
#define CARMA_SERVICES_ANTENNA_COORDINATES_H

/**
 * @file AntennaCoordinates.h
 * 
 * Tagged: Tue May  4 15:49:11 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/services/Angle.h"
#include "carma/services/Location.h"
#include "carma/services/Vector.h"

namespace carma {
  namespace services {

    class Delay;
    class Distance;
    class HourAngle;
    
    /**
     * This class performs conversions between several different antenna
     * coordinate systems.  These are described below.
     *
     * (L, L, A): The longitude/latitude/altitude coordinate system.  I
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
     * a translation and a latitude rotation. The XYZ
     * coordinate system can have as its origin the center of the
     * Earth (geocentric) or be fixed to the surface at some fiducial
     * point (topocentric).  The two interpretations are simply related by a
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
     * an absolute geocentric coordinate system.  This coordinate system
     * is typically used for VLBI. In GPS lingo, this
     * is known as Earth Center Earth Fixed (ECEF) coordinates.
     * The method getAbsXYZ() takes a
     * longitude, latitude, altitude, and an east, north, up offset
     * relative to that long/lat/alt point, and converts it to an
     * absolute (XA, YA, ZA) vector, in meters.  By comparison with
     * XYZ coordinate system, to convert from a lat/long/alt
     * point to (XA, YA, ZA), one has to perform a translation,
     * followed by both a longitude rotation and a latitude rotation.
     *
     * (E, N, U): This coordinate system is useful for specifying
     * locations relative to a given LLA point.  Coordinates are in a
     * tangent plane at the specified LLA point, with N pointing due
     * north, E pointing due east, and U pointing straight up along a
     * radial vector from the center of the earth, and passing through
     * the given LLA point.  To convert from UEN to XYZ
     * for example, one has to perform a translation by the Earth's
     * radius, followed by a longitude rotation and a latitude
     * rotation.
     *
     * This container will natively store coordinates in LLA, hence
     * the constructors which initialize these parameters.
     */
    class AntennaCoordinates : public Vector<double> {
    public:
      
      /**
       * Default constructor, uses CARMA reference position 
       * @see carma/conf/Observatory.cat
       */
      AntennaCoordinates();
      
      /**
       * Constructor with initialization of LLA
       *
       * @param longitude carma::services::Angle representing the longitude
       * @param latitude  carma::services::Angle representing the latitude 
       * @param altitude  carma::services::Length representing the Altitude
       */
      AntennaCoordinates(Angle longitude, Angle latitude, Length altitude);

      /**
       * Constructor with initialization of LLA via Location object.
       * @param location The specified Location
       * @see carma::services::Location
       */
      AntennaCoordinates(const carma::services::Location& location);

      //! @brief Copy constructor
      //! @param rhs the AntennaCordinates object to copy
      AntennaCoordinates( const AntennaCoordinates & rhs );

      /**
       * Destructor.
       */
      virtual ~AntennaCoordinates();
      

      //! @brief Assignment operator to go along with the copy constructor.
      //! @param rhs the AntennaCordinates object to copy
      AntennaCoordinates & operator=( const AntennaCoordinates & rhs );

      /**
       * Set all coordinate parameters.
       * @param longitude carma::services::Angle representing the longitude
       * @param latitude  carma::services::Angle representing the latitude 
       * @param altitude  carma::services::Length representing the Altitude
       */
      void setLla(Angle longitude, Angle latitude, Length altitude);
      
      /**
       * Set just the longitude
       * @param longitude carma::services::Angle representing the longitude
       */
      void setLongitude(Angle longitude);
      
      /**
       * Set just the latitude
       * @param latitude  carma::services::Angle representing the latitude 
       */
      void setLatitude(Angle latitude);
      
      /**
       * Set just the altitude
       * @param altitude  carma::services::Length representing the Altitude
       */
      void setAltitude(Length altitude);
      
      /**
       * Get the longitude of these coordinates.
       * @return carma::services::Angle representing the longitude
       */
      inline const Angle& longitude() const {
	return longitude_;
      }
      
      /*
       * Get the latitude of these coordinates.
       * @return carma::services::Angle representing the latitude 
       */
      inline const Angle& latitude() const {
	return latitude_;
      }
      
      /**
       * Get the altitude of these coordinates.
       * @return carma::services::Length representing the Altitude
       */
      inline const Length& altitude() const {
	return altitude_;
      }
      
      /**
       * Return absolute XYZ coordinates of the given UEN point,
       * relative to the LLA point of this object.
       * @param up    The Up coordinate in meters
       * @param east  The East coordinate in meters
       * @param north The North coordinate in meters
       * @return a carma::services::Vector containing the absolute XYZ
       * coordinates in meters.  Vector[0] = X, Vector[1] = Y, Vector[2] = Z
       *
       */
      Vector<double> getAbsXyz(double up    = 0.0,
	                       double east  = 0.0, 
			       double north = 0.0);
      
      /**
       * Return topocentric XYZ coordinates of the given UEN point, relative to
       * the LLA point of this object.
       * @param up    The Up coordinate in meters
       * @param east  The East coordinate in meters
       * @param north The North coordinate in meters
       * @return a carma::services::Vector containing the topocentric XYZ
       * coordinates in meters.  Vector[0] = X, Vector[1] = Y, Vector[2] = Z
       */
      Vector<double> getXyz(double up    = 0.0, 
	                    double east  = 0.0, 
			    double north = 0.0,
			    bool geocentric = true);
      
      /**
       * Return UEN coordinates of the specified LLA point, relative
       * to the coordinates stored in this object,
       * @param longitude An Angle representing the longitude for 
       *        which UEN should be computed.
       * @param latitude An Angle representing the longitude for 
       *        which UEN should be computed.
       * @param altitude A Length representing the longitude for 
       *        which UEN should be computed.
       * @return a carma::services::Vector containing the UEN
       * coordinates in meters.  Vector[0] = U, Vector[1] = E, Vector[2] = N
       */
      Vector<double> getUen(Angle longitude, Angle latitude, Length altitude);

      /**
       * Return UEN coordinates of the specified LLA point, relative
       * to the coordinates stored in this object,
       * @param coords An AntennaCoordinates object containing the LLA for
       *        which UEN should be computed.
       * @return a carma::services::Vector containing the UEN
       * coordinates in meters.  Vector[0] = U, Vector[1] = E, Vector[2] = N
       */
      Vector<double> getUen(AntennaCoordinates& coords);
      
      /**
       * Return the UVW coordinates for the current LLA location, of a
       * source at the requested declination and hour angle.  Note
       * that the coordinates in this container refer to an absolute
       * position, or effectively, to a baseline stretching from the
       * origin of the XYZ coordinate system to the location stored in
       * this container, which is the center of the earth.
       *
       * @param ha An HourAngle representing the hour angle of the source
       * @param declination An Angle representing the declination of the source
       * @return a carma::services::Vector containing the UVW
       * coordinates in meters.  Vector[0] = U, Vector[1] = V, Vector[2] = W
       */
      Vector<double> getUvw(HourAngle ha, Angle declination);
      
      /**
       * Return LLA of the passed UEN offset point, relative to the
       * LLA coordinates stored in this object.
       * @param up    The Up coordinate in meters
       * @param east  The East coordinate in meters
       * @param north The North coordinate in meters
       * @return a carma::services::Vector containing the longitude, latitude
       * in radians and the altitude in meters.  
       * Vector[0] = longitude, Vector[1] = latitude , Vector[2] = altitude
       */
      Vector<double> getLla(double up    = 0.0,
	                    double east  = 0.0, 
			    double north = 0.0);
      
      /**
       * Given a LLA coordinate, return its Az, El relative to the
       * position of this object.
       * @param longitude carma::services::Angle representing the longitude
       * @param latitude  carma::services::Angle representing the latitude 
       * @param altitude  carma::services::Length representing the Altitude
       * @return a carma::services::Vector of carma::services::Angle 
       * representing the azimuth and elevation, Vector[0] = AZ, Vector[1]=EL.
       */
      Vector<Angle> getAzEl(Angle longitude, Angle latitude, Length altitude);

      /**
       * Given an AntennaCoordinate, return its Az, El relative to the
       * position of this object.
       * @param coords the AntennaCordinates object in question
       * @return a carma::services::Vector of carma::services::Angle 
       * representing the azimuth and elevation, Vector[0] = AZ, Vector[1]=EL.
       */
      Vector<Angle> getAzEl(AntennaCoordinates& coords);
      
      /**
       * Increment the LLA of this object by the specified UEN offset
       * @param up    The Up coordinate in meters
       * @param east  The East coordinate in meters
       * @param north The North coordinate in meters
       */
      void add(double up, double east, double north);
      
      //------------------------------------------------------------
      // Static member functions
      //------------------------------------------------------------
      
      /**
       * Return the LLA of the point given by an UEN offset relative
       * to a fiducial LLA.
       *
       * @param longitude The fiducial longitude
       * @param latitude  The fiducial latitude
       * @param altitude  The fiducial altitude
       * @param up    The Up coordinate in meters
       * @param east  The East coordinate in meters
       * @param north The North coordinate in meters
       * @return a carma::services::Vector containing the longitude, latitude
       * in radians and the altitude in meters.  
       * Vector[0] = longitude, Vector[1] = latitude , Vector[2] = altitude
       */
      static Vector<double> llaAndUenToLla(Angle longitude,
					   Angle latitude, 
					   Length altitude, 
					   double up, double east, double north);
      
      /**
       * Return the UEN of an LLA point relative to a fiducial LLA point.
       * @param longitude The fiducial longitude
       * @param latitude  The fiducial latitude
       * @param altitude  The fiducial altitude
       * @param longitude carma::services::Angle representing the longitude
       * @param latitude  carma::services::Angle representing the latitude 
       * @param altitude  carma::services::Length representing the altitude
       * @return a carma::services::Vector containing the UEN
       * coordinates in meters.  Vector[0] = U, Vector[1] = E, Vector[2] = N
       */
      static Vector<double> llaAndLlaToUen(Angle fiducialLongitude, 
					   Angle fiducialLatitude, 
					   Length fiducialAltitude, 
					   Angle longitude, Angle latitude, 
					   Length altitude);
      
      /**
       * Return absolute XYZ of the point offset by UEN from a given
       * LLA point.
       * @param longitude carma::services::Angle representing the longitude
       * @param latitude  carma::services::Angle representing the latitude 
       * @param altitude  carma::services::Length representing the altitude
       * @param up    The Up coordinate in meters
       * @param east  The East coordinate in meters
       * @param north The North coordinate in meters
       * @return a carma::services::Vector containing the XYZ
       * coordinates in meters.  Vector[0] = X, Vector[1] = Y, Vector[2] = Z
       */
      static Vector<double> llaAndUenToAbsXyz(Angle longitude, 
	                                      Angle latitude,
					      Length altitude,
					      double up, 
					      double east, 
					      double north);
      
      /**
       * Given a topocentric XYZ, return the UEN coordinates at a
       * given latitude
       * @param X the topocentric X value in meters
       * @param Y the topocentric Y value in meters
       * @param Z the topocentric Z value in meters
       * @param latitude  carma::services::Angle representing the latitude 
       * @return a carma::services::Vector containing the UEN
       * coordinates in meters.  Vector[0] = U, Vector[1] = E, Vector[2] = N
       */
      static Vector<double> topoXYZToUen(double X, double Y, double Z, 
					 Angle latitude);
      
      /**
       * Given absolute XYZ, convert to LLA.
       * @param XA the absolute X value in meters
       * @param YA the absolute Y value in meters
       * @param ZA the absolute Z value in meters
       * @return a carma::services::Vector containing the longitude, latitude
       * in radians and the altitude in meters.  
       * Vector[0] = longitude, Vector[1] = latitude , Vector[2] = altitude
       */
      static Vector<double> absXyzToLla(double XA, double YA, double ZA);
      
      /**
       * Return the LLA of the point given by the specified absolute
       * XYZ and UEN offset.
       * @param XA the absolute X value in meters
       * @param YA the absolute Y value in meters
       * @param ZA the absolute Z value in meters
       * @param up    The Up coordinate in meters
       * @param east  The East coordinate in meters
       * @param north The North coordinate in meters
       * @return a carma::services::Vector containing the longitude, latitude
       * in radians and the altitude in meters.  
       * Vector[0] = longitude, Vector[1] = latitude , Vector[2] = altitude
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
       *
       * @param latitude The fiducial latitude
       * @param altitude The fiducial altitude
       * @param up    The Up coordinate in meters
       * @param east  The East coordinate in meters
       * @param north The North coordinate in meters
       */
      static Vector<double> laAndUenToXyz(Angle latitude, Length altitude,
					  double up=0.0, double east=0.0, 
					  double north=0.0,
					  bool geocentric=true);
      
      /**
       * Given Azimuth and Elevation, return the UEN coordinates.
       * Note that with r==1.0, this function returns the direction
       * cosines of the position specified by az/el.
       * @param az an Angle representing the azimuth
       * @param el an Angle representing the elevation
       * @param r the direction vector length
       * @return a carma::services::Vector containing the UEN
       * coordinates in meters.  Vector[0] = U, Vector[1] = E, Vector[2] = N
       */
      static Vector<double> azElToUen(Angle az, Angle el, double r=1.0);
      
      /**
       * Given UEN, return the Azimuth and Elevation
       * @param up    The Up coordinate in meters
       * @param east  The East coordinate in meters
       * @param north The North coordinate in meters
       */
      static Vector<Angle> uenToAzEl(double U, double E, double N);
      /**
       * Given UEN, return the Azimuth and Elevation
       * @param a carma::services::Vector containing up, east, and north
       * in meters.  uen[0] = up, uen[1]=east, uen[2] = north.
       * @return a carma::services::Vector of carma::services::Angle 
       * representing the azimuth and elevation, Vector[0] = AZ, Vector[1]=EL.
       */
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
      static Vector<double> haDecAndXyzToUvw(HourAngle ha, Angle declination,
					     double X, double Y, double Z);
      /**
       * Return the XYZ coordinate of a vector at requested (Ha, Dec)
       * direction and of length r.
       *
       * @param ha An HourAngle representing the hour angle of the source
       *
       * @param declination An Angle representing the declination of the source
       *
       * @param distance   the distance to the source, represented as a 
       * carma::services::Distance
       *
       * @param geocentric If true, return geocentric (X, Y, Z).  If false,
       *                   returned coordinates will be topocentric.
       *
       */
      static Vector<double> haDecToXyz(HourAngle ha, Angle declination,
				       Distance distance,
				       bool geocentric=true
				       );
      
      /**
       * Return the Az El of a source at a given HA and Dec.
       *
       * @param latitude    The latitude wrt to which the coordinates are 
       *                    desired, represented by a carma::services::Angle
       *
       * @param altitude    The altitude wrt to which the coordinates are 
       *                    desired, represented by a carma::services::Angle
       *
       * @param ha An HourAngle representing the hour angle of the source
       *
       * @param declination An Angle representing the declination of the source
       *
       * @param distance   the distance to the source, represented as a 
       * carma::services::DIstance
       *
       * @param geocentric  If true,  the Az, El returned will be geocentric.
       *         If false, the Az, El returned will be topocentric.  
       *         Note that if you request topocentric Az/El, you do not 
       *         need to correct the returned values for horizontal parallax.
       *         Horizontal parallax _is_ the correction between geocentric
       *         and topocentric Az/El.
       *
       * @return a carma::services::Vector of carma::services::Angle 
       * representing the azimuth and elevation, Vector[0] = AZ, Vector[1]=EL.
       */
      static Vector<Angle> 
	laAndHaDecToAzEl(Angle latitude, Length altitude,
			 HourAngle ha, Angle declination, 
			 Distance distance,
			 bool geocentric=true
			 );
      
      /**
       * Get the delay for the Ha and Dec of a source, given XYZ
       * coordinates of a baseline.
       *
       * Note input coordinates should be TMS XYZ, not absolute XYZ.
       *
       * Note also that the antenna positions should be earth-centered
       * if you want to apply the correction for the earth's motion.
       */
      static Delay getGeometricDelay(HourAngle ha, Angle declination,
				     double X, double Y, double Z,
				     double X0, double Y0, double Z0,
				     bool doMotionCorrection);
      
      /**
       * Calculate the delay for the specified XYZ point, for a source 
       * direction given by az/el, with respect to the reference
       * point of this AntennaCoordinates object.
       *
       * @param the latitude of the station.
       * @param the altitude of the station
       * @param the azimuth of the source w.r.t. the station location
       * @param the elevation of the source w.r.t. the station location
       * @param the elevation of the source w.r.t. the station location
       * @param the X coordinate of the station, in meters
       * @param the Y coordinate of the station, in meters
       * @param the Z coordinate of the station, in meters
       * Note: input coordinates should be TMS XYZ, not absolute XYZ
       */
      static Delay getGeometricDelay(Angle latitude, 
                                     Length altitude, 
				     Angle az, Angle el,
				     double X, double Y, double Z);
      
      //------------------------------------------------------------
      // Some utility methods which it may be convenient to leave 
      // public
      //------------------------------------------------------------
      
      /**
       * First derivatives of the UVW coordinates wrt time. (X/s)
       */
      static Vector<double> getdUvw(HourAngle ha, Angle declination, 
				    double X, double Y, double Z);

      /**
       * Second derivatives of the UVW coordinates wrt time. (X/s^2)
       */
      static Vector<double> getd2Uvw(HourAngle ha, Angle declination, 
				     double X, double Y, double Z);
      /**
       * Allows cout << coords
       */
      friend std::ostream& operator<<(std::ostream& os, const AntennaCoordinates& coords);
      
    private:

      /**
       * Angle representations of the double angles
       */
      Angle longitude_;
      Angle latitude_;
      Length altitude_;

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
      static Matrix<double> getXyzToUvwDecRot(Angle declination);
      
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
      
    }; // End class AntennaCoordinates
    
  } // End namespace services
} // End namespace carma


#endif // End #ifndef CARMA_SERVICES_ANTENNA_COORDINATES_H
