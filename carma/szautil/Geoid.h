#ifndef SZA_UTIL_GEOID_H
#define SZA_UTIL_GEOID_H

/**
 * @file Geoid.h
 * 
 * Tagged: Wed Aug 25 02:57:01 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Constants.h"
#include "carma/szautil/Declination.h"
#include "carma/szautil/Ellipsoid.h"
#include "carma/szautil/HourAngle.h"
#include "carma/szautil/Vector.h"

namespace sza {
  namespace util {
    
    //============================================================
    // An enumeration for specifying a type of coordinate system.
    //
    // These types are as follows:
    //
    //------------------------------------------------------------
    // ECF (Earth-centered fixed): 
    //
    // An absolute coordinate system (absolute XYZ, where Z points
    // along the North pole, X passes through Greenwich meridian, and
    // Y passes through longitude +90.  (Note that East longitude is
    // positive, and West longitude is negative.)
    //
    //------------------------------------------------------------
    // ENU (East-North-Up):
    //
    // A relative coordinate system, used to specify coordinates
    // relative to a fiducial point on the Earth's surface.
    // Throughout this document, the Up axis of ENU always means a
    // true orthogonal to the surface of the Earth, ie, it is parallel
    // to the geodetic vector.  Note the distinction with ENH below.
    //
    //------------------------------------------------------------
    // ENH (East-North-Height):
    //
    // A relative coordinate system, used to specify coordinates
    // relative to a fiducial point on the Earth's surface.  The
    // Height axis of this coordinate system lies along the geocentric
    // vector passing through the Earth's surface, as distinct from
    // the Up axis of ENU above, which lies along the _geodetic_
    // vector passing through the same point.  These coordinate
    // systems are related to each other by a rotation by the
    // difference between geodetic and geocentric latitudes about the
    // East axis.
    //
    //------------------------------------------------------------
    // UTM (Universal Transverse Mercator): 
    //
    // UTM is a series of transverse secant projections, at reference
    // longitudes distributed 6 degrees around the globe, and latitude
    // zero.  The UTM projection uses a scale factor of 0.9996, unlike
    // the transverse Mercator projection, which uses scale factor 1.
    //
    //------------------------------------------------------------
    // UPS (Universal Polar Stereographic):
    //
    // UPS is a stereographic projection tangent to the earth at
    // reference points of longitude=0, latitude= +-90, with a scale
    // factor of 0.994
    //
    //------------------------------------------------------------
    // XYZ: Thompson, Moran and Swenson (Eq. 4.15, 1st ed.)
    // describe a coordinate system for specifying relative antenna
    // locations: they call it (X, Y, Z).  In their system, Z lies
    // along the North Celestial Pole, X lies in the plane passing
    // through an arbitrary longitude/latitude point (at 0 Hour
    // Angle), and Y lies at -6h Hour Angle.  Thus to convert from a
    // latitude, longitude point to (X, Y, Z), one has only to perform
    // a translation and a latitude rotation. I interpret their XYZ
    // coordinate system to have as its origin the center of the
    // Earth, but there are differing opinions about this.  Some think
    // it should be taken to be fixed to the surface at some fiducial
    // point.  The two interpretations are simply related by a
    // translation by the radius of the earth, and the difference is
    // irrelevant when dealing with baseline coordinates, since those
    // are always relative.
    //
    //------------------------------------------------------------
    // UVW : Coordinates tied to the source reference frame (e.g.,
    // TMS, section 4.2).  These coordinates are simply related to
    // (X, Y, Z) by a rotation of 90-dec about the X axis, followed
    // by a rotation by 90-H about the Z axis, where dec is the
    // source declination and H the hour angle of the source.
    //
    //============================================================

    enum GeoCoordSystem {
      COORD_UNKNOWN,
      COORD_ECF,
      COORD_ENU,
      COORD_ENH,
      COORD_UTM,
      COORD_UPS,
      COORD_XYZ,
      COORD_UVW,
      COORD_GEODETIC,
      COORD_GEOCENTRIC
    };

    //============================================================
    // Source distance enumeration
    //============================================================

    enum SrcDist {
      DIST_ACTUAL, // Actual source distance
      DIST_INF     // Source at infinity
    };

    //============================================================
    // Enumeration for specifying what set of parameters (ie,
    // Geographic Datum) is used to determine the reference ellipsoid
    //============================================================

    enum GeoDatum {

      DATUM_NONE,

      //------------------------------------------------------------
      // Spherical model, used for testing

      DATUM_SPHERE,

      //------------------------------------------------------------
      //Clarke reference ellipsoid of 1866, used by NAD27 and others

      DATUM_CLARKE66,

      DATUM_NAD27,  // North American Datum of 1927 

      //------------------------------------------------------------
      // Australian National Spheroid parameters used by
      // both of the entries below:

      DATUM_ANS,           

      DATUM_AGD66,   // Australian Geodetic Datum 1966
      DATUM_GDA84,   // Australian Geodetic Datum 1984

      //------------------------------------------------------------
      // Geodetic Reference System 1980

      DATUM_GRS80,         

      DATUM_GDA94,   // Geocentric Datum of Australia 1994
      DATUM_GDA2000, // Geocentric Datum of Australia 2000 
      DATUM_NAD83,   // North American Datum of 1983
	
      //------------------------------------------------------------
      // World Geodetic System 1984, used by GPS and others

      DATUM_WGS84,

      DATUM_GPS
    };

    //============================================================
    // Enumerate hemispheres for specifying UTM zones
    //============================================================

    enum Hemisphere {
      HEMI_NONE,
      HEMI_NORTH,
      HEMI_SOUTH
    };

    //============================================================
    // An enumeration for specifying a spherical coordinate system
    //============================================================

    enum SphericalCoordSystem {
      COORD_LL,      // Longitude & Latitude
      COORD_RADEC,   // Right Ascension & Declination
      COORD_HADEC,   // Hour-angle & Declination
      COORD_AZEL,    // Azimuth & Elevation
    };

    //============================================================
    // A class for managing a vector in a spherical coordinate system
    //============================================================

    class PolarLengthVector {
    public:
      
      // We can represent a vector as lng, lat, length

      Angle longitude_;
      Angle latitude_;

      // We can represent a vector as ra, dec, length

      HourAngle ra_;
      Declination dec_;

      // We can represent a vector as ha, dec, length

      HourAngle ha_;

      // We can represent a vector as az, el, length

      Angle az_;
      Angle el_;

      Length length_;

      SphericalCoordSystem coordSystem_;

      void setCoordSystem(SphericalCoordSystem coordSystem) {
	coordSystem_ = coordSystem;
      }

      PolarLengthVector() {
      }

      PolarLengthVector(const PolarLengthVector& sap) {
	*this = sap;
      }

      PolarLengthVector(PolarLengthVector& sap) {
	*this = sap;
      }

      void operator=(const PolarLengthVector& cs) {
	*this = (PolarLengthVector&)cs;
      };

      void operator=(PolarLengthVector& cs) {

	longitude_ = cs.longitude_;
	latitude_  = cs.latitude_;
	ra_        = cs.ra_;
	ha_        = cs.ha_;
	dec_       = cs.dec_;
	az_        = cs.az_;
	el_        = cs.el_;
	length_    = cs.length_;

	coordSystem_ = cs.coordSystem_;
      };

      friend std::ostream& operator<<(std::ostream& os, const PolarLengthVector& angles);
      friend std::ostream& operator<<(std::ostream& os, PolarLengthVector& angles);

    };

    std::ostream& operator<<(std::ostream& os, const PolarLengthVector& angles);
    std::ostream& operator<<(std::ostream& os, PolarLengthVector& angles);

    //============================================================
    // A class for managing an unambiguous coordinate triplet that can
    // nevertheless be used in matrix calculations
    //============================================================

    class LengthTriplet {
    public:

      Vector<Length> coords_;

      // Generic form in arbitrary coordinate system

      Length& x_;
      Length& y_;
      Length& z_;

      // ENU-specific form, so we can unambiguously set the right
      // element without worrying about which order the parameters
      // have to be

      Length& east_;
      Length& north_;
      Length& up_;
      Length& height_;

      // ECF and XYZ-specific forms

      Length& X_;
      Length& Y_;
      Length& Z_;

      // UTM-specific elements

      Length& easting_;
      Length& northing_;
      double k_;
      unsigned zone_;
      Hemisphere hemisphere_;

      // UVW form

      Length& u_;
      Length& v_;
      Length& w_;

      // Coordinate system specifiers

      GeoDatum datum_;
      GeoCoordSystem coordSystem_;
      Length sphericalRadius_;

      void setCoordSystem(GeoCoordSystem coordSystem) {
	coordSystem_ = coordSystem;
      }
      
      void setDatum(GeoDatum datum, Length sphericalRadius=Constants::defaultEarthRadius_) {
	datum_ = datum;
	sphericalRadius_ = sphericalRadius;
      }

      void initialize() {
	k_               = 1.0;
	zone_            = 0;
	hemisphere_      = HEMI_NONE;
	datum_           = DATUM_NONE;
	coordSystem_     = COORD_UNKNOWN;
	sphericalRadius_ = Constants::defaultEarthRadius_;
      }

      void zero() {
	coords_[0].setMeters(0.0);
	coords_[1].setMeters(0.0);
	coords_[2].setMeters(0.0);
      };

      LengthTriplet() : coords_(3), 
	x_(coords_[0]),        y_(coords_[1]),  z_(coords_[2]),
	X_(coords_[0]),        Y_(coords_[1]),  Z_(coords_[2]),
	u_(coords_[0]),        v_(coords_[1]),  w_(coords_[2]),
	east_(coords_[0]),    north_(coords_[1]), up_(coords_[2]), 
	easting_(coords_[0]), northing_(coords_[1]), height_(coords_[2])
	{
	  initialize();
	};

      LengthTriplet(const LengthTriplet& triplet)  : coords_(3), 
	x_(coords_[0]),        y_(coords_[1]),  z_(coords_[2]),
	X_(coords_[0]),        Y_(coords_[1]),  Z_(coords_[2]),
	u_(coords_[0]),        v_(coords_[1]),  w_(coords_[2]),
	east_(coords_[0]),    north_(coords_[1]), up_(coords_[2]),
	easting_(coords_[0]), northing_(coords_[1]), height_(coords_[2])
	{
	  *this = triplet;
	}

      LengthTriplet(LengthTriplet& triplet)  : coords_(3), 
	x_(coords_[0]),        y_(coords_[1]),  z_(coords_[2]),
	X_(coords_[0]),        Y_(coords_[1]),  Z_(coords_[2]),
	u_(coords_[0]),        v_(coords_[1]),  w_(coords_[2]),
	east_(coords_[0]),    north_(coords_[1]), up_(coords_[2]),
	easting_(coords_[0]), northing_(coords_[1]), height_(coords_[2])
	{
	  *this = triplet;
	}

      LengthTriplet(const Vector<Length>& vec)  : coords_(3), 
	x_(coords_[0]),        y_(coords_[1]),  z_(coords_[2]),
	X_(coords_[0]),        Y_(coords_[1]),  Z_(coords_[2]),
	u_(coords_[0]),        v_(coords_[1]),  w_(coords_[2]),
	east_(coords_[0]),    north_(coords_[1]), up_(coords_[2]),
	easting_(coords_[0]), northing_(coords_[1]), height_(coords_[2])
	{
	  *this = vec;
	}

      LengthTriplet(Vector<Length>& vec)  : coords_(3), 
	x_(coords_[0]),        y_(coords_[1]),  z_(coords_[2]),
	X_(coords_[0]),        Y_(coords_[1]),  Z_(coords_[2]),
	u_(coords_[0]),        v_(coords_[1]),  w_(coords_[2]),
	east_(coords_[0]),    north_(coords_[1]), up_(coords_[2]),
	easting_(coords_[0]), northing_(coords_[1]), height_(coords_[2])
	{
	  *this = vec;
	}

      virtual ~LengthTriplet() {};

      Length magnitude() {
	Vector<double> dVec(3);
	dVec[0] = coords_[0].meters();
	dVec[1] = coords_[1].meters();
	dVec[2] = coords_[2].meters();

	Length mag;
	mag.setMeters(dVec.magnitude());

	return mag;
      };

      void operator=(const LengthTriplet& triplet) {
	*this = (LengthTriplet&)triplet;
      };

      void operator=(LengthTriplet& triplet) {
	initialize();
	coords_[0]       = triplet.coords_[0];
	coords_[1]       = triplet.coords_[1];
	coords_[2]       = triplet.coords_[2];
	k_               = triplet.k_;
	zone_            = triplet.zone_;
	coordSystem_     = triplet.coordSystem_;
	hemisphere_      = triplet.hemisphere_;
	datum_           = triplet.datum_;
	sphericalRadius_ = triplet.sphericalRadius_;
      };

      void operator=(const Vector<Length>& vec) {
	*this = ((Vector<Length>&) vec);
      }

      void operator=(Vector<Length>& vec) {

	if(vec.size() != 3) {
	  ThrowError("Cannot assign a length " << vec.size() 
		     << " vector to a LengthTriplet (obviously, \"Triplet\" implies 3)");
	}

	initialize();
	coords_ = vec;
      };

      LengthTriplet operator*(double fac) {
	coords_[0] *= fac;
	coords_[1] *= fac;
	coords_[2] *= fac;
	return *this;
      };

      LengthTriplet operator/(double fac) {
	coords_[0] /= fac;
	coords_[1] /= fac;
	coords_[2] /= fac;
	return *this;
      };

      Vector<double> operator/(const Length& length) {
	return operator/((Length&) length);
      };

      Vector<double> operator/(Length& length) {
	return coords_ / length;
      };

      LengthTriplet operator-(const LengthTriplet& triplet) {
	return operator-((LengthTriplet&) triplet);
      }

      LengthTriplet operator-(LengthTriplet& triplet) {
	LengthTriplet ret = *this;
	ret.coords_ = coords_ - triplet.coords_;
	return ret;
      }

      friend std::ostream& operator<<(std::ostream& os, const LengthTriplet& triplet);
      friend std::ostream& operator<<(std::ostream& os, LengthTriplet& triplet);

    };

    std::ostream& operator<<(std::ostream& os, const LengthTriplet& triplet);
    std::ostream& operator<<(std::ostream& os, LengthTriplet& triplet);

    //============================================================y
    // Class for managing a commonly-needed set of coordinates:
    //
    // Longitude/Latitude/Altitude
    //============================================================

    class Lla {
    public:
      Angle  longitude_;
      Angle  latitude_;
      Length altitude_;
      
      GeoDatum datum_;
      GeoCoordSystem coordSystem_;
      Length sphericalRadius_;

      Lla() {
	datum_           = DATUM_NONE;
	coordSystem_     = COORD_UNKNOWN;
	sphericalRadius_ = Constants::defaultEarthRadius_;
      }

      Lla(const Lla& lla) {
	*this = lla;
      }

      Lla(Lla& lla) {
	*this = (Lla&)lla;
      }

      void operator=(const Lla& lla) {
	*this = (Lla&)lla;
      }

      void operator=(Lla& lla) {
	longitude_       = lla.longitude_;
	latitude_        = lla.latitude_;
	altitude_        = lla.altitude_;
	datum_           = lla.datum_;
	coordSystem_     = lla.coordSystem_;
	sphericalRadius_ = lla.sphericalRadius_;
      }
      
      void setCoordSystem(GeoCoordSystem coordSystem) {
	coordSystem_ = coordSystem;
      }
      
      void setDatum(GeoDatum datum, Length sphericalRadius=Constants::defaultEarthRadius_) {
	datum_ = datum;
	sphericalRadius_ = sphericalRadius;
      }
      
      friend std::ostream& operator<<(std::ostream& os, const Lla& lla);
      friend std::ostream& operator<<(std::ostream& os, Lla& lla);
    };

    std::ostream& operator<<(std::ostream& os, const Lla& lla);
    std::ostream& operator<<(std::ostream& os, Lla& lla);

    //============================================================
    // A class for managing real-Earth geometry calculations
    //============================================================

    class Geoid : public Ellipsoid {
    public:

      // Constructor.

      Geoid();
      Geoid(GeoDatum referenceSystem);
      
      // Destructor.

      virtual ~Geoid();
      
      //-----------------------------------------------------------------------
      // Select the Earth ellipsoid reference system (ie, in the
      // terminology of the NGS, which Datum are we using?)
      //-----------------------------------------------------------------------

      void changeDatum(GeoDatum datum, Length sphericalRadius = Constants::defaultEarthRadius_);

      void setDatum(GeoDatum datum, Length sphericalRadius = Constants::defaultEarthRadius_);
      void setDefaultDatum(GeoDatum datum, Length sphericalRadius = Constants::defaultEarthRadius_);

      void restoreDefaultDatum();

      //-----------------------------------------------------------------------
      // Return parameters of the current reference system
      //-----------------------------------------------------------------------

      Length earthEquatorialRadius();
      Length earthPolarRadius();

      //-----------------------------------------------------------------------
      // Return the length of the radius vector from the center of the
      // earth to the surface at a given Geocentric latitude, for the
      // current datum
      //-----------------------------------------------------------------------

      Length geocentricRadius(Angle geocentricLatitude);

      //-----------------------------------------------------------------------
      // Return the length of the radius vector normal to the surface
      // at a given Geodetic latitude, for the current datum
      //-----------------------------------------------------------------------

      Length geodeticRadius(Angle geodeticLatitude);

      //-----------------------------------------------------------------------
      // Return the Geocentric latitude corresponding to a given
      // geodetic latitude, on the surface of the earth, for the
      // current datum
      //-----------------------------------------------------------------------

      Angle geocentricLatitude(Angle geodeticLatitude);

      //-----------------------------------------------------------------------
      // Return the geodetic latitude corresponding to a given
      // geocentric latitude on the surrface of the earth, for the
      // curent datum
      //-----------------------------------------------------------------------

      Angle geodeticLatitude(Angle geocentricLatitude);

      //-----------------------------------------------------------------------
      // Convert from Geocentric LLA and Geocentric ENU in any
      // input datum to Geodetic coordinates in the current datum
      //-----------------------------------------------------------------------
      
      Lla geocentricLlaAndEnhToGeodeticLla(Lla& geocentricLla, LengthTriplet enh=LengthTriplet());

      //-----------------------------------------------------------------------
      // Convert from Geocentric LLA and Geocentric ENU in any
      // input datum to Geocentric coordinates in the current datum
      //-----------------------------------------------------------------------

      Lla geocentricLlaAndEnhToGeocentricLla(Lla& geocentricLla, LengthTriplet enh=LengthTriplet());

      //-----------------------------------------------------------------------
      // Convert from Geodetic LLA and Geodetic ENU in any input
      // datum to Geodetic coordinates in the current datum
      //-----------------------------------------------------------------------

      Lla geodeticLlaAndEnuToGeocentricLla(Lla& geodeticLla, LengthTriplet enu=LengthTriplet());

      //-----------------------------------------------------------------------
      // Convert from Geodetic LLA and Geodetic ENU in any input
      // datum to Geocentric coordinates in the current datum
      //-----------------------------------------------------------------------

      Lla geodeticLlaAndEnuToGeodeticLla(Lla& geodeticLla, LengthTriplet enu=LengthTriplet());

      //-----------------------------------------------------------------------
      // Convert from Geodetic LLA and Geodetic ENU in any input
      // datum to UTM Northing/Easting in the current datum
      //-----------------------------------------------------------------------

      LengthTriplet geodeticLlaAndEnuToUtm(Lla& geodeticLla, LengthTriplet enu=LengthTriplet());

      //-----------------------------------------------------------------------
      // Convert from UTM coordinate in any input datum to
      // Geodetic coordinate in the current datum
      //-----------------------------------------------------------------------

      Lla utmToGeodeticLla(LengthTriplet& utm);

      //-----------------------------------------------------------------------      
      // Convert from UTM coordinate in any input datum to UTM
      // coordinate in the current datum
      //-----------------------------------------------------------------------      

      LengthTriplet utmToUtm(LengthTriplet& utm);

      //-----------------------------------------------------------------------
      // Convert from Geodetic LLA and Geodetic ENU in any input
      // datum to UPS Northing/Easting in the current datum
      //-----------------------------------------------------------------------

      LengthTriplet geodeticLlaAndEnuToUps(Lla& geodeticLla, LengthTriplet enu=LengthTriplet());

      //-----------------------------------------------------------------------
      // Convert from UPS in any input datum to Geodetic LLA in the
      // current datum
      //-----------------------------------------------------------------------

      Lla upsToGeodeticLla(LengthTriplet& ups);

      //-----------------------------------------------------------------------
      // Convert from Geodetic LLA and UTM offset relative to that
      // point, in any input datum, to Geodetic coordinate in the
      // current datum
      //-----------------------------------------------------------------------

      Lla geodeticLlaAndDeltaUtmToGeodeticLla(Lla& geodeticLla, LengthTriplet& deltaUtm);

      //=======================================================================
      // Astronomical conversions
      //=======================================================================

      //-----------------------------------------------------------------------
      // Return the (Az, El) coordinates of the specified (HA, Dec)
      // point.
      //-----------------------------------------------------------------------

      PolarLengthVector geodeticLlaAndHaDecToAzEl(Lla& geodeticLla, 
						   HourAngle ha, Declination declination, 
						   SrcDist type=DIST_INF, Length distance=Length(Length::Meters(), 0.0));

      //-----------------------------------------------------------------------
      // Return the UVW coordinates of the specified (HA, Dec) point.
      //-----------------------------------------------------------------------

      LengthTriplet geodeticLlaAndHaDecToUvw(Lla& geodeticLla, 
					     HourAngle ha, 
					     Declination declination);

    public:

      //-----------------------------------------------------------------------
      // Specifies the reference ellipsoid we are using
      //-----------------------------------------------------------------------

      GeoDatum datum_;
      GeoDatum defaultDatum_;
      Length defaultSphericalRadius_;

      //-----------------------------------------------------------------------
      // Convert from Geodetic coordinates to ECF coordinates
      //-----------------------------------------------------------------------

      LengthTriplet geodeticLlaAndEnuToEcf(Lla& geodeticLla, LengthTriplet enu=LengthTriplet());

      //-----------------------------------------------------------------------
      // Convert from Geocentric coordinates to ECF coordinates
      //-----------------------------------------------------------------------

      LengthTriplet geocentricLlaAndEnhToEcf(Lla& geocentricLla, LengthTriplet enh=LengthTriplet());

      //-----------------------------------------------------------------------
      // Conversions between Geodetic coordinates and relative XYZ coordinates
      //-----------------------------------------------------------------------

      LengthTriplet geodeticLlaAndEnuToXyz(Lla& geodeticLla, LengthTriplet enu=LengthTriplet());
      LengthTriplet geodeticLlaAndXyzToEnu(Lla& geodeticLla, LengthTriplet xyz=LengthTriplet());

      //-----------------------------------------------------------------------
      // Convert from Geocentric coordinates to relative XYZ coordinates
      //-----------------------------------------------------------------------

      LengthTriplet geocentricLlaAndEnhToXyz(Lla& geocentricLla, LengthTriplet enh=LengthTriplet());

      //-----------------------------------------------------------------------
      // Given a reference geodetic position, and a second geodetic
      // position, return the ENU coordinates of the second position
      // w.r.t. the first
      //-----------------------------------------------------------------------

      LengthTriplet geodeticLlaAndGeodeticLlaToEnu(Lla& geodeticLlaRef, Lla& geodeticLlaObs);

      //-----------------------------------------------------------------------
      // Convert from ECF coordinates to Geocentric coordinates
      //-----------------------------------------------------------------------

      Lla ecfToGeocentricLla(LengthTriplet& ecf);

      //-----------------------------------------------------------------------
      // Convert from ECF coordinates to Geodetic coordinates
      //-----------------------------------------------------------------------

      Lla ecfToGeodeticLla(LengthTriplet& ecf);

      //=======================================================================
      // UTM utilities
      //=======================================================================

      //-----------------------------------------------------------------------
      // Convert from Geodetic coordinates to projected cylindrical coordinates
      //-----------------------------------------------------------------------

      LengthTriplet transverseSecantProjection(Lla& geodeticLla, Lla& referenceLla, double k0);

      //-----------------------------------------------------------------------
      // Convert from projected cylindrical coordinates to Geodetic coordinates 
      //-----------------------------------------------------------------------

      Lla inverseTransverseSecantProjection(LengthTriplet& xyk, Lla& referenceLla, double k0);

      //-----------------------------------------------------------------------
      // Convert from Geodetic coordinates to projected polar
      // stereographic coordinates
      //-----------------------------------------------------------------------

      LengthTriplet polarStereographicProjection(Lla& geodeticLla, Lla& referenceLla, double k0);

      //-----------------------------------------------------------------------
      // Convert from projected polar stereographic coordinates to
      // Geodetic coordinates
      //-----------------------------------------------------------------------

      Lla inversePolarStereographicProjection(LengthTriplet& xyk, double k0);

      //-----------------------------------------------------------------------
      // Series expansion of term used in above calculations
      //-----------------------------------------------------------------------

      double utmSeriesExpansionForM(Angle& lat);

      //-----------------------------------------------------------------------
      // Return the UTM zone corresponding to this longitude
      //-----------------------------------------------------------------------

      unsigned longitudeToUtmZone(Angle longitude);
      
      //-----------------------------------------------------------------------
      // Convert from UTM zone number to reference longitude
      //-----------------------------------------------------------------------

      Angle utmZoneToLongitude(unsigned zone);

      //-----------------------------------------------------------------------
      // Return the UTM reference longitude corresponding to this
      // longitude
      //-----------------------------------------------------------------------

      Angle getUtmReferenceLongitude(Angle longitude);

      //=======================================================================
      // Astronomical utilities
      //=======================================================================

      LengthTriplet haDecDistToXyz(HourAngle ha, Declination dec, Length distance);
      LengthTriplet haDecDistToXyz2(HourAngle ha, Declination dec, Length distance);
      Vector<double> haDecToXyzDir(HourAngle ha, Declination dec);

      // Given XYZ, return the Ha and Dec

      PolarLengthVector xyzToHaDec(LengthTriplet& xyz);

      // Given ENU, return the Azimuth and Elevation

      PolarLengthVector enuToAzEl(LengthTriplet& enu);

      // Given Azimuth and Elevation, return the ENU

      LengthTriplet azElToEnu(Angle az, Angle el, Length distance);

      //=======================================================================
      // Rotation matrices used by this class
      //=======================================================================
      
      // Return the rotation about the east axis to convert from
      // Geodetic ENU to Geocentric ENH

      sza::util::Matrix<double> getEnuToEnhRot(Angle latDiff);

      sza::util::Matrix<double> getEnhToXyzLatRot(Angle geocentricLatitude);
      sza::util::Matrix<double> getXyzToEcfLngRot(Angle longitude);

      sza::util::Matrix<double> getXyzToUvwHaRot(HourAngle ha);
      sza::util::Matrix<double> getXyzToUvwDecRot(Declination dec);
					     
    }; // End class Geoid

    std::ostream& operator<<(std::ostream& os, GeoDatum datum);
    std::ostream& operator<<(std::ostream& os, GeoCoordSystem coordSystem);
    GeoDatum stringToDatum(std::string);

    Vector<Length> operator*(Matrix<double>& mat, LengthTriplet& triplet);

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_GEOID_H
