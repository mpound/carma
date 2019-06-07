#ifndef SZA_UTIL_LOCATION_H
#define SZA_UTIL_LOCATION_H

/**
 * @file Location.h
 * 
 * Tagged: Thu Aug  5 05:25:40 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Angle.h"
#include "carma/szautil/Astrometry.h"
#include "carma/szautil/Atmosphere.h"
#include "carma/szautil/DecAngle.h"
#include "carma/szautil/Delay.h"
#include "carma/szautil/HourAngle.h"
#include "carma/szautil/Vector.h"

namespace sza {
  namespace util {
    
    class DelayLocation;
    class DelayAntennaLocation;
    class Source;
    
    /**
     *  A class for specifying a location
     */
    class Location {
    public:
      
      /**
       * Parameters required to specify a location
       */
      enum ReqParam {
	NONE     = 0x0,
	SITE     = 0x2,
	LOCATION = 0x4,
	ALL      = SITE | LOCATION
      };
      
      /**
       * Constructor.
       */
      Location();
      Location(Location& location);
      Location(const Location& location);

      void operator=(Location& loc)
	{
	}

      void operator=(const Location& loc)
	{
	}

      /**
       * Destructor.
       */
      virtual ~Location();
      
      /**
       * A method to set a fiducial LLA point
       */
      void setFiducialSite(Angle longitude, Angle latitude, double altitude);
      
      /**
       * A method to set an UEN offset relative to the fiducial
       */
      void setOffset(double up, double east, double north);
      
      /**
       * Return true if a site and offset have ben set for this
       * location
       */
      bool canLocate();
      
      /**
       * Return true if ephemeris parameters can be interpolated for
       * this timestamp
       */
      bool canBracket(double mjdUtc);
      
      /**
       * Return true if parameters for this location have changed since
       * the last time this function was called.
       */
      bool changed();
      
      /**
       * Return the LLA coordinates (long in rad, lat in rad, altitude
       * in meters) of this object.
       */
      inline Angle longitude(bool fiducial) {
	return fiducial ? fiducialLongitude_ : actualLongitude_;
      }
      
      inline Angle latitude(bool fiducial) {
	return fiducial ? fiducialLatitude_ : actualLatitude_;
      }
      
      inline double altitude(bool fiducial) {
	return fiducial ? fiducialAltitude_ : actualAltitude_;
      }
      
      /**
       * Return the XYZ coordinates (meters) of this object.  
       *
       * @param earthCentered If true, return positions relative to
       *                      the center of the earth
       */
      inline Vector<double> getXyz(bool geocentric=true) {
	if(geocentric)
	  return geocentricXyz_;
	else
	  return topocentricXyz_;
      }
      
      inline virtual double X(bool ec=true) {
	return ec ? geocentricXyz_[0] : topocentricXyz_[0];
      }
      
      inline virtual double Y(bool ec=true) {
	return ec ? geocentricXyz_[1] : topocentricXyz_[1];
      }
      
      inline virtual double Z(bool ec=true) {
	return ec ? geocentricXyz_[2] : topocentricXyz_[2];
      }
      
      /**
       * Return the UEN coordinates (meters) of this object.  
       */
      inline Vector<double> getUen() {
	return uen_;
      }
      
      inline double up() {
	return uen_[0];
      }
      
      inline double east() {
	return uen_[1];
      }
      
      inline double north() {
	return uen_[2];
      }
      
      /**
       * Get geometric delay for an Ha Dec source position, in
       * nanoseconds
       */
      virtual Delay geometricDelay(DelayAntennaLocation* refDLoc,
				   bool doMotionCorrection);
      /**
       * Get geometric delay for an Az El source position, in
       * nanoseconds
       */
      virtual Delay geometricDelay(Angle az, Angle el, DelayAntennaLocation* refDLoc);
      
      /**
       * Get the delay due to the excess path length through the
       * troposphere
       */
      Delay troposphericDelay(DelayAntennaLocation* refDLoc);

      /**
       * Register to be called back when this object's location is updated
       */
      void registerLocationCallback(DelayLocation* delayLocation);
      
      /**
       * Convert mjd to lst for the location of this antenna
       */
      HourAngle getLst(double mjd);
      
      /**
       * Convert to Ha 1for the actual location of this antenna
       */
      HourAngle getHa(double mjdUtc, HourAngle ra);
      HourAngle getHa(double mjdUtc, Source* src);
      
      /**
       * Return a handle to the ephemeris handler
       */
      inline Astrometry& ephem() {
	return astrom_;
      }

      /**
       * Return a handle to the atmospheric handler
       */
      inline Atmosphere& atmos() {
	return atmos_;
      }
 
      Angle& azimuth() {
	return azimuth_;
      }

      Angle& elevation() {
	return elevation_;
      }

    protected:
      
      HourAngle ha_;
      DecAngle dec_;
      Angle azimuth_;
      Angle elevation_;

      // A bitmask of lacking parameters required to determine a
      // location
      
      unsigned lacking_;
      
      // True if parameters for this object have changed 
      
      bool changed_;
      
      // The fiducial point
      
      Angle fiducialLongitude_;
      Angle fiducialLatitude_;
      double fiducialAltitude_;
      
      //------------------------------------------------------------
      // Different representations of this location
      
      // Absolute (L, L, A) of this location
      
      Angle actualLongitude_;    
      Angle actualLatitude_;
      double actualAltitude_;
      
      // (U, E, N) of this location, relative to the fiducial
      
      Vector<double> uen_; 
      
      // Geocentric (X, Y, Z) relative to the fiducial
      
      Vector<double> geocentricXyz_; 
      
      // Topocentric (X, Y, Z) relative to the fiducial
      
      Vector<double> topocentricXyz_;
      
      /**
       * An object for handling astrometric conversions
       */
      Astrometry astrom_;
      
      /**
       * An object for handling atmospheric calculations
       */
      Atmosphere atmos_;

      /**
       * Update coordinate representations of this location
       */
      void updateCoordinates();
      
      // When coordinates are updated, this class can call a
      // DelayLocation object who has registered to be notified.
      
      friend class DelayLocation;
      
      DelayLocation* delayLocation_;
      
    }; // End class Location
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_LOCATION_H
