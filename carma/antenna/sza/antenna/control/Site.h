#ifndef SITE_H
#define SITE_H

/**
 * @file Site.h
 * 
 * Tagged: Thu Nov 13 16:53:51 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/control/PointingCorrections.h"

// Include the C-style struct and method definitions this class is
// based on

#include "carma/szaarrayutils/astrom.h"

// Needed for cvel

#include "carma/szaarrayutils/szaconst.h"

#include "carma/szautil/Angle.h"
#include "carma/szautil/Length.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      
      /**
       * Class for managing antenna site-specific parameters.
       */
      class Site {
	
      public:
	
	/**
	 * Constructor.
	 *
	 * @throws Exception
	 */
	Site();
	
	/**
	 * Set the antenna location fiducial parameters
	 *
	 * @throws Exception
	 */
	void setFiducial(sza::util::Angle longitude, sza::util::Angle latitude, 
			 double altitude);
	
	/**
	 * Set the antenna location offset parameters
	 *
	 * @throws Exception
	 */
	void setOffset(double north, double east, double up);

	/**
	 * Reset site parameters to default values.
	 */
	void reset();
	
	/**
	 * Return the local sidereal time for a given site and UTC.
	 */
	double convertMjdUtcToLst(double utc, double ut1utc, double eqneqx);
	
	/**
	 * Correct the azimuth and elevation for horizontal parallax.
	 *
	 * @param dist double             The distance of the source (AU), 
	 *                                or 0.0 if the distance can be assumed 
	 *                                to be infinite.
	 * @param f PointingCorrections*  The az/el pointing to be corrected.
	 */
	void applyParallax(double dist, PointingCorrections* f);
	
	/**
	 * Correct the azimuth and elevation for diurnal aberration.
	 *
	 *  @param f PointingCorrections *  The az/el pointing to be corrected.
	 */
	void applyDiurnalAberration(PointingCorrections* f);
	
	/**
	 * Install the latitude in the pointing corrections container.
	 */
	void updateLatitude(PointingCorrections* f);
	
	/**
	 * Pack fiducial site-specific data for archival in the
	 * register database.
	 */
	void packFiducial(signed* s_elements);
	
	/**
	 * Pack actual site-specific data for archival in the register
	 * database.
	 */
	void packActual(signed* s_elements);

	/**
	 * Pack site-specific data for archival in the register
	 * database.
	 */
	void packOffset(signed* s_elements);

	inline double getLongitude() {
	  return actual_.longitude;
	}

	inline double getLatitude() {
	  return actual_.latitude;
	}

	inline double getAltitude() {
	  return actual_.altitude;
	}

	inline sza::util::Angle latitude() {
	  return sza::util::Angle(sza::util::Angle::Radians(), actual_.latitude);
	}

	inline sza::util::Length altitude() {
	  return sza::util::Length(sza::util::Length::Meters(), actual_.altitude);
	}

      private:
	
	/**
	 * Round an angle into the range 0-2.pi.
	 *
	 * @param angle  double   The angle to be rounded (radians).
	 */
	double wrap2pi(double angle);
	
	/**
	 * The C-struct that this class is a wrapper for.  This is the
	 * struct where the site information is actually stored.
	 */

	// The fiducial site.  This is the location of an arbitrary
	// (lat, long, alt) point, relative to which the actual
	// antenna location is specified.

	sza::array::Site fiducial_;

	// The offset location, in meters of this antenna from the
	// fiducial position

	double north_;
	double east_;
	double up_;

	// The actual (lat, long, alt) of this antenna, after the
	// above offsets have been taken into account.

	sza::array::Site actual_;

      }; // End class Site
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
