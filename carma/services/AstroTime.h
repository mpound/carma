// $Id: AstroTime.h,v 1.21 2012/03/02 15:50:31 mpound Exp $

/**
 * @file 
 * Astronomical Time routines. 
 *
 * @author Marc Pound
 * @author Chul Gwon
 * @version $Revision: 1.21 $
 */

#ifndef CARMA_SERVICES_ASTROTIME_H
#define CARMA_SERVICES_ASTROTIME_H

#include "carma/util/Time.h"
#include "carma/services/IERSTable.h"
#include "carma/services/Location.h"
#include <memory>

namespace carma  {

  namespace services {

    class Angle;
    class HourAngle;
    class Length;

    /**
     * This class implements various astronomical time
     * computation routines.  
     */
	class AstroTime {
	public:
	  /** 
	   * No-arg Constructor 
	   */
	   AstroTime();

	  /**
	   * Constructor specifying the location for calculating the 
	   * time functions
	   * @param location The longitude, latitude, and altitude of the
	   * site represented by a Location object.
	   */
	   AstroTime(Location location);

	  /** Destructor */
	  virtual ~AstroTime();

	  /**
	   * Set the location for calculating the time functions
	   * using Conformable Quantities.
	   *
	   * @param longitude  Angle representing the site longitude
	   * @param latitude   Angle representing the site latitude
	   * @param altitude   Length representing the site altitude
	   */
	  void setSite(Angle longitude, Angle latitude, Length altitude);
	   //__attribute__((deprecated));


	  /**
	   * Set the location for calculating the time functions
	   * using native types.
	   * @param longitude  the site longitude in radians
	   * @param latitude   the site latitude in radians
	   * @param altitude   the site altitude in meters
	   */
	  void setSite(double longitude, double latitude, double altitude);

	  /**
	   * Set the location for calculating the time functions
	   * @param location The longitude, latitude, and altitude of the
	   * site represented by a Location object.
	   */
	  void setSite(Location location);

	  /**
	   * @return Location containing lon, lat, alt (Angle,Angle,Length)
	   */
	  Location getSite() const;

	  /**
	   * @param mjd modified julian day
	   * @return the local apparent sidereal time in hours at the location
	   * specified by setSite and the given mjd.
	   *
	   */
	  double localSiderealTime(double mjd = carma::util::Time::MJD());

	  /**
	   * @param mjd modified julian day
	   * specified by setSite and the given mjd.
	   */
	  double meanSiderealTime(double mjd = carma::util::Time::MJD());

	  /**
	   * @param mjd modified julian day
	   * @param precision digits to the right of the decimal for seconds
	   * @returns The local sidereal time as a sexagesimal string, 
	   * HH:MM:SS[.sss]
	   */
	  std::string lstString(double mjd = carma::util::Time::MJD(),
		                int precision = 1);

	  /**
	   * @param mjd modified julian day
	   * @return the equation of the equinoxes in seconds (i.e. 
	   * the difference between apparent and mean sidereal times)
	   * specified by setSite and the given mjd.
	   */
	  double eqnEqx(double mjd = carma::util::Time::MJD());

	  /**
	   * @param mjd modified julian day
	   * @return the UT1 minus UTC (dut1) difference in seconds at 
	   * the given mjd
	   * Based on the IERS tables
	   */
	  double ut1Utc(double mjd = carma::util::Time::MJD());

	  /**
	   * @param mjd modified julian day
	   * @return X polar motion, in arcsecs
	   * Based on the IERS tables
	   */
	  double xPolar(double mjd = carma::util::Time::MJD());

	  /**
	   * @param mjd modified julian day
	   * @return Y polar motion, in arcsecs
	   * Based on the IERS tables
	   */
	  double yPolar(double mjd = carma::util::Time::MJD());

	
	  /**
	   * Compute the hour angle for a given RA and time, at the
	   * location set in setSite()
	   *  @param   mjd    modified julian date 
	   *  @param   RA     the right ascension, in radians
	   *  @return  The hour angle in radians, guaranteed to be
	   *  between -PI and PI
	   */
	  double hourAngle(double mjd, double RA);

	  /**
	   * Compute the hour angle for a given RA and time, at the
	   * location set in setSite()
	   *  @param   mjd    modified julian date 
	   *  @param   RA     an Angle representing the right ascension
	   *  @return  an HourAngle representing the hour angle
	   */
	  HourAngle hourAngle(double mjd, Angle RA);


      /**
       * @return the age in days of the member IERS table in days
       * @see IERSTable::age()
       */
      double iersTableAge();

	  //====================================================
	  // STATIC METHODS - don't require Location
	  //====================================================
	  /**
	   *  retrieve the number of leap seconds to be added to
	   *  UTC to get TIA
	   *  @see http://maia.usno.navy.mil/eo/leapsec.html
	   *
	   *  @param   mjd    modified julian date 
	   *  @return  number of leap seconds (currently about 32)
	   */
	  static double leap(const double mjd);

	/**
	 * Computes the time in Julian Centuries (36525 days of universal
	 * time) elapsed since 2000 January 1 12 UT. This quantity is commonly
	 * called TU and<br>
	 * TU = ( JD - 2451545.0 )/36525.0;<br>
	 * @param mjd the MJD at which to compute TU.  Defaults to
	 * the MJD at the time the method is called.
	 *
	 * @return the Julian Century for the given Modified Julian Day
	 */
	static double elapsedJulCent( double mjd = carma::util::Time::MJD() );

	/**
	 * Returns the Julian Day given the Modified Julian Day.
	 * @param mjd the MJD at which to compute the JD.  Defaults to
	 * the MJD at the time the method is called.
	 * @return the Julian Day for the given MJD
	 */
	static double julianDay( double mjd = carma::util::Time::MJD() );

	/**
	 * @param hours Inpu time in hours
	 * @Return the input hours modulo 24;
	 */
    static double modulo24(double hours);

	/**
	 * Difference between Julian Day and Modified Julian Day.<br>
	 * JD = MJD + JULIAN_DAY_ZERO
	 */
	static const double JULIAN_DAY_ZERO ;

	/**
	 * Value of one Julian Century, in days.
	 */
	static const double JULIAN_CENTURY ;

	/**
	 *  The number of seconds per solar day
	 */ 
	static const double SECONDS_PER_DAY;

	//  Yes, I realize Time.h already has this, but
	//  I want the full PER_DAY set in one place.
	//  These could in principal be replaced by 
	//  calls to Units.convert(), which knows
	//  about sidereal vs. solar, etc. Must check
	//  the accuracy of its GnuUnits though.

	/**
	 *  The number of minutes per solar day
	 */
	static const double MINUTES_PER_DAY;
	
	/**
	 *  The number of solar hours per solar day
	 */
	static const double HOURS_PER_DAY;

	/**
	 *  The number of solar hours per sidereal day
	 */
	static const double HOURS_PER_SIDEREAL_DAY;

	/**
	 *  The length of a solar day in sidereal days
	 */
	static const double SOLAR_DAY;

	/**
	 *  The length of a sidereal day in solar days
	 */
	static const double SIDEREAL_DAY;

	/**
         * Number of seconds in one hour
	 */
	static const double SECONDS_PER_HOUR;
  
	/**
         * Number of minutes in one hour
	 */
	static const double MINUTES_PER_HOUR ;

	/**
         * Number of solar hours per radian, i.e. 12/pi
	 */
	static const double HOURS_PER_RADIAN;

	private:
	Location siteLocation_;    // Location holding site information 
    // Can't use auto_ptr because copy
    // construction releases ownership of
    // the contents.  Perhaps auto_ptr_ref
    // would work in this case, but I don't
    // really understand its usage.
	//std::auto_ptr<IERSTable> iers_;  
    IERSTable iers_;

	void checkIERSandLoadIfNecessary(void);  // helper function to load new IERS table
	void loadIERS(void);       // Re-read IERS Table from disk
    double lastIERSLoadTime_;  // MJD when the IERS table was last read from disk.
	};
    }
}

#endif //CARMA_SERVICES_ASTROTIME_H
