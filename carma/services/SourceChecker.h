
/**
 * $Id: SourceChecker.h,v 1.16 2009/08/12 15:31:14 mpound Exp $
 * 
 * @file 
 * @author Marc Pound
 * Check a source.
 *
 */

#ifndef CARMA_SERVICES_SOURCECHECKER_H
#define CARMA_SERVICES_SOURCECHECKER_H

#include "carma/services/AstroTime.h"
#include "carma/services/Ephemeris.h"
#include "carma/services/Neighbor.h"
#include "carma/services/Types.h"
#include "carma/services/TelescopeStatus.h"
#include "carma/services/Units.h"
#include "carma/services/Velocity.h"

#include <vector>
#include <set>

namespace carma {
namespace services {

  class Angle;
  class Source;

  /**
   * Source checker returns info on the location in the 
   * sky of a source at a given time, e.g. rise and set times.
   * Note all calculations are with respect to the date set
   * in SourceChecker::setMJD(double)!  
   */
  class SourceChecker {
  public:
    /**
     * Default Constructor. 
     */
    SourceChecker();

    /** Destructor*/
    virtual ~SourceChecker();

    /** 
     * Set the source to check.
     *  @param source    Source object to represent  
     */
    void setSource(const Source& source);

    /**
     * Set the source from a catalog.
     * @param sourceName    source name
     * @param catalogName   catalog name, defaults to 
     * CARMA/conf/catalogs/SystemSource.cat
     */
    void setSource(const std::string& source, 
	           const std::string& catalog="");

    /**
     * Set the location for source checking.
     * @param Location object indicating latitude, longitude and
     * altitude for source checking computation.  The default
     * location if this method is never called is CARMA's reference
     * position.
     * @see carma::services::Location
     * @see carma::services::Observatory
     */
    void setLocation(const Location& location);

    /**
     * Set the elevation lower limit for source rise and set times.
     * If the limit is less than zero, it will be set to zero.
     * If the limit is greater than 180, it will be set to (180 - elevDegrees).
     * @param elevDegrees The elevation limit in degrees.
     */
    void setElevLimit(double elevDegrees);

    /**
     * Set the elevation lower limit for source rise and set times.
     * If the limit is less than zero, it will be set to zero.
     * If the limit is greater than 180, it will be set to (180 - elevDegrees).
     * @param elevation The elevation limit as an Angle
     * @see carma::services::Angle
     */
    void setElevLimit(const Angle& elevation);

    /**
     * Set the upper elevation limit for azimuth wrap calculation
     * If the limit is less than zero, it will be set to zero.
     * @param elevDegrees The elevation limit in degrees.
     */
    void setElevUpperLimit(double elevDegrees);

    /**
     * Set the elevation limit for azimuth wrap calculation
     * If the limit is less than zero, it will be set to zero.
     * @param elevation The elevation limit as an Angle
     * @see carma::services::Angle
     */
    void setElevUpperLimit(const Angle& elevation);

    // NB: Should probably put the wrap limit info
    // into a separate Telescope class.
    // then SourceChecker::setTelescope(..)
    /**
     * Set the azimuth limit on the positive wrap
     */
    void setAzPositiveWrapLimit( const Angle & azimuth );

    /**
     * Set the azimuth limit on the negative wrap
     */
    void setAzNegativeWrapLimit( const Angle & azimuth );

    /**
     * Use sexagesimal in when printing angular data.
     * @param sex True to use sexagesimal, false for decimal.
     */
    inline void useSexagesimal(bool sexa) 
    {
	sexa_ = sexa;
    }

    /**
     * Use J2000 in when printing RA/DEC coordinates.
     * @param j2000 True to use sexagesimal, false for decimal.
     */
    void useJ2000(bool j2000) 
    {
	j2000_ = j2000;
    }

    /**
     * Call to toggle whether or not the info() command
     * prints out the column headers.  If using info() in loops
     * you generally want the header off after the first time.
     *
     * @param showit  true to show the header.
     */
    void showHeader(bool showit) ;

    /**
     * Get the current elevation limit for source rise and set times.
     * @return the elevation limit as an Angle
     * @see carma::services::Angle
     */
    Angle getElevLimit() const;

    /**
     * @return the upper elevation limit for azimuth wrap calculation
     */
    Angle getElevUpperLimit() const;

    /** @return Azimuth limit on positive wrap */
    Angle  getAzPositiveWrapLimit() const;

    /** @return Azimuth limit on negative wrap */
    Angle  getAzNegativeWrapLimit() const;

    /**
     * Set the frequency for the observation. This is used to
     * calculate the refraction correction.  The default frequency
     * is 100 GHz, which is good enough for most uses.  You only need call 
     * this method if you want to use optical refraction.
     *
     * @param frequency The frequency in Hertz
     */
    void setFrequency(double frequency);

    /**
     * Set the MJD for subsequent queries.  If this method has not
     * been called queries will default to the current time.
     * @param mjd The time of interest, expressed as Modified Julian Day
     * @see carma::util::Time::MJD()
     */
    void setMJD( double mjd = carma::util::Time::MJD() );

    /**
     * @return info about source location in sky at preset time given time
     */
    std::string info();

    /**
     * @return true if the source is above the current elevation
     * limit at the time given in setMJD, false otherwise 
     */
    bool isUp();

    /**
     * @return LST in hours at which source is above elevation limit
     * or a number < 0 if source never rises above limit.
     * If the source is always above the limit, a number greater than 24 (hr)
     * is returned.
     */
    double riseTime() ;

    /**
     * @return the number of minutes this source has been above the
     * elevation limit; zero if the source has set; 1440 if the source
     * never sets
     */
    double minutesSinceRise() ;

    /**
     * @return the number of minutes this source has been below the
     * elevation limit; zero if the source has risen; 1440 if the source
     * never rises
     */
    double minutesSinceSet() ;

    /**
     * @return the time until the source rises above the elevation limit,
     * in minutes, zero if the source is already risen
     * If the source never rises then 1440 = 1 day is returned.
     */
    double minutesUntilRise();

    /**
     * @return The number of minutes until the source transits.
     * Positive value indicates the source has not yet transited.
     * Negative value indicates the source has already transited
     * and abs(value) is the minutes since transit.
     */
    double minutesUntilTransit();

    /**
     * @return number of minutes until the source hits
     * the azimuth wrap limit
     *
     * NOTE: *For BIMA antennas*, using only this method for determining if 
     * a source can be tracked for a given period of time is insufficient 
     * because those antennas can wrap past az=360.  For such a limit,
     * this method essentially treats the limit as (limit-360)
     * because trigonometric functions can't tell the difference.
     * Specifically, tan(theta) == tan(theta+PI).
     * So for antennas with azPositiveWrapLimit > 360, the test must be
     * ( minutesUntilAzLimit() <  timeLimit && posLimit - currentAz < 180 . )
     */
    double minutesUntilAzLimit();

    /**
     * @return LST in hours at which source sets below elevation limit
     * or a number < 0 if source never rises above limit.
     * If the source is always above the limit, a number greater than 24 (hr)
     * is returned.
     * @todo rename this settingTime?.
     */
    double setTime() ;

    /**
     * @return the time until the source sets below the elevation limit,
     * in minutes, zero if the source is already set.
     */
    double minutesUntilSet();

    /**
     * @return lst in hours currently set in this object
     */
    double lst() ;

    /**
     * @return the local time set in this object as string
     */
    std::string localTime();

    /**
     * @return the Universal Time set in this object as string
     */
    std::string ut();

    /**
     * @return the azimuth of the source at the time set by setMJD().
     */
    carma::services::Angle getAzimuth();

    /**
     * @return the elevation of the source at the time set by setMJD().
     * regardless of elevation limit
     */
    carma::services::Angle getElevation();

    /** 
     * @return the status of the telescope with respect to limits
     * and blind spot for the give source at the given time.
     */
    TelescopeStatus getTelescopeStatus();

    /**
     * @return the time in minutes until the source
     * enters/exits the zenith blind spot.
     *
     * If (getTelescopeStatus().isInZenithBlindSpot() == false )
     * then the return value is the time until enter.
     * If (getTelescopeStatus().isInZenithBlindSpot() == true)
     * then the return value is the time until exit,
     *
     * Calling this method only makes sense 
     * if ( getTelescopeStatus().canEnterZenithBlindSpot() == true ).
     * if that value is false, this method returns zero.
     * @see TelescopeStatus
     */
    double minutesUntilBlind();

    /**
     * @return the time in minutes until a telescope tracking
     * the current source will encounter the positive azimuth
     * wrap limit.  (Under normal sidereal tracking it would never
     * encounter the negative wrap limit).
     * NB: comets might go the other way, but for such sources
     * the utility of this class is in question.
     * If the telescope will never hit the az limit, then the time
     * to the elevation (horizon) limit is returned.
     * @ see minutesUntilSet()
     * If the source never rises or is always above the horizon, then 
     * the value from minutesUntilRise() is returned (1440 or 0, respectively).
     * If the source is already past the wrap limit, then a negative
     * number is returned.
     */
    double minutesUntilAzWrapLimit();

    /** 
     * If the source azimuth can be represented by a value
     * between negative wrap limit and zero, then it
     * can be tracked on the negative wrap.
     */
    bool canBeTrackedOnNegativeWrap();

    /**
     * @return the Wrap value that will optimize time on source
     * without having to change antenna wrap.
     *
     * @param antType      - antenna type, OVRO or BIMA (they have 
     *                      different azimuth and elevation wrap limits)
     * @param antAzDegrees - antenna current azimuth, degrees
     * @param timeToTrack  - required minimum time to track without
     *                       a wrap chage.
     */
    services::AzWrapType computeOptimumWrapValue( 
	    services::AntennaType antType,
	    double antAzDegrees,
	    double timeToTrack );


    /**
     * Must be called after setSource, set elev limits, setFrequency, 
     * and setMJD
     * @param sourceList - must be all UPPER case
     * @throws util::IllegalStateException if setSource not called first.
     */
    services::NeighborSet 
	getNearest( const ::std::set< ::std::string > & sourceList, 
		    bool include,
                    unsigned short numReturn, bool ignoreNorthSouth,
                    coordSysType coordSys, 
                    sourcePntType pType, 
                    float fluxLimit );

    /**
     * Make a Neighbor object using the current object in this
     * SourceChecker instance as the reference and the input object 
     * to the method.
     *
     * @param neighborSource The neighboring object
     *
     * @param coordSys This parameter tells the method how to do
     * the distance computation.  COORDSYS_RADEC - spherical trigonometric
     * distance, COORDSYS_AZEL - slew-based distance, e.g. if the spherical
     * trig distance went 'over the top' in elevation, the slewing
     * distance would actually be longer since the antennas can't
     * go over the top.
     *
     * @param localEphemeris An ephemeris created solely for Neighbor creation.
     * This is to speed up creation of many Neighbors in loops.
     * <b>Do NOT use SourceChecker's internal Ephemeris ephem_ here!!</b>
     */
    services::Neighbor createNeighbor( const Source & neighborSource,
			               const coordSysType coordSys, Ephemeris & localEphemeris);

    /** DEBUG */
    double getMJD();

    /**
     * @return true if a slew between the reference source and the
     * neighbor source would be the long way around.
     */
    bool slewWouldCrossNorthSouthBoundary( const Source & neighborSource );


  private:
    // lower elevation limit
    Angle*       elevLimit_;
    // upper elevation limit
    Angle*       elevUpperLimit_;
    // positive wrap azimuth limit
    Angle azPositiveWrapLimit_;
    // negative wrap azimuth limit
    Angle azNegativeWrapLimit_;
    // local sidereal time in HOURS when source hits positive azimuth limit;
    double       azPositiveWrapLimitLst_;
    AstroTime    at_;
    Ephemeris    ephem_;
    Units        units_;

    // Has the MJD for queries been set yet?
    bool  needsMjd_;

    // For source info output, do we need the header or not?
    // If looping over many sources, we don't want to spit
    // out the header each time.
    bool needsHeader_;

    // output angles in sexagesimal format if sexa_ =- true
    bool sexa_;

    // output RA/DEC in J2000 or not.
    bool j2000_;

    TelescopeStatus telescopeStatus_;

    /**
     * @return the time source rise sets as string
     */
    std::string rise();

    /**
     * @return the time source sets as string
     */
    std::string set();

    /**
     * @return the doppler velocity for the source 
     */
    Velocity doppler();

    /**
     * @return the header for inf();
     * @param source The Source object.
     */
    std::string header( Source source );

    /** 
     * @return the hour angle at the given elevation limit.
     * @return NEVER_RISES if the source never rises
     * @return NEVER_SETS if the source never sets
     */
    double hourAngleAtElevLimit();

    /** 
     * @return the hour angle at the elevation upper limit.
     * @return NEVER_RISES if the source never rises
     * @return NEVER_SETS if the source never sets
     */
    double hourAngleAtElevUpperLimit();

    /** 
     * @return the hour angle at input elevation 
     * @return NEVER_RISES if the source never rises
     * @return NEVER_SETS if the source never sets
     * @param elev Angle representation of elevation
     */
    double hourAngleAtElevation( Angle * elev );

    /**
     * calcuated telescope status based on elevation and azimuth
     * wrap limits and source dec, mjd.
     */
    void computeTelescopeStatus();

    // marker for source that never sets below elevation limit
    static const double NEVER_SETS;
    // marker for source that never rises above elevation limit
    static const double NEVER_RISES;

    // oft-used strings
    static const std::string NEVER_RISES_STR;
    static const std::string NEVER_SETS_STR;
    static const std::string UNSUPPORTED_COORDSYS_STR;

    // widths for info() formatting
    static const int SOURCE_WIDTH = 10;
    static const int ANGLE_WIDTH  = 13;
    static const int TEXT_WIDTH   = 10;
    static const int TIME_WIDTH   = 12;
    static const int DOPPLER_WIDTH=  8;


  }; 

} // end services
} // end carma

#endif // CARMA_SERVICES_SOURCECHECKER_H
