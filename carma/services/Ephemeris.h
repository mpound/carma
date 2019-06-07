// -*- c++ -*-

/**
 * @file Ephemeris.h
 * Declaration of carma::services::Ephemeris
 *   
 * Ephemeris: (Webster) A publication giving the computed places of 
 * the heavenly bodies for each day of the year, with other
 * numerical data, for the use of the astronomer and
 * navigator; an astronomical almanac; as, the ``American
 * Ephemeris and Nautical Almanac.''
 * For CARMA we extend this definition also to mean the motions
 * of these heavenly bodies, notably the doppler corrections
 * and proper motions needed for an observation to keep the object
 * in the "field of view".
 *
 *
 * @author Peter Teuben
 * @version $Id: Ephemeris.h,v 1.78 2012/02/28 21:41:18 mpound Exp $
 *
 * @todo   weather/site_info - interfaces going to change
 *   
 */

#ifndef CARMA_SERVICES_EPHEMERIS_H
#define CARMA_SERVICES_EPHEMERIS_H

// just in case (novas itself also define _NOVAS_)
// but *nobody* should ever need to know this. See also services::novasWrapper
#define HAVE_LIBNOVAS


// debug also uses some static variables, so don't use this in production mode
// where threading is used.
//#define EPHEM_DEBUG

#include "carma/services/Vector.h"
#include "carma/services/Distance.h"
#include "carma/services/Length.h"
#include "carma/services/Velocity.h"
#include "carma/services/Source.h"
#include "carma/services/AntennaCoordinates.h"
#include "carma/services/AstroTime.h"
#include "carma/services/IERSTable.h"
#include "carma/services/EphemerisTable.h"
#include "carma/services/novasWrapper.h"

#include "carma/services/Atmosphere.h"


namespace carma {
  namespace services {

      /**
       *   Ephemeris wraps the NOVAS library and any other ephemeris related 
       *   functions into a simple to use class. Time is mostly handled by
       *   AstroTime
       *   For most of CARMA work one instantiates this class with a source, viz.
       *   <br><tt>
       *           Ephemeris e;
       *   </tt>
       *   after which the time should be set (default is current time)
       *   <br><tt>
       *           e.setMJD(mjd);
       *           e.setSource('venus')
       *   </tt>
       *   and probably some weather and observing site info (they default to
       *   CARMA at 100 GHz with average weather
       *   <br><tt>
       *           e.setLocation(location);
       *           e.setWeather(pres_mB,temp_C,relhum_percent);
       *           e.setFreq(freq_Hz);
       *   </tt>
       *   and the RA/DEC or AZ/EL can be retrieved (all angles are in radians)
       *   <br><tt>
       *           double ra=e.getRa();
       *           double az=e.getAz();
       *   </tt>
       *   or an instantaneous:
       *   <br><tt>
       *           Vector<double> azel=e.getAzEl(mjd,ra,dec)
       *   </tt>
       *
       *   Caution: the Ephemeris class can initialize sources in three ways,
       *   in doing so the Amar should not expect theirs paths to cross over!
       *   1) setSource(Source &)
       *   2) setSource(string &)
       *   3) setSource(double ra, double dec, double doppler)
       *
       */
    class Ephemeris {
    public:

      /**
       * Construct a default Ephemeris.
       * The default constructor also does a few sneaky things:
       *     site is the CARMA array center (use setLocation())
       *     time is now (use setMJD())
       *     temp, pressure, humidity (use setWeathe())
       *     observing frequency for refraction computation (use setFreq())
       * Note there is no default for a Source. There are 3 ways (2 good ones,
       * one deprecated one) to set a source to observe.
       */
      Ephemeris();

      /**
       *  Constructor with a source name
       *  See also setSource() with the same calling sequence
       *  @param sourceName    sourcename, can also be a planet
       *  @param catalogName   optional filename for source catalog 
       *         (default is CARMA/conf/catalogs/SystemSource.cat)
       */
      Ephemeris(const std::string &sourceName, 
                const std::string &catalogName="");

      /** Destructor */
      virtual ~Ephemeris();

      /**
       *  constructor from a source 
       *  See also setSource() with the same calling sequence
       *  @param source   a source in full FK5 
       */
      Ephemeris(const Source &source);

      /**
       *  Constructor from a location (not implemented yet)
       *  @param location the station location
       */
      Ephemeris(const Location& location);


      /**
       * Set the site information. This will only set the location 
       * of the observatory,
       * See also setWeather, setFreq, setMJD, setSource for other functions
       * related to setting ephemeris information.
       * @param location Location the station antenna coordinates
       */
      void setLocation(const Location &location);

      /**
       * Set the observatory for the internal location, as taken from a 
       * list that is
       * defined via the Location class. See also 
       * CARMA/conf/catalogs/Observatory.cat
       *
       * @param observatory   name of the observatory
       * @param topocentric   pass on if RA/DEC should be topocentric (vs. geocentric)
       *                       
       * @see setLocation(Location)
       */
      void setLocation(const std::string &observatory, bool topocentric=true);

      /**
       * @return the Location stored herein, the vantage point from
       * which all sky coordinates are calculated.
       */
      Location getLocation(void) const;


      /** update the weather, such that refraction is more accurate.
       *  Note that the observing frequency also needs to be set
       *  for an accurate refraction correction, since there is
       *  a 10-20% difference between radio and optical refraction.
       *  There are some reasonable defaults for the weather
       *  for refraction to work, so for low precision this routine
       *  does not need to be set.
       *
       *  The input values are passed through the "safe" routines
       *  of carma::environment::Atmosphere, to ensure reasonable
       *  numbers.
       *  @see carma::services::Atmosphere
       *
       * @param atmPressure  atmostpheric pressure in millibars
       * @param airTemp      air temperature in Kelvin
       * @param relHumid     relative humidity, in percent
       */
      void setWeather(const double atmPressure, const double airTemp, 
	              const double relHumid);

      /** update the observing frequency for refraction computation.
       *  This is only needed for a more accurate refraction model
       *  If this function is used, setWeather() should also be used.
       *  Currently there is separate model for radio and optical,
       *  but is otherwise independant of frequency, but check
       *  updates to carma::services::Atmosphere::
       *
       * @param freq      observing frequency, in Hz. 
       */
      void setFreq(const double freq);

      /**
       * @return observing frequency, in Hz
       */
      double getFreq( void ) const;

      /**
       *  Turn off refraction explicitly. Any setting of observing parameters
       *  that control refraction will now be ignored. This is useful when
       *  observing very nearby sources, such as the transmitter, when there
       *  no appreciable atmosphere to look through.
       *  Also note that refraction will be 0 for negative elevation.
       */
      void setRefraction(const bool refract);

      /** set a SolarSystemBody
       *  not implemented yet
       */
      void setBody(const int type, const int number, const std::string &name);

      /** set a SolarSystemBody
       *  not implemented yet
       */
      void setBody(const std::string &name);

      /** set the time of observation (defaults to the current time)
       *  and a few ancillary variables (deltat, xpolar, ypolar) from
       *  AstroTime that are needed within the Ephemeris
       *
       * @param mjd   input modified julian date (JD-2400000.5)
       *
       */
      void setMJD(const double mjd=0);

      double getMJD() const
      {
	  return mjd_;
      }


      /** set the time correction, in seconds, added to the MJD to
       *  get dynamical time (TT or TDB). 
       *
       *       deltat = UTC-UT1 + leap_seconds + 32.184 
       *
       *  It is approximately 65 seconds
       *  in the current era. This is normally not needed and retrieved
       *  by the Ephemeris class from the IERS tables.
       *  this routine only exist(ed) to cheat and compare with xephem
       *  it should really not be exposed to the public
       *
       *  ** to be deprecated **
       *
       * @param deltat    correction in seconds
       *
       */
      void setDeltaT(const double deltat=0);

      /** set a source (the catalog entry.
       *  This is the first (of 3) methods to set a source for the ephemeris.
       *  Note a new source will reset any existing RA/DEC or AZ/EL offsets.
       *
       *  @param source    Source object to represent  
       */
      void setSource(const Source &source);

      /**
       *  set a source from a name, and an optional (user) catalog
       *  planets are also allowed.
       *  This is the second (of 3) methods to set a source for the ephemeris.
       *  Note a new source will reset any existing RA/DEC or AZ/EL offsets.
       *  This member function also understands a small set of special "fixed"
       *  locations near the CARMA array, which must appear in the "position"
       *  field of the Observatory.cat catalog. The "configs" field for these
       *  stations is called "fixed". An example is the transmitter (sourceName=trans)
       *  The search order of sources is as follows:
       *  1) planet (NOVAS uses JPL DE405 tables for these)
       *  2) ephemeris tables (SOURCE.ephem in the user or system catalogs directory)
       *  3) fixed source in the local terrain (e.g. "trans" for transmitter)
       *  4) source from a catalog (user catalog or default SystemSource.cat)
       *
       * 
       * @param sourceName    source name
       * @param catalogName   catalog name, defaults to CARMA/conf/catalogs/SystemSource.cat
       *                      as derived from the [sic] location of the executable
       *                      If an existing catalog is given, that one is tried before
       *                      the system catalog.
       */
      void setSource(const std::string& sourceName, 
	             const std::string& catalogName = "");


      /** NB: private function
       *
       */
      void setEphemerisTableSource(const double ra2000, const double dec2000, const double doppler, const double distance);

      /**
       * @return the Source stored herein.
       *
       * Note if Ephemeris was instantiated from the private method
       * setSource(RA,DEC,VEL) instead of a full Source, the returned 
       * Source is very minimal and should not be used in its full context.
       *
       */
      Source getSource();

      /**
       *  sky offset in RA and DEC applied, until a new source has been specified.
       *  A positive dra means shifted to the east, in the usual astronomical fashion.
       *  Note you can only apply offsets in RA/DEC *or* AZ/EL, not both.
       *
       *  @param dra    sky offset in RA (radians)
       *  @param ddec   sky offset in DEC (radians)
       *
       */
      void setRaDecOffsets(const double dra, const double ddec);

      /**
       *  sky offset in AZ and EL applied, until a new source has been specified.
       *  Note you can only apply offsets in AZ/EL *or* RA/DEC, not both.
       *
       *  @param daz    sky offset in AZ (radians)
       *  @param del    sky offset in EL (radians)
       *
       */      
      void setAzElOffsets(const double daz, const double del);

      // some user friendly things, all in the current epoch, topocentric

      /**
       * get the topocentric RA in the current epoch
       * @return the current RA, in  radians
       */
      double getRa(void);

      /**
       * get the topocentric DEC in the current epoch
       * @return the current DEC, in  radians
       */
      double getDec(void);

      /**
       * get the Azimuth
       * @return the Azimuth,, in  radians
       */
      double getAz(void);

      /**
       * get the Elevation, in radians. If a non-zero frequency was set,
       *  or if setRefraction(true) was set in this ephemeris, refraction has 
       *  been added into the elevation.
       *  Also note that refraction will be 0 for negative elevation.
       *  See getRefrac() to get the last used refraction value.
       *
       * @return the Elevation, in  radians 
       */
      double getEl(void);

      /**
       * get the last used refraction correction. Only use this after
       * getAz/getEl/getAzEl have been called.
       * @todo rename this to getRefract() 
       *
       * @return the Refraction Correction, in degrees!.
       * 
       */
      double getRefrac(void) const;

      /**
       * get the Doppler velocity in a specific frame of reference
       * Note that normally you want the Topocentric velocity for
       * observations, but for ephemeris usage LSR or Heliocentric
       * may be more commonly used.
       * Note for solar system objects (and those using an external 
       * ephemeris) the resulting doppler is always placed at 0, to
       * ease comparison at different epochs.
       *
       * @return Doppler velocity, in m/s
       */
      double getDoppler(velocityFrameType frameType = FRAME_TOPOGRAPHIC);

      /**
       *   Convert a given current Az,El into Ra,Dec for the current site
       *   This is a somewhat peculiar routine, since novas only has
       *   the reverse routine 'equ2hor' and there is no hor2equ'\
       *   Use with caution, as not all ephemeris state information
       *   may have been initialized properly.
       *
       *   @param mjd   input MJD (modified julian date, JD-2400000.5)
       *   @param az    current epoch AZ in radians
       *   @param al    current epoch EL in radians
       *
       *   @return a pair, as a vector, of Ra and Dec. Both in radians
       */
      Vector<double> getRaDec(double mjd, double az, double el);

      /**
       *   Convert a given current epoch topocentric RA,DEC into AzEl.
       *   This routine directly calls novas::equ2hor
       *   Use with caution, as not all ephemeris state information
       *   may have been initialized properly.
       *
       *   @todo clarify this what you mean with 'properly'
       *   @todo  this routine should also set the mjd,ra,dec internally
       *
       *   @param mjd   input MJD (modified julian date, JD-2400000.5) [UTC]
       *   @param ra    current epoch topocentric RA in radians
       *   @param dec   current epoch topocentric DEC in radians
       *
       *   @return a pair, as a vector, of Az and El. Both in radians
       */
      Vector<double> getAzEl(double mjd, double ra, double dec);


      /**
       *   Convert a given current epoch topocentric RA,DEC into AzEl.
       *   This routine directly calls novas::equ2hor
       *   Use with caution, as not all ephemeris state information
       *   may have been initialized properly.
       *
       *   @todo  this routine should also set the mjd,ra,dec internally
       * 
       *   @param mjd   input MJD (modified julian date, JD-2400000.5) [UTC]
       *   @param ra    current epoch topocentric RA 
       *   @param dec   current epoch topocentric DEC
       *
       *   @return a pair, as a vector, of Az and El, in conformable quantities
       */
      Vector<Angle> getAzEl(double mjd, const Angle &ra, const Angle &dec);

      /**
       *   debug routine that dumps out the state of the Ephemeris 
       *   with lots of human readable verbiage
       */
      void Debug(void);

      /**
       * debug routine to calculate and print parameters for planetary
       * sources.
       * @todo absorb this stuff into Planet class.
       */
      void planetDebug(void);

      /**
       *  Compute the rotation of the coordinate system between the 
       *  mean equatorial coordinates (ra,dec) at J2000
       *  and the apparent coordinates as the current epoch.
       *
       *  The minimum requirements are setting an epoch and ra/dec (J2000)
       *  to derive the rotation angle from.
       *
       *  @return angle, in radians.
       */
       double angle2000(void);


      /**
       *  experiments with tracking spots on DE405 planets (object type=0)
       */

      void ShowVector(void);

      /**
       *   Set properties to be passed to novas about spinning bodies
       *   in order to produce an emphemeris for a spot on a spinning
       *   body
       *   @param majorAxis   arcsec
       *   @param minorAxis   arcsec
       *   @param axisAngle   deg
       *   @param tiltAngle   deg
       *   @param mjd         date at which we define the spot below (UTC) 
       *   @param Longitude   longitude on body - deg
       *   @param Latitude    latitude on body - deg
       *   @param SpinRate    Inverse period - 1/days (jupiter 2.1, sun 0.03 latitude dependant)
       *
       */
      void SetSpinningBodySpot(double majorAxis, double minorAxis, double axisAngle, double tiltAngle,
			       double mjd,  double Longitude, double Latitude, double SpinRate);
      


    protected:
      /**
       *   initialize all variables for this class. called by all constructors.
       */
      void initialize(void);

      /**
       *   compute refraction angle. 
       *   to be added to the elevation to get the new refracted elevation.
       *   Note it always returns refraction (unless freq < 1 or el < 0)
       *   irrespective of setRefrect(true|false)
       *
       *   It uses Atmosphere::computeRefractionCorrection() to compute the
       *   the refraction
       *
       *   @param   elevation, in radians 
       *   @return  refraction, in radians
       */
      double refract(const double elevation);

      /**
       *   the internal ephemeris compute engine. 
       *   Given an internal state (site, sky, time, weather etc.)
       *   it computes apparent ra,dec,az,el, time constants etc.etc.
       *   A number of the set-routines in Ephemeris will trigger recomputing 
       *   the state of the Ephemeris if queries are made (the get-routines)
       */
      void compute(void);

      /**
       *
       *  compute the Az,El between two Location's, used internally
       */
      void ComputeFixedAzEl(void);

      /**
       *  NOVAS helper routine for isPlanet() to construct a NOVAS body
       *  for a particular planet
       *  sourceName is actually the filename of the ephem file here.
       *  the sourcename may also have been upper cased, e.g.
       *  /somehere/conf/catalogs/SUN15.ephem
       *  since both lower and upper case will be matched.
       */
      bool my_set_body(short int type, short int number, const std::string& sourceName);

      /**
       *   return a vector with (sign,H,M,S) values for a given angle 0..360
       *   note that H and M will be integers
       */
      Vector<double> hms(double angle);
      
      /**
       *   return a vector with (sign,D,M,S) values for a given angle 0..360
       *   note that D and M will be integers
       */
      Vector<double> dms(double angle);






    private:
      novas::body      earth_;    // novas:: *we* always observe from here   body{0,3,'earth'}
      std::string      obs_;      // name of the site (via Location)
      novas::site_info site_;     // novas:: where we are (a novas:: struct)
      Location         location_; // copy of the original Location object
      
      novas::body      body_;     // novas:: the 'other' point of interest (or: SolarSystemBody)
      novas::cat_entry cat_;      // novas:: source info (dra/ddec are normally applied to ra/dec)
      novas::cat_entry fake_;     // novas:: fake source for quick ra/dec -> az,al lookup (getAzEl)
      bool             is_cat_;   // use body_ or cat_ to look at
      bool             is_eph_;   // body to be obtained from an RADEC table ephemeris?
      EphemerisTable   et_;       // if is_eph_, this is populated

      bool            fixed_;     // source is a fixed position on the planet (e.g. transmitter)
      Location fixed_object_;     // some nearby object that's fixed w.r.t. antennae
      double       fixed_az_;     // the computed (az,el) of fixed_object_ as seen from antennae
      double       fixed_el_;
 
      double     tjd_tt_;  // TT time at site [julian date]   -- not used at the time --
      double     tjd_;     // JD at site [utc]
      double     mjd_;     // MJD at site [utc]

      AstroTime  at_;      // our timekeeper (we don't use location dependant operations)

      
                           // things to look up from IERS tables
      //      IERSTable  iers_;    


      double     deltat_;  // TT-UT1 [seconds]
      double     tdbmtdt_; // TDB-TDT [seconds]  ** we don't use it **
      double     xpole_;   // x-coordinate of celestial ephemeris pole w.r.t. IERS reference pole (arcsec)
      double     ypole_;   // y-coordinate 

      double     freq_;    // Observing frequency [Hz]
      double     relhum_;  // Relative humidity [percent]
      bool       do_refract_;  // overall boolean that controls if refraction applied

      Source     source_;      // alternate for source, if given
      bool       use_source_;  // boolean to control what kind of 'source' is used (instead of ra/dec)
      std::string sourceName_; // The source name
      double     ra2000_;      // RA from catalog (J2000) [deg]
      double     dec2000_;     // DEC from catalog (J2000) [deg]
                               // Offsets are applied either in Ra,Dec *or* Az,El, not both
      int        offsetMode_;  // 0=none 1=ra/dec 2=az/el
      double     dra_;         // offet applied to any RA for mosaicing etc. [deg]
      double     ddec_;        // offet applied to any DEC for mosaicing etc.[deg]
      double     daz_;         // offset in Az [deg]
      double     del_;         // offset in El [deg]

      double     ra_;      // current RA/DEC, all topocentric  [deg]
      double     dec_;     // note these also have the DRA/DDEC applied  [deg]
      double     dis_;     // distance (0 if at infinity) [au]
      double     doppler_; // doppler velocity for this source  [km/s]
      double     az_;      // current AZ/EL [deg]
      double     el_;      // note these have the DRA/DDEC applied [deg]
      double     vearth_;  // local earth projection to source [km/s]
      double     vrest_;   // LSR projection to source (note sign) [km/s]
      double     vplanet_; // planet projection to earth [km/s]

      double     refr_;    // refraction coefficient [deg]

      // Units      units_;   // aid for unit conversion
      std::string  jpleph_;// filename of JPL planet ephemeris catalog (usually CARMA/conf/data/ephem/jpl.eph)

      carma::services::Atmosphere atm_;   // the atmosphere, for more sophosticated modeling (see also site_ )

      bool initialized_;    // keep track of overall initialization (since we have a few "constructor" type entry points)
      bool topocentric_;    // topocentric (alternative is geocentric, which we actually don't officially use)
      static const double deg2rad_;      // 180/PI  (multiply degrees with this to get to radians)
      static const double rad2deg_;      // PI/180  (multiply radians with this to get to degrees)

      bool recompute_;             // need to call compute() to recompute state ?
#ifdef EPHEM_DEBUG
      static int  ncall_;          // debugging, keep track how often expensive compute's are done
#endif

      /**
       * NB: Only to be used internally, as external is likely cause 
       * inconsistencies in a Source retrieved by getSource().
       *
       * Set some very basic source parameters.
       * This is the third (of 3) methods to set a source for the ephemeris.
       * This function should be used with caution, as it cannot construct a proper
       * Source, invalidating the Ephemeris::getSource() routine.
       * Note a new source will reset any existing RA/DEC or AZ/EL offsets..
       * 
       *
       * @param ra2000   RA in J2000 coordinates (radians) 
       * @param dec2000  DEC in J2000 coordinates (radians)
       * @param doppler  Doppler velocity of source (m/s)   -- vel system etc. ??
       * @param distance Distance to object (AU)
       */
      void setSource(const double ra2000, 
	             const double dec2000, 
		     const double doppler, 
		     const double distance = 0.0)
	  __attribute__((deprecated));


      /**
       *  find out if a source is in an ephemeris table derived
       *  from JPL's HORIZON's system. These tables are typically
       *  valid in a short (few months/years) time interval, e.g.
       *  comets, a sunspot etc. and are stored as <sourceName>.ephem
       *  files in /array/rt/catalogs, [$CARMA/conf/catalogs/observer] 
       *  $CARMA/conf/catalogs and the current directory (useful for testing).
       *  Normally the sourceName is tried in upper case , but the given
       *  case is tried as well.
       *  Ephem files need to be in VECTORS format, though some support for
       *  RADEC style tables is present but not in use.
       */
      bool isEphem(const std::string& sourceName);

      /**
       *  find out if a source is a supported planet with JPL
       *  ephemeris information. It will also generate a NOVAS body
       *  and thus this routine is not useful for the public
       */
      bool isPlanet(const std::string& sourceName);

public:
      /**
       *  find out if a source is a supported fixed source (e.g.
       *  transmitter) from the source catalog. Fixed sources
       *  need an Long,Lat,Elev from the source catalog from which
       *  a nominal Az,El can be computed.
       *
       */
      bool isFixed(const std::string& sourceName);

      /**
       *   Was a true Source was used, or should sourcename string be used
       *   to re-use an ephemeris (important for EphemerisTables)
       */
      bool useSource(void) const {
	return use_source_;
      }


    }; // class Ephemeris
  } // namespace services
} // namespace carma

 
#endif
