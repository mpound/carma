// -*- c++ -*-

/*
 *   @file carma/services/Ephemeris.cc
 *   Wraps the NOVAS library and any other ephemeris related functions into
 *   a simple class.
 *   For most of CARMA work one instantiates this class with a source, and
 *   alternatively a fixed station
 *           Ephemeris e("3c273","BIMA-9");     // really ??
 *   after which the time should be set (time needs to be set *after* a source)
 *           e.setMJD();
 *   and the RA/DEC or AZ/EL can be retrieved
 *           double ra=e.getRa();
 *           double az=e.getAz();
 *   Caveat: for more accurate observations you need to feed the ephemeris
 *   a more detailed atmosphere description (pressure, temperature, humidity)
 *   as well as an observing frequency.
 *
 *   Note that solar system calculations are often done in TT, not JD,
 *   where TT = JD + deltat
 *
 *   Known issues:
 *   - solar sytem objects are done in TT, not UTC;
 *     if you then take known RA/DEC and stuff it into a catalog, 
 *     you will be off by deltat (about 65 sec) proper motion
 *
 *   
 *   @author Peter Teuben
 *   @version $Id: Ephemeris.cc,v 1.93 2014/05/21 17:11:14 mpound Exp $
 *
 */

#include <cmath>
#include <limits>

#include "carma/services/Astro.h"
#include "carma/services/Ephemeris.h"
#include "carma/services/EphemerisException.h"
#include "carma/services/ephemFunctions.h"
#include "carma/services/Physical.h"
#include "carma/services/Frequency.h"
#include "carma/services/FluxDensity.h"
#include "carma/services/SourceCatalog.h"
#include "carma/services/stringConstants.h"
#include "carma/services/Location.h"
#include "carma/services/Types.h"

#include "carma/util/Time.h"
#include "carma/util/Trace.h"
#include "carma/util/Program.h"
#include "carma/util/Logger.h"
#include "carma/util/StringUtils.h"
#include "carma/util/FileUtils.h"
#include "carma/util/FileNotFoundException.h"
#include "carma/util/programLogging.h"

using namespace carma::services;
using namespace carma::services::constants;
using namespace carma::util;
using namespace std;

//bool   Ephemeris::initialized_ = false;
#ifdef EPHEM_DEBUG
int    Ephemeris::ncall_       = 0;
#endif

const double Ephemeris::deg2rad_  = M_PI/180.0;
const double Ephemeris::rad2deg_  = 180.0/M_PI;

//int    Ephemeris::first_       = 1;

Ephemeris::Ephemeris()
{
  initialized_ = false;
  initialize();
}
 
Ephemeris::Ephemeris(const std::string &sourceName, 
                     const std::string &catalogName)
{
  initialized_ = false;
  initialize();
  setSource(sourceName,catalogName);
}

Ephemeris::~Ephemeris()
{
}

Ephemeris::Ephemeris(const Source& s)
{
  initialized_ = false;
  initialize();
  setSource(s);
}

Ephemeris::Ephemeris(const Location& location)
{
  initialized_ = false;
  initialize();
  setLocation(location);
}

void
Ephemeris::setLocation(const Location &loc)
{
  recompute_   = true;
  site_.latitude    = loc.getLatitude().degrees();
  site_.longitude   = loc.getLongitude().degrees();
  site_.height      = loc.getAltitude().convert("meters");
  location_ = loc;
}


Location
Ephemeris::getLocation(void) const
{
  return location_;
}
 
void
Ephemeris::setWeather(const double atmPressure, 
	              const double airTemp, 
	              const double relHumid)
{
  recompute_   = true;
  // note these are to be in NOVAS units
  
  //make sure the values are within reasonable ranges,by
  //passing them through Atmosphere::safe routines.
  
  // mbar
  site_.pressure    = atm_.safeAtmPressure(atmPressure); 

  // Celsius = Kelvin - 273.15
  site_.temperature = atm_.safeAirTemperature(airTemp) 
                       + constants::Physical::ABS_ZERO;  
  // percent
  relhum_           = atm_.safeRelativeHumidity(relHumid); 
  //@todo warn here if input values were bad
}

void
Ephemeris::setFreq(const double freq)
{
  recompute_   = true;
  freq_        = freq;         // Hz 
}

double 
Ephemeris::getFreq() const
{
  return freq_  ; // Hz 
}

void
Ephemeris::setRefraction(const bool refract)
{
  recompute_   = true;
  do_refract_  = refract;
}


void
Ephemeris::setBody(const int type, const int number, const std::string &name)
{
  recompute_   = true;
  is_cat_      = false;
  // NOVAS: struct body { short type, short number, char[100] }
  if (name.length() > 99) {
    std::ostringstream os;
    os << "Ephemeris::setBody: source/file name too long for NOVAS: " << name;
    const string errorStr = os.str();
    carma::util::Program::getLogger().error(errorStr);
    throw CARMA_EXCEPTION(EphemerisException, errorStr);
  }
  novas::set_body(type, number, (char *) name.c_str(), &body_);
}


void
Ephemeris::setBody(const std::string &name)
{
  recompute_   = true;
  // TBD
  is_cat_ = false;
}

void 
Ephemeris::setMJD(const double mjd)
{
  double leap;
  mjd_ = mjd;
  recompute_   = true;
  if (mjd == 0 && tjd_ < 1)         // force the current time if not explicitly set
    mjd_ = carma::util::Time::MJD();
  leap    = at_.leap(mjd_);
  tjd_    = mjd_ + AstroTime::JULIAN_DAY_ZERO;
  tjd_   += (leap + 32.184)/86400.0; 
  deltat_ = -at_.ut1Utc(mjd_) + leap + 32.184;    // TT-UT1
  xpole_  = at_.xPolar(tjd_ - AstroTime::JULIAN_DAY_ZERO);
  ypole_  = at_.yPolar(tjd_ - AstroTime::JULIAN_DAY_ZERO);
  if (is_eph_) {
    et_.setMJD(mjd_ + deltat_/86400.0);     // et_ needs to get TT !!
  }
}

void 
Ephemeris::setDeltaT(const double deltat)
{
  recompute_   = true;
  deltat_ = deltat;     // TT-UT1
}

void
Ephemeris::setSource(const Source& source)
{
  recompute_  = true;

  dra_ = ddec_ = daz_ = del_ = 0.0;  // new source, reset all offsets
  doppler_  = 0.0;
  offsetMode_ = 0;
  sourceName_ = source.getName();
  if (isPlanet(sourceName_)) {       /* catch sneaky planet creators */
    is_cat_     = false;
    use_source_ = false;
    if ( tjd_ < 0 ) {
	setMJD( Time::MJD() );
    }
    compute();
    // force a recompute  later
    recompute_  = true;
    // warning: may need to populate the source_ object more if Amar really wants a Source !!
    return;
  }
  // @todo : catch sneaky EphemTable's? [should never happen of course]

  use_source_  = true;
  source_      = source;
  ra2000_  = source.getXCoordinate().degrees();
  dec2000_ = source.getYCoordinate().degrees();
  doppler_ = source.getVelocity().kms();

  // @todo   BIG TODO: this is a patch to fix the problem that we thought we has
  //         a system catalog with arcsec/100yr, but really have time-sec and arcsec
  //         per year.

  // FK5 format needs RA proper motion in seconds of time per (Julian?) century.
  // Source object stores in sky mas/yr in both ra and dec.
  double rapm = source.getXProperMotion() / 10.0;
  double radec =  source.getYProperMotion() / 10.0;
  double cosdec = cos(source.getYCoordinate().radians());
  if (cosdec != 0.0) rapm /= (15.0*cosdec);

  if (sourceName_.size() > 50)
  {
      std::ostringstream warnOs;
      warnOs<< "Warning: source name too long for novas (>50 chars): " 
	  << sourceName_;
      carma::util::Program::getLogger().warn(warnOs.str());
  }

  CARMA_CPTRACE(  ::carma::util::Trace::TRACEALL,
  	  " Ephemeris: Making catalog entry for " << sourceName_);
  //std::string msg = " Ephemeris: Making catalog entry for ";
  //carma::util::Program::getLogger().debug(msg+sourceName_);

  // create the NOVAS2 structure. In NOVAS3 units will change,
  // e.g. rapm and decpm will be in mas/year, parallax will be in mas

  char fk5[]      = "FK5";
  novas::make_cat_entry(fk5,const_cast<char*>(sourceName_.c_str()), 0,
		 ra2000_/15.0,
		 dec2000_,
 		 rapm, 
  		 radec,
		 source.getParallax().arcSeconds(),
		 // Note: FK5 calls this entry simple "radial velocity"
		 // with no reference frame or definition indicated!
		 source.getVelocity().kms(),
		 &cat_);

  is_cat_ = true;
}

void
Ephemeris::setSource(const std::string& sourceName, 
	             const std::string& catalogName)
{
  const string method = "Ephemeris::setSource("+sourceName+") ";
  try {
      recompute_   = true;
      fixed_ = false;

      dra_ = ddec_ = daz_ = del_ = 0.0;  // new source, reset all offsets
      doppler_ = 0.0;
      offsetMode_ = 0;
      CARMA_CPTRACE(Trace::TRACE6, method << "calling isPlanet");

      // the order in which we check for sources is codified here:
      // 1) Planet (JPL/NOVAS)
      // 2a) VECTORS EphemerisTable (files in catalogs directories in a PATH)
      // 2b) RADEC   EphemerisTable (files in catalogs directories in a PATH)
      // 3) Fixed Source (from Observatory.cat)
      // 4) Sourcename from SystemSource.cat or other catalog

      if (isPlanet(sourceName) || isEphem(sourceName)) {
	is_cat_     = false;
	use_source_ = false;
	sourceName_ = sourceName;
	if ( tjd_ < 0 ) setMJD( Time::MJD() );
	compute();
	CARMA_CPTRACE(Trace::TRACE6, method + " done with compute()" );
	CARMA_CPTRACE(Trace::TRACE6, "leaving " + method + " for planet");
	recompute_ = true;
	return;
      }

      if (isFixed(sourceName)) {    // fixed locations, such as transmitter
	sourceName_ = sourceName;
	fixed_ = true;
	return;
      } 

      use_source_  = true;  // get it from the catalog

      CARMA_CPTRACE(Trace::TRACE6, 
	      "Ephemeris::compute() looking up source " +sourceName);
      SourceCatalog sc; // @todo could make this a private member, for more rapid repeated access?
      Source source;

      // if a catalogName is given, try that before the (system) defaultCatalog
      // else just just the defaultCatalog
      if (catalogName.size() > 0) {
	  sc.open(catalogName); 
	  try {
		source = sc.lookup(sourceName);
	  } catch (SourceNotFoundException& snfe) {
		sc.open(SourceCatalog::defaultCatalog());
	  } 
      } else {
	  sc.open(SourceCatalog::defaultCatalog());
      }

      source = sc.lookup(sourceName);
      setSource(source);
      CARMA_CPTRACE(Trace::TRACE6, "leaving " + method);
  } catch ( const SourceNotFoundException & ) {
      if (catalogName.size() > 0) {
	  ostringstream errOs;
	  errOs << "Source " << sourceName << " not found in "
		<< "catalogs " << catalogName << " nor "
		<< SourceCatalog::defaultCatalog();
	   throw CARMA_EXCEPTION(SourceNotFoundException, errOs.str().c_str());
      } else {
	  throw;
      }
  } catch ( const FileNotFoundException & ) {
      throw;
  } catch ( const NotFoundException & ) {       // should never happen anymore
      throw;
  } catch ( const BaseException & ) {
      throw;
  } catch ( ... ) {
      throw CARMA_EXCEPTION(ErrorException,"Caught unkown error in " + method);
  }
}

void
Ephemeris::setSource(const double ra2000, const double dec2000, 
	             const double doppler, const double distance)
{
  // this routine is deprecated, should not be used, there are 2 other setSource()'s
  recompute_   = true;
  use_source_  = false;
  fixed_       = false;

  dra_ = ddec_ = daz_ = del_ = 0.0;  // new source, reset all offsets 
  offsetMode_ = 0;
  ra2000_      = ra2000*rad2deg_;    // we keep them in deg
  dec2000_     = dec2000*rad2deg_;   // 
  doppler_     = doppler/1000.0;     // we keep them in km/s
  dis_         = distance;           // in A.U.
  sourceName_  = NO_SOURCE;
  
  // make sure we're using the correct NOVAS units
  char fk5[]      = "FK5";
  double parallax = 0.0;
  if (distance > 0) parallax = 206264.806247096355/distance;
  novas::make_cat_entry(fk5,const_cast<char*>(sourceName_.c_str()), 0, // all fake
		 ra2000_/15.0,          // novas uses hours!!!
		 dec2000_,              // degrees
		 0.0,                   // time-seconds per century
		 0.0,                   // arc-second per century
		 parallax,              // arcsec parallax 
		 doppler_,              // km/s
		 &cat_);
  is_cat_ = true;
}

void
Ephemeris::setEphemerisTableSource(const double ra2000, const double dec2000, const double doppler, const double distance)
{
  // this routine is used internally (private) to aid EphemerisTable objects
  recompute_   = true;
  use_source_  = false;
  fixed_       = false;

  // note we're not messing with the offsetMode_
  ra2000_      = ra2000*rad2deg_;    // we keep them in deg
  dec2000_     = dec2000*rad2deg_;   // 
  doppler_     = doppler/1000.0;     // we keep doppler in km/s
  dis_         = distance;           // this was already in AU

  //printf("setE.T.S. %s @ %g : %g %g %g %g\n", sourceName_.c_str(), 
  // et_.getMJD(), ra2000_, dec2000_, doppler_, dis_);

  
  // make sure we're using the correct NOVAS units
  char fk5[]      = "FK5";  // Horizons uses ICRF/J2000.0 -- 
  double parallax = 0.0;
#if 0
  // you might think you need to set a parallax here,
  // but ephemeris tables (ra,dec,distance) already know the distance, so you can't double up
  // yet it's not perfect this way. better is to get a vector based system 
  if (distance > 0) parallax = 206264.806247096355/distance;
#endif
  novas::make_cat_entry(fk5,const_cast<char*>(sourceName_.c_str()), 0, // all fake
		 ra2000_/15.0,          // novas uses hours!!!
		 dec2000_,              // and degrees
		 0.0,                   // ra  propermotion: time-seconds per century
		 0.0,                   // dec propermotion: arc-second per century
  		 parallax,              // arcsec
		 doppler_,              // km/s
		 &cat_);
  is_cat_ = true;
}

Source 
Ephemeris::getSource(void) 
{
  // @todo   if setRaDecOffset used, these do not get applied to source_
  if (use_source_)
    return source_;     
  // needed to get correct J2000 coords for planets/ephem tables.
  compute();
  Angle ra(ra2000_,"degrees");
  Angle dec(dec2000_,"degrees");
  Velocity v(doppler_,"km/s");          // FIX: need VelFrame and VelDef from somewhere!

  // Distance variable dis_ will have been set by NOVAS if a planet, 
  // otherwise it should still be zero via initialize
  Distance distance(dis_,"AU");
  return Source(sourceName_,ra,dec,v,distance);
}

void
Ephemeris::setLocation(const std::string &observatory, bool topocentric)
{
  Location loc(observatory);
  recompute_   = true;
  topocentric_ = topocentric;
  obs_ = observatory;
  at_.setSite(loc); 

  site_.longitude   = loc.getLongitude().degrees();
  site_.latitude    = loc.getLatitude().degrees();
  site_.height      = loc.getAltitude().convert("meters");    // getValue() would have worked too
  //  site_.pressure    = atm_.DEFAULT_ATM_PRESSURE*exp(-site_.height/atm_.scaleHeight.dry);  
  //  @todo the scaleheight 9480 gives a too high value for CF, something like <8435> to get <781>
  site_.pressure    = atm_.STANDARD_ATM_PRESSURE*exp(-site_.height/9480.0);
  site_.temperature = 10.0;            // novas wants Celsius 
}

void 
Ephemeris::setRaDecOffsets(const double dra, const double ddec)
{
  recompute_   = true;
  offsetMode_ = 1;
  dra_  = dra *rad2deg_;  // dra_,ddec_ are now assumed to be angular degrees at 
  ddec_ = ddec*rad2deg_;  // the tangent point on the sky at (ra2000,dec2000)
  daz_ = del_ = 0.0;      // reset all other offsets

  // @todo  if use_source_, apply radec offsets now ???
}

void 
Ephemeris::setAzElOffsets(const double daz, const double del)
{
  recompute_   = true;
  offsetMode_ = 2;
  daz_ = daz*rad2deg_;
  del_ = del*rad2deg_;
  dra_ = ddec_ = 0.0;     // reset all other offsets
}

double
Ephemeris::getRa()
{
  compute();
  return ra_ * deg2rad_;
}

double
Ephemeris::getDec()
{
  compute();
  return dec_ * deg2rad_;
}

double
Ephemeris::getAz()
{
  compute();
  return az_ * deg2rad_;

}

double
Ephemeris::getEl()
{
  compute();
  return el_ * deg2rad_;
}

double
Ephemeris::getRefrac() const
{
  // we're making an important assumption here that no compute() is needed
  // and refraction is requested after a recent getAzEl() or getAz/getEl call
  return refr_;  // in degrees !!
}



Vector<double>
Ephemeris::getAzEl(double mjd, double ra, double dec)
{
  double az,el,zd,rar,decr, deltat,leap,xpole,ypole,tjd;
  Vector<double> azel(2);

  leap = at_.leap(mjd);
  tjd = mjd + AstroTime::JULIAN_DAY_ZERO;       // convert to jd 
  tjd += (leap + 32.184)/86400.0;               // convert to TT 
  deltat = -at_.ut1Utc(mjd) + leap + 32.184;    // TT-UT1
  xpole  = at_.xPolar(tjd - AstroTime::JULIAN_DAY_ZERO);
  ypole  = at_.yPolar(tjd - AstroTime::JULIAN_DAY_ZERO);

  ostringstream eqOS;
  eqOS << " Ephemeris::getAzEl() calling NOVAS equ2hor"
       << "  tjd:    " << setprecision(16) << tjd_ 
       << "  deltat: " << setprecision(16) << deltat_ 
       << "  xpol: " << setprecision(16) << xpole_
       << "  ypol: " << setprecision(16) << ypole_
       << "  ra: " << setprecision(8) << ra
       << "  dec: " << setprecision(8) << dec;
  novas::equ2hor  (tjd, deltat, xpole, ypole, &site_, ra/15.0*rad2deg_, dec*rad2deg_, 0, &zd, &az, &rar, &decr);
  eqOS << " zd returned: " << zd << " degrees";
  el = 90.0-zd;
  refr_ = refract(el*deg2rad_)*rad2deg_;
  eqOS << " refr: " << refr_*60.0 << " arcmin";
  CARMA_CPTRACE( Trace::TRACE6, eqOS.str() );
  if (do_refract_)
    el += refr_;
  azel[0] = (az + daz_/cos(el*deg2rad_)) * deg2rad_;
  azel[1] = (el + del_) * deg2rad_;
  return azel;
}

Vector<Angle>
Ephemeris::getAzEl(double mjd, const Angle &currentRa, const Angle &currentDec)
{
  //double az,el,zd,rar,decr, tjd;
  //double ra,dec;
  Vector<Angle> azel(2, Angle(0.0, "degrees"));
  Vector<double> azeld(2);

  azeld = getAzEl(mjd, currentRa.radians(), currentDec.radians());
  azel[0] = Angle(azeld[0], "radians");
  azel[1] = Angle(azeld[1], "radians");
  return azel;
}


/* see also sza/array/code/share/slalib/h2e.c */  

Vector<double> 
Ephemeris::getRaDec(double mjd, double az, double el)
{
  Vector<double> radec(2);
  double sa,ca,se,ce,sp,cp;
  double r,x,y,z,phi;

  /* the observatory */
  phi = site_.latitude * deg2rad_;

  /* Useful trig functions */
  sa = sin ( az );
  ca = cos ( az );
  se = sin ( el );
  ce = cos ( el );
  sp = sin ( phi );
  cp = cos ( phi );

  /* HA,DEC as x,y,z */
  x = - ca * ce * sp + se * cp;
  y = - sa * ce;
  z = ca * ce * cp + se * sp;
  
  /* back to spherical */
  r = sqrt ( x * x + y * y );
  radec[0] = ( r == 0.0 ) ? 0.0 : atan2 ( y, x ) ;
  radec[1] = atan2 ( z, r );

  /* radec[0] is now HA, not RA, so need LST :  RA = LST - HA */

  double lst =  at_.localSiderealTime(mjd) * M_PI/12.0;    // lst now in radians */
  radec[0] = lst - radec[0];
  if (radec[0] < 0) radec[0] += 2*M_PI;
  if (radec[0] > 2*M_PI) radec[0] -= 2*M_PI;

  return radec;
}

double
Ephemeris::getDoppler(velocityFrameType frameType)
{
  compute();
  if (frameType != FRAME_TOPOGRAPHIC)
    cout << "getDroppler frameType : only TOPO supported for now" << endl;
  return 1000.0 * (doppler_ - vearth_ - vplanet_ + vrest_);
}

// @todo   should use Angle::getString

Vector<double>
Ephemeris::dms(double angle)
{
  int dd,mm,sign = 1;
  double ss;
  Vector<double> ddmmss(4);

  if (angle < 0) {
    sign = -1;
    angle *= -1;
  }
  dd = (int) angle;
  angle = (angle-dd)*60.0;
  mm = (int) angle;
  ss = (angle-mm)*60.0;
  ddmmss[0] = sign;
  ddmmss[1] = dd;
  ddmmss[2] = mm;
  ddmmss[3] = ss;
  return ddmmss;
}

Vector<double>
Ephemeris::hms(double angle)
{
  return dms(angle/15.0);
}

bool
Ephemeris::isEphem(const std::string& sourceName)
{
  string efile = EphemFile(sourceName);

  if (efile.empty()) return false;

  cout << "isEphem:: found " << efile << endl;

  // now assume this ephem file exists. figure what type
  // and process them accordingly
  CARMA_CPTRACE(Trace::TRACE1, "Ephemeris::isEphem() found file " + efile);
  EphemerisTable et;
  ephemTableType ett = et.getEphemType(efile);
  if (ett == EPHEM_VECTORS) 
    return my_set_body(2,1,efile);
  else if (ett == EPHEM_RADEC) {
    is_eph_ = true;
    et_.open(efile,0);
    return true;
  } 
  CARMA_CPTRACE(Trace::TRACE1, 
	      "Ephemeris::isEphem() failed to find ephem " +sourceName);
  return false;
}

bool
Ephemeris::isPlanet(const std::string& sourceName)
{
  string lcSource = StringUtils::lowASCIIAlphaNumericToLower(sourceName);
  if (lcSource == "sun")      
      return  my_set_body(0,10,lcSource);
  if (lcSource == "moon")
      return  my_set_body(0,11,lcSource);
  if (lcSource == "mercury")
      return  my_set_body(0,1,lcSource);
  if (lcSource == "venus")
      return  my_set_body(0,2,lcSource);
  if (lcSource == "mars")
      return  my_set_body(0,4,lcSource);
  if (lcSource == "jupiter")
      return  my_set_body(0,5,lcSource);
  if (lcSource == "saturn")
      return  my_set_body(0,6,lcSource);
  if (lcSource == "uranus")
      return  my_set_body(0,7,lcSource);
  if (lcSource == "neptune")
      return  my_set_body(0,8,lcSource);
  if (lcSource == "pluto")
      return  my_set_body(0,9,lcSource);

#if 0
  /* a more complex version of solsys2.c is needed for the minor planets */
  /* and something to handle arbitrary NAIF ID's ? */
  if (sourceName == "ceres")    return  my_set_body(1,1,sourceName);
  if (sourceName == "cybele")   return  my_set_body(1,1,sourceName);
  if (sourceName == "davida")   return  my_set_body(1,1,sourceName);
  if (sourceName == "eunomia")  return  my_set_body(1,1,sourceName);
  if (sourceName == "europa")   return  my_set_body(1,1,sourceName);
  if (sourceName == "flora")    return  my_set_body(1,1,sourceName);
  if (sourceName == "hebe")     return  my_set_body(1,1,sourceName);
  if (sourceName == "hygiea")   return  my_set_body(1,1,sourceName);
  if (sourceName == "interamni")return  my_set_body(1,1,sourceName);
  if (sourceName == "iris")     return  my_set_body(1,1,sourceName);
  if (sourceName == "juno")     return  my_set_body(1,1,sourceName);
  if (sourceName == "metis")    return  my_set_body(1,1,sourceName);
  if (sourceName == "pallas")   return  my_set_body(1,1,sourceName);
  if (sourceName == "psyche")   return  my_set_body(1,1,sourceName);
  if (sourceName == "vesta")    return  my_set_body(1,1,sourceName);
#endif
  return false;
}

bool
Ephemeris::isFixed(const std::string& sourceName)
{
  string lcSource = StringUtils::lowASCIIAlphaNumericToLower(sourceName);

  // should be a better way to add fixed things, e.g. look at
  // the configs field and grab all fixed? loop over these
  // or allow any positions?

  if (lcSource == TRANSMITTER ) {
    Location loc( CARMA_OBSERVATORY , TRANSMITTER );
    fixed_object_.setLatitude(loc.getLatitude());
    fixed_object_.setLongitude(loc.getLongitude());
    fixed_object_.setAltitude(loc.getAltitude());
    return true;
  }

  return false;
}

//  there could be a lot wrong here...
//  the earth is not a sphere, so what DO our lat/lon/alt actually mean?
//  we've assumed dx = R.d(phi)
//
void
Ephemeris::ComputeFixedAzEl(void)
{
  double r = Astro::EARTH.radius;
  double cosy = cos(location_.getLatitude().radians());
  double dx = (fixed_object_.getLongitude().radians() - location_.getLongitude().radians())*cosy*r;
  double dy = (fixed_object_.getLatitude().radians()  - location_.getLatitude().radians())*r;
  double dz =  fixed_object_.getAltitude().meters()   - location_.getAltitude().meters();

  if (dx==0.0 && dy==0.0) {
    cerr << "looking at your inner self? location and fixed_object are the same" << endl;
    az_ = 0.0;
    el_ = 0.0;
    return;
  }

  fixed_az_ = atan2(dx,dy) * rad2deg_;
  fixed_el_ = atan2(dz,sqrt(dx*dx+dy*dy)) * rad2deg_;
  
#if 0
  cout << "FixedAzEl: " << dx << " " << dy << " " << dz << endl;
  cout << "Loc: " << location_.getAltitude().meters() << endl;
  cout << "Fix: " << fixed_object_.getAltitude().meters() << endl;
  cout << "Az,El=" << fixed_az_ << " " << fixed_el_ << endl;
#endif
}



bool
Ephemeris::my_set_body(short int type, short int number, const std::string& sourceName)
{
  if (sourceName.length() > 99) {
    // this is just a fairly arbitrarly limitation
    //       char name[100] 
    // in novas.h and could be changed easily. Keep the last element for the terminating 0
    std::ostringstream os;
    os << "Ephemeris::setBody: source/file name too long for NOVAS: " << sourceName;
    const string errorStr = os.str();
    carma::util::Program::getLogger().error(errorStr);
    throw CARMA_EXCEPTION(EphemerisException, errorStr);
  }
  novas::set_body(type,number,(char *)sourceName.c_str(),&body_); 
  return true;
}

void Ephemeris::ShowVector()
{
  double pos[3], vel[3];
#ifdef CARMA6
  // 0 = barycenter required
  novas::ephemeris_spot(tjd_, &body_, 0, pos, vel);
  printf("%17.9f,  A.D. yyy-mmm-dd hh:mm:ss.ss, %.15e,  %.15e,  %.15e, %.15e, %.15e, %.15e,\n",
	 tjd_, pos[0], pos[1], pos[2], vel[0], vel[1], vel[2]);
  //  cout << "BODY: " << body_.type << " " << body_.number << " " << body_.name << endl;
  //  cout << "VECTORS: " << pos[0] << " " << pos[1] << " " << pos[2] << endl;
#else
  novas::ephemeris(tjd_, &body_, 0, pos, vel);
  cout << "ShowVector-pre6: " << pos[0] << " " << pos[1] << " " << pos[2] << endl;
#endif
}

void
Ephemeris::SetSpinningBodySpot(double majorAxis, double minorAxis, double axisAngle, double tiltAngle,
			      double mjd,  double Longitude, double Latitude, double SpinRate)
{
  // this is a very special entry for novas, where it can follow the motion in VECTORS format
  // of a spot on a spinning ellipsoidal body. The angular momentum axis remains fixed in space
  // and therefore this routine is only valid for 'short' intervals where the axisAngle and titleAngle
  // do not change
  novas::set_body_spot(majorAxis,minorAxis,axisAngle,tiltAngle,
		       mjd+AstroTime::JULIAN_DAY_ZERO,Longitude,Latitude,SpinRate);
}

void
Ephemeris::Debug()
{
  double mjd = tjd_ - AstroTime::JULIAN_DAY_ZERO;          //  modified julian date
  double T = 2000.000 + (mjd - 51544.03) / 365.2422;       //  Besselian date
  Vector<double> v;
  

  //  tdbmtdt_ = tdbmtdt(mjd);       // set TDB-TDT, we don't use it otherwise (yet)

  compute();       // go and compute the current sky

  double lst = at_.localSiderealTime(mjd);  // in hr

  printf("Site:      %s\n",obs_.c_str());
  v = dms(site_.longitude);
  printf("  long:     %lf deg %s%02d:%02d:%06.3lf\n",  site_.longitude, 
	 v[0]>0?"+":"-",(int)v[1],(int)v[2],v[3]);
  v = dms(site_.latitude);
  printf("  lat:      %lf deg %s%02d:%02d:%06.3lf\n",  site_.latitude, 
	 v[0]>0?"+":"-",(int)v[1],(int)v[2],v[3]);
  printf("  alt:      %.1lf m\n", site_.height);
  printf("  %s\n", topocentric_ ?   "Topocentric" : "Geocentric");
  printf("Ephemeris:: using %s (%s)\n",  is_cat_ ?  "cat_entry" : "body",  
	 is_eph_ ? "w/ephem table" :  fixed_ ? "fixed" : "");
  printf("  source:   %s\n", sourceName_.c_str());
  printf("  tjd:      %18.10lf JD (TT)\n", tjd_);
  printf("  mjd:      %18.10lf\n", mjd);
  printf("            %s\n",carma::util::Time::getDateString(mjd).c_str());
  printf("            %s\n",carma::util::Time::getTimeString(mjd).c_str());
  printf("            %s\n",carma::util::Time::getFITSdateTimeString(mjd).c_str());
  printf("  T         %lf (besselian date)\n", T);
  //  printf("  UT2-UT1   %lf sec\n", ut2m1(mjd));
  printf("  UT1-UTC   %lf sec (dut1)\n", at_.ut1Utc(mjd));
  printf("  deltat    %lf sec (TT-UT1)\n", -at_.ut1Utc(mjd) + at_.leap(mjd) + 32.184);
  printf("  deltat    %lf sec (TT-UT1) [manual test]\n", deltat_ );
  // printf("  TDB-TDT   %lf sec\n", tdbmtdt_);
  printf("  x_pole    %lf arcsec\n", xpole_);
  printf("  y_pole    %lf arcsec\n", ypole_);
  v = hms(ra2000_); 
  printf("  ra2000:   %lf deg %s%02d:%02d:%06.3lf %10.8f rad\n", ra2000_, v[0]>0?"+":"-", (int)v[1],(int)v[2],v[3], ra2000_*M_PI/180);
  v = dms(dec2000_);
  printf("  dec2000:  %lf deg %s%02d:%02d:%06.3lf %10.8f rad\n", dec2000_,v[0]>0?"+":"-", (int)v[1],(int)v[2],v[3], dec2000_*M_PI/180);
  v = hms(ra_);
  printf("  ra:       %lf deg %s%02d:%02d:%06.3lf %10.8f rad\n", ra_,     v[0]>0?"+":"-", (int)v[1],(int)v[2],v[3], ra_*M_PI/180);
  v = dms(dec_);
  printf("  dec       %lf deg %s%02d:%02d:%06.3lf %10.8f rad\n", dec_,    v[0]>0?"+":"-", (int)v[1],(int)v[2],v[3], dec_*M_PI/180);
  v = dms(az_);
  printf("  az:       %lf deg %s%02d:%02d:%06.3lf %10.8f rad\n", az_,     v[0]>0?"+":"-", (int)v[1],(int)v[2],v[3], az_*M_PI/180);
  v = dms(el_);
  printf("  el:       %lf deg %s%02d:%02d:%06.3lf %10.8f rad\n", el_,     v[0]>0?"+":"-", (int)v[1],(int)v[2],v[3], el_*M_PI/180);
  v = hms(lst*15);
  printf("  lst:      %lf hr  %s%02d:%02d:%06.3lf\n", lst,     v[0]>0?"+":"-", (int)v[1],(int)v[2],v[3]);
  printf("  pm-ra     %lf time-sec/century\n",cat_.promora);
  printf("  pm-dec    %lf arc-sec/century\n",cat_.promodec);
  printf("  distance: %lf AU\n",dis_);
  printf("  angle2000 %lf deg\n", angle2000() * 180.0/M_PI);
  //-------------------------------------------
  // @TODO REPLACE ALL THIS WITH PLANET CLASS
  // if a planet print out estimate of current angular size
  if ( isPlanet ( sourceName_ ) ) {
      planetDebug();
  }
  printf("  Doppler:  %g  %g %g [earth rest planet] km/s\n",vearth_, vrest_, vplanet_);
  printf("  Redshift: %lf km/s\n", doppler_ - vearth_ + vrest_ - vplanet_);
  printf("  T/p/RH:   %.1lf C, %.1lf mbar, %.1lf %%\n",site_.temperature, site_.pressure, relhum_);
  printf("  freq:     %lf GHz\n", freq_ / 1e9);
  printf("  refr:     %.2lf arcsec\n", refr_ * 3600);
#ifdef EPHEM_DEBUG
  printf("  calls:    %d\n",ncall_);
#endif
}
// -------- worker routines

void Ephemeris::planetDebug() 
{
      // set the planetary monitor points.
      Distance dist(dis_,"AU");
      double dmeters = dist.meters();
      Astro::planetType thePlanet = Astro::getPlanet(sourceName_);
      // use small angle theorem: tan(theta) ~ theta.
      Angle ma(2.0*thePlanet.radius/dmeters,"radians");
      double majorAxis = ma.arcSeconds();
      double minorAxis = majorAxis * thePlanet.aspectRatio;
      // @TODO This is Tb @ 100 GHz, future Planet class will
      // have frequency dependence. (See also thePlanet.tempIndex).
      double temperature = thePlanet.brightnessTemp; 

      printf("  Major axis = %.2f arcseconds\n",majorAxis);
      printf("  Minor axis = %.2f arcseconds\n",minorAxis);
      //@ todo scale brightness temperature according to frequecny
      printf("  Brightness temp @ 100 GHz = %.2f K\n",temperature);

      // planet flux density from MIRSUBS/model.for
      // flux = solid angle * Blackbody function.
      // flux = omega * [2 H nu^2 / C^2 ] * 1 / [ exp(hv/KT) - 1) ]
      //
      // if freq_ less than 1 Hz (i.e. was unspecified on command line), 
      // use 100 GHz for flux calculation
      double curfreq = freq_;
      if ( freq_ < 1 ) 
	  curfreq = 1e11; 
      // area of ellipse = PI/4 * major axis * minor axis
      // want omega in sterradians.
      double omega = M_PI/4 * ma.radians()*ma.radians()*thePlanet.aspectRatio;
      double flux = omega * 2. * Physical::H * (pow(curfreq,3)) 
	            / (Physical::C*Physical::C) ;
      flux /=  ( exp((Physical::H*curfreq)/(Physical::K*temperature)) - 1. );
      FluxDensity f(flux,"W/(m^2 Hz)");
      Frequency afreq(curfreq,"Hz");
      printf("  Flux Density (%.1f GHz) = %.2f Jy\n",afreq.gigahertz(), f.jansky());

}

void
Ephemeris::initialize(void)
{
  if (initialized_)      // make sure this routine is only called once !
    return;

  initialized_ = true;    
  recompute_   = true;

  tjd_    = -1.0;        // to signify that time (tjd or mjd) was not initialized yet
  deltat_ = 0.0;         // also needs to be set when mjd is set, should be about 65"
  setLocation( CARMA_OBSERVATORY );  // set a default location to observe from
  use_source_ = false;
  is_eph_     = false;
  fixed_      = false;
  sourceName_ = NO_SOURCE;  // This name REQUIRED by control to mean no source.
  offsetMode_ = 0;
  dra_        = 0.0;
  ddec_       = 0.0;
  daz_        = 0.0;
  del_        = 0.0;
  ra_         = 0.0;    // all in degrees for NOVAS
  dec_        = 0.0;
  dis_        = 0.0;    // A.U. (0 means infinity)
  ra2000_     = 0.0;    // all in degrees
  dec2000_    = 0.0;
  doppler_    = 0.0;    // km/s
  // Use a proper setSource here.
  Source initialSource(
	  sourceName_,
	  Angle( ra2000_, "degree" ),
	  Angle( dec2000_, "degree" ),
	  Velocity( doppler_, "km/s" ),
	  Distance( dis_, "AU" )
	  );
  setSource( initialSource );
  az_         = 0.0;
  el_         = 0.0;
  xpole_      = 0.0;    // arcsec
  ypole_      = 0.0;
  freq_       = 1e11;   // 100GHz, in Hz 
  relhum_     = 30.0;   // percent
  do_refract_ = true; 
  refr_       = 0.0;

  char earthStr[] = "earth";
  novas::set_body(0, 3, earthStr, &earth_);    // 3rd rock from the sun

  // Pass the location of the JPL ephemeris to "novas"
  // you need solsys4.c for this (in novas-c201p1 and up)
  string jpleph_ = Program::getConfFile("data/ephem/jpl.eph");
  novas::set_jpleph( (char *) jpleph_.c_str());

}    

double
Ephemeris::refract(const double elevation)
{
  double r;

  if (freq_ < 1.0 || elevation <= 0.0) {
      ostringstream os;
      os << "Ephemeris::refract() returning zero because "
	 << "frequency or elevation are bad."
	 << "Freq = " << freq_ << " EL = " << elevation;
      CARMA_CPTRACE(Trace::TRACE6, os.str() )
      //programLogErrorIfPossible(os.str());
      return 0.0;
  }
  r = atm_.computeRefractionCorrection(
	    site_.temperature - constants::Physical::ABS_ZERO,   // Kelvin
 	    site_.pressure,                           // mbar
	    relhum_,                                  // percent
	    elevation,                                // radians
	    freq_,                                    // Hz
	    site_.height);                            // m
  return r;
}


// some notes on accuracy:
//
// the algorithm is very simple, offset dec by 1 arcmin
// and measure the ra,dec again and compute the rotation angle
//
// note that miriad's PREROTATE() does a different order from
// how NOVAS works; miriad does precession, nutation, abberation
// where NOVAS does abberation, then precession and nutation
// accuracy compared:     0.000869 vs. 876    3c273 28-jan-2003
//                        0.00142  vs. 137    
//                        0.00148  vs. 142    3c279 13-jul-2000
// This routine is currently not used for anything.

double
Ephemeris::angle2000(void)
{
  short int retval = 0;
  double r0,d0,r1,d1,theta;
  novas::cat_entry cat; 
  char fk5[]      = "FK5";

  compute();

  novas::make_cat_entry(fk5,(char *)"fakeangl", 0, // all fake
			ra2000_/15.0,          // novas uses hours!!!
			dec2000_,              // degrees
			0.0,                   // time-seconds per century
			0.0,                   // arc-second per century
			0.0,                   // arcsec parallax 
			0.0,                   // km/s
			&cat);

#if 1
  retval += novas::app_star(tjd_, &earth_, &cat, &r0, &d0);
  cat.dec += 1.0/60.0;
  retval += novas::app_star(tjd_, &earth_, &cat, &r1, &d1);
#else
  retval = novas::topo_star(tjd_, &earth_, deltat_, &cat_, &site_, &r0, &d0);
  cat.dec += 1.0/60.0;
  retval = novas::topo_star(tjd_, &earth_, deltat_, &cat_, &site_, &r1, &d1);
#endif

  if (retval) {
    std::ostringstream os;
    os << "Ephemeris::angle2000 bad return:" << retval ;
    const string errorStr = os.str();
    carma::util::Program::getLogger().error(errorStr);
    throw CARMA_EXCEPTION(EphemerisException, errorStr);
  }

  d0 = (d1 - d0);
  r0 = (r1 - r0) * 15 * cos(dec2000_*M_PI/180.0);
  theta = -atan2(r0,d0);
  if (fabs(theta) > 0.1) {
    std::ostringstream os;
    os << "Ephemeris::angle2000 odd theta=" << theta;
    carma::util::Program::getLogger().warn(os.str());
  }
  return theta;
}


void
Ephemeris::compute(void)
{
  short int retval;
  double zd, rar, decr;

  if (!recompute_) return;     // prevent rentry if nothing was changed
  recompute_ = false;

  // cout << "PJT1: " << ra2000_ << " " << dec2000_ << "\n";
  
  if (tjd_ < 0) {
    std::ostringstream os;
    os << "Ephemeris::compute - mjd not set: ";
    const string errorStr = os.str();
    carma::util::Program::getLogger().error(errorStr);
    throw CARMA_EXCEPTION(EphemerisException, errorStr);
  }

  std::ostringstream siteOs;
  siteOs << "Ephemeris::compute with site info: "
      << " Lon (d)= " << site_.longitude   
      << " Lat (d)= " << site_.latitude  
      << " Alt (m)= " << site_.height   ;
  //Program::getLogger().debug(siteOs.str());
  CARMA_CPTRACE(Trace::TRACE6,siteOs.str() );

  // for a fixed source in the neighborhood, compute the fixed AzEl here
  // add in the offsets, and set all other stuff (ra,dec,doppler) to 0
  if (fixed_) {
    ComputeFixedAzEl();
    az_ = fixed_az_;
    el_ = fixed_el_;
    if (offsetMode_ == 2) {
      if (el_ < 90.0)
	az_ += daz_ / cos(el_*deg2rad_);
      el_ += del_;
      // @todo  near zenith fix needed here?
      // theoretically, el_ can now be > 90 !!!    
      // if (el>90) el=180-el   and az += 180 ??
      // but for fixed_ we're not looking at the zenith, are we?
    }    
    ra_     = dec_     = 0.0;
    ra2000_ = dec2000_ = 0.0;
    vearth_ = vrest_ = vplanet_ = 0.0;
    return;
  }

  // cout << "PJT2: " << ra2000_ << " " << dec2000_ << "\n";

  // special ephemeris table object, only depends on the mjd[TT] having been set, see setMJD()
  if (is_eph_) {
    // set the ra,dec etc. in a cat_ for NOVAS
    // ??? et_.setMJD() ???
    et_.setMJD(mjd_ + deltat_/86400.0);     // et_ needs to get TT !!
    setEphemerisTableSource(et_.getRa(), et_.getDec(), et_.getDoppler(), et_.getDistance());
  }

  vearth_ = vrest_ = vplanet_ = 0.0;
  if (is_cat_) {
    // upon output we have:   ra (hours) dec (degrees)
    // we need to to keep ra in degrees upon exit from this routine
    if (topocentric_)
      retval = novas::topo_star_doppler(tjd_, &earth_, deltat_, &cat_, &site_, 
				&ra_, &dec_, &vearth_, &vrest_);
    else // geocentric
      retval = novas::app_star_doppler(tjd_, &earth_,           &cat_, 
			       &ra_, &dec_,  &vearth_, &vrest_);
    std::ostringstream os;
    os << "Ephemeris::computer - return value from topo/app_star was " 
       << retval
       << " topo/app got ra (h) = " << ra_
       << " topo/app got dec (deg)= " << dec_;
    CARMA_CPTRACE(Trace::TRACE6, os.str() );
    //carma::util::Program::getLogger().debug(os.str());

    if (retval) {
      std::ostringstream starOs;
      starOs << "Ephemeris::compute - app/topo_star: bad return:" 
	     << retval 
             << "  tjd:    " << setprecision(16) << tjd_ 
             << "  deltat: " << setprecision(16) << deltat_ ;
      const string errorStr = starOs.str();
      carma::util::Program::getLogger().error(errorStr);
      throw CARMA_EXCEPTION(EphemerisException, errorStr);
    }
  } else {
    retval = novas::astro_planet(tjd_, &body_, &earth_, 
			   &ra2000_, &dec2000_, &dis_);
    std::ostringstream os;
    os << "Ephemeris: return value from astro_planet was " << retval;
    os << " astro_planet got ra (h) = " << ra2000_;
    os << " astro_planet got dec (deg)= " << dec2000_;
    //carma::util::Program::getLogger().debug(os.str());
    CARMA_CPTRACE(Trace::TRACE6, os.str() );

    if (retval) {
      std::ostringstream planetOs;
      planetOs << "Ephmeris::compute - astro_planet: bad return value:" 
           << retval 
           << "  body name: " << body_.name 
           << "  tjd:    " << setprecision(16) << tjd_;
      const string errorStr = planetOs.str();
      carma::util::Program::getLogger().error(errorStr);
      throw CARMA_EXCEPTION(EphemerisException, errorStr);
    }
    ra2000_ *= 15.0;

    if (topocentric_)
      retval = novas::topo_planet_doppler(tjd_, &body_, &earth_, deltat_, &site_, 
					  &ra_, &dec_, &dis_, &vearth_, &vplanet_);
    else
      retval = novas::app_planet_doppler(tjd_, &body_, &earth_, 
					 &ra_, &dec_, &dis_, &vearth_, &vplanet_);

    if (retval) {
      std::ostringstream applanetOs;
      applanetOs << "Ephemeris::compute - app/topo_planet: bad return:" 
	         << retval 
                 << "  tjd:    " << setprecision(16) << tjd_ 
                 << "  deltat: " << setprecision(16) << deltat_ ;
      const string errorStr = applanetOs.str();
      carma::util::Program::getLogger().error(errorStr);
      throw CARMA_EXCEPTION(EphemerisException, errorStr);
    }
  }

  // Now convert ra,dec to az,el
  // we don't compute refraction via NOVAS, but via our own model (see Atmosphere below)
  // also note that this is a void function, no NOVAS failure possible! hmmmm....
	 
  ostringstream eqOS;

  eqOS << " Ephemeris::compute() calling NOVAS equ2hor"
       << "  tjd:    " << setprecision(16) << tjd_ 
       << "  deltat: " << setprecision(16) << deltat_ 
       << "  xpol: " << setprecision(16) << xpole_
       << "  ypol: " << setprecision(16) << ypole_
       << "  ra: " << setprecision(8) << ra_
       << "  dec: " << setprecision(8) << dec_;
  novas::equ2hor  (tjd_, deltat_, xpole_, ypole_, &site_, ra_, dec_, 0, &zd, &az_, &rar, &decr);
  eqOS << " zd returned: " << zd << " degrees";
  el_ = 90.0 - zd;

  // we keep RA in degrees; AZ was already in degrees in NOVAS !! 
  ra_ *= 15.0;    
  refr_ = refract(el_*deg2rad_) * rad2deg_;
  eqOS << " refr: " << refr_*60.0 << " arcmin";
  CARMA_CPTRACE( Trace::TRACE6, eqOS.str() );
  if (do_refract_)
    el_ += refr_;

  // apply either RA/DEC or AZ/EL offsets
  if (offsetMode_ == 1) {
    if (dec_ < 90.0 && dec_ > -90.0)
      ra_ += dra_ / cos(dec2000_*deg2rad_);
    dec_ += ddec_;
  } else if (offsetMode_ == 2) {
    if (el_ < 90.0)
      az_ += daz_ / cos(el_*deg2rad_);
    el_ += del_;
  }
#ifdef EPHEM_DEBUG
  ncall_++;  // keep track of how many times we've called this.
#endif

  // cout << "PJT3: " << ra2000_ << " " << dec2000_ << "\n";
}
