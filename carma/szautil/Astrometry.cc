#include "carma/szautil/Astrometry.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/QuadraticInterpolatorNormal.h"

#include "carma/szaslalib/slalib.h"

using namespace sza::util;
using namespace std;

// A macro for checking input values

#define CHECK_MJD(mjd) \
 {\
   if(mjd < 0) { \
     LogStream errStr; \
     errStr.initMessage(true); \
     errStr << "Illegal mjd: " << mjd << endl; \
     throw Error(errStr); \
   }; \
 }

// Some constants used by this class

const double Astrometry::secondsPerDay_ = 86400;
const double Astrometry::pi_            = M_PI;
const double Astrometry::twopi_         = 2*M_PI;

/**.......................................................................
 * Constructor.
 */
Astrometry::Astrometry() 
{
  ut1Utc_ = 0;
  eqnEqx_ = 0;
  
  ut1Utc_ = new QuadraticInterpolatorNormal(0.0);
  eqnEqx_ = new QuadraticInterpolatorNormal(0.0);
}

/**.......................................................................
 * Destructor.
 */
Astrometry::~Astrometry() 
{
  if(ut1Utc_ != 0) {
    delete ut1Utc_;
    ut1Utc_ = 0;
  }
  
  if(eqnEqx_ != 0) {
    delete eqnEqx_;
    eqnEqx_ = 0;
  }
}

/**.......................................................................
 * Extend the quadratic interpolation table of ut1 - utc
 * versus MJD UTC.
 *
 * @throws Exception
 */
void Astrometry::extendUt1Utc(double mjd, double ut1Utc)
{
  ut1Utc_->extend(mjd, ut1Utc);
}

/**.......................................................................
 * Extend the quadratic interpolation table of the equation
 * of the equinoxes versus Terrestrial Time (as a Modified
 * Julian Date).
 *
 * @throws Exception
 */
void Astrometry::extendEqnEqx(double tt, double eqnEqx)
{
  eqnEqx_->extend(tt, eqnEqx);
}

/**.......................................................................
 * Get the value of UT1-UTC for a given UTC.
 *
 * @throws Exception
 */
double Astrometry::getUt1Utc(double mjdUtc)
{
  if(ut1Utc_->canBracket(mjdUtc))
    return ut1Utc_->evaluate(mjdUtc);
  else {
    LogStream errStr;
    errStr.initMessage(true);
    errStr << "UT1-UTC ephemeris can't bracket mjd = " << mjdUtc << endl;
    throw Error(errStr);
  }
}

/**.......................................................................
 * Get the value of the equation of the equinoxes for a
 * given terrestrial time.
 *
 * @throws Exception
 */
double Astrometry::getEqnEqx(double mjdTt)
{
  if(eqnEqx_->canBracket(mjdTt))
    return eqnEqx_->evaluate(mjdTt);
  else {
    LogStream errStr;
    errStr.initMessage(true);
    errStr << "EqnEqx ephemeris can't bracket mjd = " << mjdTt << endl;
    throw Error(errStr);
  }
}

/**.......................................................................
 * Return true if ephemeris parameters can be interpolated for
 * this timestamp
 */
bool Astrometry::canBracket(double mjdUtc)
{
  return ut1Utc_->canBracket(mjdUtc) && 
    eqnEqx_->canBracket(mjdUtcToMjdTt(mjdUtc));
}


/**......................................................................
 * Return the local sidereal time for a given site and UTC.
 *
 * Input:
 *  utc     double    The current date and time (UTC), expressed as a
 *                    Modified Julian Date.
 *  longitude double  The longitude at which the lst is desired
 *  ut1Utc   double    The current value of UT1-UTC. If you don't need
 *                    more than one second of accuracy, this can be
 *                    given as zero.
 *  eqnEqx  double    The current value of the equation of the
 *                    equinoxes if you want apparent sidereal time,
 *                    or 0 if you can make do with mean sidereal time.
 *                    The equation of the equinoxes is a slowly varying
 *                    number that is computationally intensive to calculate,
 *                    so it doesn't make sense to calculate it anew on
 *                    each call.
 * Output:
 *  return  double    The local sidereal time, expressed in radians.
 */
HourAngle Astrometry::mjdUtcToLst(double mjd, Angle longitude, double ut1Utc, 
				  double eqnEqx)
{
  HourAngle lst;     // The local sidereal time 
  LogStream errStr;

  // Check arguments.

  CHECK_MJD(mjd);

  // Determine the local apparent (mean if eqnEqx is 0) sidereal time.

  lst.setRadians(slaGmst(mjd + ut1Utc/secondsPerDay_) + eqnEqx + 
		 longitude.radians());

  // The addition of the longitude and the equation of the equinoxes
  // may have caused lst to go outside the range 0-2.pi. Correct this.

  if(lst.radians() < 0)
    lst.addRadians(twopi_);
  else if(lst.radians() > twopi_)
    lst.addRadians(-twopi_);

  return lst;
}

/**.......................................................................
 * Same as above, using internal ephemerides
 */
HourAngle Astrometry::mjdUtcToLst(double mjdUtc, Angle longitude)
{
  return mjdUtcToLst(mjdUtc, longitude, 
		     ut1Utc_->evaluate(mjdUtc),
		     eqnEqx_->evaluate(mjdUtcToMjdTt(mjdUtc)));
}

/**......................................................................
 * Return the Terestrial time (aka Ephemeris Time), corresponding to a
 * given UTC (expressed as a Modified Julian date).
 *
 * Input:
 *
 *  mjd      double   The Modified Julian date.
 *
 * Output:
 *
 *  return   double   The corresponding Terrestrial Time
 */
double Astrometry::mjdUtcToMjdTt(double mjdUtc)
{
  CHECK_MJD(mjdUtc);

  return mjdUtc + slaDtt(mjdUtc) / secondsPerDay_;
}

/**.......................................................................
 * Convert from apparent place to mean place
 */
void Astrometry::apparentToMeanPlace(HourAngle& apparentRa, DecAngle& apparentDec, TimeVal& date, 
				     double equinox,
				     HourAngle& meanRa, DecAngle& meanDec)
{
  double rm, dm;
  slaAmp(apparentRa.radians(), apparentDec.radians(), date.getMjd(), equinox, &rm, &dm);

  meanRa.setRadians(rm);
  meanDec.setRadians(dm);
}

/**.......................................................................
 * Convert from apparent place to mean place
 */
void Astrometry::apparentToJ2000Place(HourAngle& apparentRa, DecAngle& apparentDec, TimeVal& date, 
				      HourAngle& meanRa, DecAngle& meanDec)
{
  apparentToMeanPlace(apparentRa, apparentDec, date, 2000.0, meanRa, meanDec);
}

/**.......................................................................
 * Convert from mean place to apparent place
 */
void Astrometry::meanToApparentPlace(HourAngle& meanRa, DecAngle& meanDec, TimeVal& date, 
				     double equinox,
				     HourAngle& apparentRa, DecAngle& apparentDec)
{
  double ra, da;

  slaMap(meanRa.radians(), meanDec.radians(), 0.0, 0.0, 
	 0.0, 0.0, equinox, date.getMjd(), 
	 &ra, &da);

  apparentRa.setRadians(ra);
  apparentDec.setRadians(da);
}

/**.......................................................................
 * Convert from J2000 place to apparent place
 */ 
void Astrometry::j2000ToApparentPlace(HourAngle& meanRa, DecAngle& meanDec, TimeVal& date, 
				      HourAngle& apparentRa, DecAngle& apparentDec)
{
  meanToApparentPlace(meanRa, meanDec, date, 2000.0, apparentRa, apparentDec);
}

void Astrometry::b1950ToJ2000(HourAngle& raB1950, DecAngle& decB1950, HourAngle& raJ2000, DecAngle& decJ2000)
{
  // Call slalib routine to convert from B1950.0 FK4 position,
  // determined at the specified Besselian epoch, to J2000.0 FK5
  // position, assuming zero proper motion and parallax.

  double ra, dec;
  double bepoch = 1950.0;

  slaFk45z(raB1950.radians(), decB1950.radians(), bepoch, &ra, &dec);

  raJ2000.setRadians(ra);
  decJ2000.setRadians(dec);
}

void Astrometry::j2000ToB1950(HourAngle& raJ2000, DecAngle& decJ2000, HourAngle& raB1950, DecAngle& decB1950)
{
  // Call slalib routine to convert from J2000.0 FK5 position to
  // B1950.0 FK4 position at the specified Besselian epoch, assuming
  // zero proper motion and parallax.

  double ra, dec;
  double dra, ddec;
  double bepoch = 1950.0;

  slaFk54z(raJ2000.radians(), decJ2000.radians(), bepoch, &ra, &dec, &dra, &ddec);

  raB1950.setRadians(ra);
  decB1950.setRadians(dec);
}

Angle Astrometry::angularSeparation(HourAngle ra1, DecAngle dec1, HourAngle ra2, DecAngle dec2)
{
  Angle sep;

  double cd1 = cos(dec1.radians());
  double sd1 = sin(dec1.radians());
  double cr1 = cos(ra1.radians());
  double sr1 = sin(ra1.radians());

  double cd2 = cos(dec2.radians());
  double sd2 = sin(dec2.radians());
  double cr2 = cos(ra2.radians());
  double sr2 = sin(ra2.radians());

  double arg = (sd1 * sd2) + (cd1 * sr1 * cd2 * sr2) + (cd1 * cr1 * cd2 * cr2);

  if(arg > 1.0)
    arg = 1.0;
  if(arg < -1.0)
    arg = -1.0;

  sep.setRadians(acos(arg));

  return sep;
}

Astrometry::Date Astrometry::mjdUtcToCalendarDate(double mjdUtc)
{
  double frc;     /* The unused fraction of a day returned by slaDjcl() */
  double integer; /* The integral part of a number */
  int status;     /* The status return value of slaDjcl() */

  Date date;

  // Perform the conversion.

  slaDjcl(mjdUtc, &date.year_, &date.month_, &date.day_, &frc, &status);
  
  // Check for errors.

  switch(status) {
  case 0:        /* No error */
    break;
  case 1:        /* Invalid mjd */
    ThrowError("MJD before 4701BC March 1");
    break;
  };
  
  // Fill in the hours minutes and seconds fields.

  frc = modf(frc * 24, &integer);

  date.hour_ = (unsigned int)integer;

  frc = modf(frc * 60, &integer);

  date.min_ = (unsigned int)integer;

  frc = modf(frc * 60, &integer);

  date.sec_ = (unsigned int)integer;
  date.nsec_ = (unsigned int)(frc * 1000000000U);

  return date;
}
