//$Id: AstroTime.cc,v 1.35 2012/06/15 11:49:25 teuben Exp $

#include "carma/services/Units.h"
#include "carma/services/AstroTime.h"
#include "carma/services/Angle.h"
#include "carma/services/HourAngle.h"
#include "carma/services/Length.h"
#include "carma/services/novasWrapper.h"
#include "carma/util/Time.h"
#include "carma/util/StringUtils.h"

#include <cmath>
#include <iostream>

using namespace std;
using namespace carma::util;
using namespace carma::services;

// conversion factor from Astronomical Almanac
const double AstroTime::SOLAR_DAY = 0.99726956633;
// this is just 1/SOLAR_DAY
const double AstroTime::SIDEREAL_DAY = 1.00273790935;

const double AstroTime::SECONDS_PER_HOUR  =  3600.0;
const double AstroTime::SECONDS_PER_DAY   = 86400.0;
const double AstroTime::MINUTES_PER_DAY   =  1440.0;
const double AstroTime::MINUTES_PER_HOUR  =    60.0;
const double AstroTime::HOURS_PER_DAY     =    24.0;
const double AstroTime::HOURS_PER_RADIAN  =    12.0/M_PI;
const double AstroTime::HOURS_PER_SIDEREAL_DAY = 
		      HOURS_PER_DAY*SIDEREAL_DAY;
const double AstroTime::JULIAN_DAY_ZERO = 2400000.5;
const double AstroTime::JULIAN_CENTURY = 36525.0;

// constructors
AstroTime::AstroTime() :
lastIERSLoadTime_( Time::MJD() )
{ }

AstroTime::AstroTime(Location location) : 
siteLocation_(location),
lastIERSLoadTime_( Time::MJD() )
{ 
}

// destructor
// Why does deleting this cause a segfault?
AstroTime::~AstroTime() { }

void AstroTime::setSite(Angle longitude, Angle latitude, Length altitude) 
{
    
    siteLocation_.setLongitude(longitude);
    siteLocation_.setLatitude(latitude);
    siteLocation_.setAltitude(altitude);
}

void AstroTime::setSite(double longitude, double latitude, double altitude) 
{
      setSite(
	      Location(
		  Angle(longitude,"radians"),
		  Angle(latitude,"radians"),
		  Length(altitude,"meters")
		  )
	     );
}

void AstroTime::setSite(Location location) 
{ 
    siteLocation_ = location; 
}

Location AstroTime::getSite() const 
{
    return siteLocation_;
}

double AstroTime::localSiderealTime(double mjd) 
{
    double gst; // output from sidereal_time

    // calculate the equation of the equinoxes
    double eq = eqnEqx(mjd);

    // novas expects JD, and we want to pass in mjd
    double jd = julianDay(mjd);

    // need to further correct jd to be in UT1 (instead of UTC)
    novas::sidereal_time(jd+ut1Utc(mjd)/SECONDS_PER_DAY, 0.0, eq, &gst);

    // need to adjust GST to LST
    double lst = gst +
	    HourAngle(siteLocation_.getLongitude().getValue(),
		      siteLocation_.getLongitude().getUnits()).hours();

    // add 24.0 if negative since HourAngle goes from -12 to 12
    return modulo24(lst);
}

double AstroTime::meanSiderealTime(double mjd) 
{
    double gst;

    // novas expects JD, and we want to pass in mjd
    double jd = julianDay(mjd);
    // need to further correct jd to be in UT1 (instead of UTC)
    novas::sidereal_time(jd+ut1Utc(mjd)/SECONDS_PER_DAY, 0.0, 0.0, &gst);

    // need to adjust GST to LST
    double mst = ( gst + 
	    HourAngle(siteLocation_.getLongitude().getValue(),
		      siteLocation_.getLongitude().getUnits()).hours());

    return modulo24(mst);
  }

std::string AstroTime::lstString(double mjd, int precision)
{
    double lstHours = localSiderealTime(mjd);
    return StringUtils::hms(lstHours*M_PI/12, precision);
}

double AstroTime::eqnEqx(double mjd) 
{
    // output values from earthtilt for a given TDB Julian date
    double mobl; // mean obliquity of ecliptic in degrees
    double tobl; // true obliquity of ecliptic in degrees
    double eq;   // equation of equinoxes in seconds of time
    double dpsi; // nutation in longitude in arcsec
    double deps; // nutation in obliquity in arcsec

    double tjd;
    tjd = mjd 
      + AstroTime::JULIAN_DAY_ZERO 
      + (leap(mjd) + 32.184)/(SECONDS_PER_DAY); 

    novas::earthtilt(tjd, &mobl, &tobl, &eq, &dpsi, &deps);
    
    return eq;
}

// It is possible, though rare, for the IERS table 
// to become out of date.  If someone has fixed this by 
// putting a new IERStable in place, we want to reload it.
// However, we do NOT want to check IERSTable.isOutOfDate() 
// every time and force a disk read if true, since we don't
// know when someone will update it and could wind up doing
// many unnecessary disk reads in the meantime.  Therefore reload it 
// every 12 hours, so it will never be out of date for longer
// than 12 hours, which is quite acceptable for calculating ut1-utc.

void 
AstroTime::checkIERSandLoadIfNecessary() {
    double now = Time::MJD();
    double daysOld = now - lastIERSLoadTime_;
    if ( daysOld > 0.5 ) loadIERS();
}

double AstroTime::ut1Utc(double mjd) 
{
    checkIERSandLoadIfNecessary();
    return iers_.dut1(mjd);
}

double 
AstroTime::iersTableAge() 
{
    return iers_.age();
}

double AstroTime::xPolar(double mjd) 
{
    checkIERSandLoadIfNecessary();
    return iers_.xpolar(mjd);
}

double AstroTime::yPolar(double mjd) 
{
    checkIERSandLoadIfNecessary();
    return iers_.ypolar(mjd);
}

double AstroTime::hourAngle( double mjd, double RA ) 
{
    double lst = localSiderealTime(mjd)*M_PI/12.0; // radians
    double haValue = (lst - RA); // radians
    if ( haValue > M_PI ) haValue -= 2.0*M_PI;
    if ( haValue < -M_PI ) haValue += 2.0*M_PI;
    return haValue;
    // use HourAngle object to force hour angle to be 
    // between -12 and 12 hours (-PI and PI)
    //return HourAngle(haValue,"radians").radians();
}

HourAngle AstroTime::hourAngle(double mjd, Angle RA)
{
    // value in radians
    double HA = hourAngle(mjd,RA.radians());
    return HourAngle(HA,"radians");
}

double AstroTime::leap(const double mjd) 
{
    double jd = julianDay(mjd);

    // following list taken from ftp://maia.usno.navy.mil/ser7/tai-utc.dat
    // Should really do this with intelligent tables, so
    // that we don't have to recompile when a new leap second
    // is issued (which may be never happen again
    // if the precision timing community has its way).
    // Also recall the leapsecond is buried in conf/catalogs/IERS.tab
    // 1999 JAN  1 =JD 2451179.5  TAI-UTC=  32.0       S + (MJD - 41317.) X 0.0      S
    // 0.........1.........2.........3.........4.........5
    // Table t('tai-utc.dat');
    // vector<double> jd = t.getDoubleColumn(16,24);
    // vector<double> ls = t.getDoubleColumn(36,40);
    if (jd > 2456109.5)  return 35.0;      //  2012 JUL  1
    if (jd > 2454832.5)  return 34.0;      //  2009 JAN  1
    if (jd > 2453736.5)  return 33.0;      //  2006 JAN  1
    if (jd > 2451179.5)  return 32.0;      //  1999 JAN  1
    if (jd > 2450630.5)  return 31.0;      //  1997 JUL  1
    if (jd > 2450083.5)  return 30.0;      //  1996 JAN  1
    if (jd > 2449534.5)  return 29.0;      //  1994 JUL  1
    if (jd > 2449169.5)  return 28.0;      //  1993 JUL  1
    if (jd > 2448804.5)  return 27.0;      //  1992 JUL  1
    if (jd > 2448257.5)  return 26.0;      //  1991 JAN  1
    if (jd > 2447892.5)  return 25.0;      //  1990 JAN  1
    if (jd > 2447161.5)  return 24.0;      //  1988 JAN  1
    if (jd > 2446247.5)  return 23.0;      //  1985 JUL  1
    if (jd > 2445516.5)  return 22.0;      //  1983 JUL  1
    if (jd > 2445151.5)  return 21.0;      //  1982 JUL  1
    if (jd > 2444786.5)  return 20.0;      //  1981 JUL  1
    if (jd > 2444239.5)  return 19.0;      //  1980 JAN  1
    if (jd > 2443874.5)  return 18.0;      //  1979 JAN  1
    if (jd > 2443509.5)  return 17.0;      //  1978 JAN  1
    if (jd > 2443144.5)  return 16.0;      //  1977 JAN  1
    if (jd > 2442778.5)  return 15.0;      //  1976 JAN  1
    if (jd > 2442413.5)  return 14.0;      //  1975 JAN  1
    if (jd > 2442048.5)  return 13.0;      //  1974 JAN  1
    if (jd > 2441683.5)  return 12.0;      //  1973 JAN  1
    if (jd > 2441499.5)  return 11.0;      //  1972 JUL  1
    if (jd > 2441317.5)  return 10.0;      //  1972 JAN  1
    if (jd > 2439887.5)  return 4.2131700; //  1968 FEB  1
    if (jd > 2439126.5)  return 4.3131700; //  1966 JAN  1
    if (jd > 2439004.5)  return 3.8401300; //  1965 SEP  1
    if (jd > 2438942.5)  return 3.7401300; //  1965 JUL  1
    if (jd > 2438820.5)  return 3.6401300; //  1965 MAR  1
    if (jd > 2438761.5)  return 3.5401300; //  1965 JAN  1
    if (jd > 2438639.5)  return 3.4401300; //  1964 SEP  1
    if (jd > 2438486.5)  return 3.3401300; //  1964 APR  1
    if (jd > 2438395.5)  return 3.2401300; //  1964 JAN  1
    if (jd > 2438334.5)  return 1.9458580; //  1963 NOV  1
    if (jd > 2437512.5)  return 1.3728180; //  1961 AUG  1
    if (jd > 2437665.5)  return 1.8458580; //  1962 JAN  1
    if (jd > 2437300.5)  return 1.4228180; //  1961 JAN  1
    return 0; // if it gets here, then it's before the inception
	      // of the leap second
}

double AstroTime::elapsedJulCent(double mjd) 
{
    double jd = julianDay(mjd);

    // Note half day difference between definition of
    // TU and definition of MJD. (12UT vs. 0 UT)
    double offset = (Time::JD2000)+0.5;
    double tu = ( jd - offset )/JULIAN_CENTURY;
    return tu;
}


double AstroTime::julianDay(double mjd) 
{
    return (mjd + JULIAN_DAY_ZERO);
}

void  AstroTime::loadIERS(void) 
{
    iers_.reload();
    lastIERSLoadTime_ = Time::MJD();
}

double AstroTime::modulo24(double hours)
{
      while ( hours < HOURS_PER_DAY )
	      hours += HOURS_PER_DAY;
      while ( hours > HOURS_PER_DAY )
	      hours -= HOURS_PER_DAY;
      return hours ;
}  

