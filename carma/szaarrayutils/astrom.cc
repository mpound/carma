#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <math.h>
#include <float.h>

#include "carma/szaarrayutils/lprintf.h"

#include "carma/szaarrayutils/szaconst.h"
#include "carma/szaarrayutils/slalib.h"
#include "carma/szaarrayutils/input.h"
#include "carma/szaarrayutils/output.h"
#include "carma/szaarrayutils/astrom.h"

#include "carma/szautil/String.h"

#ifdef _GPP
  namespace sza {
    namespace array {
#endif

/*.......................................................................
 * Return the current date and time (utc) in its Gregorian components.
 *
 * Input/Output:
 *  date          Date *  On output the UTC Gregorian date will be assigned
 *                        to *date.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Time not available.
 */
int current_date(Date *date)
{
  struct tm utc;       /* The current UTC */
  time_t tv_sec;       /* The number of seconds since the unix epoch */
  long tv_nsec;        /* The number of nanoseconds into the current second */
/*
 * Check arguments.
 */
  if(!date) {
    lprintf(stderr, "current_date: NULL argument.\n");
    return 1;
  };
/*
 * Get the current calendar time.
 */
#if defined(VXW)
  {                        /* Use posix realtime extensions where available */
    struct timespec ts;
    if(clock_gettime(CLOCK_REALTIME, &ts) == -1) {
      lprintf(stderr, "current_date: Time not available.\n");
      return 1;
    };
    tv_sec = ts.tv_sec;
    tv_nsec = ts.tv_nsec;
  };
#else
  {
    struct timeval tp;
    if(gettimeofday(&tp, NULL)) {
      lprintf(stderr, "current_date: Time not available.\n");
      return 1;
    };
    tv_sec = tp.tv_sec;
    tv_nsec = tp.tv_usec * 1000;
  };
#endif
/*
 * Convert calendar time to UTC.
 */
  if(!gmtime_r(&tv_sec, &utc)) {
    lprintf(stderr, "current_date: UTC not available.\n");
    return 1;
  };
  if(init_Date(date, utc.tm_year + 1900, utc.tm_mon + 1, utc.tm_mday,
	       utc.tm_hour, utc.tm_min, utc.tm_sec, tv_nsec))
    return 1;
  return 0;
}

/*.......................................................................
 * Return the current date and time in UTC as a Modified Julian Date.
 *
 * Output:
 *  return double  The UTC as a MJD, or -1 on error.
 */
double current_mjd_utc(void)
{
  Date date;  /* The current time and date in calendar components */
  if(current_date(&date))
    return -1.0;
  return date_to_mjd_utc(&date);
}

/*.......................................................................
 * Return the current Terrestrial Dynamic Time as a Modified Julian Date.
 *
 * Output:
 *  return double  The TDT as a MJD, or -1 on error.
 */
double current_mjd_tt(void)
{
  Date date;  /* The current time and date in calendar components */
  if(current_date(&date))
    return -1.0;
  return date_to_mjd_tt(&date);
}

/*.......................................................................
 * Convert a Gregorian date to a UTC MJD.
 *
 * Note that this function doesn't use the slalib slaCldj(), because
 * it isn't reentrant.
 *
 * Input:
 *  date   Date *  The Gregorian date to convert.
 * Output:
 *  return double  The UTC MJD equivalent of the input date, or -1 on
 *                 error.
 */
double date_to_mjd_utc(Date *date)
{
  long jd;      /* Julian Day Number */
  int y,m,d;    /* Local copies of date->year, date->month, date->day */
/*
 * Check arguments.
 */
  if(!date || date->year < -4713) {
    lprintf(stderr, "date_to_mjd_utc: Invalid date.\n");
    return -1.0;
  };
/*
 * Get local copies of the components of the calendar date.
 */
  y = date->year;
  m = date->month;
  d = date->day;
/*
 * Compute the Julian Day Number using equation 12.92-1 on page 604 of
 * the Explanatory Supplement to the Astronomical Almanac (1992).
 */
  jd = (1461L * (y + 4800L + (m - 14L) / 12L)) / 4L +
       (367L * (m - 2L - 12L * ((m - 14L) / 12L))) / 12L -
       (3L * ((y + 4900L + (m - 14L) / 12L) / 100L)) / 4L +
       d - 32075L;
/*
 * Convert to MJD and add in the time of day.
 */
  return (jd - 2400001) + date_to_time_of_day(date) / 24.0;
}

/*.......................................................................
 * Return the local sidereal time for a given site and date.
 *
 * Input:
 *  date      Date *  The Gregorian date.
 *  site      Site *  The site description object of the local site.
 *  ut1utc  double    The current value of UT1-UTC. If you don't need
 *                    more than one second of accuracy, this can be
 *                    given as zero.
 *  eqex    double    The current value of the equation of the
 *                    equinoxes if you want apparent sidereal time,
 *                    or 0 if you can make do with mean sidereal time.
 *                    The equation of the equinoxes is a slowly varying
 *                    number that is computationally intensive to calculate,
 *                    so it doesn't make sense to calculate it anew on
 *                    each call.
 * Output:
 *  return  double    The local sidereal time, expressed in radians.
 *                    On error -1 is returned.
 */
double date_to_lst(Date *date, Site *site, double ut1utc, double eqex)
{
  double mjd_utc; /* The current utc as a MJD */
/*
 * Check arguments.
 */
  if(!date || !site) {
    lprintf(stderr, "date_to_lst: NULL arguments.\n");
    return -1;
  };
/*
 * Convert the Gregorian date into a modified Julian date.
 */
  mjd_utc = date_to_mjd_utc(date);
  if(mjd_utc < 0)
    return -1;
  return mjd_utc_to_lst(mjd_utc, site, ut1utc, eqex);
}

/*.......................................................................
 * Given a Gregorian date, return the corresponding Terrestrial
 * (aka Ephemeris) time as a Modified Julian Date (JD - 2400000.5).
 *
 * Input:
 *  date       Date * The Gregorian date to convert.
 * Output:
 *  return   double   The requested time, or -1.0 on error.
 */
double date_to_mjd_tt(Date *date)
{
  double utc_mjd;
/*
 * Get the current UTC as a MJD.
 */
  utc_mjd = date_to_mjd_utc(date);
  if(utc_mjd < 0)
    return -1.0;
  return mjd_utc_to_mjd_tt(utc_mjd);
}

/*.......................................................................
 * Given a Gregorian date/time, return the time of day (utc) in floating
 * point hours.
 *
 * Input:
 *  date       Date * The date to query.
 * Output:
 *  return   double   The time of day, in hours (0.0 - 24.0), or -1
 *                    on error.
 */
double date_to_time_of_day(Date *date)
{
  return date->hour + (date->min + (date->sec + date->nsec*1e-9) / 60.0) / 60.0;
}

/*.......................................................................
 * Convert a UTC expressed as a MJD, to the equivalent Gregorian date.
 *
 * Input:
 *  utc    double   The UTC as a Modified Julian Date.
 * Input/Output:
 *  date     Date * On output the Gregorian date will have been assigned
 *                  to *date.
 * Output:
 *  return    int   0 - OK.
 *                  1 - Error.
 */
int mjd_utc_to_date(double utc, Date *date)
{
  double frc;     /* The unused fraction of a day returned by slaDjcl() */
  double integer; /* The integral part of a number */
  int status;     /* The status return value of slaDjcl() */
/*
 * Check arguments.
 */
  if(!date) {
    lprintf(stderr, "mjd_utc_to_date: NULL argument(s).\n");
    return 1;
  };
/*
 * Perform the conversion.
 */
  slaDjcl(utc, &date->year, &date->month, &date->day, &frc, &status);
/*
 * Check for errors.
 */
  switch(status) {
  case 0:        /* No error */
    break;
  case 1:        /* Invalid mjd */
    lprintf(stderr, "mjd_utc_to_date: MJD before 4701BC March 1.\n");
    return 1;
    break;
  };
/*
 * Fill in the hours minutes and seconds fields.
 */
  frc = modf(frc * 24, &integer);
  date->hour = (int)integer;
  frc = modf(frc * 60, &integer);
  date->min = (int)integer;
  frc = modf(frc * 60, &integer);
  date->sec = (int)integer;
  date->nsec = (int)(frc * 1000000000U);
  return 0;
}

/*.......................................................................
 * Convert from UTC Modified Julian Day number to calender date.
 *
 * Input:
 *  mjd        long    The UTC expressed as a Modified Julian Day number.
 * Input/Output:
 *  year        int *  The corresponding Gregorian year will be assigned
 *                     to *year (including century).
 *  month       int *  The calender month in the year (1-12).
 *  day         int *  The day of the month (1-31).
 * Output:
 *  return      int    0 - OK.
 *                     1 - Invalid mjd.
 */
int mjd_utc_to_ymd(long mjd, int *year, int *month, int *day)
{
  long jd = mjd + 2400001;   /* Convert to Julian Day Number */
  long a,b,c,d;              /* Intermediate variables */
/*
 * The following algorithm is taken from equation 12.92-2 in the
 * Explanatory Supplement to the Astronomical Almanac (1992), page 604.
 */
  a =  jd + 68569L;
  b =  (4 * a) / 146097L;
  a -= (146097L * b + 3L) / 4L;
  c =  (4000L * (a + 1L)) / 1461001L;
  a -= (1461L * c) / 4L - 31L;
  d =  (80L * a) / 2447L;
  if(day)
    *day = a - (2447L * d) / 80L;
  a =  d / 11L;
  if(month)
    *month = d + 2L - 12L * a;
  if(year)
    *year = 100L * (b-49L) + c + a;
  return 0;
}

/*.......................................................................
 * Convert from UTC Modified Julian Day number to calender gregorian year
 * plus the day-number within that year.
 *
 * Input:
 *  mjd        long    The UTC expressed as a Modified Julian Day number.
 * Input/Output:
 *  year        int *  The corresponding Gregorian year will be assigned
 *                     to *year (including century).
 *  dayno       int *  The day number within the year (1..365), will be
 *                     assigned to *dayno.
 * Output:
 *  return      int    0 - OK.
 *                     1 - Invalid mjd.
 */
int mjd_utc_to_year_day(long mjd, int *year, int *dayno)
{
  long jd = mjd + 2400001;   /* Convert to Julian Day Number */
  long a,b,c;                /* Intermediate variables */
/*
 * The following algorithm is adapted from equation 12.92-2 in the
 * Explanatory Supplement to the Astronomical Almanac (1992), page 604.
 */
  a =  jd + 68569L;
  b =  (4 * a) / 146097L;
  a -= (146097L * b + 3L) / 4L;
  c =  (4000L * (a + 1L)) / 1461001L;
  *year = 100L * (b-49L) + c + (80L * (a - (1461L * c) / 4L + 31L)) / 26917L;
  *dayno = mjd - mjd_of_year(*year) + 1;
  return 0;
}

/*.......................................................................
 * Return the Terestrial time (aka Ephemeris Time), corresponding to a
 * given UTC (expressed as a Modified Julian date).
 *
 * Input:
 *  utc      double   The Modified Julian date.
 * Output:
 *  return   double   The corresponding Terestrial Time, or
 *                    -1 if utc < 0.
 */
double mjd_utc_to_mjd_tt(double utc)
{
  if(utc < 0) {
    lprintf(stderr, "mjd_utc_to_mjd_tt: Illegal utc.\n");
    return -1;
  };
  return utc + slaDtt(utc) / daysec;
}

/*.......................................................................
 * Return the local sidereal time for a given site and UTC.
 *
 * Input:
 *  utc     double    The current date and time (UTC), expressed as a
 *                    Modified Julian Date.
 *  site      Site *  The site description object of the local site.
 *  ut1utc  double    The current value of UT1-UTC. If you don't need
 *                    more than one second of accuracy, this can be
 *                    given as zero.
 *  eqex    double    The current value of the equation of the
 *                    equinoxes if you want apparent sidereal time,
 *                    or 0 if you can make do with mean sidereal time.
 *                    The equation of the equinoxes is a slowly varying
 *                    number that is computationally intensive to calculate,
 *                    so it doesn't make sense to calculate it anew on
 *                    each call.
 * Output:
 *  return  double    The local sidereal time, expressed in radians.
 *                    On error -1 is returned.
 */
double mjd_utc_to_lst(double utc, Site *site, double ut1utc, double eqex)
{
  double lst;     /* The local sidereal time */
/*
 * Check arguments.
 */
  if(!site) {
    lprintf(stderr, "mjd_utc_to_lst: NULL argument.\n");
    return -1;
  };
  if(utc < 0) {
    lprintf(stderr, "mjd_utc_to_lst: Illegal utc.\n");
    return -1;
  };
/*
 * Determine the local apparent (mean if eqex is 0) sidereal time.
 */
  lst = slaGmst(utc + ut1utc/daysec) + eqex + site->longitude;
/*
 * The addition of the longitude and the equation of the equinoxes
 * may have caused lst to go outside the range 0-2.pi. Correct this.
 */
  if(lst < 0)
    lst += twopi;
  else if(lst > twopi)
    lst -= twopi;
  return lst;
}

/*.......................................................................
 * Return the time of day corresponding to a given UTC (expressed as
 * a Modified Julian Date.
 *
 * Input:
 *  utc      double   The Modified Julian date.
 * Output:
 *  return   double   The time of day in floating point hours, or
 *                    -1 if utc < 0.
 */
double mjd_utc_to_time_of_day(double utc)
{
  double dummy;
  if(utc < 0) {
    lprintf(stderr, "mjd_utc_to_time_of_day: Illegal utc.\n");
    return -1;
  };
  return modf(utc,&dummy) * 24.0;
}

/*.......................................................................
 * Create a new site specification object. The object should be initialized
 * with set_Site() or read_Site() before use.
 *
 * Output:
 *  return         Site *  The new object, or NULL on error.
 */
Site *new_Site(void)
{
  Site *site;  /* The object to be returned */
/*
 * Allocate the container.
 */
  site = (Site* )malloc(sizeof(Site));
  if(!site) {
    lprintf(stderr, "new_Site: Insufficient memory.\n");
    return NULL;
  };
/*
 * Before attempting any operation that might fail, initialize the
 * container at least up to the point at which it can safely be passed
 * to del_Site().
 */
  set_Site(site, 0.0, 0.0, 0.0);
  return site;
}

/*.......................................................................
 * Change the location recorded in a Site object.
 *
 * Input:
 *  site         Site *  The site object to change.
 *  longitude  double    The longitude of the site in radians (-pi..pi).
 *                       East is positive, West is negative.
 *  latitude   double    The latitude of the site in radians (-pi/2..pi/2).
 *  altitude   double    The height of the site in meters above sealevel.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error (the original site object remains
 *                                  unchanged).
 */
int set_Site(Site *site, double longitude, double latitude, double altitude)
{
/*
 * Check arguments.
 */
  if(!site) {
    lprintf(stderr, "set_Site: NULL argument.\n");
    return 1;
  };
  if(longitude < -pi || longitude > pi) {
    lprintf(stderr, "set_Site: Illegal longitude.\n");
    return 1;
  };
  if(latitude < -halfpi || latitude > halfpi) {
    lprintf(stderr, "set_Site: Illegal latitude.\n");
    return 1;
  };
/*
 * Record the new location.
 */
  site->altitude = altitude;
  site->longitude = longitude;
  site->latitude = latitude;
/*
 * Precompute trig terms. Since pi/2 can not be represented to infinite
 * precision, it is likely that cos(pi/2) will be slightly non-zero, and
 * perhaps negative. Since some algorithms assume that cos(latitude) will
 * always be positive, force it to be so.
 */
  site->sin_lat = sin(site->latitude);
  site->cos_lat = cos(site->latitude);
  if(site->cos_lat < 0.0 || fabs(site->latitude)==halfpi) {
    site->sin_lat = site->latitude > 0.0 ? 1.0 : -1.0;
    site->cos_lat = 0.0;
  };
/*
 * Compute the distance of the site from the center of the Earth and
 * the equatorial plane.
 */
  {
    double z;  /* The distance of the site from the equatorial plane */
    slaGeoc(site->latitude, site->altitude, &site->raxis, &z);
/*
 * Compute the distance between the site and the Earth's true center.
 */
    site->rcent = sqrt(z*z + site->raxis * site->raxis);
  };
/*
 * Compute the tangential rotational velocity of the Earth at the site.
 */
  site->velocity = twopi * site->raxis * au_to_m / daysec;
  return 0;
}

/*.......................................................................
 * Create a new site specification object from parameters in a site
 * specification file.
 *
 * Input:
 *  stream  InputStream *  The stream from which to read the site's
 *                         characteristics. This should contain:
 *                          <longitude> <latitude> <altitude>
 *                         Where longitude is an angle in degrees (0..360),
 *                         specified in decimal or sexagesimal.
 *                         Latitude is an angle in degrees (-90..90).
 *                         Altitude is the height above sea level, measured
 *                         in meters.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error (the original site object remains
 *                                    unchanged).
 */
int read_Site(Site *site, InputStream *stream)
{
  double longitude;   /* The longitude of the site (degrees, east +ve) */
  double latitude;    /* The latitude of the site (degrees) */
  double altitude;    /* The height of the site above sea level (m) */
/*
 * Check arguments.
 */
  if(!site || !stream) {
    lprintf(stderr, "read_Site: NULL argument(s).\n");
    return 1;
  };
/*
 * Read the configuration parameters from the input stream.
 */
  if(input_skip_white(stream, 1, 0))
    return 1;
  if(input_sexagesimal(stream, 0, &longitude) ||
     longitude < -180.0 || longitude > 180) {
    input_error(stream, 1, "A longitude (-180 -> 180 degrees) was expected.\n");
    return 1;
  };
  if(input_skip_white(stream, 1, 0))
    return 1;
  if(input_sexagesimal(stream, 0, &latitude) ||
     latitude < -90.0 || latitude > 90.0) {
    input_error(stream, 1, "A latitude (-90 -> 90 degrees) was expected.\n");
    return 1;
  };
  if(input_skip_white(stream, 1, 0))
    return 1;
  if(input_double(stream, 0, &altitude)) {
    input_error(stream, 1, "An altitude (meters above sea level) was expected.\n");
    return 1;
  };
/*
 * Record the new values.
 */
  return set_Site(site, longitude * dtor, latitude * dtor, altitude);
}

/*.......................................................................
 * Delete a site-specification object.
 *
 * Input:
 *  site   Site *  The object to be deleted.
 * Output:
 *  return Site *  The deleted object (always NULL).
 */
Site *del_Site(Site *site)
{
  if(site) {
    free(site);
  };
  return NULL;
}

/*.......................................................................
 * Given a Gregorian date, return the corresponding daynumber within the year.
 * Day numbers start at 1.
 *
 * Input:
 *  date     Date *   A Gregorian date.
 * Output:
 *  return    int     The day number (1-366) or -1 if 'date' is invalid.
 */
int day_of_year(Date *date)
{
  int isleap;   /* True if the specified year is a leap year */
  int dayno;    /* The day-number to be returned */
  int i;
/*
 * Check arguments.
 */
  if(!date) {
    lprintf(stderr, "day_of_year: NULL date argument.\n");
    return -1;
  };
/*
 * Does the specified date refer to a leap year?
 */
  isleap = is_leap_year(date->year);
/*
 * Count days per month up to the target month.
 */
  dayno = 0;
  for(i=1; i<date->month; i++)
    dayno += days_in_month(isleap, i);
/*
 * Add in the day of the month.
 */
  dayno += date->day;
  return dayno;
}

/*.......................................................................
 * Return the month and day of month corresponding to a given number of
 * days into the year.
 *
 * Input:
 *  dayno    int    The day number within the year (1-366).
 *  is_leap  int    True if the year is a leap year.
 * Input/Output:
 *  month    int *  On output, *month will contain the month of the year
 *                  (1-12).
 *  day      int *  On output, *day will contain the day of the month
 *                  (1-31).
 * Output:
 *  return   int    0 - OK.
 *                  1 - Error.
 */
int dayno_to_date(int dayno, int is_leap, int *month, int *day)
{
  int m;   /* The month being considered */
/*
 * Check the validity of the day number.
 */
  if(dayno < 1 || dayno > (is_leap ? 366:365)) {
    lprintf(stderr, "month_and_day: dayno (%d) out of range 1-%d\n",
	    dayno, is_leap ? 366:365);
    return 1;
  };
/*
 * Decrement dayno by the number of days in each successive month,
 * until there are fewer days left than in the month being considered.
 */
  for(m=1; m<=12; m++) {
    int ndays = days_in_month(is_leap, m);
    if(dayno <= ndays)
      break;
    dayno -= ndays;
  };
/*
 * Record the return values.
 */
  if(month)
    *month = m;
  if(day)
    *day = dayno;
  return 0;
}

/*.......................................................................
 * Return 1 if the specified year is a leap year, 0 otherwise.
 *
 * Input:
 *  year     int    The year, including century.
 * Output:
 *  return   int    0 - Not a leap year.
 *                  1 - 'year' is a leap year.
 */
int is_leap_year(int year)
{
  return (year%4 == 0 && year%100 != 0) || year%400 == 0;
}

/*.......................................................................
 * Return the number of days in a given month.
 *
 * Input:
 *  is_leap   int   True if the month is in a leap year (see is_leap_year()).
 *  month     int   The month to query (1-12).
 * Output:
 *  return    int   The number of days in the month, or -1 if 'month' is
 *                  out of range.
 */
int days_in_month(int is_leap, int month)
{
/*
 * Record the number of days per month for a normal year and a leap year.
 */
  static char daytab[2][12] = {
    {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
    {31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}
  };
/*
 * Check arguments.
 */
  if(month < 1 || month > 12) {
    lprintf(stderr, "days_in_month: Bad month.\n");
    return -1;
  };
  return daytab[is_leap?1:0][month-1];
}

/*.......................................................................
 * Return the name of a given month.
 *
 * Input:
 *  month      int    The month of the year to be named (1-12).
 *  upper_case int    If non-zero return month names in capital letters.
 *  abbreviate int    If non-zero return three-letter month names.
 * Output:
 *  return    char *  The name of the month or NULL on error.
 */
const char *name_of_month(int month, int upper_case, int abbreviate)
{
  static const char *capitalized_names[] = {
    "January", "February", "March",     "April",   "May",      "June",
    "July",    "August",   "September", "October", "November", "December"
  };
  static const char *upper_case_names[] = {
    "JANUARY", "FEBRUARY", "MARCH",     "APRIL",   "MAY",      "JUNE",
    "JULY",    "AUGUST",   "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER"
  };
  static const char *capitalized_short[] = {
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  };
  static const char *upper_case_short[] = {
    "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
    "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
  };
/*
 * Check arguments.
 */
  if(month < 1 || month > 12) {
    lprintf(stderr, "name_of_month: Bad month.\n");
    return NULL;
  };
  if(abbreviate)
    return upper_case ? upper_case_short[month-1] : capitalized_short[month-1];
  else
    return upper_case ? upper_case_names[month-1] : capitalized_names[month-1];
}

/*.......................................................................
 * Fill a Date structure from Gregorian components.
 *
 * Input:
 *  date    Date *   The object to be filled.
 *  year     int     The year (including century).
 *  month    int     The month of the year (1-12).
 *  day      int     The day of the month (1-31).
 *  hour     int     The number of hours into the day (0-23).
 *  min      int     The number of minutes into the hour (0-59).
 *  sec      int     The number of seconds into the hour (0-60).
 *  nsec     int     The number of nano-seconds into the second (0-1000000000).
 * Output:
 *  return   int     0 - OK.
 *                   1 - Error.
 */
int init_Date(Date *date, int year, int month, int day, int hour, int min,
	      int sec, int nsec)
{
/*
 * Check inputs.
 */
  if(!date) {
    lprintf(stderr, "init_Date: NULL arguments.\n");
    return 1;
  };
  if(month < 1 || month > 12) {
    lprintf(stderr, "init_Date: Month (%d) out of range 1-12.\n", month);
    return 1;
  };
  if(day < 1 || day > days_in_month(is_leap_year(year), month)) {
    lprintf(stderr, "init_Date: Day (%d) out of expected range 0-%d.\n", day,
	    days_in_month(is_leap_year(year), month));
    return 1;
  };
  if(hour < 0 || hour > 23) {
    lprintf(stderr, "init_Date: Hour (%d) out of range 0-23.\n", hour);
    return 1;
  };
  if(min < 0 || min > 59) {
    lprintf(stderr, "init_Date: Minute (%d) out of range 0-59.\n", min);
    return 1;
  };
  if(sec < 0 || sec > 60) {
    lprintf(stderr, "init_Date: Second (%d) out of range 0-60.\n", sec);
    return 1;
  };
  if(nsec < 0 || nsec > 1000000000) {
    lprintf(stderr, "init_Date: nsec (%d) out of range 0-1e9.\n", sec);
    return 1;
  };
/*
 * Fill in the container.
 */
  date->nsec = nsec;
  date->sec = sec;
  date->min = min;
  date->hour = hour;
  date->day = day;
  date->month = month;
  date->year = year;
  return 0;
}

/*.......................................................................
 * Return the visibility of a given parallel of declination with
 * reference to a horizon that is defined by a geodetic latitude and
 * elevation.
 *
 * Input:
 *  site      Site *  The location of the observer.
 *  el      double    The elevation of the local horizon (radians).
 *  dec     double    The topocentric apparent Declination to characterize
 *                     (radians).
 * Input/Output:
 *  ha      double *  If the declination circle crosses the horizon
 *                    then *ha will contain the magnitude of the hour
 *                    angle at which sources at that declination would
 *                    cross the horizon. Thus a source of Right
 *                    Ascension ra, and declination dec, would rise
 *                    at sidereal time ra-ha and set at ra+ha.
 * Output:
 *  return DecSpan    The visibility of the parallel of declination, from:
 *                      DEC_ABOVE_HORIZON - The parallel of
 *                        declination is entirely above the horizon. 
 *                      DEC_BELOW_HORIZON - The parallel of
 *                        declination is entirely below the horizon.
 *                      DEC_SPANS_HORIZON - The parallel of
 *                        declination is partly above and partly below
 *                        the horizon. The crossing points are
 *                        returned in *ha.
 */
DecSpan dec_visibility(Site *site, double el, double dec, double *ha)
{
/*
 * The equation that gives the hour angle at which the parallel of
 * declination crosses the horizon is:
 *
 *             sin(el) - sin(dec) * sin(lat)
 *   cos(ha) = -----------------------------
 *                 cos(dec) * cos(lat)      
 *
 * Compute the numerator (top) and denominator (bot) of the right hand
 * side.
 */
  double cos_dec = cos(dec);
  double top = sin(el) - sin(dec) * site->sin_lat;
  double bot = cos_dec * site->cos_lat;
/*
 * If either the declination or the site-latitude is +/- 90 degrees
 * then top/bot is inifinite. In such cases the declination circle
 * never crosses the horizon, and the unknown sign of the infinity
 * determines its visibility. Since we don't know the sign of the
 * result, we have to determine the visibility by considering the
 * geometry of each case.
 */
  if(bot == 0.0) {
/*
 * Is the site at one of the Earth's poles?
 */
    if(fabs(site->cos_lat) < fabs(cos_dec)) {
      if(site->latitude > 0)                   /* North pole site */
	return dec > el ? DEC_ABOVE_HORIZON : DEC_BELOW_HORIZON;
      else                                     /* South pole site */
	return -dec > el ? DEC_ABOVE_HORIZON : DEC_BELOW_HORIZON;
/*
 * Or is the source above one of the Earth's poles?
 */
    } else {
      if(dec > 0)                              /* North pole source */
	return site->latitude > el ? DEC_ABOVE_HORIZON : DEC_BELOW_HORIZON;
      else                                     /* South pole source */
	return -site->latitude > el ? DEC_ABOVE_HORIZON : DEC_BELOW_HORIZON;
    };
/*
 * The denominator is non-zero so we can safely compute cos(ha).
 */
  } else {
    double cos_ha = top/bot;
/*
 * If the parallel of declination doesn't cross the horizon then
 * the magnitude of cos_ha > 1 and the source must either always
 * be visible or never be visible.
 */
    if(fabs(cos_ha) > 1.0) {
/*
 * To work out whether the source is always visible or never visible
 * at the current latitude, work out the elevation of a hipothetical
 * source at hour angle 0. 
 */
      if(asin(cos(dec - site->latitude)) > el)
	return DEC_ABOVE_HORIZON;
      else
	return DEC_BELOW_HORIZON;
/*
 * If the magnitude of cos_ha <= 1.0 then the declination circle does
 * cross the horizon, so acos(cos_ha) returns the magnitude of the two
 * hour angles at which a source of declination 'dec' would cross the
 * horizon.
 */
    } else {
      *ha = acos(cos_ha);
      return DEC_SPANS_HORIZON;
    };
  };
}

/*.......................................................................
 * Display a date and time to an ASCII output stream.
 *
 * Input:
 *  stream    OutputStream *  The stream to write to.
 *  flags             char *  This is should be a string consisting of a
 *                            concatenation of zero or more of the
 *                            following printf()-style flags:
 *                             '-'  -  Left justify the string within
 *                                     the specified field width.
 *                             ':'  -  Separate the year and hour fields
 *                                     by a colon instead of a space.
 *  width              int    The minimum field width. If the output
 *                            string takes fewer characters it will be
 *                            padded with leading spaces by default,
 *                            or by trailing spaces if the '-' flag is present.
 *  precision          int    The number of figures after the decimal point
 *                            in the seconds field, or:
 *                             -1 - Suppress the seconds field entirely.
 *                             -2 - Suppress the minutes and seconds fields.
 *                             -3 - Don't display the time of day.
 *  utc             double    The UTC to be rendered, expressed as a Modified
 *                            Julian Date.
 * Output:
 *  return             int    0 - OK.
 *                            1 - Error.
 */
int output_utc(OutputStream *stream, char *flags, int width, int precision,
	       double utc)
{
  Date date;                 /* The UTC split into Gregorian components */
  char secs[DBL_DIG+6];      /* String with room for a maximum precision */
                             /*  floating point number + a bit */
  double sec;                /* Decimal seconds */
  char *cptr;                /* A pointer into flags[] */
  int left_adjust = 0;       /* Left adjustment requested */
  int yyyy_hh_separator=' '; /* The character used to separate the year field */
                             /*  from the hours field. */
  int show_seconds = 1;      /* Don't display seconds unless true */
  int show_minutes = 1;      /* Don't display minutes or seconds unless true */
  int show_hours = 1;        /* Don't display the time of day unless true */
  size_t slen;               /* The length of the string in stream->work[] */
/*
 * Check arguments.
 */
  if(!stream || !flags) {
    lprintf(stderr, "output_date_and_time: NULL argument(s).\n");
    return 1;
  };
/*
 * Impose limits on the length constraints.
 */
  if(width < 1)
    width = 1;
/*
 * Negative precisions indicate that the caller wants to suppress
 * selected trailing time-of-day fields. Note the deliberate omission
 * of break statements between switch cases.
 */
  if(precision < 0) {
    switch(precision) {
    default:
    case -3:
      show_hours = 0;
    case -2:
      show_minutes = 0;
    case -1:
      show_seconds = 0;
    };
    precision = 0;
/*
 * Restrict the precision to what can be represented (and to what will
 * fit in the secs[] buffer).
 */
  } else if(precision > DBL_DIG+1) {
    precision = DBL_DIG+1;
  };
/*
 * Decompose the flags string.
 */
  for(cptr=flags; *cptr; cptr++) {
    switch(*cptr) {
    case '-':
      left_adjust = 1;
      break;
    case ':':
      yyyy_hh_separator = ':';
      break;
    };
  };
/*
 * Split the utc into Gregorian components.
 */
  if(mjd_utc_to_date(utc, &date))
    return 1;
/*
 * Combine the integer seconds and nano-seconds values to yield floating
 * point seconds.
 */
  sec = date.sec + date.nsec / 1.0e9;
/*
 * Render the seconds part with the requested precision.
 */
  if(precision > 0)
    sprintf(secs, "%#0*.*f", precision+3, precision, sec);
  else
    sprintf(secs, "%02.0f", sec);
/*
 * If the rounded number is 60 then replace the seconds output by 0.0 and
 * round up to the next highest minute.
 */
  if(secs[0] == '6' && secs[1] == '0') {
    sec = 0.0;
    if(precision > 0)
      sprintf(secs, "%#0*.*f", precision+3, precision, sec);
    else
      sprintf(secs, "%02.0f", sec);
/*
 * Carry to the next minute.
 */
    if(++date.min == 60) {
      date.min = 0;
      if(++date.hour == 24) {
	date.hour = 0;
	if(++date.day > days_in_month(is_leap_year(date.year), date.month)) {
	  date.day = 1;
	  if(++date.month > 12) {
	    date.month = 1;
	    date.year++;
	  };
	};
      };
    };
  };
/*
 * Render the date in the stream work buffer.
 */
  sprintf(stream->work, "%02d-%.3s-%04d", date.day,
	  name_of_month(date.month, 1, 1), date.year);
/*
 * Determine the location of the end of the date string.
 */
  slen = strlen(stream->work);
/*
 * Display the hours field?
 */
  if(show_hours) {
    sprintf(stream->work+slen, "%c%02d", yyyy_hh_separator, date.hour);
    slen += strlen(stream->work+slen);
  };
/*
 * Display the minutes field?
 */
  if(show_minutes) {
    sprintf(stream->work+slen, ":%02d", date.min);
    slen += strlen(stream->work+slen);
  };
/*
 * Display the seconds field?
 */
  if(show_seconds) {
    sprintf(stream->work+slen, ":%s", secs);
    slen += strlen(stream->work+slen);
  };
/*
 * Determine the number of padding characters that will need to be added
 * to reach the requested width.
 */
  width -= slen;
  if(width < 0)
    width = 0;
/*
 * If right-adjustment was requested pad up to the field width with
 * spaces.
 */
  if(!left_adjust && width > 0 && output_spaces(stream, width))
    return 1;
/*
 * Output the date string.
 */
  if(write_OutputStream(stream, stream->work))
    return 1;
/*
 * If left adjustment was requested, pad up to the field width with spaces.
 */
  if(left_adjust && width > 0 && output_spaces(stream, width))
    return 1;
  return 0;
}

/*.......................................................................
 * Display a date and time to an ASCII output stream.
 *
 * Input:
 *  stream    OutputStream *  The stream to write to.
 *  flags             char *  This is should be a string consisting of a
 *                            concatenation of zero or more of the
 *                            following printf()-style flags:
 *                             '-'  -  Left justify the string within
 *                                     the specified field width.
 *                             ':'  -  Separate the year and hour fields
 *                                     by a colon instead of a space.
 *  width              int    The minimum field width. If the output
 *                            string takes fewer characters it will be
 *                            padded with leading spaces by default,
 *                            or by trailing spaces if the '-' flag is present.
 *  precision          int    The number of figures after the decimal point
 *                            in the seconds field, or:
 *                             -1 - Suppress the seconds field entirely.
 *                             -2 - Suppress the minutes and seconds fields.
 *                             -3 - Don't display the time of day.
 *  utc             double    The UTC to be rendered, expressed as a Modified
 *                            Julian Date.
 * Output:
 *  return             int    0 - OK.
 *                            1 - Error.
 */
int outputCarmaUtc(OutputStream *stream, char *flags, int width, int precision,
		   double utc)
{
  Date date;                 /* The UTC split into Gregorian components */
  char secs[DBL_DIG+6];      /* String with room for a maximum precision */
                             /*  floating point number + a bit */
  double sec;                /* Decimal seconds */
  char *cptr;                /* A pointer into flags[] */
  int left_adjust = 0;       /* Left adjustment requested */
  int yyyy_hh_separator=' '; /* The character used to separate the year field */
                             /*  from the hours field. */
  int show_seconds = 1;      /* Don't display seconds unless true */
  int show_minutes = 1;      /* Don't display minutes or seconds unless true */
  int show_hours = 1;        /* Don't display the time of day unless true */
  size_t slen;               /* The length of the string in stream->work[] */
/*
 * Check arguments.
 */
  if(!stream || !flags) {
    lprintf(stderr, "output_date_and_time: NULL argument(s).\n");
    return 1;
  };
/*
 * Impose limits on the length constraints.
 */
  if(width < 1)
    width = 1;
/*
 * Negative precisions indicate that the caller wants to suppress
 * selected trailing time-of-day fields. Note the deliberate omission
 * of break statements between switch cases.
 */
  if(precision < 0) {
    switch(precision) {
    default:
    case -3:
      show_hours = 0;
    case -2:
      show_minutes = 0;
    case -1:
      show_seconds = 0;
    };
    precision = 0;
/*
 * Restrict the precision to what can be represented (and to what will
 * fit in the secs[] buffer).
 */
  } else if(precision > DBL_DIG+1) {
    precision = DBL_DIG+1;
  };
/*
 * Decompose the flags string.
 */
  for(cptr=flags; *cptr; cptr++) {
    switch(*cptr) {
    case '-':
      left_adjust = 1;
      break;
    case ':':
      yyyy_hh_separator = ':';
      break;
    };
  };
/*
 * Split the utc into Gregorian components.
 */
  if(mjd_utc_to_date(utc, &date))
    return 1;
/*
 * Combine the integer seconds and nano-seconds values to yield floating
 * point seconds.
 */
  sec = date.sec + date.nsec / 1.0e9;
/*
 * Render the seconds part with the requested precision.
 */
  if(precision > 0)
    sprintf(secs, "%#0*.*f", precision+3, precision, sec);
  else
    sprintf(secs, "%02.0f", sec);
/*
 * If the rounded number is 60 then replace the seconds output by 0.0 and
 * round up to the next highest minute.
 */
  if(secs[0] == '6' && secs[1] == '0') {
    sec = 0.0;
    if(precision > 0)
      sprintf(secs, "%#0*.*f", precision+3, precision, sec);
    else
      sprintf(secs, "%02.0f", sec);
/*
 * Carry to the next minute.
 */
    if(++date.min == 60) {
      date.min = 0;
      if(++date.hour == 24) {
	date.hour = 0;
	if(++date.day > days_in_month(is_leap_year(date.year), date.month)) {
	  date.day = 1;
	  if(++date.month > 12) {
	    date.month = 1;
	    date.year++;
	  };
	};
      };
    };
  };
/*
 * Render the date in the stream work buffer.
 */
  std::string monthName = sza::util::String::capitalized(name_of_month(date.month, 1, 1));

  sprintf(stream->work, "%04d-%.3s-%02d", date.year,
	  monthName.c_str(), date.day);
/*
 * Determine the location of the end of the date string.
 */
  slen = strlen(stream->work);
/*
 * Display the hours field?
 */
  if(show_hours) {
    sprintf(stream->work+slen, "%c%02d", yyyy_hh_separator, date.hour);
    slen += strlen(stream->work+slen);
  };
/*
 * Display the minutes field?
 */
  if(show_minutes) {
    sprintf(stream->work+slen, ":%02d", date.min);
    slen += strlen(stream->work+slen);
  };
/*
 * Display the seconds field?
 */
  if(show_seconds) {
    sprintf(stream->work+slen, ":%s", secs);
    slen += strlen(stream->work+slen);
  };
/*
 * Determine the number of padding characters that will need to be added
 * to reach the requested width.
 */
  width -= slen;
  if(width < 0)
    width = 0;
/*
 * If right-adjustment was requested pad up to the field width with
 * spaces.
 */
  if(!left_adjust && width > 0 && output_spaces(stream, width))
    return 1;
/*
 * Output the date string.
 */
  if(write_OutputStream(stream, stream->work))
    return 1;
/*
 * If left adjustment was requested, pad up to the field width with spaces.
 */
  if(left_adjust && width > 0 && output_spaces(stream, width))
    return 1;
  return 0;
}

/*.......................................................................
 * Read a UTC date from an ASCII input stream and return it as
 * a Modified Julian Date. The expected format is as described for
 * output_utc() except that the number of digits in each of the
 * numeric fields can be less than the number shown there. Also trailing
 * fields after the year field can be omitted.
 *
 * Input:
 *  stream   InputStream *  The stream to read from.
 *  tell             int    Non-zero to report errors to stderr.
 *  nospace          int    By default, the year field can be separated
 *                          from the hour field by either a colon or a space.
 *                          To disallow a space as a separator, set nospace
 *                          to non-zero.
 * Input/Output:
 *  utc           double *  The MJD UTC that was read.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
int input_utc(InputStream *stream, int tell, int nospace, double *utc)
{
  int day,month,year;   /* The date-fields decoded by input_date_and_time() */
  int hour,min;         /* The decoded hour and minute values */
  double sec;           /* The decoded seconds value */
  Date date;            /* The current date and time */
  double s_int, s_frc;  /* The integral and fractional seconds */
/*
 * Check the arguments.
 */
  if(!stream || !utc) {
    lprintf(stderr, "input_utc: NULL argument(s).\n");
    return 1;
  };
/*
 * Read the date fields.
 */
  if(input_date_and_time(stream, tell, nospace, &year, &month, &day, &hour,
			 &min, &sec))
    return 1;
/*
 * Split the seconds field into integral and fractional parts.
 */
  s_frc = modf(sec, &s_int);
/*
 * Collect the date and time information in a Date object.
 */
  if(init_Date(&date, year, month, day, hour, min, (int)s_int,
	       (int)(s_frc*1e9)))
    return 1;
/*
 * Convert the date and time to a Modified Julian Date.
 */
  *utc = date_to_mjd_utc(&date);
  return 0;
}

/*.......................................................................
 * Return the Modified Julian Day number of the first day of a given
 * year. The algorithm is derived from equation 12.92-1 in the
 * "Explanatory Supplement to the Astronomical Almanac".
 *
 * Input:
 *  year     int   The Gregorian calendar year to return an MJD for.
 *                 The result is valid for year >= -4712.
 * Output:
 *  return  long   The requested Modified Julian Day number.
 */
long mjd_of_year(int year)
{
  return (1461L * (year + 4799L))/4 - (3*((year+4899L)/100))/4 - 2431739L;
}

#ifdef _GPP // End namespace construction for C++
  }
}
#endif
