#ifndef astrom_h
#define astrom_h

#include "carma/szaarrayutils/input.h"
#include "carma/szaarrayutils/output.h"

namespace sza {
  namespace array {
      
      typedef struct {
	double longitude;   /* The longitude of the site (radians, east +ve) */
	double latitude;    /* The latitude of the site (radians) */
	double altitude;    /* The height of the site above sea level (m) */
	double sin_lat;     /* sin(latitude) */
	double cos_lat;     /* cos(latitude) */
	double rcent;       /* The distance between the site and the
			       center of the Earth (AU) */
	double raxis;       /* The distance between the site and the
			       rotation axis of the Earth (AU) */
	double velocity;    /* The geocentric rotational velocity of
			       the site (m/s) */
      } Site;
      
      Site *new_Site(void);
      int set_Site(Site *site, double longitude, double latitude, 
		   double altitude);
      int read_Site(Site *site, InputStream *stream);
      Site *del_Site(Site *site);
      
      
      typedef struct {
	int nsec;    /* The number of nanoseconds after second (0-1000000000) */
	int sec;     /* The number of seconds after the minute (0-60) */
	int min;     /* The number of minutes after the hour (0-59) */
	int hour;    /* The number of hours since midnight (0-23) */
	int day;     /* The day of the month (1-31) */
	int month;   /* The month of the year (1-12) */
	int year;    /* The Gregorian year (includes the century) */
      } Date;
      
      int init_Date(Date *date, int year, int month, int day, int hour, int min,
		    int sec, int nsec);
      
      int current_date(Date *date);
      
      double date_to_mjd_utc(Date *date);
      double date_to_mjd_tt(Date *date);
      double date_to_lst(Date *date, Site *site, double ut1utc, double eqex);
      double date_to_time_of_day(Date *date);
      
      double current_mjd_utc(void);
      double current_mjd_tt(void);
      
      int mjd_utc_to_date(double utc, Date *date);
      double mjd_utc_to_mjd_tt(double utc);
      double mjd_utc_to_lst(double utc, Site *site, double ut1utc, double eqex);
      double mjd_utc_to_time_of_day(double utc);
      int mjd_utc_to_ymd(long mjd, int *year, int *month, int *day);
      int mjd_utc_to_year_day(long mjd, int *year, int *dayno);
      
      int day_of_year(Date *date);
      int is_leap_year(int year);
      int days_in_month(int is_leap, int month);
      const char *name_of_month(int month, int upper_case, int abbreviate);
      int dayno_to_date(int dayno, int is_leap, int *month, int *day);
      long mjd_of_year(int year);
      
      /*
       * Render a date on an output stream using a format like:
       *
       *   dd-mmm-yyyy hh:mm:ss.s  or  dd-mmm-yyyy:hh:mm:ss.s
       *
       * Note that mmm is a 3-letter month-name abbreviation like APR.
       * If the result takes less than width characters spaces will be
       * prepended to make up the deficit. If the flags[] string contains
       * a '-' character then spaces will be appended instead. The precision
       * argument specifies the number of decimal places to show in the seconds
       * field. If precision=0, then no decimal point will be displayed. If
       * the flags[] string contains a ':' character then the yyyy field will
       * be separated from the hh field by a colon instead of the normal space.
       * The utc argument should be a UTC expressed as a Modified Julian Date.
       */
      int output_utc(OutputStream *stream, char *flags, int width, 
		     int precision, double utc);

      int outputCarmaUtc(OutputStream *stream, char *flags, int width, 
			 int precision, double utc);
      
      /*
       * Read a UTC date from an ASCII input stream and return it as
       * a Modified Julian Date. The expected format is as described for
       * output_utc() except that the number of digits in each of the
       * numeric fields can be less than the number shown there. Also
       * trailing fields after the year field can be omitted.
       */
      int input_utc(InputStream *stream, int tell, int nospaces, double *utc);
      
      /*
       * The following type is returned by dec_visibility() to report
       * whether a specified parallalel of declination lies entirely above
       * a given horizon, entirely below that horizon, or whether it crosses
       * the horizon.  If it crosses the horizon then dec_visibility()
       * returns the magnitude of the hour angles at which a source at
       * the specified declination would cross the horizon.
       */
      typedef enum {
	DEC_ABOVE_HORIZON,   /* The given parallel of declination is
				entirely above the horizon */
	DEC_BELOW_HORIZON,   /* The given parallel of declination is
				entirely below the horizon */
	DEC_SPANS_HORIZON    /* The given parallel of declination is
				partly above and partly below the
				horizon. The crossing points are at
				+/- the returned hour angle */
      } DecSpan;
      
      DecSpan dec_visibility(Site *site, double el, double dec, double *ha);
      
  }
}

#endif
