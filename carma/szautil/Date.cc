#include "carma/szautil/Date.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/String.h"

#include <iostream>
#include <sstream>
#include <cmath>
#include <cstring>
#include <iomanip>

using namespace std;
using namespace sza::util;

/* dnint(A) - round to nearest whole number (double) */
#define dnint(A) ((A)<0.0?ceil((A)-0.5):floor((A)+0.5))

/* max(A,B) - larger (most +ve) of two numbers (generic) */
#define gmax(A,B) ((A)>(B)?(A):(B))

/* dmod(A,B) - A modulo B (double) */
#define dmod(A,B) ((B)!=0.0?((A)*(B)>0.0?(A)-(B)*floor((A)/(B))\
                                        :(A)+(B)*floor(-(A)/(B))):(A))


const char* Date::months[] = {
    "Jan",
    "Feb",
    "Mar",
    "Apr",
    "May",
    "Jun",
    "Jul",
    "Aug",
    "Sep",
    "Oct",
    "Nov",
    "Dec"
  };


/**.......................................................................
 * Constructors
 */
Date::Date() {}

Date::Date(const Date& date)
{
  *this = (Date&) date;
}

Date::Date(Date& date)
{
  *this = &date;
}

Date::Date(Date* date)
{
  mjd_ = date->mjd_;
}


Date::Date(std::string date) 
{
  setTo(date);
}

Date::Date(double mjd) : mjd_(mjd) {}

/**.......................................................................
 * Destructor.
 */
Date::~Date() {}

double Date::calToMjd(unsigned day, std::string month, int year)
{
  return calToMjd(validateDay(day, month), validateMonth(month), validateYear(year));
}

void Date::setToDateAndTime(std::string dateAndTime)
{
  String str(dateAndTime);

  // Allow dates to be separated by blanks or dashes

  unsigned int day  = str.findNextStringSeparatedByChars(" -").toInt();
  std::string month = str.findNextStringSeparatedByChars(" -").str();
  unsigned int year = str.findNextStringSeparatedByChars(" -:").toInt();

  // Allow for the day and year to be swapped

  if(day > 100 && year < 100) {
    unsigned int tmp = year;
    year = day;
    day  = tmp;
  }

  mjd_ = calToMjd(day, month, year);  

  // Now parse the time

  unsigned int hour = str.findNextStringSeparatedByChars(":").toInt();
  unsigned int min  = str.findNextStringSeparatedByChars(":").toInt();
  double sec        = str.findNextStringSeparatedByChars(" ").toDouble();

  // Add the fraction of the day since the MJD boundary

  mjd_ += ((hour * 60 + min) * 60 + sec)/86400;
}

void Date::setTo(std::string date, std::string time) 
{
  String str(date);

  // Allow dates to be separated by blanks or dashes

  unsigned int day  = str.findNextStringSeparatedByChars(" -").toInt();
  std::string month = str.findNextStringSeparatedByChars(" -").str();
  unsigned int year = str.findNextStringSeparatedByChars(" -").toInt();

  // Allow for the day and year to be swapped

  if(day > 100 && year < 100) {
    unsigned int tmp = year;
    year = day;
    day  = tmp;
  }
  mjd_ = calToMjd(day, month, year);  

  // Now parse the time

  String tstr(time);

  unsigned int hour = tstr.findNextStringSeparatedByChars(":").toInt();
  unsigned int min  = tstr.findNextStringSeparatedByChars(":").toInt();
  double sec        = tstr.findNextStringSeparatedByChars(" ").toDouble();

  // Add the fraction of the day since the MJD boundary

  mjd_ += ((hour * 60 + min) * 60 + sec)/86400;
}

void Date::setTo(std::string date) 
{
  String str(date);

  unsigned int day  = str.findNextStringSeparatedByChars(" -").toInt();
  std::string month = str.findNextStringSeparatedByChars(" -").str();
  unsigned int year = str.findNextStringSeparatedByChars(" -").toInt();

  mjd_ = calToMjd(day, month, year);  
}

void Date::operator=(std::string date)
{
  setTo(date);
}


/**.......................................................................
 * Convert from calendar date to MJD
 *  - - - - - - - - -
 *   s l a C a l d j
 *  - - - - - - - - -
 *
 *  Gregorian calendar to Modified Julian Date.
 *
 *  (Includes century default feature:  use slaCldj for years
 *   before 100AD.)
 *
 *  Given:
 *     iy,im,id   int      year, month, day in Gregorian calendar
 *
 *  Returned:
 *     *djm       double   Modified Julian Date (JD-2400000.5) for 0 hrs
 *     *j         int      status:
 *                           0 = ok
 *                           1 = bad year   (MJD not computed)
 *                           2 = bad month  (MJD not computed)
 *                           3 = bad day    (MJD computed)
 *
 *  The year must be -4699 (i.e. 4700BC) to 2049
 *
 */
double Date::calToMjd(int id, int im, int iy)
{
  long iyL, imL;
  
  
  /* Month lengths in days */
  static int mtab[12] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
  
  
  
  /* Validate year */
  if ( iy < -4699 ) {
    ThrowError("Invalid year");
  }
  
  /* Validate month */
  if ( ( im < 1 ) || ( im > 12 ) ) { 
    ThrowError("Invalid month");
  }
  
  /* Allow for leap year */
  mtab[1] = ( ( ( iy % 4 ) == 0 ) &&
	      ( ( ( iy % 100 ) != 0 ) || ( ( iy % 400 ) == 0 ) ) ) ?
    29 : 28;
  
  /* Validate day */
  
  
  if(( id < 1 || id > mtab[im-1] ))
    ThrowError("Invalid day");
  
  /* Lengthen year and month numbers to avoid overflow */
  
  iyL = (long) iy;
  imL = (long) im;
  
  /* Perform the conversion */

  return (double)
    ( ( 1461L * ( iyL - ( 12L - imL ) / 10L + 4712L ) ) / 4L
      + ( 306L * ( ( imL + 9L ) % 12L ) + 5L ) / 10L
      - ( 3L * ( ( iyL - ( 12L - imL ) / 10L + 4900L ) / 100L ) ) / 4L
      + (long) id - 2399904L );
}

void Date::slaClyd( int iy, int im, int id, int *ny, int *nd, int *jstat )
/*
**  - - - - - - - -
**   s l a C l y d
**  - - - - - - - -
**
**  Gregorian calendar to year and day in year (in a Julian calendar
**  aligned to the 20th/21st century Gregorian calendar).
**
**  Given:
**     iy,im,id     int    year, month, day in Gregorian calendar
**
**  Returned:
**     ny          int    year (re-aligned Julian calendar)
**     nd          int    day in year (1 = January 1st)
**     jstat       int    status:
**                          0 = OK
**                          1 = bad year (before -4711)
**                          2 = bad month
**                          3 = bad day (but conversion performed)
**
**  Notes:
**
**  1  This routine exists to support the low-precision routines
**     slaEarth, slaMoon and slaEcor.
**
**  2  Between 1900 March 1 and 2100 February 28 it returns answers
**     which are consistent with the ordinary Gregorian calendar.
**     Outside this range there will be a discrepancy which increases
**     by one day for every non-leap century year.
**
**  3  The essence of the algorithm is first to express the Gregorian
**     date as a Julian Day Number and then to convert this back to
**     a Julian calendar date, with day-in-year instead of month and
**     day.  See 12.92-1 and 12.95-1 in the reference.
**
**  Reference:  Explanatory Supplement to the Astronomical Almanac,
**              ed P.K.Seidelmann, University Science Books (1992),
**              p604-606.
**
**  Last revision:   26 November 1994
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   long i, j, k, l, n, iyL, imL;

/* Month lengths in days */
   static int mtab[12] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };



/* Validate year */
   if ( iy < -4711 ) { *jstat = 1; return; }

/* Validate month */
   if ( ( im < 1 ) || ( im > 12 ) ) { *jstat = 2; return; }

/* Allow for (Gregorian) leap year */
   mtab[1] = ( ( ( iy % 4 ) == 0 ) &&
             ( ( ( iy % 100 ) != 0 ) || ( ( iy % 400 ) == 0 ) ) ) ?
             29 : 28;

/* Validate day */
   *jstat = ( id < 1 || id > mtab[im-1] ) ? 3 : 0;

/* Perform the conversion */
   iyL = (long) iy;
   imL = (long) im;
   i = ( 14 - imL ) /12L;
   k = iyL - i;
   j = ( 1461L * ( k + 4800L ) ) / 4L
     + ( 367L * ( imL - 2L + 12L * i ) ) / 12L
     - ( 3L * ( ( k + 4900L ) / 100L ) ) / 4L + (long) id - 30660L;
   k = ( j - 1L ) / 1461L;
   l = j - 1461L * k;
   n = ( l - 1L ) / 365L - l / 1461L;
   j = ( ( 80L * ( l - 365L * n + 30L ) ) / 2447L ) / 11L;
   i = n + j;
   *nd = 59 + (int) ( l -365L * i + ( ( 4L - n ) / 4L ) * ( 1L - j ) );
   *ny = (int) ( 4L * k + i ) - 4716;
}

std::string Date::mjdToCal()
{
  return Date::mjdToCal(mjd_);
}

std::string Date::mjdToCal(double mjd)
{
  int iymdf[4];
  double df;
  std::ostringstream  os;
  int status;

  slaDjcl(mjd, &iymdf[0], &iymdf[1], &iymdf[2], &df, &status);

  os << std::setw(2) << std::setfill('0') << iymdf[2] << " " << months[iymdf[1]-1] << " " << iymdf[0];
  os << " ";

  unsigned seconds = (unsigned)(df * 86400);
  unsigned hours   = seconds / 3600;
  unsigned mins    = (seconds - hours * 3600) / 60;
  unsigned secs    = (seconds - hours * 3600 - mins * 60);

  os << std::setw(2) << std::setfill('0') << hours << ":" << std::setw(2) << std::setfill('0') << mins << ":" 
     << std::setw(2) << std::setfill('0') << secs;

  return os.str();
}

std::string Date::mjdToArcCal()
{
  return Date::mjdToArcCal(mjd_);
}

std::string Date::mjdToArcCal(double mjd)
{
  int iymdf[4];
  double df;
  std::ostringstream  os;
  int status;

  slaDjcl(mjd, &iymdf[0], &iymdf[1], &iymdf[2], &df, &status);

  os << std::setw(2) << std::setfill('0') << iymdf[2] << "-" << months[iymdf[1]-1] << "-" << iymdf[0];
  os << ":";

  unsigned seconds = (unsigned)(df * 86400);
  unsigned hours   = seconds / 3600;
  unsigned mins    = (seconds - hours * 3600) / 60;
  unsigned secs    = (seconds - hours * 3600 - mins * 60);

  os << std::setw(2) << std::setfill('0') << hours << ":" << std::setw(2) << std::setfill('0') << mins << ":" 
     << std::setw(2) << std::setfill('0') << secs;

  return os.str();
}
std::string Date::mjdToHorizonsCal()
{
  return Date::mjdToHorizonsCal(mjd_);
}

std::string Date::mjdToHorizonsCal(double mjd)
{
  int iymdf[4];
  double df;
  std::ostringstream  os;
  int status;

  slaDjcl(mjd, &iymdf[0], &iymdf[1], &iymdf[2], &df, &status);

  os << iymdf[0] << "-" << months[iymdf[1]-1] << "-" << std::setw(2) << std::setfill('0') << iymdf[2];
  os << " ";

  unsigned seconds = (unsigned)(df * 86400);
  unsigned hours   = seconds / 3600;
  unsigned mins    = (seconds - hours * 3600) / 60;
  unsigned secs    = (seconds - hours * 3600 - mins * 60);

  os << std::setw(2) << std::setfill('0') << hours << ":" << std::setw(2) << std::setfill('0') << mins << ":"
     << std::setw(2) << std::setfill('0') << secs;

  return os.str();
}

std::string Date::mjdToBuildDate(double mjd)
{
  int iymdf[4];
  double df;
  std::ostringstream  os;
  int status;

  slaDjcl(mjd, &iymdf[0], &iymdf[1], &iymdf[2], &df, &status);

  os << std::setw(2) << std::setfill('0') << iymdf[2] << " ";
  os << months[iymdf[1]-1] << " ";
  os << std::setw(4) << iymdf[0];

  return os.str();
}

std::string Date::mjdToBuildTime(double mjd)
{
  int iymdf[4];
  double df;
  std::ostringstream  os;
  int status;

  slaDjcl(mjd, &iymdf[0], &iymdf[1], &iymdf[2], &df, &status);

  unsigned seconds = (unsigned)(df * 86400);
  unsigned hours   = seconds / 3600;
  unsigned mins    = (seconds - hours * 3600) / 60;
  unsigned secs    = (seconds - hours * 3600 - mins * 60);

  os << std::setw(2) << std::setfill('0') << hours << ":" << std::setw(2) << std::setfill('0') << mins << ":"
     << std::setw(2) << std::setfill('0') << secs;

  return os.str();
}

void Date::mjdToCalDate(double mjd, unsigned& year, unsigned& month, unsigned& day)
{
  int iymdf[4];
  std::ostringstream  os;

  slaDjcal(0, mjd, iymdf);

  year  = iymdf[0];
  month = iymdf[1];
  day   = iymdf[2];
}

void Date::slaDjcl(double djm, int *iy, int *im, int *id, double *fd, int *j)
/*
**  - - - - - - - -
**   s l a D j c l
**  - - - - - - - -
**
**  Modified Julian Date to Gregorian year, month, day,
**  and fraction of a day.
**
**  Given:
**     djm      double     Modified Julian Date (JD-2400000.5)
**
**  Returned:
**     *iy      int        year
**     *im      int        month
**     *id      int        day
**     *fd      double     fraction of day
**     *j       int        status:
**                      -1 = unacceptable date (before 4701BC March 1)
**
**  The algorithm is derived from that of Hatcher 1984 (QJRAS 25, 53-55).
**
**  Defined in slamac.h:  dmod
**
**  Last revision:   20 April 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
  double f, d;
  long jd, n4, nd10;

/* Check if date is acceptable */
   if ( ( djm <= -2395522.0 ) || ( djm >= 1e9 ) ) {
      *j = -1;
      return;
   } else {
      *j = 0;

   /* Separate day and fraction */
      f = dmod ( djm, 1.0 );
      if ( f < 0.0 ) f += 1.0;
      d = djm - f;
      d = dnint ( d );

   /* Express day in Gregorian calendar */
      jd = (long) dnint ( d ) + 2400001;
      n4 = 4L*(jd+((6L*((4L*jd-17918L)/146097L))/4L+1L)/2L-37L);
      nd10 = 10L*(((n4-237L)%1461L)/4L)+5L;
      *iy = (int) (n4/1461L-4712L);
      *im = (int) (((nd10/306L+2L)%12L)+1L);
      *id = (int) ((nd10%306L)/10L+1L);
      *fd = f;
      *j = 0;
   }
}

void Date::slaDjcal ( int ndp, double djm, int iymdf[4])
/*
**  - - - - - - - - -
**   s l a D j c a l
**  - - - - - - - - -
**
**  Modified Julian Date to Gregorian calendar, expressed
**  in a form convenient for formatting messages (namely
**  rounded to a specified precision, and with the fields
**  stored in a single array).
**
**  Given:
**     ndp      int       number of decimal places of days in fraction
**     djm      double    Modified Julian Date (JD-2400000.5)
**
**  Returned:
**     iymdf    int[4]    year, month, day, fraction in Gregorian calendar
**     *j       long      status:  nonzero = out of range
**
**  Any date after 4701BC March 1 is accepted.
**
**  Large ndp values risk internal overflows.  It is typically safe
**  to use up to ndp=4.
**
**  The algorithm is derived from that of Hatcher 1984 (QJRAS 25, 53-55).
**
**  Defined in slamac.h:  dmod
**
**  Last revision:   19 March 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double fd, df, f, d;
   long jd, n4, nd10;

/* Validate */

   if ( ( djm <= -2395520.0 ) || ( djm >= 1.0e9 ) ) {
     ThrowError("Invalid MJD: " << djm);
   } else {

   /* Denominator of fraction */
      fd = pow ( 10.0, (double) gmax ( ndp, 0 ) );
      fd = dnint ( fd );

   /* Round date and express in units of fraction */
      df = djm * fd;
      df = dnint ( df );

   /* Separate day and fraction */
      f = dmod ( df, fd );
      if ( f < 0.0 ) f += fd;
      d = ( df - f ) / fd;

   /* Express day in Gregorian calendar */
      jd = (long) dnint ( d ) + 2400001L;
      n4 = 4L * ( jd + ( ( 2L * ( ( 4L * jd - 17918L ) / 146097L)
                                       * 3L ) / 4L + 1L ) / 2L - 37L );
      nd10 = 10L * ( ( ( n4 - 237L ) % 1461L ) / 4L ) + 5L;
      iymdf[0] = (int) ( ( n4 / 1461L ) - 4712L );
      iymdf[1] = (int) ( ( ( nd10 / 306L + 2L ) % 12L ) + 1L );
      iymdf[2] = (int) ( ( nd10 % 306L ) / 10L + 1L );
      iymdf[3] = (int) dnint ( f );
      }
}

std::ostream& 
sza::util::operator<<(std::ostream& os, Date& date)
{
  os << Date::mjdToCal(date.mjd_);
  return os;
}

std::ostream& 
sza::util::operator<<(std::ostream& os, const Date& date)
{
  os << Date::mjdToCal(date.mjd_);
  return os;
}

unsigned Date::validateMonth(std::string month)
{
  for(unsigned iM=0; iM < 12; iM++)
    if(strcasecmp(months[iM], month.c_str())==0)
      return iM+1;

  ThrowError("Invalid month: " << month);
}

int Date::validateYear(int year)
{
  if(year < -4699 || year > 2049)
    ThrowError("Invalid year: " << year);

  return year;
}

unsigned Date::validateDay(unsigned day, std::string month)
{
  return day;
}

bool Date::operator>(Date& date)
{
  return (mjd_ > date.mjd_);
}

bool Date::operator>=(Date& date)
{
  return (mjd_ >= date.mjd_);
}

bool Date::operator<(Date& date)
{
  return (mjd_ < date.mjd_);
}

bool Date::operator<(const Date& date)
{
  return (mjd_ < date.mjd_);
}

bool Date::operator<=(Date& date)
{
  return (mjd_ <= date.mjd_);
}

bool Date::operator==(Date& date)
{
  return fabs(mjd_ - date.mjd_) < 1;
}

/**.......................................................................
 * Subtraction operator for Date.
 */
Date Date::operator-(double days)
{
  if(mjd_ - days < 0)
    ThrowError("Subtraction of " << mjd_ << " and " << days << " would produce an invalid date");

  Date diff(mjd_-days);
  return diff;
}

/**.......................................................................
 * Subtraction operator for Date.
 */
Date Date::operator-(Date& days)
{
  if(mjd_ - days.mjd_ < 0)
    ThrowError("Subtraction of " << mjd_ << " and " << days.mjd_ << 
	       " would produce an invalid date");

  Date diff(mjd_-days.mjd_);
  return diff;
}

/**.......................................................................
 * Addition operator for Date.
 */
void Date::operator+=(double days)
{
  mjd_ += days;
}

/**.......................................................................
 * Addition operator for Date.
 */
Date Date::operator+(double days)
{
  Date sum(mjd_ + days);
  return sum;
}

/**.......................................................................
 * Subtraction operator for Date.
 */
int Date::deltaDays(Date& date1, Date& date2)
{
  return (int)(date1.mjd_ - date2.mjd_ + 1);
}

unsigned Date::numberOfDays()
{
  if(mjd_ >= 0)
    return (unsigned) mjd_;
  else 
    ThrowError("Cannot convert: " << mjd_ << " to an unsigned integer");
}

unsigned Date::day()
{
  int iymdf[4];

  slaDjcal(0, mjd_, iymdf);

  return iymdf[2];
}

std::string Date::month()
{
  int iymdf[4];
  std::ostringstream  os;

  slaDjcal(0, mjd_, iymdf);

  os << months[iymdf[1]-1];

  return os.str();
}

unsigned Date::year()
{
  int iymdf[4];
  slaDjcal(0, mjd_, iymdf);
  return iymdf[0];
}

bool Date::isEmpty()
{
  return mjd_ < 1;
}

double Date::calToMjd(int id, int im, int iy, int hour, int min, int sec)
{
  double days = calToMjd(id, im, iy);
  double seconds = (hour * 60 + min) * 60 + sec;

  return days + seconds/86400;
}

unsigned Date::dayInYear()
{
  int iymdf[4];

  slaDjcal(0, mjd_, iymdf);
  
  int id, iy;
  int status=0;

  slaClyd(iymdf[0], iymdf[1], iymdf[2], &iy, &id, &status);

  return id;
}

void Date::addHours(double hours)
{
  mjd_ += hours/24;;
}

void Date::addDays(double days)
{
  mjd_ += days;
}
