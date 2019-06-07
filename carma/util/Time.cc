
/**
 * @file   
 * Implementation for some common time functions.
 *
 * @author Original: Steve Scott
 * @reviewer Original: Colby Kraybill, Peter Teuben
 * @inspector Original: Peter Teuben
 *
 * $Id: Time.cc,v 1.40 2012/07/10 16:16:54 abeard Exp $
 * $CarmaCopyright$
 *
 */

#include "carma/util/Time.h"
#include "carma/util/ErrorException.h"

#include <iostream>
#include <iomanip>
#include <limits>
#include <sstream>
#include <sys/time.h>

#include <math.h>

using namespace std;
using namespace carma::util;

const double Time::MINUTES_PER_DAY = Time::SECONDS_PER_DAY / 60.0;
const double Time::MILLISECONDS_PER_DAY = 1000 * Time::SECONDS_PER_DAY;
const double Time::MICROSECONDS_PER_DAY = 1000000 * Time::SECONDS_PER_DAY;
const double Time::FRAMES_PER_DAY = 2 * Time::SECONDS_PER_DAY;

// Default constructor does nothing
Time::Time() { mjd_ = 0.0; }

Time::Time(double mjd) {
  mjd_ = mjd;
}


// Destructor does nothing
Time::~Time() { }

double Time::getStoredMJD() const {
  return mjd_;
}

double Time::MJD()
{
  struct timespec ts;

  clock_gettime(CLOCK_REALTIME, &ts);
  return Time::timespec2MJD (ts);
}


double Time::timespec2MJD (const struct timespec ts)
{
  const double max = numeric_limits<double>::max( );
  double mjd  = MJD1970 + (ts.tv_sec + ts.tv_nsec*1e-9)/SECONDS_PER_DAY;
  return ::nextafter( mjd, max );
}

double Time::timeval2MJD (const struct timeval & tv)
{
  const double max = numeric_limits<double>::max( );
  double mjd  = MJD1970 + (tv.tv_sec + tv.tv_usec*1e-6)/SECONDS_PER_DAY;
  return ::nextafter( mjd, max );
}

time_t Time::gettime_t(const frameType frame)
{
  double mjd = MJD(frame);
  time_t secs = static_cast<time_t>(SECONDS_PER_DAY * (mjd - MJD1970));
  return secs;
}

double Time::MJD(frameType frame)  
{
  const double max = numeric_limits<double>::max( );
  double mjd = frame/FRAMES_PER_DAY + MJD2000;
  return ::nextafter( mjd, max );
}


double Time::get2kd()  
{
  return (MJD() - MJD2000);
}

frameType Time::computeClosestFrame()  
{
  return static_cast<frameType> (0.5 + FRAMES_PER_DAY * get2kd());
}


frameType Time::computeClosestFrame(double mjd)  
{
  return static_cast<frameType> (0.5 + FRAMES_PER_DAY * (mjd - MJD2000));
}

frameType Time::computeClosestFrame(const std::string& date,
                                    const std::string& format,
                                    const Time::TimeZone tz) 
{
  double mjd = computeMJD(date,format,tz);
  // if zero was returned then the string could not be parsed
  return (mjd > 0) ? computeClosestFrame(mjd) 
    : static_cast<frameType> ( 0 );

}

// THIS IS COMPUTATIONALLY EXPENSIVE 
// USE diffHrsFromGmt where possible
struct tm Time::getGmtFromLmt(const std::string &date,
			      const std::string &format,
			      const Time::TimeZone tz) {
  // initialize return time with input variables
  struct tm returnTime;
  strptime(date.c_str(), format.c_str(), &returnTime);

  // start with local time
  struct tm localTime;
  strptime(date.c_str(), format.c_str(), &localTime);
  time_t secSince1970InLocalTime = mktime(&localTime);

  struct tm *gmTime = gmtime(&secSince1970InLocalTime);
  // calculate number of hours from GMT
  int diffHoursFromGmt = 0;
  if (gmTime->tm_mday != localTime.tm_mday) {
    // if days are different, we've wrapped
    diffHoursFromGmt = gmTime->tm_hour+24 - localTime.tm_hour;
  } else {
    diffHoursFromGmt = gmTime->tm_hour - localTime.tm_hour;
  }

  // modify returnTime to reflect the time at GMT
  returnTime.tm_hour = localTime.tm_hour - diffHoursFromGmt + tz;

  return returnTime;
}

int Time::diffHrsFromGmt( const struct tm myTime ,
	                     const time_t secondsSince1970 )
{

  struct tm *gmTime = gmtime(&secondsSince1970);
  // calculate number of hours from GMT
  int diffHoursFromGmt = 0;
  if (gmTime->tm_mday != myTime.tm_mday) {
    // if days are different, we've wrapped
    diffHoursFromGmt = gmTime->tm_hour+24 - myTime.tm_hour;
  } else {
    diffHoursFromGmt = gmTime->tm_hour - myTime.tm_hour;
  }
  return diffHoursFromGmt;

}


double Time::computeMJD(const std::string& date,
                        const std::string& format,
                        const Time::TimeZone tz) 
{
  struct tm myTime;
  strptime(date.c_str(),format.c_str(),&myTime);
  time_t secondsSince1970 = mktime(&myTime);

  if (tz != Time::LOCAL) {
    myTime.tm_hour -= diffHrsFromGmt( myTime, secondsSince1970 );
    myTime.tm_hour += tz;
    // now recompute seconds since 1970
    secondsSince1970 = mktime(&myTime);
  }

  if (secondsSince1970 != -1 ) {
	return computeMJD(secondsSince1970);
  } else {
	return 0;
  }
}

double Time::computeMJD1(const std::string& date,
                        const std::string& format,
                        const Time::TimeZone tz) 
{
  struct tm myTime;

  // strptime/mktime ALWAYS assumes that the "struct tm" is in local
  // time.  therefore, if an alternate time zone is selected, we
  // need to do modify myTime in such a way that it will give the
  // new time.
  if (tz != Time::LOCAL) {
    myTime = getGmtFromLmt(date, format, tz);
  } else {
    // if we just want local time, this is all we need
    strptime(date.c_str(),format.c_str(),&myTime);
  }

  time_t secondsSince1970 = mktime(&myTime);

  if (secondsSince1970 != -1 ) {
	return computeMJD(secondsSince1970);
  } else {
	return 0;
  }
}


frameType Time::computeCurrentFrame()  
{
  return static_cast<frameType> (FRAMES_PER_DAY*get2kd());
}


frameType Time::computeFrame(double mjd)  
{
  const double max = numeric_limits<double>::max( );
  mjd = ::nextafter( mjd, max );
  return static_cast<frameType> (FRAMES_PER_DAY * (mjd - MJD2000));
}



double Time::computeTimeToNextFrame(double delay)  
{
  double now  = SECONDS_PER_DAY*get2kd();    // in seconds since 2k
  double lastHalfSecond = 0.5*floor(now*2);  // in seconds since 2k
  return 0.5 - (now - lastHalfSecond) + delay;
}


double Time::computeTimeToFrame(frameType frame, double delay)  
{
  double now  = SECONDS_PER_DAY*get2kd();       // in seconds since 2k
  return (0.5*frame - now) + delay;
}


double Time::computeFrameTimeDiff(frameType frame, double delay)  
{
  return -computeTimeToFrame(frame, delay);
}

double Time::computeMJD(time_t seconds) 
{
  const double max = numeric_limits<double>::max( );
  const double mjd = MJD1970 + static_cast< double >( seconds )/SECONDS_PER_DAY;
  return ::nextafter( mjd, max );
}

string Time::getTimeString(frameType frame, int precision)  
{
  return getTimeString(MJD(frame), precision);
}


string Time::getTimeString(int precision)  
{
  return getTimeString(MJD(), precision);
}


string Time::getDateString(const std::string &dateFormat)  
{
  return getDateString(static_cast< int >( MJD() ), dateFormat);
}


string Time::getDateString(frameType frame, const std::string &dateFormat)  
{
  return getDateString(static_cast< int >( MJD(frame) ), dateFormat);
}


string Time::getDateString(double mjd, const std::string &dateFormat)  
{
  return getDateString( static_cast< int >(mjd), dateFormat);
}


string Time::getDateTimeString(frameType frame, int precision, const std::string &dateFormat)  
{
  return getDateTimeString(MJD(frame), precision, dateFormat);
}


string Time::getDateTimeString(int precision, const std::string &dateFormat)  
{
  return getDateTimeString(MJD(), precision, dateFormat);
}

string Time::getFITSdateString()  
{
  return getFITSdateString(static_cast< int >( MJD() ));
}


string Time::getFITSdateString(frameType frame)  
{
  return getFITSdateString(static_cast< int >( MJD(frame) ));
}


string Time::getFITSdateTimeString(frameType frame, int precision)  
{
  return getFITSdateTimeString(MJD(frame), precision);
}

string Time::getFITSdateTimeString(int precision)  
{
  return getFITSdateTimeString(MJD(), precision);
}

  
/*
 * To prepare for string output we need to round off the time so that
 * all fields will be self consistent. 
 */
double Time::roundMJD(double mjd, int precision)  
{
  // Range check the precision
  if (precision > 15) precision = 15;
  if (precision <  0) precision =  0;

  // Round needs to be half of final digit...
  double round = 0.5;
  for (int i=0; i < precision; i++) round *= 0.1;

  // Add on the rounding factor
  mjd += round/SECONDS_PER_DAY;
    
  return  mjd ;
}    



int     Time::computeIntegerMJD(double mjd, int precision) 
{
  mjd = roundMJD(mjd, precision);
  return static_cast<int>(mjd);
}


/*
 * The ostringstream operator used to print out the seconds always rounds -
 * while this seems like a good thing, the rounding is not extended up to
 * the minutes and hours, so we have to do that ourselves.
 * If the precision is zero, then add two microseconds so that
 * frame boundaries get rounded correctly. Because of the error in representing
 * a frame as a real number when it is converted to an MJD, we can end up
 * with a frame being XXX.4999982 seconds, which then looks like a truncation
 * to XXX rather than a round to XXX+1.
 */
string Time::getTimeString(double mjd, int precision)  
{
  // If the precision is zero, then add a little so that
  // frame boundaries get rounded correctly. 
  if (precision == 0) mjd += 2e-6/Time::SECONDS_PER_DAY;
  
  double roundedMJD  = roundMJD(mjd, precision);
  double roundedSecs = (roundedMJD - mjd)*SECONDS_PER_DAY; // seconds
  
  if ((precision < 0) || (precision > 10)) {
    ostringstream o;
    o << "Time::getTimeString(); requested precision (" << precision
      << ") is out of range[0,10]";
    throw CARMA_ERROR(o);
  }

  // Get fractional part, the time of day
  double tod = roundedMJD - static_cast<int>(roundedMJD);
  
  // Convert to integer seconds
  int seconds = static_cast<int>(tod*SECONDS_PER_DAY);
    
  int width = precision + 3;     // Precision plus two digits plus decimal pt
  if (precision == 0) width = 2; // No decimal point    
   
  double fractionalSeconds = tod*SECONDS_PER_DAY - seconds; 
  int hrs   = seconds/3600;
  seconds  -= hrs*3600;
  int mins  = seconds/60;
  seconds  -= mins*60;
  // Remove the rounding factor (output automatically rounds)
  double realSeconds = seconds + fractionalSeconds - roundedSecs ;
  if (0) cout << seconds << "/"  << setprecision(0) << realSeconds << "/"  
       << setprecision(8) << realSeconds << "/" 
       << setprecision(precision+1) << realSeconds << "/" 
       << roundedSecs << endl;
  if (0) cout << setprecision(7) << mjd*Time::SECONDS_PER_DAY << "/" 
       << setprecision(7) << roundedMJD*Time::SECONDS_PER_DAY << "/" 
       << setprecision(1) << roundedMJD*Time::SECONDS_PER_DAY << "/" 
       << setprecision(0) << roundedMJD*Time::SECONDS_PER_DAY 
       << endl;
  // But if it is less than zero it prints "-0", so avoid negative values
  if (realSeconds < 0.0) realSeconds = 0.0;
  ostringstream os;
  os.setf(ios::fixed);
  os.fill('0');
  os << setw(2) << hrs << ":" << setw(2) << mins << ":"
     << setw(width) << setprecision(precision) << realSeconds;
  return os.str();
}

string Time::getNonBuggyByDesignDateTimeString(double mjd, const ::std::string &dateFormat)
{
  // Get unix style time in seconds
  time_t secs = static_cast< time_t >(SECONDS_PER_DAY*(mjd - MJD1970));
  char buf[64];
  struct tm t;

  gmtime_r(&secs, &t);

  // System call to convert time to string, encoded format in quotes
  strftime(buf, sizeof(buf), dateFormat.c_str(), &t);
  return buf;
}

string Time::getDateString(int mjd, const std::string &dateFormat)
{
  // Get unix style time in seconds
  time_t secs = static_cast< time_t >(SECONDS_PER_DAY*(mjd - MJD1970));
  char buf[64];
  struct tm t;

  gmtime_r(&secs, &t);

  // System call to convert time to string, encoded format in quotes
  strftime(buf, sizeof(buf), dateFormat.c_str(), &t);
  return buf;
}


string Time::getFITSdateString(int mjd)
{
  // Get unix style time in seconds
  time_t secs = static_cast< time_t >(SECONDS_PER_DAY*(mjd - MJD1970));
  char buf[16];
  struct tm t;

  gmtime_r(&secs, &t);

  // System call to convert time to string, encoded format in quotes
  strftime(buf, sizeof(buf), "%Y-%m-%d", &t);
  return buf;
}

string Time::getDateTimeString(double mjd, int precision, const std::string &dateFormat)
{
  string timeString = getDateString(computeIntegerMJD(mjd, precision),
        dateFormat) +
    " " +
    getTimeString(mjd, precision);
  return timeString;
}

string Time::getFITSdateTimeString(double mjd, int precision)
{
  string timeString = getFITSdateString(computeIntegerMJD(mjd, precision)) +
    "T" +
    getTimeString(mjd, precision);
  return timeString;
}

ostream& operator<<(ostream &os, const carma::util::Time& time)
{
  os << time.getTimeString(static_cast<int>(os.precision()));
  return os;
}

