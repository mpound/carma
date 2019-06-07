#include <sstream>
#include <errno.h>
#include <cstring>

#include <iomanip>

#include "carma/szautil/Astrometry.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/RegDate.h"
#include "carma/szautil/TimeVal.h"

// The MJD corresponding to 0 UTC, 1 January 1970.  This is the base
// for the system ABSTIME clock.

#define MJD0 40587

// The zero-point for calculating ids based on mjd (1 January 2007)                                     

#define MJDID0 54101

#define SEC_PER_DAY            86400
#define NSEC_PER_USEC           1000
#define NSEC_PER_MSEC        1000000
#define MSEC_PER_SEC            1000
#define USEC_PER_SEC         1000000
#define NSEC_PER_SEC      1000000000
#define NSEC_PER_HALF_SEC  500000000

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor with no initialization.
 */
TimeVal::TimeVal()
{
  timeVal_.tv_sec   = 0;
  timeVal_.tv_usec  = 0;

  timeSpec_.tv_sec  = 0;
  timeSpec_.tv_nsec = 0;

  seconds_          = 0;
  nanoSeconds_      = 0;

  mjdDays_          = 0;
  mjdSeconds_       = 0;
  mjdNanoSeconds_   = 0;
}

/**.......................................................................
 * Constructor with time initialization.
 *
 *  NB: The time will be the sum of the seconds, microseconds and
 *  nanoseconds passed.
 */
TimeVal::TimeVal(unsigned long seconds, unsigned long microSeconds, 
		 unsigned long nanoSeconds)
{
  setTime(seconds, microSeconds, nanoSeconds);
}

/**.......................................................................
 * Constructor with time initialization.
 *
 *  NB: The time will be the sum of the seconds, microseconds and
 *  nanoseconds passed.
 */
TimeVal::TimeVal(unsigned long seconds, unsigned long nanoSeconds)
{
  setTime(seconds, nanoSeconds);
}

/**.......................................................................
 * Constructor with initialization via timeval struct.
 */
TimeVal::TimeVal(const struct timeval& tVal)
{
  // Set up all internal time specifications to reflect the values in
  // the timeval struct.

  setTime(tVal);
}

/**.......................................................................
 * Constructor with initialization via timespec struct.
 */
TimeVal::TimeVal(const struct timespec& timeSpec)
{
  // Set up all internal time specifications to reflect the values in
  // the timespec struct.

  setTime(timeSpec);
}

/**.......................................................................
 * Constructor with initialization via double MJD
 */
TimeVal::TimeVal(double mjd)
{
  // Set up all internal time specifications to reflect the MJD

  setMjd(mjd);
}

/**.......................................................................
 * Set the time.
 */
void TimeVal::setTime(unsigned long seconds, unsigned long microSeconds, 
		      unsigned long nanoSeconds)
{
  // Compute the number of nanoseconds in the sub-second
  // specifications.

  unsigned long nsec = microSeconds * NSEC_PER_USEC + nanoSeconds;

  // Compute the number of seconds, accounting for nsec > NSEC_PER_SEC

  unsigned long sec  = seconds;

  // Correct the number of nanoseconds to be < NSEC_PER_SEC

  while(nsec >= NSEC_PER_SEC) {
    nsec -= NSEC_PER_SEC;
    sec++;
  }

  // Set up all internal time specifications to reflect these values.

  setSeconds(sec);
  setNanoSeconds(nsec);
}

/**.......................................................................
 * Set the time.
 */
void TimeVal::setTime(unsigned long seconds, unsigned long nanoSeconds)
{
  setSeconds(seconds);
  setNanoSeconds(nanoSeconds);
}

/**.......................................................................
 * Set the time with a timespec struct.
 */
void TimeVal::setTime(const struct timespec& timeSpec)
{
  // Set up all internal time specifications to reflect the values in
  // the timespec struct.

  setSeconds(timeSpec.tv_sec);
  setNanoSeconds(timeSpec.tv_nsec);
}

/**.......................................................................
 * Set the time with a timeval struct.
 */
void TimeVal::setTime(const struct timeval& timeval)
{
  // Set up all internal time specifications to reflect the values in
  // the timespec struct.

  setSeconds(timeval.tv_sec);
  setMicroSeconds(timeval.tv_usec);
}

/**.......................................................................
 * Set just the number of seconds.
 */
void TimeVal::setSeconds(unsigned long seconds)
{
  seconds_         = seconds;
  timeVal_.tv_sec  = seconds;
  timeSpec_.tv_sec = seconds;

  // Set up the MJD too

  mjdDays_    = seconds / SEC_PER_DAY;
  mjdSeconds_ = seconds - mjdDays_ * SEC_PER_DAY;
  mjdDays_   += MJD0;
}

/**.......................................................................
 * Set the number of microseconds.
 */
void TimeVal::setMicroSeconds(unsigned long microSeconds)
{
  if(microSeconds >= USEC_PER_SEC)
    throw Error("TimeVal::setMicroSeconds: "
		"argument is too large ( > 1 sec).\n");

  nanoSeconds_      = microSeconds * NSEC_PER_USEC;
  timeVal_.tv_usec  = microSeconds;
  timeSpec_.tv_nsec = microSeconds * NSEC_PER_USEC;

  // Set up the MJD too.

  mjdNanoSeconds_   = nanoSeconds_;
}

/**.......................................................................
 * Set the number of nanoseconds.
 */
void TimeVal::setNanoSeconds(unsigned long nanoSeconds)
{
  if(nanoSeconds >= NSEC_PER_SEC)
    throw Error("TimeVal::setNanoSeconds: "
		"argument is too large ( >= 1 sec).\n");

  nanoSeconds_      = nanoSeconds;
  timeVal_.tv_usec  = nanoSeconds / NSEC_PER_USEC;
  timeSpec_.tv_nsec = nanoSeconds;

  // Set up the MJD too.

  mjdNanoSeconds_   = nanoSeconds_;
}

/**.......................................................................
 * Increment the time, by fractional seconds.
 */
void TimeVal::incrementSeconds(double seconds)
{
  long incSec = (long)seconds;
  long incNanoSec = (long)((seconds - incSec) * NSEC_PER_SEC);

  long sec  = seconds_     + incSec;
  long nsec = nanoSeconds_ + incNanoSec;

  while(nsec >= NSEC_PER_SEC) {
    nsec -= NSEC_PER_SEC;
    sec++;
  }

  while(nsec < 0) {
    nsec += NSEC_PER_SEC;
    sec--;
  }

  // Update all internal time representations.
  
  setSeconds(sec);
  setNanoSeconds(nsec);
}

/**.......................................................................
 * Increment the time, by nanoSeconds.
 */
void TimeVal::incrementNanoSeconds(unsigned nanoSeconds)
{
  long incNanoSec = (long)nanoSeconds;

  long sec  = seconds_;
  long nsec = nanoSeconds_ + incNanoSec;

  while(nsec >= NSEC_PER_SEC) {
    nsec -= NSEC_PER_SEC;
    sec++;
  }

  while(nsec < 0) {
    nsec += NSEC_PER_SEC;
    sec--;
  }

  // Update all internal time representations.
  
  setSeconds(sec);
  setNanoSeconds(nsec);
}

/**.......................................................................
 * Reset internal structs to stored values.
 */
void TimeVal::reset()
{
  timeVal_.tv_sec  = seconds_;
  timeVal_.tv_usec = nanoSeconds_ / NSEC_PER_USEC;

  timeSpec_.tv_sec  = seconds_;
  timeSpec_.tv_nsec = nanoSeconds_;
}

/**.......................................................................
 * Return a timeval pointer suitable for passing to select()
 */
struct timeval* TimeVal::timeVal()
{
  return &timeVal_;
}

/**.......................................................................
 * Return a timespec pointer suitable for passing to clock_gettime(), etc.
 */
struct timespec* TimeVal::timeSpec()
{
  return &timeSpec_;
}

/**.......................................................................
 * Addition operator for TimeVal.  Note that addition only applies to
 * the unchangeable members of this class.  Ie, in the returned
 * object, the timeVal_ struct will contain the addition of the preset
 * times, not the addition of timeVal_ structs, which can be different.
 */
const TimeVal TimeVal::operator+(const TimeVal& tVal)
{
  unsigned long sec, nsec;

  sec  = seconds_     + tVal.seconds_;
  nsec = nanoSeconds_ + tVal.nanoSeconds_;

  while(nsec >= NSEC_PER_SEC) {
    nsec -= NSEC_PER_SEC;
    sec++;
  }

  return TimeVal(sec, 0, nsec);
}

/**.......................................................................
 * Subtraction operator for TimeVal.  Note that while TimeVal objects
 * should only contain positive time values, the subtraction of the
 * nanoseconds portion can be negative.
 */
const TimeVal TimeVal::operator-(const TimeVal& tVal)
{
  unsigned long deltaSec;
  long deltaNsec;

  if(tVal.seconds_ > seconds_)
    throw Error("TimeVal::operator-: "
		"result will be negative.\n");

  deltaSec  = seconds_     - tVal.seconds_;
  deltaNsec = nanoSeconds_ - tVal.nanoSeconds_;

  while(deltaNsec < 0) {
    deltaNsec += NSEC_PER_SEC;
    deltaSec--;
  }

  return TimeVal(deltaSec, 0, deltaNsec);
}

/**.......................................................................
 * Return the total time in MJD days.
 */
double TimeVal::getTimeInMjdDays()
{
  return mjdDays_ + 
    (double)(mjdSeconds_ + (double)(mjdNanoSeconds_) / NSEC_PER_SEC) 
    / SEC_PER_DAY;
}

/**.......................................................................
 * Return the total time in seconds.
 */
double TimeVal::getTimeInSeconds()
{
  return seconds_ + (double)(nanoSeconds_) / NSEC_PER_SEC;
}

/**.......................................................................
 * Return the total time in milliseconds.
 */
double TimeVal::getTimeInMilliSeconds()
{
  return seconds_ * MSEC_PER_SEC + nanoSeconds_ / NSEC_PER_MSEC;
}

/**.......................................................................
 * Return the total time in microseconds.
 */
double TimeVal::getTimeInMicroSeconds()
{
  return seconds_ * USEC_PER_SEC + nanoSeconds_ / NSEC_PER_USEC;
}

/**.......................................................................
 * Return the total time as integer nanoseconds.
 */
unsigned long TimeVal::getTimeInNanoSeconds()
{
  return seconds_ * NSEC_PER_SEC + nanoSeconds_;
}

/**.......................................................................
 * Return just the fractional seconds, as a double
 */
double TimeVal::getFractionalTimeInSeconds()
{
  return (double)(nanoSeconds_) / NSEC_PER_SEC;
}

/**.......................................................................
 * Return just the integer seconds
 */
unsigned long TimeVal::getSeconds()
{
  return seconds_;
}

/**.......................................................................
 * Return just the integer microseconds
 */
unsigned long TimeVal::getMicroSeconds()
{
  return nanoSeconds_ / NSEC_PER_USEC;
}

/**.......................................................................
 * Return just the integer nanoseconds
 */
unsigned long TimeVal::getNanoSeconds()
{
  return nanoSeconds_;
}

/**.......................................................................
 * Return the time elapsed as fractional seconds.  Note that the
 * ***Elapsed() methods only apply to our timeval struct, since a
 * timespec struct is not used as a countdown timer.
 */
double TimeVal::getElapsedTimeInSeconds()
{
  unsigned long dsec  = seconds_     - timeVal_.tv_sec;
  unsigned long dnsec = nanoSeconds_ - timeVal_.tv_usec * NSEC_PER_USEC;

  return dsec + double(dnsec) / NSEC_PER_SEC;
}

/**.......................................................................
 * Return the time in our timeVal struct as integer microseconds.
 */
double TimeVal::getElapsedTimeInMicroSeconds()
{
  unsigned long dsec  = seconds_     - timeVal_.tv_sec;
  unsigned long dnsec = nanoSeconds_ - timeVal_.tv_usec * NSEC_PER_USEC;

  return dsec * USEC_PER_SEC + (double)(dnsec) / NSEC_PER_USEC;
}

/**.......................................................................
 * Return the time in our timeVal struct as integer nanoseconds.
 */
unsigned long TimeVal::getElapsedTimeInNanoSeconds()
{
  unsigned long dsec  = seconds_     - timeVal_.tv_sec;
  unsigned long dnsec = nanoSeconds_ - timeVal_.tv_usec * NSEC_PER_USEC;

  return dsec * NSEC_PER_SEC + dnsec;
}

/**.......................................................................
 * Fill this structure with the current time.
 */
void TimeVal::setToCurrentTime(clockid_t clock)
{
#if HAVE_RT

  if(clock_gettime(clock, &timeSpec_) < 0) {
    ostringstream os;
    os << "TimeVal::setToCurrentTime: "
       << strerror(errno)
       << " in clock_gettime()." << endl << ends;
    throw Error(os.str());
  }

  // Update all internal time representations to reflect the values
  // returned in timeSpec_.

  setTime(timeSpec_);

#else

  if(gettimeofday(&timeVal_, 0) < 0) {
    ostringstream os;
    os << "TimeVal::setToCurrentTime: "
       << strerror(errno)
       << " in gettimeofday()." << endl << ends;
    throw Error(os.str());
  }

  // Update all internal time representations to reflect the values
  // returned in timeVal_.

  setTime(timeVal_);

#endif
}

/**.......................................................................
 * Return the mjd day number corresponding to this time.
 */
unsigned long TimeVal::getMjdDays()
{
  return mjdDays_;
}

/**.......................................................................
 * Return the mjd seconds corresponding to this time.
 */
unsigned long TimeVal::getMjdSeconds()
{
  return mjdSeconds_;
}

/**.......................................................................
 * Return the number of milliseconds into this MJD day.
 */
unsigned long TimeVal::getMjdMilliSeconds()
{
  return mjdSeconds_ * MSEC_PER_SEC + mjdNanoSeconds_ / NSEC_PER_MSEC;
}

/**.......................................................................
 * Return the mjd nanoseconds corresponding to this time.
 */
unsigned long TimeVal::getMjdNanoSeconds()
{
  return mjdNanoSeconds_;
}

/**.......................................................................
 * Return the mjd day number as a double
 */
double TimeVal::getMjd()
{
  return mjdDays_ + (mjdSeconds_ + (double)(mjdNanoSeconds_) / NSEC_PER_SEC) 
    / SEC_PER_DAY;
}

/**.......................................................................
 * Return the fractional part of the mjd day number as a double
 */
double TimeVal::getFractionalMjdDay()
{
  return (mjdSeconds_ + (double)(mjdNanoSeconds_) / NSEC_PER_SEC) 
    / SEC_PER_DAY;
}

/**.......................................................................
 * Set the time, as a double MJD.
 */
void TimeVal::setMjd(double mjd)
{
  unsigned long days = (unsigned long)mjd;
  unsigned long milliSeconds = (unsigned long)((mjd - days) * SEC_PER_DAY * MSEC_PER_SEC);

  setMjd(days, milliSeconds);
}

/**.......................................................................
 * Set the time, as an MJD.
 */
void TimeVal::setMjd(unsigned long days, unsigned long seconds, 
		     unsigned long nanoSeconds)
{
  unsigned long sec = (days - MJD0) * SEC_PER_DAY + seconds;

  // Update all internal time representations.

  setSeconds(sec);
  setNanoSeconds(nanoSeconds);
}

/**.......................................................................
 * Set the time, as an MJD.
 */
void TimeVal::setMjd(unsigned long days, unsigned long milliSeconds)
{
  unsigned long mjdSec = milliSeconds / MSEC_PER_SEC;
  unsigned long mjdNanoSec = (milliSeconds - mjdSec * MSEC_PER_SEC) * 
    NSEC_PER_MSEC;
  
  // Update all internal time representations.

  setMjd(days, mjdSec, mjdNanoSec);
}

/**.......................................................................
 * Get a unique identifier constructed as the integral number of
 * half-seconds since MJD0.
 */
unsigned int TimeVal::getMjdId(unsigned nanoSecondInterval)
{
  unsigned nIntervalPerSecond = NSEC_PER_SEC/nanoSecondInterval;

  double result = nIntervalPerSecond*((mjdDays_ - MJDID0) * SEC_PER_DAY + mjdSeconds_) +
    (double)(mjdNanoSeconds_)/nanoSecondInterval;

  return lrint(result);
}

/**.......................................................................
 * Return a human-readable string representation of the UTC time
 * managed by this object
 */
std::string TimeVal::getUtcString()
{
  time_t time;
  time = (time_t)getTimeInSeconds();

  std::string timeString(ctime(&time));

  // Append non-integral seconds to the time

  std::string::size_type idx;

  idx = timeString.find('\n');
  if(idx != std::string::npos)
    timeString.erase(idx);

  return timeString;
}

/**.......................................................................
 * Write the contents to an ostream
 */
ostream& 
sza::util::operator<<(ostream& os, TimeVal& tVal)
{
  RegDate date(tVal);
  //  os << tVal.getUtcString() << " (" << tVal.getMjdMilliSeconds() % 1000 << ")";

  os << date;
  return os;
}

std::string TimeVal::dateString()
{
  Astrometry::Date date = Astrometry::mjdUtcToCalendarDate(getMjd());

  std::ostringstream os;

  os << date.year_ 
     << setw(2) << setfill('0') << date.month_ 
     << setw(2) << setfill('0') << date.day_
     << "_" 
     << setw(2) << setfill('0') << date.hour_ 
     << setw(2) << setfill('0') << date.min_
     << setw(2) << setfill('0') << date.sec_;

  return os.str();
}
