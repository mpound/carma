#include "carma/szautil/Time.h"

#include <iomanip>

using namespace std;

using namespace sza::util;

const double Time::nsPerSecond_  = 1e9;
const double Time::secPerMinute_ = 60;
const double Time::secPerHour_   = 3600;
const double Time::secPerDay_    = 86400;
const double Time::dayPerYear_   = 365;

/**.......................................................................
 * Constructor.
 */
Time::Time() 
{
  initialize();
}

/**.......................................................................
 * Copy constructor.
 */
Time::Time(const Time& time) 
{
  setSeconds(time.seconds());
}

Time::Time(const Seconds& units, double s)
{
  setSeconds(s);
}

Time::Time(const NanoSeconds& units, double ns)
{
  setNanoSeconds(ns);
}

/**.......................................................................
 * Destructor.
 */
Time::~Time() {}

void Time::initialize()
{
  setSeconds(0.0);
}

std::ostream& sza::util::operator<<(std::ostream& os, Time& time)
{
  if(time.years() > 1.0) {
    os << std::setw(5) << std::setprecision(3) << time.years() << " years";
  } else if(time.days() > 1.0) {
    os << std::setw(5) << std::setprecision(3) << time.days() << " days";
  } else if(time.hours() > 1.0) {
    os << std::setw(5) << std::setprecision(3) << time.hours() << " hours";
  } else if(time.minutes() > 1.0) {
    os << std::setw(5) << std::setprecision(3) << time.minutes() << " min";
  } else {
    os << std::setw(5) << std::setprecision(3) << time.seconds() << " sec";
  }
  
  return os;
}
