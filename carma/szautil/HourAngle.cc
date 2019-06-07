#include "carma/szautil/Exception.h"
#include "carma/szautil/HourAngle.h"
#include "carma/szautil/LogStream.h"

using namespace std;
using namespace sza::util;

const double HourAngle::hourPerRad_   = 12/M_PI;
const double HourAngle::arcSecPerRad_ = 3600*12/M_PI;
const double HourAngle::arcSecPerSec_ = 24.0/23.93447192;

/*
 * Destructor.
 */
HourAngle::~HourAngle() {}

/**.......................................................................
 * Write the contents of this object to an ostream
 */
ostream& 
sza::util::operator<<(ostream& os, HourAngle& hour)
{
  os << hour.doubleToSexagesimal(hour.hours());
  return os;
}

/**.......................................................................
 * Addition operator for HourAngle.
 */
HourAngle HourAngle::operator+(HourAngle& angle)
{
  HourAngle sum;
  sum.setRadians(radians_);
  sum.addRadians(angle.radians());
  return sum;
}

/**.......................................................................
 * Subtraction operator for HourAngle.
 */
HourAngle HourAngle::operator-(HourAngle& angle)
{
  HourAngle diff;
  diff.setRadians(radians_);
  diff.addRadians(-angle.radians());
  return diff;
}

void HourAngle::setHours(double hours)
{
  setRadians(hours / hourPerRad_);
}

void HourAngle::setHours(std::string hours)
{
  setRadians(sexagesimalToDouble(hours) / hourPerRad_);
}

void HourAngle::setHours(double hr, double min, double sec)
{
  double hours = hr + (min + sec/60)/60;
  setHours(hours);
}
