#include "carma/szautil/Length.h"
#include "carma/szautil/Speed.h"

using namespace std;

using namespace sza::util;

const unsigned Speed::secPerHour_  =     3600;
const double Speed::metersPerMile_ = 1609.344;

/**.......................................................................
 * Constructor.
 */
Speed::Speed() 
{
  initialize();
}

Speed::Speed(const CentimetersPerSec& units, double cmPerSec)
{
  setCentimetersPerSec(cmPerSec);
}

Speed::Speed(const MetersPerSec& units, double mPerSec)
{
  setMetersPerSec(mPerSec);
}

/**.......................................................................
 * Destructor.
 */
Speed::~Speed() {}

void Speed::setCentimetersPerSec(double cmPerSec)
{
  cmPerSec_ = cmPerSec;
}

void Speed::setMetersPerSec(double mPerSec)
{
  cmPerSec_ = mPerSec * Length::cmPerM_;
}

double Speed::centimetersPerSec()
{
  return cmPerSec_;
}

double Speed::metersPerSec()
{
  return cmPerSec_ / Length::cmPerM_;
}

void Speed::initialize()
{
  setCentimetersPerSec(0.0);
}

void Speed::setMilesPerHour(double mph)
{
  double cmPerMile = metersPerMile_ * 100;
  setCentimetersPerSec((mph * cmPerMile) / secPerHour_);
}


double Speed::mph()
{
  double cmPerMile = metersPerMile_ * 100;
  return (cmPerSec_ * secPerHour_) / cmPerMile;
}
