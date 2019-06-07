#include "carma/szautil/Constants.h"
#include "carma/szautil/Frequency.h"
#include "carma/szautil/Wavelength.h"

#include <iomanip>
#include <cmath>
#include <cstring>

using namespace std;
using namespace sza::util;

Speed Frequency::lightSpeed_ = 
Speed(Speed::CentimetersPerSec(), 2.99792458e10);

const double Frequency::HzPerGHz_   = 1e9;
const double Frequency::HzPerMHz_   = 1e6;

/**.......................................................................
 * Constructor.
 */
Frequency::Frequency() 
{
  initialize();
}

/**.......................................................................
 * Constructor.
 */
Frequency::Frequency(const MegaHz& units, double MHz) 
{
  setMHz(MHz);
}

/**.......................................................................
 * Constructor.
 */
Frequency::Frequency(const GigaHz& units, double GHz) 
{
  setGHz(GHz);
}

/**.......................................................................
 * Constructor.
 */
Frequency::Frequency(Wavelength& wavelength) 
{
  setHz(lightSpeed_.centimetersPerSec() / wavelength.centimeters());
}

/**.......................................................................
 * Private constructor can only be called by Rx
 */
Frequency::Frequency(double Hz) 
{
  setHz(Hz);
}

/**.......................................................................
 * Destructor.
 */
Frequency::~Frequency() {}

// Set the frequency, in GHz

void Frequency::setGHz(double GHz)
{
  setHz(GHz * HzPerGHz_);
}

// Set the frequency, in MHz

void Frequency::setMHz(double MHz)
{
  setHz(MHz * HzPerMHz_);
}

// Set the frequency, in Hz

void Frequency::setHz(double Hz)
{
  Hz_     = Hz;
  finite_ = finite(Hz);
}

/**.......................................................................
 * Allows cout << Frequency
 */
std::ostream& sza::util::operator<<(std::ostream& os, Frequency& frequency)
{
  os << setw(18) << setprecision(12) << frequency.GHz() << " (GHz)";
  return os;
}

void Frequency::initialize()
{
  setHz(0.0);
}

/** .......................................................................
 * Subtract two Frequencys
 */
Frequency Frequency::operator-(Frequency& frequency)
{
  Frequency diff;
  diff.setHz(Hz_ - frequency.Hz());
  return diff;
}

/** .......................................................................
 * Add two Frequencys
 */
Frequency Frequency::operator+(Frequency& frequency)
{
  Frequency sum;
  sum.setHz(Hz_ + frequency.Hz());
  return sum;
}

/** .......................................................................
 * Compare two Frequencys
 */
bool Frequency::operator<(Frequency& frequency)
{
  return  Hz_ < frequency.Hz();
}

/** .......................................................................
 * Compare two Frequencys
 */
bool Frequency::operator>(Frequency& frequency)
{
  return  Hz_ > frequency.Hz();
}

void Frequency::setVal(double val, std::string units)
{
  if(strcasecmp(units.c_str(), "GHz")==0)
    setGHz(val);
  else if(strcasecmp(units.c_str(), "MHz")==0)
    setMHz(val);
  else if(strcasecmp(units.c_str(), "Hz")==0)
    setHz(val);
  else {
    ThrowError("Unrecognized unit: " << units << " (should be one of: GHz, MHz, Hz)");
  }
}

double Frequency::microns()
{
  return centimeters() * Wavelength::micronsPerCm_;
}

double Frequency::centimeters()
{
  return Constants::lightSpeed_.centimetersPerSec() / Hz_;
}

double Frequency::meters()
{
  return centimeters() / Wavelength::cmPerM_;
}

Wavelength Frequency::wavelength()
{
  Wavelength wave;
  wave.setCentimeters(Constants::lightSpeed_.centimetersPerSec() / Hz());
  return wave;
}
