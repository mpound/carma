#include "carma/szautil/Frequency.h"
#include "carma/szautil/Wavelength.h"

using namespace std;
using namespace sza::util;

Speed Wavelength::lightSpeed_ = 
Speed(Speed::CentimetersPerSec(), 2.99792458e10);

const double Wavelength::cmPerAngstrom_ = 1.0e-10;

/**.......................................................................
 * Constructor.
 */
Wavelength::Wavelength() {
  initialize();
}

Wavelength::Wavelength(const Frequency& frequency)
{
  setFrequency(frequency);
}

Wavelength::Wavelength(const Length::Centimeters& units, double cm) {
  setCentimeters(cm);
}

Wavelength::Wavelength(const Microns& units, double microns) {
  setMicrons(microns);
}

/**.......................................................................
 * Destructor.
 */
Wavelength::~Wavelength() {};

void Wavelength::setMicrons(double microns) 
{
  cm_ = microns/micronsPerCm_;
}

void Wavelength::setAngstroms(double angstroms) 
{
  cm_ = angstroms * cmPerAngstrom_;
}

double Wavelength::microns() 
{
  return cm_ * micronsPerCm_;
}

double Wavelength::angstroms() 
{
  return cm_ / cmPerAngstrom_;
}

void Wavelength::initialize()
{
  setFinite(true);
  Length::initialize();
}

void Wavelength::setFrequency(Frequency& freq)
{
  if(freq.Hz() > 0.0) 
    setCentimeters(lightSpeed_.centimetersPerSec() / freq.Hz());
  else
    setFinite(false);
}

void Wavelength::setFrequency(const Frequency& freq)
{
  setFrequency((Frequency&) freq);
}
