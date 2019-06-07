#include "carma/szautil/Attenuation.h"

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
Attenuation::Attenuation() 
{
  initialize();
}

Attenuation::Attenuation(const dBUnit& units, double dB)
{
  setdB(dB);
}

/**.......................................................................
 * Destructor.
 */
Attenuation::~Attenuation() {}

void Attenuation::setdB(double dB)
{
  dB_ = dB;
}

void Attenuation::initialize()
{
  setdB(0.0);
}

