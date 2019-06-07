#include "carma/szautil/Voltage.h"

#include <iomanip>
#include <cmath>

using namespace std;
using namespace sza::util;

const double Voltage::centiVoltsPerVolt_ =  100;
const double Voltage::milliVoltsPerVolt_ = 1000;

/**.......................................................................
 * Constructor.
 */
Voltage::Voltage() 
{
  initialize();
}

/**.......................................................................
 * Constructor.
 */
Voltage::Voltage(const Volts& units, double volts) 
{
  setVolts(volts);
}

Voltage::~Voltage() {};

Voltage::Voltage(const CentiVolts& units, double centiVolts)
{
  setCentiVolts(centiVolts);
}

/**.......................................................................
 * Constructor.
 */
Voltage::Voltage(const MilliVolts& units, double milliVolts) 
{
  setMilliVolts(milliVolts);
}

/**.......................................................................
 * Set the voltage, in volts
 */
void Voltage::setVolts(double volts)
{
  volts_ = volts;
}

/**.......................................................................
 * Set the voltage, in centiVolts
 */
void Voltage::setCentiVolts(double centiVolts)
{
  setVolts(centiVolts / centiVoltsPerVolt_);
}

/**.......................................................................
 * Set the voltage, in milliVolts
 */
void Voltage::setMilliVolts(double milliVolts)
{
  setVolts(milliVolts / milliVoltsPerVolt_);
}

/**.......................................................................
 * Allows cout << Voltage
 */
std::ostream& sza::util::operator<<(std::ostream& os, Voltage& voltage)
{
  os << setw(18) << setprecision(12) << voltage.volts() << " (volts)";
  return os;
}

void Voltage::initialize()
{
  setVolts(0.0);
}
