#include "carma/szautil/Temperature.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

using namespace std;

using namespace sza::util;

const double Temperature::kelvinZeroPointInC_ = 273.15;

/**.......................................................................
 * Constructor.
 */
Temperature::Temperature() 
{
  initialize();
}

/**.......................................................................
 * Constructors.
 */
Temperature::Temperature(const Kelvin& units, double kelvin) 
{
  initialize();
  setK(kelvin);
}

Temperature::Temperature(const Centigrade& units, double centigrade) 
{
  initialize();
  setC(centigrade);
}

Temperature::Temperature(const Celsius& units, double celsius) 
{
  initialize();
  setC(celsius);
}

Temperature::Temperature(const Fahrenheit& units, double fahrenheit) 
{
  initialize();
  setF(fahrenheit);
}

/**.......................................................................
 * Destructor.
 */
Temperature::~Temperature() {}

void Temperature::setC(double centigrade)
{
  centigrade_ = centigrade;
}

void Temperature::setF(double fahrenheit)
{
  centigrade_ = (fahrenheit - 32.0) * 5.0/9;
}

void Temperature::setK(double kelvin)
{
  centigrade_ = kelvin - kelvinZeroPointInC_;
}

void Temperature::setKelvin(double kelvin)
{
  setK(kelvin);
}

void Temperature::setMilliKelvin(double milliKelvin)
{
  setK(milliKelvin/1000);
}

double Temperature::C()
{
  return centigrade_;
}

double Temperature::F()
{
  return (9.0/5 * centigrade_ + 32.0);
}

double Temperature::K()
{
  return centigrade_ + kelvinZeroPointInC_;
}

void Temperature::initialize()
{
  setC(0.0);
}

void Temperature::Kelvin::addNames()
{
  COUT("Calling real addNames() method");

  addName("Kelvin");
  addName("kelvin");
}

/**.......................................................................
 * Write the contents of this object to an ostream
 */
ostream& 
sza::util::operator<<(ostream& os, Temperature& temp)
{
  os << temp.K() << " K";
  return os;
}
