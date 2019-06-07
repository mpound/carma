// $Id: Pressure.cc,v 1.3 2014/04/02 23:11:12 iws Exp $

/**
 * @file 
 * Representation of Pressure
 *
 * @author Marc Pound
 * @version $Revision: 1.3 $
 */

#include "carma/services/Pressure.h"
#include "carma/services/Physical.h"
#include "carma/services/Units.h"
#include "carma/util/IllegalArgumentException.h"

using namespace carma::services;
using namespace carma::util;
using namespace std;

Pressure::Pressure(double value, const std::string& units) 
    : ConformableQuantity(value,units)
{
    // pressure less than zero not allowed
    if ( value < 0.0 )
	throw CARMA_EXCEPTION(IllegalArgumentException,
		"Pressure less than zero.");
}

Pressure::~Pressure() { }


double Pressure::millibar() const
{
    return convert("millibar");
}

double Pressure::atmosphere() const
{
    return convert("atm");
}

const Pressure Pressure::operator+(const Pressure& pressure) const
{
    // convert both values to mbar and add them.
    double val1 = convert("mbar");
    double val2 = pressure.convert("mbar");
    double sum = val1 + val2;

    return Pressure(sum, "mbar");
}

const Pressure Pressure::operator-(const Pressure& pressure) const
{
    // convert both values to mbar and subtract them.
    double val1 = convert("mbar");
    double val2 = pressure.convert("mbar");
    double sum = val1 - val2;

    return Pressure(sum, "Hz");
}

bool Pressure::operator<(const Pressure &pressure) const {
  double val1 = convert("mbar");
  double val2 = pressure.convert("mbar");
  
  if (val1 < val2) return true;

  return false;
}

bool Pressure::operator>(const Pressure &pressure) const {
  double val1 = convert("mbar");
  double val2 = pressure.convert("mbar");
  
  if (val1 > val2) return true;

  return false;
}

Pressure& Pressure::operator+=(const Pressure& pressure) {
  double thisValue = convert("mbar");
  thisValue += pressure.convert("mbar");
  reset(thisValue, "mbar");
  return *this;
}

Pressure& Pressure::operator-=(const Pressure& pressure) {
  double thisValue = convert("mbar");
  thisValue -= pressure.convert("mbar");
  reset(thisValue, "mbar");
  return *this;
}

/**
 * Write the contents of this object to an ostream
 */
std::ostream& carma::services::operator<<( 
	std::ostream& os, 
	const carma::services::Pressure& pressure 
				          )
{
  os << pressure.getValue() << " " << pressure.getUnits();
  return os;
}
