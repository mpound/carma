// $Id: Frequency.cc,v 1.10 2014/04/02 23:11:12 iws Exp $

/**
 * @file carma/services/Frequency.cc
 * Representation of Frequency
 *
 * @author Marc Pound
 * @version $Revision: 1.10 $
 */

#include "carma/services/Frequency.h"
#include "carma/services/Physical.h"
#include "carma/services/Units.h"
#include "carma/util/IllegalArgumentException.h"

using namespace carma::services;
using namespace carma::util;
using namespace std;

Frequency::Frequency(double value, const std::string& units) 
    : ConformableQuantity(value,units)
{
    // frequency less than zero not allowed
    if ( value < 0.0 )
	throw CARMA_EXCEPTION(IllegalArgumentException,
		"Frequency less then zero.");
}

Frequency::~Frequency() { }


double Frequency::hertz() const
{
    return convert("Hz");
}

double Frequency::gigahertz() const
{
    return convert("GHz");
}

double Frequency::megahertz() const
{
    return convert("MHz");
}

double Frequency::millihertz() const
{
    return convert("millihertz");
}

double Frequency::wavelength(const std::string& units) const
{
    // If we ask for reciprocal units, i.e. the
    // requested units  divided by c (speed o' light)
    // the conversion comes out correct.  Note we could also
    // convert Physical::C/getValue() in " (m/s) /"+getUnits() to 
    // the requested units.  However, that adds an additional dependency
    // on Physical.cc.  And if for some reason Physical::C changes
    // from m/s to cm/s or whatever, we'd have to remember to change
    // this function, too.  Best to let GNU units handle it all
    // since it knows what "c" means.
    
    std::string reciprocalUnits_ = units + " / c";
    return convert( (char *)(reciprocalUnits_.c_str()) );
}

const Frequency Frequency::operator+(const Frequency& frequency) const
{
    // convert both values to hz and add them.
    double val1 = convert("Hz");
    double val2 = frequency.convert("Hz");
    double sum = val1 + val2;

    return Frequency(sum, "Hz");
}

const Frequency Frequency::operator-(const Frequency& frequency) const
{
    // convert both values to radians and subtract them.
    double thisValue = convert("Hz");
    double otherValue = frequency.convert("Hz");
    double sum = thisValue - otherValue;

    // throw exception if it's going to lead to a negative frequency
    if ( sum < 0.0 )
      throw CARMA_EXCEPTION(IllegalArgumentException,
			    "Resulting subtraction will lead to negative frequency");

    return Frequency(sum, "Hz");
}

bool Frequency::operator<(const Frequency &frequency) const {
  double val1 = convert("Hz");
  double val2 = frequency.convert("Hz");
  
  if (val1 < val2) return true;

  return false;
}

bool Frequency::operator>(const Frequency &frequency) const {
  double val1 = convert("Hz");
  double val2 = frequency.convert("Hz");
  
  if (val1 > val2) return true;

  return false;
}

Frequency& Frequency::operator+=(const Frequency& frequency) {
  double thisValue = convert("Hz");
  thisValue += frequency.convert("Hz");
  reset(thisValue, "Hz");
  return *this;
}

Frequency& Frequency::operator-=(const Frequency& frequency) {
  double thisValue = convert("Hz");
  double appenderValue = frequency.convert("Hz");

  // only allow positive frequencies
  if ( thisValue > appenderValue ) {
    thisValue -= appenderValue;
    reset(thisValue, "Hz");
    return *this;
  } else {
    throw CARMA_EXCEPTION(IllegalArgumentException,
			  "Resulting decrement will lead to negative frequency");
  }
}

/**
 * Write the contents of this object to an ostream
 */
std::ostream& carma::services::operator<<( 
	std::ostream& os, 
	const carma::services::Frequency& frequency 
				          )
{
  os << frequency.getValue() << " " << frequency.getUnits();
  return os;
}
