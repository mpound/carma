// $Id: AngularRate.cc,v 1.3 2005/05/18 19:23:53 mpound Exp $

/**
 * @file carma/services/AngularRate.cc
 * Representation of AngularRate
 *
 * @author Marc Pound
 * @version $Revision: 1.3 $
 */

#include "carma/services/AngularRate.h"
#include "carma/services/ConformabilityException.h"

using namespace carma::services;
using namespace ::std;

AngularRate::AngularRate(double value, 
	           const std::string& units) : ConformableQuantity(value, units) {
}

AngularRate::~AngularRate() { }

const AngularRate AngularRate::operator+(const AngularRate& ar) const
{
    // convert both values to radians/s and add them.
    double val1 = convert("radians/s");
    double val2 = ar.convert("radians/s");
    double sum = val1 + val2;
    return AngularRate(sum,"radians/s");
}

const AngularRate AngularRate::operator-(const AngularRate& ar) const
{
    // convert both values to rad/s and subtract them.
    double val1 = convert("radians/s");
    double val2 = ar.convert("radians/s");
    double sum = val1 - val2;
    return AngularRate(sum,"radians/s");
}

AngularRate &AngularRate::operator+=(const AngularRate &ar) {
  double thisValue = convert("radians/s");
  thisValue += ar.convert("radians/s");
  reset(thisValue, "radians/s");
  return *this;
}

AngularRate &AngularRate::operator-=(const AngularRate &ar) {
  double thisValue = convert("radians/s");
  thisValue -= ar.convert("radians/s");
  reset(thisValue, "radians/s");
  return *this;
}

/**.......................................................................
 * Write the contents of this object to an ostream
 */
std::ostream& carma::services::operator<<( std::ostream& os, 
                                           const carma::services::AngularRate&
                                           ar )
{
  os << ar.getValue() << " " << ar.getUnits();
  return os;
}
