// $Id: Length.cc,v 1.5 2005/12/19 19:58:38 mpound Exp $

/**
 * @file carma/services/Length.cc
 * Representation of Length
 *
 * @author Marc Pound
 * @version $Revision: 1.5 $
 */

#include "carma/services/Length.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/StringUtils.h"

using namespace carma::services;
using namespace carma::util;
using namespace std;

Length::Length(double value, const std::string& units) 
    : ConformableQuantity(value, units)
{
    // solve the "AU" != "au" problem in units.dat by converting all units
    // to lowercase. (AU does not exist in units.dat, only au does)
    try {
	std::string ul = 
	    carma::util::StringUtils::lowASCIIAlphaNumericToLower(units);
	ConformableQuantity::reset(value,ul);
    } catch (const out_of_range& ex) { 
	// This exception occurs if high ASCII was used in the units string.
	// If so, then a ConformabilityException will be thrown
	// as soon as the user attempts to use this method.  So, defer
	// the exception until that happens.
    }
    
}


Length::~Length() { }

const Length Length::operator+(const Length& length) const
{
    // convert both values to km and add them.
    double val1 = convert("km");
    double val2 = length.convert("km");
    double sum = val1 + val2;
    return Length(sum,"km");
}

const Length Length::operator-(const Length& length) const
{
    // convert both values to km and subtract them.
    double val1 = convert("km");
    double val2 = length.convert("km");
    double sum = val1 - val2;
    return Length(sum,"km");
}

Length &Length::operator+=(const Length &length) {
  double thisValue = convert("km");
  thisValue += length.convert("km");
  reset(thisValue, "km");
  return *this;
}

Length &Length::operator-=(const Length &length) {
  double thisValue = convert("km");
  thisValue -= length.convert("km");
  reset(thisValue, "km");
  return *this;
}

/**.......................................................................
 * Write the contents of this object to an ostream
 */
std::ostream& carma::services::operator<<( std::ostream& os, 
	                                   const carma::services::Length& length )
{
  os << length.getValue() << " " << length.getUnits();
  return os;
}
