// $Id: ConformableQuantity.cc,v 1.7 2009/09/10 16:32:31 mpound Exp $

/**
 * @file carma/services/ConformableQuantity.cc
 * Representation of ConformableQuantity
 *
 * @author Marc Pound
 * @version $Revision: 1.7 $
 */

#include "carma/services/ConformableQuantity.h"
#include "carma/services/Units.h"

using namespace carma::services;
using namespace std;

ConformableQuantity::ConformableQuantity(double value, 
	                                 const std::string& units) 
    : value_(value),units_(units)
{
// nothing else needed here
}

ConformableQuantity::~ConformableQuantity() { }

double 
ConformableQuantity::convert(const std::string& convertTo) const
{
    // NB: use units_ here, not getUnits() because subclasses
    // may have overridden getUnits() (e.g HourAngle) for
    // special cases.
    // Note we must return u_.convert(double,char*,char*) 
    // here and NOT NOT NOT value_ * u_.convert(char*, char*) because
    // we support conversion of reciprocal units in Units.cc,
    // and value_*u_.convert(char*, char*) would be wrong in that case.

    // speed things up if the units to convert to are the same
    // as the internal units.

    if ( convertTo == units_ ) return value_;

    return u_.convert(value_,
	              (char *)(units_.c_str()),
	              (char *)(convertTo.c_str())
		     );

}

std::string
ConformableQuantity::getUnits() const 
{
    return units_;
}

void 
ConformableQuantity::reset(double value, const std::string& units) 
{
    value_ = value;
    units_ = units;
}

const ConformableQuantity 
ConformableQuantity::operator+(const ConformableQuantity& quantity) const
{
    // convert both values to the current units and add them.
    double val2 = quantity.convert( getUnits() );
    double sum = getValue() + val2;
    // See Eckel 2nd Edition, pp 507-508 on why you want this form
    return ConformableQuantity( sum, getUnits() );
}

const ConformableQuantity 
ConformableQuantity::operator-(const ConformableQuantity& quantity) const
{
    // convert both values to the current units and subtract them.
    double val2 = quantity.convert( getUnits() );
    double diff = getValue() - val2;
    // See Eckel 2nd Edition, pp 507-508 on why you want this form
    return ConformableQuantity( diff, getUnits() );
}

ConformableQuantity&
ConformableQuantity::operator+=(const ConformableQuantity& quantity) {
  double thisValue = getValue();
  string thisUnits = getUnits();
  thisValue += quantity.convert(thisUnits);
  reset(thisValue, thisUnits);
  return *this;
}

ConformableQuantity&
ConformableQuantity::operator-=(const ConformableQuantity& quantity) {
  double thisValue = getValue();
  string thisUnits = getUnits();
  thisValue -= quantity.convert(thisUnits);
  reset(thisValue, thisUnits);
  return *this;
}

/*
 * MUST MODIFY THESE TO DO INTERVAL DOUBLE COMPARES.
bool
ConformableQuantity::operator==(const ConformableQuantity& quantity) {
  // first check that the quantities are actually conformable
  double thisValue = getValue();
  string thisUnits = getUnits();
  double thatValue = quantity.convert(thisUnits);
  return ( thisValue == thatValue );
}

bool
ConformableQuantity::operator!=(const ConformableQuantity& quantity) {
  // first check that the quantities are actually conformable
  double thisValue = getValue();
  string thisUnits = getUnits();
  double thatValue = quantity.convert(thisUnits);
  return ( thisValue != thatValue );
}

bool
ConformableQuantity::operator<(const ConformableQuantity& quantity) {
  double thisValue = getValue();
  string thisUnits = getUnits();
  double thatValue = quantity.convert(thisUnits);
  return ( thisValue < thatValue );
}

bool
ConformableQuantity::operator<=(const ConformableQuantity& quantity) {
  double thisValue = getValue();
  string thisUnits = getUnits();
  double thatValue = quantity.convert(thisUnits);
  return ( thisValue <= thatValue );
}
*/


/**.......................................................................
 * Write the contents of this object to an ostream
 */
std::ostream& carma::services::operator<<(std::ostream& os, 
	                 ConformableQuantity& quantity)
{
  os << quantity.getValue() << " " << quantity.getUnits();
  return os;
}
