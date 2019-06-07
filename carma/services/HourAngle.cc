
// $Id: HourAngle.cc,v 1.19 2008/10/14 15:36:26 scott Exp $

/**
 * @file carma/services/HourAngle.cc
 * Representation of HourAngle
 *
 * @author Marc Pound
 * @version $Revision: 1.19 $
 */

#include "carma/services/HourAngle.h"
#include "carma/services/ConformableQuantity.h"

#include <stdexcept>
#include <iomanip>

#include "carma/util/StringUtils.h"

using namespace carma::services;
using namespace carma::util;
using namespace ::std;

HourAngle::HourAngle(double value, const std::string& units) 
    : Angle(value, units), HOURS_("hours")
{
  // use this check to see if "hours" was passed through
  reset(value, units);
} 

HourAngle::~HourAngle() { }

const HourAngle HourAngle::operator+(const HourAngle& hourangle) const
{
    // convert both values to radians and add them.
    double val1 = convert("radians");
    double val2 = hourangle.convert("radians");
    double sum = val1 + val2;
    //std::cout.precision(16);
    //std::cout << "\nHourAngle val1: " << val1 << std::endl;
    //std::cout << "HourAngle val2: " << val2 << std::endl;
    //std::cout << "HourAngle sum : " << sum << std::endl;
    return moduloPi(sum);

}

const HourAngle HourAngle::operator-(const HourAngle& hourangle) const
{
    // convert both values to radians and subtract them.
    double val1 = convert("radians");
    double val2 = hourangle.convert("radians");
    double diff = val1 - val2;
    return moduloPi(diff);

}

HourAngle& HourAngle::operator+=(const HourAngle& angle) {
  double thisValue = convert("radians");
  thisValue += angle.convert("radians");
  reset(thisValue, "radians");
  return *this;
}

HourAngle& HourAngle::operator-=(const HourAngle& angle) {
  double thisValue = convert("radians");
  thisValue -= angle.convert("radians");
  reset(thisValue, "radians");
  return *this;
}

double HourAngle::convert(const std::string& convertTo) const {

    // if convertTo is "hours" we must make it "circle (hours/day)"
    if ( isHours(convertTo) )
	return hours();
    else
	return ConformableQuantity::convert(convertTo);
}

double HourAngle::hours() const {
    //double value24 = convert( hourUnits() );
    //return moduloPiDouble(value24);
    return convert( hourUnits() );
}

/**
 * Handle the special case of when units_ is hourUnits.
 */
std::string HourAngle::getUnits() const {
    std::string units = ConformableQuantity::getUnits();
    if ( units == hourUnits() )
	return HOURS_;
    else 
	// avoid reference to local variable
    return ConformableQuantity::getUnits();
}

// Following previous usage with unsigned output, but think that would
// normally want a signed output or an hour angle.
std::string HourAngle::getString(int precision) {
  return StringUtils::hms(radians(), precision);
}

std::string HourAngle::hms(double decimalHours, int precision)
{
    // Code previously forced positive value for hours, 
    // so we duplicate that in rewrite...
    if (decimalHours < 0) decimalHours += 24.0;
    HourAngle ha(decimalHours, "hours");
    return ha.getString(precision);
}

void HourAngle::reset(double value, const std::string &units) {
  if( isHours(units) ) ConformableQuantity::reset(value, hourUnits());
}

bool HourAngle::isHours(const std::string& units) const {
    try {
	std::string ul = 
	    carma::util::StringUtils::lowASCIIAlphaNumericToLower(units);
        //cout << "HA CONSTRUCTOR : " << value << " " 
	//     << units << " tolower: " << ul << endl;
	if ( ( ul == "hours" ) ||
	     ( ul == "hour"  ) ||
	     ( ul == "hrs"   ) ||
	     ( ul == "hr"    )
	   ) {
	    //cout << "RESETTING " << units << " to " << hourUnits() << endl;
	    return true;
        } else {
	    //cout << "DID NOT TO CHANGE UNITS" <<endl;
	    return false;
	}
    } catch (const out_of_range& ex) { 
	// This exception occurs if high ASCII was used in the units string.
	// If so, then a ConformabilityException will be thrown
	// as soon as the user attempts to use this method.  So, defer
	// the exception until that happens.
	    return false;
    }
}

double HourAngle::moduloPiDouble(double value) const
{
    while(value > M_PIl) 
	value -= twoPi_;
    while(value < -M_PIl)
	value += twoPi_;
    return value;
}

HourAngle HourAngle::moduloPi(double value) const
{
    double modVal = moduloPiDouble(value);
    // now make the units be "circle" hours
    std::string hu = hourUnits(); 
    const char *cp = hu.c_str();
    HourAngle ha( u_.convert(modVal,"radians",cp), hourUnits() );
    return ha;
}

/**.......................................................................
 * Write the contents of this object to an ostream
 */
std::ostream& carma::services::operator<<(std::ostream& os, 
	                                  const carma::services::HourAngle& 
					                   hourangle)
{
  os << hourangle.hours() << " hours ";
  return os;
}
