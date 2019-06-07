// $Id: Temperature.cc,v 1.6 2014/04/02 23:11:12 iws Exp $

/**
 * @file carma/services/Temperature.cc
 * Representation of Temperature
 *
 * @author Peter Teuben
 * @version $Revision: 1.6 $
 */

#include "carma/services/Temperature.h"
#include "carma/services/Physical.h"
#include "carma/services/Units.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/StringUtils.h"
#include <sstream>

using namespace carma::services;
using namespace carma::util;
using namespace std;

Temperature::Temperature(double value, const std::string& units) 
    : ConformableQuantity(value,units)
{
    // The units library requires the prefix/function "temp" to do
    // the nonlinear temperature scale conversions.
    // (The prefix "deg" does temperature difference conversions).
    // See units man page.
    
    // Store everything internally as Kelvin.
    Units u;
    // Default this to the input value in case the units are actually
    // some form of kelvin
    double newValue = value;

    // Deal with C, F, R (rankine)
    if ( units == "C" || units == "F" || units == "R" ) {
	std::ostringstream tostr;
	tostr << "temp"<<units<<"("<<value<<")";
	newValue = u.convert(1.0,tostr.str().c_str(),"K");
    }

    string ul = StringUtils::lowASCIIAlphaNumericToLower(units);

    if ( ul == "celsius" || ul == "centigrade" )
    {
	std::ostringstream tostr;
	tostr << "tempC("<<value<<")";
	newValue = u.convert(1.0,tostr.str().c_str(),"K");
    }

    if ( ul == "fahrenheit" )
    {
	std::ostringstream tostr;
	tostr << "tempF("<<value<<")";
	newValue = u.convert(1.0,tostr.str().c_str(),"K");
    }

    if ( ul == "rankine" )
    {
	std::ostringstream tostr;
	tostr << "tempR("<<value<<")";
	newValue = u.convert(1.0,tostr.str().c_str(),"K");
    }

    if ( newValue < 0.0 ) 
	throw CARMA_EXCEPTION(carma::util::IllegalArgumentException,
		"Temperature less than 0 K");

    ConformableQuantity::reset(newValue,"K");

}

Temperature::~Temperature() { }

// override parent class
void
Temperature::reset(double value, const std::string& units)
{
    // The units library requires the prefix/function "temp" to do
    // the nonlinear temperature scale conversions.
    // (The prefix "deg" does temperature difference conversions).
    // See units man page.
    
    // Store everything internally as Kelvin.
    Units u;
    // Default this to the input value in case the units are actually
    // some form of kelvin
    double newValue = value;
    
    // Deal with C, F, R (rankine)
    if ( units == "C" || 
	 units == "F" || 
	 units == "R" ) {
	std::ostringstream tostr;
	tostr << "temp"<<units<<"("<<value<<")";
	newValue = u.convert(1.0,tostr.str().c_str(),"K");
    }

    string ul = StringUtils::lowASCIIAlphaNumericToLower(units);

    if ( ul == "celsius" || 
	 ul == "centigrade" )
    {
	std::ostringstream tostr;
	tostr << "tempC("<<value<<")";
	newValue = u.convert(1.0,tostr.str().c_str(),"K");
    }

    if ( ul == "fahrenheit" )
    {
	std::ostringstream tostr;
	tostr << "tempF("<<value<<")";
	newValue = u.convert(1.0,tostr.str().c_str(),"K");
    }

    if ( ul == "rankine" )
    {
	std::ostringstream tostr;
	tostr << "tempR("<<value<<")";
	newValue = u.convert(1.0,tostr.str().c_str(),"K");
    }

    if ( newValue < 0.0 ) 
	throw CARMA_EXCEPTION(carma::util::IllegalArgumentException,
		"Temperature less than 0 K");

    ConformableQuantity::reset(newValue,"K");
}

double Temperature::celsius() const
{
    return convert("tempC");
}

double Temperature::fahrenheit() const
{
    return convert("tempF");
}

double Temperature::kelvin() const
{
    return convert("K");
}

const Temperature Temperature::operator+(const Temperature& t) const
{
    // NB: Do we need to convert here since the constructor always
    // converts to K?
    // convert both values to K and add them.
    double val1 = convert("K");
    double val2 = t.convert("K");
    double sum = val1 + val2;
    if (sum<0) throw 
	CARMA_EXCEPTION(IllegalArgumentException,"Temperature < 0K in +");

    return Temperature(sum, "K");
}

const Temperature Temperature::operator-(const Temperature& t) const
{
    // NB: Do we need to convert here since the constructor always
    // converts to K?
    // convert both values to K and subtract them.
    double val1 = convert("K");
    double val2 = t.convert("K");
    double sum = val1 - val2;
    if (sum<0) throw 
	CARMA_EXCEPTION(IllegalArgumentException,"Temperature < 0K in -");

    return Temperature(sum, "K");
}

bool Temperature::operator<(const Temperature &t) const {
  double val1 = convert("K");
  double val2 = t.convert("K");
    // NB: Do we need to convert here since the constructor always
    // converts to K?
  
  if (val1 < val2) return true;

  return false;
}

bool Temperature::operator>(const Temperature &t) const {
  double val1 = convert("K");
  double val2 = t.convert("K");
    // NB: Do we need to convert here since the constructor always
    // converts to K?
  if (val1 > val2) return true;

  return false;
}

Temperature& Temperature::operator+=(const Temperature& t) {
    // NB: Do we need to convert here since the constructor always
    // converts to K?
  double thisValue = convert("K");
  thisValue += t.convert("K");
  reset(thisValue, "K");
  return *this;
}

Temperature& Temperature::operator-=(const Temperature& t) {
    // NB: Do we need to convert here since the constructor always
    // converts to K?
  double thisValue = convert("K");
  thisValue -= t.convert("K");
  reset(thisValue, "K");
  return *this;
}

/**
 * Write the contents of this object to an ostream
 */
std::ostream& carma::services::operator<<( 
			std::ostream& os, const carma::services::Temperature& t
				          )
{
  os << t.getValue() << " " << t.getUnits();
  return os;
}
