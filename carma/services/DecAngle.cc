
// $Id: DecAngle.cc,v 1.5 2005/12/12 22:01:41 mpound Exp $

/**
 * @file 
 *
 * @author Marc Pound
 * @version $Revision: 1.5 $
 */

#include "carma/services/Angle.h"
#include "carma/services/ConformabilityException.h"
#include "carma/services/DecAngle.h"
#include "carma/services/Units.h"

#include <iomanip>
#include <sstream>
#include <iostream>

using namespace carma::services;
using namespace std;

DecAngle::DecAngle(double value, const std::string& units) 
    : Angle(value,units)
{
    double radianVal = convert("radians");
    try {
      // try-catch so we can change the exception message
      reset(this->checkValue(radianVal), "radians");
    } catch ( ConformabilityException& ex ) {
	ostringstream os;
	os << "Input value ("
	   <<value 
	   << " " 
	   << units
	   << " = " 
	   << radianVal 
	   << " radians) is outside valid Declination range.";

	throw CARMA_EXCEPTION(ConformabilityException, os.str());
    }
}

DecAngle::~DecAngle() { }

std::string DecAngle::dms(int precision) const {
    return Angle::dms(false, precision);
}

double DecAngle::checkValue(double value) const
{

    // try to get the value within -90 to 90 degrees,
    // the valid declination range
    while (value > M_PI_2 ) { value -= 2*M_PI;}
    while (value < -M_PI_2 ) { value += 2*M_PI;}

    if ( value > M_PI_2 )
	throw CARMA_EXCEPTION(ConformabilityException,
		"Sum of declination angle is greater than 90 degrees");
    if ( value < -M_PI_2 )
	throw CARMA_EXCEPTION(ConformabilityException,
		"Sum of declination angle is less than -90 degrees");

    return value;

}

const DecAngle DecAngle::operator+(const DecAngle& angle) const
{
    // convert both values to radians and add them.
    double val1 = convert("radians");
    double val2 = angle.convert("radians");
    double sum = val1 + val2;
    checkValue(sum);
    return DecAngle(sum, "radians");
}

const DecAngle DecAngle::operator-(const DecAngle& angle) const
{
    // convert both values to radians and subtract them.
    double val1 = convert("radians");
    double val2 = angle.convert("radians");
    double sum = val1 - val2;
    checkValue(sum);
    return DecAngle(sum, "radians");
}

DecAngle& DecAngle::operator+=(const DecAngle& angle) {
  double thisValue = convert("radians");
  thisValue += angle.convert("radians");
  checkValue(thisValue);
  reset(thisValue, "radians");
  return *this;
}

DecAngle& DecAngle::operator-=(const DecAngle& angle) {
  double thisValue = convert("radians");
  thisValue -= angle.convert("radians");
  checkValue(thisValue);
  reset(thisValue, "radians");
  return *this;
}

/**.......................................................................
 * Write the contents of this object to an ostream
 */
std::ostream& carma::services::operator<<( std::ostream& os, 
	                                const carma::services::DecAngle& angle 
					)
{
  os << angle.getValue() << " " << angle.getUnits();
  return os;
}
