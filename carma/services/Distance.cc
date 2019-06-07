// $Id: Distance.cc,v 1.6 2004/09/30 17:48:23 cgwon Exp $

/**
 * @file carma/services/Distance.cc
 * Representation of Distance
 *
 * @author Marc Pound
 * @version $Revision: 1.6 $
 */

#include "carma/services/Angle.h"
#include "carma/services/Distance.h"
#include "carma/util/IllegalArgumentException.h"

using namespace carma::services;
using namespace carma::util;
using namespace std;

Distance::Distance(double value, const std::string& units) 
    : Length(value, units)
{
    // distance less than zero are not allowed.
    if ( value < 0.0 )
	throw CARMA_EXCEPTION(IllegalArgumentException,
		"Distance less then zero.");

}


Distance::~Distance() { }

const Distance Distance::operator+(const Distance& distance) const
{
    // convert both values to km and add them.
    double val1 = convert("km");
    double val2 = distance.convert("km");
    double sum = val1 + val2;
    return Distance(sum,"km");
}


const Angle Distance::getParallax() const
{

    // take care of the zero distance, so we don't
    // get divide by zero.
    if ( isInfinite() ) {
	//Angle zero(0.0,"arcsec");
	return Angle (0.0,"arcsec");
    }

    // The parallax in arcsec = 1 /Distance[pc], 
    // so convert this distance to parsecs.
    double parallax = 1.0 / convert("parsec");

    // create the Angle object and return it
    //Angle a(parallax,"arcsec");
    return Angle(parallax,"arcsec");
}

Distance Distance::getDistance(const Angle& parallax) {

    if ( parallax.getValue() > 0.0 ) {
	// The reciprocal of the parallax in arcsec is 
	// distance in parsec.
        double dParsec = 1.0 / parallax.convert("arcsec");
	return *new Distance(dParsec,"parsec");
    } else {
	// If parallax is zero, then distance is infinite,
	// return an infinite distance.  If the parallax
	// was negative then something was wrong, in which
	// case, also return a zero distance.
	//
	// @todo Could throw IllegalArgumentException...could
	// make a Parallax class that only has positive angles, too.
	return *new Distance(0,"parsec");
    }
}

bool Distance::isInfinite() const {
    return (getValue() > 0 ? false : true);
}

const Distance Distance::operator-(const Distance& distance) const
{
    // convert both values to km and subtract them.
    double val1 = convert("km");
    double val2 = distance.convert("km");
    double sum = val1 - val2;
    return Distance(sum,"km");
}

Distance &Distance::operator+=(const Distance &distance) {
  double thisValue = convert("km");
  thisValue += distance.convert("km");
  reset(thisValue, "km");
  return *this;
}

Distance &Distance::operator-=(const Distance &distance) {
  double thisValue = convert("km");
  thisValue -= distance.convert("km");
  reset(thisValue, "km");
  return *this;
}

/**.......................................................................
 * Write the contents of this object to an ostream
 */
std::ostream& carma::services::operator<<( std::ostream& os, 
	                                   const carma::services::Distance& distance )
{
  os << distance.getValue() << " " << distance.getUnits();
  return os;
}
