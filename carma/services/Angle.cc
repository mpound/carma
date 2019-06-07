// $Id: Angle.cc,v 1.24 2008/10/14 15:39:01 scott Exp $

/**
 * @file carma/services/Angle.cc
 * Representation of Angle
 *
 * @author Marc Pound
 * @version $Revision: 1.24 $
 */

#include "carma/services/Angle.h"
#include "carma/util/StringUtils.h"

#include <iomanip>
#include <sstream>
#include <iostream>

using namespace carma;
using namespace carma::services;
using namespace carma::util;
using namespace std;

namespace {
    
    bool
    anglesEqualEnough( const Angle & left, const Angle & right ) 
    {
        // We define 'equal enough' as within the resolution of our 29 bit 
        // encoders, about 2.41 mas.
        const double epsInDeg = 1.0 / ( pow( 2.0, 29.0 ) );

        if ( abs( left.degrees( false ) - right.degrees( false ) ) < epsInDeg )
            return true;
        else
            return false;
    }

} // namespace <unnamed>

const std::string Angle::RADIANS_STR = "radians";
const std::string Angle::DEGREES_STR = "degrees";
const std::string Angle::HOURS_STR   = "hours";
const std::string Angle::ARCSEC_STR  = "arcseconds";
const std::string Angle::ARCMIN_STR  = "arcminutes";

// WARNING: Use "radians" string literal instead of RADIANS_STR.  Since
// RADIANS_STR is static, it may not be initialized if you use Angle
// instances in file static memory (e.g. Angle constants in unnamed namespaces).
Angle::Angle( )
    : ConformableQuantity( 0.0, "radians" ) 
{
// nothing else needed here
}

Angle::Angle(double value, const std::string& units) 
    : ConformableQuantity(value,units)
{
// nothing else needed here
}

Angle::~Angle() { }


double Angle::radians(bool modulo) const
{
    if ( modulo )
        return moduloTwoPiDouble(convert(RADIANS_STR));
    else
        return convert(RADIANS_STR);
}

double Angle::hours(bool modulo) const
{
    return degrees(modulo)/15.0;
}

double Angle::degrees(bool modulo) const
{
    if (modulo) {
	double radianValue = radians(true);
	return ( radianValue * u_.convert(RADIANS_STR, DEGREES_STR) );
    }
    else
        return convert(DEGREES_STR);
}

double Angle::arcSeconds(bool modulo) const
{
    if (modulo) {
	double radianValue = radians(true);
	return ( radianValue * u_.convert(RADIANS_STR,ARCSEC_STR) );
    } 
    else 
    return convert(ARCSEC_STR);
}

double Angle::arcMinutes(bool modulo) const
{
    if (modulo) {
	double radianValue = radians(true);
	return ( radianValue * u_.convert(RADIANS_STR,ARCMIN_STR) );
    } 
    else 
    return convert(ARCMIN_STR);
}

std::string Angle::dms(bool modulo, int precision) const
{
    double radianVal = radians (modulo);
    return getAngleString(radianVal, precision, true, true);
}

const Angle Angle::moduloTwoPi(double value) const
{
    // force to be between 0 and 2PI
    while (value >= twoPi_)
       value -= twoPi_;
    while (value < 0.0)
       value += twoPi_;
    // See Eckel, 2nd Edition, pp507-508
    return Angle(value, RADIANS_STR);
}

double Angle::moduloTwoPiDouble(double value) const
{
    // force to be between 0 and 2PI
    while (value >= twoPi_) 
       value -= twoPi_;
    while (value < 0.0) 
       value += twoPi_;
    return value;
}

const Angle Angle::operator+(const Angle& angle) const
{
    // convert both values to radians and add them.
    double val1 = convert(RADIANS_STR);
    double val2 = angle.convert(RADIANS_STR);
    double sum = val1 + val2;
    // See Eckel, 2nd Edition, pp507-508
    return Angle( sum, RADIANS_STR);
}

const Angle Angle::operator-(const Angle& angle) const
{
    // convert both values to radians and subtract them.
    double val1 = convert(RADIANS_STR);
    double val2 = angle.convert(RADIANS_STR);
    double sum = val1 - val2;
    // See Eckel, 2nd Edition, pp507-508
    return Angle( sum, RADIANS_STR);
}

const Angle services::operator*(const double left, const Angle & right ) 
{
    return Angle( left * right.radians( false ), Angle::RADIANS_STR );
}

const Angle services::operator*( const Angle & left, const double right )
{
    return Angle( left.radians( false ) * right, Angle::RADIANS_STR );
}

const Angle Angle::operator/(const double scalar) const
{
    return Angle( convert(RADIANS_STR) / scalar, RADIANS_STR );
}

Angle& Angle::operator+=(const Angle& angle) {
  double thisValue = convert(RADIANS_STR);
  thisValue += angle.convert(RADIANS_STR);
  reset(thisValue, RADIANS_STR);
  return *this;
}

Angle& Angle::operator-=(const Angle& angle) {
  double thisValue = convert(RADIANS_STR);
  thisValue -= angle.convert(RADIANS_STR);
  reset(thisValue, RADIANS_STR);
  return *this;
}

Angle & Angle::operator*=(const double scalar) {
    reset( scalar * convert(RADIANS_STR), RADIANS_STR );
    return *this;
}

Angle & Angle::operator/=(const double scalar) {
    reset( convert(RADIANS_STR) / scalar, RADIANS_STR );
    return *this;
}

bool Angle::operator<( const Angle & right ) const
{
    return ( radians( false ) < right.radians( false ) );
}

bool Angle::operator>( const Angle & right ) const
{
    return ( radians( false ) > right.radians( false ) );
}

bool Angle::operator<=( const Angle & right ) const
{
    if ( anglesEqualEnough( *this, right ) ) {
        return true;
    }

    return ( radians( false ) <= right.radians( false ) );
}

bool Angle::operator>=( const Angle & right ) const
{
    if ( anglesEqualEnough( *this, right ) ) {
        return true;
    }

    return ( radians( false ) >= right.radians( false ) );
}

#if 0
// we could also think about doing it to the class value  self?
string Angle::getAngleString(int precision, bool dms)
{
  return getAngleString( convert(RADIANS_STR), precision, dms);
}
#endif

string Angle::getAngleString(double angleRadians, int precision, bool dms,
	bool useSign)
{
    if (useSign) {
        if (dms) return StringUtils::sdms(angleRadians, precision);
        else     return StringUtils::shms(angleRadians, precision);
    }
    else {
        if (dms) return StringUtils::dms(angleRadians, precision);
        else     return StringUtils::hms(angleRadians, precision);
    }
    return "getAngleString(): serious programming error";
    
    // -------------------- Old code -----------------------
    // Delete after 01 December, 2008 - Steve Scott
     int sign = 1;
     int dd,mm;
     double ss;
     if (angleRadians < 0) {
         sign   = -1;
         angleRadians *= -1;
    }
    angleRadians *= (dms ? 180 : 12) / M_PI; // convert the input radians to degrees
    dd = (int) angleRadians;
    angleRadians = (angleRadians-dd)*60;
    mm = (int) angleRadians;
    ss = (angleRadians-mm)*60;
    dd = sign*dd;
    int width = precision+3;
    if (precision == 0)width = 2;
    ostringstream os;
    os.setf(ios::fixed);
    os.setf(ios::internal);
    os.fill('0');
    if (useSign && sign == 1) os << "+";  //or os << setiosflags(showpos)
    if (dd == 0 && sign < 0) os << "-";
    os << (dd < 0 ? setw(3) : setw(2) ) << dd << ":"
       << setw(2) << mm << ":"
       << setw(width) << setprecision(precision) << ss;
   return os.str();
}


/**.......................................................................
 * Write the contents of this object to an ostream
 */
std::ostream& carma::services::operator<<( std::ostream& os, 
	                                   const carma::services::Angle& angle )
{
  os << angle.getValue() << " " << angle.getUnits();
  return os;
}
