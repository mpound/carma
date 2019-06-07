/**
 * @file
 *
 * Implementation of the specializations of a monitor point for
 * different datatypes.
 *
 * @author: Steve Scott
 *
 * $Id: monitorPointSpecializations.cc,v 1.108 2012/10/29 21:35:13 iws Exp $
 * $CarmaCopyright$
 *
 */

#include <carma/util/checking.h>
#include <carma/util/complexManip.h>
#include <carma/util/Time.h>
#include <carma/monitor/MonitorPoint.h>
#include <carma/monitor/MonitorPointThreshold.h>
#include <carma/monitor/monitorPointSpecializations.h>
#include <carma/monitor/sanityChecks.h>
#include <carma/monitor/ScratchAverages.h>
#include <carma/util/programLogging.h>

#include <boost/algorithm/string/join.hpp>

#include <math.h>
#include <limits.h>
#include <values.h>

#include <iosfwd>
#include <iomanip>

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;

//---------------------- MonitorPointChar------------------------

MonitorPointChar::MonitorPointChar(const string& s,
        MONITOR_POINT_TYPE monitorPointType):
        MonitorPointNumeric(s, MONITOR_VALUE_TYPE_BYTE, monitorPointType)
{
}

char MonitorPointChar::getValue(int sampleIndex) const
{
    return getValueChar(sampleIndex);
}

void MonitorPointChar::setValue(char c, int sampleIndex) const
{
    MonitorPoint::setValue(c, sampleIndex);
}

string MonitorPointChar::getValueToString(char c) const
{
    ostringstream s;
    if ( (c < 32) || (c > 126)) {
        // Not printable
        return "?";
    }
    else {
        // Printable
        string s;
        s += c;
        return s;
    }
    // No longer used, but might be handy for debugging.
    s << "'" << c << "'" << "X" << hex << setw(2) << static_cast< unsigned short >(getValue());
    return s.str();
}

string MonitorPointChar::getValueToString(int sampleIndex) const
{
    return getValueToString(getValue(sampleIndex));
}

string MonitorPointChar::getAverageToString() const
{
    return getValueToString(getAve());
}

void MonitorPointChar::setWarnLowDefault (const char threshold)
{
    MonitorValue val;

    val.byte = threshold;
    MonitorPoint::setWarnLowDefault (val);
}

void MonitorPointChar::setWarnHighDefault (const char threshold)
{
    MonitorValue val;

    val.byte = threshold;
    MonitorPoint::setWarnHighDefault (val);
}

void MonitorPointChar::setErrorLowDefault (const char threshold)
{
    MonitorValue val;

    val.byte = threshold;
    MonitorPoint::setErrorLowDefault (val);
}

void MonitorPointChar::setErrorHighDefault (const char threshold)
{
    MonitorValue val;

    val.byte = threshold;
    MonitorPoint::setErrorHighDefault (val);
}

//---------------------- MonitorPointByte------------------------

MonitorPointByte::MonitorPointByte(const string& s,
        MONITOR_POINT_TYPE monitorPointType):
        MonitorPointNumeric(s, MONITOR_VALUE_TYPE_BYTE, monitorPointType)
{
}

unsigned char MonitorPointByte::getValue(int sampleIndex) const
{
    return static_cast<unsigned char>(getValueChar(sampleIndex));
}

void MonitorPointByte::setValue(unsigned char c, int sampleIndex) const
{
    MonitorPoint::setValue( static_cast<char>(c), sampleIndex);
}

string MonitorPointByte::getValueToString(int sampleIndex) const
{
    ostringstream s;
    s << static_cast<unsigned short>(getValue(sampleIndex));
    return s.str();
}

string MonitorPointByte::getAverageToString() const
{
    ostringstream s;
    s << static_cast<unsigned short>(getAve());
    return s.str();
}

void MonitorPointByte::setWarnLowDefault (const unsigned char threshold)
{
    MonitorValue val;

    val.byte = threshold;
    MonitorPoint::setWarnLowDefault (val);
}

void MonitorPointByte::setWarnHighDefault (const unsigned char threshold)
{
    MonitorValue val;

    val.byte = threshold;
    MonitorPoint::setWarnHighDefault (val);
}

void MonitorPointByte::setErrorLowDefault (const unsigned char threshold)
{
    MonitorValue val;

    val.byte = threshold;
    MonitorPoint::setErrorLowDefault (val);
}

void MonitorPointByte::setErrorHighDefault (const unsigned char threshold)
{
    MonitorValue val;

    val.byte = threshold;
    MonitorPoint::setErrorHighDefault (val);
}

//---------------------- MonitorPointShort ------------------------

MonitorPointShort::MonitorPointShort(const string& s,
        MONITOR_POINT_TYPE monitorPointType):
        MonitorPointNumeric(s, MONITOR_VALUE_TYPE_SHORT, monitorPointType)
{
}

short MonitorPointShort::getValue(int sampleIndex) const
{
    return getValueShort(sampleIndex);
}

void MonitorPointShort::setValue(short i, int sampleIndex) const
{
    MonitorPoint::setValue(i, sampleIndex);
}

string MonitorPointShort::getValueToString(int sampleIndex) const
{
    ostringstream s;
    s << getValue(sampleIndex);
    return s.str();
}

string MonitorPointShort::getAverageToString() const
{
    ostringstream s;
    s << getAve();
    return s.str();
}

void MonitorPointShort::setWarnLowDefault (const short threshold)
{
    MonitorValue val;

    val.sh = threshold;
    MonitorPoint::setWarnLowDefault (val);
}

void MonitorPointShort::setWarnHighDefault (const short threshold)
{
    MonitorValue val;

    val.sh = threshold;
    MonitorPoint::setWarnHighDefault (val);
}

void MonitorPointShort::setErrorLowDefault (const short threshold)
{
    MonitorValue val;

    val.sh = threshold;
    MonitorPoint::setErrorLowDefault (val);
}

void MonitorPointShort::setErrorHighDefault (const short threshold)
{
    MonitorValue val;

    val.sh = threshold;
    MonitorPoint::setErrorHighDefault (val);
}

//---------------------- MonitorPointInt ------------------------

MonitorPointInt::MonitorPointInt(const string& s,
        MONITOR_POINT_TYPE monitorPointType):
        MonitorPointNumeric(s, MONITOR_VALUE_TYPE_INTEGER, monitorPointType)
{
}

long MonitorPointInt::getValue(int sampleIndex) const
{
    return getValueLong(sampleIndex);
}

void MonitorPointInt::setValue(long i, int sampleIndex) const
{
    MonitorPoint::setValue(i, sampleIndex);
}

string MonitorPointInt::getValueToString(int sampleIndex) const
{
    ostringstream s;
    s << getValue(sampleIndex);
    return s.str();
}

string MonitorPointInt::getAverageToString() const
{
    ostringstream s;
    s << getAve();
    return s.str();
}

void MonitorPointInt::setWarnLowDefault (const long threshold)
{
    MonitorValue val;

    val.lo = threshold;
    MonitorPoint::setWarnLowDefault (val);
}

void MonitorPointInt::setWarnHighDefault (const long threshold)
{
    MonitorValue val;

    val.lo = threshold;
    MonitorPoint::setWarnHighDefault (val);
}

void MonitorPointInt::setErrorLowDefault (const long threshold)
{
    MonitorValue val;

    val.lo = threshold;
    MonitorPoint::setErrorLowDefault (val);
}

void MonitorPointInt::setErrorHighDefault (const long threshold)
{
    MonitorValue val;

    val.lo = threshold;
    MonitorPoint::setErrorHighDefault (val);
}

//---------------------- MonitorPointBool------------------------

MonitorPointBool::MonitorPointBool(const string& s,
        MONITOR_POINT_TYPE monitorPointType):
        MonitorPoint(s, MONITOR_VALUE_TYPE_BOOLEAN, monitorPointType)
{
}

bool MonitorPointBool::getValue(int sampleIndex) const
{
    return getValueBoolean(sampleIndex);
}

bool MonitorPointBool::getAve() const
{
    return getAveBoolean();
}

void MonitorPointBool::setValue(bool b, int sampleIndex) const
{
    MonitorPoint::setValue(b, sampleIndex);
}


string MonitorPointBool::getValueToString(int sampleIndex) const
{
    return (getValue(sampleIndex) ? "true" : "false");
}

string MonitorPointBool::getAverageToString() const
{
    return (getAve() ? "true" : "false");
}


void MonitorPointBool::setWarnLowDefault (bool boolVal)
{
    clearAllDefaults();

    MonitorValue value;
    value.bo = boolVal;
    MonitorPoint::setWarnLowDefault (value);
}


void MonitorPointBool::setWarnHighDefault (bool boolVal)
{
    clearAllDefaults();

    MonitorValue value;
    value.bo = boolVal;
    MonitorPoint::setWarnHighDefault (value);
}


void MonitorPointBool::setErrorLowDefault (bool boolVal)
{
    clearAllDefaults();

    MonitorValue value;
    value.bo = boolVal;
    MonitorPoint::setErrorLowDefault (value);
}


void MonitorPointBool::setErrorHighDefault (bool boolVal)
{
    clearAllDefaults();

    MonitorValue value;
    value.bo = boolVal;
    MonitorPoint::setErrorHighDefault (value);
}




bool MonitorPointBool::getWarnLow (const MonitorPointThreshold& threshold) const
{
    bool boolThreshold = 0;

    if (threshold.warnLowIsSet())  {
        boolThreshold = threshold.getBoolThresholdValue (THRESHOLD_LOW_WARN_VALUE);
    }  else if (warnLowDefaultIsSet())  {
        MonitorValue val = MonitorPoint::getWarnLowDefault ();
        boolThreshold = val.bo;
    }  else  {
        ostringstream os;
        os << "MonitorPointBool::getWarnLow - THRESHOLD_LOW_WARN_VALUE"
           << " not set (either threshold or default) for monitor point "
           << getCanonicalName();
        throw CARMA_ERROR (os.str());
    }

    return boolThreshold;
}


bool MonitorPointBool::getWarnHigh (const MonitorPointThreshold& threshold) const
{
    bool boolThreshold = 0;

    checkThreshold (threshold);

    if (threshold.warnHighIsSet())  {
        boolThreshold = threshold.getBoolThresholdValue (THRESHOLD_HIGH_WARN_VALUE);
    }  else if (warnHighDefaultIsSet())  {
        MonitorValue val = MonitorPoint::getWarnHighDefault ();
        boolThreshold = val.bo;
    }  else  {
        ostringstream os;
        os << "MonitorPointBool::getWarnHigh - THRESHOLD_HIGH_WARN_VALUE"
           << " not set (either threshold or default) for monitor point "
           << getCanonicalName();
        throw CARMA_ERROR (os.str());
    }

    return boolThreshold;
}


bool MonitorPointBool::getErrorLow (const MonitorPointThreshold& threshold) const
{
    bool boolThreshold = 0;

    checkThreshold (threshold);

    if (threshold.errorLowIsSet())  {
        boolThreshold = threshold.getBoolThresholdValue (THRESHOLD_LOW_ERROR_VALUE);
    }  else if (errorLowDefaultIsSet())  {
        MonitorValue val = MonitorPoint::getErrorLowDefault ();
        boolThreshold = val.bo;
    }  else  {
        ostringstream os;
        os << "MonitorPointBool::getErrorLow - THRESHOLD_LOW_ERROR_VALUE"
           << " not set (either threshold or default) for monitor point "
           << getCanonicalName();
        throw CARMA_ERROR (os.str());
    }

    return boolThreshold;
}


bool MonitorPointBool::getErrorHigh (const MonitorPointThreshold& threshold) const
{
    bool boolThreshold = 0;

    checkThreshold (threshold);

    if (threshold.errorHighIsSet())  {
        boolThreshold = threshold.getBoolThresholdValue (THRESHOLD_HIGH_ERROR_VALUE);
    }  else if (errorHighDefaultIsSet())  {
        MonitorValue val = MonitorPoint::getErrorHighDefault ();
        boolThreshold = val.bo;
    }  else  {
        ostringstream os;
        os << "MonitorPointBool::getErrorHigh - THRESHOLD_HIGH_ERROR_VALUE"
           << " not set (either threshold or default) for monitor point "
           << getCanonicalName();
        throw CARMA_ERROR (os.str());
    }

    return boolThreshold;
}


//---------------------- MonitorPointEnum ------------------------

MonitorPointEnum::MonitorPointEnum(
        const string& name,
        const bool bitmask,
        MONITOR_POINT_TYPE monitorPointType)
    : MonitorPoint(name, MONITOR_VALUE_TYPE_INTEGER, monitorPointType)
    , nEnumerations_(0)
    , bitmask_(bitmask)
{
    // This initialization to zero is used as a flag to getWidth
    width_ = 0;
}

short MonitorPointEnum::getWidth() const
{
    // width was set by the user
    if (width_ != 0)
        return width_;

    // bitmasks are displayed as either a single value or a 10-digit hex string
    if (bitmask_)
        width_ = 10;

    // find the longest single value
    for (int i=0; i < nEnumerations_; i++) {
        const int enumValue = bitmask_ ? (1 << i) : i;
        const size_t len = convertToString(enumValue).size();
        width_ = max(width_, static_cast<short>(len));
    }

    return width_;
}


string MonitorPointEnum::getRawStringForEnumValue( const int enumValue ) const
{
    if (bitmask_) {
        // check that no bits other than the valid bitmask values are set
        const uint32_t mask = (1 << nEnumerations_) - 1;
        if ((enumValue & ~mask) != 0) {
            std::ostringstream oss;
            oss << "Invalid enum bitmask value " << enumValue
                << " for " << getCanonicalName();
            throw CARMA_ERROR(oss.str());
        }

        return convertToString(enumValue);
    }

    if (enumValue < 0 || enumValue >= nEnumerations_) {
        std::ostringstream oss;
        oss << "Invalid enum value " << enumValue
            << " for " << getCanonicalName();
        throw CARMA_ERROR(oss.str());
    }

    return convertToString(enumValue);
}


void MonitorPointEnum::setNumEnumerations(int num)
{
    nEnumerations_ = num;
}

int MonitorPointEnum::getNumEnumerations() const
{
    return nEnumerations_;
}

int MonitorPointEnum::getValue(int sampleIndex) const
{
    return getValueLong(sampleIndex);
}

void MonitorPointEnum::setValue(long i, int sampleIndex) const
{
    if (bitmask_) {
        const uint32_t mask = (1 << nEnumerations_) - 1;
        if ((i & ~mask) != 0) {
            std::ostringstream oss;
            oss << "Invalid enum bitmask value " << i
                << " sampleIndex " << sampleIndex
                << " for " << getCanonicalName();
            throw CARMA_ERROR(oss.str());
        }
    }

    MonitorPoint::setValue(i, sampleIndex);
}

void MonitorPointEnum::addEnumWarnLowDefault (const long enumValue)
{
    const long enumValAsBits = bitmask_ ? enumValue : 1 << enumValue;
    MonitorValue value = getWarnLowDefault();
    value.lo |= enumValAsBits;
    MonitorPoint::setWarnLowDefault(value);
}


void MonitorPointEnum::addEnumWarnHighDefault (const long enumValue)
{
    const long enumValAsBits = bitmask_ ? enumValue : 1 << enumValue;
    MonitorValue value = getWarnHighDefault();
    value.lo |= enumValAsBits;
    MonitorPoint::setWarnHighDefault(value);
}


void MonitorPointEnum::addEnumErrorLowDefault (const long enumValue)
{
    const long enumValAsBits = bitmask_ ? enumValue : 1 << enumValue;
    MonitorValue value = getErrorLowDefault();
    value.lo |= enumValAsBits;
    MonitorPoint::setErrorLowDefault(value);
}


void MonitorPointEnum::addEnumErrorHighDefault (const long enumValue)
{
    const long enumValAsBits = bitmask_ ? enumValue : 1 << enumValue;
    MonitorValue value = getErrorHighDefault();
    value.lo |= enumValAsBits;
    MonitorPoint::setErrorHighDefault(value);
}


void MonitorPointEnum::removeEnumWarnLowDefault (const long enumValue)
{
    const long enumValAsBits = bitmask_ ? enumValue : 1 << enumValue;
    MonitorValue value = getWarnLowDefault();
    value.lo &= ~enumValAsBits;
    MonitorPoint::setWarnLowDefault(value);
}


void MonitorPointEnum::removeEnumWarnHighDefault (const long enumValue)
{
    const long enumValAsBits = bitmask_ ? enumValue : 1 << enumValue;
    MonitorValue value = getWarnHighDefault();
    value.lo &= ~enumValAsBits;
    MonitorPoint::setWarnHighDefault(value);
}


void MonitorPointEnum::removeEnumErrorLowDefault (const long enumValue)
{
    const long enumValAsBits = bitmask_ ? enumValue : 1 << enumValue;
    MonitorValue value = getErrorLowDefault();
    value.lo &= ~enumValAsBits;
    MonitorPoint::setErrorLowDefault(value);
}


void MonitorPointEnum::removeEnumErrorHighDefault (const long enumValue)
{
    const long enumValAsBits = bitmask_ ? enumValue : 1 << enumValue;
    MonitorValue value = getErrorHighDefault();
    value.lo &= ~enumValAsBits;
    MonitorPoint::setErrorHighDefault (value);
}


//---------------------- MonitorPointReal ------------------------

MonitorPointReal::MonitorPointReal(const string& s,
        MonitorValueType monitorValueType,
        MONITOR_POINT_TYPE monitorPointType):
        MonitorPointNumeric(s, monitorValueType, monitorPointType)
{
    // Set default precision
    setPrecision(3);
}

//---------------------- MonitorPointFloat ------------------------

MonitorPointFloat::MonitorPointFloat(const string& s,
        MONITOR_POINT_TYPE monitorPointType):
        MonitorPointReal(s, MONITOR_VALUE_TYPE_FLOAT, monitorPointType)
{
}

float MonitorPointFloat::getValue(int sampleIndex) const
{
    return  getValueFloat(sampleIndex);
}

void MonitorPointFloat::setValue(float f, int sampleIndex) const
{
    MonitorPoint::setValue(f, sampleIndex);
}


string MonitorPointFloat::getValueToString(int sampleIndex) const
{
    ostringstream s;
    s.setf(ios::fixed);
    s << setprecision(getPrecision()) << getValue(sampleIndex);
    return s.str();
}

string MonitorPointFloat::getAverageToString() const
{
    ostringstream s;
    s.setf(ios::fixed);
    s << setprecision(getPrecision()) << getAve();
    return s.str();
}

void MonitorPointFloat::setWarnLowDefault (const float threshold)
{
    MonitorValue val;

    val.fl = threshold;
    MonitorPoint::setWarnLowDefault (val);
}

void MonitorPointFloat::setWarnHighDefault (const float threshold)
{
    MonitorValue val;

    val.fl = threshold;
    MonitorPoint::setWarnHighDefault (val);
}

void MonitorPointFloat::setErrorLowDefault (const float threshold)
{
    MonitorValue val;

    val.fl = threshold;
    MonitorPoint::setErrorLowDefault (val);
}

void MonitorPointFloat::setErrorHighDefault (const float threshold)
{
    MonitorValue val;

    val.fl = threshold;
    MonitorPoint::setErrorHighDefault (val);
}

//---------------------- MonitorPointDouble ------------------------

MonitorPointDouble::MonitorPointDouble(const string& s,
        MONITOR_POINT_TYPE monitorPointType):
        MonitorPointReal(s, MONITOR_VALUE_TYPE_DOUBLE, monitorPointType)
{
}

double MonitorPointDouble::getValue(int sampleIndex) const
{
    return  getValueDouble(sampleIndex);
}

void MonitorPointDouble::setValue(double f, int sampleIndex) const
{
    MonitorPoint::setValue(f, sampleIndex);
}

string MonitorPointDouble::getValueToString(int sampleIndex) const
{
    ostringstream s;
    s.setf(ios::fixed);
    s << setprecision(getPrecision()) << getValue(sampleIndex);
    return s.str();
}

string MonitorPointDouble::getAverageToString() const
{
    ostringstream s;
    s.setf(ios::fixed);
    s << setprecision(getPrecision()) << getAve();
    return s.str();
}

void MonitorPointDouble::setWarnLowDefault (const double threshold)
{
    MonitorValue val;

    val.db = threshold;
    MonitorPoint::setWarnLowDefault (val);
}

void MonitorPointDouble::setWarnHighDefault (const double threshold)
{
    MonitorValue val;

    val.db = threshold;
    MonitorPoint::setWarnHighDefault (val);
}

void MonitorPointDouble::setErrorLowDefault (const double threshold)
{
    MonitorValue val;

    val.db = threshold;
    MonitorPoint::setErrorLowDefault (val);
}

void MonitorPointDouble::setErrorHighDefault (const double threshold)
{
    MonitorValue val;

    val.db = threshold;
    MonitorPoint::setErrorHighDefault (val);
}

//---------------------- MonitorPointComplex ------------------------


MonitorPointComplex::MonitorPointComplex(const string& s,
        MONITOR_POINT_TYPE monitorPointType):
        MonitorPoint(s, MONITOR_VALUE_TYPE_COMPLEX, monitorPointType),
        stringReturnType_(AMP)
{
    setPrecision(3);
    setWidth(15);
}

complex< float >
MonitorPointComplex::getValue( const int sampleIndex ) const
{
    return  getValueComplex(sampleIndex);
}

void
MonitorPointComplex::setValue(const complex< float > & f, int sampleIndex) const
{
    MonitorPoint::setValue(f, sampleIndex);
}

void MonitorPointComplex::setPrecision(short int precision)
{
    precision_ = precision;
}

short int MonitorPointComplex::getPrecision() const
{
    return precision_;
}

void
MonitorPointComplex::setStringRepresentation(
        const MonitorPointComplex::stringValueReturnType& type
        )
{
        stringReturnType_ = type;
}

MonitorPointComplex::stringValueReturnType
MonitorPointComplex::getStringRepresentation(void) const
{
        return stringReturnType_;
}



string MonitorPointComplex::getValueToString(complex< float > f) const
{
    ostringstream s;
    s.setf(ios::fixed);
    switch ( MonitorPointComplex::stringReturnType_ ) {
        default:
        case AMP:
            // uses carma/util/complexManip.h
            s << setprecision(getPrecision()) << amp(f);
            break;
        case PHASE:
            // uses carma/util/complexManip.h
            s << setprecision(getPrecision()) << phase(f);
            break;
        case REAL:
            s << setprecision(getPrecision()) <<f.real() ;
            break;
        case IMAG:
            s << setprecision(getPrecision()) <<f.imag() ;
            break;
        case COMPLEX:
            s << "("
              << setprecision(getPrecision()) << f.real()
              << ","
              << setprecision(getPrecision()) <<f.imag()
              <<")";
            // uses carma/util/complexManip.h
            break;
    }

    return s.str();
}

string MonitorPointComplex::getValueToString(int sampleIndex) const
{
    return getValueToString(getValue(sampleIndex));
}

string MonitorPointComplex::getAverageToString() const
{
    return getValueToString(getAve());
}

bool MonitorPointComplex::operator==(const MonitorComponent& component) const
{
    // Make sure its a monitor point and not a monitor container
    const MonitorPointComplex* mp =
            dynamic_cast<const MonitorPointComplex*>(&component);
    if ( mp == 0 ) return false;

    const MonitorPointComplex& m = *mp;

    // Check base type
    if (! this->MonitorPoint::isEqualTo (m)) return false;

    // Check values
    // Values are checked independent of status flags -- is this
    // correct ? - Amar
    for (int i=0; i<getNumSamples(); i++) {
        if (m.getValue(i) != getValue(i))return false;
    }

    return true;
}


void MonitorPointComplex::setWarnLowDefault (const float threshold)
{
    MonitorValue val;

    val.complex[0] = threshold;
    val.complex[1] = 0.0f;
    MonitorPoint::setWarnLowDefault (val);
}

void MonitorPointComplex::setWarnHighDefault (const float threshold)
{
    MonitorValue val;

    val.complex[0] = threshold;
    val.complex[1] = 0.0f;
    MonitorPoint::setWarnHighDefault (val);
}

void MonitorPointComplex::setErrorLowDefault (const float threshold)
{
    MonitorValue val;

    val.complex[0] = threshold;
    val.complex[1] = 0.0f;
    MonitorPoint::setErrorLowDefault (val);
}

void MonitorPointComplex::setErrorHighDefault (const float threshold)
{
    MonitorValue val;

    val.complex[0] = threshold;
    val.complex[1] = 0.0f;
    MonitorPoint::setErrorHighDefault (val);
}


//---------------------- MonitorPointAbstime ------------------------

MonitorPointAbstime::MonitorPointAbstime(const string& s,
        MONITOR_POINT_TYPE monitorPointType):
        MonitorPointReal(s, MONITOR_VALUE_TYPE_DOUBLE, monitorPointType)
{
    // Set the monitor point string width to be able to handle date/time
    setWidth(16);
    // And no fractions of a second
    setPrecision(0);
    // And to output date and time
    format_ = DATE_AND_TIME;
}

void MonitorPointAbstime::setFormat(MonitorPointAbstime::ABSTIME_FORMAT f)
{
    format_ = f;
}

MonitorPointAbstime::ABSTIME_FORMAT MonitorPointAbstime::getFormat() const
{
    return format_;
}

double MonitorPointAbstime::getValue(int sampleIndex) const
{
    return  getValueDouble(sampleIndex);
}

void MonitorPointAbstime::setValue(double f, int sampleIndex) const
{
    MonitorPoint::setValue(f, sampleIndex);
}

string MonitorPointAbstime::getFormattedAbstime(double mjd) const
{
    switch (format_) {
        case DATE: return Time::getDateString(mjd);
            break;
        case TIME: return Time::getTimeString(mjd, getPrecision());
            break;
        case DATE_AND_TIME: return Time::getDateTimeString(mjd, getPrecision());
            break;
    }
    return "bad format code";
}

string MonitorPointAbstime::getValueToString(int sampleIndex) const
{
    return getFormattedAbstime(getValue(sampleIndex));
}

string MonitorPointAbstime::getAverageToString() const
{
    return getFormattedAbstime(getAve());
}

void MonitorPointAbstime::setWarnLowDefault (const double threshold)
{
    MonitorValue val;

    val.db = threshold;
    MonitorPoint::setWarnLowDefault (val);
}

void MonitorPointAbstime::setWarnHighDefault (const double threshold)
{
    MonitorValue val;

    val.db = threshold;
    MonitorPoint::setWarnHighDefault (val);
}

void MonitorPointAbstime::setErrorLowDefault (const double threshold)
{
    MonitorValue val;

    val.db = threshold;
    MonitorPoint::setErrorLowDefault (val);
}

void MonitorPointAbstime::setErrorHighDefault (const double threshold)
{
    MonitorValue val;

    val.db = threshold;
    MonitorPoint::setErrorHighDefault (val);
}

//---------------------- MonitorPointRA ------------------------
MonitorPointRA::MonitorPointRA(const string& s,
        MONITOR_POINT_TYPE monitorPointType):
        MonitorPointDouble(s, monitorPointType)
{
    // Set the monitor point string width to be able to handle RA
    setWidth(11);
    // Hundredths of a second
    setPrecision(2);
}
// Implementation incomplete
std::string MonitorPointRA::getAverageToString() const {
    return "RA";
}
//---------------------- MonitorPointDec ------------------------
MonitorPointDec::MonitorPointDec(const string& s,
        MONITOR_POINT_TYPE monitorPointType):
        MonitorPointDouble(s, monitorPointType)
{
    // Set the monitor point string width to be able to handle Dec
    setWidth(11);
    // Tenths of an arcsecond
    setPrecision(1);
}
// Implementation incomplete
std::string MonitorPointDec::getAverageToString() const {
    return "DEC";
}

//---------------------- MonitorPointString ------------------------

MonitorPointString::MonitorPointString(const string& s,
        MONITOR_POINT_TYPE monitorPointType):
        MonitorPoint(s, MONITOR_VALUE_TYPE_STRING, monitorPointType)
{
    // Default width
    setWidth(7);
}

namespace {

bool
isBadStringValueChar( const char c )
{
    return ((c == '\0') || (c == '\n') || (c == '\t'));
}


string
myEscapeString( const string & s )
{
    string result;

    string::const_iterator i = s.begin();
    const string::const_iterator iEnd = s.end();

    for ( ; i != iEnd; ++i ) {
        if ( *i == '\0' )
            result += "\\0";
        else if ( *i == '\n' )
            result += "\\n";
        else if ( *i == '\t' )
            result += "\\t";
        else if ( *i == '\r' )
            result += "\\r";
        else if ( *i == '"' )
            result += "\\\"";
        else if ( *i == '\\' )
            result += "\\\\";
        else
            result += *i;
    }

    return result;
}


} // namespace < anonymous >


// This should work even for a null string (numSamps=0)
string
MonitorPointString::getValue( ) const
{
    const int origNumChunks = getNumSamples();
    const int numChunks = ::std::min( 10, origNumChunks );

    if ( numChunks < 0 ) {
        ostringstream oss;

        oss << "MonitorPointString::getValue: "
            << getCanonicalName() + " had too few chunks ("
            << numChunks << ")";

        programLogErrorIfPossible( oss.str() );

        return string();
    }

    if ( numChunks < 1 )
        return string();

    string result;
    result.reserve( 8 * numChunks );

    bool badEnd = false;
    char endChar = '\0';
    size_t lastChunkCount = 0;

    for ( int i = 0; i < numChunks; ++i ) {
        const MonitorValueStringChunk chunk = getValueStringChunk( i );

        // Scan for bad chars
        size_t j = 0;
        for ( ; j < 8; ++j ) {
            if ( isBadStringValueChar( chunk.chunkChars[ j ] ) )
                break;
        }

        result.append( chunk.chunkChars, (chunk.chunkChars + j) );

        lastChunkCount = j;

        if ( lastChunkCount < 8 ) {
            // Ended before end of chunk
            endChar = chunk.chunkChars[ lastChunkCount ];

            if ( i < (numChunks - 1) ) {
                badEnd = true;  // Bad char before last chunk
            } else if ( endChar != '\0' ) {
                badEnd = true;  // Non-null bad char in last chunk
            } else {
                // Null char in last chunk (this is okay if the rest are null)

                // Scan the rest for non-nulls chars
                size_t k = lastChunkCount + 1;
                for ( ; k < 8; ++k ) {
                    if ( chunk.chunkChars[ k ] != '\0' )
                        break;
                }

                if ( k < 8 )
                    badEnd = true;  // Non-null after null char in last chunk
            }

            break;  // We always stop appending at the first short chunk
        }
    }

    if ( badEnd ) {
        ostringstream oss;

        oss << "MonitorPointString::getValue: " << getCanonicalName() << " ";

        if ( origNumChunks > 10 )
            oss << "had too many chunks (" << origNumChunks << ") and";
        else
            oss << numChunks << " chunk";

        oss << " value \"" << myEscapeString( result )
            << "\" ended due to bad character ("
            << static_cast< int >( endChar ) << ") in the data";

        programLogErrorIfPossible( oss.str() );
    } else if ( (lastChunkCount == 0) && (numChunks != 1) ) {
        ostringstream oss;

        oss << "MonitorPointString::getValue: " << getCanonicalName() << " ";

        if ( origNumChunks > 10 )
            oss << "had too many chunks (" << origNumChunks << ") and";
        else
            oss << numChunks << " chunk";

        oss << " value \"" << myEscapeString( result )
            << "\" had empty last chunk";

        programLogErrorIfPossible( oss.str() );
    } else if ( origNumChunks > 10 ) {
        ostringstream oss;

        oss << "MonitorPointString::getValue: "
            << getCanonicalName() << " had too many chunks ("
            << origNumChunks << ")";

        programLogErrorIfPossible( oss.str() );
    }

    return result;
}


void
MonitorPointString::setValue( const string & str ) const
{
    MonitorValueStringChunk chunks[ 10 ];
    int numChunks = 1;
    size_t chunkChars = 0;

    string::const_iterator i = str.begin();
    const string::const_iterator iEnd = str.end();

    for ( ; i != iEnd; ++i ) {
        if ( isBadStringValueChar( *i ) )
            continue;

        if ( chunkChars < 8 )  // 8 chars per chunk
            ++chunkChars;
        else if ( numChunks < 10 ) {  // at most 10 chunks
            ++numChunks;

            chunkChars = 1;
        } else
            break;  // We hit the max number of chunks

        chunks[ numChunks - 1 ].chunkChars[ chunkChars - 1 ] = *i;
    }

    // fill out the last chunk with null characters
    for ( size_t j = chunkChars; j < 8; ++j )
        chunks[ numChunks - 1 ].chunkChars[ j ] = '\0';

    setNumSamples( numChunks );

    MonitorPoint::setValuesStringChunksAndValidities( chunks,
                                                      numChunks,
                                                      MonitorPoint::VALID );
}


// No longer enclose in quotes
string MonitorPointString::getValueToString() const
{
    return getValue();
}

// Ignore the index...
string MonitorPointString::getValueToString(int i) const
{
    return getValueToString();
}

// No longer enclose in quotes
string MonitorPointString::getAverageToString() const
{
    return getAve();
}


bool MonitorPointString::operator==(const MonitorComponent& component) const
{
    // Make sure its a monitor point and not a monitor container
    const MonitorPointString* mp =
            dynamic_cast<const MonitorPointString*>(&component);
    if ( mp == 0 ) return false;

    const MonitorPointString& m = *mp;

    // Check base type
    if (! this->MonitorPoint::isEqualTo (m))  return false;

    // Check values
    // Values are checked independent of status flags -- is this
    // correct ? - Amar
    if (m.getValue() != getValue())  return false;

    return true;
}


void MonitorPointString::setWarnLowDefault (const string strValue)
{
    MonitorValue value;

    int strLength = sizeof(value) - 1;
    string str (strValue.substr (0,strLength));
    memcpy (value.str, str.c_str(), strLength);
    MonitorPoint::setWarnLowDefault (value);
}


void MonitorPointString::setWarnHighDefault (const string strValue)
{
    MonitorValue value;

    int strLength = sizeof(value) - 1;
    string str (strValue.substr (0,strLength));
    memcpy (value.str, str.c_str(), strLength);
    MonitorPoint::setWarnHighDefault (value);
}


void MonitorPointString::setErrorLowDefault (const string strValue)
{
    MonitorValue value;

    int strLength = sizeof(value) - 1;
    string str (strValue.substr (0,strLength));
    memcpy (value.str, str.c_str(), strLength);
    MonitorPoint::setErrorLowDefault (value);
}


void MonitorPointString::setErrorHighDefault (const string strValue)
{
    MonitorValue value;

    int strLength = sizeof(value) - 1;
    string str (strValue.substr (0,strLength));
    memcpy (value.str, str.c_str(), strLength);
    MonitorPoint::setErrorHighDefault (value);
}


string MonitorPointString::getWarnLow (const MonitorPointThreshold& threshold) const
{
    string stringThreshold;

    if (threshold.warnLowIsSet())  {
        stringThreshold = threshold.getStringThresholdValue (THRESHOLD_LOW_WARN_VALUE);
    }  else if (warnLowDefaultIsSet())  {
        MonitorValue value;

        value = MonitorPoint::getWarnLowDefault();
        string str (value.str);
        stringThreshold = str;
    }  else  {
        ostringstream os;
        os << "MonitorPointString::getWarnLow - THRESHOLD_LOW_WARN_VALUE"
           << " not set (either threshold or default) for monitor point "
           << getCanonicalName();
        throw CARMA_ERROR (os.str());
    }

    return stringThreshold;
}


string MonitorPointString::getWarnHigh (const MonitorPointThreshold& threshold) const
{
    string stringThreshold;

    checkThreshold (threshold);

    if (threshold.warnHighIsSet())  {
        stringThreshold = threshold.getStringThresholdValue (THRESHOLD_HIGH_WARN_VALUE);
    }  else if (warnHighDefaultIsSet())  {
        MonitorValue value;

        value = MonitorPoint::getWarnHighDefault();
        string str (value.str);
        stringThreshold = str;
    }  else  {
        ostringstream os;
        os << "MonitorPointString::getWarnHigh - THRESHOLD_HIGH_WARN_VALUE"
           << " not set (either threshold or default) for monitor point "
           << getCanonicalName();
        throw CARMA_ERROR (os.str());
    }

    return stringThreshold;
}


string MonitorPointString::getErrorLow (const MonitorPointThreshold& threshold) const
{
    string stringThreshold;

    checkThreshold (threshold);

    if (threshold.errorLowIsSet())  {
        stringThreshold = threshold.getStringThresholdValue (THRESHOLD_LOW_ERROR_VALUE);
    }  else if (errorLowDefaultIsSet())  {
        MonitorValue value;

        value = MonitorPoint::getErrorLowDefault();
        string str (value.str);
        stringThreshold = str;
    }  else  {
        ostringstream os;
        os << "MonitorPointString::getErrorLow - THRESHOLD_LOW_ERROR_VALUE"
           << " not set (either threshold or default) for monitor point "
           << getCanonicalName();
        throw CARMA_ERROR (os.str());
    }

    return stringThreshold;
}


string MonitorPointString::getErrorHigh (const MonitorPointThreshold& threshold) const
{
    checkThreshold (threshold);

    string stringThreshold;

    if (threshold.errorHighIsSet())  {
        stringThreshold = threshold.getStringThresholdValue (THRESHOLD_HIGH_ERROR_VALUE);
    }  else if (errorHighDefaultIsSet())  {
        MonitorValue value;

        value = MonitorPoint::getErrorHighDefault();
        string str (value.str);
        stringThreshold = str;
    }  else  {
        ostringstream os;
        os << "MonitorPointString::getErrorHigh - THRESHOLD_HIGH_ERROR_VALUE"
           << " not set (either threshold or default) for monitor point "
           << getCanonicalName();
        throw CARMA_ERROR (os.str());
    }

    return stringThreshold;
}



//---------------------- MonitorPointSerialNo ------------------------

MonitorPointSerialNo::MonitorPointSerialNo(const string& s,
        MONITOR_POINT_TYPE monitorPointType):
        MonitorPoint(s, MONITOR_VALUE_TYPE_SERIAL_NUMBER, monitorPointType)
{
}

// Ignore the sample number
int MonitorPointSerialNo::getValue(int sampleIndex) const
{
    return getValueSerialNo();
}

// Ignore the sample number
void MonitorPointSerialNo::setValue(long i, int sampleIndex) const
{
    MonitorPoint::setValueSerialNo(i);
}

// Always one sample
void MonitorPointSerialNo::setNumSamples(int numSamples) const
{
    MonitorPoint::setNumSamples(1);
}

// Ignore the sample number
string MonitorPointSerialNo::getValueToString(int sampleIndex) const
{
    ostringstream s;
    s << getValue();
    return s.str();
}

string MonitorPointSerialNo::getAverageToString() const
{
    ostringstream s;
    s << getAve();
    return s.str();
}


//=================================================================
//=================================================================
//----------------------Frame averages for all types----------------

double
MonitorPointChar::getValueNumeric( const int sampleIndex ) const
{
    return static_cast< double >( getValue( sampleIndex ) );
}


double
MonitorPointChar::getAveNumeric( ) const
{
    return static_cast< double >( getAve() );
}


char MonitorPointChar::getAccumulatedAverage (const MonitorPointAverageNumeric& accumulator) const
{
    return static_cast<char>(round(getAccumulatedAverageDouble (accumulator)));
}


char MonitorPointChar::getMaxValue (const MonitorPointAverageNumeric& accumulator) const
{
    return static_cast<char>(round(getMaxValueDouble (accumulator)));
}


char MonitorPointChar::getMinValue (const MonitorPointAverageNumeric& accumulator) const
{
    return static_cast<char>(round(getMinValueDouble (accumulator)));
}


void
MonitorPointChar::updateFrameAverage( ScratchAverages & scratchAvgs )
{
    if (getNumSamples() <= 1) return;

    setAve(static_cast<char>(round(computeFrameAverage( scratchAvgs.numericAccum ))));

    const VALIDITY sanitizedValidity =
        sanityCheckValidity(
            getAveValidityNumeric(),
            this,
            true,
            "MonitorPointChar::updateFrameAverage" );

    setAveValidity( sanitizedValidity );
    setBlankingFlagging (getBlankingFlaggingNumeric());
}

string MonitorPointChar::getAccumulatedAverageAsString (const MonitorPointAverageNumeric& accumulator) const
{
    ostringstream os;
    os << getAccumulatedAverage (accumulator);
    return os.str();
}


enum MonitorPoint::VALIDITY
MonitorPointChar::evaluateSampleTolerance(
    const MonitorPointThreshold & t,
    const char                    val ) const
{
    enum MonitorPoint::VALIDITY v = VALID_GOOD;
    if (! t.isSet())
        return v;

    unsigned char errHi  = t.getByteThresholdValue(THRESHOLD_HIGH_ERROR_VALUE);
    unsigned char errLo  = t.getByteThresholdValue(THRESHOLD_LOW_ERROR_VALUE);
    unsigned char warnHi = t.getByteThresholdValue(THRESHOLD_HIGH_WARN_VALUE);
    unsigned char warnLo = t.getByteThresholdValue(THRESHOLD_LOW_WARN_VALUE);

         if (t.errorHighIsSet()  &&  (val >= errHi))  v = VALID_ERROR_HIGH;
    else if (t.errorLowIsSet()   &&  (val <= errLo))  v = VALID_ERROR_LOW;
    else if (t.warnHighIsSet()   &&  (val >= warnHi)) v = VALID_WARNING_HIGH;
    else if (t.warnLowIsSet()    &&  (val <= warnLo)) v = VALID_WARNING_LOW;
    else v = VALID_GOOD;

    return v;
}


void
MonitorPointChar::evaluateTolerance( const MonitorPointThreshold & t )
{
    const int numSamps = getNumSamples();

    for ( int i = 0; i < numSamps; ++i ) {
        const enum MonitorPoint::VALIDITY v = getValidity(i);
        if ( isValid(v) ) {
            const char charVal = getValue(i);
            setValidity( evaluateSampleTolerance( t, charVal ), i );
        }
    }

    if ( numSamps > 1 ) {
        const enum MonitorPoint::VALIDITY v = getAveValidity();
        if ( isValid(v) ) {
            const char charVal = getAve();
            setAveValidity( evaluateSampleTolerance( t, charVal ) );
        }
    }
}


double
MonitorPointByte::getValueNumeric( const int sampleIndex ) const
{
    return static_cast< double >( getValue( sampleIndex ) );
}


double
MonitorPointByte::getAveNumeric( ) const
{
    return static_cast< double >( getAve() );
}


unsigned char MonitorPointByte::getAccumulatedAverage (const MonitorPointAverageNumeric& accumulator) const
{
    return static_cast<unsigned char>(round(getAccumulatedAverageDouble (accumulator)));
}


unsigned char MonitorPointByte::getMaxValue (const MonitorPointAverageNumeric& accumulator) const
{
    return static_cast<unsigned char>(round(getMaxValueDouble (accumulator)));
}


unsigned char MonitorPointByte::getMinValue (const MonitorPointAverageNumeric& accumulator) const
{
    return static_cast<unsigned char>(round(getMinValueDouble (accumulator)));
}


void
MonitorPointByte::updateFrameAverage( ScratchAverages & scratchAvgs )
{
    if (getNumSamples() <= 1) return;

    setAve(static_cast<char>(round(computeFrameAverage( scratchAvgs.numericAccum ))));

    const VALIDITY sanitizedValidity =
        sanityCheckValidity(
            getAveValidityNumeric(),
            this,
            true,
            "MonitorPointByte::updateFrameAverage" );

    setAveValidity( sanitizedValidity );
    setBlankingFlagging (getBlankingFlaggingNumeric());
}

string MonitorPointByte::getAccumulatedAverageAsString (const MonitorPointAverageNumeric& accumulator) const
{
    ostringstream os;
    os << getAccumulatedAverage (accumulator);
    return os.str();
}


enum MonitorPoint::VALIDITY
MonitorPointByte::evaluateSampleTolerance(
    const MonitorPointThreshold & t,
    const unsigned char           val ) const
{
    enum MonitorPoint::VALIDITY v = VALID_GOOD;
    if (! t.isSet())
        return v;

    unsigned char errHi  = t.getByteThresholdValue(THRESHOLD_HIGH_ERROR_VALUE);
    unsigned char errLo  = t.getByteThresholdValue(THRESHOLD_LOW_ERROR_VALUE);
    unsigned char warnHi = t.getByteThresholdValue(THRESHOLD_HIGH_WARN_VALUE);
    unsigned char warnLo = t.getByteThresholdValue(THRESHOLD_LOW_WARN_VALUE);

         if (t.errorHighIsSet()  &&  (val >= errHi))  v = VALID_ERROR_HIGH;
    else if (t.errorLowIsSet()   &&  (val <= errLo))  v = VALID_ERROR_LOW;
    else if (t.warnHighIsSet()   &&  (val >= warnHi)) v = VALID_WARNING_HIGH;
    else if (t.warnLowIsSet()    &&  (val <= warnLo)) v = VALID_WARNING_LOW;
    else v = VALID_GOOD;

    return v;
}


void
MonitorPointByte::evaluateTolerance( const MonitorPointThreshold & t )
{
    const int numSamps = getNumSamples();

    for ( int i = 0; i < numSamps; ++i ) {
        const enum MonitorPoint::VALIDITY v = getValidity(i);
        if ( isValid(v) ) {
            const unsigned char byteVal = getValue(i);
            setValidity( evaluateSampleTolerance( t, byteVal ), i );
        }
    }

    if ( numSamps > 1 ) {
        const enum MonitorPoint::VALIDITY v = getAveValidity();
        if ( isValid(v) ) {
            const unsigned char byteVal = getAve();
            setAveValidity( evaluateSampleTolerance( t, byteVal ) );
        }
    }
}


double
MonitorPointShort::getValueNumeric( const int sampleIndex ) const
{
    return static_cast< double >( getValue( sampleIndex ) );
}


double
MonitorPointShort::getAveNumeric( ) const
{
    return static_cast< double >( getAve() );
}


short MonitorPointShort::getAccumulatedAverage (const MonitorPointAverageNumeric& accumulator) const
{
    return static_cast<short>(round(getAccumulatedAverageDouble (accumulator)));
}


short MonitorPointShort::getMaxValue (const MonitorPointAverageNumeric& accumulator) const
{
    return static_cast<short>(round(getMaxValueDouble (accumulator)));
}


short MonitorPointShort::getMinValue (const MonitorPointAverageNumeric& accumulator) const
{
    return static_cast<short>(round(getMinValueDouble (accumulator)));
}



void
MonitorPointShort::updateFrameAverage( ScratchAverages & scratchAvgs )
{
    if (getNumSamples() <= 1) return;

    setAve(static_cast<short>(round(computeFrameAverage( scratchAvgs.numericAccum ))));

    const VALIDITY sanitizedValidity =
        sanityCheckValidity(
            getAveValidityNumeric(),
            this,
            true,
            "MonitorPointShort::updateFrameAverage" );

    setAveValidity( sanitizedValidity );
    setBlankingFlagging (getBlankingFlaggingNumeric());
}

string MonitorPointShort::getAccumulatedAverageAsString (const MonitorPointAverageNumeric& accumulator) const
{
    ostringstream os;
    os << getAccumulatedAverage (accumulator);
    return os.str();
}


enum MonitorPoint::VALIDITY
MonitorPointShort::evaluateSampleTolerance(
    const MonitorPointThreshold & t,
    const short                   val ) const
{
    enum MonitorPoint::VALIDITY v = VALID_GOOD;
    if (! t.isSet())
        return v;

    short errHi  = t.getShortThresholdValue(THRESHOLD_HIGH_ERROR_VALUE);
    short errLo  = t.getShortThresholdValue(THRESHOLD_LOW_ERROR_VALUE);
    short warnHi = t.getShortThresholdValue(THRESHOLD_HIGH_WARN_VALUE);
    short warnLo = t.getShortThresholdValue(THRESHOLD_LOW_WARN_VALUE);

         if (t.errorHighIsSet()  &&  (val >= errHi))  v = VALID_ERROR_HIGH;
    else if (t.errorLowIsSet()   &&  (val <= errLo))  v = VALID_ERROR_LOW;
    else if (t.warnHighIsSet()   &&  (val >= warnHi)) v = VALID_WARNING_HIGH;
    else if (t.warnLowIsSet()    &&  (val <= warnLo)) v = VALID_WARNING_LOW;
    else v = VALID_GOOD;

    return v;
}



void
MonitorPointShort::evaluateTolerance( const MonitorPointThreshold & t )
{
    const int numSamps = getNumSamples();

    for ( int i = 0; i < numSamps; ++i ) {
        const enum MonitorPoint::VALIDITY v = getValidity(i);
        if ( isValid(v) ) {
            const short shortVal = getValue(i);
            setValidity( evaluateSampleTolerance( t, shortVal ), i );
        }
    }

    if ( numSamps > 1 ) {
        const enum MonitorPoint::VALIDITY v = getAveValidity();
        if ( isValid(v) ) {
            const short shortVal = getAve();
            setAveValidity( evaluateSampleTolerance( t, shortVal ) );
        }
    }
}


double
MonitorPointInt::getValueNumeric( const int sampleIndex ) const
{
    return static_cast< double >( getValue( sampleIndex ) );
}


double
MonitorPointInt::getAveNumeric( ) const
{
    return static_cast< double >( getAve() );
}


long MonitorPointInt::getAccumulatedAverage (const MonitorPointAverageNumeric& accumulator) const
{
    return static_cast<long>(round(getAccumulatedAverageDouble (accumulator)));
}


long MonitorPointInt::getMaxValue (const MonitorPointAverageNumeric& accumulator) const
{
    return static_cast<long>(round(getMaxValueDouble (accumulator)));
}


long MonitorPointInt::getMinValue (const MonitorPointAverageNumeric& accumulator) const
{
    return static_cast<long>(round(getMinValueDouble (accumulator)));
}



void
MonitorPointInt::updateFrameAverage( ScratchAverages & scratchAvgs )
{
    if (getNumSamples() <= 1) return;

    setAve(static_cast<long int>(round(computeFrameAverage( scratchAvgs.numericAccum ))));

    const VALIDITY sanitizedValidity =
        sanityCheckValidity(
            getAveValidityNumeric(),
            this,
            true,
            "MonitorPointInt::updateFrameAverage" );

    setAveValidity( sanitizedValidity );
    setBlankingFlagging (getBlankingFlaggingNumeric());
}

string MonitorPointInt::getAccumulatedAverageAsString (const MonitorPointAverageNumeric& accumulator) const
{
    ostringstream os;
    os << getAccumulatedAverage (accumulator);
    return os.str();
}


enum MonitorPoint::VALIDITY
MonitorPointInt::evaluateSampleTolerance(
    const MonitorPointThreshold & t,
    const long                    val ) const
{
    enum MonitorPoint::VALIDITY v = VALID_GOOD;
    if (! t.isSet())
        return v;

    long errHi  = t.getLongThresholdValue(THRESHOLD_HIGH_ERROR_VALUE);
    long errLo  = t.getLongThresholdValue(THRESHOLD_LOW_ERROR_VALUE);
    long warnHi = t.getLongThresholdValue(THRESHOLD_HIGH_WARN_VALUE);
    long warnLo = t.getLongThresholdValue(THRESHOLD_LOW_WARN_VALUE);

         if (t.errorHighIsSet()  &&  (val >= errHi))  v = VALID_ERROR_HIGH;
    else if (t.errorLowIsSet()   &&  (val <= errLo))  v = VALID_ERROR_LOW;
    else if (t.warnHighIsSet()   &&  (val >= warnHi)) v = VALID_WARNING_HIGH;
    else if (t.warnLowIsSet()    &&  (val <= warnLo)) v = VALID_WARNING_LOW;
    else v = VALID_GOOD;

    return v;
}



void
MonitorPointInt::evaluateTolerance( const MonitorPointThreshold & t )
{
    const int numSamps = getNumSamples();

    for ( int i = 0; i < numSamps; ++i ) {
        const enum MonitorPoint::VALIDITY v = getValidity(i);
        if ( isValid(v) ) {
            const long longVal = getValue(i);
            setValidity( evaluateSampleTolerance( t, longVal ), i );
        }
    }

    if ( numSamps > 1 ) {
        const enum MonitorPoint::VALIDITY v = getAveValidity();
        if ( isValid(v) ) {
            const long longVal = getAve();
            setAveValidity( evaluateSampleTolerance( t, longVal ) );
        }
    }
}


void
MonitorPointBool::resetAccumulator( MonitorPointAverageBool & accum ) const
{
    accum.setAccumulator(0);
    accum.setMaxValue(0);
    accum.setMinValue(1);
    accum.resetAveProperties();
}


enum MonitorPoint::VALIDITY
MonitorPointBool::accumulateSample( MonitorPointAverageBool & accum,
                                    const int                 sampIndex ) const
{
    const enum MonitorPoint::VALIDITY sampValidity = getValidity( sampIndex );
    if ( isValid( sampValidity ) ) {
        const bool sampValue = getValue( sampIndex );
        if ( sampValue ) {
            // Increment the accumulated valid "true" count
            accum.setAccumulator( accum.getAccumulator() + 1 );
            accum.setMaxValue(1);
        } else {
            accum.setMinValue(0);
        }
        accum.incrementNumValidSamples();
    }
    accum.incrementNumTotalSamples();

    return sampValidity;
}


void
MonitorPointBool::accumulate( MonitorPointAverageBool & accum ) const
{
    enum MonitorPoint::VALIDITY v = INVALID_NO_DATA;
    const int nSamples = getNumSamples();

    for (int i=0; i<nSamples; i++) {
        v = accumulateSample (accum, i);
    }

    enum MonitorPoint::VALIDITY aveValidity = accum.getValidity();
    if (v > aveValidity) aveValidity = v;

    if (accum.getNumValidSamples() == 0) {
        if (accum.getNumTotalSamples() == 0)  {
            aveValidity = INVALID_NO_DATA;
        }  else  {
            aveValidity = v;
        }
        CARMA_CHECK (accum.getAccumulator() == 0);
    }
    else {
        // Some are good
        aveValidity = VALID_NOT_CHECKED;
    }

    accum.setValidity(aveValidity);
}


void
MonitorPointBool::accumulateAverage( MonitorPointAverageBool & accum )
{
    const bool frameAvgValue = getAveBoolean();

    nValidSamples_ = getNumValidSamples();
    if ( frameAvgValue  == true )
        accum.setAccumulator( accum.getAccumulator() + 1 );
    accum.setValidity( getAveValidity() );
    accum.setBlanking( getBlankingFlagging() );
    accum.incrementNumTotalSamples( getNumSamples() );
    accum.incrementNumValidSamples( nValidSamples_ );
    if ( frameAvgValue == true )
        accum.setMaxValue( 1 );
    else
        accum.setMinValue( 0 );
}


bool
MonitorPointBool::getAccumulatedAverage(
    const MonitorPointAverageBool & accum ) const
{
    const long trueCount = accum.getAccumulator();
    const long validSampsCount = accum.getNumValidSamples();

    bool multiframeAvgValue = false;
    
    if ( trueCount > validSampsCount )
        multiframeAvgValue = true;
    else {
        const long notTrueCount = validSampsCount - trueCount;
    
        // if majority of valid samples are false, return false as average
        if ( notTrueCount > trueCount )
            multiframeAvgValue = false;
        else
            multiframeAvgValue = true;
    }

    return multiframeAvgValue;
}


bool
MonitorPointBool::getMaxValue( const MonitorPointAverageBool & accum ) const
{
    return static_cast<bool>(accum.getMaxValue());
}


bool
MonitorPointBool::getMinValue( const MonitorPointAverageBool & accum ) const
{
    return static_cast<bool>(accum.getMinValue());
}


// Does an AND of all legit values
void
MonitorPointBool::updateFrameAverage( ScratchAverages & scratchAvgs )
{
    if (getNumSamples() <= 1) return;

    resetAccumulator (scratchAvgs.boolAccum);
    accumulate (scratchAvgs.boolAccum);

    setAve (getAccumulatedAverage (scratchAvgs.boolAccum));

    const VALIDITY sanitizedValidity =
        sanityCheckValidity(
            scratchAvgs.boolAccum.getValidity(),
            this,
            true,
            "MonitorPointBool::updateFrameAverage" );

    setAveValidity( sanitizedValidity );
    setBlankingFlagging (scratchAvgs.boolAccum.getBlanking());
    nValidSamples_ = scratchAvgs.boolAccum.getNumValidSamples();
}


string
MonitorPointBool::getAccumulatedAverageAsString(
    const MonitorPointAverageBool & accum ) const
{
    ostringstream oss;

    oss << getAccumulatedAverage( accum );

    return oss.str();
}


enum MonitorPoint::VALIDITY
MonitorPointBool::evaluateSampleTolerance(
    const MonitorPointThreshold & t,
    const bool                    val ) const
{
    enum MonitorPoint::VALIDITY v = VALID_GOOD;
    if (! t.isSet())
        return v;

    // for bool values, we test for warnings or errors - only one is set in
    // the threshold object, and theres no low or high as there are only
    // two values in a bool range. The value in the threshold object is used
    // to set validity of the sample value to ERROR or WARNING based on the
    // specified threshold value.

    bool errHi  = t.getBoolThresholdValue(THRESHOLD_HIGH_ERROR_VALUE);
    bool errLo  = t.getBoolThresholdValue(THRESHOLD_LOW_ERROR_VALUE);
    bool warnHi = t.getBoolThresholdValue(THRESHOLD_HIGH_WARN_VALUE);
    bool warnLo = t.getBoolThresholdValue(THRESHOLD_LOW_WARN_VALUE);

         if (t.errorHighIsSet()  &&  (val == errHi))  v = VALID_ERROR;
    else if (t.errorLowIsSet()   &&  (val == errLo))  v = VALID_ERROR;
    else if (t.warnHighIsSet()   &&  (val == warnHi)) v = VALID_WARNING;
    else if (t.warnLowIsSet()    &&  (val == warnLo)) v = VALID_WARNING;
    else v = VALID_GOOD;

    return v;
}


void
MonitorPointBool::evaluateTolerance( const MonitorPointThreshold & t )
{
    const int numSamps = getNumSamples();

    for ( int i = 0; i < numSamps; ++i ) {
        const enum MonitorPoint::VALIDITY v = getValidity(i);
        if ( isValid(v) ) {
            const bool boolVal = getValue(i);
            setValidity( evaluateSampleTolerance( t, boolVal ), i );
        }
    }

    if ( numSamps > 1 ) {
        const enum MonitorPoint::VALIDITY v = getAveValidity();
        if ( isValid(v) ) {
            const bool boolVal = getAve();
            setAveValidity( evaluateSampleTolerance( t, boolVal ) );
        }
    }
}


void MonitorPointEnum::resetAccumulator
                                 (MonitorPointAverageEnum& accumulator) const
{
    accumulator.setAccumulator (0);
    if (bitmask_) {
        accumulator.setMaxValue(0);
        accumulator.setMinValue(0);
    } else {
        accumulator.setMaxValue (INT_MIN);
        accumulator.setMinValue (INT_MAX);
    }
    accumulator.resetAveProperties();
}


enum MonitorPoint::VALIDITY
MonitorPointEnum::accumulateSample( MonitorPointAverageEnum & accum,
                                    const int                 index ) const
{
    const enum MonitorPoint::VALIDITY v = getValidity( index );

    if ( isValid(v) ) {
        const int val = static_cast< int >( getValue( index ) );

        int newMaxVal = ::std::max( val, accum.getMaxValue() );
        int newMinVal = ::std::min( val, accum.getMinValue() );

        // bitmasks use a bitwise OR of all previous values
        if (bitmask_) {
            newMaxVal = val | accum.getMaxValue();
            newMinVal = val | accum.getMinValue();
        }

        // New way
        accum.setAccumulator( static_cast< long >( newMaxVal ) );

        // Old way we decided we didn't want - TWC 5 Feb 2007
        // accum.setAccumulator( static_cast< long >( val ) );

        accum.incrementNumValidSamples();

        accum.setMaxValue( newMaxVal );
        accum.setMinValue( newMinVal );
    }

    accum.incrementNumTotalSamples();

    return v;
}


void MonitorPointEnum::accumulate (MonitorPointAverageEnum& accumulator) const
{
    enum MonitorPoint::VALIDITY v = INVALID_NO_DATA;
    const int nSamples = getNumSamples();

    for (int i=0; i<nSamples; i++) {
        v = accumulateSample (accumulator, i);
    }

    enum MonitorPoint::VALIDITY aveValidity = accumulator.getValidity();
    if (v > aveValidity) aveValidity = v;

    if (accumulator.getNumValidSamples() == 0) {
        if (accumulator.getNumTotalSamples() == 0)  {
            aveValidity = INVALID_NO_DATA;
        }  else  {
            aveValidity = v;
        }
    }
    else {
        // Some are good
        aveValidity = VALID_NOT_CHECKED;
    }

    // set to last valid value
    accumulator.setValidity(aveValidity);
}


void
MonitorPointEnum::accumulateAverage( MonitorPointAverageEnum & accum )
{
    const int enumAve = getAveLong();

    int newMaxVal = ::std::max( enumAve, accum.getMaxValue() );
    int newMinVal = ::std::min( enumAve, accum.getMinValue() );
    if (bitmask_) {
        newMaxVal = enumAve | accum.getMaxValue();
        newMinVal = enumAve | accum.getMinValue();
    }

    nValidSamples_ = getNumValidSamples();

    // New way
    accum.setAccumulator( static_cast< long >( newMaxVal ) );

    // Old way we decided we didn't want - TWC 5 Feb 2007
    // accum.setAccumulator (enumAve);

    accum.setValidity (getAveValidity());
    accum.setBlanking (getBlankingFlagging());
    accum.incrementNumTotalSamples (getNumSamples());
    accum.incrementNumValidSamples (nValidSamples_);

    // New way
    accum.setMaxValue( newMaxVal );
    accum.setMinValue( newMinVal );

    // Old way we decided we didn't want - TWC 5 Feb 2007
    // accum.setMaxValue (getAveLong());
    // accum.setMinValue (getAveLong());
}


int MonitorPointEnum::getAccumulatedAverage
                           (const MonitorPointAverageEnum& accumulator) const
{
    return accumulator.getAccumulator();
}


int MonitorPointEnum::getMaxValue (const MonitorPointAverageEnum& accumulator) const
{
    return accumulator.getMaxValue();
}


int MonitorPointEnum::getMinValue (const MonitorPointAverageEnum& accumulator) const
{
    return accumulator.getMinValue();
}



// Take the last legit value
void
MonitorPointEnum::updateFrameAverage( ScratchAverages & scratchAvgs )
{
    if (getNumSamples() <= 1) return;

    resetAccumulator (scratchAvgs.enumAccum);
    accumulate (scratchAvgs.enumAccum);

    setAve (static_cast<long>(getAccumulatedAverage (scratchAvgs.enumAccum)));

    const VALIDITY sanitizedValidity =
        sanityCheckValidity(
            scratchAvgs.enumAccum.getValidity(),
            this,
            true,
            "MonitorPointEnum::updateFrameAverage" );

    setAveValidity( sanitizedValidity );
    setBlankingFlagging (scratchAvgs.enumAccum.getBlanking());
    nValidSamples_ = scratchAvgs.enumAccum.getNumValidSamples();
}

string MonitorPointEnum::getAccumulatedAverageAsString (const MonitorPointAverageEnum& accumulator) const
{
    ostringstream os;
    os << getAccumulatedAverage (accumulator);
    return os.str();
}


enum MonitorPoint::VALIDITY
MonitorPointEnum::evaluateSampleTolerance(
    const MonitorPointThreshold & t,
    const int                     enumVal ) const
{
    CARMA_CHECK( enumVal >= 0 );
    CARMA_CHECK( enumVal < static_cast< int >( sizeof( int ) ) );

    long valAsBits = 1 << enumVal;

    enum MonitorPoint::VALIDITY v = VALID_GOOD;
    if (! t.isSet())
        return v;

    unsigned long errHi  = t.getLongThresholdValue(THRESHOLD_HIGH_ERROR_VALUE);
    unsigned long errLo  = t.getLongThresholdValue(THRESHOLD_LOW_ERROR_VALUE);
    unsigned long warnHi = t.getLongThresholdValue(THRESHOLD_HIGH_WARN_VALUE);
    unsigned long warnLo = t.getLongThresholdValue(THRESHOLD_LOW_WARN_VALUE);

         if (t.errorHighIsSet()  &&  ((valAsBits & errHi) != 0))  v = VALID_ERROR;
    else if (t.errorLowIsSet()   &&  ((valAsBits & errLo) != 0))  v = VALID_ERROR;
    else if (t.warnHighIsSet()   &&  ((valAsBits & warnHi) != 0)) v = VALID_WARNING;
    else if (t.warnLowIsSet()    &&  ((valAsBits & warnLo) != 0)) v = VALID_WARNING;
    else v = VALID_GOOD;

    return v;
}


void
MonitorPointEnum::evaluateTolerance( const MonitorPointThreshold & t )
{
    const int numSamps = getNumSamples();

    for ( int i = 0; i < numSamps; ++i ) {
        const enum MonitorPoint::VALIDITY v = getValidity(i);
        if ( isValid(v) ) {
            const int enumVal = getValue(i);
            setValidity( evaluateSampleTolerance( t, enumVal ), i );
        }
    }

    if ( numSamps > 1 ) {
        const enum MonitorPoint::VALIDITY v = getAveValidity();
        if ( isValid(v) ) {
            const int enumVal = getAve();
            setAveValidity( evaluateSampleTolerance( t, enumVal ) );
        }
    }
}


double
MonitorPointFloat::getValueNumeric( const int sampleIndex ) const
{
    return static_cast< double >( getValue( sampleIndex ) );
}


double
MonitorPointFloat::getAveNumeric( ) const
{
    return static_cast< double >( getAve() );
}


float
MonitorPointFloat::getAccumulatedAverage(
    const MonitorPointAverageNumeric & accum ) const
{
    return static_cast< float >( getAccumulatedAverageDouble( accum ) );
}


float
MonitorPointFloat::getMaxValue(
    const MonitorPointAverageNumeric & accum ) const
{
    return static_cast< float >( getMaxValueDouble( accum ) );
}


float
MonitorPointFloat::getMinValue(
    const MonitorPointAverageNumeric & accum ) const
{
    return static_cast< float >( getMinValueDouble( accum ) );
}


void
MonitorPointFloat::updateFrameAverage( ScratchAverages & scratchAvgs )
{
    if ( getNumSamples() <= 1 )
        return;

    setAve(
        static_cast< float >(
            computeFrameAverage( scratchAvgs.numericAccum ) ) );

    const VALIDITY sanitizedValidity =
        sanityCheckValidity(
            getAveValidityNumeric(),
            this,
            true,
            "MonitorPointFloat::updateFrameAverage" );

    setAveValidity( sanitizedValidity );
    setBlankingFlagging( getBlankingFlaggingNumeric() );
}


string
MonitorPointFloat::getAccumulatedAverageAsString(
    const MonitorPointAverageNumeric & accum) const
{
    ostringstream oss;

    oss << getAccumulatedAverage( accum );

    return oss.str();
}


enum MonitorPoint::VALIDITY
MonitorPointFloat::evaluateSampleTolerance(
    const MonitorPointThreshold & t,
    const float                   val ) const
{
    enum MonitorPoint::VALIDITY v = VALID_GOOD;

    if ( ! t.isSet() )
        return v;

    const float errHi  = t.getFloatThresholdValue(THRESHOLD_HIGH_ERROR_VALUE);
    const float errLo  = t.getFloatThresholdValue(THRESHOLD_LOW_ERROR_VALUE);
    const float warnHi = t.getFloatThresholdValue(THRESHOLD_HIGH_WARN_VALUE);
    const float warnLo = t.getFloatThresholdValue(THRESHOLD_LOW_WARN_VALUE);

         if (t.errorHighIsSet()  &&  (val >= errHi))  v = VALID_ERROR_HIGH;
    else if (t.errorLowIsSet()   &&  (val <= errLo))  v = VALID_ERROR_LOW;
    else if (t.warnHighIsSet()   &&  (val >= warnHi)) v = VALID_WARNING_HIGH;
    else if (t.warnLowIsSet()    &&  (val <= warnLo)) v = VALID_WARNING_LOW;
    else v = VALID_GOOD;

    return v;
}


void
MonitorPointFloat::evaluateTolerance( const MonitorPointThreshold & t )
{
    const int numSamps = getNumSamples();

    for ( int i = 0; i < numSamps; ++i ) {
        const enum MonitorPoint::VALIDITY v = getValidity(i);
        if ( isValid(v) ) {
            const float floatVal = getValue(i);
            setValidity( evaluateSampleTolerance( t, floatVal ), i );
        }
    }

    if ( numSamps > 1 ) {
        const enum MonitorPoint::VALIDITY v = getAveValidity();
        if ( isValid(v) ) {
            const float floatVal = getAve();
            setAveValidity( evaluateSampleTolerance( t, floatVal ) );
        }
    }
}


double
MonitorPointDouble::getValueNumeric( const int sampleIndex ) const
{
    return static_cast< double >( getValue( sampleIndex ) );
}


double
MonitorPointDouble::getAveNumeric( ) const
{
    return static_cast< double >( getAve() );
}


double MonitorPointDouble::getAccumulatedAverage (const MonitorPointAverageNumeric& accumulator) const
{
    return getAccumulatedAverageDouble (accumulator);
}


double MonitorPointDouble::getMaxValue (const MonitorPointAverageNumeric& accumulator) const
{
    return getMaxValueDouble (accumulator);
}


double MonitorPointDouble::getMinValue (const MonitorPointAverageNumeric& accumulator) const
{
    return getMinValueDouble (accumulator);
}


void
MonitorPointDouble::updateFrameAverage( ScratchAverages & scratchAvgs )
{
    if (getNumSamples() <= 1) return;

    setAve(computeFrameAverage( scratchAvgs.numericAccum ));

    const VALIDITY sanitizedValidity =
        sanityCheckValidity(
            getAveValidityNumeric(),
            this,
            true,
            "MonitorPointDouble::updateFrameAverage" );

    setAveValidity( sanitizedValidity );
    setBlankingFlagging (getBlankingFlaggingNumeric());
}

string MonitorPointDouble::getAccumulatedAverageAsString (const MonitorPointAverageNumeric& accumulator) const
{
    ostringstream os;
    os << getAccumulatedAverage (accumulator);
    return os.str();
}

enum MonitorPoint::VALIDITY
MonitorPointDouble::evaluateSampleTolerance(
    const MonitorPointThreshold & t,
    const double                  val ) const
{
    enum MonitorPoint::VALIDITY v = VALID_GOOD;
    if (! t.isSet())
        return v;

    double errHi  = t.getDoubleThresholdValue(THRESHOLD_HIGH_ERROR_VALUE);
    double errLo  = t.getDoubleThresholdValue(THRESHOLD_LOW_ERROR_VALUE);
    double warnHi = t.getDoubleThresholdValue(THRESHOLD_HIGH_WARN_VALUE);
    double warnLo = t.getDoubleThresholdValue(THRESHOLD_LOW_WARN_VALUE);

         if (t.errorHighIsSet()  &&  (val >= errHi))  v = VALID_ERROR_HIGH;
    else if (t.errorLowIsSet()   &&  (val <= errLo))  v = VALID_ERROR_LOW;
    else if (t.warnHighIsSet()   &&  (val >= warnHi)) v = VALID_WARNING_HIGH;
    else if (t.warnLowIsSet()    &&  (val <= warnLo)) v = VALID_WARNING_LOW;
    else v = VALID_GOOD;

    return v;
}


void
MonitorPointDouble::evaluateTolerance( const MonitorPointThreshold & t )
{
    const int numSamps = getNumSamples();

    for ( int i = 0; i < numSamps; ++i ) {
        const enum MonitorPoint::VALIDITY v = getValidity(i);
        if ( isValid(v) ) {
            const double doubleVal = getValue(i);
            setValidity( evaluateSampleTolerance( t, doubleVal ), i );
        }
    }

    if ( numSamps > 1 ) {
        const enum MonitorPoint::VALIDITY v = getAveValidity();
        if ( isValid(v) ) {
            const double doubleVal = getAve();
            setAveValidity( evaluateSampleTolerance( t, doubleVal ) );
        }
    }
}


void MonitorPointComplex::resetAccumulator
                                 (MonitorPointAverageComplex& accumulator) const
{
    complex< float > total = complex< float >( 0.0f, 0.0f );
    accumulator.setAccumulator (total);
    accumulator.setMaxValue( complex< float >( 0.0f, 0.0f ) );
    accumulator.setMinValue( complex< float >( MAXFLOAT, MAXFLOAT ) );
    accumulator.resetAveProperties();
}


enum MonitorPoint::VALIDITY MonitorPointComplex::accumulateSample(
        MonitorPointAverageComplex& accumulator, int index) const
{
    enum MonitorPoint::VALIDITY v;

    v = getValidity(index);
    if (isValid(v)) {
        accumulator.incrementAccumulator(getValue (index));
        accumulator.incrementNumValidSamples();
        if(::std::norm(getValue(index)) > ::std::norm(accumulator.getMaxValue())) {
            accumulator.setMaxValue(getValue(index));
        }
        if(::std::norm(getValue(index)) < ::std::norm(accumulator.getMinValue())) {
            accumulator.setMinValue(getValue(index));
        }

    }
    accumulator.incrementNumTotalSamples ();

    return v;
}


void MonitorPointComplex::accumulate(MonitorPointAverageComplex& accumulator) const
{
    enum MonitorPoint::VALIDITY v = INVALID_NO_DATA;
    const int nSamples = getNumSamples();

    for (int i=0; i<nSamples; i++) {
        v = accumulateSample (accumulator, i);
    }

    enum MonitorPoint::VALIDITY aveValidity = accumulator.getValidity();
    if (v > aveValidity) aveValidity = v;

    if (accumulator.getNumValidSamples() == 0) {
        if (accumulator.getNumTotalSamples() == 0)  {
            aveValidity = INVALID_NO_DATA;
        }  else  {
            aveValidity = v;
        }
    }
    else {
        // Some are good
        aveValidity = VALID_NOT_CHECKED;
    }

    accumulator.setValidity(aveValidity);
}


complex< float >
MonitorPointComplex::getMaxSampleValue () const
{
    complex< float > maxSample = getValue (0);
    for (int  i = 1;  i < getNumSamples();  i++)  {
        if (::std::norm(maxSample) < ::std::norm(getValue(i)))
            maxSample = getValue(i);
    }

    return maxSample;
}


complex< float >
MonitorPointComplex::getMinSampleValue () const
{
    complex< float > minSample = getValue(0);
    for (int  i = 1;  i < getNumSamples();  i++)  {
        if (::std::norm(minSample) > ::std::norm(getValue(i)))
            minSample = getValue(i);
    }

    return minSample;
}


void
MonitorPointComplex::accumulateAverage( MonitorPointAverageComplex & accum )
{
    const int numSamples = getNumSamples();
    nValidSamples_       = getNumValidSamples();

    if (isValid()) {
        // Snapshot average is just a strobe sample of the most recent value
        // done by setting accum to most recent val and numSamp to 1.
        if (isSnapshotAverage()) {
            accum.setAccumulator(getAve());
            accum.setNumValidSamples(1);
        }
        else {
            accum.incrementAccumulator(getAve());
            accum.incrementNumValidSamples(1);
        }
    }
    accum.incrementNumTotalSamples();
    accum.setValidity( getAveValidity() );
    accum.setBlanking( getBlankingFlagging() );

    float maxNorm = ::std::norm( accum.getMaxValue() );
    float minNorm = ::std::norm( accum.getMinValue() );

    for ( int i = 0; i < numSamples; ++i ) {
        const complex< float > sampVal = getValue( i );
        const float sampNorm = ::std::norm( sampVal );

        if ( sampNorm > maxNorm ) {
            accum.setMaxValue( sampVal );
            maxNorm = sampNorm;
        }

        if ( sampNorm < minNorm ) {
            accum.setMinValue( sampVal );
            minNorm = sampNorm;
        }
    }
}


complex< float >
MonitorPointComplex::getAccumulatedAverage
                         (const MonitorPointAverageComplex& accum) const
{
    if (accum.getNumValidSamples() == 0) {
        return  complex< float >( 0.0f, 0.0f );
    }
    float samps = accum.getNumValidSamples();
    return accum.getAccumulator()/samps;
}


complex< float >
MonitorPointComplex::getMaxValue (const MonitorPointAverageComplex& accumulator) const
{
    return (accumulator.getNumValidSamples() == 0)
             ?  complex< float >( 0.0f, 0.0f )
             :  accumulator.getMaxValue() ;
}


complex< float >
MonitorPointComplex::getMinValue (const MonitorPointAverageComplex& accumulator) const
{
    return (accumulator.getNumValidSamples() == 0)
             ?  complex< float >( 0.0f, 0.0f )
             :  accumulator.getMinValue() ;
}



void
MonitorPointComplex::updateFrameAverage( ScratchAverages & scratchAvgs )
{
    if (getNumSamples() <= 1) return;

    resetAccumulator (scratchAvgs.complexAccum);
    accumulate (scratchAvgs.complexAccum);

    setAve (scratchAvgs.complexAccum.getAccumulator());

    const VALIDITY sanitizedValidity =
        sanityCheckValidity(
            scratchAvgs.complexAccum.getValidity(),
            this,
            true,
            "MonitorPointComplex::updateFrameAverage" );

    setAveValidity( sanitizedValidity );
    setBlankingFlagging (scratchAvgs.complexAccum.getBlanking());
    nValidSamples_ = scratchAvgs.complexAccum.getNumValidSamples();
}

string MonitorPointComplex::getAccumulatedAverageAsString (const MonitorPointAverageComplex& accumulator) const
{
    ostringstream os;
    os << getAccumulatedAverage (accumulator);
    return os.str();
}


enum MonitorPoint::VALIDITY
MonitorPointComplex::evaluateSampleTolerance(
    const MonitorPointThreshold & t,
    const complex< float >        cmplx ) const
{
    enum MonitorPoint::VALIDITY v = VALID_GOOD;
    if (! t.isSet())
        return v;

    float val = ::std::norm(cmplx);

    float errHi  = ::std::norm(t.getComplexThresholdValue(THRESHOLD_HIGH_ERROR_VALUE));
    float errLo  = ::std::norm(t.getComplexThresholdValue(THRESHOLD_LOW_ERROR_VALUE));
    float warnHi = ::std::norm(t.getComplexThresholdValue(THRESHOLD_HIGH_WARN_VALUE));
    float warnLo = ::std::norm(t.getComplexThresholdValue(THRESHOLD_LOW_WARN_VALUE));

         if (t.errorHighIsSet()  &&  (val >= errHi))  v = VALID_ERROR_HIGH;
    else if (t.errorLowIsSet()   &&  (val <= errLo))  v = VALID_ERROR_LOW;
    else if (t.warnHighIsSet()   &&  (val >= warnHi)) v = VALID_WARNING_HIGH;
    else if (t.warnLowIsSet()    &&  (val <= warnLo)) v = VALID_WARNING_LOW;
    else v = VALID_GOOD;

    return v;
}


void
MonitorPointComplex::evaluateTolerance( const MonitorPointThreshold & t )
{
    const int numSamps = getNumSamples();

    for ( int i = 0; i < numSamps; ++i ) {
        const enum MonitorPoint::VALIDITY v = getValidity(i);
        if ( isValid(v) ) {
            const complex< float > complexVal = getValue(i);
            setValidity( evaluateSampleTolerance( t, complexVal ), i );
        }
    }

    if ( numSamps > 1 ) {
        const enum MonitorPoint::VALIDITY v = getAveValidity();
        if ( isValid(v) ) {
            const complex< float > complexVal = getAve();
            setAveValidity( evaluateSampleTolerance( t, complexVal ) );
        }
    }
}


double
MonitorPointAbstime::getValueNumeric( const int sampleIndex ) const
{
    return static_cast< double >( getValue( sampleIndex ) );
}


double
MonitorPointAbstime::getAveNumeric( ) const
{
    return static_cast< double >( getAve() );
}


double MonitorPointAbstime::getAccumulatedAverage (const MonitorPointAverageNumeric& accumulator) const
{
    return getAccumulatedAverageDouble (accumulator);
}


void
MonitorPointAbstime::updateFrameAverage( ScratchAverages & scratchAvgs )
{
    if (getNumSamples() <= 1) return;

    setAve(computeFrameAverage( scratchAvgs.numericAccum ));

    const VALIDITY sanitizedValidity =
        sanityCheckValidity(
            getAveValidityNumeric(),
            this,
            true,
            "MonitorPointAbstime::updateFrameAverage" );

    setAveValidity( sanitizedValidity );
    setBlankingFlagging (getBlankingFlaggingNumeric());
}

double MonitorPointAbstime::getMaxValue (const MonitorPointAverageNumeric& accumulator) const
{
    return (accumulator.getNumValidSamples() == 0)
             ?  0.0
             :  getMaxValueDouble (accumulator) ;
}


double MonitorPointAbstime::getMinValue (const MonitorPointAverageNumeric& accumulator) const
{
    return (accumulator.getNumValidSamples() == 0)
             ?  0.0
             :  getMinValueDouble (accumulator) ;
}



string MonitorPointAbstime::getAccumulatedAverageAsString (const MonitorPointAverageNumeric& accumulator) const
{
    ostringstream os;
    return Time::getDateTimeString(getAccumulatedAverageDouble(accumulator),
        getPrecision());
}


enum MonitorPoint::VALIDITY
MonitorPointAbstime::evaluateSampleTolerance(
    const MonitorPointThreshold & t,
    const double                  val ) const
{
    enum MonitorPoint::VALIDITY v = VALID_GOOD;
    if (! t.isSet())
        return v;

    double errHi  = t.getDoubleThresholdValue(THRESHOLD_HIGH_ERROR_VALUE);
    double errLo  = t.getDoubleThresholdValue(THRESHOLD_LOW_ERROR_VALUE);
    double warnHi = t.getDoubleThresholdValue(THRESHOLD_HIGH_WARN_VALUE);
    double warnLo = t.getDoubleThresholdValue(THRESHOLD_LOW_WARN_VALUE);

         if (t.errorHighIsSet()  &&  (val >= errHi))  v = VALID_ERROR_HIGH;
    else if (t.errorLowIsSet()   &&  (val <= errLo))  v = VALID_ERROR_LOW;
    else if (t.warnHighIsSet()   &&  (val >= warnHi)) v = VALID_WARNING_HIGH;
    else if (t.warnLowIsSet()    &&  (val <= warnLo)) v = VALID_WARNING_LOW;
    else v = VALID_GOOD;

    return v;
}


void
MonitorPointAbstime::evaluateTolerance( const MonitorPointThreshold & t )
{
    const int numSamps = getNumSamples();

    for ( int i = 0; i < numSamps; ++i ) {
        const enum MonitorPoint::VALIDITY v = getValidity(i);
        if ( isValid(v) ) {
            const double abstimeVal = getValue(i);
            setValidity( evaluateSampleTolerance( t, abstimeVal ), i );
        }
    }

    if ( numSamps > 1 ) {
        const enum MonitorPoint::VALIDITY v = getAveValidity();
        if ( isValid(v) ) {
            const double abstimeVal = getAve();
            setAveValidity( evaluateSampleTolerance( t, abstimeVal ) );
        }
    }
}


void MonitorPointString::resetAccumulator
                                 (MonitorPointAverageString& accumulator) const
{
    const string initialVal;
    const string maxVal     = "zzZZ";

    accumulator.setAccumulator (initialVal);
    accumulator.setMaxValue (initialVal);
    accumulator.setMinValue (maxVal); // should be set to real
                                      // last lexicographic string
    accumulator.resetAveProperties();
}


enum MonitorPoint::VALIDITY MonitorPointString::accumulateSample (MonitorPointAverageString& accumulator, int index) const
{
    // do nothing - strings have no "samples"
    return VALID_NOT_CHECKED;
}

void MonitorPointString::accumulate (MonitorPointAverageString& accumulator) const
{
    // how do I count number of samples for a string ?
    accumulator.incrementNumTotalSamples (1);
    accumulator.incrementNumValidSamples (1);

    accumulator.setAccumulator (getValue());
    // use lexicographic order to determine max and min
    accumulator.setMaxValue (::std::max (getValue(), accumulator.getMaxValue()));
    accumulator.setMinValue (::std::min (getValue(), accumulator.getMinValue()));

    // dont worry about validity ?
    accumulator.setValidity(VALID_NOT_CHECKED);
}


void MonitorPointString::accumulateAverage (MonitorPointAverageString& accumulator)
{
    // Logic to determine if we take this sample or keep the one in the accum
    // If this one is good or if accum is invalid, store this sample
    bool setAccumToThisSample = true;
    if (isValid()) {
        setAccumToThisSample = true;
    }
    else {
        if (isValid(accumulator.getValidity())) {
            setAccumToThisSample = false;
        }
        else {
            setAccumToThisSample = true;
        }
    }
   
    if (setAccumToThisSample) {
        accumulator.setAccumulator (getAve());
        accumulator.setValidity (getAveValidity());
        accumulator.setBlanking (getBlankingFlagging());
        if (isValid()) accumulator.incrementNumValidSamples (1);
    } 
    accumulator.incrementNumTotalSamples (1);
    accumulator.setMaxValue (max (getValue(), accumulator.getMaxValue()));
    accumulator.setMinValue (min (getValue(), accumulator.getMinValue()));
}


string MonitorPointString::getAccumulatedAverage
                         (const MonitorPointAverageString& accumulator) const
{
    return accumulator.getAccumulator();
}


string MonitorPointString::getMaxValue (const MonitorPointAverageString& accumulator) const
{
    return accumulator.getMaxValue();
}


string MonitorPointString::getMinValue (const MonitorPointAverageString& accumulator) const
{
    return accumulator.getMinValue();
}



void
MonitorPointString::updateFrameAverage( ScratchAverages & scratchAvgs )
{
    if (getNumSamples() <= 1) return;

    const VALIDITY sanitizedValidity =
        sanityCheckValidity(
            getValidity(),
            this,
            true,
            "MonitorPointString::updateFrameAverage" );

    setAveValidity( sanitizedValidity );

    return;

    /*** do nothing - the string is the "average"
    if (getNumSamples() <= 1) return;

    MonitorPointAverageString accumulator ;
    resetAccumulator (accumulator);
    accumulate (accumulator);

    setAveValidity (accumulator.getValidity());
    setBlankingFlagging (accumulator.getBlanking());
    ***/
}

string MonitorPointString::getAccumulatedAverageAsString (const MonitorPointAverageString& accumulator) const
{
    ostringstream os;
    os << getAccumulatedAverage (accumulator);
    return os.str();
}


enum MonitorPoint::VALIDITY
MonitorPointString::evaluateSampleTolerance(
    const MonitorPointThreshold & t,
    const string                  val ) const
{
    enum MonitorPoint::VALIDITY v = VALID_GOOD;
    if (! t.isSet())
        return v;

    string errHi  = t.getStringThresholdValue(THRESHOLD_HIGH_ERROR_VALUE);
    string errLo  = t.getStringThresholdValue(THRESHOLD_LOW_ERROR_VALUE);
    string warnHi = t.getStringThresholdValue(THRESHOLD_HIGH_WARN_VALUE);
    string warnLo = t.getStringThresholdValue(THRESHOLD_LOW_WARN_VALUE);

         if (t.errorHighIsSet()  &&  (val == errHi))  v = VALID_ERROR_HIGH;
    else if (t.errorLowIsSet()   &&  (val == errLo))  v = VALID_ERROR_LOW;
    else if (t.warnHighIsSet()   &&  (val == warnHi)) v = VALID_WARNING_HIGH;
    else if (t.warnLowIsSet()    &&  (val == warnLo)) v = VALID_WARNING_LOW;
    else v = VALID_GOOD;

    return v;
}


void
MonitorPointString::evaluateTolerance( const MonitorPointThreshold & t )
{
    // do nothing - validity of first sample is validity of string
    // setValidity (VALID_GOOD);
    const enum MonitorPoint::VALIDITY v = getValidity();
    if ( isValid(v) ) {
        string strVal = getValue();
        evaluateSampleTolerance( t , strVal );
    }
}


void MonitorPointSerialNo::resetAccumulator
                                 (MonitorPointAverageSerialNo& accumulator) const
{
    accumulator.setAccumulator (0);
    accumulator.setMaxValue (INT_MIN);
    accumulator.setMinValue (INT_MAX);
    accumulator.resetAveProperties();
}


enum MonitorPoint::VALIDITY MonitorPointSerialNo::accumulateSample (MonitorPointAverageSerialNo& accumulator, int index) const
{
    // do nothing - has only one sample, and average computation already
    // takes care of this
    return VALID_NOT_CHECKED;
}


// take snapshot only
void MonitorPointSerialNo::accumulate (MonitorPointAverageSerialNo& accumulator) const
{
    int nSamples = getNumSamples();
    CARMA_CHECK (nSamples == 1);
    accumulator.incrementNumTotalSamples (nSamples);
    accumulator.incrementNumValidSamples (nSamples);

    // greatest serial # ?
    accumulator.setMaxValue (::std::max (static_cast<long>(getValue()), accumulator.getMaxValue()));
    // least serial # ?
    accumulator.setMinValue (::std::min (static_cast<long>(getValue()), accumulator.getMinValue()));

    accumulator.setAccumulator (getValue());

    // dont bother with validity ?
    accumulator.setValidity (VALID_NOT_CHECKED);
}


void MonitorPointSerialNo::accumulateAverage (MonitorPointAverageSerialNo& accumulator)
{
    accumulator.setAccumulator (getAve());
    accumulator.setValidity (getAveValidity());
    accumulator.setBlanking (getBlankingFlagging());
    accumulator.incrementNumTotalSamples (getNumSamples());
    accumulator.incrementNumValidSamples (1);
    accumulator.setMaxValue (::std::max (static_cast<long>(getValue()), accumulator.getMaxValue()));
    accumulator.setMinValue (::std::min (static_cast<long>(getValue()), accumulator.getMinValue()));
}


long MonitorPointSerialNo::getAccumulatedAverage
                       (const MonitorPointAverageSerialNo& accumulator) const
{
    return accumulator.getAccumulator();
}


long MonitorPointSerialNo::getMaxValue (const MonitorPointAverageSerialNo& accumulator) const
{
    return accumulator.getMaxValue();
}


long MonitorPointSerialNo::getMinValue (const MonitorPointAverageSerialNo& accumulator) const
{
    return accumulator.getMinValue();
}



// Only one sample allowed, so do nothing
void
MonitorPointSerialNo::updateFrameAverage( ScratchAverages & scratchAvgs )
{
    // Don't do anything!!
}


string MonitorPointSerialNo::getAccumulatedAverageAsString (const MonitorPointAverageSerialNo& accumulator) const
{
    ostringstream os;
    os << getAccumulatedAverage (accumulator);
    return os.str();
}

enum MonitorPoint::VALIDITY
MonitorPointSerialNo::evaluateSampleTolerance(
    const MonitorPointThreshold & threshold,
    const long                    serialNoVal ) const
{
    enum MonitorPoint::VALIDITY v = VALID_GOOD;

    /**
     * No thresholding required for serial number
     */

    return v;
}


void
MonitorPointSerialNo::evaluateTolerance( const MonitorPointThreshold & t )
{
    const int numSamps = getNumSamples();

    for ( int i = 0; i < numSamps; ++i ) {
        const enum MonitorPoint::VALIDITY v = getValidity(i);
        if ( isValid(v) ) {
            const long serialNoVal = getValue(i);
            setValidity( evaluateSampleTolerance( t, serialNoVal ), i );
        }
    }

    if ( numSamps > 1 ) {
        const enum MonitorPoint::VALIDITY v = getAveValidity();
        if ( isValid(v) ) {
            const long serialNoVal = getAve();
            setAveValidity( evaluateSampleTolerance( t, serialNoVal ) );
        }
    }
}
