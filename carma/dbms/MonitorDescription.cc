/**
 * $Id: MonitorDescription.cc,v 1.31 2011/12/21 22:56:43 mpound Exp $
 * MonitorDescription class
 * - holds monitor point information ... for use with XML parser
 *
 */

#include <iomanip>
#include <iostream>
#include <sstream>
#include <cstdlib>
#include <cmath>

#include <values.h>

#include "carma/util/Trace.h"
#include "carma/util/ErrorException.h"
#include "carma/dbms/ColumnNames.h"
#include "carma/dbms/MonitorDescription.h"

using namespace ::std;
using namespace carma::monitor;
using namespace carma::dbms;


MonitorDescription::MonitorDescription(
    const string &               canonicalName,
    const MonitorPointType &     mpType, 
    const MonitorPointDataType & dataType,
    const bool &                 persistent, 
    const unsigned &                 updateInterval,
    const bool &                 spectrum ) :
units_(""),
integrateFunction_(""),
time_(carma::util::Time::computeCurrentFrame()),
warnLo_(0.0),
warnHi_(0.0),
errLo_(0.0),
errHi_(0.0),
isDefaultWarnLo_(true), 
isDefaultWarnHi_(true),
isDefaultErrLo_(true),
isDefaultErrHi_(true) {
    cname_ = canonicalName;
    monitorPointType_ = mpType;
    dataType_ = dataType;
    persistent_ = persistent;
    updateInterval_ = updateInterval;
    spectrum_ = spectrum;
    // default here, location_ and device_ can be set by mutator methods
    location_ = "NOLOCATION";
    device_ = "NODEVICE";
    // shortName_, longName_, and description_ can be set by mutator methods
    /*
    unsigned maxLength = MonitorConfigurationDatabase::shortNameMaxLength();
    shortName_ = (cname_.length() > maxLength) 
        ? cname_.substr(cname_.length()-maxLength+1) : cname_;
    */
    shortName_ = cname_;
    longName_ = cname_;
    description_ = cname_;
}

string MonitorDescription::toString() const {
    ostringstream d;
    d << "canonical name: " << cname_ << endl;
    d << "monitor point type: ";
    d << carma::dbms::toString(monitorPointType_) << endl;
    d << "data type: ";
    d << carma::dbms::toString(dataType_) << endl;
    d << "update interval: " << updateInterval_ << endl;

    d << "is point persistent: ";
    if(persistent_) {
        d << "yes";
    } else {
        d << "no";
    }
    d << endl;
    d << "is point a spectrum: ";
    if(spectrum_) {
        d << "yes";
    } else {
        d << "no";
    }
    d << endl;
    d << "short name: " << shortName_ << endl;
    d << "long name: " << longName_ << endl;
    d << "units: " << units_ << endl;
    d << "description: " << description_ << endl;
    d << "integrate function: " << integrateFunction_ << endl;
    d << "description became valid at: " << time_ << endl;
    if (dataType_ == DATATYPE_ENUMERATION) {
        d << "Enumerators:" << endl;
        for(unsigned int i=0; i < enumeratorNames_.size() ; i++) {
            d << i << " " << enumeratorNames_[i] << " " 
              << enumeratorDescriptions_.find(enumeratorNames_[i])->second 
              << endl;
        }
    }
    d << setprecision(24);
    d << "Low error threshold " 
      << getThresholdValue(THRESHOLD_LOW_ERROR_VALUE) << " (is ";
    if(!isDefaultErrLo_) {
        d << "not ";
    }
    d << "default)" << endl;
    d << "Low warning threshold " 
      << getThresholdValue(THRESHOLD_LOW_WARN_VALUE) << " (is ";
    if(!isDefaultWarnLo_) {
        d << "not ";
    }
    d << "default)" << endl;
    d << "High warning threshold " 
      << getThresholdValue(THRESHOLD_HIGH_WARN_VALUE) << " (is ";
    if(!isDefaultWarnHi_) {
        d << "not ";
    }
    d << "default)" << endl;
    d << "High error threshold " 
      << getThresholdValue(THRESHOLD_HIGH_ERROR_VALUE) << " (is ";
    if(!isDefaultErrHi_) {
        d << "not ";
    }
    d << "default)" << endl;
    d << "size: " << sizeof(*this) << " bytes" << endl;
    return d.str();
}

//FIXME need to add enumerators as well
bool MonitorDescription::isEqualExceptForTime(const MonitorDescription& md) 
    const {
    string msg;
    if(!areStaticFieldsEqual(md,msg)) {
        return false;
    }
    if (updateInterval_ != md.getUpdateInterval()) { return false; }
    if (shortName_ != md.getShortName()) { return false; }
    if (longName_ != md.getLongName()) { return false; }
    if (description_ != md.getDescription()) { return false; }
    if (integrateFunction_ != md.getIntegrateFunction()) { return false; }

    // thresholds are special cases in that they can be NULL in the database,
    // in which case their values are the "default value" in the C++ code
    // so a couple of checks are necessary to prove equivalency

    // the first test is for default values, if one description uses a default
    // threshold value while the other does not, this is a sufficient test
    // to prove inequality
    if(isDefaultErrLo_ 
       != md.isDefaultThresholdValue(THRESHOLD_LOW_ERROR_VALUE)) 
        { return false; }
    if(isDefaultErrHi_ 
       != md.isDefaultThresholdValue(THRESHOLD_HIGH_ERROR_VALUE))
        { return false; }
    if(isDefaultWarnLo_ 
       != md.isDefaultThresholdValue(THRESHOLD_LOW_WARN_VALUE))
        { return false; }
    if(isDefaultWarnHi_ 
       != md.isDefaultThresholdValue(THRESHOLD_HIGH_WARN_VALUE))
        { return false; }

    // the next test needs to be done if both threshold values are not the
    // default values; that is, the threshold in question has been explicitly
    // set.  In this case, we must use a small fractional tolerance to check
    // for equality to avoid differences due to round off errors
    double fepsilon = 1e-10;
    if(!isDefaultErrLo_ 
       && !md.isDefaultThresholdValue(THRESHOLD_LOW_ERROR_VALUE)
       && abs(1-(errLo_/getThresholdValue(THRESHOLD_LOW_ERROR_VALUE))) 
       > fepsilon) { return false; }
    if(!isDefaultErrHi_ 
       && !md.isDefaultThresholdValue(THRESHOLD_HIGH_ERROR_VALUE)
       && abs(1-(errHi_/getThresholdValue(THRESHOLD_HIGH_ERROR_VALUE))) 
       > fepsilon) { return false; }
    if(!isDefaultWarnLo_ 
       && !md.isDefaultThresholdValue(THRESHOLD_LOW_WARN_VALUE)
       && abs(1-(warnLo_/getThresholdValue(THRESHOLD_LOW_WARN_VALUE))) 
       > fepsilon) { return false; }
    if(!isDefaultWarnHi_ 
       && !md.isDefaultThresholdValue(THRESHOLD_HIGH_WARN_VALUE)
       && abs(1-(warnHi_/getThresholdValue(THRESHOLD_HIGH_WARN_VALUE))) 
       > fepsilon) { return false; }


    // because enumerators can be added, two types of enumoerator comparisons
    // need to be done here and in areStaticFieldsEqual
    // the comparison here checks to see if exactly the same enumerators (in
    // the same order) are in both MonitorDescriptions
    if (dataType_ == DATATYPE_ENUMERATION) {
        vector<string> otherEnumNames = md.getEnumerators();
        if(enumeratorNames_.size() != otherEnumNames.size()) { return false; }
        map<string,string> myEnumDescs = getEnumeratorDescriptions();
        map<string,string> otherEnumDescs = md.getEnumeratorDescriptions();
        for(unsigned int i = 0; i < enumeratorNames_.size(); i++) {
            if(enumeratorNames_[i] != otherEnumNames[i]) { return false; }
            if(myEnumDescs[enumeratorNames_[i]] 
               != otherEnumDescs[otherEnumNames[i]]) { return false; }
        }
    }
    return true;
}

void MonitorDescription::makeStaticChangedString( ostringstream &ss,
			       const char *fieldName,
			       const string &oldValue,
			       const string &newValue)const
{
 string fn(fieldName);
 makeStaticChangedString(ss, fn, oldValue, newValue);
}

void MonitorDescription::makeStaticChangedString( ostringstream &ss,
			       const string &fieldName,
			       const string &oldValue,
			       const string &newValue)const
{
   ss << " name=\"" << cname_ << "\""
      << " changedField=\"" << fieldName << "\"" << " Old=\""
      << oldValue << "\" New=\"" << newValue << "\"";
}

// Convert a bool to a 0 or 1.
static const char *b2s(const bool v)
{ static const char *T = "1";
  static const char *F = "0";
  return (v) ? T : F;
}

bool MonitorDescription::areStaticFieldsEqual(const MonitorDescription& other,
                                              string& msg) const {
    ostringstream ss;
    if (cname_ != other.getName()) { 
#if 0
      msg = "Names are different \"" + cname_ + "\"!=\"" + other.getName()
	+ "\"";
#else
      string ov, nv;
      ov = cname_;
      nv = other.getName();
      makeStaticChangedString(ss, "name", ov, nv);
      msg = ss.str();
#endif
      return false; 
    }

    if (monitorPointType_ != other.getMonitorPointType()) { 
#if 0
        ss << "monitor point types are different \"" 
           << carma::dbms::toString(monitorPointType_) 
           << "\"!=\"" << carma::dbms::toString(other.getMonitorPointType())
	   << "\"";
#else
	string ov, nv;
	ov = carma::dbms::toString(monitorPointType_);
	nv = carma::dbms::toString(other.getMonitorPointType());
	makeStaticChangedString(ss, getColumnName(COLUMN_MPTYPEID), ov, nv);
#endif
        msg = ss.str();
        return false; 
    }

    if (dataType_ != other.getDataType()) {
#if 0
        ss << "data types are different \"" << carma::dbms::toString(dataType_)
           << "\"!=\"" << carma::dbms::toString(other.getDataType())
	   << "\"";
#else
	string ov, nv;
	ov = carma::dbms::toString(dataType_);
	nv = carma::dbms::toString(other.getDataType());
	makeStaticChangedString(ss, getColumnName(COLUMN_DATATYPEID), ov, nv);
#endif
        msg = ss.str();
        return false; 
    }    

    if (persistent_ != other.isPersistent()) { 
	{ string ov, nv;
	  ov = b2s(persistent_);
	  nv = b2s(other.isPersistent());
	  makeStaticChangedString(ss, "isPersistent", ov, nv);
	}
        msg = ss.str();
        return false; 
    }

    if (spectrum_ != other.isSpectrum()) { 
      string ov, nv;
      ov = b2s(spectrum_);
      nv = b2s(other.isSpectrum());
      makeStaticChangedString(ss, "isSpectrum", ov, nv);
      msg = ss.str();
      return false; 
    }

    if (units_ != other.getUnits()) { 
      makeStaticChangedString(ss, "units", units_, other.getUnits());
      msg = ss.str();
      return false; 
    }

    if (location_ != other.getLocation()) { 
#if 0
        ss << "locations are different \"" << location_ << "\"!=\"" 
           << other.getLocation() << "\"";
#else
	makeStaticChangedString(ss, getColumnName(COLUMN_LOCATIONID), location_,
				other.getLocation());
#endif
        msg = ss.str();
        return false; 
    }

    if (device_ != other.getDevice()) { 
#if 0
        ss << "Devices are different \"" << device_ << "\"!=\"" 
           << other.getDevice() << "\"";
#else
	makeStaticChangedString(ss, getColumnName(COLUMN_DEVICEID), device_,
				other.getDevice());
#endif
        msg = ss.str();
        return false; 
    }

    // keeping track of enumerators is a pain in the ass since they can be
    // added with time but once added, cannot change.  For the static field
    // test to pass, the first n enumerators must be equal, where n is the
    // minimum number of enums for each description.  While enumerator names,
    // once added, cannot change, enumerator descriptions are allowed to change
    // so they are not tested by this method
    if (dataType_ == DATATYPE_ENUMERATION) {
        vector<string> otherEnumNames = other.getEnumerators();
        int n = min(enumeratorNames_.size(),otherEnumNames.size());
	string ov(""), nv("");
	bool isOK = true;
        for(int i = 0; i < n; i++) {
	  if(i > 0)
	  { ov += ","; nv += ",";
	  }
	  ov += enumeratorNames_[i];
	  nv += otherEnumNames[i];
	  if(enumeratorNames_[i] != otherEnumNames[i]) { 
#if 0
                ss << "Enumerator names at index " << i << " are different \""
                   << enumeratorNames_[i] << "\"!=\"" << otherEnumNames[i]
		   << "\"";
                msg = ss.str();
                return false; 
#else
		isOK = false;
#endif
	      }
	}
	if(!isOK)
	{ makeStaticChangedString(ss, "!ENUMERATION", ov, nv);
	  msg = ss.str();
	  return false;
	}
    }
    return true;
}

#if 0
/* Returns a copy of another monitor description with possibly a different
 name &/or time.
*/
MonitorDescription MonitorDescription::makeCopy(
	const string& canonicalName, 
	carma::util::frameType time)const
{
  MonitorDescription md = *this;
  if(canonicalName != "")
    md.cname_ = canonicalName;
  if(time != 0)
    md.time_ = time;
  return md;
}
#endif

void MonitorDescription::addEnumerator(const string& enumerator, 
                                       const string& description) {
    if(dataType_ != DATATYPE_ENUMERATION) {
        string emsg = "Enumerators are only permitted for montitor points of ";
        emsg += "DATATYPE_ENUMERATION. This point is of type " 
            + carma::dbms::toString(dataType_);
        throw CARMA_ERROR(emsg);
    }
    enumeratorNames_.push_back(enumerator);
    enumeratorDescriptions_[enumerator] = description;
}

void MonitorDescription::setEnumeratorDescription(const string& enumerator, 
                                                  const string& description) {
    string emsg;
    if(dataType_ != DATATYPE_ENUMERATION) {
        emsg = "Enumerators are only permitted for montitor points of ";
        emsg += "DATATYPE_ENUMERATION. This point is of type " 
            + carma::dbms::toString(dataType_);
        throw CARMA_ERROR(emsg);
    }
    if (enumeratorDescriptions_.count(enumerator) == 0) {
        emsg = "Specified enumerator " + enumerator + " does not exist";
        throw CARMA_ERROR(emsg);
    }
    enumeratorDescriptions_[enumerator] = description;
}

vector<string> MonitorDescription::getEnumerators() const {
    if(dataType_ != DATATYPE_ENUMERATION) {
        string emsg = "Enumerators are only permitted for montitor points of ";
        emsg += "DATATYPE_ENUMERATION. This point is of type " 
            + carma::dbms::toString(dataType_);
        throw CARMA_ERROR(emsg);
    }
    return enumeratorNames_;
}

map<string,string> MonitorDescription::getEnumeratorDescriptions() const {
    if(dataType_ != DATATYPE_ENUMERATION) {
        string emsg = "Enumerators are only permitted for montitor points of ";
        emsg += "DATATYPE_ENUMERATION. This point is of type " 
            + carma::dbms::toString(dataType_);
        throw CARMA_ERROR(emsg);
    }
    return enumeratorDescriptions_;
}


void MonitorDescription::setThresholdValue
          (const ThresholdValueEnum& thresholdType, const double& value) {
    switch(thresholdType) {
    case THRESHOLD_LOW_ERROR_VALUE:
        errLo_ = value;
        isDefaultErrLo_ = false;
        break;
    case THRESHOLD_LOW_WARN_VALUE:
        warnLo_ = value;
        isDefaultWarnLo_ = false;
        break;
    case THRESHOLD_HIGH_WARN_VALUE:
        warnHi_ = value;
        isDefaultWarnHi_ = false;
        break;
    case THRESHOLD_HIGH_ERROR_VALUE:
        errHi_ = value;
        isDefaultErrHi_ = false;
        break;
    default:
        ostringstream emsg;
        emsg << "Unhandled ThresholdValueEnum " << thresholdType;
        throw CARMA_ERROR(emsg.str());
    }
}

double MonitorDescription::getThresholdValue
    (const ThresholdValueEnum& thresholdType) const {
    if(isDefaultThresholdValue(thresholdType)) {
        return getThresholdDefaultValue(thresholdType);
    } else {
        switch(thresholdType) {
        case THRESHOLD_LOW_ERROR_VALUE:
            return errLo_;
            break;
        case THRESHOLD_LOW_WARN_VALUE:
            return warnLo_;
            break;
        case THRESHOLD_HIGH_WARN_VALUE:
            return warnHi_;
            break;
        case THRESHOLD_HIGH_ERROR_VALUE:
            return errHi_;
            break;
        default:
            ostringstream emsg;
            emsg << "Unhandled ThresholdValueEnum " << thresholdType;
            throw CARMA_ERROR(emsg.str());
        }
    }
}

bool MonitorDescription::isDefaultThresholdValue
    (const ThresholdValueEnum& thresholdType) const {
    switch(thresholdType) {
    case THRESHOLD_LOW_ERROR_VALUE:
        return isDefaultErrLo_;
        break;
    case THRESHOLD_LOW_WARN_VALUE:
        return isDefaultWarnLo_;
        break;
    case THRESHOLD_HIGH_WARN_VALUE:
        return isDefaultWarnHi_;
        break;
    case THRESHOLD_HIGH_ERROR_VALUE:
        return isDefaultErrHi_;
        break;
    default:
        ostringstream emsg;
        emsg << "Unhandled ThresholdValueEnum " << thresholdType;
        throw CARMA_ERROR(emsg.str());
    }
}
    
double MonitorDescription::getThresholdDefaultValue
    (const ThresholdValueEnum& thresholdType) const {
    switch(dataType_) {
    case DATATYPE_BYTE: {
        return 0;
        break;
    }
    case DATATYPE_SHORT: {
        switch(thresholdType) {
        case THRESHOLD_LOW_ERROR_VALUE:
            return MINSHORT;
            break;
        case THRESHOLD_LOW_WARN_VALUE:
            return MINSHORT + 1;
            break;
        case THRESHOLD_HIGH_WARN_VALUE:
            return MAXSHORT - 1;
            break;
        case THRESHOLD_HIGH_ERROR_VALUE:
            return MAXSHORT;
            break;
        default:
            ostringstream emsg;
            emsg << "Unhandled ThresholdValueEnum " << thresholdType;
            throw CARMA_ERROR(emsg.str());
        }
    }
    case DATATYPE_INTEGER: {
        switch(thresholdType) {
        case THRESHOLD_LOW_ERROR_VALUE:
            return MININT;
            break;
        case THRESHOLD_LOW_WARN_VALUE:
            return MININT + 1;
            break;
        case THRESHOLD_HIGH_WARN_VALUE:
            return MAXINT - 1;
            break;
        case THRESHOLD_HIGH_ERROR_VALUE:
            return MAXINT;
            break;
        default:
            ostringstream emsg;
            emsg << "Unhandled ThresholdValueEnum " << thresholdType;
            throw CARMA_ERROR(emsg.str());
        }
    }
    // FIXME do we really need this case?
    case DATATYPE_BOOLEAN: {
        switch(thresholdType) {
        case THRESHOLD_LOW_ERROR_VALUE:
            return 0;
            break;
        case THRESHOLD_LOW_WARN_VALUE:
            return 0;
            break;
        case THRESHOLD_HIGH_WARN_VALUE:
            return 1;
            break;
        case THRESHOLD_HIGH_ERROR_VALUE:
            return 1;
            break;
        default:
            ostringstream emsg;
            emsg << "Unhandled ThresholdValueEnum " << thresholdType;
            throw CARMA_ERROR(emsg.str());
        }
    }
    case DATATYPE_FLOAT: {
        switch(thresholdType) {
        case THRESHOLD_LOW_ERROR_VALUE:
            return (-1.0)*MAXFLOAT;
            break;
        case THRESHOLD_LOW_WARN_VALUE:
            return (-0.99)*MAXFLOAT;
            break;
        case THRESHOLD_HIGH_WARN_VALUE:
            return 0.99*MAXFLOAT;
            break;
        case THRESHOLD_HIGH_ERROR_VALUE:
            return MAXFLOAT;
            break;
        default:
            ostringstream emsg;
            emsg << "Unhandled ThresholdValueEnum " << thresholdType;
            throw CARMA_ERROR(emsg.str());
        }
    }
    case DATATYPE_DOUBLE: {
        switch(thresholdType) {
        case THRESHOLD_LOW_ERROR_VALUE:
            return (-1.0)*MAXDOUBLE;
            break;
        case THRESHOLD_LOW_WARN_VALUE:
            return (-0.99)*MAXDOUBLE;
            break;
        case THRESHOLD_HIGH_WARN_VALUE:
            return 0.99*MAXDOUBLE;
            break;
        case THRESHOLD_HIGH_ERROR_VALUE:
            return MAXDOUBLE;
            break;
        default:
            ostringstream emsg;
            emsg << "Unhandled ThresholdValueEnum " << thresholdType;
            throw CARMA_ERROR(emsg.str());
        }
    }
    case DATATYPE_COMPLEX: {
        // FIXME for COMPLEX types,the values represent the amplitude 
        // thresholds. Need to check with Amar/Steve if this is OK.
        switch(thresholdType) {
        case THRESHOLD_LOW_ERROR_VALUE: {
            return 0;
            break;
        }
        case THRESHOLD_LOW_WARN_VALUE: {
            return MINFLOAT;
            break;
        }
        case THRESHOLD_HIGH_WARN_VALUE: {
            return 0.99*MAXFLOAT;
            break;
        }
        case THRESHOLD_HIGH_ERROR_VALUE: {
            return MAXFLOAT;
            break;
        }
        default: {
            ostringstream emsg;
            emsg << "Unhandled ThresholdValueEnum " << thresholdType;
            throw CARMA_ERROR(emsg.str());
        }
        }
    }
    case DATATYPE_STRING: {
        // thresholding makes no sense in this case, possibly should
        // throw exception
        return 0;
        break;
    }
    case DATATYPE_SERIAL_NUMBER: {
        // thresholding makes no sense in this case, possibly should
        // throw exception
        return 0;
        break;
    }
    case DATATYPE_CHAR: {
        // thresholding makes no sense in this case, possibly should
        // throw exception
        return 0;
        break;
    }
    case DATATYPE_ENUMERATION: {
        // thresholding makes no sense in this case, possibly should
        // throw exception
        return 0;
        break;
    }
    case DATATYPE_ABSTIME: {
        // thresholding makes no sense in this case, possibly should
        // throw exception
        return 0;
        break;
    }
    default: {
        ostringstream emsg;
        emsg << "Unhandled monitor description data type " << dataType_;
        throw CARMA_ERROR(emsg.str());
    }
    }
}
