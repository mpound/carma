/**
 * @version $Id: SaxHandler.cc,v 1.30 2011/12/21 22:56:43 mpound Exp $
 *
 * - implementation of class SaxHandler
 *
 */

#include <xercesc/sax/AttributeList.hpp>
#include <iostream>
#include <sstream>
#include <stdlib.h>
#include "carma/dbms/MPMLException.h"
#include "carma/dbms/SaxHandler.h"
#include "carma/monitor/types.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

using namespace std;
using namespace carma::dbms;

void SaxHandler::characters(const XMLCh *const chars, const unsigned length) {  
    if(elementStack_.size() > 1) {
        unsigned parentIndex = elementStack_.size() - 2;
        string parent = elementStack_[parentIndex];


        if(parent == "SensePoint" || parent == "SoftPoint" 
           || parent == "ControlPoint" || parent == "MonitorPoint"
           || parent == "enum") {
            char *msg = XMLString::transcode(chars);
            string message(msg);
            vector<char> badChars;
            badChars.push_back('\n');
            message = carma::util::StringUtils::erase(message,badChars);
            badChars.push_back(' ');
            message = carma::util::StringUtils::trim(message,"\n ");
            message = carma::util::StringUtils::collapse(message,badChars);
            string currentElement = elementStack_.back();
            vector<MonitorDescription>::iterator md = 
                currentMonitorDescriptions_.begin();
            vector<MonitorDescription>::const_iterator mdEnd = 
                currentMonitorDescriptions_.end();
            while(md != mdEnd) {
                if(currentElement == "shortName") {
                    md->setShortName(message);
                    /*
                    unsigned maxLength 
                        = MonitorConfigurationDatabase::shortNameMaxLength();
                    if(message.length() > maxLength) {
                        string newShortName 
                            = message.substr(0,maxLength);
                        ostringstream emsg;
                        emsg << "MySQL limits column names to <= 64 "
                             << "characters. The short name '" << message 
                             << "' for monitor point " << md->getName()
                             << " will be shortened to " << newShortName;
                        carma::util::Program::getLogger() 
                            << log4cpp::Priority::WARN << emsg.str();
                        md->setShortName(newShortName);
                    } else {
                        md->setShortName(message);
                    }
                    */
                } else if(currentElement == "longName") {
                    md->setLongName(message);
                } else if(currentElement == "units") {
                    md->setUnits(message);
                } else if(currentElement == "warnLo") {
                    md->setThresholdValue
                        (carma::monitor::THRESHOLD_LOW_WARN_VALUE,
                         atof(message.c_str()));
                } else if(currentElement == "warnHi") {
                    md->setThresholdValue
                        (carma::monitor::THRESHOLD_HIGH_WARN_VALUE,
                         atof(message.c_str()));
                } else if(currentElement == "errLo") {
                    md->setThresholdValue
                        (carma::monitor::THRESHOLD_LOW_ERROR_VALUE,
                                 atof(message.c_str()));
                } else if(currentElement == "errHi") {
                    md->setThresholdValue
                        (carma::monitor::THRESHOLD_HIGH_ERROR_VALUE,
                                 atof(message.c_str()));
                } else if(currentElement == "description") {
                    if(parent == "enum") {
                        md->setEnumeratorDescription(currentEnumerator_,
                                                     message);
                    } else {
                        md->setDescription(message);
                    }
                }
                md++;
            }
            XMLString::release(&msg);  
        } 
    }
}

void SaxHandler::startElement(const XMLCh *const elementName, 
                              AttributeList &attributes) {
    // obtain name of element
    char *message = XMLString::transcode(elementName);
    string msg(message);
    if(elementStack_.size() == 0 && msg != "Subsystem") {
        string emsg = "The root element must be \"Subsystem\", not \"" + msg
            + "\"";
        throw CARMA_EXCEPTION(MPMLException,emsg);
    }
    elementStack_.push_back(msg);
    if(msg.substr(0,6) == "Common") {
        string emsg = "<Common*> element found in file which I cannot ";
        emsg += "process. Run mpmlgen with the -L switch to create an mpml ";
        emsg += "file which has had the <Common*> elements merged into the ";
        emsg += "main file";
        throw CARMA_EXCEPTION(MPMLException,emsg);
    }
    if (msg == "Subsystem" || msg == "Container" || msg == "Device") {
        processContainer(msg,attributes);
    } else if (msg == "SensePoint") {
        processPoint(MPTYPE_SENSE, attributes);
    } else if (msg == "SoftPoint" || msg == "MonitorPoint") {
        processPoint(MPTYPE_SOFT, attributes);
    } else if (msg == "ControlPoint") {
        processPoint(MPTYPE_CONTROL, attributes);
    } else if (msg == "enum") {
        processEnum(attributes);
    }
    XMLString::release(&message);
}

void SaxHandler::endElement(const XMLCh *const elementName) {
    // obtain name of element
    char *message = XMLString::transcode(elementName);
    string msg(message);
    elementStack_.pop_back();
    if (msg == "Container" || msg == "Device" || msg == "Subsystem") {
        char *containerName = containerNames_.back();
        // remove last name from list of container names
        containerNames_.pop_back();
        containerCount_.pop_back();
        containerTypes_.pop_back();
        if(msg == "Device") { 
            devices_.pop_back(); 
        }
        XMLString::release(&containerName);
    } else if (msg == "SensePoint" || msg == "SoftPoint" 
               || msg == "ControlPoint" || msg == "MonitorPoint") {
        if(currentMonitorDescriptions_[0].getDataType() 
           == DATATYPE_ENUMERATION 
           && currentMonitorDescriptions_[0].getEnumerators().size() == 0) {
            ostringstream emsg;
            emsg <<"Point " << currentMonitorDescriptions_[0].getName() 
                 << " has been specified to be of type ENUMERATION but "
                 << "contains no enumerators";
            throw CARMA_EXCEPTION(MPMLException, emsg.str());
        }
        for(unsigned i = 0; i < currentMonitorDescriptions_.size(); i++) {
            monitorDescriptions_.push_back(currentMonitorDescriptions_[i]);
        }
        currentMonitorDescriptions_.clear();
    }
    XMLString::release(&message);
}

void SaxHandler::processContainer(const std::string& containerType, 
                                  AttributeList &attributes) {
    if(containerType == "Subsystem") {
        containerTypes_.push_back(C_SUBSYSTEM);
    } else if (containerType == "Container") {
        containerTypes_.push_back(C_CONTAINER);
    } else if (containerType == "Device") {
        containerTypes_.push_back(C_DEVICE);
    } else {
        string emsg = "Unknown container type " + containerType ;
        throw CARMA_EXCEPTION(MPMLException, emsg);
    }        
    // attribute information
    unsigned int numAttributes = attributes.getLength();
    char *attributeName, *attributeValue;
    unsigned count = 1;
    char *name;
    bool isMaxPointsSet = false;
    bool isMaxSamplesSet = false;
    bool deviceIDFound = false;
    string deviceID;
    for (unsigned int i = 0; i < numAttributes; i++) {
        attributeName = XMLString::transcode(attributes.getName(i));
        attributeValue = XMLString::transcode(attributes.getValue(i));
        if (XMLString::equals(attributeName, "name")) {
            name = XMLString::replicate(attributeValue);  
            containerNames_.push_back(name);  
        } else if (XMLString::equals(attributeName, "count")) {
            count = atoi(XMLString::replicate(attributeValue));
        } else if (XMLString::equals(attributeName, "location") 
                   && containerType == "Subsystem") {
            location_ = string(XMLString::replicate(attributeValue));
        } else if (XMLString::equals(attributeName, "id") 
                   && containerType == "Device") {
            deviceID = string(XMLString::replicate(attributeValue));
            deviceIDFound = true;
        } else if (containerType == "Subsystem" 
                   && XMLString::equals(attributeName, "maxpoints")) {
            aggregateSubsystemMaxPoints_ 
                = atoi(XMLString::replicate(attributeValue));
            isMaxPointsSet = true;
        } else if (containerType == "Subsystem" 
                   && XMLString::equals(attributeName, "maxsamples")) {
            aggregateSubsystemMaxSamples_ 
                = atoi(XMLString::replicate(attributeValue));
            isMaxSamplesSet = true;
        }
        XMLString::release(&attributeName);
        XMLString::release(&attributeValue);
    }
    if (containerType == "Subsystem") {
        aggregateSubsystemCount_ = count;
        aggregateSubsystemName_  = name;
        if(!isMaxSamplesSet) {
            string emsg = "maxsamples attribute not specified";
            throw CARMA_EXCEPTION(MPMLException,emsg);
        }
        if(!isMaxPointsSet) {
            string emsg = "maxpoints attribute not specified";
            throw CARMA_EXCEPTION(MPMLException,emsg);
        }
    }
    if (containerType == "Device" && !deviceIDFound && devices_.size()==0 ) {
        string emsg = "Top level device " + string(name) 
            + " has no id attribute";
        throw CARMA_EXCEPTION(MPMLException,emsg);
    }
    if(containerType == "Device") {
        Device d;
        d.name = name;
        d.id = (deviceIDFound) ? deviceID : "";
        devices_.push_back(d);
    }
    containerCount_.push_back(count);
}


namespace {
    
char gSerialNoName[] = "serialNo";
    
}


void SaxHandler::processPoint(MonitorPointType type, AttributeList &attributes)
{
    // attribute information
    unsigned numAttributes = attributes.getLength();
    char *attributeName, *attributeValue, *name;
    unsigned count = 1;
    vector<char *> names = containerNames_;
    vector<unsigned> counts = containerCount_;
    string dataTypeStr = "";
    MonitorPointDataType dataType;
    string persistentStr;
    bool persistent = true;
    unsigned updateInterval = 1;
    unsigned i = 0;
    ostringstream emsg;
    bool spectrum = false;
    // loop over attributes
    for (i = 0; i < numAttributes; i++) {
        attributeName = XMLString::transcode(attributes.getName(i));
        attributeValue = XMLString::transcode(attributes.getValue(i));
        if (XMLString::equals(attributeName, "count")) {
            count = atoi(attributeValue);
        } else if(XMLString::equals(attributeName, "name")) {
            name = attributeValue;
        } else if(XMLString::equals(attributeName, "type")) {
            dataTypeStr = carma::util::StringUtils
                ::lowASCIIAlphaNumericToLower(string(attributeValue));
            if(dataTypeStr == "byte") {
                dataType = DATATYPE_BYTE;
            } else if (dataTypeStr == "short") {
                dataType = DATATYPE_SHORT;
            } else if (dataTypeStr == "int") {
                dataType = DATATYPE_INTEGER;
            } else if (dataTypeStr == "bool") {
                dataType = DATATYPE_BOOLEAN;
            } else if (dataTypeStr == "float") {
                dataType = DATATYPE_FLOAT;
            } else if (dataTypeStr == "double") {
                dataType = DATATYPE_DOUBLE;
            } else if (dataTypeStr == "complex") {
                dataType = DATATYPE_COMPLEX;
            } else if (dataTypeStr == "string") {
                dataType = DATATYPE_STRING;
            } else if (dataTypeStr == "serialno") {
                dataType = DATATYPE_SERIAL_NUMBER;
            } else if (dataTypeStr == "char") {
                dataType = DATATYPE_CHAR;
            } else if (dataTypeStr == "enum") {
                dataType = DATATYPE_ENUMERATION;
            } else if (dataTypeStr == "abstime") {
                dataType = DATATYPE_ABSTIME;
            } else {
                emsg << "Unknown data type " << dataTypeStr;
                throw CARMA_EXCEPTION(MPMLException, emsg.str());
            }
        } else if(XMLString::equals(attributeName, "persistent")) {
            persistentStr = carma::util::StringUtils
                ::lowASCIIAlphaNumericToLower(string(attributeValue));
            if(persistentStr == "true") {
                persistent = true;
            } else if(persistentStr == "false") {
                persistent = false;
            } else {
                emsg << "Unknown persistent value " << persistentStr;
                throw CARMA_EXCEPTION(MPMLException, emsg.str());
            }
        } else if(XMLString::equals(attributeName, "update")) {
            updateInterval = atoi(attributeValue);
        } else if(XMLString::equals(attributeName, "spectrum")) {
            string spectrumStr = carma::util::StringUtils
                ::lowASCIIAlphaNumericToLower(string(attributeValue));
            if(spectrumStr == "true") {
                spectrum = true;
            } else if(spectrumStr == "false") {
                spectrum = false;
            } else {
                emsg << "Unknown spectrum attribute value " << spectrumStr;
                throw CARMA_EXCEPTION(MPMLException, emsg.str());
            }
        }
    }
    if(dataTypeStr == "") {
        emsg << "No data type specified for point " << name;
        throw CARMA_EXCEPTION(MPMLException, emsg.str());
    }
    // need to set a specific name for serialNo points...
    if(dataType == DATATYPE_SERIAL_NUMBER) 
        name = gSerialNoName;
    names.push_back(name);
    counts.push_back(count);
    vector<string> combos = getCombinations(names, counts);
    for(i = 0; i < combos.size(); i++) {
        MonitorDescription md(combos[i],type,dataType,persistent,
                              updateInterval,spectrum);
        //if(type == MPTYPE_SENSE) {
        if(devices_.size() > 0) {
            md.setLocation(getLocation(combos[i]));
            md.setDevice(getDevice(combos[i]));
        }
        currentMonitorDescriptions_.push_back(md);
    }
}

void SaxHandler::processEnum(const AttributeList& attributes) {
    unsigned numAttributes = attributes.getLength();
    char *attributeName, *attributeValue;
    vector<char *> names = containerNames_;
    vector<unsigned> counts = containerCount_;
    string name = "";
    for (unsigned int i = 0; i < numAttributes; i++) {
        attributeName = XMLString::transcode(attributes.getName(i));
        attributeValue = XMLString::transcode(attributes.getValue(i));
        if (XMLString::equals(attributeName, "name")) {
            if(name.length() > 255) {
                ostringstream emsg;
                emsg << "Enum names are not permitted to be longer than 255 "
                     << "characters. Enum name " << attributeValue << " of "
                     << "monitor point " 
                     << currentMonitorDescriptions_.front().getName()
                     << " is in violation of this rule";
                throw CARMA_EXCEPTION(MPMLException, emsg.str());
            }
            name = string(attributeValue);
        }
    }
    ostringstream emsg;
    if(name == "") {
        emsg << "No name specified for enum";
        throw CARMA_EXCEPTION(MPMLException, emsg.str());
    }
    currentEnumerator_ = name;
    for(unsigned int i = 0; i < currentMonitorDescriptions_.size(); i++) {
        currentMonitorDescriptions_[i].addEnumerator(name);
    }
 }


string SaxHandler::getContainerName() {
    string containerName = "";
    for(unsigned int i = 0; i < containerNames_.size(); i++) {
        containerName += string(containerNames_[i]);
        if(i < containerNames_.size() - 1) {
            containerName += ".";
        }
    }
    return containerName;
}

vector<string> SaxHandler::getCombinations(const vector<char *>& names,
                                           const vector<unsigned>& counts,
                                           const string& prepend) const {
    vector<unsigned> tCounts;
    vector<char *> tNames;
    vector<string> combos, tCombos;
    if(names.size() > 1) {
        for(unsigned int i = 1; i < names.size(); i++) {
            tNames.push_back(names[i]);
            tCounts.push_back(counts[i]);
        }
    }
    ostringstream ss;
    for(unsigned int j = 1; j <= counts[0]; j++) {
        ss.str("");
        ss << prepend << names[0];
        if(counts[0] > 1) {
           ss << j;
        }
        if(counts.size() > 1) {
            ss << "." ;
            tCombos = getCombinations(tNames,tCounts,ss.str());
            for(unsigned k = 0; k < tCombos.size(); k++) {
                combos.push_back(tCombos[k]);
            }
        } else {
            combos.push_back(ss.str());
        }
    }
    return combos;
}


/*** SAX ErrorHandler methods ***/
void SaxHandler::error(const SAXParseException &e) {
  char *systemId = XMLString::transcode(e.getSystemId());
  char *message = XMLString::transcode(e.getMessage());

  std::cerr << "\nError at file " << systemId
	    << ", line " << e.getLineNumber()
	    << ", char " << e.getColumnNumber()
	    << "\n  Message: " << message 
	    << std::endl;

  XMLString::release(&systemId);
  XMLString::release(&message);
}

void SaxHandler::fatalError(const SAXParseException &e) {
  char *systemId = XMLString::transcode(e.getSystemId());
  char *message = XMLString::transcode(e.getMessage());

  std::cerr << "\nFatal Error at file " << systemId
	    << ", line " << e.getLineNumber()
	    << ", char " << e.getColumnNumber()
	    << "\n  Message: " << message 
	    << std::endl;

  XMLString::release(&systemId);
  XMLString::release(&message);
}

void SaxHandler::warning(const SAXParseException &e) {
  char *systemId = XMLString::transcode(e.getSystemId());
  char *message = XMLString::transcode(e.getMessage());

  std::cerr << "\nWarning at file " << systemId
	    << ", line " << e.getLineNumber()
	    << ", char " << e.getColumnNumber()
	    << "\n  Message: " << message 
	    << std::endl;

  XMLString::release(&systemId);
  XMLString::release(&message);
}


string SaxHandler::getLocation(const string& cname) const {
    int dotpos = cname.find(".");
    string subsysName = cname.substr(0,dotpos);
    if (containerNames_[0] == subsysName) {
        return location_;
    } 
    int baseSize = string(containerNames_[0]).size();
    int numlen = dotpos - baseSize;
    string number = cname.substr(baseSize,numlen);
    return location_ + number;
}

string SaxHandler::getDevice(const string& cname) const {
    if (devices_.size() == 0) {
        string emsg = "mpml exception, device specification incorrect for "
            + cname;
        throw CARMA_EXCEPTION(MPMLException,emsg);
    }
    // the rightmost node is not a container, but a monitor point, so ignore
    // it
    int dotpos = cname.rfind(".");
    string tmpname = cname.substr(0,dotpos);
    string deviceName;
    // work from the most immediate ancestor back
    //for(int i=0; i < containerTypes_.size(); i++) {
    int deviceIndex = devices_.size()-1;
    for(int i=(containerTypes_.size()-1); i >=0; i--) {
        dotpos = tmpname.rfind(".");
        if(containerTypes_[i] == C_DEVICE) {
            if(devices_[deviceIndex].id == "") {
                deviceIndex--;
                tmpname = tmpname.substr(0,dotpos);
            } else {
                // device with valid id found, this is what we're looking for
                // we return from this block only, and this block must return
                deviceName = tmpname.substr(dotpos+1);
                if (containerNames_[i] == deviceName) {
                    // @count=1, so don't need to append a number
                    return devices_[deviceIndex].id;
                } else {
                    // @count > 1, must append a number to the id
                    int baseSize = string(containerNames_[i]).size();
                    //int numlen = dotpos - baseSize;
                    int numlen = tmpname.size() - 1 - dotpos - baseSize;
                    assert(numlen > 0);
                    string number = tmpname.substr(tmpname.size()-numlen,
                                                   numlen);
                    assert(atoi(number.c_str()) > 0);
                    return devices_[deviceIndex].id + number;
                }
            }
        } else {
            tmpname = tmpname.substr(0,dotpos);
        }
    }
    string emsg = "Sense point " + cname + " is not a child of a <Device> "
        + "element";
    throw CARMA_EXCEPTION(MPMLException, emsg);
}

