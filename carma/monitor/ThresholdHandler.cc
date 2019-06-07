/**
 * @file
 * $Id: ThresholdHandler.cc,v 1.6 2011/01/03 18:48:35 iws Exp $
 *
 * @author Chul Gwon
 *
 * ThresholdHandler contains methods to deal with setting thresholds
 * via mpml file.  The actual setting of thresholds is done by passing
 * a pointer to a carma::monitor::MonitorSystemWithThreshold object,
 * but the ThresholdHandler parses the mpml and makes the necessary calls.
 *
 */

#include <xercesc/sax/AttributeList.hpp>
#include "ThresholdHandler.h"
#include "carma/monitor/MonitorSystemWithThreshold.h"
#include "carma/monitor/MonitorPointThreshold.h"
#include <iostream>
#include <sstream>
#include <stdlib.h>
#include <map>

using namespace ::std;
using namespace carma::monitor;


namespace {
  const char * const kErrHiString = "errHi";
  const char * const kErrLoString = "errLo";
  const char * const kWarnHiString = "warnHi";
  const char * const kWarnLoString = "warnLo";

  map<const char*, carma::monitor::ThresholdValueEnum> gThresholdValues;

  typedef enum {
    SUBSYSTEM, CONTAINER, POINT, 
    WARNLO, WARNHI, ERRLO, ERRHI, 
    ENDELEMENT
  } ElementTypeEnum;

  // BULLSHIT_TWC 9 Sept 2005 - Having this is as a global is not thread happy
  ElementTypeEnum gCurrentElement; // type of element that we're currently parsing
}


ThresholdHandler::ThresholdHandler(carma::monitor::MonitorSystemWithThreshold *system) {
  system_ = system;

  // create a mapping between the xml names and the carma::monitor::ThresholdValueEnums
  gThresholdValues[kErrHiString]  = carma::monitor::THRESHOLD_HIGH_ERROR_VALUE;
  gThresholdValues[kErrLoString]  = carma::monitor::THRESHOLD_LOW_ERROR_VALUE;
  gThresholdValues[kWarnHiString] = carma::monitor::THRESHOLD_HIGH_WARN_VALUE;
  gThresholdValues[kWarnLoString] = carma::monitor::THRESHOLD_LOW_WARN_VALUE;
}

ThresholdHandler::~ThresholdHandler() {
  //XMLString::release(&gSubsystemName);
  //XMLString::release(&gSubsystemCount);
}

void ThresholdHandler::setThresholds(const ElementInfoStruct &subsystemInfo,
                                     const vector<ElementInfoStruct> &containerInfo,
                                     const PointInfoStruct &mpInfo,
                                     const char *thresholdName,
                                     const char *value) {
  ostringstream os;

  os << subsystemInfo.name;
  int subsystemCount = static_cast<int>(atoi(subsystemInfo.count));
  if (subsystemCount > 1)  os << "*"; // add this if count != 1
  os << ".";

  int containerCount = 0;
  vector<ElementInfoStruct>::const_iterator containerInfoEnd = containerInfo.end();
  for (vector<ElementInfoStruct>::const_iterator i = containerInfo.begin(); 
       i < containerInfoEnd;
       i++){
    os << i->name;
    containerCount = static_cast<int>(atoi(i->count));
    if (containerCount > 1) os << "*";
    os << ".";
  }

  os << mpInfo.name;
  int mpCount = static_cast<int>(atoi(mpInfo.count));
  if (mpCount > 1) os << "*";

  setThreshold(os.str().c_str(), mpInfo.type, thresholdName, value);
}

void ThresholdHandler::setThreshold(const char *canonicalName,
                                    const char *mpType,
                                    const char *thresholdName,
                                    const char *value) {

  // the xml data types can be looked up in $CARMA_SRC/conf/mpml2cpp.xsl
  if (XMLString::equals(mpType, "byte")){
    system_->setThresholdValues<char>(canonicalName, gThresholdValues[thresholdName], value[0]);
  } else if (XMLString::equals(mpType, "char")) {
    system_->setThresholdValues<char>(canonicalName, gThresholdValues[thresholdName], value[0]);
  } else if (XMLString::equals(mpType, "float")) {
    system_->setThresholdValues<float>(canonicalName, gThresholdValues[thresholdName], static_cast< float >(atof(value)));
  } else if (XMLString::equals(mpType, "double")) {
    system_->setThresholdValues<double>(canonicalName, gThresholdValues[thresholdName], static_cast< double >(atof(value)));
  } else if (XMLString::equals(mpType, "int")) {
    system_->setThresholdValues<long>(canonicalName, gThresholdValues[thresholdName], static_cast< long >(atoi(value)));
  } else if (XMLString::equals(mpType, "short")) {
    system_->setThresholdValues<short>(canonicalName, gThresholdValues[thresholdName], static_cast< short >(atoi(value)));
  } else if (XMLString::equals(mpType, "bool")) {
    //system_->setThresholdValues<bool>(canonicalName, gThresholdValues[thresholdName], (bool)atoi(value));
  } else if (XMLString::equals(mpType, "enum")) {
    //    system_->setThresholdValues<enum>(canonicalName, gThresholdValues[thresholdName], (enum)value);
  } else if (XMLString::equals(mpType, "serialNo")) {
    system_->setThresholdValues<long>(canonicalName, gThresholdValues[thresholdName], static_cast< long >(atoi(value)));
  } else if (XMLString::equals(mpType, "string")) {
    system_->setThresholdValues<string>(canonicalName, gThresholdValues[thresholdName], string(value));
  }
}


void 
ThresholdHandler::characters( const XMLCh * const chars, 
                              const unsigned int  length ) {
    const char * thresholdName = 0;
    
    /* gCurrentElement value set in startElement */
    switch ( gCurrentElement ) {
        case WARNLO:  thresholdName = kWarnLoString;  break;
        case WARNHI:  thresholdName = kWarnHiString;  break;
        case ERRLO:   thresholdName = kErrLoString;   break;
        case ERRHI:   thresholdName = kErrHiString;   break;
        default:      thresholdName = 0;              break;
    }

    if ( thresholdName != 0 ) {
        char * message = XMLString::transcode( chars );

        setThresholds( subsystemInfo_,
                       containerInfo_,
                       pointInfo_,
                       thresholdName,
                       message );

        // BULLSHIT_TWC - 9 Sept 2005 - This is not exception safe
        XMLString::release( &message );
    }
}

void 
ThresholdHandler::startElement(const XMLCh *const elementName, 
                         AttributeList &attributes) {

  // obtain name of element
  char *message = XMLString::transcode(elementName);

  if (XMLString::equals(message, "Subsystem")) {
    processSubsystem(attributes);
  } else if (XMLString::equals(message, "Container")) {
    processContainer(attributes);
  } else if (XMLString::equals(message, "Device")) {
    processContainer(attributes);
  } else if (XMLString::equals(message, "MonitorPoint")) {
    processPoint(attributes);
  } else if (XMLString::equals(message, "SoftPoint")) {
    processPoint(attributes);
  } else if (XMLString::equals(message, "ControlPoint")) {
    processPoint(attributes);

    /* following elements do not have attributes, so do not need to be parsed */
  } else if (XMLString::equals(message, kWarnLoString)) {
    gCurrentElement = WARNLO;
  } else if (XMLString::equals(message, kWarnHiString)) {
    gCurrentElement = WARNHI;
  } else if (XMLString::equals(message, kErrLoString)) {
    gCurrentElement = ERRLO;
  } else if (XMLString::equals(message, kErrHiString)) {
    gCurrentElement = ERRHI;
  }

  XMLString::release(&message);
}

void 
ThresholdHandler::endElement(const XMLCh *const elementName) {
  // obtain name of element
  char *message = XMLString::transcode(elementName);

  if (XMLString::equals(message, "Subsystem")) {
    gCurrentElement = ENDELEMENT;

    // subsystem is the root element ... should call whichever
    // loadDefinition function at this point ...

  } else if (XMLString::equals(message, "Container")) {
    // remove last name from list of container names
    containerInfo_.pop_back();

    // this check necessary because containers can contain other
    // containers 
    if (containerInfo_.size() == 0) {
      gCurrentElement = SUBSYSTEM;
    } else {
      gCurrentElement = CONTAINER;
    }

  } else if (XMLString::equals(message, "Device")) {
    // remove last name from list of container names
    containerInfo_.pop_back();

    // this check necessary because containers can contain other
    // containers 
    if (containerInfo_.size() == 0) {
      gCurrentElement = SUBSYSTEM;
    } else {
      gCurrentElement = CONTAINER;
    }

  /* points are elements of a container */
  } else if (XMLString::equals(message, "MonitorPoint")) {
    gCurrentElement = CONTAINER;
  } else if (XMLString::equals(message, "ControlPoint")) {
    gCurrentElement = CONTAINER;
  } else if (XMLString::equals(message, "SoftPoint")) {
    gCurrentElement = CONTAINER;

  /* the following are all elements to a monitor point*/
  } else if (XMLString::equals(message, "warnLo")) {
    gCurrentElement = POINT;
  } else if (XMLString::equals(message, "warnHi")) {
    gCurrentElement = POINT;
  } else if (XMLString::equals(message, "errLo")) {
    gCurrentElement = POINT;
  } else if (XMLString::equals(message, "errHi")) {
    gCurrentElement = POINT;
  }

  XMLString::release(&message);
}

void ThresholdHandler::processSubsystem(AttributeList &attributes) {
  // attribute information
  unsigned int numAttributes = attributes.getLength();
  char *attributeName, *attributeValue;
  
  subsystemInfo_.count = XMLString::replicate("1"); // initialize the count

  // store information from attributes into the global subsystemInfo_ variable
  for (unsigned int i = 0; i < numAttributes; i++) {
    attributeName = XMLString::transcode(attributes.getName(i));
    attributeValue = XMLString::transcode(attributes.getValue(i));
    if (XMLString::equals(attributeName, "name")) {
      subsystemInfo_.name = XMLString::replicate(attributeValue);
    } else if (XMLString::equals(attributeName, "count")) {
      subsystemInfo_.count = XMLString::replicate(attributeValue);
    }
    XMLString::release(&attributeName);
    XMLString::release(&attributeValue);
  }
}

void ThresholdHandler::processContainer(AttributeList &attributes) {
  // attribute information
  unsigned int numAttributes = attributes.getLength();
  char *attributeName, *attributeValue;

  ElementInfoStruct containerInfo;
  containerInfo.count = XMLString::replicate("1");

  // store information from attributes into the global subsystemInfo_ variable
  for (unsigned int i = 0; i < numAttributes; i++) {
    attributeName = XMLString::transcode(attributes.getName(i));
    attributeValue = XMLString::transcode(attributes.getValue(i));
    if (XMLString::equals(attributeName, "name")) {
      containerInfo.name = XMLString::replicate(attributeValue);
    } else if (XMLString::equals(attributeName, "count")) {
      containerInfo.count = XMLString::replicate(attributeValue);
    }
  }

  // add container information to global vector of container info
  containerInfo_.push_back(containerInfo);  

  XMLString::release(&attributeName);
  XMLString::release(&attributeValue);
}

void ThresholdHandler::processPoint(AttributeList &attributes) {
  // attribute information
  unsigned int numAttributes = attributes.getLength();
  char *attributeName, *attributeValue;

  pointInfo_.count = XMLString::replicate("1"); // initialize the count

  // obtain name and count info for monitor point
  for (unsigned int i = 0; i < numAttributes; i++) {
    attributeName = XMLString::transcode(attributes.getName(i));
    attributeValue = XMLString::transcode(attributes.getValue(i));
    if (XMLString::equals(attributeName, "name")) {
      pointInfo_.name = XMLString::replicate(attributeValue);
    } else if (XMLString::equals(attributeName, "count")) {
      pointInfo_.count = XMLString::replicate(attributeValue);
    } else if (XMLString::equals(attributeName, "type")) {
      pointInfo_.type = XMLString::replicate(attributeValue);
    }
    XMLString::release(&attributeName);
    XMLString::release(&attributeValue);
  }

}


/*** SAX ErrorHandler methods ***/
void ThresholdHandler::error(const SAXParseException &e) {
  char *systemId = XMLString::transcode(e.getSystemId());
  char *message = XMLString::transcode(e.getMessage());

  cerr << "\nError at file " << systemId
            << ", line " << e.getLineNumber()
            << ", char " << e.getColumnNumber()
            << "\n  Message: " << message 
            << endl;

  XMLString::release(&systemId);
  XMLString::release(&message);
}

void ThresholdHandler::fatalError(const SAXParseException &e) {
  char *systemId = XMLString::transcode(e.getSystemId());
  char *message = XMLString::transcode(e.getMessage());

  cerr << "\nFatal Error at file " << systemId
            << ", line " << e.getLineNumber()
            << ", char " << e.getColumnNumber()
            << "\n  Message: " << message 
            << endl;

  XMLString::release(&systemId);
  XMLString::release(&message);
}

void ThresholdHandler::warning(const SAXParseException &e) {
  char *systemId = XMLString::transcode(e.getSystemId());
  char *message = XMLString::transcode(e.getMessage());

  cerr << "\nWarning at file " << systemId
            << ", line " << e.getLineNumber()
            << ", char " << e.getColumnNumber()
            << "\n  Message: " << message 
            << endl;

  XMLString::release(&systemId);
  XMLString::release(&message);
}
