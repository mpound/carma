/** 
 * @file
 *
 * $Id: ThresholdFileWriter.cc,v 1.6 2011/01/03 18:48:35 iws Exp $
 *
 */

#include "carma/monitor/ThresholdFileWriter.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/MonitorSystemWithThreshold.h"
#include "carma/monitor/MonitorPoint.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/util/Trace.h"
#include "carma/util/StringUtils.h"

#include <iostream>

namespace {
  typedef struct {
    std::string type;  // name of the tag
    std::string value; // value for the "name" attribute of the tag
  } TagInfo;

  // list of hierarchy of tags that are currently open
  std::vector<TagInfo> gTagInfo;

  std::vector<std::string> gCurrentName;

  int gIndent = 0; // # of \t's to indent tag
  std::string gIndentString("   ");

  // global threshold system
  carma::monitor::MonitorSystemWithThreshold *gMswt;

  void printAttributes(const std::pair<std::string, std::string> &attribute) {
    std::cout << " " << attribute.first << "=\"" << attribute.second << "\"";
  }

  void indentStartTag() {
    for (int i = 0; i < gIndent; i++) std::cout << gIndentString;
    gIndent++;
  }

  // needIndent added in case the end tag is on the same line as the
  // start tag
  void indentEndTag(bool needIndent) {
    gIndent--;
    if ( needIndent == true )
      for (int i = 0; i < gIndent; i++) std::cout << gIndentString;
  }

  void indentEntry() {
    int indent = gIndent + 1;
    for (int i = 0; i < indent; i++) std::cout << gIndentString;
  }

  // method containing switch for printing out threshold using correct
  // data type
  void printThreshold(const carma::monitor::MonitorPointThreshold &threshold,
		      const carma::monitor::MonitorValueType pointType,
		      const carma::monitor::ThresholdValueEnum valueType) {
    switch(pointType) {
    case carma::monitor::MONITOR_VALUE_TYPE_BYTE:
      std::cout << gMswt->getThresholdValue<char>(threshold, valueType);
      break;
    case carma::monitor::MONITOR_VALUE_TYPE_SHORT:
      std::cout << gMswt->getThresholdValue<short>(threshold, valueType);
      break;
    case carma::monitor::MONITOR_VALUE_TYPE_INTEGER:
      std::cout << gMswt->getThresholdValue<long>(threshold, valueType);
      break;
    case carma::monitor::MONITOR_VALUE_TYPE_BOOLEAN:
      std::cout << gMswt->getThresholdValue<bool>(threshold, valueType);
      break;
    case carma::monitor::MONITOR_VALUE_TYPE_FLOAT:
      std::cout << gMswt->getThresholdValue<float>(threshold, valueType);
      break;
    case carma::monitor::MONITOR_VALUE_TYPE_DOUBLE:
      std::cout << gMswt->getThresholdValue<double>(threshold, valueType);
      break;
    case carma::monitor::MONITOR_VALUE_TYPE_COMPLEX:
      std::cout << threshold.getComplexThresholdValue(valueType);
      break;
    case carma::monitor::MONITOR_VALUE_TYPE_STRING:
      std::cout << gMswt->getThresholdValue<std::string>(threshold, valueType);
      break;
    case carma::monitor::MONITOR_VALUE_TYPE_SERIAL_NUMBER:
      std::cout << gMswt->getThresholdValue<long>(threshold, valueType);
      break;
    }
  }
} // end anonymous namespace

using namespace carma::monitor;

ThresholdFileWriter::ThresholdFileWriter(MonitorSystem &system) 
  : system_(system) {
  gMswt = new carma::monitor::MonitorSystemWithThreshold(system);
}

ThresholdFileWriter::~ThresholdFileWriter() {}

void 
ThresholdFileWriter::createXmlFile() {
  std::map<std::string, std::string> attributes;
  MonitorPointIterator mpIter(system_);  

  attributes.clear();

  while (mpIter++) {
    MonitorPoint &mp = mpIter.getMonitorPoint();
    std::string canonicalName = mp.getCanonicalName();
    MonitorPointThreshold &threshold = gMswt->getThreshold(canonicalName);

    if ( threshold.isSet() ) {
      //std::cout << mp.getCanonicalName() << mp.getName() << std::endl;
      gCurrentName.clear();
      gCurrentName = carma::util::StringUtils::tokenize(canonicalName, ".");
      
      int nContainers = gCurrentName.size() - 1; // don't count MP name
      int nTags = 0;
      
      if ( !gTagInfo.empty() ) {
	for (int i = 0; i < nContainers; i++) {
	  nTags = gTagInfo.size();
	  if ( (gCurrentName[i] != gTagInfo[i].value) && (i < nTags) ) {
	    while (nTags > i) {
	      createEndTag();
	      nTags--;
	    }
	    // if it gets here, we've found where the names don't match
	    // and closed all the mpml tags accordingly
	    break;
	  }
	}
      }
      
      // if the tag names are empty, then we have a new subsystem
      if ( gTagInfo.empty() ) {
	createEndTag();
	attributes.insert( ::std::make_pair("name", gCurrentName[0]) );
	createStartTag("Subsystem", attributes);
	nTags = 1; // set this since used for indexing containers
	attributes.clear();
      }
      
      // index last container so we can determine if it's a Device or
      // not (assuming that there won't be Devices that contain other
      // Containers/Devices)
      int lastContainerIndex = nContainers - 1;
      for (int i = nTags; i < lastContainerIndex; i++) {
	attributes.insert( ::std::make_pair("name", gCurrentName[i]) );
	createStartTag("Container", attributes);
	attributes.clear();
      }

      // check to see if the last container is a Device or just a Container
      if (nTags < nContainers) {
          attributes.insert( 
            ::std::make_pair("name", gCurrentName[lastContainerIndex]) );
          createStartTag("Container", attributes);
          attributes.clear();
      }

      // print out monitor point information
      addPoint(mp, threshold);
    }
  }
  while ( !gTagInfo.empty() ) createEndTag();
}

void
ThresholdFileWriter::addPoint(const MonitorPoint &point,
			      const MonitorPointThreshold &threshold) {
			      
  std::map<std::string, std::string> attributes;

  // put name into attributes
  attributes.insert( ::std::make_pair("name", gCurrentName.back()) );

  // put type into attributes
  carma::monitor::MonitorValueType pointType = point.getValuetype();
  switch(pointType) {
  case carma::monitor::MONITOR_VALUE_TYPE_BYTE:
    attributes.insert( ::std::make_pair("type", "byte") );
    break;
  case carma::monitor::MONITOR_VALUE_TYPE_SHORT:
    attributes.insert( ::std::make_pair("type", "short") );
    break;
  case carma::monitor::MONITOR_VALUE_TYPE_INTEGER:
    attributes.insert( ::std::make_pair("type", "int") );
    break;
  case carma::monitor::MONITOR_VALUE_TYPE_BOOLEAN:
    attributes.insert( ::std::make_pair("type", "bool") );
    break;
  case carma::monitor::MONITOR_VALUE_TYPE_FLOAT:
    attributes.insert( ::std::make_pair("type", "float") );
    break;
  case carma::monitor::MONITOR_VALUE_TYPE_DOUBLE:
    attributes.insert( ::std::make_pair("type", "double") );
    break;
  case carma::monitor::MONITOR_VALUE_TYPE_COMPLEX:
    attributes.insert( ::std::make_pair("type", "complex") );
    break;
  case carma::monitor::MONITOR_VALUE_TYPE_STRING:
    attributes.insert( ::std::make_pair("type", "string") );
    break;
  case carma::monitor::MONITOR_VALUE_TYPE_SERIAL_NUMBER:
    attributes.insert( ::std::make_pair("type", "serialNo") );
    break;
  }

  carma::monitor::MonitorPoint::MONITOR_POINT_TYPE mpType = point.getMonitorPointType();
  switch(mpType) {
  case carma::monitor::MonitorPoint::MONITOR:
    createStartTag("MonitorPoint", attributes);
    break;
  case carma::monitor::MonitorPoint::CONTROL:
    createStartTag("ControlPoint", attributes);
    break;
  }
  attributes.clear();

  createStartTag("units", attributes, false); // pass in empty attributes map
  std::cout << point.getUnits();
  createEndTag(false);

  createStartTag("precision", attributes, false); // pass in empty attributes map
  std::cout << point.getPrecision();
  createEndTag(false);

  createStartTag("errHi", attributes, false); // pass in empty attributes map
  printThreshold(threshold, pointType, carma::monitor::THRESHOLD_HIGH_ERROR_VALUE);
  createEndTag(false);
  
  createStartTag("errLo", attributes, false); // pass in empty attributes map
  printThreshold(threshold, pointType, carma::monitor::THRESHOLD_LOW_ERROR_VALUE);
  createEndTag(false);
  
  createStartTag("warnHi", attributes, false); // pass in empty attributes map
  printThreshold(threshold, pointType, carma::monitor::THRESHOLD_HIGH_WARN_VALUE);
  createEndTag(false);
  
  createStartTag("warnLo", attributes, false); // pass in empty attributes map
  printThreshold(threshold, pointType, carma::monitor::THRESHOLD_LOW_WARN_VALUE);
  createEndTag(false);

  createEndTag(); // end tag for MonitorPoint
}

void
ThresholdFileWriter::createStartTag(const std::string &tagName,
				    const std::map<std::string,std::string> &attributes,
				    bool includeEndl) {

  indentStartTag();

  std::cout << "<" << tagName;

  TagInfo tempTagInfo;
  tempTagInfo.type = tagName; // add tag name

  if ( !attributes.empty() ) {
    // print out attributes
    for_each(attributes.begin(),attributes.end(),printAttributes);

    // add "name" of tag if available
    std::map<std::string, std::string>::const_iterator aIterator = attributes.find("name");
    if (aIterator != attributes.end()) {
      tempTagInfo.value = aIterator->second;
    }
  }

  gTagInfo.push_back(tempTagInfo);
  std::cout << ">";

  // don't necessarily want this for tags that begin and terminate on one line
  if (includeEndl) std::cout << std::endl;
}

void
ThresholdFileWriter::createEndTag(bool needIndent) {
  if ( !gTagInfo.empty() ) {
    TagInfo lastTag = gTagInfo.back();
    std::string tagName = lastTag.type;

    indentEndTag(needIndent);

    std::cout << "</" << tagName << ">" << std::endl;

    gTagInfo.pop_back();
  }
}
