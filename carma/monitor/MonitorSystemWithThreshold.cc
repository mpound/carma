/**
 * @file MonitorSystemWithThreshold.cc
 * $Id: MonitorSystemWithThreshold.cc,v 1.10 2006/05/25 23:03:43 tcosta Exp $
 * 
 * @author Chul Gwon
 *
 */

#include "carma/monitor/types.h"
#include "carma/monitor/MonitorSystemWithThreshold.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Trace.h"

using namespace carma::monitor;
using namespace carma::util;
using namespace std;

MonitorSystemWithThreshold::MonitorSystemWithThreshold(MonitorSystem &monitorSystem) :
  SystemThresholdFrameBuffer(monitorSystem),
  monitorSystem_(monitorSystem) {}

MonitorPointThreshold&
MonitorSystemWithThreshold::getThreshold (const string &mpName) {
  MonitorPoint & monitorPoint = monitorSystem_.getMonitorPoint(mpName, true);

  // need to get tag id to get threshold
  tagIDType tagId = monitorPoint.getTagID();

  // put threshold value in vector of thresholds
  CARMA_CPTRACE(Trace::TRACE1, "tagId: " << tagId);

  try {
    MonitorPointThreshold& mpt = SystemThresholdFrame::getThreshold(tagId);
    return mpt;
  } catch (const ThresholdObjectNotFoundException &ex) {
    CARMA_CPTRACE(Trace::TRACE1, ex.getErrorMessage());

    throw;
  }
}

vector<MonitorPointThreshold*>
MonitorSystemWithThreshold::getThresholds (const string &searchString) {
  vector<MonitorPointThreshold*> thresholds;

  // get list of monitor point names matching searchString
  vector<string> monitorPointNames;
  monitorPointNames = getMonitorPointNames(searchString);

  // fill vector with threshold values
  vector<string>::iterator nameIterator;
  vector<string>::iterator monitorPointNamesEnd = monitorPointNames.end();

  for (nameIterator = monitorPointNames.begin();
       nameIterator != monitorPointNamesEnd;
       nameIterator++) {

    MonitorPointThreshold& mpt = getThreshold(*nameIterator);
    thresholds.push_back(&mpt);
  }

  return thresholds;
}

vector<string>
MonitorSystemWithThreshold::getMonitorPointNames(const string &searchString) {
  // vector containing monitor point name matches from searchString pattern
  vector<string> matchedPoints;

  vector<string> mpHierarchy;
  mpHierarchy = getAllMonitorPointNames();

  vector<string>::iterator mpName;
  vector<string>::iterator mpHierarchyEnd = mpHierarchy.end();
  for (mpName = mpHierarchy.begin(); mpName != mpHierarchyEnd; mpName++) {
    if (StringUtils::miniGlob(searchString.c_str(), (*mpName).c_str())) {
      matchedPoints.push_back(*mpName);
    }
  }
  
  return matchedPoints;
}

vector<string>
MonitorSystemWithThreshold::getAllMonitorPointNames() {
  vector<string> mpHierarchy; // complete hierarchy of MP names
  monitorSystem_.hierarchyToVector(mpHierarchy, true, false);
  
  return mpHierarchy;
}
