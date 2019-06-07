#include "carma/szautil/CoordRange.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/MonitorPointManager.h"
#include "carma/szautil/MonitorPoint.h"
#include "carma/szautil/ArrayMapDataFrameManager.h"
#include "carma/szautil/RegMapDataFrameManager.h"
#include "carma/szautil/Sort.h"

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructors.
 */
MonitorPointManager::MonitorPointManager()
{
  isArrMap_ = true;
}

/**.......................................................................
 * Constructors.
 */
MonitorPointManager::MonitorPointManager(ArrayMapDataFrameManager* arrMapFm)
{
  arrMapFm_ = arrMapFm;
  regMapFm_ = 0;
  isArrMap_ = true;
}

MonitorPointManager::MonitorPointManager(RegMapDataFrameManager* regMapFm) 
{ 
  arrMapFm_ = 0;
  regMapFm_ = regMapFm;
  isArrMap_ = false;
}

/**.......................................................................
 * Destructor.
 */
MonitorPointManager::~MonitorPointManager() 
{
  clear();
}

/**.......................................................................
 * Add an ArrayMap monitor point to the list of points maintained by
 * this object
 */
MonitorPoint* MonitorPointManager::
addMonitorPoint(std::string regMapName, std::string boardName, 
		std::string blockName,  CoordRange* range)
{
  return addMonitorPoint((char*)regMapName.c_str(), 
			 (char*)boardName.c_str(), 
			 (char*)blockName.c_str(), range);
}

/**.......................................................................
 * Add an ArrayMap monitor point to the list of points maintained by
 * this object
 */
MonitorPoint* MonitorPointManager::
addMonitorPoint(char* regMapName, char* boardName, char* blockName, 
		CoordRange* range)
{
  if(!isArrMap_)
    ThrowError("ArrayMap monitor point requested for a RegMap "
	       "MonitorPointManager")

  MonitorPoint* monitorPoint = 0; 

  try {
    monitorPoint = new MonitorPoint(arrMapFm_, arrMapFm_->getArrReg(regMapName),
				    arrMapFm_->findReg(regMapName, boardName, 
						       blockName), range);
  } catch(Exception& err) {
    ThrowError("Caught exception: " << err.what() 
	       << " while adding monitor point for: " << regMapName << "." 
	       << boardName << "." << blockName);
  }

  // Add the new monitor point to both the vector of MonitorPoints
  // and to the map

  monitorPoints_.push_back(monitorPoint);

  ostringstream os;
  os << regMapName << "." << boardName << "." << blockName;

  if(range != 0)
    os << *range;

  monitorMap_[os.str()] = monitorPoint;

  return monitorPoint;
}

/**.......................................................................
 * Add a monitor point to the list of points maintained by
 * this object
 */
MonitorPoint* MonitorPointManager::
addMonitorPoint(char* boardName, char* blockName, CoordRange* range)
{
  if(isArrMap_)
    ThrowError("RegMap monitor point requested for an ArrayMap MonitorPointManager")

  MonitorPoint* monitorPoint = 0; 
  try {
    monitorPoint = new MonitorPoint(regMapFm_, regMapFm_->findReg(boardName, blockName), range);
  } catch(Exception& err) {
    ThrowError("Caught exception: " << err.what() << " while adding monitor point for: " << boardName << "." 
	       << blockName);
  }

  monitorPoints_.push_back(monitorPoint);

  ostringstream os;
  os << boardName << "." << blockName;

  if(range != 0)
    os << *range;

  monitorMap_[os.str()] = monitorPoint;

  return monitorPoint;
}

/**.......................................................................
 * Add a monitor point to the list of points maintained by
 * this object
 */
MonitorPoint* MonitorPointManager::
addMonitorPoint(char* regMapName, char* boardName, char* blockName, int index)
{
  if(!isArrMap_)
    ThrowError("ArrayMap monitor point requested for a RegMap "
	       "MonitorPointManager");

  MonitorPoint* monitorPoint=0;
 
  try {
    monitorPoint = new MonitorPoint(arrMapFm_, 
				    arrMapFm_->getArrReg(regMapName),
				    arrMapFm_->findReg(regMapName, boardName, 
						       blockName), 
				    index);
  } catch(Exception& err) {
    ThrowError("Caught exception: " << err.what() 
	       << " while adding monitor point for: " << regMapName << "." 
	       << boardName << "." << blockName);
  }

  ostringstream os;
  os << regMapName << "." << boardName << "." << blockName << "[" << index << "]";

  monitorMap_[os.str()] = monitorPoint;

  return monitorPoint;
}

/**.......................................................................
 * Add a monitor point to the list of points maintained by
 * this object
 */
MonitorPoint* MonitorPointManager::
addMonitorPoint(char* boardName, char* blockName, int index)
{
  if(isArrMap_)
    ThrowError("RegMap monitor point requested for an ArrayMap "
	       "MonitorPointManager");

  MonitorPoint* monitorPoint = 0;
 
  try {
    monitorPoint = new MonitorPoint(regMapFm_, 
				    regMapFm_->findReg(boardName, blockName), 
				    index);
  } catch(Exception& err) {
    ThrowError("Caught exception: " << err.what() 
	       << " while adding monitor point for: " 
	       << boardName << "." << blockName);
  }

  monitorPoints_.push_back(monitorPoint);

  ostringstream os;
  os << boardName << "." << blockName << "[" << index << "]";

  monitorMap_[os.str()] = monitorPoint;

  return monitorPoint;
}

/**.......................................................................
 * Return a point to a monitor point maintained by this object
 */
MonitorPoint* MonitorPointManager::
findMonitorPoint(std::string regMapName, std::string boardName, 
		 std::string blockName,  CoordRange* range, bool doThrow)
{
  return findMonitorPoint((char*)regMapName.c_str(), (char*)boardName.c_str(), 
			  (char*)blockName.c_str(),  range, doThrow);
}

MonitorPoint* MonitorPointManager::
findMonitorPoint(char* regMapName, char* boardName, char* blockName, 
		 CoordRange* range, bool doThrow)
{
  ostringstream os;
  os << regMapName << "." << boardName << "." << blockName;

  if(range != 0)
    os << *range;

  if(!isArrMap_)
    ThrowError("ArrayMap monitor point requested for a RegMap MonitorPointManager");

  std::map<std::string, MonitorPoint*>::iterator slot;
  slot = monitorMap_.find(os.str());

  MonitorPoint* mp=0;
  if(slot != monitorMap_.end()) {
    mp = slot->second;
    if(range==0 || mp->coordRange() == *range)
      return mp;
  }

  if(doThrow) {
    ThrowError("No such monitor point: " << os.str());
  }

  return mp;
}

/**.......................................................................
 * Return a point to a monitor point maintained by this object
 */
MonitorPoint* MonitorPointManager::
findMonitorPoint(std::string boardName, std::string blockName, 
		 CoordRange* range, bool doThrow)
{
  return findMonitorPoint((char*)boardName.c_str(), (char*)blockName.c_str(), 
			  range, doThrow);
}

MonitorPoint* MonitorPointManager::
findMonitorPoint(char* boardName, char* blockName, CoordRange* range, bool doThrow)
{
  if(isArrMap_)
    ThrowError("RegMap monitor point requested for an ArrayMap "
	       "MonitorPointManager");

  std::map<std::string, MonitorPoint*>::iterator slot;

  ostringstream os;
  os << boardName << "." << blockName;
  
  if(range != 0)
    os << *range;

  slot = monitorMap_.find(os.str());

  MonitorPoint* mp=0;
  if(slot != monitorMap_.end()) {
    mp = slot->second;
    if(range==0 || mp->coordRange() == *range)
      return mp;
  }

  if(doThrow) {
    ThrowError("No such monitor point: " << boardName << "." << blockName);
  }

  return mp;
}

/**.......................................................................
 * Reset all monitor points
 */
void MonitorPointManager::reset()
{
  for(unsigned iMon=0; iMon < monitorPoints_.size(); iMon++) {
    monitorPoints_[iMon]->reset();
  }
}

/**.......................................................................
 * Remove all monitor points
 */
void MonitorPointManager::clear()
{
  for(unsigned iMon=0; iMon < monitorPoints_.size(); iMon++) {
    delete monitorPoints_[iMon];
    monitorPoints_[iMon] = 0;
  }

  monitorPoints_.clear();
  monitorMap_.clear();
}

/**.......................................................................
 * List the monitor points maintained by this object
 */
void MonitorPointManager::list()
{
  std::map<std::string, MonitorPoint*>::iterator iMap;

  for(iMap = monitorMap_.begin(); iMap != monitorMap_.end(); iMap++)
    COUT(iMap->first);
}

/**.......................................................................
 * Return a list of all monitor points
 */
std::vector<std::string> MonitorPointManager::getList(bool sort)
{
  std::ostringstream os;
  std::map<std::string, MonitorPoint*>::iterator iMap;

  vector<string> outputList, sortedList;

  if(monitorMap_.begin() == monitorMap_.end()) {
    os << "(no registers are being monitored)" << std::endl;
    outputList.push_back(os.str());
  } else {
    for(iMap = monitorMap_.begin(); iMap != monitorMap_.end(); iMap++) {
      outputList.push_back(iMap->second->format((std::string&)iMap->first));
    }
  }
  
  if(sort) {
    sortedList = Sort::sort(outputList);
    return sortedList;
  } else
    return outputList;
}

/**.......................................................................
 * Format a monitor point
 */
std::string MonitorPointManager::
formatReg(string regMapName, string boardName, string blockName, 
	  CoordRange* range)
{
  ostringstream os;
  os << regMapName << "." << boardName << "." << blockName;

  if(range != 0)
    os << *range;

  std::map<std::string, MonitorPoint*>::iterator slot;
  slot = monitorMap_.find(os.str());

  MonitorPoint* mp=0;

  if(slot != monitorMap_.end()) {

    mp = slot->second;

    if(range==0 || mp->coordRange() == *range) {
      return slot->second->format((std::string&)slot->first);
    }
  } else {
    ThrowError("No such monitor point: " << os);
  }
}

/**.......................................................................
 * Output operator for this object
 */
std::ostream& sza::util::operator<<(std::ostream& os, MonitorPointManager& mp)
{
  std::map<std::string, MonitorPoint*>::iterator iMap;

  if(mp.monitorMap_.begin() == mp.monitorMap_.end()) {
    os << "(no registers are being monitored)" << std::endl;
  } else {
    for(iMap = mp.monitorMap_.begin(); iMap != mp.monitorMap_.end(); iMap++) {
      os << iMap->first << std::endl;
    }
  }

  return os;
}

/**.......................................................................
 * Remove a monitor point from the list of points maintained by this
 * object
 */
void MonitorPointManager::
remMonitorPoint(string regMapName, string boardName, 
		string blockName,  CoordRange* range) 
{
  ostringstream os;
  os << regMapName << "." << boardName << "." << blockName;

  if(range != 0)
    os << *range;

  remMonitorPoint(os);
}

/**.......................................................................
 * Remove a monitor point from the list of points maintained by this
 * object
 */
void MonitorPointManager::
remMonitorPoint(string boardName,  string blockName, 
		CoordRange* range)
{
  ostringstream os;
  os << boardName << "." << blockName;

  if(range != 0)
    os << *range;

  remMonitorPoint(os);
}


void MonitorPointManager::
remMonitorPoint(std::ostringstream& os)
{
  // Find the map slot corresponding to the passed string

  std::map<std::string, MonitorPoint*>::iterator 
    slot = monitorMap_.find(os.str());

  // Check if a slot was found -- if so, delete it from both the map
  // and the vector

  if(slot != monitorMap_.end()) {

    MonitorPoint* mp = slot->second;

    // Now if the range matches too, delete this object (I shouldn't
    // have to check this, since the range was already used to
    // construct the map name -- why do I?)
    
    monitorMap_.erase(slot);

    // Now erase the corresponding vector element as well

    std::vector<MonitorPoint*>::iterator vSlot;
    for(vSlot=monitorPoints_.begin(); vSlot != monitorPoints_.end(); vSlot++) {
      if(*vSlot == mp)
	break;
    }
    
    if(vSlot != monitorPoints_.end()) {
      monitorPoints_.erase(vSlot);
    } else {
      ThrowError("No monitor point vector element was found to correspond to: "
		 << slot->first);
    }
    
    // Finally, delete the memory for this monitor point
    
    delete mp;
  }
}
