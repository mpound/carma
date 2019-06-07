#include "carma/szautil/Debug.h"
#include "carma/antenna/sza/antenna/canbus/CanMonitor.h"
#include "carma/antenna/sza/antenna/canbus/CanMonitorPoint.h"

using namespace std;
using namespace sza::util;
using namespace sza::antenna::canbus;

/**.......................................................................
 * Constructor.
 */
CanMonitor::CanMonitor(sza::antenna::control::SzaShare* share,
		       std::string boardName) : Board(share, boardName) {}

/**.......................................................................
 * Destructor.
 */
CanMonitor::~CanMonitor() 
{
  for(unsigned iMon=0; iMon < monitorPoints_.size(); iMon++) 
      delete monitorPoints_[iMon];
}

/**.......................................................................
 * Add a monitor point to the list of points maintained by
 * this object
 */
CanMonitorPoint* CanMonitor::addMonitorPoint(char* name)
{
  CanMonitorPoint* monitorPoint = new CanMonitorPoint(share_, findReg(name));
  monitorPoints_.push_back(monitorPoint);
  monitorMap_[name] = monitorPoint;

  return monitorPoint;
}

/**.......................................................................
 * Return a point to a monitor point maintained by this object
 */
CanMonitorPoint* CanMonitor::findMonitorPoint(char* name)
{
  std::map<std::string, CanMonitorPoint*>::iterator slot;
  slot = monitorMap_.find(name);
  
  if(Debug::debugging(Debug::DEBUG2)) {
    cout << monitorMap_.size() << endl;
  }

  if(slot != monitorMap_.end())
    return slot->second;

  ThrowError("No such monitor point: " << name);
}
