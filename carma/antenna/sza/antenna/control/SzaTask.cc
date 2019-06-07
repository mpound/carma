#include "carma/szautil/Debug.h"
#include "carma/antenna/sza/antenna/control/SzaTask.h"

using namespace sza::antenna::control;
using namespace sza::util;

/**
 * Constructor just initializes the shared object pointer to
 * NULL.
 */
SzaTask::SzaTask()
{
#if DIR_HAVE_CARMA
  canMaster_ = 0;
#endif
  share_     = 0;
};

/**
 * Destructor.
 */
SzaTask::~SzaTask() 
{
  DBPRINT(true, Debug::DEBUG7, "Inside SzaTask destructor");

#if DIR_HAVE_CARMA
  removeCanDevices();
  
  try {
    
    // Delete any CAN devices allocated in this task.
    
    for(unsigned idev=0; idev < canDevices_.size(); idev++)
      if(canDevices_[idev] != 0) {
	delete canDevices_[idev];
	canDevices_[idev] = 0;
      }
  } catch (...) { // Handle any non-sza exceptions.
    ThrowError(" Caught an exception (SzaTask)");
  }
#endif
};

/**
 * Public method to get a pointer to our shared object.
 */
SzaShare* SzaTask::getShare() {
  return share_;
}

#if DIR_HAVE_CARMA
/**
 * Public method to get a pointer to our shared object.
 */
sza::antenna::canbus::CanMaster* SzaTask::getCanMaster() {
  return canMaster_;
}

/** 
 * Add devices to the CAN network.
 */
void SzaTask::addCanDevices()
{
  for(unsigned idev=0; idev < canDevices_.size(); idev++) 
    canMaster_->addCanDevice(canDevices_[idev]);
}

/** 
 * Enable monitoring for all CAN devices on the network.
 */
void SzaTask::enableCanMonitoring(bool timeStampEnable, 
				  bool blankingFrameEnable,
				  bool slowMonitorEnable)
{
  for(unsigned idev=0; idev < canDevices_.size(); idev++) {
    canDevices_[idev]->enableMonitorPackets(timeStampEnable,
					    blankingFrameEnable,
					    slowMonitorEnable);
    canDevices_[idev]->sendTimeStamp();
    
    canDevices_[idev]->setState(carma::canbus::ONLINE);
  }
}

/** 
 * Remove devices from the CAN network.
 */
void SzaTask::removeCanDevices()
{
  for(unsigned idev=0; idev < canDevices_.size(); idev++)
    canMaster_->removeCanDevice(canDevices_[idev]);
}
#endif
