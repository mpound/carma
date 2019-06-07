#ifndef SZA_ANTENNA_CONTROL_SZATASK_H
#define SZA_ANTENNA_CONTROL_SZATASK_H

/**
 * @file SzaTask.h
 * 
 * Tagged: Thu Nov 13 16:53:54 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Directives.h"

#include "carma/antenna/sza/antenna/control/SzaShare.h"

#if DIR_HAVE_CARMA
#include "carma/antenna/sza/antenna/canbus/CanDevice.h"
#include "carma/antenna/sza/antenna/canbus/CanMaster.h"
#endif

namespace sza {
  namespace antenna {
    namespace control {
      
      class SzaTask {
	
      public:
	
	/**
	 * Constructor.
	 */
	SzaTask();
	
	/**
	 * Make this virtual so that inheritors destructors are
	 * properly called even if they are upcast.
	 */
	virtual ~SzaTask();
	
#if DIR_HAVE_CARMA
	/**
	 * Public method to get a pointer to our CAN master object.
	 */
	sza::antenna::canbus::CanMaster* getCanMaster();
#endif
	
	/**
	 * Public method to get a pointer to our shared object.
	 */
	SzaShare* getShare();
	
	protected:
	
	/**
	 * The shared-memory object.
	 */
	SzaShare* share_;
	
#if DIR_HAVE_CARMA
	/**
	 * A reference to the CAN bus master, if any.
	 */
	sza::antenna::canbus::CanMaster* canMaster_;
	
	/**
	 * A std::vector of devices, if any, controlled by this task on
	 * the CAN network.
	 */
	std::vector <sza::antenna::canbus::CanDevice*> canDevices_;
	
	/**
	 * Add our devices to the CAN network.
	 */
	void addCanDevices();
	
	/**
	 * Remove our devices from the CAN network.
	 */
	void removeCanDevices();

	/**
	 * Enable return of monitoring packets for all CAN devices
	 * controlled by this taks.
	 */
	void enableCanMonitoring(bool timeStampEnable, 
				 bool blankingFrameEnable,
				 bool slowMonitorEnable);
	
#endif
      }; // End class SzaTask

    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
