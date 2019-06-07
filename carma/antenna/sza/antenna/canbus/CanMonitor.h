#ifndef SZA_ANTENNA_CANBUS_CANMONITOR_H
#define SZA_ANTENNA_CANBUS_CANMONITOR_H

/**
 * @file CanMonitor.h
 * 
 * Tagged: Sat Oct 23 22:15:09 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/canbus/CanMonitorPoint.h"

#include "carma/antenna/sza/antenna/control/Board.h"

#include <map>
#include <string>
#include <vector>

namespace sza {
  namespace antenna {
    namespace canbus {
      
      // Forward declaration of SzaShare lets us use it without defining
      // it
      
      namespace control {
	class SzaShare;
      }
      
      class CanMonitorPoint;

      class CanMonitor : public sza::antenna::control::Board {
      public:
	
	/**
	 * Constructor.
	 */
	CanMonitor(sza::antenna::control::SzaShare* share,
		   std::string boardName);

	/**
	 * Destructor.
	 */
	virtual ~CanMonitor();
	
	/**
	 * Add a monitor point to the list of points maintained by
	 * this object.  Return a pointer to the newly created monitor
	 * point, if desired
	 */
	CanMonitorPoint* addMonitorPoint(char* name);

	/**
	 * Return a point to a monitor point maintained by this object
	 */
	CanMonitorPoint* findMonitorPoint(char* name);

      private:

	std::vector<CanMonitorPoint*> monitorPoints_;
	std::map<std::string, CanMonitorPoint*> monitorMap_;

      }; // End class CanMonitor
      
    } // End namespace canbus
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CANBUS_CANMONITOR_H
