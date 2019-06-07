#ifndef SZA_UTIL_MONITORPOINTMANAGER_H
#define SZA_UTIL_MONITORPOINTMANAGER_H

/**
 * @file MointorPointManager.h
 * 
 * Tagged: Sat Oct 23 22:15:09 PDT 2004
 * 
 * @author Erik Leitch
 */
#include <map>
#include <string>
#include <vector>

namespace sza {
  namespace util {
    
    class ArrayMapDataFrameManager;
    class CoordRange;
    class MonitorPoint;
    class RegMapDataFrameManager;
    
    class MonitorPointManager {
    public:
      
      /**
       * Constructors.
       */
      MonitorPointManager();
      MonitorPointManager(ArrayMapDataFrameManager* arrMapFm);
      MonitorPointManager(RegMapDataFrameManager*   regMapFm);
      
      /**
       * Destructor.
       */
      virtual ~MonitorPointManager();
      
      /**
       * Add a monitor point to the list of points maintained by this
       * object.  Return a pointer to the newly created monitor point,
       * if desired
       */
      MonitorPoint* addMonitorPoint(char* regMapName, char* boardName, 
				    char* blockName, 
				    CoordRange* range=0);

      MonitorPoint* addMonitorPoint(std::string regMapName, 
				    std::string boardName, 
				    std::string blockName, 
				    CoordRange* range=0);

      MonitorPoint* addMonitorPoint(char* regMapName, char* boardName, 
				    char* blockName, 
				    int   index);
      
      MonitorPoint* addMonitorPoint(char* boardName,  char* blockName, 
				    CoordRange* range=0);

      MonitorPoint* addMonitorPoint(char* boardName,  char* blockName, 
				    int   index);
      
      /**
       * Return a pointer to a monitor point maintained by this object
       */
      MonitorPoint* findMonitorPoint(char* boardName, char* blockName, 
				     CoordRange* range=0, bool doThrow=true);

      MonitorPoint* findMonitorPoint(char* regMapName, char* boardName, 
				     char* blockName, 
				     CoordRange* range=0, bool doThrow=true);

      MonitorPoint* findMonitorPoint(std::string boardName, 
				     std::string blockName, 
				     CoordRange* range=0, bool doThrow=true);

      MonitorPoint* findMonitorPoint(std::string regMapName, 
				     std::string boardName, 
				     std::string blockName, 
				     CoordRange* range=0, bool doThrow=true);
      

      // Remove a single monitor point maintained by this object

      void remMonitorPoint(std::string regMapName, 
			   std::string boardName, 
			   std::string blockName, 
			   CoordRange* range=0);
      
      void remMonitorPoint(std::string boardName, 
			   std::string blockName, 
			   CoordRange* range=0);
      
      void remMonitorPoint(std::ostringstream& os);
      
      // Reset all monitor points managed by this object
      
      void reset();

      // Clear all monitor points managed by this object

      void clear();

      // List monitor points managed by this object

      void list();

      // Return a list of all monitor points

      std::vector<std::string> getList(bool sort=false);

      /**
       * An operator for printing this object
       */
      friend std::ostream& operator<<(std::ostream& os, 
				      MonitorPointManager& mp);

      // Return a formatted printout of a single monitor point

      std::string formatReg(std::string regMapName, std::string boardName, std::string blockName, 
			    CoordRange* range);

    private:
      
      bool isArrMap_;
      RegMapDataFrameManager* regMapFm_;
      ArrayMapDataFrameManager* arrMapFm_;

      std::vector<MonitorPoint*> monitorPoints_;
      std::map<std::string, MonitorPoint*> monitorMap_;
      
    }; // End class Monitor
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_MONITORPOINTMANAGER_H
