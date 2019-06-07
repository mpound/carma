#ifndef SZA_UTIL_ARRAYREGMAPDATAFRAMEMANAGER_H
#define SZA_UTIL_ARRAYREGMAPDATAFRAMEMANAGER_H

/**
 * @file ArrayRegMapDataFrameManager.h
 * 
 * Tagged: Tue Oct 26 10:52:37 PDT 2004
 * 
 * @author Erik Leitch
 */
// C header files from the array control code

#include "carma/szaarrayutils/szaregs.h"

#include "carma/szautil/RegMapDataFrameManager.h"
#include "carma/szautil/TimeVal.h"

namespace sza {
  namespace util {
    
    class ArrayRegMapDataFrameManager : public RegMapDataFrameManager {
    public:
      
      /**
       * Constructor.
       */
      ArrayRegMapDataFrameManager(bool archivedOnly = false, 
				  DataFrame* dataFrame = 0);
      
      /**
       * Destructor.
       */
      virtual ~ArrayRegMapDataFrameManager();

      void initialize(bool archivedOnly = false, DataFrame* dataFrame = 0);
      
    private:
    }; // End class ArrayRegMapDataFrameManager
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_ARRAYREGMAPDATAFRAMEMANAGER_H
