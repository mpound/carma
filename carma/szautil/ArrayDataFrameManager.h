#ifndef SZA_TRANSLATOR_ARRAYDATAFRAMEMANAGER_H
#define SZA_TRANSLATOR_ARRAYDATAFRAMEMANAGER_H

/**
 * @file ArrayDataFrameManager.h
 * 
 * Tagged: Sat Mar 20 05:20:30 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/ArrayMapDataFrameManager.h"
#include "carma/szautil/AntennaDataFrameManager.h"
#include "carma/szautil/Coord.h"

// C header files from the array control code

#include "carma/szaarrayutils/szaregs.h"

namespace sza {
  namespace util {
    
    class ArrayDataFrameManager : public ArrayMapDataFrameManager {
    public:
      
      /**
       * Constructors
       */
      ArrayDataFrameManager(bool archivedOnly=false, ArrayMap* arrayMap=NULL);
      ArrayDataFrameManager(ArrayDataFrameManager& fm);

      /**
       * Destructor.
       */
      virtual ~ArrayDataFrameManager();
      
      /**
       * Initialize this object.
       */
      void initialize(ArrayMap* arrayMap, bool archivedOnly_=false);
      
      /**
       * Assignment operators
       */
      void operator=(ArrayDataFrameManager& fm);
      
      /**
       * Write an antenna data frame into our array frame
       */
      void writeAntennaRegMap(AntennaDataFrameManager& fm, bool lockFrame);

      /**
       * Find the register map corresponding to an antenna data frame
       */
      ArrRegMap* findAntennaRegMap(AntennaDataFrameManager& fm);

    }; // End class ArrayDataFrameManager
    
  } // End namespace util
} // End namespace sza

#endif // End #ifndef SZA_TRANSLATOR_ARRAYDATAFRAMEMANAGER_H
