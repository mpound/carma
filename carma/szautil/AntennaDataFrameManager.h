#ifndef SZA_UTIL_ANTENNADATAFRAMEMANAGER_H
#define SZA_UTIL_ANTENNADATAFRAMEMANAGER_H

/**
 * @file AntennaDataFrameManager.h
 * 
 * Tagged: Sat Mar 20 05:20:30 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/AntNum.h"
#include "carma/szautil/RegMapDataFrameManager.h"
#include "carma/szautil/AntennaDataFrame.h"

// C header files from the array control code

#include "carma/szaarrayutils/szaregs.h"

namespace sza {
  namespace util {
    
    class AntennaDataFrameManager : public sza::util::RegMapDataFrameManager {
    public:
      
      /**
       * Constructors
       */
      AntennaDataFrameManager(bool archivedOnly=false);

      AntennaDataFrameManager(const sza::util::AntNum& antNum,
			      bool archivedOnly=false);

      AntennaDataFrameManager(sza::util::AntNum* antNum,
			      bool archivedOnly=false);

      AntennaDataFrameManager(AntennaDataFrameManager& fm);
      
      /**
       * Destructor.
       */
      virtual ~AntennaDataFrameManager();
      
      /**
       * Set the antenna number associated with this dataframe.
       * @throws Exception
       */
      void setAnt(sza::util::AntNum::Id antennaId);
      
      /**
       * Return the antenna number associated with this dataframe.
       *
       * @throws Exception
       */
      unsigned int getAntIntId();
      
      /**
       * Return the antenna descriptor associated with this dataframe.
       */
      AntNum getAnt();

      /**
       * Initialize this object.
       */
      void initialize(bool archivedOnly = false);
      
      /**
       * Assignment operators
       */
      void operator=(RegMapDataFrameManager& fm);
      void operator=(AntennaDataFrameManager& fm);

    private:
      
      AntennaDataFrame* antFrame_; // Pointer to the frame managed by
				   // this object

    }; // End class AntennaDataFrameManager
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_ANTENNADATAFRAMEMANAGER_H
