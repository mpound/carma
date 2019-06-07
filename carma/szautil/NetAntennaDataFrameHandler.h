#ifndef SZA_UTIL_NETANTENNADATAFRAMEHANDLER_H
#define SZA_UTIL_NETANTENNADATAFRAMEHANDLER_H

/**
 * @file NetAntennaDataFrameHandler.h
 * 
 * Tagged: Mon Apr  5 00:19:48 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/AntNum.h"
#include "carma/szautil/AntennaDataFrameManager.h"
#include "carma/szautil/NetHandler.h"

namespace sza {
  namespace util {
    
    class NetAntennaDataFrameHandler : public NetHandler {
    public:
      
      /**
       * Constructor.
       */
      NetAntennaDataFrameHandler();
      
      /**
       * Destructor.
       */
      virtual ~NetAntennaDataFrameHandler();
      
      /**
       * Return a pointer to our data frame manager.
       */
      AntennaDataFrameManager* getFrame();

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
      unsigned int getAnt();

    private:

      AntennaDataFrameManager frame_;

    }; // End class NetAntennaDataFrameHandler
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_NETANTENNADATAFRAMEHANDLER_H
