#ifndef SZA_UTIL_NETMONITORFRAME_H// $Id: NetMonitorFrame.h,v 1.1 2011/06/08 18:40:13 eml Exp $
#define SZA_UTIL_NETMONITORFRAME_H

/**
 * @file NetMonitorFrame.h
 * 
 * Tagged: Mon May 23 15:48:44 PDT 2011
 * 
 * @version: $Revision: 1.1 $, $Date: 2011/06/08 18:40:13 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/Mutex.h"
#include "carma/szautil/NetArrayDataFrameManager.h"

namespace sza {
  namespace util {

    class NetMonitorFrame {
    public:

      /**
       * Constructor.
       */
      NetMonitorFrame();

      /**
       * Destructor.
       */
      virtual ~NetMonitorFrame();

      void print();
      static void printArrayTemplate(ArrayTemplate* arrayTemplate);

      ArrayTemplate* getArrayTemplate();
      ArrayMap*      getArrayMap();

    public:

      NetArrayDataFrameManager nadfm_;
      Mutex guard_;

    }; // End class NetMonitorFrame

  } // End namespace util
} // End namespace sza

#endif // End #ifndef SZA_UTIL_NETMONITOR_FRAME_H

