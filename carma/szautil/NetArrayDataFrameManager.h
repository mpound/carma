// $Id: NetArrayDataFrameManager.h,v 1.3 2013/07/09 17:12:00 eml Exp $

#ifndef SZA_UTIL_NETARRAYDATAFRAMEMANAGER_H
#define SZA_UTIL_NETARRAYDATAFRAMEMANAGER_H

/**
 * @file NetArrayDataFrameManager.h
 * 
 * Tagged: Tue Apr 19 16:13:28 PDT 2011
 * 
 * @version: $Revision: 1.3 $, $Date: 2013/07/09 17:12:00 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/ArrayDataFrameManager.h"
#include "carma/szautil/NetArrayTemplate.h"
#include "carma/szautil/NetUnion.h"

namespace sza {
  namespace util {

    class NetArrayDataFrameManager : public ArrayDataFrameManager,
      public NetUnion {
    public:

      enum {
	MEM_TEMPLATE = 0x1,
	MEM_FRAME    = 0x2,
	MEM_UCVEC    = 0x3
      };

      /**
       * Constructor.
       */
      NetArrayDataFrameManager();

      /**
       * Destructor.
       */
      virtual ~NetArrayDataFrameManager();

      // Over load the base-class NetUnion deserialize class

      void deserialize(const std::vector<unsigned char>& bytes);
      void deserializeNativeOrder(const std::vector<unsigned char>& bytes);

      void setTo(ArrayTemplate* arrayTemplate);
      void setTo(unsigned id);

      void resetArrayMap();

      ArrayTemplate* getArrayTemplate();

      void resize();

      // This NetUnion has a NetArrayTemplate, and the frame data from
      // the base-class ArrayDataFrameManager as parts of the union

      NetArrayTemplate nat_;
      std::vector<unsigned char> ucvec_;

    }; // End class NetArrayDataFrameManager

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_NETARRAYDATAFRAMEMANAGER_H
