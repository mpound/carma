// $Id: CorrelatorDataPool.h,v 1.2 2007/05/08 23:20:42 abeard Exp $

#ifndef CARMA_CORRELATOR_LIB_CORRELATORDATAPOOL_H
#define CARMA_CORRELATOR_LIB_CORRELATORDATAPOOL_H

#include <vector>
#include "carma/util/PthreadMutex.h"
#include "carma/correlator/lib/CorrelatorData.h"

/**
 * @file CorrelatorDataPool.h
 * 
 * Tagged: Tue Aug 30 10:40:49 PDT 2005
 * 
 * @version: $Revision: 1.2 $, $Date: 2007/05/08 23:20:42 $
 * 
 * @author Rick Hobbs
 */
namespace carma {
  namespace correlator {
    namespace lib {

      class CorrelatorDataPool {
      public:

        /**
         * Constructor.
         */
        CorrelatorDataPool();

        /**
         * Destructor.
         */
        virtual ~CorrelatorDataPool();
        
        /**
         *  Return the first available CorrelatorData object from the pool.
         *  If all objects in the pool are still in use, then create a new
         *  object
         */
        CorrelatorData* getCorrelatorData();

        /**
         *  Returns the total size of the pool(both in use and available
         *  objects
         */
        int getPoolSize();

        /**
         *  Return the number of objects in pool currently being used
         */
        int getInUseCount();

        /**
         *  Return the number of objects in pool that are available
         */
        int getAvailableCount();

      private:
        std::vector<carma::correlator::lib::CorrelatorData*> pool_;
        carma::util::PthreadMutex              mutex_;

      }; // End class CorrelatorDataPool

    } // End namespace lib
  } // End namespace correlator
} // End namespace carma



#endif // End #ifndef CARMA_CORRELATOR_LIB_CORRELATORDATAPOOL_H
