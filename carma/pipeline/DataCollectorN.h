#ifndef CARMA_PIPELINE_DATACOLLECTORN_H
#define CARMA_PIPELINE_DATACOLLECTORN_H

#include "carma/correlator/obsRecord2/CorrDataConsumer.h"
#include "carma/util/PthreadMutex.h"

#include <string>

namespace carma {

namespace corba {
  class Server;
}

namespace correlator {
namespace lib {

class CorrelatorData;

}  // namespace carma::correlator::lib
}  // namespace carma::correlator

namespace pipeline {

class DataContainer;

      /**
       * Concrete class responsible for connecting to a single band of
       * Correlator Data. Uses notification channels.
       */
      class DataCollectorN :
        public carma::correlator::obsRecord2::CorrDataConsumer::Listener {
      public:

        /**
         * Constructor
         */
        DataCollectorN( const std::string & ncName, 
                        DataContainer & dataContainer,
                        carma::corba::Server & server );
        
        /**
         * Destructor
         */
        virtual ~DataCollectorN();
        
        /**
         *  called when data is available
         */
        void processData( carma::correlator::lib::CorrelatorData * cd );

        /**
         * Is collection active.
         */
        bool isCollectorActive( ) const;

        /**
         * Retrieve deserialization statistics
         */
        carma::correlator::obsRecord2::CorbaCorrConsumerStats
        getCorbaCorrConsumerStats( ) const;

      private:    

        DataContainer & _dataContainer;
        carma::correlator::obsRecord2::CorrDataConsumer * corrConsumer_;

        bool corrConsumerActive_;
        mutable carma::util::PthreadMutex corrConsumerActiveMutex_;

        bool collectionActive_;
        mutable carma::util::PthreadMutex collectionActiveMutex_;

        static const std::string className_;

        const std::string ncName_;
        
        struct CollectionThreadArgs;

        static void dataCollectionThread( CollectionThreadArgs & args ); 
      
      };
      
}  // namespace carma::pipeline
}  // namespace carma

#endif
