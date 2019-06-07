#ifndef CARMA_PIPELINE_CATCHDATA_H
#define CARMA_PIPELINE_CATCHDATA_H

#include "carma/pipeline/DataContainer.h"
#include "carma/pipeline/pipelineUtils.h"
#include "carma/util/FrameAlignedTimer.h"
#include "carma/util/types.h"

#include <list>
#include <memory>
#include <string>
#include <vector>
#include <pthread.h>

namespace carma {

  namespace corba {
    class Server;
  }

  namespace correlator {
    namespace lib {
      class CorrelatorListener;
      class CorrelatorData;
    }
  }

  namespace monitor {
    class PipelineSubsystem;
  }


  namespace pipeline {
       
      class DataContainer;
      class DataCollectorN;

      /**
       * Class used to mediate the merging and writing of a data object at 
       * specific times. It instantiates dataCollectors which are responsible
       * for collecting data for part of the data object. This class will
       * write out a complete data object regardless of whether the collectors
       * have filled in their part. 
       */
      class CatchData {
      public:    

        /**
         * Creates all necessary objects 
         */
        explicit CatchData( PipelineType pt, 
                            long monitorWriteDelayMs, 
                            int corrDataWriteDelayMs,
                            const std::string & channelPrefix,
                            const std::string & channelSuffix,
                            carma::corba::Server & server );

        /**
         * Shuts down timer and releases any memory 
         */
        virtual ~CatchData();

        /**
         * add a listener for receiving the dataFrame
         * after data have been collected
         */
        void addCorrelatorListener(carma::correlator::lib::CorrelatorListener* cListener);

        void run();

      private:
        
        // Disallow copy and assignment
        CatchData( const CatchData & rhs );
        CatchData & operator=( const CatchData & rhs );
        
        struct CorrelatorDataStatsInfo;
        struct SharedMonitorInfo;

        /**
         * Executed when timer completes. This will then write out a complete
         * data object. 
         */
        void performUpdate( bool     firstTime,
                            size_t * numBandsHighWaterMark,
                            carma::util::frameType frame );
        
        static void harvestCorrelatorDataStatsInfo(
            CorrelatorDataStatsInfo *                      cdsi,
            const carma::correlator::lib::CorrelatorData & cd );
        
        static void monitorUpdateThread( CatchData & This );

        class MonitorUpdateTQRH; // Thread quit request handler 
        
        void notifyListeners(
            carma::correlator::lib::CorrelatorData * cd ) const;
            
        void processMonitorPoints( const CorrelatorDataStatsInfo & cdsi );
        void fillMonitorData( ) const;
        void computeStats( const CorrelatorDataStatsInfo & cdsi );

        const PipelineType pt_;
        const int waitTimeInMillis_;

        carma::pipeline::DataContainer _dataContainer;
        ::std::list<carma::correlator::lib::CorrelatorListener *> _listeners;

        mutable std::auto_ptr< carma::monitor::PipelineSubsystem > monitorData_;
        ::std::vector< DataCollectorN * > collectors_;

        ::std::auto_ptr< SharedMonitorInfo > smi_;
        mutable carma::util::PthreadMutex smiMutex_;

        carma::util::FrameAlignedTimer frameTimer_;
        ::pthread_t monitorUpdateThread_;
      };

}  // End namespace pipeline
}  // End namespace carma

#endif
