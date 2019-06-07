#ifndef CARMA_PIPELINE_PUBLISHERSTAGE_H
#define CARMA_PIPELINE_PUBLISHERSTAGE_H


#include "carma/pipeline/pipelineUtils.h"
#include "carma/pipeline/Stage.h"
#include "carma/util/ConcurrentQueue.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/SimpleStatisticsAccumulators.h"
#include "carma/util/ThreadQuit.h"

#include <boost/shared_ptr.hpp>
#include <map>
#include <string>

namespace carma {
namespace correlator {
namespace lib {
    class CorrelatorData;
}  // namespace carma::correlator::lib

namespace obsRecord2 {
    class CorbaCorrProducer;
}  // namespace carma::correlator::obsRecord2
}  // namespace carma::correlator

namespace monitor {
    class PipelineSubsystem;
}  // namespace carma::monitor

namespace pipeline {

    //! @brief Class used to publish Correlator Data and send data out via
    //!        a notification channel
    //!
    //! @author Rick Hobbs
    class Publisher: public Stage {
    public:

        //! @brief Constructor
        Publisher(
                carma::monitor::PipelineSubsystem & monitor,
                const std::string &                 channelName,
                const std::string &                 servedObjectName, 
                carma::pipeline::PipelineType plType );

        //! @brief Destructor
        virtual ~Publisher( );

    private:

        void preprocess( carma::correlator::lib::CorrelatorDataPtr cd );

        void processBand( carma::correlator::lib::CorrelatorBand * cb );

        carma::correlator::lib::CorrelatorDataPtr
        postprocess( carma::correlator::lib::CorrelatorDataPtr cd );

        void fillMonitorData( );

        static void publishCorrDataThread( Publisher & This );

        class PublishCorrDataTQRH : 
            public carma::util::ThreadQuitRequestHandler {
        public:

            explicit PublishCorrDataTQRH( Publisher & mom );

            virtual ~PublishCorrDataTQRH( );

            void HandleQuitRequest( ::pthread_t thread );

        private:

            Publisher & mom_;
        };

        struct PublishRequest {
            carma::correlator::lib::CorrelatorDataPtr data;
        };

        typedef carma::util::ConcurrentQueue<PublishRequest> PublishQueue;

        struct Shared {
            Shared( );
            mutable carma::util::PthreadMutex mutex;
            carma::util::FloatStatAccumulator corbaSendTimeAcc;
            float lastCorbaSendMillis;
            float lastCorbaSendKilobytes;
        };

        Shared shared_;
        PublishQueue publishRequestQueue_;

        carma::monitor::PipelineSubsystem & monitorData_;

        typedef carma::correlator::obsRecord2::CorbaCorrProducer CorrProducer;
        typedef ::boost::shared_ptr< CorrProducer > CorrProducerPtr;
        typedef ::std::map< int, CorrProducerPtr  > CorrProducerMap; 

        CorrProducerMap corrProducers_;
        ::pthread_t publisherThreadId_;

        const PublishQueue::size_type maxQueueRequests_;
    }; // class Publisher

}  // namespace carma::pipeline
}  // namespace carma
#endif
