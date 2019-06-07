// $Id: IntegratorStage.h,v 1.4 2014/05/15 17:11:13 scott Exp $

#ifndef CARMA_PIPELINE_INTEGRATORSTAGE_H
#define CARMA_PIPELINE_INTEGRATORSTAGE_H

#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/pipeline/Stage.h"

#include <string>

namespace carma {

namespace monitor {
    class PipelineMonitorInput;
    class PipelineSubsystem;
} // namespace monitor

namespace pipeline {

    class Integrator : public Stage {
    public:

        /**
         * Constructor.
         */
        Integrator( carma::monitor::PipelineSubsystem& monitor,
                    const carma::monitor::PipelineMonitorInput & plmi );

        /**
         * Destructor.
         */
        virtual ~Integrator();

        /**
         *  Start integrating.
         *  @param intTime Integration length in seconds.
         *  @param numRecords Number of integration records (-1 = infinite).
         *  @param gapTime Inter-record gap length in seconds.
         *  @param seqNo Number to be placed in monitor stream when finished.
         */
        void startIntegration( double intTime, 
                int    numRecords,
                double gapTime,
                int seqNo );

        /**
         *  Stop integrating
         */
        void stopIntegration();

        /**
         * Reset time since last integration.
         */
        void resetTimeSinceLastIntegration( );

    private:

        void stopIntegrationHoldingLock();

        void preprocess( const carma::correlator::lib::CorrelatorDataPtr cd );

        void processBand( carma::correlator::lib::CorrelatorBand * cb );

        carma::correlator::lib::CorrelatorDataPtr 
            postprocess( carma::correlator::lib::CorrelatorDataPtr cd );

        void fillMonitorData();

        void reserveExpectedBands();

        carma::correlator::lib::CorrelatorDataPtr cdThisFrame_; // This frame
        carma::correlator::lib::CorrelatorDataPtr cdAccumulator_; // Accumulator

        std::string bfMode_; // blank/flag mode is retrieved from data
        carma::monitor::PipelineSubsystem & monitorData_;

        struct SharedIntegratorInfo;
        ::std::auto_ptr<SharedIntegratorInfo> shared_; 
        bool integrateThisFrame_;
        bool firstFrameInRecord_;
        bool lastFrameInRecord_;
        bool noData_;
        const carma::monitor::PipelineMonitorInput & plmi_;

    }; // class Integrator

} } // namespace carma::pipeline
#endif // End #ifndef CARMA_PIPELINE_INTEGRATORSTAGE_H
