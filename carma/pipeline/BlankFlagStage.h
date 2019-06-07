
#ifndef CARMA_PIPELINE_BLANKFLAGSTAGE_H
#define CARMA_PIPELINE_BLANKFLAGSTAGE_H

#include "carma/monitor/PipelineCommon.h"
#include "carma/pipeline/pipelineUtils.h"
#include "carma/pipeline/Stage.h"
#include "carma/pipeline/ShadowingCalculator.h"

namespace carma {

namespace correlator {
namespace lib {
    class CorrelatorData;
} } // namespace correlator::lib

namespace monitor {
    class PipelineSubsystem;
    class PipelineMonitorInput;
} // namespace monitor

namespace pipeline {

    class BlankFlag : public Stage {
    public:

        /**
         * Constructor.
         */
        explicit BlankFlag( carma::monitor::PipelineSubsystem & monitor,
                            carma::monitor::AstroSubsystem & astroMonitor,
                            const carma::monitor::PipelineMonitorInput & plmi,
                            carma::pipeline::PipelineType plType );

        /**
         * Destructor.
         */
        virtual ~BlankFlag( );

    private:

        void preprocess( const carma::correlator::lib::CorrelatorDataPtr cd );

        void processBand( carma::correlator::lib::CorrelatorBand * cb );

        carma::correlator::lib::CorrelatorDataPtr
        postprocess( carma::correlator::lib::CorrelatorDataPtr cd );

        void fillMonitorData();

        int blankCount_; 
        int flagCount_;  
        int totalDataCount_; 

        int pctBlanked_;
        int pctFlagged_;

        // Swept volume and baseline shadowed ants - both are 
        // Carma ant idx based per ShadowingCalculator interface.
        std::vector<bool> svShadowedAnts_; 
        std::vector<bool> blShadowedAnts_; 

        const carma::monitor::PipelineMonitorInput & plmi_;
        carma::monitor::PipelineSubsystem & monitorData_;
        carma::monitor::AstroSubsystem & astroMonitor_;
        ShadowingCalculator shadowing_;
        const carma::pipeline::PipelineType plType_;

    }; // class BlankFlag

} } // namespace carma::pipeline
#endif // #ifndef CARMA_PIPELINE_BLANKFLAGSTAGE_H
