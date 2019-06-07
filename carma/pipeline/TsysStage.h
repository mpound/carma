// $Id: TsysStage.h,v 1.2 2013/05/15 15:23:00 abeard Exp $

#ifndef CARMA_PIPELINE_TSYSSTAGE_H
#define CARMA_PIPELINE_TSYSSTAGE_H

#include "carma/monitor/PipelineCommon.h"
#include "carma/monitor/NoiseStatusCommon.h"
#include "carma/pipeline/pipelineUtils.h"
#include "carma/pipeline/Stage.h"
#include "carma/pipeline/TsysPipelineInfo.h"
#include "carma/util/PthreadRWLock.h"
#include "carma/util/PthreadMutex.h"

/**
 * @file Tsys.h
 * 
 * Tagged: Tue Nov  9 10:43:11 PST 2004
 * 
 * @version: $Revision: 1.2 $, $Date: 2013/05/15 15:23:00 $
 * 
 * @author Rick Hobbs & Andrew Beard
 */
namespace carma {

    namespace monitor {
        class AstroSubsystem;
        class PipelineSubsystem;
    }

    namespace correlator {
        namespace lib {
            class CorrelatorData;
        }
    }

    namespace pipeline {

        class TsysStage : public Stage {
        public:

            /**
             * Constructor.
             * @param monitor Reference to Pipeline monitor subsystem.
             * @param plmi Reference to PipelineMonitorInput object.
             */
            TsysStage(
                    carma::monitor::PipelineSubsystem & monitor, 
                    const carma::monitor::PipelineMonitorInput & plmi,
                    enum carma::pipeline::PipelineType plType,
                    carma::monitor::AstroSubsystem & astroMonitor ); 

            /**
             * Destructor.
             */
            virtual ~TsysStage();

            /**
             * Reset tsys.
             */
            void resetTsys( const std::vector< int > & carmaAntNoVec );

            /**
             * Turn tsys calibration on or off.  
             * Default is on. This does not prevent calculation of tsys values 
             * and monitor data, only application of tsys to the data.  
             */
            void applyTsysToData( bool apply );

            /**
             * Is tsys being applied to the data?
             */
            bool isTsysBeingApplied( ) const;

            /**
             * Turn flux calibration on or off (default is on).
             */
            void applyFluxCalibrationToData( bool apply );

            /**
             * Is flux being applied to the data?
             */
            bool isFluxBeingApplied( ) const;

            /**
             * Log detailed information about calibration.
             */
            void logCalibration( short astroband );

            /**
             * Retrieve TsysPipelineInfo for use by other stages.
             * TsysPipelineInfo contains calculated tsys parameters which other
             * stages such as SelfCal need.
             */
            const TsysPipelineInfo & getTsysPipelineInfo( ) const;

        private:

            void
                preprocess( carma::correlator::lib::CorrelatorDataPtr cd ); 

            void
                processBand( carma::correlator::lib::CorrelatorBand * cb );

            carma::correlator::lib::CorrelatorDataPtr 
                postprocess( carma::correlator::lib::CorrelatorDataPtr cd );

            void fillMonitorData();

            const carma::monitor::PipelineMonitorInput & plmi_;

            TsysPipelineInfo tsysInfo_;

            bool applyTsys_;
            bool applyFlux_;
            mutable carma::util::PthreadRWLock applyTsysAndFluxRWLock_;

            std::vector< int > resetPendingAntNoVec_;
            mutable carma::util::PthreadMutex resetPendingMutex_;

            carma::monitor::NoiseStatusMonitorPointEnum::NOISESTATUS noiseStatus_;
            bool noiseStatusValid_;

            carma::monitor::PipelineSubsystem & monitorData_;
            carma::monitor::AstroSubsystem & astroMonitor_;

            const enum carma::pipeline::PipelineType plType_;

            short logBandOnce_;

        }; // class TsysStage

    } // namespace pipeline
} // namespace carma
#endif // #ifndef CARMA_PIPELINE_TSYSSTAGE_H
