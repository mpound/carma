/**
 * @file
 * Declaration for carma::pipeline::Pipeline class.
 *
 * $Id: Pipeline.h,v 1.1 2011/08/18 23:25:52 abeard Exp $
 */
#ifndef CARMA_PIPELINE_PIPELINE_H
#define CARMA_PIPELINE_PIPELINE_H

#include "carma/correlator/lib/CorrelatorDataPtr.h"

#include <memory>

namespace carma {
    
    namespace monitor {
        class AstroSubsystem;
        class PipelineSubsystem;
    }

    namespace correlator {
        namespace lib {
            class CorrelatorData;
        } // namespace lib
    } // namespace correlator
    
    namespace pipeline {

        class Stage;

        class Pipeline {
        public:

            /** 
             * Construct a pipeline instance.
             * @param monitorData Reference to PipelineSubsystem instance.
             * @param astroMonitor Reference to AstroSubsystem instance.
             * @param monitorWriteDelay Delay in ms after the frame time 
             *         before writing monitor data to the FSP.
             */
            explicit 
                Pipeline( carma::monitor::PipelineSubsystem & monitorData,
                        carma::monitor::AstroSubsystem & astroMonitor,
                        long monitorWriteDelayMs,
                        bool autowrite);

            virtual ~Pipeline( );

            /**
             * Add a stage to the end of the pipeline.
             */
            void pushStageBack( Stage & stage ); 

            /**
             * Clear all stages from the pipeline.  
             */
            void clear( );

            /**
             * Pass correlator data sequentially through the pipeline.
             */
            void processCorrelatorData( 
                    carma::correlator::lib::CorrelatorDataPtr data ) const;

            /**
             * Increment missed monitor info count.
             */
            void incrementMissedMonitorInfo( int by = 1 );

        private:

            Pipeline( const Pipeline & );
            Pipeline & operator=( const Pipeline & );

            class Pimpl;
            const ::std::auto_ptr<Pimpl> pimpl_;

        }; // class Pipeline 

    } // namespace pipeline
} // namespace carma
#endif
