#ifndef CARMA_MONITOR_PIPELINESUBSYSTEM_H
#define CARMA_MONITOR_PIPELINESUBSYSTEM_H

/**
 * @file PipelineSubsystem.h
 * 
 * @author Rick Hobbs
 * @author Andy Beard (Templatization)
 */

#include "carma/monitor/PipelineCommon.h"

#include <utility>

namespace carma {
    namespace monitor {

        /**
         * Abstract base class for retrieving pipeline monitor system
         * components common between the spectral line and wideband systems. 
         * This is a wrapper over the mpml2cpp generated WBPipelineSubsystem
         * and SLPipelineSubsystem classes which allows them to be used
         * generically (i.e. correlator generic applications).
         * @see carma::monitor::PipelineSubsystemTemplate
         * @see carma::monitor::PipelineSubsystemSL
         * @see carma::monitor::PipelineSubsystemWB
         */
        class PipelineSubsystem {
        public:

            /**
             * Constructor.
             */
            explicit PipelineSubsystem( );

            /**
             * Destructor.
             */
            virtual ~PipelineSubsystem( );

            /**
             * Retrieve band count - differs among wb and sl systems.
             */
            virtual int getBandCount( ) const = 0;

            virtual 
            carma::monitor::PipelineStatus &
            getPipelineStatus( ) const = 0;

            virtual
            carma::monitor::StageStats &
            getCatchDataStageStats( ) const = 0;

            virtual
            carma::monitor::CatchDataStage &
            getCatchDataStage( ) const = 0;

            virtual
            carma::monitor::CatchDataBand &
            getCatchDataBand( int bandIdx ) const = 0;
            
            virtual
            carma::monitor::StageStats &
            getCoherenceStageStats( ) const = 0;

            virtual
            carma::monitor::StageStats &
            getDecimationStageStats( ) const = 0;

            virtual
            carma::monitor::Decimation &
            getDecimation( int bandIdx ) const = 0;

            virtual
            carma::monitor::SelfCal &
            getSelfCal( int bandIdx, bool usb ) const = 0;

            virtual
            carma::monitor::StageStats &
            getPassBandStageStats( ) const = 0;

            virtual
            carma::monitor::StageStats &
            getTsysStageStats( ) const = 0;

            virtual 
            carma::monitor::TsysStage &
            getTsysStage( ) const = 0;

            virtual
            carma::monitor::StageStats &
            getBlankFlagStageStats( ) const = 0;

            virtual
            carma::monitor::BlankFlagStage &
            getBlankFlagStage( ) const = 0;

            virtual
            carma::monitor::StageStats &
            getLinelengthStageStats( ) const = 0;

            virtual
            carma::monitor::StageStats &
            getIFcorrectionStageStats( ) const = 0;

            virtual
            carma::monitor::StageStats &
            getWvrStageStats( ) const = 0;

            virtual
            carma::monitor::StageStats &
            getSelfCalStageStats( ) const = 0;

            virtual
            carma::monitor::StageStats &
            getIntegratorStageStats( ) const = 0;

            virtual
            carma::monitor::IntegratorStage &
            getIntegratorStage( ) const = 0;

            virtual
            carma::monitor::StageStats &
            getCorrelatorPublisherStageStats( ) const = 0;

            virtual
            carma::monitor::CorrelatorPublisherStage &
            getCorrelatorPublisherStage( ) const = 0;

            virtual
            carma::monitor::StageStats &
            getVisBrickStageStats( ) const = 0;

            virtual
            carma::monitor::VisBrickStage &
            getVisBrickStage( ) const = 0;

           /**
             * Retrieve reference to lastIntegration container
             */
            virtual 
            carma::monitor::LastIntegration& lastIntegration() const = 0;
            
            /**
             * Start the monitor system auto writer (automatically writes 
             * data to the Frame Scriber Publisher every half second).
             */
            virtual void startAutoWriter( float delay ) = 0; 

            /**
             * Stop the autowriter.
             */
            virtual void stopAutoWriter( ) = 0;

            /**
             * Check to see if autowriter is alive.
             */
            virtual bool autoWriterIsAlive( ) const = 0;

            /**
             * Write frame data.
             */
            virtual void write( ) = 0;

        protected:
        
            // Nothing protected
        
        private:

            // Nothing private

        }; // End class PipelineSubsystem
    } // End namespace monitor
} // End namespace carma
            
#endif // End #ifndef CARMA_MONITOR_PIPELINESUBSYSTEM_H
