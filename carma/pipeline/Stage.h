// $Id: Stage.h,v 1.1 2011/08/18 23:25:52 abeard Exp $

#ifndef CARMA_PIPELINE_STAGE_H
#define CARMA_PIPELINE_STAGE_H

#include "carma/correlator/lib/CorrelatorDataPtr.h"
#include "carma/util/Time.h"

#include <memory>

/**
 * @file Stage.h
 * 
 * Tagged: Wed Jun 29 11:00:27 PDT 2005
 * 
 * @version: $Revision: 1.1 $, $Date: 2011/08/18 23:25:52 $
 * 
 * @author Rick Hobbs (original concept)
 * @author Andy Beard (rewrite)
 */
namespace carma {

    namespace correlator {
        namespace lib {
            class CorrelatorBand;
            class CorrelatorData;
        } // namespace lib
    } // namespace correlator 

    namespace monitor {
        class StageStats;
    } // namespace monitor

    namespace pipeline {

        /**
         * Base interface for a correlator pipeline stage. 
         */
        class Stage {
        public:

            /**
             * Destructor.
             */
            virtual ~Stage( );

            /**
             * Activate this stage.
             */
            void activate( );

            /**
             * Deactivate this stage.
             */
            void deactivate( );

            /**
             * Is this stage active?
             */
            bool isStageActive( ) const;

            /**
             * Get name of stage.
             */
            std::string getName( ) const;

            /**
             * Preprocess correlator data.
             * This routine is intended for routine processing done prior
             * to calibrating or processing band data.  Examples are 
             * updating internal state used for processing, updating 
             * antenna monitor information, calculating tsys 
             * coefficients etc.
             * @param cd Input correlator data to preprocess.
             */
            void preprocessCorrelatorData(
                    const carma::correlator::lib::CorrelatorDataPtr cd );

            /**
             * Process a single correlator band. 
             * This will be done in parallel with other bands.
             * @param cb Pointer to input correlator band to process - a 
             *  null ptr terminates the processing pipeline for this band.
             */
            void processCorrelatorBand( 
                    carma::correlator::lib::CorrelatorBand * cb );

            /**
             * Postprocess correlator data
             * This is the final processing step for a stage and is
             * called after processing all band data.  
             * @param cd Pointer to calibrated correlator data.
             * @note The input CorrelatorDataPtr is not const and hence
             *  can be swapped or nullified by the implementation.
             * @return True to terminate pipeline, false otherwise. 
             * @return CorrelatorData object to pass to the next stage - a
             *  null pointer terminates the postprocessing pipeline. 
             */
            carma::correlator::lib::CorrelatorDataPtr
            postprocessCorrelatorData( 
                carma::correlator::lib::CorrelatorDataPtr cd );

            /**
             * Fill per stage monitor data. 
             * It is important to note that this routine is not called
             * in parallel with any of the above processCorrelatorData
             * routines.  It is guaranteed to be called only after
             * postprocessCorrelatorData has returned.  The downside to this
             * is that the monitor data might not get included into the 
             * frame if processing takes longer than expected.  The upside
             * is that individual stages do not need to implement locks
             * around data being accessed by both this routine and process 
             * routines.
             */
            void fillStageMonitorData( );

            /**
             * Write internally stored and calculated stage stats.
             * Called externally to update monitor stage stats.
             */
            void fillMonitorStageStats( );

        protected:

            virtual void preprocess( 
                    const carma::correlator::lib::CorrelatorDataPtr cd ) = 0;

            virtual void
                processBand( carma::correlator::lib::CorrelatorBand * cb ) = 0;

            virtual carma::correlator::lib::CorrelatorDataPtr
                postprocess( carma::correlator::lib::CorrelatorDataPtr cd ) = 0;

            /**
             * Fill in monitor data
             * Implementor should count on this being called from a separate
             * thread.  In particular do not implement a blocking call
             * or introduce the possibility of blocking on a mutex which is
             * waiting for a lengthy operation to complete in the processing
             * stage.
             */
            virtual void fillMonitorData( )  = 0;

            /**
             * Constructor
             * @param name Name of stage for logging purposes.
             * @see processBandData
             */
            explicit 
                Stage( carma::monitor::StageStats & stageStats,
                        const std::string & name );

            /**
             * Get correlator data frame count.
             * This is useful if a client needs to know which frame count
             * the data belongs to in order to retrieve corresponding
             * monitor data.
             */
            carma::util::frameType getDataFrameCount( ) const;

        private:

            Stage( const Stage & );
            Stage & operator=( const Stage & );

            struct Pimpl;
            const ::std::auto_ptr<Pimpl> pimpl_;

        }; // End class Stage

    } // namespace pipeline
} // namespace carma

#endif // #ifndef CARMA_PIPELINE_STAGE_H
