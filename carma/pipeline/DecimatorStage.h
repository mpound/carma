// $Id: DecimatorStage.h,v 1.5 2013/05/15 15:23:00 abeard Exp $

#ifndef CARMA_PIPELINE_DECIMATORSTAGE_H
#define CARMA_PIPELINE_DECIMATORSTAGE_H

#include "carma/monitor/PipelineCommon.h"
#include "carma/pipeline/Stage.h"
#include "carma/pipeline/pipelineUtils.h"
#include "carma/util/FftwRealToRealPlanManager.h"

#include <pthread.h>
#include <set>

namespace carma {

namespace monitor {
    class PipelineSubsystem;
    class PipelineMonitorInput;
} // namespace monitor

namespace correlator {
namespace lib {
    class CorrelatorBand;
    class CorrelatorBaseline;
    class CorrelatorSideband;
    class CorrelatorData;
} // namespace lib
} // namespace correlator

namespace pipeline {

    class Decimator : public Stage {
    public:

        /**
         * Constructor.
         * @param monitor Reference to preconstructed monitor system.
         * @param parallel Process bands in parallel threads if true. 
         */
        Decimator( carma::monitor::PipelineSubsystem & monitor,
                   const carma::monitor::PipelineMonitorInput & plmi, 
                   carma::pipeline::PipelineType pipelineType );

        /**
         * Destructor.
         */
        virtual ~Decimator( );

        /**
         *  Set to true to keep end channels for all bands.
         */
        void keepEndChannels( bool keep );

        /**
         * Keep end channels for a particular band.
         * @param Astro Band Number (1 based).
         */
        void keepEndChannels( bool keep, int astroBandNo );

        /**
         * Turn decimation on or off for all bands.
         * @param on True to enable decimation, false otherwise. 
         */
        void decimation( bool on );

        /**
         * Turn decimation on or off for a particular band.
         * @param on True to enable decimation, false otherwise. 
         * @param Astro Band Number ( 1 based ).
         */
        void decimation( bool on, int astroBandNo );

        /**
         * Retrieve a set of default fftw sizes for wisdom
         * creation and testing.
         */
        static ::std::set< carma::util::FftwRealVector::size_type > 
            defaultFftwPlanSizes( ); 

    private:

        void
            preprocess(const carma::correlator::lib::CorrelatorDataPtr cd);

        void
            processBand( carma::correlator::lib::CorrelatorBand * cb );

        carma::correlator::lib::CorrelatorDataPtr 
            postprocess( carma::correlator::lib::CorrelatorDataPtr cd );

        void fillMonitorData( );

        void resetDecimationBandInfo( );

        struct DecimationBandInfo { 
            DecimationBandInfo();
            int expectedChans;
            int outputChans;
            int numLags;
            float outBandwidth;
            float outDeltaFreq;
            bool keepEndChannels;
            bool decimate;
            carma::monitor::Decimation::
                DecimateMonitorPointEnum::DECIMATE decimateType;
            carma::monitor::Decimation::
                WindowMonitorPointEnum::WINDOW windowType;
            carma::util::FftwRealToRealPlanManager fftwPlans;
            ::pthread_mutex_t mutex;
        };

        void calculateNewFrequencyParams( 
                float inBandwidth,
                DecimationBandInfo & decBand );

        bool
            inferInputChannelCount( 
                    const carma::correlator::lib::CorrelatorBand & band,
                    DecimationBandInfo & decBand );

        void
            processBaseline( 
                    carma::correlator::lib::CorrelatorBaseline & baseline,
                    DecimationBandInfo & decBand );

        void
            processSideband( 
                    carma::correlator::lib::CorrelatorSideband & sb,
                    DecimationBandInfo & decBand );

        void
            updateAdditionalSidebandParameters(
                    carma::correlator::lib::CorrelatorSideband & sb,
                    const DecimationBandInfo & decBand );

        void
            cacheHannFactorsIfNotAlready( int numLags );

        typedef unsigned short astroBandNo;
        typedef ::std::map< astroBandNo, 
                DecimationBandInfo> DecimationInfoMap;

        DecimationInfoMap decimationInfo_;

        carma::monitor::PipelineSubsystem & monitorData_;

        const carma::pipeline::PipelineType plType_;
        const carma::monitor::PipelineMonitorInput & plmi_;

        // Keyed by number of lags. Invariant: first == vector.size().
        typedef std::map< int, std::vector< double > > HannFactorsMap;
        HannFactorsMap hannFactors_;

    }; // End class Decimator

} // End namespace pipeline
} // End namespace carma
#endif // End #ifndef CARMA_PIPELINE_DECIMATORSTAGE_H
