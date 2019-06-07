// $Id: SelfCalStage.h,v 1.2 2013/05/15 15:23:00 abeard Exp $

#ifndef CARMA_PIPELINE_UTIL_SELFCALSTAGE_H
#define CARMA_PIPELINE_UTIL_SELFCALSTAGE_H


#include "carma/monitor/PipelineMonitorInput.h"
#include "carma/pipeline/pipelineUtils.h"
#include "carma/pipeline/Stage.h"
#include "carma/services/Selfcal.h"
#include "carma/util/PthreadMutex.h"

#include <map>
#include <set>
#include <utility>

namespace carma {

    namespace correlator {
        namespace lib {
            class CorrelatorBaseline;
            class CorrelatorData;
        } // namespace lib
    } // namespace correlator

    namespace monitor {
        class AstroSubsystem;
        class PipelineMonitorInput;
        class PipelineSubsystem;
        class Input;
        class SelfCal;
    }
    
    namespace pipeline {

        class TsysStage;

        class SelfCalStage : public Stage {
        public:

            /**
             * Constructor.
             * @param monitor Reference to initialized monitor subsystem.
             * @param tsys Reference to TsysStage object.
             */
            SelfCalStage(
                    carma::monitor::PipelineSubsystem& monitor,
                    const carma::pipeline::TsysStage& tsys,
                    carma::monitor::AstroSubsystem & astro,
                    const carma::monitor::PipelineMonitorInput & plmi,
                    carma::pipeline::PipelineType plType );

            /**
             * Destructor.
             */
            virtual ~SelfCalStage();

            /**
             * Set reference antenna number.
             */
            void setReferenceAntNo( int antNo );

        private:

            typedef int SidebandIndex;
            typedef int AntIndex;
            typedef ::std::vector< carma::services::Complex > ComplexVector; 
            typedef ::std::vector< double > SnrVector;
            typedef ::std::set< int > AntSet;
            typedef ::std::pair< int, carma::monitor::PolType > AstroBandPolPair;

            class SelfCalSidebandInfo {
            public:

                SelfCalSidebandInfo( );

                SelfCalSidebandInfo( int antennas, int maxIterations, bool usb,
                        carma::monitor::SelfCal * mon,
                        SelfCalStage * mom );

                SelfCalSidebandInfo( const SelfCalSidebandInfo & rhs );
                SelfCalSidebandInfo & 
                operator=( const SelfCalSidebandInfo & rhs );

                void reset( ); // Reset for next frame/integration 

                void setVis( int antenna1No, int antenna2No, 
                        const std::complex< float > & avgVis,
                        double weight );

                void setBandPol( int bandNo, carma::monitor::PolType pol );

                AstroBandPolPair getBandPol() const;

                void calculateSelfCal( double rootbtau ); 

                void fillMonitorData( );

                std::pair< bool, ComplexVector > getVis( ) const;

                SnrVector getSnr( ) const;

                void setRefAnt( int antNo );

            private:

                carma::services::Selfcal  selfCal_;
                bool                      solutionValid_; 

                ComplexVector             vis_; // Indexed by input index
                ComplexVector             visErrors_; // Ditto
                SnrVector                 snr_; // Ditto

                AntSet                    seenAnts_;
                int                       referenceAntennaNo_;
                bool                      explicitRefAnt_;
                carma::util::PthreadMutex refAntMutex_; 

                // Note the following change dynamically based on setVis
                int                       bandNo_;
                carma::monitor::PolType   pol_; 

                const int                 nAnts_;
                const int                 maxIter_;
                const bool                usb_; // usb if true lsb if false.
                carma::monitor::SelfCal * mon_;
                SelfCalStage *            mom_;
            };

            typedef ::std::pair< SelfCalSidebandInfo, 
                    SelfCalSidebandInfo > UsbLsbInfoPair;
            typedef ::std::vector< UsbLsbInfoPair > SelfCalInfoVec;
            typedef ::std::map< AstroBandPolPair, 
                    SelfCalInfoVec::size_type > AstroBandPolInfoIdxMap;

            void
                preprocess( const carma::correlator::lib::CorrelatorDataPtr cd ); 

            void
                processBand( carma::correlator::lib::CorrelatorBand * cb );

            carma::correlator::lib::CorrelatorDataPtr
                postprocess( carma::correlator::lib::CorrelatorDataPtr cd );

            void fillMonitorData( );

            void processBaseline( 
                    int bandNumber,
                    const carma::correlator::lib::CorrelatorBaseline & baseline,
                    std::set< SelfCalInfoVec::size_type > & seenInfos,
                    double & iTime );

            void writeChannelAvgsToMonitorSystem( );

            double calculateWeight( int input1Num, 
                    int input2Num, 
                    int bandNum,
                    bool usb,
                    double intTime ) const;

            double calculateSystemNoise( double rootbtau,
                    int inputNum,
                    int bandNum,
                    bool usb ) const;

            void zeroSelfcalSolutions( );

            SelfCalInfoVec::size_type
            getInfoIdx( int astroBandNo, carma::monitor::PolType pol ) const;

            SelfCalInfoVec selfCalInfo_;

            carma::monitor::PipelineSubsystem & mon_;
            carma::monitor::AstroSubsystem & astroMonitor_;
            AstroBandPolInfoIdxMap bandPolToInfoIdx_;
            const carma::pipeline::TsysStage & tsys_;
            const carma::monitor::PipelineMonitorInput & plmi_;
            const int maxIterations_;
            const int nBands_;
            const int nAnts_;
            unsigned errors_;
            const carma::pipeline::PipelineType plType_;

      }; // End class SelfCalStage

  } // End namespace pipeline
} // End namespace carma
#endif // End #ifndef CARMA_PIPELINE_UTIL_SELFCALSTAGE_H
