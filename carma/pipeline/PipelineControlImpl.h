#ifndef CARMA_PIPELINE_PIPELINECONTROLIMPL_H
#define CARMA_PIPELINE_PIPELINECONTROLIMPL_H

#include <string>
#include <tao/Basic_Types.h>

namespace carma {

namespace util {
    class SeqShort;
}

namespace pipeline {

    class BlankFlag;
    class CoherenceStage;
    class Decimator;
    class Integrator;
    class Publisher;
    class SelfCalStage;
    class TsysStage;
    class VisBrickWriter;

    class PipelineControlImpl { 
    public:

        PipelineControlImpl(
            carma::pipeline::CoherenceStage & coherence,
            carma::pipeline::BlankFlag & blankFlag,
            carma::pipeline::Integrator & integrator,
            carma::pipeline::VisBrickWriter & visbrick,
            carma::pipeline::Decimator & decimator,
            carma::pipeline::SelfCalStage & selfCal,
            carma::pipeline::TsysStage & tsys,
            carma::pipeline::Publisher & publisher );

        virtual ~PipelineControlImpl();

        virtual void resetTsys( const carma::util::SeqShort & carmaAntNoSeq );

        virtual void keepEndChannels(bool keep, CORBA::UShort astroBandNo );

        virtual void decimate(bool dec, CORBA::UShort astroBandNo );

        virtual void startIntegration( double intTime,
                                       CORBA::Long numRecords,
                                       double gap,
                                       CORBA::Boolean science,
                                       CORBA::Long seqNo );

        virtual void stopIntegration();

        virtual void resetTimeSinceLastIntegration( );

        void applyTsysCalibration( CORBA::Boolean apply );

        void applyFluxCalibration( CORBA::Boolean apply );
        
        virtual void activateCoherenceMonitor();
        virtual void deactivateCoherenceMonitor();

        virtual void activateDecimator();
        virtual void deactivateDecimator();

        virtual void activateTsys();
        virtual void deactivateTsys();

        virtual void activateBlankFlag();
        virtual void deactivateBlankFlag();

        virtual void activateSelfCal();
        virtual void deactivateSelfCal();

        virtual void activatePublisher();
        virtual void deactivatePublisher();

        virtual void logCalibrationOnce( CORBA::Short astroband );

        virtual void setReferenceAnt( CORBA::Short antNo );

    private:

        carma::pipeline::CoherenceStage & coherence_;
        carma::pipeline::BlankFlag & blankFlag_;
        carma::pipeline::Integrator & integrator_;
        carma::pipeline::VisBrickWriter & visbrick_;
        carma::pipeline::Decimator & decimator_;
        carma::pipeline::SelfCalStage & selfCal_;
        carma::pipeline::TsysStage & tsys_;
        carma::pipeline::Publisher & publisher_;

    }; // End class PipelineControlImpl

} // End namespace pipeline
} // End namespace carma
#endif // End #ifndef CARMA_PIPELINE_PIPELINECONTROLIMPL_H
