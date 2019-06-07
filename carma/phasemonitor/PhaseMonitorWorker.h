#ifndef CARMA_PHASEMONITOR_PHASEMONITORWORKER_H
#define CARMA_PHASEMONITOR_PHASEMONITORWORKER_H

#include <fstream>
#include <iostream>
#include <string>
#include <vector>

namespace log4cpp {
    class Category;
}

namespace carma {

namespace monitor {
    class PhaseMonitorSubsystem;
}

namespace phasemonitor {

    class AntennaParameters;
    class PhaseMonitorDevice;
    class PhaseMonitorSamples;

    class PhaseMonitorWorker {
    public:

        PhaseMonitorWorker( std::string outDir,
                            carma::monitor::PhaseMonitorSubsystem * mon,
                            PhaseMonitorDevice & dev, 
                            const AntennaParameters & params,
                            PhaseMonitorSamples & samples );

        void replay();

        void operator()();
        
        void getPhases( float *diffPhase, float  *sumPhase,
                        float *ampSW, float *ampNE, 
                        float *compORS, float *rawVolts,
                        float & swPhaseDegrees, float & nePhaseDegrees );

        void phaseJumpCheck( float &phase, float &lastPhase );

        static const int _totalPhaseSamples = 600;

    private:

        void writeOneSecDataToFile( double mjd, 
                                    float skyPhase, 
                                    float groundPhase,
                                    const float * const calibratedVolts,
                                    int skipped,
                                    int errors );

        void writeSampleTimeToFile( );

        std::string _outDir;
        log4cpp::Category & _log;
        carma::monitor::PhaseMonitorSubsystem * _mon;
        PhaseMonitorDevice & _phdev;
        const AntennaParameters & _phparams;
        PhaseMonitorSamples & _phsamples;
        bool _firstPoint;
        float _lastGoodPhaseSW, _lastGoodPhaseNE;
        const std::vector< float > _offV, _rotCos, _rotSin, _scale;

        std::ofstream _oneSecData;

    }; // class PhaseMonitorWorker

} } // carma::phasemonitor

::std::ostream& operator<<( ::std::ostream & os, 
                            ::carma::phasemonitor::PhaseMonitorWorker &worker );

#endif // CARMA_PHASEMONITOR_PHASEMONITORWORKER_H
