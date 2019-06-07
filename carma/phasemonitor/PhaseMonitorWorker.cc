/*
 * $Id: PhaseMonitorWorker.cc,v 1.21 2013/02/06 20:07:29 abeard Exp $
 */

#include "carma/phasemonitor/PhaseMonitorWorker.h"

#include "carma/monitor/PhaseMonitorSubsystem.h"
#include "carma/phasemonitor/AntennaParameters.h"
#include "carma/phasemonitor/exceptions.h"
#include "carma/phasemonitor/PhaseMonitorDevice.h"
#include "carma/phasemonitor/PhaseMonitorSamples.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/FrameAlignedTimer.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedStopWatchTimer.h"
#include "carma/util/SimpleStatisticsAccumulators.h"
#include "carma/util/StopWatch.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

#include <boost/thread.hpp>
#include <log4cpp/Category.hh>

using namespace std;
using namespace log4cpp;
using namespace boost;
using namespace carma::util;
using namespace carma::monitor;
using namespace carma::phasemonitor;

PhaseMonitorWorker::PhaseMonitorWorker(
    string outDir,
    PhaseMonitorSubsystem * mon,
    PhaseMonitorDevice & dev,
    const AntennaParameters & params,
    PhaseMonitorSamples & samples ) :
  _outDir( outDir ),
  _log( Program::getLogger() ),
  _mon( mon ),
  _phdev( dev ),
  _phparams( params ),
  _phsamples( samples ),
  _firstPoint( true ),
  _lastGoodPhaseSW( 0.0 ),
  _lastGoodPhaseNE( 0.0 ),
  _offV( params.getVoltageOffsets() ),
  _rotCos( params.getCosTerms() ),
  _rotSin( params.getSinTerms() ),
  _scale( params.getScales() )
{
    const string oneSecFileName( outDir + "/1sec.new" );
    _oneSecData.open( oneSecFileName.c_str(), 
            ofstream::out | ofstream::app );
    if ( !_oneSecData.good() ) 
        throw CARMA_ERROR( "Unable to open " + oneSecFileName ); 
}

void
PhaseMonitorWorker::operator()()
try {
    CPTRACE( Trace::TRACE1, "  PhaseMonitorWorker::operator()()" );

    // The old version of phasemonitor ran with a specialized
    // one second wait routine.  This version relies on the
    // .5 second tick of a frame aligned timer. Waking
    // up every second tick to keep a phase of 1Hz in data
    // taking
    long tick = 0; // will roll over in 68 years...
    unsigned int consecutiveErrors = 0;
    unsigned int identicalVoltageErrors = 0;
    unsigned int badVoltageErrors = 0;
    unsigned int invalidResponseErrors = 0;
    unsigned int serialCommErrors = 0;
    unsigned int otherErrors = 0;
    int atodreconfigs = 0;
    int sampleErrors = 0;
    int n = 0;

    float skyPhase[_totalPhaseSamples], groundPhase[_totalPhaseSamples],
          ampSW[_totalPhaseSamples], ampNE[_totalPhaseSamples],
          swPhaseDegrees = 0., nePhaseDegrees = 0.,
          compORS[4], rawVolts[4], skyRMS = 0., groundRMS = 0.;
    time_t sampleTimes[_totalPhaseSamples];
    time_t lastSampleTime = 0;

    if ( !_mon ) 
        throw CARMA_EXCEPTION( PhaseMonitorWorkerException, 
            "Monitor system is null but you're running in daemon mode "
            "which writes to the monitor system." );

    MonitorPointAbstime &mTimestamp = _mon->timestamp();
    MonitorPointInt &mSkyPhase = _mon->skyPhase();
    MonitorPointFloat &mSkyRMS = _mon->skyRMS();
    MonitorPointInt &mSkyPath = _mon->skyPath();
    MonitorPointInt &mGroundPhase = _mon->groundPhase();
    MonitorPointFloat &mGroundRMS = _mon->groundRMS();
    MonitorPointInt &mGroundPath = _mon->groundPath();
    MonitorPointFloat &mBoxTemp = _mon->instrument().boxTemp();
    MonitorPointInt &mSampleErrors = _mon->instrument().sampleErrors();
    PhaseMonitorSubsystem::Instrument &mInstrument = _mon->instrument();
    PhaseMonitorSubsystem::Arm &mNeArm = _mon->neArm();
    PhaseMonitorSubsystem::Arm &mSwArm = _mon->swArm();

    util::FrameAlignedTimer _framer( 0 );
    carma::util::DoubleStatAccumulator readStatsAccumulator;
    carma::util::StopWatch stopwatch( StopWatch::WALL_CLOCK );
    
    int skipped;

    const time_t _startSecond = time(NULL);
    float _boxTempC = 0.;
    double sampleMJD = Time::MJD();
    const double _startMJD( Time::MJD() );

    while ( true )
    {
        // Start out by getting temp info...
        // This will get sampled once every 10 minutes
        try { 
            _boxTempC = _phdev.queryTemperatureC();
        } catch (...) {
            // Stifle
        }

        skipped = 0;

        CPTRACE( Trace::TRACE2, "   Resetting FrameAlignedTimer" );
        _framer.ResetNextFireTime();

        // Start building up samples... By default we collect 10 minutes worth.
        while ( n < _totalPhaseSamples )
        {
            CPTRACE( Trace::TRACE3, "*SLEEP* Waiting for next fire" );
            boost::this_thread::interruption_point();
            _framer.WaitForNextFireTime();
            boost::this_thread::interruption_point();
            CPTRACE( Trace::TRACE3, "*POW*  Got Framer Fired Timer" );

            if ( tick++ % 2 == 0 ) { // Only sample once a second

                try
                {
                    sampleTimes[n] = time(NULL) - _startSecond;
                    skipped = ( lastSampleTime == 0 ? 0 :
                                sampleTimes[n] - lastSampleTime );

                    {
                        ScopedStopWatchTimer scopedwatch( stopwatch );

                        getPhases( skyPhase+n, groundPhase+n, ampSW+n,
                                   ampNE+n, compORS, rawVolts,
                                   swPhaseDegrees, nePhaseDegrees );
                    }
                    
                    readStatsAccumulator( stopwatch.getElapsedTime() );

                    // Now log to local copy of data
                    writeOneSecDataToFile( Time::MJD(), skyPhase[n], groundPhase[n],
                            compORS, skipped, consecutiveErrors );

                    lastSampleTime = sampleTimes[n]; 
                    consecutiveErrors = 0;
                    n++; // only increase in the event of a good sample..

                } catch (...) {

                    try {
                        // ADB: As a general rule I frown on using exception 
                        // handling for normal program flow. In this case
                        // however, to avoid a complete overhaul, I'm keeping 
                        // the original logic and expanding on it with different
                        // exceptions for different types of errors.
                        const string err( getStringForCaught() );
                        CARMA_CPTRACE( Trace::TRACE1, err );
                        programLogErrorIfPossible( err );
                        ++sampleErrors;
                        if ( ++consecutiveErrors > 50 ) {
                            if ( ++atodreconfigs <= 2 ) {
                                programLogWarnIfPossible( "Reconfiguring AtoD." );
                                _phdev.AtoDSetup();
                                ++atodreconfigs;
                                consecutiveErrors = 0;
                           } else { 
                                throw CARMA_ERROR( "100 too many errors in a row..." );
                           }
                        }

                        throw;
                    } catch ( const IdenticalVoltageException & ) {
                        ++identicalVoltageErrors;
                    } catch ( const BadVoltageException & ) {
                        ++badVoltageErrors;
                    } catch ( const InvalidResponseException & ) {
                        ++invalidResponseErrors;
                    } catch ( const SerialCommException & ) {
                        ++serialCommErrors;
                    } catch ( const PhaseMonitorDeviceException & ) {
                        ++otherErrors;
                    } // Anything else bubbles.
                } 
            } // if ( tick++ % 2 == 0 ) 

            // Fill out monitor info
            int r = (n > 0 ? n-1 : 0 );

            mTimestamp.setValue( sampleMJD );
            mSkyRMS.setValue( skyRMS );
            mGroundRMS.setValue( groundRMS );
            mSkyPath.setValue( static_cast< int >( _phsamples.getCurrentSkyPath() ) );
            mGroundPath.setValue( static_cast< int >( _phsamples.getCurrentGroundPath() ) );
            mSkyPhase.setValue( static_cast< int >( skyPhase[r] ) );
            mGroundPhase.setValue( static_cast< int >( groundPhase[r] ) );
            mSampleErrors.setValue( sampleErrors );
            mBoxTemp.setValue( _boxTempC );

            mSwArm.amplitude().setValue( ampSW[r] );
            mSwArm.phase().setValue( swPhaseDegrees );
            mSwArm.rawQ().setValue( rawVolts[0] );
            mSwArm.rawI().setValue( rawVolts[1] );
            mSwArm.calibrationQoffsetV().setValue( _offV[0] );
            mSwArm.calibrationIoffsetV().setValue( _offV[1] );
            mSwArm.calibrationEllipseAngle().setValue( _rotCos[0] );
            mSwArm.calibrationQScale().setValue( _scale[0] );
            mSwArm.calibrationIScale().setValue( _scale[1] );
            mSwArm.calibratedQ().setValue( compORS[0] );
            mSwArm.calibratedI().setValue( compORS[1] );

            mNeArm.amplitude().setValue( ampNE[r] );
            mNeArm.phase().setValue( nePhaseDegrees );
            mNeArm.rawQ().setValue( rawVolts[2] );
            mNeArm.rawI().setValue( rawVolts[3] );
            mNeArm.calibrationQoffsetV().setValue( _offV[2] );
            mNeArm.calibrationIoffsetV().setValue( _offV[3] );
            mNeArm.calibrationEllipseAngle().setValue( _rotCos[2] );
            mNeArm.calibrationQScale().setValue( _scale[2] );
            mNeArm.calibrationIScale().setValue( _scale[3] );
            mNeArm.calibratedQ().setValue( compORS[2] );
            mNeArm.calibratedI().setValue( compORS[3] );

            mInstrument.atodReconfigs().setValue( atodreconfigs );
            mInstrument.identicalVoltageErrors().setValue( identicalVoltageErrors );
            mInstrument.badVoltageErrors().setValue( badVoltageErrors );
            mInstrument.invalidResponseErrors().setValue( invalidResponseErrors );
            mInstrument.serialCommErrors().setValue( serialCommErrors );
            mInstrument.otherErrors().setValue( otherErrors );
            mInstrument.voltageReadLatency().setValue( stopwatch.getElapsedTime() * 1000 );

        } // while n < _totalsamples

        // Write out samples file
        _phsamples.process( n, _startMJD, sampleTimes, skyRMS, groundRMS,
                skyPhase, groundPhase, ampSW, ampNE, sampleErrors, _boxTempC );

        sampleMJD = Time::MJD();

        writeSampleTimeToFile( );

        n = sampleErrors = 0;

        // Log some stats
        ostringstream statsOss;
        statsOss << "Phase monitor query stats for last 10min sample (in seconds): "
            << "max: " << accumulators::max( readStatsAccumulator ) << ", "
            << "min: " << accumulators::min( readStatsAccumulator ) << ", "
            << "mean: " << accumulators::mean( readStatsAccumulator ) << "+/-" 
            << ::sqrt(::fabs(accumulators::variance( readStatsAccumulator )));
        programLogInfoIfPossible( statsOss.str() );
            
        readStatsAccumulator =  carma::util::DoubleStatAccumulator();
    }
} catch ( boost::thread_interrupted & ) {
    return;
} catch (...) {
    const string err( getStringForCaught() );
    programLogErrorIfPossible( err );
}

void
PhaseMonitorWorker::replay() 
{
    float skyPhase[_totalPhaseSamples], groundPhase[_totalPhaseSamples],
          ampSW[_totalPhaseSamples], ampNE[_totalPhaseSamples],
          swPhaseDegrees = 0., nePhaseDegrees = 0.,
          compORS[4], rawVolts[4], skyRMS = 0., groundRMS = 0.;
    time_t sampleTime = 0, lastSampleTime = 0;
    time_t sampleTimes[_totalPhaseSamples];
    const time_t startCtime( time(NULL) );
    const double startMJD( Time::MJD() );
    const double ONE_SEC_MJD( 1.0/86400.0 );
    float _boxTempC = 0.;

    bool done( false );
    while ( !done ) { 
    
        int n = 0;
        int sampleErrors = 0;
        int consecutiveErrors = 0;

        while ( n < _totalPhaseSamples && !done )
        {
            try { 

                getPhases( skyPhase+n, groundPhase+n, ampSW+n,
                        ampNE+n, compORS, rawVolts,
                        swPhaseDegrees, nePhaseDegrees );

                // We're not on a timer for replay so just count up.
                sampleTimes[n] = ++sampleTime - startCtime;
                const int skipped = ( lastSampleTime == 0 ? 0 : 
                    sampleTimes[n] - lastSampleTime );

                lastSampleTime = sampleTimes[n]; 

                writeOneSecDataToFile( startMJD + lastSampleTime * ONE_SEC_MJD, skyPhase[n], groundPhase[n],
                        compORS, skipped, consecutiveErrors );
                consecutiveErrors = 0;
                ++n;
            } catch (...) { 

                try {
                    CPTRACE( Trace::TRACE5, getStringForCaught() );
                    throw;
                } catch ( const PhaseMonitorDeviceReplayException & ) {
                    done = true;
                } catch ( const IdenticalVoltageException & ) {
                    ++sampleErrors;
                    ++consecutiveErrors;
                } catch ( const BadVoltageException & ) {
                    ++sampleErrors;
                    ++consecutiveErrors;
                } catch ( const InvalidResponseException & ) {
                    ++sampleErrors;
                    ++consecutiveErrors;
                } catch ( const SerialCommException & ) {
                    ++sampleErrors;
                    ++consecutiveErrors;
                } catch ( const PhaseMonitorDeviceException & ) {
                    ++sampleErrors;
                    ++consecutiveErrors;
                } // Anything else bubbles.

            }

        } // n < _totalPhaseSamples 
    
        // Write out samples file
        _phsamples.process( n, 
                startMJD + lastSampleTime * ONE_SEC_MJD, sampleTimes, 
                skyRMS, groundRMS, skyPhase, groundPhase, 
                ampSW, ampNE, sampleErrors, _boxTempC );

    } // while ( !done )
}

/*
 * Reads the voltages from the A/D; checks for errors; removes offsets; 
 * maps the elliptical tracks traced out by the correlator voltages into 
 * circles; calculates phase; unwraps phase; takes sum and difference of the
 * two measured phases to obtain ground path and sky path components;
 * Returns difference and sum phase (in degrees), the amplitudes of the 
 * signals from the SW and NE dishes (in Volts) and the corrected voltages
 * measured by the 4 A/D channels (In-phase SW, Quad. SW, In-phase NE, 
 * Quad. NE)
 */
void PhaseMonitorWorker::getPhases( float *diffPhase, float  *sumPhase,
                                    float *ampSW, float *ampNE, 
                                    float *compORS, float *rawVolts,
                                    float & swPhaseDegrees, float & nePhaseDegrees )
{
  CPTRACE( Trace::TRACE5, "  getPhases(...)" );
  _phdev.queryVoltages( rawVolts );

  /*
   * The in-phase and quadrature voltages should trace out a circle centered on
   * zero as the phase varies. They don't. Instead they trace an ellipse
   * whose center is offset from the origin and with principal axes rotated 
   * with respect to voltage axes. The reasons for this are not clear. The 
   * offsets, rotations and scalings carried out below convert the ellipse to 
   * a circle centered on the origin.    
   */
  float rawOff[4];
  for ( int i = 0; i < 4; i++ )
    rawOff[i] = rawVolts[i] + _offV[i];

  CPTRACE( Trace::TRACE7, "    rawOff[0]:" << rawOff[0] << " rawOff[1]:"
      << rawOff[1] << " rawOff[2]:" << rawOff[2] << " rawOff[3]:" << rawOff[3] );

  /* Now rotate the ellipse so that major and minor axes are vertical and 
     horizontal. Angles are in radians (positive angles cw of +y axis) */
  float rawOffRot[4];
  rawOffRot[0] = rawOff[0] * cos(_rotCos[0]) + rawOff[1] * sin(_rotSin[0]) ;
  rawOffRot[1] = rawOff[1] * cos(_rotCos[1]) - rawOff[0] * sin(_rotSin[1]) ;
  rawOffRot[2] = rawOff[2] * cos(_rotCos[2]) + rawOff[3] * sin(_rotSin[2]) ;
  rawOffRot[3] = rawOff[3] * cos(_rotCos[3]) - rawOff[2] * sin(_rotSin[3]) ;

  CPTRACE( Trace::TRACE7, "    rawOffRot[0]:" << rawOffRot[0] << " rawOffRot[1]:"
      << rawOffRot[1] << " rawOffRot[2]:" << rawOffRot[2] << " rawOffRot[3]:" << rawOffRot[3] );

  /* And scale the axes to make a circle */
  float rawOffRotScale[4];
  for ( int i = 0; i < 4; i++ )
    compORS[i] = rawOffRotScale[i] = rawOffRot[i] * _scale[i];

  /* Calculate phases */
  const float DEG_PER_RAD( 57.296 );
  swPhaseDegrees = atan2(rawOffRotScale[0], rawOffRotScale[1]) * DEG_PER_RAD;
  nePhaseDegrees = atan2(rawOffRotScale[2], rawOffRotScale[3]) * DEG_PER_RAD;

  if ( ! _firstPoint )
  {
    try
    {
      phaseJumpCheck( swPhaseDegrees, _lastGoodPhaseSW );
    }
    catch ( PhaseMonitorWorkerException &pmwe )
    {
      ostringstream err;
      err << "Phase jump of " << swPhaseDegrees - _lastGoodPhaseSW 
	<< " degrees from SW antenna";

      _log << Priority::INFO << err.str();
      CPTRACE( Trace::TRACE1, err.str() );
    }

    try
    {
      phaseJumpCheck( nePhaseDegrees, _lastGoodPhaseNE );
    }
    catch ( PhaseMonitorWorkerException &pmwe )
    {
      ostringstream err;
      err << "Phase jump of " << nePhaseDegrees - _lastGoodPhaseNE 
	<< " degrees from NE antenna";

      _log << Priority::INFO << err.str();
      CPTRACE( Trace::TRACE1, err.str() );
    }
  } // ! _firstPoint

  // Take sum and difference
  *diffPhase = ( swPhaseDegrees - nePhaseDegrees );
  *sumPhase  = ( swPhaseDegrees + nePhaseDegrees );

  _lastGoodPhaseSW = swPhaseDegrees;
  _lastGoodPhaseNE = nePhaseDegrees;

  // Calculate amplitudes of corrected signals in Volts
  *ampSW = hypot( rawOffRotScale[0], rawOffRotScale[1] );
  *ampNE = hypot( rawOffRotScale[2], rawOffRotScale[3] );

  CPTRACE( Trace::TRACE7, "    *diffPhase:" << *diffPhase << " sumPhase:"
      << *sumPhase << " phaseSW:" << swPhaseDegrees << " phaseNE:" << nePhaseDegrees 
      << " *ampSW:" << *ampSW << " *ampNE:" << *ampNE );
  _firstPoint = false;
}

/* Take out phase wraps by comparing current value to previous value
   If the phase change is larger than 60 degrees assume the point is bad */
void PhaseMonitorWorker::phaseJumpCheck( float &phase, float &lastPhase )
{
  phase += floor( (lastPhase - phase + 180.) / 360. ) * 360.;

  if ( fabs( phase - lastPhase ) > 60. )
    throw CARMA_EXCEPTION( PhaseMonitorWorkerException, "" );
}

void 
PhaseMonitorWorker::writeOneSecDataToFile( const double mjd, 
                                           const float skyPhase,
                                           const float groundPhase,
                                           const float * const calibratedVolts,
                                           const int skipped, 
                                           const int errors )
{
    CPTRACE( Trace::TRACE1, "Writing one sec data to file " );
    const int skyPathDiff = static_cast< int >( 
        skyPhase * (PhaseMonitorSamples::_lambda / 720.) );
    const int groundPathDiff = static_cast< int >( 
        groundPhase * (PhaseMonitorSamples::_lambda / 720.) );

    _oneSecData.precision( 15 );
    _oneSecData.width( 15 );
    _oneSecData << mjd;
    _oneSecData.precision( 7 );
    _oneSecData << " " << skyPathDiff << " " << groundPathDiff;
    _oneSecData.precision(5);
    _oneSecData << " " << calibratedVolts[0] 
                << " " << calibratedVolts[1]
                << " " << calibratedVolts[2] 
                << " " << calibratedVolts[3]
                << " " << skipped << " " << errors << endl;
}

void 
PhaseMonitorWorker::writeSampleTimeToFile( )
{
    // Write out sample time stamp
    const string timeFileName( _outDir + "/last_sample_time" );
    ofstream _timeFile;
    _timeFile.open( timeFileName.c_str(),
                    ofstream::out | ofstream::trunc );

    if ( !_timeFile.good() ) 
        throw CARMA_ERROR( "Unable to open " + timeFileName ); 

    time_t T;
    time(&T);
    _timeFile << ctime(&T) << endl;
    _log << Priority::INFO << "Updating '" << _outDir << "/last_sample_time' with: '"
        << ctime(&T);
    _timeFile.flush();
    _timeFile.close();
}

ostream& operator<<( ostream& os, PhaseMonitorWorker &worker )
{
  os << "PhaseMonitorWorker()";

  return os;
}

