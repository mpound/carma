/*
 * $Id: PhaseMonitorSamples.cc,v 1.10 2013/02/06 20:07:29 abeard Exp $
 */

#include "carma/phasemonitor/PhaseMonitorSamples.h"
#include "carma/phasemonitor/exceptions.h"

using namespace std;
using namespace carma::util;
using namespace carma::phasemonitor;

#ifndef PMSTHROW
#define PMSTHROW( a ) \
  do { \
    ostringstream _errs; \
    _errs << a; \
    throw CARMA_EXCEPTION( PhaseMonitorSamplesException, _errs ); \
  } while (0)
#else
#error "PMSTHROW is already defined, it will probably break this code."
#endif

PhaseMonitorSamples::PhaseMonitorSamples( std::string fileName )
{
  CPTRACE( Trace::TRACE1, "PhaseMonitorSamples( '" << fileName << "' )" );

  _runningSum = _runningWght = _runningAvg = 0.0;

  _sampleCount = 0;
  _mostRecentSample = 0;
  _fileName = fileName;
  _file.open( _fileName.c_str(), ofstream::out | ofstream::app );

  if ( !_file.good() ) 
    throw CARMA_ERROR( "Unable to open " + _fileName + "." );
  
  for ( int i = 0; i < _ntrend; ++i ) {
    _timeTrend[i] = 0.;
    _skyPathTrend[i] = 0.;
    _groundPathTrend[i] = 0.;
  }
}

/*
 * Convert phases to path lengths in microns;
 * Calculate the mean time, paths and amplitudes over NSAMPLE 1 sec readings;
 * Fit a linear trend to the last NTREND sky path values to take out effect of
 * satellite motion; compute the rms for the last sample sky and ground paths;
 * Write values to sample.dat, and a few other files
 */
void PhaseMonitorSamples::process(
    int n, double startMJD, time_t *sampleTimes,
    float &skyRMS, float &groundRMS,
    float *skyPhase, float* groundPhase,
    float *ampSW, float *ampNE,
    int errors, float boxTempC )
{
  static bool startup = true;
  float skyPath[n], groundPath[n];

  CPTRACE( Trace::TRACE3, " PhaseMonitorSamples::process(): " );
  CPTRACE( Trace::TRACE3, "  n: " << n );
  CPTRACE( Trace::TRACE3, "  startMJD: " << startMJD  );

  CPTRACE( Trace::TRACE3, "   _sampleCount: " << _sampleCount );

  // Convert phases to path length in microns
  for ( int j = 0; j < n; j++ )
  {
    skyPath[j] = skyPhase[j] * _lambda / 720.;
    groundPath[j] = groundPhase[j] * _lambda / 720.;
    CPTRACE( Trace::TRACE3, "  sampleTimes["<<j<<"]: " << sampleTimes[j] );
  }

  // Determine the mean values
  _timeTrend[_sampleCount] = (float)Maths::mean( sampleTimes, n );
  _skyPathTrend[_sampleCount] = (float)Maths::mean( skyPath, n );
  _groundPathTrend[_sampleCount] = (float)Maths::mean( groundPath, n );
  float sampleAmpSW = (float)Maths::mean( ampSW, n );
  float sampleAmpNE = (float)Maths::mean( ampNE, n );

  CPTRACE( Trace::TRACE3,"  _timeTrend["<<_sampleCount<<"]: " << _timeTrend[_sampleCount] );
  CPTRACE( Trace::TRACE3,"  _skyPathTrend["<<_sampleCount<<"]: " << _skyPathTrend[_sampleCount] );
  CPTRACE( Trace::TRACE3,
      "  _groundPathTrend["<<_sampleCount<<"]: " << _groundPathTrend[_sampleCount] );
  CPTRACE( Trace::TRACE3,"  sampleAmpSW: " << sampleAmpSW );
  CPTRACE( Trace::TRACE3,"  sampleAmpNE: " << sampleAmpNE );

  float skyA = 0., skyB = 0.;
  int nfit;

  if ( startup )
   nfit = _sampleCount+1;
  else
    nfit = _ntrend;

  Maths::fitLine( _timeTrend, _skyPathTrend, nfit, skyA, skyB );

  CPTRACE( Trace::TRACE3, "  skyA: " << skyA << " skyB: " << skyB );

  // Now remove trend from current sky sample
  for ( int j = 0; j < n; j++ )
    skyPath[j] -= skyA * sampleTimes[j] + skyB;

  // Calculate remaining rms
  skyRMS = Maths::rms( skyPath, n );
  groundRMS = Maths::rms( groundPath, n );

  CPTRACE( Trace::TRACE3, "  skyRMS: " << skyRMS << " groundRMS: " << groundRMS );

  // Time for middle of sample in MJD
  double sampleTimeMJD =
    startMJD + (_timeTrend[_sampleCount] * _oneMJDsec);

  // Take care of running average
  _runningSum  = _runningSum  * _avgMultiplier + skyRMS;
  _runningWght = _runningWght * _avgMultiplier + 1.0;
  _runningAvg  = _runningSum / _runningWght;

  CPTRACE( Trace::TRACE3, "  _runningSum: " << _runningSum << " _runningWght: " << _runningWght
      << " _runningAvg: " << _runningAvg );

  _file.precision(14);

  _file << sampleTimeMJD << " " << (int)_skyPathTrend[_sampleCount]
    << " " << (int)_groundPathTrend[_sampleCount]
    << " " << (int)skyRMS << " " << (int)groundRMS
    << " " << sampleAmpSW << " " << sampleAmpNE
    << " " << errors << " " << _runningAvg << " " << boxTempC << endl;
  _file.flush();

  _mostRecentSample = _sampleCount;
  _sampleCount++;

  if ( _sampleCount == _ntrend )
  {
    startup = false;
    _sampleCount = 0;
  }

}


float PhaseMonitorSamples::getTimeTrend( int i )
{
  if ( i > -1 && i < _ntrend )
    return _timeTrend[i];
  else
    PMSTHROW( "Time Trend access out of range [0, " << _ntrend-1 << "]: " << i );
}

float PhaseMonitorSamples::getSkyPathTrend( int i )
{
  if ( i > -1 && i < _ntrend )
    return _skyPathTrend[i];
  else
    PMSTHROW( "Sky Path Trend access out of range [0, " << _ntrend-1 << "]: " << i );
}

float PhaseMonitorSamples::getGroundPathTrend( int i )
{
  if ( i > -1 && i < _ntrend )
    return _groundPathTrend[i];
  else
    PMSTHROW( "Ground Path Trend access out of range [0, " << _ntrend-1 << "]: " << i );
}

ostream& operator<<( ostream& os, PhaseMonitorSamples &dev )
{
  return os;
}

