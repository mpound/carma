#ifndef CARMA_PHASEMONITORSAMPLES_H
#define CARMA_PHASEMONITORSAMPLES_H
/*
 * $Id: PhaseMonitorSamples.h,v 1.4 2013/02/06 20:07:29 abeard Exp $
 */


#include <math.h>
#include <fcntl.h>
#include <ctype.h>
#include <stdio.h> // for sprintf
#include <unistd.h>
#include <errno.h>
#include <termios.h>
#include <sys/time.h>

#include <iostream>
#include <string>
#include <iosfwd>
#include <fstream>

#include "carma/util/Trace.h"
#include "carma/util/Time.h"

#include "carma/phasemonitor/Maths.h"

namespace carma
{
  namespace phasemonitor
  {
    class PhaseMonitorSamples
    {
      public:
	PhaseMonitorSamples( std::string fileName );

	void process( int n, double startMJD,
	    time_t *sampleTimes,
	    float &skyRMS, float &groundRMS,
	    float *skyPhase, float *groundPhase,
	    float *ampSW, float *ampNE,
	    int errors, float boxTempC );
	float getTimeTrend( int i );
	float getSkyPathTrend( int i );
	float getGroundPathTrend( int i );
	float getCurrentSkyPath() { return getSkyPathTrend( _mostRecentSample ); };
	float getCurrentGroundPath() { return getGroundPathTrend( _mostRecentSample ); };

	int trendsCount() { return _ntrend; };

	static const float _lambda = 24200.0; // Wavelen@12.4GHz in microns

      private:
	std::string _fileName;
	std::ofstream _file;
	int _sampleCount;
    int _mostRecentSample;
	static const int _ntrend = 3;
	static const float _avgMultiplier = 0.9; // Smoothing time
    static const double _oneMJDsec = .00001157407;

	float _timeTrend[_ntrend];
	float _skyPathTrend[_ntrend], _groundPathTrend[_ntrend];
	float _runningSum, _runningWght, _runningAvg;

    };
  } // phasemonitor
} // carma

::std::ostream& operator<<( ::std::ostream& os,
    ::carma::phasemonitor::PhaseMonitorSamples &samples );

#endif // CARMA_PHASEMONITORSAMPLES_H
