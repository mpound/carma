/*
 * @usage tTelemFile oscfreq=80.0
 * @key oscfreq 80.0 d Osc Freq.
 * @logger DEFAULT_FACILITY carma.bima.ComputeHarmonics
 */


// C++ Includes...
#include <iostream>
#include <sstream>

// CARMA Includes...
#include "carma/util/Program.h"
#include "carma/util/BaseException.h"

using namespace std;
using namespace carma::util;

int Program::main()
{
  try
  {
    ostringstream oss;
    cout << "This program computes xfreq's and reference MHz for a given osc freq" << endl;

    double oscfreq = getDoubleParameter( "oscfreq" );

    int mhmin = (int)((oscfreq - .05) / 12.5 + 1);
    int mhmax = (int)((oscfreq - .05) / 8.0);
    int nharm;
    double xGHz, refMHz;

    cout << "oscfreq: " << oscfreq << endl;
    for ( int m = mhmin; m <= mhmax; m++ )
    {
      xGHz = (oscfreq - .05) / m;
      nharm = (int)((xGHz - .01) / 1.105);
      refMHz = 1000. * (xGHz - .01) / nharm;
      cout << "xGHz=" << xGHz << " refMHz=" << refMHz << endl;
    }

  }
  catch ( const carma::util::BaseException & be )
  {
    cerr << be.what() << endl;
  }

  exit(1);
}

