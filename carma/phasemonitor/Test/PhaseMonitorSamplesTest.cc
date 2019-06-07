/** 
 * @file carma/phasemonitor/Test/PhaseMonitorSamplesTest.cc
 *
 * $Id: PhaseMonitorSamplesTest.cc,v 1.9 2013/02/06 20:07:31 abeard Exp $
 *
 * CppUnit test fixture for carma::phasemonitor::PhaseMonitorSamples 
 * For these tests we cannot use == since
 * we are testing floating point numbers. Therefore we test
 * that the normalized difference between the expected
 * answer and the returned answer is less than
 * numeric_limits.epsilon(), which is ~10^-7 for floats.
 *
 * Author: Colby Gutierrez-Kraybill
 * Version: $Revision: 1.9 $
 * $Date: 2013/02/06 20:07:31 $
 *
 */
#include <cmath>
#include <limits>
#include <iomanip>
#include <iostream>

#include <math.h>

#include "carma/phasemonitor/exceptions.h"
#include "PhaseMonitorSamplesTest.h"

using namespace carma::phasemonitor;
using namespace carma::util;
using namespace CppUnit;
using namespace std;

#define PMSCPPUNIT_ASSERT( A, B ) \
  do { \
    cout << "  asserting " << #A << "=" << B; \
    _normalizedDiff = fabsl( (A - B) / A ); \
    cout << " (residual: " << _normalizedDiff << " < " << _epsilon << ")" << endl;\
    CPPUNIT_ASSERT(  _normalizedDiff < _epsilon );\
  } while (0)

void PhaseMonitorSamplesTest::setUp() 
{   

  _tPMS = new PhaseMonitorSamples( "/tmp/" );

  // float that is not zero.  
  numeric_limits<float> floatLimits;
  _epsilon = floatLimits.epsilon();
}

void PhaseMonitorSamplesTest::tearDown() 
{   
  //  delete _tPMS;
}

void PhaseMonitorSamplesTest::testProcess()
{
  cout << "PhaseMonitorSamplesTest::testProcess()" << endl;

  int n = PhaseMonitorWorker::_totalPhaseSamples;
  int sE = 0;
  time_t tS[n];
  float sP[n], gP[n], aSW[n], aNE[n], skyRMS, groundRMS;

  // Hardcoded epoch seconds because we need to have
  // reproducability for the process method below...
  tS[0] = 1164861620;

  for ( int i = 0; i < n; i++ )
  {
    tS[i] = tS[0]+1;
    sP[i] = sin(tS[i]);
    gP[i] = cos(tS[i]);
    aSW[i] = sin(tS[i]);
    aNE[i] = cos(tS[i]);
  }

  float T = 25.0;
  double sMJD = Time::computeMJD( tS[0] );
  cout << setprecision(12);
  cout << "  computeMJD( " << tS[0] << "): " << sMJD << endl;

  _tPMS->process( n, sMJD, tS, skyRMS, groundRMS, sP, gP, aSW, aNE, sE, T );

  // loosen up epsilon for this check...
  float oe = _epsilon;
  _epsilon = 1e-01;
  cout << " skyRMS: " << skyRMS << " groundRMS: " << groundRMS << endl;
  PMSCPPUNIT_ASSERT( skyRMS, 1.1529324054718 );
  PMSCPPUNIT_ASSERT( groundRMS, 0.627636551856995 );
  _epsilon = oe;
}

void PhaseMonitorSamplesTest::testPMSECopyAndString()
{
  cout << "PhaseMonitorSamplesTest::testPMSECopyAndString()" << endl;

  cout
    << " Testing copy form of PhaseMonitorSamplesException c'tor" << endl;
  bool caught = false;
  ostringstream test("PhaseMonitorSamplesException Testing!");
  try
  {
    throw CARMA_EXCEPTION( PhaseMonitorSamplesException, test );
    cout << "  Not caught!" << endl;
  } catch ( PhaseMonitorSamplesException &pmse )
  {
    cout << "  Caught! what() - " << pmse.what() << endl;
    PhaseMonitorSamplesException testpmse( pmse );
    cout << "   C'tor by copy: " << pmse.what() << endl;
    caught = true;
  }


  CPPUNIT_ASSERT( caught );
}


