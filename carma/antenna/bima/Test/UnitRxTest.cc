/** 
 * @file carma/antenna/bima/Test/AudioControlThreadTest.cc
 *
 * $Id: UnitRxTest.cc,v 1.5 2011/01/05 01:27:30 eml Exp $
 *
 * CppUnit test fixture for carma::antenna/bima::AudioControlThread
 * For these tests we cannot use == since
 * we are testing floating point numbers. Therefore we test
 * that the normalized difference between the expected
 * answer and the returned answer is less than
 * numeric_limits.epsilon(), which is ~10^-7 for floats.
 *
 * Author: Colby Gutierrez-Kraybill
 * Version: $Revision: 1.5 $
 * $Date: 2011/01/05 01:27:30 $
 *
 */
#include <cmath>
#include <limits>
#include <iostream>

#include <math.h>

#include "UnitRxTest.h"
#include "carma/util/StartPthread.h"
#include "carma/util/Program.h"

#include "carma/antenna/bima/SharedMemory.h"


#include <log4cpp/Category.hh>

using namespace carma::antenna::bima;
using namespace carma::util;
using namespace CppUnit;
using namespace std;

#define RXCPPUNIT_ASSERT( A, B ) \
  do { \
    cout << "  asserting " << #A << "=" << B; \
    _normalizedDiff = fabsl( (A - B) / A ); \
    cout << " (residual: " << _normalizedDiff << " < " << _epsilon << ")" << endl;\
    CPPUNIT_ASSERT(  _normalizedDiff < _epsilon );\
  } while (0)

void UnitRxTest::setUp() 
{   

  // unit is a special entry in the conf/antenna/bima/desc.tab
  Configuration config( "unit1", Program::getConfDir() );

  // Need to set up shared mem file here because regular
  // telemetry process won't be running
  SharedMemory *bimaShm = new SharedMemory( config.getAntenna().c_str() );
  bimaShm->init();

  config.setEmulate( true );

  _tRx = new Rx( config );
  _tLO = new LO( config );

  // float that is not zero.  
  numeric_limits<float> floatLimits;
  _epsilon = floatLimits.epsilon();

}

void UnitRxTest::tearDown() 
{   
  // Kill off the primary ACT thread
  delete _tRx;
}

void UnitRxTest::testRxConfig()
{
  cout << "UnitRxTest::testRxConfig()..." << endl;
  cout << *_tRx << endl;
  cout << " ...OK!" << endl;
}

void UnitRxTest::testSetLOTermAtten()
{
  cout << "UnitRxTest::testSetLOTermAtten()...";

  int setdB = 4;
  _tRx->setLOTermAtten( setdB );
  cout << " setdB:"<<setdB;
  int dB = _tRx->getLOTermAtten();
  cout << " dB:"<<dB;

  CPPUNIT_ASSERT( setdB == dB );

  cout << " <- OK!" << endl;
}

void UnitRxTest::testScanSIS()
{
#if 0
  cout << "UnitRxTest::testScanSIS()...";

  const int n = 10;
  int i = 1;
  float *vmxr = new float[n+1];
  float *imxr = new float[n+1];
  float *ifpwr = new float[n+1];

  _tRx->scanSIS( i, i, n, vmxr, imxr, ifpwr );

  for ( int j = 0; j < n; j++ )
  {
    float v = vmxr[j]+1.;
    RXCPPUNIT_ASSERT( v, 1. );
    float ir = imxr[j]+1.;
    RXCPPUNIT_ASSERT( ir, 1. );
    float f = ifpwr[j]+1.;
    RXCPPUNIT_ASSERT( f, 1. );
  }

  cout << "OK!" << endl;
#endif
}


void UnitRxTest::testTune()
{
  cout << "UnitRxTest::testTune()...";

  cout << " errors about the tuning itself can be ignored!" << endl;

  _tRx->tune( 85.0, true, true );

  cout << "OK!" << endl;
}

void UnitRxTest::testLockX()
{
  cout << "UnitRxTest::testLockX()... Should fail!" << endl;
  _tLO->lockX(9.0);
  double c = _tLO->getCommanded();
  RXCPPUNIT_ASSERT( c, 9.0 );
  c = _tLO->xBandIFLevel() + 1.;
  RXCPPUNIT_ASSERT( c, 1. );
  cout << " Will now intentially throw out of range error" << endl;
  bool caught = false;
  try
  {
    _tLO->lockX(50.);
    cout << "   Not caught!" << endl;
  }
  catch ( ErrorException &ee )
  {
    cout << "  Caught! what() - " << ee.what() << endl;
    caught = true;
  }
  CPPUNIT_ASSERT( caught );
  cout << "   OK!" << endl;
}


void UnitRxTest::testRelockLogic()
{
  cout << "UnitRxTest::testRelockLogic()..." << endl;

  _tRx->disableRelock();
  CPPUNIT_ASSERT( _tRx->isRelockEnabled() == false );

  _tRx->enableRelock();
  CPPUNIT_ASSERT( _tRx->isRelockEnabled() == true );
  CPPUNIT_ASSERT( _tRx->numTriesRelock() == 0 );

  _tRx->incRelock();
  CPPUNIT_ASSERT( _tRx->numTriesRelock() == 1 );
  CPPUNIT_ASSERT( _tRx->isRelockEnabled() == true );

  _tRx->incRelock();
  CPPUNIT_ASSERT( _tRx->numTriesRelock() == 2 );
  CPPUNIT_ASSERT( _tRx->isRelockEnabled() == true );

  _tRx->incRelock();
  CPPUNIT_ASSERT( _tRx->numTriesRelock() == 3 );
  CPPUNIT_ASSERT( _tRx->isRelockEnabled() == true );

  _tRx->incRelock();
  CPPUNIT_ASSERT( _tRx->numTriesRelock() == 3 );
  CPPUNIT_ASSERT( _tRx->isRelockEnabled() == false );

  cout << "   OK!" << endl;
}
