/** 
 * @file carma/phasemonitor/Test/PhaseMonitorDeviceTest.cc
 *
 * $Id: PhaseMonitorWorkerTest.cc,v 1.3 2013/02/06 20:07:31 abeard Exp $
 *
 * CppUnit test fixture for carma::phasemonitor::PhaseMonitorWorker 
 * For these tests we cannot use == since
 * we are testing floating point numbers. Therefore we test
 * that the normalized difference between the expected
 * answer and the returned answer is less than
 * numeric_limits.epsilon(), which is ~10^-7 for floats.
 *
 * Author: Colby Gutierrez-Kraybill
 * Version: $Revision: 1.3 $
 * $Date: 2013/02/06 20:07:31 $
 *
 */
#include <cmath>
#include <limits>
#include <iostream>

#include "carma/monitor/PhaseMonitorSubsystem.h"
#include "carma/phasemonitor/exceptions.h"
#include "PhaseMonitorWorkerTest.h"

using namespace carma::phasemonitor;
using namespace CppUnit;
using namespace std;

#define PMWCPPUNIT_ASSERT( A, B ) \
  do { \
    cout << "  asserting " << #A << "=" << B; \
    _normalizedDiff = fabsl( (A - B) / A ); \
    cout << " (residual: " << _normalizedDiff << " < " << _epsilon << ")" << endl;\
    CPPUNIT_ASSERT(  _normalizedDiff < _epsilon );\
  } while (0)

void PhaseMonitorWorkerTest::setUp() 
{   
  // First, create a table file that will be fed into the AntennaParameters
  // class
  // See the phasemonitor documentation for an explanation of the
  // parameters in this table.
  _nameTemplate = strndup( "/tmp/phasemon.params.unittest.XXXXXX", 80 );
  _tearDownUnlinkNameTemplate = false;
  CPPUNIT_ASSERT( _nameTemplate != NULL );

  // Insure our umask doesn't cause a problem
  ::umask( 00 );

  int tmpfd = ::mkstemp( _nameTemplate );
  // If fd is -1, there's been an error opening the file
  CPPUNIT_ASSERT( tmpfd > -1 );
  // Should we error if fd turns up as one of the stdin/out/err ?

  // Sample table 
  const char *parameters = "#|chan|offV   |cos |sin |scale|\n 0 0.1 0.2 0.3 0.4\n 1 0.5 0.6 0.7 0.8\n 2 0.9 1.0 1.1 1.2\n 3 1.3 1.4 1.5 1.6\n";

  size_t len = strlen( parameters );
  size_t bytesOut = write( tmpfd, parameters, len );
  CPPUNIT_ASSERT( bytesOut == len );
  close( tmpfd );
  _tearDownUnlinkNameTemplate = true;

  carma::monitor::PhaseMonitorSubsystem mon;
  _tPMD = new PhaseMonitorDevice( "", true, "" );
  AntennaParameters params( _nameTemplate );
  _tPMS = new PhaseMonitorSamples( "/tmp/sample.dat" );
  _tPMW = new PhaseMonitorWorker( "/tmp", &mon, *_tPMD, params, *_tPMS );

  // Our tests are based on epsilon(), the smallest
  // float that is not zero.  
  numeric_limits<float> floatLimits;
  _epsilon = floatLimits.epsilon();

}

void PhaseMonitorWorkerTest::tearDown() 
{   
  delete _tPMW;
  delete _tPMS;
  delete _tPMD;

  if ( _tearDownUnlinkNameTemplate )
    ::unlink( _nameTemplate );
}

void PhaseMonitorWorkerTest::testGetPhases()
{
  cout << "PhaseMonitorWorkerTest::testGetPhases()" << endl;

  float dP, sP, aSW, aNE, cORS[4], rV[4], swPhase, nePhase;
  _tPMW->getPhases(&dP, &sP, &aSW, &aNE, cORS, rV, swPhase, nePhase);
  cout
    << " dP: " << dP
    << " sP: " << sP
    << " aSW: " << aSW
    << " aNE: " << aNE;
  for (int i = 0; i < 4; i++ )
    cout << " cORS["<<i<<"]: " << cORS[i] << " rV["<<i<<"]: " << rV[i];
  cout << endl;

  PMWCPPUNIT_ASSERT( dP, 86.3807067871094 );
  PMWCPPUNIT_ASSERT( sP, 202.168670654297 );
  PMWCPPUNIT_ASSERT( aSW, 1.16284036636353 );
  PMWCPPUNIT_ASSERT( aNE, 1.79933905601501 );

  PMWCPPUNIT_ASSERT( cORS[0], 0.678991556167603 );
  PMWCPPUNIT_ASSERT( cORS[1], -0.944017052650452 );
  PMWCPPUNIT_ASSERT( cORS[2], 1.52415537834167 );
  PMWCPPUNIT_ASSERT( cORS[3], 0.956332325935364 );

  PMWCPPUNIT_ASSERT( rV[0], 1.65100002288818 );
  PMWCPPUNIT_ASSERT( rV[1], -0.563000023365021 );
  PMWCPPUNIT_ASSERT( rV[2], -1.22300004959106 );
  PMWCPPUNIT_ASSERT( rV[3], 0.321000009775162 );

}

void PhaseMonitorWorkerTest::testPhaseJumpCheck()
{
  cout << "PhaseMonitorWorkerTest::testPhaseJumpCheck()" << endl;

  bool caught = false;
  try
  {
    cout << " This should throw an exception" << endl;
    float f = 0., l = 180.;
    _tPMW->phaseJumpCheck( f, l );
    cout << "  Not caught!" << endl;
  }
  catch ( PhaseMonitorWorkerException &pmwe )
  {
    caught = true;
    cout << "  Caught! what() - " << pmwe.what() << endl;
  }
}

void PhaseMonitorWorkerTest::testOstream()
{
  cout << "PhaseMonitorWorkerTest::testOstream()" << endl;
  cout << *_tPMW << endl;
}

void PhaseMonitorWorkerTest::testPMWECopyAndString()
{
  cout << "PhaseMonitorWorkerTest::testPMWECopyAndString()" << endl;

  cout
    << " Testing copy form of PhaseMonitorWorkerException c'tor" << endl;
  bool caught = false;
  ostringstream test("PhaseMonitorWorkerException Testing!");
  try
  {
    throw CARMA_EXCEPTION( PhaseMonitorWorkerException, test );
    cout << "  Not caught!" << endl;
  } catch ( PhaseMonitorWorkerException &pmwe )
  {
    cout << "  Caught! what() - " << pmwe.what() << endl;
    PhaseMonitorWorkerException testpmwe( pmwe );
    cout << "   C'tor by copy: " << pmwe.what() << endl;
    caught = true;
  }


  CPPUNIT_ASSERT( caught );
}

