/** 
 * @file carma/phasemonitor/Test/PhaseMonitorDeviceTest.cc
 *
 * $Id: PhaseMonitorDeviceTest.cc,v 1.9 2013/02/06 20:07:31 abeard Exp $
 *
 * CppUnit test fixture for carma::phasemonitor::PhaseMonitorDevice 
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
#include <iostream>

#include <stdlib.h> // mkstemp()
#include <sys/types.h> //umask
#include <sys/stat.h> //umask

#include "PhaseMonitorDeviceTest.h"
#include "carma/phasemonitor/exceptions.h"

using namespace carma::phasemonitor;
using namespace CppUnit;
using namespace std;

#define PMDCPPUNIT_ASSERT( A, B ) \
  do { \
    cout << "  asserting " << #A << "=" << B; \
    _normalizedDiff = fabsl( (A - B) / A ); \
    cout << " (residual: " << _normalizedDiff << " < " << _epsilon << ")" << endl;\
    CPPUNIT_ASSERT(  _normalizedDiff < _epsilon );\
  } while (0)

void PhaseMonitorDeviceTest::setUp() 
{   
  _tearDownNameTemplate = false;
  _tearDownUnlinkNameTemplate = false;

  // First, create a table file that will be fed into the class
  // See the phasemonitor documentation for an explanation of the
  // parameters in this table.
  _nameTemplate = strndup( "/tmp/phasemon.device.unittest.XXXXXX", 80 );
  CPPUNIT_ASSERT( _nameTemplate != NULL );
  _tearDownNameTemplate = true;

  // Insure our umask doesn't cause a problem
  ::umask( 00 );

  int tmpfd = ::mkstemp( _nameTemplate );
  // If fd is -1, there's been an error opening the file
  CPPUNIT_ASSERT( tmpfd > -1 );
  // Should we error if fd turns up as one of the stdin/out/err ?

  // Sample data
  const char *replayData
    = ">+0.1000-0.2000+0.3000-0.4000\n>+0.5000-0.6000+0.7000-0.8000\n>+0.9000-1.0000+1.1000-1.2000\n>+1.3000-1.4000+1.5000-11.000\n"; // last row should cause exception

  size_t len = strlen( replayData );
  size_t bytesOut = write( tmpfd, replayData, len );
  CPPUNIT_ASSERT( bytesOut == len );
  close( tmpfd );
  _tearDownUnlinkNameTemplate = true;

  string sNameTemp = string( _nameTemplate );

  this->_tPMD = new carma::phasemonitor::PhaseMonitorDevice( "", true,
      "/tmp/phasemonitor.unittest.datarecord", false, sNameTemp );

  // Our tests are based on epsilon(), the smallest
  // float that is not zero.  
  numeric_limits<float> floatLimits;
  _epsilon = floatLimits.epsilon();
}

void PhaseMonitorDeviceTest::tearDown() 
{   
  delete _tPMD;

  if ( _tearDownUnlinkNameTemplate )
    unlink( _nameTemplate );

  //if ( _tearDownNameTemplate )
  //  free( _nameTemplate );

  bool tearDown = true;
  CPPUNIT_ASSERT( tearDown );
}

// Should return true
void PhaseMonitorDeviceTest::testIsReplay()
{
  cout << "PhaseMonitorDeviceTest::testIsReplay()" << endl;

  CPPUNIT_ASSERT( _tPMD->isReplay() );
}

void PhaseMonitorDeviceTest::testSleepSomeNanos()
{
  cout << "PhaseMonitorDeviceTest::testSleepSomeNanos()" << endl;

  _tPMD->testSleepSomeNanos();

  CPPUNIT_ASSERT( true );
}

void PhaseMonitorDeviceTest::testCommand()
{
  cout << "PhaseMonitorDeviceTest::testCommand()" << endl;

  _tPMD->command( "testCommand" );
  bool testCommand = true;
  CPPUNIT_ASSERT( testCommand );
}

void PhaseMonitorDeviceTest::testInquire()
{
  cout << "PhaseMonitorDeviceTest::testInquire()" << endl;

  string reply = _tPMD->inquire( "$012" );
  cout << "  inquire( $012 ) reply: '" << reply
    << "' == '!01080600'" << endl;
  CPPUNIT_ASSERT( reply == "!01080600" );

  reply = _tPMD->inquire( "$0151F" );
  cout << "  inquire( $0151F ) reply: '' == '' " << endl;
  CPPUNIT_ASSERT( reply == "" );

  reply = _tPMD->inquire( "#014" );
  cout << "  inquire( #014 ) reply: '" << reply
    << "' == '>+09.9900'" << endl;
  CPPUNIT_ASSERT( reply == ">+09.9900" );

  reply = _tPMD->inquire( "#01" );
  cout << "  inquire( #01 ) reply: '" << reply
    << "' == '>+1.6510-0.5630-1.2230+0.3210'" << endl;
  CPPUNIT_ASSERT( reply == ">+1.6510-0.5630-1.2230+0.3210" );

}

// startOfReply is a private member of PhaseMonitorDevice
// So there is a wrapper around the actual test
void PhaseMonitorDeviceTest::testBadStartOfReply()
{
  cout << "PhaseMonitorDeviceTest::testTestBadStartOfReply()" << endl;

  cout << " Will now force a parsing exception to occur" << endl;
  CPPUNIT_ASSERT( _tPMD->testBadStartOfReply() );
}

// Loads voltages from the replay file
void PhaseMonitorDeviceTest::testQueryVoltages()
{
  float voltages[4];

  cout << "PhaseMonitorDeviceTest::testQueryVoltages()" << endl;

  // Run four times to pick up all four lines in the replay file
  long double p = 0.1;
  long double sign = 1.0;
  for ( int i = 0; i < 3; i++ )
  {
    _tPMD->queryVoltages( voltages );
    for ( int j = 0; j < 4; j++ )
    {
      PMDCPPUNIT_ASSERT( voltages[j], (p * sign) );
      p = p + 0.1;
      sign = sign * -1;
    }
  }

  // The fourth row of voltages in the file has a special bad
  // voltage, test for that
  bool badVoltsCaught = false;
  cout <<
    " Next replay file row should cause queryVoltages to throw an exception"
    << endl;
  try
  {
    _tPMD->queryVoltages( voltages );
    cout << " Not caught!" << endl;
  }
  catch ( BadVoltageException & pmde )
  {
    cout << "  Caught! what() - " << pmde.what() << endl;
    badVoltsCaught = true;
  }
  CPPUNIT_ASSERT( badVoltsCaught );

  cout
    << " Now test queryVoltages without replay,"
    << " using pseudo terminal comms" << endl;
  _tPMD->stopReplay( );
  _tPMD->queryVoltages( voltages );
  PMDCPPUNIT_ASSERT( voltages[0], 1.6510 );
  PMDCPPUNIT_ASSERT( voltages[1], -0.5630 );
  PMDCPPUNIT_ASSERT( voltages[2], -1.2230 );
  PMDCPPUNIT_ASSERT( voltages[3], 0.3210 );
}

void PhaseMonitorDeviceTest::testQueryTemperatureC()
{
  cout << "PhaseMonitorDeviceTest::testQueryTemperatureC()" << endl;

  float temp = _tPMD->queryTemperatureC();
  PMDCPPUNIT_ASSERT( temp, 999.0 );
}

void PhaseMonitorDeviceTest::testTestBadVolts()
{
  cout << "PhaseMonitorDeviceTest::testTestBadVolts()" << endl;

  _tPMD->setTestBadVolts( true );
  CPPUNIT_ASSERT( _tPMD->testBadVolts() );


}

void PhaseMonitorDeviceTest::testEmulate()
{
  cout << "PhaseMonitorDeviceTest::testEmulate()" << endl;

  CPPUNIT_ASSERT( _tPMD->isEmulating() );
}

void PhaseMonitorDeviceTest::testDevFileName()
{
  cout << "PhaseMonitorDeviceTest::testDevFileName()" << endl;

  cout
    << " Unit test uses psuedo terminal,"
    << " should start with /dev/pts on system using glibc 2.0" << endl;
  string name = _tPMD->getDeviceFileName();

  cout << "  dev: " << name;
  CPPUNIT_ASSERT( name.find_first_of( "/dev/pts/", 0 ) == 0 );
  cout << " W00T!" << endl;
}

void PhaseMonitorDeviceTest::testPMDECopyAndString()
{
  cout << "PhaseMonitorDeviceTest::testPMDEString()" << endl;

  cout
    << " Testing copy form of PhaseMonitorDeviceException c'tor" << endl;
  bool caught = false;
  ostringstream test("Testing!");
  try
  {
    throw CARMA_EXCEPTION( PhaseMonitorDeviceException, test );
    cout << "  Not caught!" << endl;
  } catch ( PhaseMonitorDeviceException &pmde )
  {
    cout << "  Caught! what() - " << pmde.what() << endl;
    PhaseMonitorDeviceException testpmde( pmde );
    cout << "   C'tor by copy: " << pmde.what() << endl;
    caught = true;
  }


  CPPUNIT_ASSERT( caught );
}


void PhaseMonitorDeviceTest::testOstream()
{
  cout << "PhaseMonitorDeviceTest::testOstream() " << endl;
  cout << *_tPMD << endl;
}

void PhaseMonitorDeviceTest::testConvertStringToFloat()
{
  cout << "PhaseMonitorDeviceTest::testConvertStringToFloat() " << endl;

  cout
    << " Will now force convertStringToFloat to throw exceptions in the"
    << " following order: unknown, erange underflow, erange overflow,"
    << " erange unknown " << endl;

  string bad[4];
  bad[0] = string("BAD");
  bad[3] = string("1B.AD49");
  bad[1] = string("1.0e-50");
  bad[2] = string("1.0e+50");

  bool caught;
  for ( int i = 0; i < 4; i++ )
  {
    caught = false;
    try
    {
      _tPMD->convertStringToFloat( bad[i], string("test") );
    }
    catch ( InvalidResponseException &pmde )
    {
      cout << "  Caught! what() - " << pmde.what() << endl;
      caught = true;
    }
    CPPUNIT_ASSERT( caught );
  }

}
