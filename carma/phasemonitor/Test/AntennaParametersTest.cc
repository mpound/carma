/** 
 * @file carma/phasemonitor/Test/AntennaParametersTest.cc
 *
 * $Id: AntennaParametersTest.cc,v 1.8 2013/02/06 20:07:31 abeard Exp $
 *
 * CppUnit test fixture for carma::phasemonitor::AntennaParameters 
 * For these tests we cannot use == since
 * we are testing floating point numbers. Therefore we test
 * that the normalized difference between the expected
 * answer and the returned answer is less than
 * numeric_limits.epsilon(), which is ~10^-7 for floats.
 *
 * Author: Colby Gutierrez-Kraybill
 * Version: $Revision: 1.8 $
 * $Date: 2013/02/06 20:07:31 $
 *
 */
#include <cmath>
#include <limits>
#include <iostream>

#include <stdlib.h> // mkstemp()
#include <sys/types.h> //umask,open
#include <sys/stat.h> //umask,open
#include <fcntl.h> //open
#include <unistd.h> //lseek

#include "AntennaParametersTest.h"
#include "carma/util/ErrorException.h"

using namespace carma::phasemonitor;
using namespace CppUnit;
using namespace std;

#define APCPPUNIT_ASSERT( A, B ) \
  do { \
    cout << "  asserting " << #A << "=" << B; \
    _normalizedDiff = fabsl( (A - B) / A ); \
    cout << " (residual: " << _normalizedDiff << " < " << _epsilon << ")" << endl;\
    CPPUNIT_ASSERT(  _normalizedDiff < _epsilon );\
  } while (0)

void AntennaParametersTest::setUp() 
{   
  _tearDownNameTemplate = false;
  _tearDownUnlinkNameTemplate = false;

  // First, create a table file that will be fed into the class
  // See the phasemonitor documentation for an explanation of the
  // parameters in this table.
  _nameTemplate = strndup( "/tmp/phasemon.params.unittest.XXXXXX", 80 );
  CPPUNIT_ASSERT( _nameTemplate != NULL );
  _tearDownNameTemplate = true;

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

  string sNameTemp = string( _nameTemplate );

  this->_tAP = new carma::phasemonitor::AntennaParameters( sNameTemp );

  // Our tests are based on epsilon(), the smallest
  // float that is not zero.  
  numeric_limits<float> floatLimits;
  _epsilon = floatLimits.epsilon();
}

void AntennaParametersTest::tearDown() 
{   
  delete _tAP;

  if ( _tearDownUnlinkNameTemplate )
    ::unlink( _nameTemplate );
}

void AntennaParametersTest::testParameters()
{
  float offV[AntennaParameters::channels];
  float rotCos[AntennaParameters::channels];
  float rotSin[AntennaParameters::channels];
  float scale[AntennaParameters::channels];

  cout << "AntennaParametersTest::testParameters() " << endl;

  this->_tAP->getParameters( offV, rotCos, rotSin, scale );
  long double p = 0.1;

  for ( unsigned int i = 0; i < AntennaParameters::channels; i++ )
  {
    cout << "  asserting offV[i]=p : " << offV[i] << " = "
      << p;
    APCPPUNIT_ASSERT( offV[i], p );
    p = p + 0.1;
    APCPPUNIT_ASSERT( rotCos[i], p );
    p = p + 0.1;
    APCPPUNIT_ASSERT( rotSin[i], p );
    p = p + 0.1;
    APCPPUNIT_ASSERT( scale[i], p );
    p = p + 0.1;
  }
}


void AntennaParametersTest::testFileLength()
{
  cout << "AntennaParametersTest::testFileLength() " << endl;
  // Snip off the end of the sample parameters file to
  // cause a exception
  int tmpfd;
  CPPUNIT_ASSERT( ( tmpfd = open( _tAP->getFilename().c_str(), O_RDWR ) ) > -1 );

  // remove the last 2 columns of the last row, so that
  // Table lookup fails
  lseek( tmpfd, 8*sizeof(char), SEEK_END );
  CPPUNIT_ASSERT( write( tmpfd, "\0", 1 ) == 1 );

  CPPUNIT_ASSERT( close( tmpfd ) > -1 );

  bool caught = false;
  cout
    << "  Will now attempt to force exception with garbled parameter file"
    << endl;
  try
  {
    _tAP->load( _tAP->getFilename() );
    cout << "   Not caught!" << endl;
  }
  catch ( carma::util::ErrorException & cuee )
  {
    caught = true;
    cout << "   Caught! what() - " << cuee.what() << endl;
  }

  CPPUNIT_ASSERT( caught );
}

void AntennaParametersTest::testOstream()
{
  cout << "AntennaParametersTest::testOstream() " << endl;
  cout << *_tAP << endl;
}

