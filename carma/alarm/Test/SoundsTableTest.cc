/** 
 * @file carma/alarm/Test/SoundsTableTest.cc
 *
 * $Id: SoundsTableTest.cc,v 1.1 2006/11/30 10:56:55 colby Exp $
 *
 * CppUnit test fixture for carma::alarm::SoundsTable
 * For these tests we cannot use == since
 * we are testing floating point numbers. Therefore we test
 * that the normalized difference between the expected
 * answer and the returned answer is less than
 * numeric_limits.epsilon(), which is ~10^-7 for floats.
 *
 * Author: Colby Gutierrez-Kraybill
 * Version: $Revision: 1.1 $
 * $Date: 2006/11/30 10:56:55 $
 *
 */
#include <cmath>
#include <limits>
#include <iostream>

#include <math.h>

#include "SoundsTableTest.h"
#include "carma/util/Program.h"

#include <log4cpp/Category.hh>

using namespace carma::alarm;
using namespace carma::util;
using namespace CppUnit;
using namespace std;

#define WTCPPUNIT_ASSERT( A, B ) \
  do { \
    cout << "  asserting " << #A << "=" << B; \
    _normalizedDiff = fabsl( (A - B) / A ); \
    cout << " (residual: " << _normalizedDiff << " < " << _epsilon << ")" << endl;\
    CPPUNIT_ASSERT(  _normalizedDiff < _epsilon );\
  } while (0)

void SoundsTableTest::setUp() 
{   

  _tST = new SoundsTable();

  // float that is not zero.  
  numeric_limits<float> floatLimits;
  _epsilon = floatLimits.epsilon();
}

void SoundsTableTest::tearDown() 
{   
  delete _tST;
}

/** test get a default sound by name */
void SoundsTableTest::testGetSoundFileByName()
{
  cout << "SoundsTableTest::testGetSoundFileByName()" << endl;

  cout << "  Retreiving filename using default 'alarm' name";
  CPPUNIT_ASSERT( _tST->getSoundFileByName("alarm").size() > 0 );
  cout << "   answer: '" << _tST->getSoundFileByName("alarm") << "'" << endl;

  cout << "  Requesting a non-existent alarm name should default to 'alarm'";
  CPPUNIT_ASSERT( _tST->getSoundFileByName("BADNAME")
      == _tST->getSoundFileByName("alarm") );
  cout << "...it does!" << endl;
}


void SoundsTableTest::testGetSoundFullPathFileByName()
{
  cout << "SoundsTableTest::testGetSoundFileByName()" << endl;

  cout << "  Retreiving fullpath filename using default 'alarm' name";
  CPPUNIT_ASSERT( _tST->getSoundFullPathFileByName("alarm").size() > 0 );
  cout << "   answer: '" << _tST->getSoundFullPathFileByName("alarm") << "'" << endl;

  cout << "  Requesting a non-existent alarm name should default to 'alarm' when getting"
    << " fullpath";
  CPPUNIT_ASSERT( _tST->getSoundFileByName("BADNAME")
      == _tST->getSoundFileByName("alarm") );
  cout << "...it does!" << endl;
}
