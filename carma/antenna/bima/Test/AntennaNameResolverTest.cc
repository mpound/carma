/** 
 * @file carma/antenna/bima/Test/AudioControlThreadTest.cc
 *
 * $Id: AntennaNameResolverTest.cc,v 1.2 2006/12/02 08:03:48 colby Exp $
 *
 * CppUnit test fixture for carma::antenna/bima::AudioControlThread
 * For these tests we cannot use == since
 * we are testing floating point numbers. Therefore we test
 * that the normalized difference between the expected
 * answer and the returned answer is less than
 * numeric_limits.epsilon(), which is ~10^-7 for floats.
 *
 * Author: Colby Gutierrez-Kraybill
 * Version: $Revision: 1.2 $
 * $Date: 2006/12/02 08:03:48 $
 *
 */
#include <cmath>
#include <limits>
#include <iostream>

#include <math.h>

#include "AntennaNameResolverTest.h"
#include "carma/util/StartPthread.h"
#include "carma/util/Program.h"

#include <log4cpp/Category.hh>

using namespace carma::antenna::bima;
using namespace CppUnit;
using namespace std;

void AntennaNameResolverTest::setUp() 
{   
}

void AntennaNameResolverTest::tearDown() 
{   
}


void AntennaNameResolverTest::testConstructors()
{
  cout << "AntennaNameResolverTest::testConstructors()..." << endl;

  cout << "  Default constructor...";
  AntennaNameResolver anr1;
  cout << "OK! (" << anr1.getAntennaName() << ")" << endl;

  cout << "  const char * constructor...";
  AntennaNameResolver anr2("bima1");
  CPPUNIT_ASSERT( anr2.getAntennaName().compare( "bima1" ) == 0 );
  cout << "OK! (" << anr2.getAntennaName() << ")" << endl;
}

