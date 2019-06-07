/** 
 * @file carma/antenna/bima/Test/AudioControlThreadTest.cc
 *
 * $Id: TelemetryTest.cc,v 1.3 2012/08/01 18:43:02 friedel Exp $
 *
 * CppUnit test fixture for carma::antenna/bima::AudioControlThread
 * For these tests we cannot use == since
 * we are testing floating point numbers. Therefore we test
 * that the normalized difference between the expected
 * answer and the returned answer is less than
 * numeric_limits.epsilon(), which is ~10^-7 for floats.
 *
 * Author: Colby Gutierrez-Kraybill
 * Version: $Revision: 1.3 $
 * $Date: 2012/08/01 18:43:02 $
 *
 */
#include <cmath>
#include <limits>
#include <iostream>

#include <math.h>

#include "TelemetryTest.h"
#include "carma/util/StartPthread.h"
#include "carma/util/Program.h"

#include <log4cpp/Category.hh>

using namespace carma::antenna::bima;
using namespace carma::util;
using namespace CppUnit;
using namespace std;

void TelemetryTest::setUp() 
{   
  Configuration config( "unit1", Program::getConfDir() );
  config.setEmulate( true );
  _tT = new Telemetry( 0, 0, config,NULL );
}

void TelemetryTest::tearDown() 
{   
}

void TelemetryTest::testProcessMsgs()
{

}

