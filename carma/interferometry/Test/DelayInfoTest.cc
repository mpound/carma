/**
 * @file carma/interferometry/Test/DelayInfoTest.cc
 *
 * $Id: DelayInfoTest.cc,v 1.3 2010/09/24 20:30:33 mpound Exp $
 *
 * CppUnit test fixture for carma::interferometry::DelayInfo.
 * Not a whole lot to test here. It's just a bunch of vectors.
 * @author Marc Pound
 * @version: $Revision: 1.3 $
 * $Date: 2010/09/24 20:30:33 $
 */
#include "DelayInfoTest.h"

using namespace carma::interferometry;
using namespace CppUnit;
using namespace std;

void DelayInfoTest::setUp() 
{   
}

void DelayInfoTest::tearDown() 
{   
}

void DelayInfoTest::testSomething() 
{
    tInfo.reset();
    tInfo.X.at(0) = 44.0;
    tInfo.Y.at(22) = 98.0;
    CPPUNIT_ASSERT( 1 == 1 );
}
