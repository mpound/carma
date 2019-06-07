/** @file
 * $Id: DelayInfoTest.h,v 1.1 2004/04/13 15:31:15 mpound Exp $
 *
 * CppUnit test fixture for carma::interferometry
 *
 * Author: Marc Pound
 * Version: $Revision: 1.1 $
 * $Data: $
 *
 */
#ifndef CARMA_INTERFEROMETRY_DELAY_INFO_TEST_H
#define CARMA_INTERFEROMETRY_DELAY_INFO_TEST_H

#include "carma/interferometry/DelayInfo.h"

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

/**
 * carma::interferemetry::DelayInfo test class for CppUnit.
 */
class DelayInfoTest : public CppUnit::TestFixture {
public:	

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();
	 
    CPPUNIT_TEST_SUITE(DelayInfoTest);

    CPPUNIT_TEST( testSomething );

    CPPUNIT_TEST_SUITE_END();
	
    /** test something */
    void testSomething();
	
private:

    //carma::interferometry::DelayInfo* tInfo;
    carma::interferometry::DelayInfo tInfo;
	
};
#endif //CARMA_INTERFEROMETRY_DELAY_INFO_TEST_H
