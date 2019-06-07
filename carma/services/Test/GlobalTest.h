/** @file
 * $Id: GlobalTest.h,v 1.2 2004/09/15 15:50:45 mpound Exp $
 *
 * CppUnit test fixture for carma::services::Global 
 *
 * Author: Marc Pound
 * Version: $Revision: 1.2 $
 * $Data: $
 *
 */
#ifndef CARMA_SERVICES_GLOBAL_TEST_H
#define CARMA_SERVICES_GLOBAL_TEST_H

#include "carma/services/Global.h"
#include "carma/services/Types.h"

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

/**
 * carma::services::Global test class for CppUnit.
 */
class GlobalTest : public CppUnit::TestFixture {
public:	

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();
	 
    CPPUNIT_TEST_SUITE(GlobalTest);

    CPPUNIT_TEST( testConstants);

    CPPUNIT_TEST_SUITE_END();
	

    /** test the routines that should return constant values */
    void testConstants();
	
};
#endif //CARMA_SERVICES_GLOBAL_TEST_H
