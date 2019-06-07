/** @file
 * $Id: PhysicalTest.h,v 1.1 2004/09/09 19:11:55 mpound Exp $
 *
 * CppUnit test fixture for carma::services::Physical 
 *
 * Author: Marc Pound
 * Version: $Revision: 1.1 $
 * $Data: $
 *
 */
#ifndef CARMA_SERVICES_PHYSICAL_TEST_H
#define CARMA_SERVICES_PHYSICAL_TEST_H

#include "carma/services/Physical.h"

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

/**
 * carma::services::Physical test class for CppUnit.
 */
class PhysicalTest : public CppUnit::TestFixture {
public:	

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();
	 
    CPPUNIT_TEST_SUITE(PhysicalTest);

    CPPUNIT_TEST( testConstants);

    CPPUNIT_TEST_SUITE_END();
	
    /** test the constant values */
    void testConstants();
	
};
#endif //CARMA_SERVICES_PHYSICAL_TEST_H
