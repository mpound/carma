/** @file 
 *
 * CppUnit test fixture for carma::dbms::DBConnection 
 *
 * Author: Dave Mehringer
 *
 */
#ifndef CARMA_DBMS_TABLETEST_H
#define CARMA_DBMS_TABLETEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>
#include "carma/dbms/Table.h"

/**
 * carma::dbms::TableTest test class for CppUnit.
 */
class TableTest : public CppUnit::TestFixture {
public:	

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();
	 

    CPPUNIT_TEST_SUITE(TableTest);
    CPPUNIT_TEST( populateTests );
    CPPUNIT_TEST_SUITE_END();
	
    void populateTests();
	
	
};
#endif //CARMA_DBMS_TABLETEST_H
