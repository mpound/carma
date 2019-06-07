/** @file
 * $Id: TableTest.h,v 1.4 2009/05/27 00:19:01 mpound Exp $
 *
 * CppUnit test fixture for carma::services::Table
 *
 * Author: Peter Teuben
 * Version: $Revision: 1.4 $
 * $Data: $
 *
 */
#ifndef CARMA_SERVICES_TABLE_TEST_H
#define CARMA_SERVICES_TABLE_TEST_H

#include "carma/services/Table.h"

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

/**
 * carma::services::Table test class for CppUnit.
 */
class TableTest : public CppUnit::TestFixture {
public:	

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();
	 
    CPPUNIT_TEST_SUITE(TableTest);

    CPPUNIT_TEST( testTable1 );
    //CPPUNIT_TEST( testBug454 );
    CPPUNIT_TEST( testDataVerification );

    CPPUNIT_TEST_SUITE_END();
	
    /** test simple table creation and access */
    void testTable1( void );

    /**
     * A regression test for bug 454 (missing EOL in file)
     * This test will fail if the bug has reappeared.
     */
    void testBug454( void );

    /**
     * Will test for badly formatted numbers in a Table column
     */
    void testDataVerification( void );
	
};
#endif //CARMA_SERVICES_TABLE_TEST_H
