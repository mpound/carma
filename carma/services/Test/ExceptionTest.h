/** 
 * @file
 *
 * CppUnit test fixture for carma::services::*Exception
 *
 * Author: Marc Pound
 * Version: $Revision: 1.1 $
 * $Date: 2006/12/01 21:08:56 $
 *
 */
#ifndef CARMA_SERVICES_EXCEPTION_TEST_H
#define CARMA_SERVICES_EXCEPTIOn_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

/**
 * CpuUnit test class for all Exceptions possible from carma::services 
 */
class ExceptionTest : public CppUnit::TestFixture {
public:	

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();
	 
    CPPUNIT_TEST_SUITE(ExceptionTest);

    CPPUNIT_TEST( testExceptions );

    CPPUNIT_TEST_SUITE_END();
	
    /** test all the exception classes */
    void testExceptions();
    /**indicate test has passed */
    void pass();
    /**indicate test has failed */
    void fail();

};
#endif 
