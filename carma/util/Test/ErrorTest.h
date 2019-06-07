#ifndef CARMA_UTIL_ERRORTEST_H
#define CARMA_UTIL_ERRORTEST_H

/** @file
 *
 * CppUnit test fixture for carma::util error exception classes.
 *
 * @author: Steve Scott
 *
 * $Id: ErrorTest.h,v 1.4 2006/03/10 18:45:47 tcosta Exp $
 * 
 *
 */

#include "carma/util/ErrorException.h"

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

class ErrorTest : public CppUnit::TestFixture {
public:	

    void setUp();
    void tearDown();
	 

    CPPUNIT_TEST_SUITE(ErrorTest);

    CPPUNIT_TEST( testConstructor );
    CPPUNIT_TEST( testThrow );
    CPPUNIT_TEST( testPolymorphism );
    CPPUNIT_TEST( streamInsertion );

    CPPUNIT_TEST_SUITE_END();

	
    void testConstructor();
    void testThrow();
    void testPolymorphism();
    void streamInsertion();
private:
    ::std::string fullstr( const std::string & msg );
    
    int lineNumber_;
};

#endif // CARMA_UTIL_EXCEPTIONTEST_H
