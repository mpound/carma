/** @file 
 *
 * CppUnit test fixture for filter classes
 *
 * @author Dave Mehringer
 * @version $Id: FilterTest.h,v 1.2 2004/12/11 08:54:38 dmehring Exp $
 *
 * $CarmaCopyright$
 */
#ifndef CARMA_DBMS_FILTERTEST_H
#define CARMA_DBMS_FILTERTEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

/**
 * carma::dbms::FilterTest test class for CppUnit.
 */
class FilterTest : public CppUnit::TestFixture {
public:	

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();
	 

    CPPUNIT_TEST_SUITE(FilterTest);
    CPPUNIT_TEST( twoComponentFilterTests );
    CPPUNIT_TEST( monitorPointFilterTests );
    CPPUNIT_TEST_SUITE_END();
	
    void twoComponentFilterTests();
    void monitorPointFilterTests();
};
#endif //CARMA_DBMS_FILTERTEST_H
