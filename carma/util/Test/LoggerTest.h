/**
 *
 * Unit test for SyslogLayout.cc and FileLayout.cc
 *
 * @author Original: Marc Pound
 * $Id: LoggerTest.h,v 1.3 2004/02/20 18:59:15 mpound Exp $
 *
 * $CarmaCopyright$
 *
 */

#ifndef CARMA_UTIL_LOGGERTEST_H
#define CARMA_UTIL_LOGGERTEST_H

#include "carma/util/Logger.h"
#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>


class LoggerTest : public CppUnit::TestFixture {
public:
    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();
	      

    CPPUNIT_TEST_SUITE(LoggerTest);

    CPPUNIT_TEST(testFileLayout);
    CPPUNIT_TEST(testSyslogLayout);
    CPPUNIT_TEST(testOstreamLayout);

    CPPUNIT_TEST_SUITE_END();

    /** Test logging to a file */
    void testFileLayout();

    /** Test logging to syslog*/
    void testSyslogLayout();

    /** Test logging to an output stream */
    void testOstreamLayout();
};

#endif //CARMA_UTIL_LOGGERTEST_H
