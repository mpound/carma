
/**
 *
 * Unit test for Time.cc
 *
 * @author: Steve Scott  
 * $Id: TimeTest.h,v 1.5 2007/12/14 18:40:32 abeard Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <cppunit/extensions/HelperMacros.h>


class TimeTest : public CppUnit::TestCase {
public:
    TimeTest(std::string name);
    ~TimeTest();
    void runTest();

private:
    void test1();
    void testRounding();
    void testConversions();
    void profile();
    void dateConversion();
};

