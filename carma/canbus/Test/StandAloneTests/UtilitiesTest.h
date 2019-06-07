#ifndef UTILITIES_TEST_H
#define UTILITIES_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>
#include "carma/canbus/Utilities.h"

namespace carma {
namespace canbus {
namespace test {

/**
 * CppUnit test class for the carma::canbus::Message class.
 */
class UtilitiesTest : public CppUnit::TestFixture {
public:
	void setUp();

	void tearDown();

	CPPUNIT_TEST_SUITE(UtilitiesTest);

	CPPUNIT_TEST( testMode );
	CPPUNIT_TEST( testHost );
	CPPUNIT_TEST( testId );
	CPPUNIT_TEST( testEngId );
    CPPUNIT_TEST( testUbyte );
    CPPUNIT_TEST( testUshort );
    CPPUNIT_TEST( testUlong );
    CPPUNIT_TEST( testSshort );
    CPPUNIT_TEST( testSlong );
    CPPUNIT_TEST( testFloat );
    CPPUNIT_TEST( testDouble );
	CPPUNIT_TEST( testPadding );
    CPPUNIT_TEST( testFastTime );
    CPPUNIT_TEST( testSlowTime );

	CPPUNIT_TEST_SUITE_END();

	void testMode();
	void testHost();
	void testId();
	void testEngId();
    void testKey();
    void testUbyte();
    void testUshort();
    void testUlong();
    void testSshort();
    void testSlong();
    void testFloat();
    void testDouble();
    void testPadding();
    void testFastTime();
    void testSlowTime();
    
private:
	idType appId;
    idType engId;
};
}}} // End namespace carma::canbus::test
#endif

