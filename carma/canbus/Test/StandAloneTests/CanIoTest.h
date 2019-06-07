/** @file
 * CppUnit test fixture for carma::canbus::CanOutput and carma::canbus::CanIo 
 * classes.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.6 $
 * $Date: 2012/07/25 17:51:32 $
 * $Id: CanIoTest.h,v 1.6 2012/07/25 17:51:32 abeard Exp $
 */
#ifndef CARMA_CANBUS_TEST_CANIO_TEST_H
#define CARMA_CANBUS_TEST_CANIO_TEST_H

#include "carma/canbus/JanzCanIo.h"

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

namespace carma {
namespace canbus {
namespace test {

/**
 * Helper class for CppUnit CanIoTest.
 */
class TestCanIo : public carma::canbus::JanzCanIo {
    public:
    
    TestCanIo() {};

    // Expose this publicly so that I can post and then 
    // retrieve messages.
    void queueMessage(const carma::canbus::Message &msg) {
        carma::canbus::JanzCanIo::queueMessage(msg);
    };
};

/**
 * carma::canbus::CanIo test class for CppUnit.
 */
class CanIoTest : public CppUnit::TestFixture {
public:	

	void setUp();

	void tearDown();
	 
	CPPUNIT_TEST_SUITE(CanIoTest);

	CPPUNIT_TEST( testPostMessage );
    CPPUNIT_TEST( testGetMessage );
    CPPUNIT_TEST( testStatus );

	CPPUNIT_TEST_SUITE_END();
	
    void testPostMessage();
    void testGetMessage();
    void testStatus();
	
private:

    TestCanIo *canIo_;
	
};
}}} // End namespace carma::canbus::test
#endif
