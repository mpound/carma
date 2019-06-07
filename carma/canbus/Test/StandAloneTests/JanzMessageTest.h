/** @file
 * CppUnit test fixture for carma::canbus::Message class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.1 $
 * $Date: 2012/07/13 18:55:30 $
 * $Id: JanzMessageTest.h,v 1.1 2012/07/13 18:55:30 abeard Exp $
 */
#ifndef JANZMESSAGE_TEST_H
#define JANZMESSAGE_TEST_H

#include "carma/canbus/JanzMessage.h"

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>


namespace carma {
namespace canbus {
namespace test {

/**
 * CppUnit test class for the carma::canbus::Message class.
 */
class JanzMessageTest : public CppUnit::TestFixture {
public:	

	void setUp();

	void tearDown();
	 
	CPPUNIT_TEST_SUITE(JanzMessageTest);

	CPPUNIT_TEST( testMessageType );
	CPPUNIT_TEST( testId );
	CPPUNIT_TEST( testBusId );
	CPPUNIT_TEST( testRawMessage );
	CPPUNIT_TEST( testAssignment );
	CPPUNIT_TEST( testEquality );
	CPPUNIT_TEST( testData );

	CPPUNIT_TEST_SUITE_END();
	
	void testMessageType();
	void testId();
	void testBusId();
	void testRawMessage();
	void testAssignment();
	void testEquality();
	void testData();
	
	// Todo: Test exceptions...
	
private:
	
	carma::canbus::JanzMessage *stdMsg;	   // Standard CAN message
	carma::canbus::JanzMessage *xtdMsg;    // Extended CAN message
	carma::canbus::JanzMessage *idMsg;     // Message with id and busId.
	carma::canbus::JanzMessage *dataMsg;   // Message with everything specified.
};
}}} // End namespace carma::canbus::test
#endif
