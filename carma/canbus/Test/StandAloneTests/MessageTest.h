/** @file
 * CppUnit test fixture for carma::canbus::Message class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.5 $
 * $Date: 2012/07/13 18:55:30 $
 * $Id: MessageTest.h,v 1.5 2012/07/13 18:55:30 abeard Exp $
 */
#ifndef MESSAGE_TEST_H
#define MESSAGE_TEST_H

#include "carma/canbus/Message.h"

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>


namespace carma {
namespace canbus {
namespace test {

/**
 * CppUnit test class for the carma::canbus::Message class.
 */
class MessageTest : public CppUnit::TestFixture {
public:	

	void setUp();

	void tearDown();
	 
	CPPUNIT_TEST_SUITE(MessageTest);

	CPPUNIT_TEST( testId );
	CPPUNIT_TEST( testBusId );
	CPPUNIT_TEST( testData );
	CPPUNIT_TEST( testAssignmentAndEquality );
    CPPUNIT_TEST( testInsertionOperators );

	CPPUNIT_TEST_SUITE_END();
	
	void testId();
	void testBusId();
	void testData();
	void testAssignmentAndEquality();
    void testInsertionOperators();
	
	// Todo: Test exceptions...
	
private:
	
	carma::canbus::Message * defaultMsg; // Standard CAN message
	carma::canbus::Message * idMsg;      // Message with id and busId.
	carma::canbus::Message * dataMsg;    // Message with everything specified.
    carma::canbus::Message * copyMsg;    // Created with copy constructor.

};
}}} // End namespace carma::canbus::test
#endif
