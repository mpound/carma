/** @file
 * $Id: MessageTest.cc,v 1.9 2012/07/13 18:55:30 abeard Exp $
 *
 * CppUnit test fixture implementation for carma::canbus::Message class.
 *
 * Author: Andy Beard
 * Version: $Revision: 1.9 $
 * $Data: $
 *
 */
#include <vector>

#include "MessageTest.h"
#include "carma/canbus/Message.h"
#include "carma/canbus/Utilities.h"

using namespace std;
using namespace carma;
using namespace carma::canbus;
using namespace carma::canbus::test;

namespace {

const carma::canbus::idType MSG_ID = 0x123456;
const carma::canbus::busIdType BUS_ID = 1;

} // namespace < unnamed >

void MessageTest::setUp() 
{
    DataVector data;

    defaultMsg = new canbus::Message( );
    idMsg = new canbus::Message( MSG_ID, BUS_ID );
    dataMsg = new canbus::Message( MSG_ID, data, BUS_ID );
    copyMsg = new canbus::Message( *dataMsg );
}

void MessageTest::tearDown() 
{
    delete defaultMsg;
    delete idMsg;
    delete dataMsg;
    delete copyMsg;
}

void MessageTest::testId()
{
    CPPUNIT_ASSERT( defaultMsg->getId() == 0 );

    defaultMsg->setId( MSG_ID );
    CPPUNIT_ASSERT( defaultMsg->getId() == MSG_ID );

    CPPUNIT_ASSERT( idMsg->getId() == MSG_ID );
    CPPUNIT_ASSERT( dataMsg->getId() == MSG_ID );
    CPPUNIT_ASSERT( copyMsg->getId() == dataMsg->getId() );
}

void MessageTest::testBusId()
{
    CPPUNIT_ASSERT( defaultMsg->getBusId() == 0 );

    defaultMsg->setBusId( BUS_ID );
    CPPUNIT_ASSERT( defaultMsg->getBusId() == BUS_ID );
    CPPUNIT_ASSERT( idMsg->getBusId() == BUS_ID );
    CPPUNIT_ASSERT( dataMsg->getBusId() == BUS_ID );
    CPPUNIT_ASSERT( copyMsg->getBusId() == dataMsg->getBusId() );
}

void MessageTest::testData()
{
    DataVector indata;
    for ( int i = 1; i < 9; ++i ) 
        indata.push_back( static_cast< byteType >( i ) );

    defaultMsg->setData( indata );
    const DataVector outdata = defaultMsg->getData( );

    CPPUNIT_ASSERT( indata.size() == outdata.size() );
    CPPUNIT_ASSERT( indata == outdata );

    // Test inserting a vector that's too large.
    try {
        indata.push_back( 0x09 );
        defaultMsg->setData( indata );
        CPPUNIT_ASSERT( false );
    } catch (...) {
        CPPUNIT_ASSERT( true );
    }

}

void MessageTest::testAssignmentAndEquality()
{
    CPPUNIT_ASSERT( *defaultMsg == *defaultMsg );

    *defaultMsg = *idMsg;
    CPPUNIT_ASSERT( *defaultMsg == *idMsg );

    // Test based on id alone
    defaultMsg->setId( MSG_ID + 1 );
    CPPUNIT_ASSERT( *defaultMsg != *idMsg );

    // Test based on busId alone
    *defaultMsg = *idMsg;
    CPPUNIT_ASSERT( *defaultMsg == *idMsg );
    defaultMsg->setBusId( BUS_ID + 1 );
    CPPUNIT_ASSERT( *defaultMsg != *idMsg );

    // Test based on data alone
    *defaultMsg = *idMsg;
    CPPUNIT_ASSERT( *defaultMsg == *idMsg );
    DataVector data;
    data.push_back( 0x01 );
    defaultMsg->setData( data );
    CPPUNIT_ASSERT( *defaultMsg != *idMsg );

    CPPUNIT_ASSERT( *dataMsg == *copyMsg );
}

void MessageTest::testInsertionOperators()
{
    carma::canbus::Message msg;
    const unsigned char achar[4] = { 'a', 'b', 'c', 'd' };
    const unsigned short aushort =  0xaa;
    const short ashort = 0xbb;

    msg << achar[0] << achar[1] << achar[2] << achar[3] << aushort << ashort;

    DataVector data = msg.getData( );

    CPPUNIT_ASSERT( data.size() == 8 );
    CPPUNIT_ASSERT( dataToUbyte(data) == achar[0] );
    CPPUNIT_ASSERT( data.size() == 7 );
    CPPUNIT_ASSERT( dataToUbyte(data) == achar[1] );
    CPPUNIT_ASSERT( data.size() == 6 );
    CPPUNIT_ASSERT( dataToUbyte(data) == achar[2] );
    CPPUNIT_ASSERT( data.size() == 5 );
    CPPUNIT_ASSERT( dataToUbyte(data) == achar[3] );
    CPPUNIT_ASSERT( data.size() == 4 );
    CPPUNIT_ASSERT( dataToUshort(data) == aushort );
    CPPUNIT_ASSERT( data.size() == 2 );
    CPPUNIT_ASSERT( dataToShort(data) == ashort );
    CPPUNIT_ASSERT( data.size() == 0 );

    data.clear();
    msg.setData( data ); 
    
    const unsigned long aulong = 0xcafe;
    const long along = 0xbabe;

    msg << aulong << along;

    data = msg.getData();

    CPPUNIT_ASSERT( data.size() == 8 );
    CPPUNIT_ASSERT( dataToUlong(data) == aulong );
    CPPUNIT_ASSERT( data.size() == 4 );
    CPPUNIT_ASSERT( dataToLong(data) == along );
    CPPUNIT_ASSERT( data.size() == 0 );

    data.clear();
    msg.setData( data ); 

    // Test floats and doubles. In general testing equality of floating 
    // point numbers is a bad idea.  I go out of my way here to make
    // sure that the tested numbers are exactly representable in order to
    // prevent equality failures due to extra bits in floating point 
    // registers as opposed to values pulled from memory.
    const float afloat = 1.00; // Exactly representable.

    msg << afloat << afloat;
    
    data = msg.getData();

    CPPUNIT_ASSERT( data.size() == 8 );
    CPPUNIT_ASSERT( dataToFloat( data ) == afloat );
    CPPUNIT_ASSERT( data.size() == 4 );
    CPPUNIT_ASSERT( dataToFloat( data ) == afloat );
    CPPUNIT_ASSERT( data.size() == 0 );
    
    data.clear();
    msg.setData( data ); 

    const double adouble = 2.5; // Exactly representable.

    msg << adouble;
    
    data = msg.getData();

    CPPUNIT_ASSERT( data.size() == 8 );
    CPPUNIT_ASSERT( dataToDouble( data ) == adouble );
    CPPUNIT_ASSERT( data.size() == 0 );

}
