/** @file
 * $Id: JanzMessageTest.cc,v 1.1 2012/07/13 18:55:30 abeard Exp $
 *
 * CppUnit test fixture implementation for carma::canbus::JanzMessage class.
 *
 * Author: Andy Beard
 * Version: $Revision: 1.1 $
 * $Data: $
 *
 */
#include <vector>

#include "carma/canbus/Test/StandAloneTests/JanzMessageTest.h"
#include "carma/canbus/JanzMessage.h"
#include "carma/canbus/Utilities.h"

using namespace std;
using namespace carma;
using namespace carma::canbus;
using namespace carma::canbus::test;

void JanzMessageTest::setUp() 
{
    vector<byteType> data;
    data.insert(data.end(), 0xA1);
    data.insert(data.end(), 0xa2);

    stdMsg = new canbus::JanzMessage(canbus::JanzMessage::STANDARD);
    xtdMsg = new canbus::JanzMessage(canbus::JanzMessage::EXTENDED);
    idMsg = new canbus::JanzMessage(0x12345, 0);
    dataMsg = new canbus::JanzMessage(0x123456, data, 0);
}

void JanzMessageTest::tearDown() 
{
    delete stdMsg;
    delete xtdMsg;
    delete idMsg;
    delete dataMsg;
}

void JanzMessageTest::testMessageType() 
{
    CPPUNIT_ASSERT(stdMsg->getMessageType() == canbus::JanzMessage::STANDARD);
    CPPUNIT_ASSERT(xtdMsg->getMessageType() == canbus::JanzMessage::EXTENDED);
    CPPUNIT_ASSERT(idMsg->getMessageType() == canbus::JanzMessage::EXTENDED);
    CPPUNIT_ASSERT(dataMsg->getMessageType() == canbus::JanzMessage::EXTENDED);
}

void JanzMessageTest::testId()
{
    stdMsg->setId(0x123);
    CPPUNIT_ASSERT(stdMsg->getId() == 0x123);

    xtdMsg->setId(idMsg->getId());
    CPPUNIT_ASSERT(xtdMsg->getId() == idMsg->getId());
}

void JanzMessageTest::testBusId()
{
    // Confirm constructed value is correct.
    CPPUNIT_ASSERT(xtdMsg->getBusId() == 0);

    // Confirm newly set value is correct.	
    xtdMsg->setBusId(1);
    CPPUNIT_ASSERT(xtdMsg->getBusId() == 1);
}

void JanzMessageTest::testRawMessage()
{
    canbus::JanzMessage sMsg(stdMsg->getRawMessage(), stdMsg->getBusId());
    canbus::JanzMessage xMsg(xtdMsg->getRawMessage(), xtdMsg->getBusId());

    // Confirm types are the same.
    CPPUNIT_ASSERT(sMsg.getMessageType() == stdMsg->getMessageType());
    CPPUNIT_ASSERT(xMsg.getMessageType() == xtdMsg->getMessageType());

    // Confirm Ids are the same.
    CPPUNIT_ASSERT(sMsg.getId() == stdMsg->getId());
    CPPUNIT_ASSERT(xMsg.getId() == xtdMsg->getId());

    // Confirm Bus Ids are the same...
    CPPUNIT_ASSERT(sMsg.getBusId() == stdMsg->getBusId());
    CPPUNIT_ASSERT(xMsg.getBusId() == xtdMsg->getBusId());

    // Confirm data is the same...
    CPPUNIT_ASSERT(sMsg.getData() == stdMsg->getData());
    CPPUNIT_ASSERT(xMsg.getData() == xtdMsg->getData());
}

void JanzMessageTest::testAssignment()
{
    canbus::JanzMessage msg;
    msg = *stdMsg;
    CPPUNIT_ASSERT(msg == *stdMsg);
    msg = *xtdMsg;
    CPPUNIT_ASSERT(msg == *xtdMsg);
    msg = *idMsg;
    CPPUNIT_ASSERT(msg == *idMsg);
    msg = *dataMsg;
    CPPUNIT_ASSERT(msg == *dataMsg);
}

void JanzMessageTest::testEquality()
{
    CPPUNIT_ASSERT(*stdMsg == *stdMsg);
    CPPUNIT_ASSERT(*xtdMsg == *xtdMsg);
    CPPUNIT_ASSERT(!(*stdMsg == *xtdMsg));
    CPPUNIT_ASSERT(*stdMsg != *xtdMsg);
}

void JanzMessageTest::testData()
{
    vector<byteType> data = dataMsg->getData();
    vector<byteType> idData;

    CPPUNIT_ASSERT(data.size() == 2);
    CPPUNIT_ASSERT(data[0] == 0xA1);
    CPPUNIT_ASSERT(data[1] == 0xA2);

    idMsg->setData(data);
    idData = idMsg->getData();
    CPPUNIT_ASSERT(idData.size() == 2);
    CPPUNIT_ASSERT(idData[0] == 0xA1);
    CPPUNIT_ASSERT(idData[1] == 0xA2);

}
