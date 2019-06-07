/** @file
 * $Id: CanIoTest.cc,v 1.4 2004/03/05 20:00:04 abeard Exp $
 *
 * CppUnit test fixture implementation for carma::canbus::Message class.
 *
 * Author: Andy Beard
 * Version: $Revision: 1.4 $
 * $Data: $
 *
 */
#include <map>
#include <vector>

#include "CanIoTest.h"

#include "carma/canbus/Utilities.h"
#include "carma/canbus/Types.h"

using namespace carma;
using namespace carma::canbus;
using namespace carma::canbus::test;
using namespace std;

// -----------------------------------------------------------------------------
void CanIoTest::setUp() 
{
    canIo_ = new TestCanIo();
}

// -----------------------------------------------------------------------------
void CanIoTest::tearDown()
{
    delete canIo_;
}

// -----------------------------------------------------------------------------
void CanIoTest::testPostMessage()
{
    carma::canbus::Message msg;
    msg.setBusId(0);
    canIo_->postMessage(msg);
    msg.setBusId(ALL_BUSSES);
    canIo_->postMessage(msg);
}

// -----------------------------------------------------------------------------
void CanIoTest::testGetMessage()
{
    carma::canbus::Message msg, returnMsg;
    vector<byteType> data;
    data.insert(data.end(), 'a');
    data.insert(data.end(), 'n');
    data.insert(data.end(), 'd');
    data.insert(data.end(), 'y');
    
    msg.setId(createId(true, 128, 1, 0x100));
    msg.setBusId(0);
    msg.setData(data);

    canIo_->queueMessage(msg);
    returnMsg = canIo_->getMessage();
    CPPUNIT_ASSERT(msg == returnMsg);
}

// -----------------------------------------------------------------------------
void CanIoTest::testStatus()
{
    map<busIdType, carma::canbus::busStatusType> stat = canIo_->getBusStatus();
    map<busIdType, carma::canbus::busStatusType>::iterator bi;

    // Iterate through busses.
    for (bi = stat.begin(); bi != stat.end(); bi++) {
        CPPUNIT_ASSERT(bi->second.rxErrors == 0);
        CPPUNIT_ASSERT(bi->second.txErrors == 0);
        CPPUNIT_ASSERT(bi->second.slowMsgsLost == 0);
        CPPUNIT_ASSERT(bi->second.fastMsgsLost == 0);
        CPPUNIT_ASSERT(bi->second.state == NO_ERRORS);
    }
}
