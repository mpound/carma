/** @file
 * $Id: DeviceTest.cc,v 1.13 2005/07/07 00:01:26 abeard Exp $
 *
 * CppUnit test fixture implementation for carma::canbus::Message class.
 *
 * Author: Andy Beard
 * Version: $Revision: 1.13 $
 * $Data: $
 *
 */
#include <map>

#include "DeviceTest.h"

#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"

using namespace carma;
using namespace carma::canbus;
using namespace carma::canbus::test;
using namespace std;

// -----------------------------------------------------------------------------
CppUnitDevice::CppUnitDevice(nodeType node, CanOutput &io) : 
    Device(12, node, io) 
{
    setBusId(0);
    
}
    
// -----------------------------------------------------------------------------
map<msgType, string> CppUnitDevice::getControls() const 
{
    return Device::getControls();
}

// -----------------------------------------------------------------------------
map<msgType, string> CppUnitDevice::getHalfSecMonitors() const
{
    map<msgType, string> mons;
    // Setup such that processMsg will throw an exception if the node is
    // 1 or 6.  If the node is 2, simulate msg triggers the exception.
    // This will be used to test error handling in the TestMaster class.
    if (node_ == 1 || node_ == 2 || node_ == 6) {
        mons[THROW_EXCEPTION] = "CppUnitDevice::EXCEPTION";
    } else {
        mons[BLANKING_PACKET_1] = "CppUnitDevice::BLANKING_PACKET_1";
    }
    return mons;
}

// -----------------------------------------------------------------------------
map<msgType, string> CppUnitDevice::getSlowMonitors() const
{
    map<msgType, string> mons;
    return mons;
}

// -----------------------------------------------------------------------------
void CppUnitDevice::processMsg(msgType messageId, vector<byteType>&data, 
    bool sim)
{
    switch (messageId) {
        case THROW_EXCEPTION : 
            throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
                "CppUnitDevice::processMsg() - Throwing an exception for "
                "testing - don't be frightened.");
            break;
        case BLANKING_PACKET_1 :
            break;
        default:
            break;
    }
}


// -----------------------------------------------------------------------------
carma::canbus::Message CppUnitDevice::simulateMsg(msgType messageId)
{
    carma::canbus::Message msg;
    switch (messageId) {
        case THROW_EXCEPTION :
            if (node_ == 2) {
                throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
                    "CppUnitDevice::simulateMsg() - Throwing an exception for "
                    "test purposes - don't be frightened.");
            } else {
                msg.setId(createId(true , api_, node_, THROW_EXCEPTION));
            }
            break;
        case BLANKING_PACKET_1 :
            msg.setId(createId(true, api_, node_, BLANKING_PACKET_1));
            break;
        default:
            break;
    }
    return msg;
}

// -----------------------------------------------------------------------------
void DeviceTest::setUp() 
{
    io = new CppUnitCanIo(); 
    dev = io->createCppUnitDevice();
}

// -----------------------------------------------------------------------------
void DeviceTest::tearDown() 
{
	delete dev;
    delete io;
}

// -----------------------------------------------------------------------------
void DeviceTest::testApi()
{
    CPPUNIT_ASSERT(dev->getApi() == 12);
}

// -----------------------------------------------------------------------------
void DeviceTest::testNode()
{
    CPPUNIT_ASSERT(dev->getNode() == 1);
}

// -----------------------------------------------------------------------------
void DeviceTest::testBusId()
{
    busIdType ogBusId = dev->getBusId(); 
    dev->setBusId(1);
    CPPUNIT_ASSERT(dev->getBusId() == 1);
    dev->setBusId(ogBusId);
}

// -----------------------------------------------------------------------------
void DeviceTest::testState()
{
    dev->setState(ONLINE);
    CPPUNIT_ASSERT(dev->getState() == ONLINE);
}

// -----------------------------------------------------------------------------
void DeviceTest::testLastRxTime()
{
    dev->setLastRxTime(1.0);
    CPPUNIT_ASSERT(dev->getLastRxTime() == 1.0);
}

// -----------------------------------------------------------------------------
void DeviceTest::testControls()
{
    map<msgType, string> ctrls = dev->getControls();
    CPPUNIT_ASSERT(ctrls.size() == 5);
}

// -----------------------------------------------------------------------------
void DeviceTest::testReset()
{
     dev->reset();
}
