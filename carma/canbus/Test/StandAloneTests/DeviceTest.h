/** @file
 * CppUnit test fixture for carma::canbus::Device class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.10 $
 * $Date: 2012/07/25 17:51:32 $
 * $Id: DeviceTest.h,v 1.10 2012/07/25 17:51:32 abeard Exp $
 */
#ifndef DEVICE_TEST_H
#define DEVICE_TEST_H

#include "carma/canbus/Device.h"
#include "carma/canbus/JanzCanIo.h"

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

#include <string>

namespace carma {
namespace canbus {
namespace test {

/**
 * Helper carma::canbus::Device class derivative for use with CppUnit 
 * DeviceTest class.
 *
 * The CppUnitDevice is a fake CAN Device.
 */
class CppUnitDevice : public carma::canbus::Device {
    public:

        CppUnitDevice(nodeType node, CanOutput &io);

        std::map<msgType, std::string> getControls() const;

        std::map<msgType, std::string> getHalfSecMonitors() const;

        std::map<msgType, std::string> getSlowMonitors() const;

        void processMsg(msgType messageId, std::vector<byteType>& data, 
            bool sim);

        carma::canbus::Message simulateMsg(msgType messageId);

    private:

        enum monitors {
            THROW_EXCEPTION = 0x110,
            BLANKING_PACKET_1 = 0x111
        };

};

/**
 * Helper class for use with the CppUnitDevice class.
 */
class CppUnitCanIo : public carma::canbus::JanzCanIo {
    public:
    CppUnitCanIo() : JanzCanIo() {};
    CppUnitDevice * createCppUnitDevice() {
        return new CppUnitDevice(1, *this);
    };
};


/**
 * CppUnit test class for the carma::canbus::Device class. 
 */
class DeviceTest : public CppUnit::TestFixture {
public:	

	void setUp();

	void tearDown();
	 
	CPPUNIT_TEST_SUITE(DeviceTest);

	CPPUNIT_TEST( testApi );
	CPPUNIT_TEST( testNode );
	CPPUNIT_TEST( testBusId );
	CPPUNIT_TEST( testState );
	CPPUNIT_TEST( testLastRxTime );
    CPPUNIT_TEST( testControls );
    CPPUNIT_TEST( testReset );

	CPPUNIT_TEST_SUITE_END();
	
    void testApi();
    void testNode();
    void testBusId();
    void testState();
    void testLastRxTime();
    void testControls();
    void testReset();
	
private:

    CppUnitCanIo *io;
    CppUnitDevice *dev;
	
};
}}} // End namespace carma::canbus::test
#endif
