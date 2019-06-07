/**@file
 * CppUnit Test class for WbdcMaster class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.9 $
 * $Date: 2012/01/25 22:26:07 $
 * $Id: WbdcMasterTest.h,v 1.9 2012/01/25 22:26:07 abeard Exp $
 */
#ifndef WBDCMASTER_TEST_H
#define WBDCMASTER_TEST_H

// System includes
#include <pthread.h>

// Carma includes
#include "carma/corba/corba.h"
#include "carma/corba/Server.h"
#include "carma/downconverter/wideband/WbdcMaster.h"
#include "carma/downconverter/Test/ClientTest.h"

// CppUnit includes
#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

namespace carma {
namespace corba {
    class Server;
}} // namespace carma::corba

/**
 * CppUnit test for the WbdcMaster class. 
 *
 * This test class tests the majority of the Wideband Downconverter CANbus
 * code by first running for several seconds in emulation mode and then 
 * invoking all methods on all published DOs.  
 */
class WbdcMasterTest : public CppUnit::TestFixture {
public:

    WbdcMasterTest();

    void setUp();
    void tearDown();

    CPPUNIT_TEST_SUITE(WbdcMasterTest);
    
    CPPUNIT_TEST( testDeviceInternals );
    CPPUNIT_TEST( testSystemControls );
    CPPUNIT_TEST( testDownconverterControls );
    CPPUNIT_TEST( testQuadModControls );
    CPPUNIT_TEST( testNoiseSourceControls );
    CPPUNIT_TEST( testLoMonitorControls );

    CPPUNIT_TEST_SUITE_END();

    void testDeviceInternals();
    void testSystemControls();
    void testDownconverterControls();
    void testQuadModControls();
    void testNoiseSourceControls();
    void testLoMonitorControls();

private:
    static void *testSystemThreadEntry(void *arg);
    static void *testDownconverterThreadEntry(void *arg);
    static void *testQuadModThreadEntry(void *arg);
    static void *testNoiseSourceThreadEntry(void *arg);
    static void *testLoMonitorThreadEntry(void *arg);
    
   carma::downconverter::WbdcMaster *master_;
   ClientTest *client_;
   carma::corba::Server & server_;
};
#endif
