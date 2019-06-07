/** @file
 * $Id: PhaseMonitorDeviceTest.h,v 1.4 2006/11/30 01:50:13 colby Exp $
 *
 * CppUnit test fixture for carma::phasemonitor::PhaseMonitorDevice
 *
 * Author: Colby Gutierrez-Kraybill
 * Version: $Revision: 1.4 $
 * $Date: 2006/11/30 01:50:13 $
 *
 */
#ifndef CARMA_PHASEMONITOR_PHASEMONITORDEVICE_TEST_H
#define CARMA_PHASEMONITOR_PHASEMONITORDEVICE_TEST_H

#include "carma/phasemonitor/PhaseMonitorDevice.h"

#ifndef CPPUNIT_STD_NEED_ALLOCATOR
#define CPPUNIT_STD_NEED_ALLOCATOR 0
#endif

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

/**
 * carma::phasemonitor::PhaseMonitorDevice test class for CppUnit.
 */
class PhaseMonitorDeviceTest : public CppUnit::TestFixture
{
  public:	

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();

    CPPUNIT_TEST_SUITE(PhaseMonitorDeviceTest);

    CPPUNIT_TEST( testEmulate );
    CPPUNIT_TEST( testDevFileName );
    CPPUNIT_TEST( testIsReplay );
    CPPUNIT_TEST( testTestBadVolts );
    CPPUNIT_TEST( testBadStartOfReply );
    CPPUNIT_TEST( testSleepSomeNanos );
    CPPUNIT_TEST( testCommand );
    CPPUNIT_TEST( testInquire );
    CPPUNIT_TEST( testQueryVoltages );
    CPPUNIT_TEST( testQueryTemperatureC );
    CPPUNIT_TEST( testPMDECopyAndString );
    CPPUNIT_TEST( testOstream );
    CPPUNIT_TEST( testConvertStringToFloat );

    CPPUNIT_TEST_SUITE_END();

    /** test that emulation gets set */
    void testEmulate();

    /** test that device file name in emulation mode matches known pattern */
    void testDevFileName();

    /** test that badVolts test gets set (required for emulate && ! replay */
    void testTestBadVolts();

    /** test that bad return reply throws exception */
    void testBadStartOfReply();

    /** test that replay of data is set up */
    void testIsReplay();

    /** test sleep some nanos */
    void testSleepSomeNanos();

    /** test sending command, in emulate mode this is a no-op */
    void testCommand();

    /** test replay of data in emulate mode */
    void testInquire();

    /** test that replay of data is parsed correctly,
      * same parser used to regular operations */
    void testQueryVoltages();

    /** test that replay of data is parsed correctly,
      * same parser used to regular operations */
    void testQueryTemperatureC();

    /** test PhaseMonitorDeviceException overloaded c'tors */
    void testPMDECopyAndString();

    /** test ostream operator */
    void testOstream();

    /** test convertStringToFloat */
    void testConvertStringToFloat();

  private:

    // This must be long double because we are taking
    // differences resulting in a very small number.
    // Double representation "fluff" in the last digit
    // will move up or down in the mantissa depending
    // on the actual value in the comparison. Therefore
    // we normalize the comparison by the return value.
    long double _normalizedDiff; 

    // How small we allow diff to be and still pass a test.
    double _epsilon; 

    carma::phasemonitor::PhaseMonitorDevice* _tPMD;

    char *_nameTemplate;
    bool _tearDownNameTemplate;
    bool _tearDownUnlinkNameTemplate;

};
#endif // CARMA_PHASEMONITOR_PHASEMONITORDEVICE_TEST_H
