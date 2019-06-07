/** @file
 * CppUnit test fixture for carma::canbus::Device class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.6 $
 * $Date: 2004/03/19 21:15:09 $
 * $Id: MasterTest.h,v 1.6 2004/03/19 21:15:09 abeard Exp $
 */
#ifndef MASTER_TEST_H
#define MASTER_TEST_H

#include "carma/canbus/Master.h"

#include "DeviceTest.h"

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

namespace carma {
namespace canbus {
namespace test {

/**
 * Helper class for CppUnit MasterTest class.
 */
class CppUnitMaster : public carma::canbus::Master {
    public:
        
        CppUnitMaster();

        virtual ~CppUnitMaster();

        // Expose the following to test them.
        int getOnlineNodeCount();

        int getOfflineNodeCount();
        
        std::map<carma::canbus::msgType, std::string> getControls() const;
        
        void updateStatus();

        void softwareReset();

        void setTime();

        carma::canbus::Device * getDevice(carma::canbus::nodeType node);

    private:

       static void *runThreadEntry(void *arg);
       pthread_t runThreadId_; 
};
      

/**
 * CppUnit test class for testing the carma::canbus::Master base class.
 */
class MasterTest : public CppUnit::TestFixture {
public:	

	void setUp();

	void tearDown();
	 
	CPPUNIT_TEST_SUITE(MasterTest);

    CPPUNIT_TEST( testAll );

	CPPUNIT_TEST_SUITE_END();
	
    void testAll();
    void testGetDevice();
    void testNodeCount();
    void testSoftwareReset();
    void testControls();
	
private:

    CppUnitMaster *master_;
    
	
};
}}} // End namespace carma::canbus::test
#endif
