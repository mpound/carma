#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"

#include "MasterTest.h"
#include <pthread.h>
#include <unistd.h>

using namespace carma::canbus;
using namespace carma::canbus::test;
using namespace std;

// -----------------------------------------------------------------------------
CppUnitMaster::CppUnitMaster() 
{
    // Declare three devices
    // Nodes 1 and 3 will throw exceptions at certain locations in the master
    // code, these are handled exceptions and normal execution should continue.
    CppUnitDevice *dev;
    for (nodeType n = 1; n < 7; n++) {
        dev = new CppUnitDevice(n, *this);
        addDevice(dev);
    }

    // Run in seperate thread.
    int status;
    status = pthread_create(&runThreadId_, NULL,
            runThreadEntry, (void *)this);
    if (status != 0) {
        throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
            "CppUnitMaster::run()- Unable to start CppUnitMaster run thread " +
                (const string) strerror(status));
    }
}       
    
// -----------------------------------------------------------------------------
CppUnitMaster::~CppUnitMaster()
{
    void *result;
    int status;

    // Cancel run thread
    status = pthread_cancel(runThreadId_);
    if (status != 0) {
        cerr << "Error cancelling run thread." << endl;
    }

    // Block on it and wait for return value...
    status = pthread_join(runThreadId_, &result);
    if (status != 0) {
        cerr << "Error joining on run thread." <<endl;
    }
    CPPUNIT_ASSERT(result == PTHREAD_CANCELED);
    
    Master::stop();
}

// -----------------------------------------------------------------------------
int CppUnitMaster::getOnlineNodeCount()
{
    return Master::getOnlineNodeCount();
}

// -----------------------------------------------------------------------------
int CppUnitMaster::getOfflineNodeCount()
{
    return Master::getOfflineNodeCount();
}

// -----------------------------------------------------------------------------
map<msgType, string> CppUnitMaster::getControls() const
{
    map<msgType, string> ctrls;
    return ctrls;
}

// -----------------------------------------------------------------------------
Device * CppUnitMaster::getDevice(nodeType node)
{
    return Master::getDevice(12, node);
}

// -----------------------------------------------------------------------------
void CppUnitMaster::updateStatus()
{
    // no-op
}

// -----------------------------------------------------------------------------
void CppUnitMaster::softwareReset()
{
    Master::softwareReset();
}

// -----------------------------------------------------------------------------
void CppUnitMaster::setTime()
{
    Master::setTime();
}

// -----------------------------------------------------------------------------
void * CppUnitMaster::runThreadEntry(void *arg)
{
    Master *This = (Master *) arg;
    This->run();
    return EXIT_SUCCESS;
}


// -----------------------------------------------------------------------------
// Implementation of MasterTest
// -----------------------------------------------------------------------------
void MasterTest::setUp() 
{
    master_ = new CppUnitMaster();
    
}

// -----------------------------------------------------------------------------
void MasterTest::tearDown()
{
    delete master_;
}

// -----------------------------------------------------------------------------
void MasterTest::testAll()
{
    // Allow it time to setup and run.
    // The sleep goes here so that main will block on it.
    cerr << endl << "Running test master... " << endl;
    cerr << "This may take a few seconds." << endl;
    sleep(6); // Allow time to run...
    testNodeCount();
    testGetDevice();
    testSoftwareReset();
    testControls();
    master_->setTime();
}
    
// -----------------------------------------------------------------------------
void MasterTest::testGetDevice()
{
    Device *dev = master_->getDevice(3);
    CPPUNIT_ASSERT(dev != NULL);
    CPPUNIT_ASSERT(dev->getNode() == 3);
}

// -----------------------------------------------------------------------------
void MasterTest::testNodeCount()
{
    CPPUNIT_ASSERT(master_->getOnlineNodeCount() == 0);
    CPPUNIT_ASSERT(master_->getOfflineNodeCount() == 3);
}

// -----------------------------------------------------------------------------
void MasterTest::testSoftwareReset()
{
    master_->softwareReset();
    CPPUNIT_ASSERT(true);
}

// -----------------------------------------------------------------------------
void MasterTest::testControls()
{
    map<msgType, string> ctrls = master_->getControls();
    CPPUNIT_ASSERT(ctrls.size() == 0);
}
