
/** @file
 *
 * CppUnit test fixture implementation for carma::util observer classes.
 *
 * @author: Steve Scott
 *
 * $Id: ObserverTest.cc,v 1.2 2005/01/06 00:55:35 scott Exp $
 * 
 *
 */
 
#include <iostream>
#include <vector>

#include "ObserverTest.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ObserverRegistry.h"

using namespace carma::util;
using namespace CppUnit;
using namespace std;


// Uses char* constructor
void ObserverTest::setUp()
{
    observable_ = new Observable();
    observable2_ = new Observable();
    o1_         = new TestObserver();
    o2_         = new TestObserver();
    o3_         = new TestObserver();
    o4_         = new TestObserver();
} 


void ObserverTest::tearDown() 
{
    delete observable_;
    delete observable2_;
    delete o1_; o1_ = 0;
    delete o2_; o2_ = 0;
    delete o3_; o3_ = 0;
    delete o4_; o4_ = 0;
}

// Tests registration and unregistration by deletion...
void ObserverTest::observerRegistry()
{
    ObserverRegistry& oreg = ObserverRegistry::instance();
    // The test observer construction should register 4 observers
    CPPUNIT_ASSERT(oreg.getNumObservers() == 4);
    
    // Test IDs
    CPPUNIT_ASSERT(o1_->regID() == 1);
    CPPUNIT_ASSERT(o4_->regID() == 4);
 
    // Remove an observer; test num observers and that ids are unchanged   
    delete o3_; o3_ = 0;
    CPPUNIT_ASSERT(oreg.getNumObservers() == 3);
    CPPUNIT_ASSERT(o4_->regID() == 4);
        
    // Add a new observer; test num observers and id  
    o3_ = new TestObserver();
    CPPUNIT_ASSERT(oreg.getNumObservers() == 4);
    CPPUNIT_ASSERT(o3_->regID() == 5);
}


// Reg and unreg with observable
void ObserverTest::registerObservers()
{
    // Check that we know where we are...
    int firstID = 6;
    CPPUNIT_ASSERT(o1_->regID() == firstID);
    
    // Should have no observers
    CPPUNIT_ASSERT( observable_->getNumObservers() == 0);
    
    // Register one and check
    observable_->registerObserver(*o1_);
    CPPUNIT_ASSERT( observable_->getNumObservers() == 1);
    
    // Register more and check
    observable_->registerObserver(*o2_);
    observable_->registerObserver(*o3_);
    observable_->registerObserver(*o4_);
    CPPUNIT_ASSERT( observable_->getNumObservers() == 4);
    
    // Unregister, reregister...
    observable_->unregisterObserver(*o2_);
    CPPUNIT_ASSERT( observable_->getNumObservers() == 3);
    // Unreg twice
    bool caughtException = false;
    try {
        observable_->unregisterObserver(*o2_);
    }
    catch (ErrorException& e) { 
        caughtException = true; 
        //cerr << e << endl;
    }
    CPPUNIT_ASSERT( caughtException == true);
    observable_->registerObserver(*o2_);
    CPPUNIT_ASSERT( observable_->getNumObservers() == 4);
    // Register twice
    caughtException = false;
    try {
        observable_->registerObserver(*o2_);
    }
    catch (exception& e) { 
        caughtException = true; 
        //cout << e << endl;
    }
    CPPUNIT_ASSERT( caughtException == true);
    
    // Delete one; shouldn't affect observable until we do a notify
    delete o3_; o3_ = 0;
    CPPUNIT_ASSERT( observable_->getNumObservers() == 4);
    observable_->notifyObservers();
    CPPUNIT_ASSERT( observable_->getNumObservers() == 3);

    // Delete more; shouldn't affect observable until we do a notify
    delete o1_; o1_ = 0;
    delete o4_; o4_ = 0;
    CPPUNIT_ASSERT( observable_->getNumObservers() == 3);
    observable_->notifyObservers();
    CPPUNIT_ASSERT( observable_->getNumObservers() == 1);
    delete o2_; o2_ = 0;
    observable_->notifyObservers();
    CPPUNIT_ASSERT( observable_->getNumObservers() == 0);
    observable_->notifyObservers();
    CPPUNIT_ASSERT( observable_->getNumObservers() == 0);
    
}

void ObserverTest::notifyObservers()
{
 
    // Register one and check
    observable_->registerObserver(*o1_);
    CPPUNIT_ASSERT( observable_->getNumObservers() == 1);
    // Check that event count is zero'ed
    o1_->totalEventCount(0);
    CPPUNIT_ASSERT( o1_->totalEventCount() == 0);

    // Notify and see if we get one
    observable_->notifyObservers();
    CPPUNIT_ASSERT( o1_->eventCount() == 1);
    CPPUNIT_ASSERT( o1_->totalEventCount() == 1);
    
    // Register more and check
    observable_->registerObserver(*o2_);
    observable_->registerObserver(*o3_);
    observable_->registerObserver(*o4_);
    CPPUNIT_ASSERT( observable_->getNumObservers() == 4);
   
    // Notify and see if we get more
    observable_->notifyObservers();
    CPPUNIT_ASSERT( o1_->eventCount() == 2);
    CPPUNIT_ASSERT( o2_->eventCount() == 1);
    CPPUNIT_ASSERT( o3_->eventCount() == 1);
    CPPUNIT_ASSERT( o4_->eventCount() == 1);
    CPPUNIT_ASSERT( o1_->totalEventCount() == 5);

    // Delete one and check
    delete o2_; o2_ = 0;
    observable_->notifyObservers();
    CPPUNIT_ASSERT( o1_->eventCount() == 3);
    CPPUNIT_ASSERT( o3_->eventCount() == 2);
    CPPUNIT_ASSERT( o4_->eventCount() == 2);
    CPPUNIT_ASSERT( o1_->totalEventCount() == 8);
    observable_->notifyObservers();
    CPPUNIT_ASSERT( o1_->eventCount() == 4);
    CPPUNIT_ASSERT( o3_->eventCount() == 3);
    CPPUNIT_ASSERT( o4_->eventCount() == 3);
    CPPUNIT_ASSERT( o1_->totalEventCount() == 11);

    // Check with a second observable
    observable2_->registerObserver(*o3_);
    o3_->setObservable(observable2_);
    observable2_->notifyObservers();
    CPPUNIT_ASSERT( o1_->eventCount() == 4);
    CPPUNIT_ASSERT( o3_->eventCount() == 4);
    CPPUNIT_ASSERT( o3_->o2EventCount() == 1);
    CPPUNIT_ASSERT( o4_->eventCount() == 3);
    CPPUNIT_ASSERT( o1_->totalEventCount() == 12);
    
    // And notify for someone registered with two observables
    observable_->notifyObservers();
    CPPUNIT_ASSERT( o1_->eventCount() == 5);
    CPPUNIT_ASSERT( o3_->eventCount() == 5);
    observable2_->notifyObservers();
    CPPUNIT_ASSERT( o1_->eventCount() == 5);
    CPPUNIT_ASSERT( o3_->eventCount() == 6);

}

int ObserverTest::TestObserver::totalEventCount_ = 0;





