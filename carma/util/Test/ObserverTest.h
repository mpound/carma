
#ifndef CARMA_UTIL_OBSERVERTEST_H
#define CARMA_UTIL_OBSERVERTEST_H

/** @file
 *
 * CppUnit test fixture for carma::util error exception classes.
 *
 * @author: Steve Scott
 *
 * $Id: ObserverTest.h,v 1.1 2005/01/06 00:01:04 scott Exp $
 * 
 *
 */

#include "carma/util/Observable.h"

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

class ObserverTest : public CppUnit::TestFixture {
public:	

    void setUp();
    void tearDown();
	 
    CPPUNIT_TEST_SUITE(ObserverTest);

    CPPUNIT_TEST( observerRegistry );
    CPPUNIT_TEST( registerObservers );
    CPPUNIT_TEST( notifyObservers );

    CPPUNIT_TEST_SUITE_END();

	
    void observerRegistry();
    void registerObservers();
    void notifyObservers();
    carma::util::Observable* getObservable2(){ return observable2_;}
    
    
    class TestObserver : public carma::util::Observer
    {
    public:
        TestObserver():eventCount_(0), o2EventCount_(0){};
        void observerUpdate(carma::util::Observable& o) { 
            eventCount_++; 
            totalEventCount_++;
            if (&o == observable_)o2EventCount_++;
        }
        int  eventCount() { return eventCount_; }
        void  setObservable(carma::util::Observable* observable) 
            { observable_ = observable; }
        int o2EventCount() { return o2EventCount_; }
        int  totalEventCount() { return totalEventCount_; }
        int  totalEventCount(int t) { return totalEventCount_=t; }
    private:
        carma::util::Observable* observable_;
        carma::util::Observable* observable2_;
        int eventCount_;
        int o2EventCount_;
        static int totalEventCount_;
    };
private:
    carma::util::Observable* observable_;
    carma::util::Observable* observable2_;
    TestObserver*   o1_;
    TestObserver*   o2_;
    TestObserver*   o3_;
    TestObserver*   o4_;
};

#endif // CARMA_UTIL_OBSERVERTEST_H


