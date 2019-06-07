
/** @file
 *
 * CppUnit test fixture implementation for carma::util observer classes.
 *
 * @author: Steve Scott
 *
 * $Id: IPQtest.cc,v 1.1 2006/01/04 20:41:02 scott Exp $
 * 
 *
 */
 
#include <iostream>
#include <vector>

#include "IPQtest.h"
#include "carma/util/ErrorException.h"
#include "carma/util/IPQwriter.h"
#include "carma/util/IPQfileWriter.h"

using namespace carma::util;
using namespace CppUnit;
using namespace std;

void IPQtest::setUp() {
    fname_ = "test";
}

void IPQtest::tearDown() {
}

//IPQtest::IPQtest() {}

void IPQtest::bufferDestructor()
{
    bool first = true;
    
    for (int i=0; i < numDestructions_; i++) {
        try {
            IPQwriter<DATA>* ipq = 
                new IPQwriter<DATA>(fname_, true, numElements_);
            delete(ipq);
        } catch (BaseException& e) {
            if (first) cerr << endl;
            first = false;
            cerr << e << endl;
            CPPUNIT_ASSERT(false);
        }
    }
}

void IPQtest::fileDestructor()
{
    bool first = true;
    
    for (int i=0; i < numDestructions_; i++) {
        //cerr << "IPQfileWriter:" << i << endl;
        try {
            IPQfileWriter<DATA>* ipq = 
                new IPQfileWriter<DATA>(fname_, true, numElements_);
            delete(ipq);
        } catch (BaseException& e) {
            if (first) cerr << endl;
            first = false;
            cerr << e << endl;
            CPPUNIT_ASSERT(false);
        }
    }
}





