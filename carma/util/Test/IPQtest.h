
#ifndef CARMA_UTIL_IPQTEST_H
#define CARMA_UTIL_IPQTEST_H

/** @file
 *
 * CppUnit test fixture for carma::util error exception classes.
 *
 * @author: Steve Scott
 *
 * $Id: IPQtest.h,v 1.3 2006/01/04 23:56:15 scott Exp $
 * 
 *
 */

#include "carma/util/IPQbuffer.h"
#include "carma/util/IPQfileWriter.h"

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

class IPQtest : public CppUnit::TestFixture {
public:	

    void setUp();
    void tearDown();
	 
    CPPUNIT_TEST_SUITE(IPQtest);

    CPPUNIT_TEST(bufferDestructor);
    CPPUNIT_TEST(fileDestructor);

    CPPUNIT_TEST_SUITE_END();

	// Tests constructors/destructors	
    void bufferDestructor();
    void fileDestructor();
    
private:
    // Need more than 1024 to check for open file leaks
    static const int numDestructions_ = 2000;
    static const int size_            = 123;
    static const int numElements_     = 10;
    std::string fname_;
    // Data that is put into the ipq
    struct DATA {
        char data[size_];
    };
};

#endif // CARMA_UTIL_IPQTEST_H


