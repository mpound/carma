
#include "UtilitiesTest.h"

#include "carma/canbus/Utilities.h"
#include <vector>

using namespace std;
using namespace carma::canbus;
using namespace carma::canbus::test;

// -----------------------------------------------------------------------------
void UtilitiesTest::setUp() 
{
    appId = createId(true, 64, 23, 0x110);
    engId = createEngId(false, 64, 23, 0x210);
}


// -----------------------------------------------------------------------------
void UtilitiesTest::tearDown()
{}

// -----------------------------------------------------------------------------
void UtilitiesTest::testMode()
{
    CPPUNIT_ASSERT(getMode(appId) == APPLICATION);
    CPPUNIT_ASSERT(getMode(engId) == ENGINEERING);
}

// -----------------------------------------------------------------------------
void UtilitiesTest::testHost()
{
    CPPUNIT_ASSERT(isToHost(appId));
    CPPUNIT_ASSERT((!isToHost(engId)));
}
            
// -----------------------------------------------------------------------------
void UtilitiesTest::testId()
{
    apiType api;
    nodeType node;
    msgType mid;
    
    fromId(api, node, mid, appId);
    CPPUNIT_ASSERT(api == 64);
    CPPUNIT_ASSERT(node == 23);
    CPPUNIT_ASSERT(mid == 0x110);
}
   
// -----------------------------------------------------------------------------
void UtilitiesTest::testEngId()
{
    boardType bt;
    serialNumberType sn;
    msgType mid;

    fromEngId(bt, sn, mid, engId);
    CPPUNIT_ASSERT(bt == 64);
    CPPUNIT_ASSERT(sn == 23);
    CPPUNIT_ASSERT(mid == 0x210);
}

// -----------------------------------------------------------------------------
void UtilitiesTest::testUbyte()
{
    vector<byteType> data;
    uByteToData(data, 0xa1);
    CPPUNIT_ASSERT(dataToUbyte(data) == 0xa1);
}
    
// -----------------------------------------------------------------------------
void UtilitiesTest::testUshort()
{
    vector<byteType> data;
    uShortToData(data, 0x1234);
    CPPUNIT_ASSERT(dataToUshort(data) == 0x1234);

    // Signed shorts should be the bottom half of unsigned shorts
    unsigned short us = 10;
    uShortToData(data, us);
    CPPUNIT_ASSERT(dataToShort(data) == us);
}

// -----------------------------------------------------------------------------
void UtilitiesTest::testUlong()
{
    vector<byteType> data;
    uLongToData(data, 0x56781234);
    CPPUNIT_ASSERT(dataToUlong(data) == 0x56781234);
}

// -----------------------------------------------------------------------------
void UtilitiesTest::testSshort()
{
    vector<byteType> data;
    sShortToData(data, 0x5678);
    CPPUNIT_ASSERT(dataToShort(data) == 0x5678);
}

// -----------------------------------------------------------------------------
void UtilitiesTest::testSlong()
{
    vector<byteType> data;
    sLongToData(data, 0x15263748);
    CPPUNIT_ASSERT(dataToLong(data) == 0x15263748);
}

// -----------------------------------------------------------------------------
void UtilitiesTest::testFloat()
{
    // Need a number with an exact fp representation.
    vector<byteType> data;
    floatToData(data, 1.0);
    CPPUNIT_ASSERT(dataToFloat(data) == 1.0);
}

// -----------------------------------------------------------------------------
void UtilitiesTest::testDouble()
{
    // Need a number with an exact fp representation.
    vector<byteType> data;
    doubleToData(data, 1.0);
    CPPUNIT_ASSERT(dataToDouble(data) == 1.0);
}

// -----------------------------------------------------------------------------
void UtilitiesTest::testPadding()
{
    vector<byteType> data;
    padWithZeros(data);
    CPPUNIT_ASSERT(data.size() == 8);
    for (vector<byteType>::size_type i = 0; i < data.size(); i++) {
        CPPUNIT_ASSERT(dataToUbyte(data) == 0);
    }
}

// -----------------------------------------------------------------------------
void UtilitiesTest::testFastTime()
{
    timespec ts = calculateTimeToNextHalfSec();
    CPPUNIT_ASSERT(ts.tv_sec == 0);
    CPPUNIT_ASSERT(ts.tv_nsec <= 500000000);
}

// -----------------------------------------------------------------------------
void UtilitiesTest::testSlowTime()
{
    timespec ts = calculateTimeToNextSlowBoundary();
    CPPUNIT_ASSERT(ts.tv_sec <= 5);
}
