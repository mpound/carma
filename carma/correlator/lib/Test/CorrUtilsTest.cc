/** 
 * @file 
 *
 * $Id: CorrUtilsTest.cc,v 1.8 2013/12/02 14:04:51 mpound Exp $
 *
 * CppUnit test fixture for carma::services::CorrUtilsQuantities
 * and its derived classes.
 *
 * Author: Marc Pound
 * Version: $Revision: 1.8 $
 * $Date: 2013/12/02 14:04:51 $
 *
 */
#include "CorrUtilsTest.h"
#include "carma/correlator/lib/corrUtils.h"
#include "carma/util/NotFoundException.h"
#include <map>


using namespace carma;
using namespace carma::correlator;
using namespace CppUnit;
using namespace std;

void CorrUtilsTest::setUp() 
{   
}

void CorrUtilsTest::tearDown() 
{   

}

void CorrUtilsTest::testHwType()
{
    unsigned int bandNo;

    for( bandNo = 1; bandNo < 9; bandNo++ ) {
        const hardwareType hType = hwType( bandNo );
        CPPUNIT_ASSERT( hType == HARDWARE_TYPE_CARMA );
    }
    for( bandNo = 9; bandNo < 25; bandNo++ ) {
        const hardwareType hType = hwType( bandNo );
        CPPUNIT_ASSERT( hType == HARDWARE_TYPE_COBRA);
    }
    for( bandNo = 25; bandNo < 40; bandNo++ ) {
        const hardwareType hType = hwType( bandNo );
        CPPUNIT_ASSERT( hType == HARDWARE_TYPE_C3G);
    }

    CPPUNIT_ASSERT( hwType( 0 ) == HARDWARE_TYPE_UNKNOWN );
    CPPUNIT_ASSERT( hwType( 45 ) == HARDWARE_TYPE_UNKNOWN );

}

void CorrUtilsTest::testActualBW()
{
    map<string,float> bw;
    bw.insert( make_pair("500MHz",500.0) );
    bw.insert( make_pair("250MHz",250.0) );
    bw.insert( make_pair("125MHz",125.0) );
    bw.insert( make_pair("62MHz",62.5) );
    bw.insert( make_pair("31MHz",31.25) );
    bw.insert( make_pair("8MHz",7.8125) );
    bw.insert( make_pair("2MHz",1.953125) );
    map<string,float>::const_iterator iter = bw.begin();
    float retVal;
    while( iter != bw.end() ) {
        retVal = actualBandwidth( iter->first );
        CPPUNIT_ASSERT ( retVal == iter->second );
        ++iter;
    }
    try {
        retVal = actualBandwidth("foobar");
    } catch ( util::NotFoundException & nfe ) {
        cout << " Caught exception properly for invalid bandwidth string "
            << endl;
        CPPUNIT_ASSERT( true );
        return;
    }
    cout << " Failed to catch exception properly for invalid bandwidth string "
         << endl;
    CPPUNIT_ASSERT( false );
}


void CorrUtilsTest::testNumExpectedCorrBands()
{
    CPPUNIT_ASSERT( numExpectedCorrBands("LL",CORR_WIDEBAND) == 1 );
    CPPUNIT_ASSERT( numExpectedCorrBands("RR",CORR_SPECTRAL) == 1 );
    CPPUNIT_ASSERT( numExpectedCorrBands("DUALPOL",CORR_SPECTRAL) == 2 );
    CPPUNIT_ASSERT( numExpectedCorrBands("FULLSTOKES",CORR_SPECTRAL) == 2 );
    CPPUNIT_ASSERT( numExpectedCorrBands("MAXSENS_DUALPOL",CORR_SPECTRAL) == 1 );
    CPPUNIT_ASSERT( numExpectedCorrBands("MAXSENS_DUALPOL",CORR_WIDEBAND) == 1 );
    CPPUNIT_ASSERT( numExpectedCorrBands("MAXSENS_CARMA23",CORR_SPECTRAL) == 2 );
    CPPUNIT_ASSERT( numExpectedCorrBands("MAXSENS_CARMA23",CORR_WIDEBAND) == 1 );
    CPPUNIT_ASSERT( numExpectedCorrBands("FOOBAR",CORR_SPECTRAL) == 0 );
}

void CorrUtilsTest::testNumExpectedAstroChans()
{
    // no matter what WIDEBAND is 15 channels
    CPPUNIT_ASSERT(
      numExpectedAstroChans(CORR_WIDEBAND, CORR_BW_500MHZ, CORR_2BIT, CORR_SINGLEPOL ) 
                   == 15 ); 

    CPPUNIT_ASSERT(
      numExpectedAstroChans(CORR_WIDEBAND, CORR_BW_500MHZ, CORR_4BIT, CORR_SINGLEPOL ) 
                   == 15 ); 

    CPPUNIT_ASSERT(
      numExpectedAstroChans(CORR_WIDEBAND, CORR_BW_8MHZ, CORR_4BIT, CORR_SINGLEPOL ) 
                   == 15 ); 

    CPPUNIT_ASSERT(
      numExpectedAstroChans(CORR_SPECTRAL, CORR_BW_500MHZ, CORR_2BIT, CORR_SINGLEPOL ) 
                   == 95 ); 
    CPPUNIT_ASSERT(
      numExpectedAstroChans(CORR_SPECTRAL, CORR_BW_500MHZ, 
                            CORR_2BIT, CORR_CARMA23 ) == 47 ); 

    CPPUNIT_ASSERT(
      numExpectedAstroChans(CORR_SPECTRAL, CORR_BW_500MHZ, CORR_4BIT, CORR_SINGLEPOL ) 
                   == 15 ); 

    CPPUNIT_ASSERT(
      numExpectedAstroChans(CORR_ANY , CORR_BW_125MHZ, CORR_4BIT, CORR_SINGLEPOL ) 
                   == 0 ); 
    CPPUNIT_ASSERT(
      numExpectedAstroChans(CORR_ANY , CORR_BW_125MHZ, CORR_4BIT, CORR_CARMA23 ) 
                   == 0 ); 

    CPPUNIT_ASSERT(
      numExpectedAstroChans(CORR_SPECTRAL , CORR_BW_8MHZ, CORR_3BIT, CORR_SINGLEPOL ) 
                   == 319 ); 
    CPPUNIT_ASSERT(
      numExpectedAstroChans(CORR_SPECTRAL , CORR_BW_8MHZ, CORR_3BIT, CORR_CARMA23 ) 
                   == 159 ); 

    CPPUNIT_ASSERT(
      numExpectedAstroChans(CORR_SPECTRAL, CORR_BW_2MHZ, CORR_2BIT, CORR_SINGLEPOL ) 
                   == 383 ); 
    CPPUNIT_ASSERT(
      numExpectedAstroChans(CORR_SPECTRAL, CORR_BW_2MHZ, CORR_2BIT, CORR_CARMA23 ) 
                   == 191 ); 

    CPPUNIT_ASSERT(
      numExpectedAstroChans(CORR_SPECTRAL, CORR_BW_31MHZ, CORR_4BIT, CORR_SINGLEPOL ) 
                   == 159 ); 
    CPPUNIT_ASSERT(
      numExpectedAstroChans(CORR_SPECTRAL, CORR_BW_31MHZ, CORR_4BIT, CORR_FULLPOL ) 
                   == 79 ); 

    CPPUNIT_ASSERT(
      numExpectedAstroChans(CORR_SPECTRAL, CORR_BW_62MHZ, CORR_4BIT, CORR_SINGLEPOL ) 
                   == 127 ); 
    CPPUNIT_ASSERT(
      numExpectedAstroChans(CORR_SPECTRAL, CORR_BW_62MHZ, CORR_4BIT, CORR_CARMA23 ) 
                   == 63 ); 

    CPPUNIT_ASSERT(
      numExpectedAstroChans(CORR_SPECTRAL, CORR_BW_250MHZ, CORR_3BIT, CORR_DUALPOL ) 
                   == 79 ); 
    CPPUNIT_ASSERT(
      numExpectedAstroChans(CORR_SPECTRAL, CORR_BW_250MHZ, CORR_3BIT, CORR_CARMA23 ) 
                   == 39 ); 
}

void CorrUtilsTest::testCorrelatorEfficiency()
{
    float eff= correlatorEfficiency(CORR_2BIT);
    CPPUNIT_ASSERT( eff < 0.873 && eff > 0.871 );
    eff= correlatorEfficiency(CORR_3BIT);
    CPPUNIT_ASSERT( eff < 0.964 && eff > 0.962 );
    eff= correlatorEfficiency(CORR_4BIT);
    CPPUNIT_ASSERT( eff <  0.985 && eff > 0.983 );
}
    
