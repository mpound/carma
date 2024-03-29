/** 
 * @file
 *
 * CppUnit test fixture for carma/util/lib/corrUtils.h
 * $Id: CorrUtilsTest.h,v 1.2 2014/06/03 19:45:36 mpound Exp $
 *
 * Author: Marc Pound
 *
 */
#ifndef CARMA_UTIL_CORRUTILS_TEST_H
#define CARMA_UTIL_CORRUTILS_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>


/**
 * carma/util/corrUtils class for CppUnit.
 */
class CorrUtilsTest : public CppUnit::TestFixture {
public:    

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();
     
    CPPUNIT_TEST_SUITE( CorrUtilsTest );

    CPPUNIT_TEST( testHwType );
    CPPUNIT_TEST( testActualBW );
    CPPUNIT_TEST( testNumExpectedCorrBands );
    CPPUNIT_TEST( testNumExpectedAstroChans );
    CPPUNIT_TEST( testCorrelatorEfficiency );

    CPPUNIT_TEST_SUITE_END();
    
    /** test the hardware type mapping to band number */
    void testHwType( void );

    /** test the mapping of bandwidth string to float value */
    void testActualBW( void );

    /** test expected number of correlator bands vs. astroband config */
    void testNumExpectedCorrBands( void );

    /** test expected number of astroband channels vs. ab config */
    void testNumExpectedAstroChans( void );  

    /** verify the correff constants */
    void testCorrelatorEfficiency( void );

private:
   

};
#endif //CARMA_UTIL_CORRUTILS_TEST_H
