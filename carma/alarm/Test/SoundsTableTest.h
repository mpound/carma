/** @file
 * $Id: SoundsTableTest.h,v 1.1 2006/11/30 10:56:55 colby Exp $
 *
 * CppUnit test fixture for carma::alarm::SoundsTable
 *
 * Author: Colby Gutierrez-Kraybill
 * Version: $Revision: 1.1 $
 * $Date: 2006/11/30 10:56:55 $
 *
 */
#ifndef CARMA_ALARM_SOUNDSTABLE_TEST_H
#define CARMA_ALARM_SOUNDSTABLE_TEST_H

#include "carma/alarm/SoundsTable.h"

#ifndef CPPUNIT_STD_NEED_ALLOCATOR
#define CPPUNIT_STD_NEED_ALLOCATOR 0
#endif

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

/**
 * carma::alarm::SoundsTable test class for CppUnit.
 */
class SoundsTableTest : public CppUnit::TestFixture
{
  public:	

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();

    CPPUNIT_TEST_SUITE(SoundsTableTest);

    CPPUNIT_TEST( testGetSoundFileByName );
    CPPUNIT_TEST( testGetSoundFullPathFileByName );

    CPPUNIT_TEST_SUITE_END();

    /** test get a default sound by name */
    void testGetSoundFileByName();

    /** test getting full path info to a default sound by name */
    void testGetSoundFullPathFileByName();


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

    carma::alarm::SoundsTable *_tST;
    bool _tearDownUnlinkNameTemplate;

};
#endif // CARMA_ALARM_SOUNDSTABLE_TEST_H
