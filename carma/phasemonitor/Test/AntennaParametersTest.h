/** @file
 * $Id: AntennaParametersTest.h,v 1.3 2006/11/29 23:25:28 colby Exp $
 *
 * CppUnit test fixture for carma::phasemonitor::AntennaParameters
 *
 * Author: Colby Gutierrez-Kraybill
 * Version: $Revision: 1.3 $
 * $Date: 2006/11/29 23:25:28 $
 *
 */
#ifndef CARMA_PHASEMONITOR_ANTENNAPARAMETERS_TEST_H
#define CARMA_PHASEMONITOR_ANTENNAPARAMETERS_TEST_H

#include "carma/phasemonitor/AntennaParameters.h"

#ifndef CPPUNIT_STD_NEED_ALLOCATOR
#define CPPUNIT_STD_NEED_ALLOCATOR 0
#endif

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

/**
 * carma::phasemonitor::AntennaParameters test class for CppUnit.
 */
class AntennaParametersTest : public CppUnit::TestFixture
{
  public:	

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();

    CPPUNIT_TEST_SUITE(AntennaParametersTest);

    CPPUNIT_TEST( testParameters );
    CPPUNIT_TEST( testFileLength );
    CPPUNIT_TEST( testOstream );

    CPPUNIT_TEST_SUITE_END();

    /** test the sample parameters to see if the file is read correctly */
    void testParameters();

    /** test the file for the proper number of rows... */
    void testFileLength();

    /** test the ostream output, for show really... */
    void testOstream();

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

    carma::phasemonitor::AntennaParameters* _tAP;
    char *_nameTemplate;
    bool _tearDownNameTemplate;
    bool _tearDownUnlinkNameTemplate;


};
#endif // CARMA_PHASEMONITOR_ANTENNAPARAMETERS_TEST_H
