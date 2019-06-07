/** @file
 * $Id: WSTest.h,v 1.2 2011/05/04 20:36:23 iws Exp $
 *
 * CppUnit test fixture for carma::environment::WS
 *
 * Author: Peter Teuben
 * Version: $Revision: 1.2 $
 * $Data: $
 *
 */
#ifndef CARMA_ENVIRONMENT_WS_TEST_H
#define CARMA_ENVIRONMENT_WS_TEST_H

#include <carma/environment/WS.h>

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

/**
 * carma::environment::WS test class for CppUnit.
 */
class WSTest : public CppUnit::TestFixture {
public:

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();

    CPPUNIT_TEST_SUITE(WSTest);
    // put your CPPUNIT_TEST() in here
    CPPUNIT_TEST_SUITE_END();


private:

    // This must be long double because we are taking
    // differences resulting in a very small number.
    // Double representation "fluff" in the last digit
    // will move up or down in the mantissa depending
    // on the actual value in the comparison. Therefore
    // we normalize the comparison by the return value.
    long double normalizedDiff;

    // How small we allow diff to be and still pass a test.
    double epsilon;

    carma::environment::WS* tWS;

};
#endif //CARMA_ENVIRONMENT_WS_TEST_H
