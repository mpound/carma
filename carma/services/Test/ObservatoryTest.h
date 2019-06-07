
/** @file
 * $Id: ObservatoryTest.h,v 1.1 2005/06/24 15:54:05 mpound Exp $
 *
 * CppUnit test fixture for carma::services::Observatory,
 * carma::services::Location, and carma::services::Pad
 *
 * @author: Marc Pound
 * @version: $Revision: 1.1 $
 *
 */
#ifndef CARMA_SERVICES_OBSERVATORY_TEST_H
#define CARMA_SERVICES_OBSERVATORY_TEST_H

#include "carma/services/Observatory.h"

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

/**
 * carma::services::Table test class for CppUnit.
 */
class ObservatoryTest : public CppUnit::TestFixture {
public:	

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();
	 
    CPPUNIT_TEST_SUITE(ObservatoryTest);

    CPPUNIT_TEST( testObservatory );

    CPPUNIT_TEST_SUITE_END();
	
    /** test simple observatory creation and access */
    void testObservatory();
	
};
#endif //CARMA_SERVICES_OBSERVATORY_TEST_H
