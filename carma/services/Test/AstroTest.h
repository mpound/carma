/** @file
 * $Id: AstroTest.h,v 1.2 2008/04/21 18:00:09 mpound Exp $
 *
 * CppUnit test fixture for carma::services::Astro 
 * and carma::services::PlanetTemperature
 *
 * Author: Marc Pound
 * Version: $Revision: 1.2 $
 * $Data: $
 *
 */
#ifndef CARMA_SERVICES_ASTRO_TEST_H
#define CARMA_SERVICES_ASTRO_TEST_H


#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

/**
 * carma::services::Astro test class for CppUnit.
 */
class AstroTest : public CppUnit::TestFixture {
public:	

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();
	 
    CPPUNIT_TEST_SUITE(AstroTest);

    CPPUNIT_TEST( testConstants );
    CPPUNIT_TEST( testPlanetTemperature );

    CPPUNIT_TEST_SUITE_END();
	
    /** test the constant values */
    void testConstants();
    /** test the planet temperature interpolation */
    void testPlanetTemperature();
	
};
#endif //CARMA_SERVICES_ASTRO_TEST_H
