/** @file
 * $Id: AntennaNameResolverTest.h,v 1.2 2006/12/02 08:03:48 colby Exp $
 *
 * CppUnit test fixture for carma::antenna::bima::AntennaNameResolverTest
 *
 * Author: Colby Gutierrez-Kraybill
 * Version: $Revision: 1.2 $
 * $Date: 2006/12/02 08:03:48 $
 *
 */
#ifndef CARMA_ANTENNA_BIMA_ANTENNANAMERESOLVER_TEST_H
#define CARMA_ANTENNA_BIMA_ANTENNANAMERESOLVER_TEST_H

#include "carma/antenna/bima/AntennaNameResolver.h"

#ifndef CPPUNIT_STD_NEED_ALLOCATOR
#define CPPUNIT_STD_NEED_ALLOCATOR 0
#endif

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

/**
 * carma::antenna::bima::AntennaNameResolverTest test class for CppUnit.
 */
class AntennaNameResolverTest : public CppUnit::TestFixture
{
  public:	

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();

    CPPUNIT_TEST_SUITE(AntennaNameResolverTest);

    CPPUNIT_TEST( testConstructors );

    CPPUNIT_TEST_SUITE_END();

    /** exercise */
    void testConstructors();

};
#endif // CARMA_ANTENNA_BIMA_ANTENNANAMERESOLVER_TEST_H
