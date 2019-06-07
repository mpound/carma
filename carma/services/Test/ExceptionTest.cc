/** 
 * @file 
 *
 * $Id: ExceptionTest.cc,v 1.2 2008/11/24 14:22:06 mpound Exp $
 *
 * CppUnit test fixture for carma::services:: exception classes
 *
 * Author: Marc Pound
 *
 */
#include "ExceptionTest.h"
#include "carma/services/Angle.h"
#include "carma/services/Ephemeris.h"
#include "carma/services/Frequency.h"
#include "carma/services/Pressure.h"
#include "carma/services/Source.h"
#include "carma/services/SourceCatalog.h"
#include "carma/services/SourceChecker.h"
#include "carma/services/SpectralLineCatalog.h"
#include "carma/services/Types.h"
#include "carma/services/Units.h"
#include "carma/services/Velocity.h"
#include "carma/services/CatalogEntryNotFoundException.h"
#include "carma/services/ConformabilityException.h"        
#include "carma/services/EphemerisException.h"
#include "carma/services/SpectralLineNotFoundException.h"
#include "carma/services/SourceNotFoundException.h"
#include "carma/services/UnsupportedCoordSysException.h"
#include "carma/util/ErrorException.h"

using namespace carma::services;
using namespace carma::util;
using namespace CppUnit;
using namespace std;

void ExceptionTest::setUp() 
{   

}

void ExceptionTest::tearDown() 
{   

}

void ExceptionTest::testExceptions()
{
    // Ephemeris
    Ephemeris e;
    const string badSource("fooooo");
    const string bodyNameTooLong("aaaaaaaabbbbbbbbbbbbbbbbccccccccccccccdddddddddddddddddddeeeeeeeeeeeeeefffffffffffffffggggggggggggghhhhhhhhhhhhhhhiiiiiiiiiiijjjjjjjjjjjkkkkkkkkkkkkkkllllllllllllllll");

    try {
	e.setSource( badSource );
	fail();
    } catch ( const SourceNotFoundException & ex ) {
	cout << "Correctly caught SourceNotFound: "
	     << ex.getMessage()
	     << endl;
	pass();
    } catch ( ... ) {
	cout << "Caught unexpected exception from Ephemeris"
	     << endl;
	fail();
    }


    try {
	e.setBody( 1, 1, bodyNameTooLong );
	fail();
    } catch ( const EphemerisException & ex ) {
	cout << "Correctly caught EphemerisException: "
	     << ex.getMessage()
	     << endl;
	pass();
    } catch ( ... ) {
	cout << "Caught unexpected exception from Ephemeris"
	     << endl;
	fail();
    }


    // Catalogs
    try {
	SourceCatalog sc;
	sc.open( SourceCatalog::defaultCatalog() );
	sc.lookup( badSource );
	fail();
    } catch ( const CatalogEntryNotFoundException & ex ) {
	cout << "Correctly caught CatalogEntryNotFound: "
	     << ex.getMessage()
	     << endl;
	pass();
    } catch (const carma::util::NotFoundException & nex) {
	cout << "Problem opening "
	     << SourceCatalog::defaultCatalog()
	     << ": "
	     << nex.getMessage()
	     << endl;
	fail();
    } catch ( ... ) {
	cout << "Caught unexpected exception from SourceCatalog"
	     << endl;
	fail();
    }


    try {
	const string badLine("xxxx(17-21)");
	const Frequency freqLow(100,"GHz");
	const Frequency freqHigh(110,"GHz");
	SpectralLineCatalog sc;
	sc.open( "conf/catalogs/SpectralLine.cat" );
	sc.lookup( badLine, freqLow, freqHigh );
	fail();
    } catch ( const SpectralLineNotFoundException & ex ) {
	cout << "Correctly caught SpectralLineNotFound: "
	     << ex.getMessage()
	     << endl;
	pass();
    } catch (const carma::util::NotFoundException & nex) {
	cout << "Problem opening conf/catalogs/SpectralLine.cat: " 
	     << nex.getMessage()
	     << endl;
	fail();
    } catch ( ... ) {
	cout << "Caught unexpected exception from SpectralLineCatalog "
	     << endl;
	fail();
    }


    // Conformable quantities
    try {
	const Pressure p(14,"GHz");
	double value = p.millibar();
	value = 0;//shut up compiler warning
	fail();
    } catch ( const ConformabilityException & ex ) {
	cout << "Correctly caught Conformability: "
	     << ex.getMessage()
	     << endl;
	pass();
    } catch ( ... ) {
	cout << "Caught unexpected exception from non-conforming quantity."
	     << endl;
	fail();
    }

    // Coordinate Systems.
    try {
	const coordSysType unsupportedCsys = COORDSYS_GALACTIC;
	const Source s( "BadCoordSys", Angle(0,"radian"), Angle(0,"radian"),
		Velocity(0,"km/s"), Angle(0,"radian"),
		unsupportedCsys );
	SourceChecker checker;
	checker.setSource( s );
	double value = checker.riseTime();
	cout << "Csys test should not have gotten here.";
	value = 0;//shut up compiler warning
	fail();
    } catch ( const UnsupportedCoordSysException & ex ) {
	cout << "Correctly caught UnsupportedCoordSys: "
	     << ex.getMessage()
	     << endl;
	pass();
    } catch ( const ErrorException & ee ) {
	cout << "Incorrectly caught ErrorException: "
	     << ee.getMessage()
	     << endl;
	fail();
    } catch ( ... ) {
	cout << "Caught unexpected exception from Source with "
	     << " bad coordinate system."
	     << endl;
	fail();
    }

}

void ExceptionTest::pass()
{
	CPPUNIT_ASSERT( 1 == 1 );
}

void ExceptionTest::fail()
{
	CPPUNIT_ASSERT( 1 == 0 );
}

