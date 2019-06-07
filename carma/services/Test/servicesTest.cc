
/**
 * @file
 * Implementation of the unit tests for the services library
 *
 * @author: Marc Pound
 *
 * $Id: servicesTest.cc,v 1.10 2010/02/18 22:14:09 abeard Exp $
 *                  
 * $CarmaCopyright$
 *
 */

// @version $Revision: 1.10 $
// @usage   servicesTest <no parameters>
// @description 
// Test for the services library
//
// @noKeys
// @logger DEFAULT_FACILITY carma.services.Test.servicesTest
//

#include <cppunit/TextTestRunner.h>
#include "carma/util/Program.h"

#include "AstroTest.h"
#include "AtmosphereTest.h"
#include "ConformableTest.h"
#include "EphemerisTest.h"
#include "ExceptionTest.h"
#include "GlobalTest.h"
#include "PhysicalTest.h"
#include "SourceCheckerTest.h"
#include "TableTest.h"
#include "UnitsTest.h"
#include "ObservatoryTest.h"

#include <unistd.h>

using namespace carma::util;

int Program::main() 
{
    CppUnit::TextTestRunner runner;

    // Add in test suites here.  For any class Foo, you
    // must create carma::services::test::FooTest,
    // following the existing examples.  Also don't forget
    // to add the #include "FooTest.h" statement above.
    
    //runner.addTest( AstroTimeTest::suite() ); // not ready yet
    runner.addTest( AtmosphereTest::suite() );
    runner.addTest( AstroTest::suite() );
    runner.addTest( ConformableTest::suite() );
    runner.addTest( EphemerisTest::suite() );
    runner.addTest( ExceptionTest::suite() );
    runner.addTest( GlobalTest::suite() );
    runner.addTest( ObservatoryTest::suite() );
    runner.addTest( PhysicalTest::suite() );
    runner.addTest( SourceCheckerTest::suite() );
    runner.addTest( TableTest::suite() );
    runner.addTest( UnitsTest::suite() );

    bool ok = runner.run();

    return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}

