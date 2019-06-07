
#include "cppunit/TextTestRunner.h"

#include "carma/util/Program.h"
#include "carma/canbus/Types.h"
#include "carma/util/ErrorException.h"

#include "CanIoTest.h"
#include "DeviceTest.h"
#include "MasterTest.h"
#include "JanzMessageTest.h"
#include "MessageTest.h"
#include "UtilitiesTest.h"

#include <unistd.h>

#include <iostream>

using namespace carma::canbus::test;
using namespace std;

/**
 * @version $Revision: 1.22 $
 *
 * @description
 * Non-hardware test program for canbus library.
 *
 * @usage 
 * standAloneTest
 * 
 * @noKeys
 *
 * @logger TEST_FACILITY carma.test.canbus.StandAloneTests.standAloneTest
 */

int carma::util::Program::main() 
{
    try {

        bool result;
        CppUnit::TextTestRunner runner;
        runner.addTest( JanzMessageTest::suite() );
        runner.addTest( MessageTest::suite() );
        runner.addTest( UtilitiesTest::suite() );
        runner.addTest( DeviceTest::suite() );
        runner.addTest( CanIoTest::suite() );
        runner.addTest( MasterTest::suite() );

        result = runner.run();
        sleep(5);
        return (result ? EXIT_SUCCESS : EXIT_FAILURE);
    } catch (carma::util::ErrorException &err) {
        cerr << err.what();
        return EXIT_FAILURE;
    } catch (...) {
        cerr << "main() - Unknown exception caught." << endl;
        return EXIT_FAILURE;
    }
    return EXIT_FAILURE;
}
