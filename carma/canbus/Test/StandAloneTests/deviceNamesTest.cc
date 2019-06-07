/** @file  
 * Simple test of carma::canbus::DeviceNames class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.2 $
 * $Date: 2006/04/14 20:34:49 $
 * $Id: deviceNamesTest.cc,v 1.2 2006/04/14 20:34:49 tcosta Exp $
 */

#include "carma/canbus/DeviceNames.h"
#include "carma/util/Program.h"

#include <iostream>

using namespace carma::canbus;
using namespace carma::util;
using namespace std;

/**
 * @version $Revision: 1.2 $
 *
 * @usage deviceNamesTest 
 *
 * @description
 * Simple test of carma::canbus::DeviceNames class. Prints out known 
 * CAN device names.
 * 
 * @noKeys
 *
 * @logger TEST_FACILITY carma.test.canbus.StandAloneTests.deviceNamesTest
 */

int Program::main( ) 
{
    // Print out all unknown device apis
    for ( apiType i = 0; i < 512; i++ ) {
        if ( !DeviceNames::isRegistered(i) )
            cout << "API " << i << ": " << DeviceNames::getName(i) << endl;
    }

    // Now print out all known devices.
    for ( apiType i = 0; i < 512; i++ ) {
        if ( DeviceNames::isRegistered(i) )
            cout << "API " << i << ": " << DeviceNames::getName(i) << endl;
    }
    return EXIT_SUCCESS;
}
 
