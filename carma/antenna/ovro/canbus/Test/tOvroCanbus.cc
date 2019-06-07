#include "carma/antenna/ovro/canbus/OvroMaster.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"

#include <iostream>

using namespace carma::antenna::ovro;
using namespace carma::monitor;
using namespace carma::util;
using namespace std;

/**
 * @version $Id: tOvroCanbus.cc,v 1.4 2007/10/12 21:27:05 abeard Exp $
 *
 * @usage tOvroCanbus
 * 
 * @description 
 * Test application for ovro canbus classes.  This exercises each canbus::Device
 * derivative's simulateMsg functions.
 *
 * @noKeys
 *
 * @logger TEST_FACILITY carma.test.antenna.ovro.tOvroCanbus
 */
int 
Program::main( ) 
try {

    const short unsigned int antNo = 1;
    const bool simOfflineNodes = true;
    const int runTime = 10; // Seconds

    OvroSubsystem monitorSubsys( antNo );

    OvroMaster master( antNo, 
                       simOfflineNodes, 
                       monitorSubsys );

    master.start( );

    sleep( runTime );

    master.stop( );

    return 0;

} catch ( const std::exception & err ) {
    cerr << err << endl;
    return 1;
} catch ( ... ) {
    return 1;
}
