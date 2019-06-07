/** 
 * Test application for GpioDio class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.1 $
 * $Date: 2013/02/26 18:59:38 $
 * $Id: tGpioDio.cc,v 1.1 2013/02/26 18:59:38 abeard Exp $
 */
#include "carma/canbus/GpioDio.h"

#include "carma/util/Program.h"

#include <unistd.h>
#include <iostream>

using namespace carma;
using namespace carma::canbus;
using namespace carma::util;
using namespace std;

/** 
 * @version $Revision: 1.1 $
 * 
 * @description
 * Test for GpioDio class. Will continuously run through a test sequence.
 * \nExample: tSocketCan basePin=236
 *
 * @usage tGpioDio [basePin=<basePin>]
 * 
 * @key basePin @noDefault i Base pin of gpio device.
 *
 * @logger TEST_FACILITY carma.test.canbus.tSocketCan
 */
int Program::main( )
{
    if ( !parameterWasSpecified( "basePin" ) ) {
        cerr << "Base pin must be specified." << endl;
        return 1;
    }

    const int basePin( getIntParameter( "basePin" ) );
    vector< int > basePins;
    basePins.push_back( basePin );
    GpioDio gpio( basePins, true );
    gpio.clear();

    cout << "Beginning test sequence..." << endl;
    while ( true ) {
        cout << "resetHi..." << endl;
        gpio.resetHi();
        sleep( 2 );
        cout << "resetLo..." << endl;
        gpio.resetLo();
        sleep( 2 );
        cout << "powerOn..." << endl;
        gpio.powerOn();
        sleep( 2 );
        cout << "powerOff..." << endl;
        gpio.powerOff();
        sleep( 2 );
        cout << "reservedHi()..." << endl;
        gpio.reservedHi();
        sleep( 2 );
        cout << "reservedLo()..." << endl;
        gpio.reservedLo();
        sleep( 2 );
    }

    return 0;
}

