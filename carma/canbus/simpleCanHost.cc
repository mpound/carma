/** @file
 * Very basic CAN host application. 
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.1 $
 * $Date: 2010/06/18 22:48:37 $
 * $Id: simpleCanHost.cc,v 1.1 2010/06/18 22:48:37 abeard Exp $
 */

// C++ Standard Library
#include <iostream>

// Carma includes
#include "carma/util/Program.h"
#include "carma/util/ErrorException.h"
#include "carma/canbus/MasterOfNone.h"

using namespace carma::canbus;
using namespace carma::util;
using namespace std;

/**
 * @version $Revision: 1.1 $
 *
 * @description
 * \n Simple CAN host application.  This application performs the most basic
 * of CAN bus actions. It reads messages from the CANbus(ses) and places 
 * them into DirectCan queues (and hence makes them available to a canOverIp
 * server) while sending time stamps out at the normal 10 second rate.  It 
 * will be most useful to those who only wish to use canOverIp for CAN msg
 * processing and control.  It requires only CAN hardware and drivers.
 *
 * @usage simpleCanHost [board=[0-15] [canbus=[0-1]]
 *
 * @key board   0 i The Janz CAN/Dio Board Id.
 * @key canbus -1 i Canbus to control (both Bus0 and Bus1 by default).
 * @key echo true bool Echo all transmitted messages back to host.
 * @logger DEFAULT_FACILITY carma.canbus.simplecanhost
 */
int Program::main() {
    
    try {

        int board = getIntParameter("board");
        int canbus = getIntParameter("canbus");
        const bool echo = getBoolParameter("echo");
        MasterOfNone *master;

        // Validate input parameters...
        if ( ! ( ( board >= 0 && board <= 0xf ) ) ) { // Invalid board
            cerr << "Usage: " << getUsageString( ) << endl;
            return EXIT_FAILURE;
        }
        
        if ( canbus != -1 && canbus > 1 ) { // Invalid canbus
            cerr << "Usage: " << getUsageString() << endl;
            return EXIT_FAILURE;
        }

        // Create master.
        if (canbus == -1) {
            master = new MasterOfNone(board);
        } else {
            master = new MasterOfNone(board, canbus);
        }

        master->echoAll( echo );

        // Run it.
        master->run();

        // We'll never get here.
        master->stop();
        delete master;
        return EXIT_SUCCESS;

    } catch (const carma::util::ErrorException &ex) {
        cerr << ex.what() << endl;
        cerr << endl << "Usage: " << getUsageString() << endl;
    } catch (...) {
        cerr << "Unknown exception caught in Program::main()." << endl;
        cerr << endl << "Usage: " << getUsageString() << endl;
    }
    return EXIT_FAILURE;
} // End Program::main()
