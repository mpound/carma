/**@file 
 * Initialization program for startup of CAN crates.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.1 $
 * $Date: 2010/06/18 21:35:34 $
 * $Id: caninit.cc,v 1.1 2010/06/18 21:35:34 abeard Exp $
 */

#include <iostream>

#include "carma/canbus/CanDio.h"
#include "carma/util/Program.h"

using namespace carma::util;
using namespace carma::canbus;
using namespace std;

/**
 * @version $Revision: 1.1 $
 *
 * @description
 * This program is responsible for initializing Dio lines so that they do 
 * not remain stuck in a reset state. It should be ran from startup scripts
 * on any crate carrying the Carma/Janz CAN/Dio card.  Please note the 
 * command line options.  If two complete CAN/Dio cards exist, this program
 * should be ran separately for each one.  
 * Example:
 *     canInit board=0
 *  initializes dio lines a and b on modulbus 0 slot 3 - Note that dio cards
 *  always inhabit slot 3 on the Caram/Janz CAN/Dio cards.
 *
 * @usage canInit board=<[0-15]> canbus=<[0,1]>
 *
 * @key board           0 i Janz CAN/Dio cPCI board number [0-15].
 * @key canbus @noDefault i Initialize only one CAN bus DIO line [0,1].
 * @logger DEFAULT_FACILITY carma.canbus.caninit
 */
int Program::main() 
{
    try {
        int board = getIntParameter("board");
        carma::canbus::CanDio * can;
        
        if ( parameterWasSpecified("canbus") ) {
            int canbus = getIntParameter("canbus");
            can = new CanDio(board, canbus);
        } else {
            can = new CanDio(board);
        }

        // Reset the canbus and dio lines.
        can->reset();

    } catch (std::exception &ex) {
        cerr << ex.what() << endl;
        return EXIT_FAILURE;
    } catch (...) {
        cerr << "Unknown exception - exiting." << endl;
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}   
