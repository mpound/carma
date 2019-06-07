/** @file
 * Server process for Carma Clock system.  
 * This process simultaneously serves as the CANbus Master for retrieving
 * /sending control packets from/to devices on the CANbus and a Corba control
 * server for invoking control commands remotely on the CANbus system
 * and it's devices.
 *
 * <dl><dt><b>Author </b></dt><dd>Chul Gwon </dl>
 * $Revision: 1.19 $
 * $Date: 2012/03/13 05:17:56 $
 * $Id: carmaClockHost.cc,v 1.19 2012/03/13 05:17:56 abeard Exp $
 *
 * @usage carmaClockHost [emulate=false] [board=0] [bus=0] [imr=imrhost]
 *
 * @key emulate     f   b  Emulate Janz hardware instead of opening janz CAN/Dio
 * @key board       0   i  Board ID of the Janz CAN/Dio cPCI board
 * @key bus        -1   i  Control only CANbus 0 or 1 (both by default)
 * @key autowrite true  b Enable autowriter - requires FSP.
 * @key awdelay  0.200  d Monitor system autowrite delay in seconds.
 *
 * @logger DEFAULT_FACILITY carma.clock
 */

// Carma includes
#include "carma/corba/corba.h"
#include "carma/corba/Server.h"
#include "carma/clock/ClockMaster.h"
#include "carma/clock/clockSystem.h"
#include "carma/clock/clockSystem_skel.h"
#include "carma/clock/clockSystem_skel_tie.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"

#include <iostream>
#include <signal.h>

using namespace carma::clock;
using namespace carma::corba;
using namespace carma::util;
using namespace std;

// Helper function declarations, definitions follow main.
void setSigAction(); // Needed to bypass carma signal handler.


int Program::main()
{
    setSigAction();
    carma::clock::ClockMaster *master = NULL; // CAN Master/CORBA server 
    try {
        // get command line parameters
        bool emulate = getBoolParameter("emulate");
        int board = getIntParameter("board");
        int bus = getIntParameter("bus");

         // Validate command line options. 
        if (emulate && (parameterWasSpecified("board") || parameterWasSpecified("bus"))) {
            cerr << "Incompatible options, you must either emulate the "
                << "janz hardware or use janz devices with the specified "
                << "options - not both!" << endl;
            cerr << getUsageString( ) << endl;
            return EXIT_FAILURE;
        } 

        corba::Server & corbaServer = Program::getProgram().getCorbaServer( );

        // Create the CanMaster - any error creating ClockMaster will throw
        // an exception - a null pointer won't be returned.
        const double autoWriteDelayInS = getDoubleParameter( "awdelay" );
        if (emulate) {
            const bool autowrite = getBoolParameter( "autowrite" );
            master = new ClockMaster(autowrite, autoWriteDelayInS );
        } else if (parameterWasSpecified("bus")) {
            master =  new ClockMaster( board, bus, autoWriteDelayInS );
        } else {
            master = new ClockMaster( board, autoWriteDelayInS );
        }

        namespace Pcc = POA_carma::clock;
        corbaServer.addServant< Pcc::System_tie >( *master,  
                                                   "carma.clock.ClockControl" );
            
        corbaServer.run( );

    } catch (...) {
        cerr << getStringForCaught() << endl;
        return 1;
    } 

    return 0; 
}


// -----------------------------------------------------------------------------
void setSigAction() 
{
    int SIGNUM = 2; // SIGINT
    struct sigaction action;
    // action.sa_flags = SA_SIGINFO;
    sigfillset (&(action.sa_mask));
    action.sa_handler = SIG_DFL; // Specify Default action - abort().
    action.sa_restorer = NULL;
    sigaction (SIGNUM, &action, NULL);
}
