/** @file
 * Server process for Carma Wideband Downconverter system.  
 * This process simultaneously serves as the CANbus Master for retrieving
 * /sending control packets from/to devices on the CANbus and a Corba control
 * server for invoking control commands remotely on the CANbus system
 * and it's devices.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.38 $
 * $Date: 2012/02/15 21:42:44 $
 * $Id: carmaWbdcHost.cc,v 1.38 2012/02/15 21:42:44 abeard Exp $
 */

#include "carma/corba/corba.h"
#include "carma/corba/Server.h"
#include "carma/downconverter/wideband/WbdcMaster.h"
#include "carma/downconverter/common/downconverterSystem.h"
#include "carma/downconverter/common/downconverterSystem_skel.h"
#include "carma/downconverter/common/downconverterSystem_skel_tie.h"
#include "carma/monitor/WbdcSubsystem.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"

#include <iostream>
#include <signal.h>

using namespace carma::downconverter;
using namespace carma::util;
using namespace std;

void setSigAction(); // Needed to bypass carma signal handler.

/**
 * @version $Revision: 1.38 $
 * 
 * @usage \nUsage: carmaWbdcHost [emulate=false] [board=0 [bus=0]] [imr=imr] [-- <extraargs>]\n 
 * 
 * @description
 *  \ncarmaWbdcHost is the CANbus master process and CORBA control server 
 *  for the Carma Wideband Downconverter subsystem.  The program makes use of 
 *  the CORBA Naming Service to publish DO references and the Notification
 *  service to initialize the monitor system.  The preferred method to retrieve
 *  references to these services is through the imr (via imr system keyword).
 *  This however is not required and a user may instead initialize these 
 *  services using '-ORBInitRef corbaloc' options following a ' -- ' on the 
 *  command line (see usage for details).  \n
 *  The default command line options work for a system with Janz hardware on
 *  board 0 and controls both busses.
 *  On a system which has no janz hardware, use the emulate=true option. 
 *  \nExample: If using an IMR on host imrhost:
 *  \tcarmaWbdcHost emulate=true imr=imrhost
 *  \nExample: Running on a system with Janz hardware using both busses:
 *  \tcarmaWbdcHost board=0 imr=imrhost
 *  \nExample: Using only one of the two CANbusses on the Janz board
 *  \tcarmaWbdcHost board=0 bus=0 imr=imrhost
 *  \nExample: Default hardware and standalone (non imr) nameserver on inyo:
 *  \tcarmaWbdcHost -- -ORBInitRef NameService=corbaloc::inyo:2000/NameService
 *  \nSee carmaWbdcHost --usage and --keywords for more details.
 *
 * @key emulate false b Emulate Janz hardware instead of opening janz CAN/Dio.
 * @key board       0 i The board id of the Janz CAN/Dio cPCI board.
 * @key canbus     -1 i Control only CANbus 0 or 1 (both by default).
 * @key echo     true b Echo all sent messages back through read interface.
 * @logger DEFAULT_FACILITY carma.downconverter.wideband
 */
int Program::main()
{
    setSigAction();

    try {
        bool emulate = getBoolParameter("emulate");
        int board = getIntParameter("board");
        int canbus = getIntParameter("canbus");
        bool echo = getBoolParameter("echo");

        // Validate command line options. 
        if (emulate && 
            (parameterWasSpecified("board") || 
             parameterWasSpecified("canbus"))) {
            cerr << "Incompatible options, you must either emulate the "
                << "janz hardware or use janz devices with the specified "
                << "options - not both!" << endl;
            cerr << getUsageString() << endl;
            return EXIT_FAILURE;
        } 

        corba::Server & server = getCorbaServer();

        monitor::WbdcSubsystem wbdcMon;

        const double AUTOWRITER_DELAY = 0.200;
        wbdcMon.startAutoWriter( AUTOWRITER_DELAY );

        carma::downconverter::WbdcMaster *master = NULL; 

        // Create the CanMaster - any error creating WbdcMaster will throw
        // an exception - a null pointer won't be returned.
        if (emulate) {
            master = new WbdcMaster( server, wbdcMon );
        } else if (parameterWasSpecified("canbus")) {
            master = new WbdcMaster( server, board, canbus, wbdcMon );
        } else {
            master = new WbdcMaster( server, board, wbdcMon );
        } 

        // Set echo all
        master->echoAll(echo);

        server.addServant< POA_carma::downconverter::System_tie >( 
            *master, 
            WBDCCONTROL_NAME );
            
        // Poll on the server until done. Polling rather than blocking is 
        // implemented for debug to test that everything is properly 
        // destroyed.  We could block forever here as well. 
        const ::timespec amillisecond = { 0, 1000000 };
        while (!master->isDone() && !server.terminated() ) {
            server.work();
            nanosleep( &amillisecond, 0 ); // Sleep to not monopolize the CPU.
        }

    } catch (CORBA::Exception & ex) {
        cerr << "Make sure CORBA environment is setup either via " 
            << "command line, config file and/or environment.  See "
            << "carmaWbdcHost --description for more details. " << endl;
    } catch (carma::util::ErrorException & err) {
        cerr << err << endl;
    } 
    // A concession to a useless warning
    return EXIT_SUCCESS; 
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
