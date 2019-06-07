/** @file
 * Server process for Carma Spectral Line Downconverter system.  
 * This process simultaneously serves as the CANbus Master for retrieving
 * /sending control packets from/to devices on the CANbus and a Corba control
 * server for invoking control commands remotely on the CANbus system
 * and it's devices.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.15 $
 * $Date: 2014/05/06 21:08:46 $
 * $Id: carmaSldcHost.cc,v 1.15 2014/05/06 21:08:46 iws Exp $
 */

#include "carma/canbus/CanDio.h"
#include "carma/downconverter/spectral/SldcMaster.h"
#include "carma/downconverter/spectral/SldcControlServer.h"
#include "carma/monitor/SldcSubsystem.h"
#include "carma/monitor/SignalPathSubsystem.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"

#include <iostream>
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>
#include <utility>

using namespace carma::canbus;
using namespace carma::downconverter;
using namespace carma::monitor;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

/**
 * @version $Revision: 1.15 $
 * 
 * @usage \nUsage: carmaSldcHost [emulate=false] [board=0 [canbus=0]] [mon=true] [echo=true]\n\t\t\t [imr=imr] [-- <extraargs>]\n 
 * 
 * @description
 *  \ncarmaSldcHost is the CANbus master process and CORBA control server 
 *  for the Carma Spectral Line Downconverter subsystem.  The program makes use
 *  of the CORBA Naming Service to publish DO references and the Notification
 *  service to initialize the monitor system.  The preferred method to retrieve
 *  references to these services is through the imr (via imr system keyword).
 *  This however is not required and a user may instead initialize these 
 *  services using '-ORBInitRef corbaloc' options following a ' -- ' on the 
 *  command line (see usage for details).  \n
 *  The default command line options work for a system with Janz hardware on
 *  board 0 and controls both busses.
 *  On a system which has no janz hardware, use the emulate=true option. 
 *  \nExample: If using an IMR on host imrhost:
 *  \tcarmaSldcHost emulate=true imr=imrhost
 *  \nExample: Running on a system with Janz hardware using both busses:
 *  \tcarmaSldcHost board=0 imr=imrhost
 *  \nExample: Using only one of the two CANbusses on the Janz board
 *  \tcarmaSldcHost board=0 bus=0 imr=imrhost
 *  \nExample: Default hardware and standalone (non imr) nameserver on inyo:
 *  \tcarmaSldcHost -- -ORBInitRef NameService=corbaloc::inyo:2000/NameService
 *  \nSee carmaSldcHost --usage and --keywords for more details.
 *
 * @key emulate false      b Emulate CAN/DIO hardware (implies sim above).
 * @key board1  @noDefault i The board id of the first Janz CAN/Dio cPCI board.
 * @key canbus1 @noDefault i Control only bus 0 or 1 for board1 (default both).
 * @key board2  @noDefault i The board id of the second Janz CAN/Dio cPCI board.
 * @key canbus2 @noDefault i Control only bus 0 or 1 for board2 (default both).
 * @key echo    true       b Echo all sent CAN messages back to read interface.
 * @key sim     false      b Simulate offline nodes (default false).
 * @key awdelay 0.200      d Monitor system autowrite delay in seconds.
 *
 * @logger DEFAULT_FACILITY carma.downconverter.spectral
 */
int Program::main()
{
    try {

        const bool clEmulate = getBoolParameter("emulate"); 

        // Validate our command line options. 
        if ( clEmulate && ( parameterWasSpecified("board1") || 
                            parameterWasSpecified("canbus1") ||
                            parameterWasSpecified("board2") ||
                            parameterWasSpecified("canbus2") ) ) {
            cerr << "Incompatible options: can't specify emulation with board "
                << "and/or canbus options." << endl << getUsageString() << endl;
            return EXIT_FAILURE;
        } 

        // Initialize monitor system according to command line options.
        carma::monitor::SldcSubsystem sldcMonitorSubsystem;
        carma::monitor::SignalPathSubsystem signalPathMonitorSubsystem;

        const double writeDelayInS = getDoubleParameter( "awdelay" );
        sldcMonitorSubsystem.startAutoWriter( writeDelayInS );
        signalPathMonitorSubsystem.startAutoWriter( writeDelayInS );

        std::auto_ptr< SldcMaster > master;
    
        if ( clEmulate ) {
            master = auto_ptr< SldcMaster >( 
                new SldcMaster( sldcMonitorSubsystem, 
                                signalPathMonitorSubsystem ) );
        } else if ( parameterWasSpecified( "board1" ) ) {
            const int clBoard1 = getIntParameter( "board1" );

            const bool term = true;
            vector< CanDio::DevTermPair > devTermPairs;

            if ( parameterWasSpecified( "canbus1" ) ) {
                const int clBus1 = getIntParameter( "canbus1" );
                devTermPairs.push_back( make_pair( make_pair( clBoard1, 
                                                              clBus1 ), term ));
            } else {
                devTermPairs.push_back( make_pair( make_pair( clBoard1, 
                                                              0 ), term ) );
                devTermPairs.push_back( make_pair( make_pair( clBoard1, 
                                                              1 ), term ) );
            }
        
            if ( parameterWasSpecified( "board2" ) ) {
                const int clBoard2 = getIntParameter( "board2" );

                if ( parameterWasSpecified( "canbus2" ) ) {
                    const int clBus2 = getIntParameter( "canbus2" );
                    devTermPairs.push_back( make_pair( make_pair( clBoard2, 
                                                                  clBus2 ), 
                                                       term ) );
                } else {
                    devTermPairs.push_back( make_pair( make_pair( clBoard2, 
                                                                  0 ), term ) );
                    devTermPairs.push_back( make_pair( make_pair( clBoard2, 
                                                                  1 ), term ) );
                }
            }

            const bool clSim = getBoolParameter("sim");
            master = auto_ptr< SldcMaster >( 
                new SldcMaster( devTermPairs, clSim, 
                                sldcMonitorSubsystem, 
                                signalPathMonitorSubsystem ) );
        } else {
            cerr << "Incompatible options, must specify either emulation or "
                << "canbus hardware parameters." << endl 
                << getUsageString() << endl;
            return EXIT_FAILURE;
        }
        
        const bool clEcho = getBoolParameter("echo");

        master->echoAll(clEcho);

        // Kick off the CAN master - doesn't block
        master->start();
        
        corba::Server & corbaServer = getCorbaServer( );

        // Create the control server
        carma::downconverter::SldcControlServer server( corbaServer,
                                                        *master,
                                                        sldcMonitorSubsystem );

        server.run(); // Block on control server
        
        // Stop the master
        master->stop();
        
    } catch (...) {
        ostringstream errorMsg;
        errorMsg << "Exception caught in Program::main() - Exiting. Error is:";
        errorMsg << getStringForCaught();
        cerr << errorMsg.str() << endl;
        Program::getLogger() << Priority::CRIT << errorMsg.str();
        return EXIT_FAILURE;
    }
    // We made it!
    return EXIT_SUCCESS; 
}
