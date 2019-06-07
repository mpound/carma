/**
 * @file 
 * Carma ovro antenna CAN host process.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.51 $
 * $Date: 2013/01/18 01:00:30 $
 * $Id: ovroCanHost.cc,v 1.51 2013/01/18 01:00:30 abeard Exp $
 */

// Carma Includes
#include "carma/antenna/ovro/canbus/OvroMaster.h"
#include "carma/antenna/ovro/control/ControlServer.h"
#include "carma/monitor/MonitorPointSet.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/monitor/OvroSubsystemPrewrite.h"
#include "carma/services/Global.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"

// STL Includes
#include <iostream>
#include <memory>

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

using namespace carma::antenna::common;
using namespace carma::antenna::ovro;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

/**
 * @version $Revision: 1.51 $
 *
 * @usage \nUsage: ovroCanHost [emulate=false] [board=0] [antNo=[1-6]] [-- <extraargs>]\n
 *
 * @description
 *  \novroCanHost is the CANbus master process and CORBA control server
 *  for all Ovro antenna canbus subystems.  The program does not require 
 *  an ORBacus IMR or nameserver to function.  However, this program CAN be
 *  used in an IMR environment by using the imr= system keyword.
 *  See ovroCanHost --system for more details.
 *  \nExample: Typical debug environment:
 *  \tovroCanHost board=0 ant=1 autowrite=false
 *  \nExample: Production environment:
 *  \tovroCanHost board=0 ant=1 imr=imr
 *  \nExample: No Janz hardware (emulation mode):
 *  \tovroCanHost emulate=true ant=1 imr=imr 
 *
 * @key ant @mandatory  i Antenna No. this program is running on.
 * @key autowrite true  b Enable autowriter - requires FSP.
 * @key awdelay  0.200  d Autowriter write delay in seconds.
 * @key board        0  i The board id of the Janz CAN/Dio cPCI carrier board.
 * @key canbus       0  i Control only a single canbus (0 or 1 - default both).
 * @key echo      true  b Echo sent CAN messages back through read interface.
 * @key emulate  false  b Emulate Janz hardware instead of opening Janz CAN/Dio.
 * @key simulate false  b Simulates all CAN nodes when offline.
 * @key reset    false  b Resets all CAN modules upon startup.
 * @key terminate false b Terminates both CAN busses at the Janz cards. 
 * @logger DEFAULT_FACILITY carma.antenna.ovro
 */
int Program::main()
{
    try {
        // Retrieve command line options (clX = command line option X).
        const unsigned short clAntNo = getIntParameter("ant"); 
        const bool clAutowrite = getBoolParameter("autowrite");
        const double clWriteDelay = getDoubleParameter("awdelay");
        const int clBoard = getIntParameter("board");
        const int clCanbus = getIntParameter("canbus");
        const bool clEcho = getBoolParameter("echo");
        const bool clEmulate = getBoolParameter("emulate");
        const bool clSimulate = getBoolParameter("simulate");
        const bool clReset = getBoolParameter("reset");
        const bool clTerminate = getBoolParameter("terminate");
        const string clImr = getImrHostname( );

        // Validate input antenna number.
        if ( !( clAntNo >= 1 && clAntNo <= Global::nOvroAntennas() ) ) {
            cerr << "Antenna number must be between [1-" 
                << Global::nOvroAntennas() << "]." << endl;
            return EXIT_FAILURE;
        } else {
            ostringstream logname;
            logname << "carma.antenna.ovro" << clAntNo;
            setInstanceLogname( logname.str( ) ); 
        }

        // Validate emulate and board/canbus options.
        if ( clEmulate && 
             ( parameterWasSpecified("board") || 
               parameterWasSpecified("canbus") ) ) {
            cerr << "Incompatible options, you must either emulate the "
                << "janz hardware or use janz devices by specifying "
                << "board and/or canbus options." << endl;
            cerr << getUsageString() << endl;
            return EXIT_FAILURE;
        }

        // Simulating and NOT emulating at the same time is dangerous on the 
        // RTS since a malfunctioning or broken encoder module will be replaced
        // with a simulated version. 
        if ( !clEmulate && clSimulate && 
             ( clImr.find( "carma.pvt" ) != string::npos ) ) {
            programLogErrorIfPossible( "Running in hybrid simulation-real bus"
                " mode on the RTS is DANGEROUS - exiting." );
            return EXIT_FAILURE;
        }
        
        // Create and initialize the monitor system.
        OvroSubsystem ovroMonSubsys( clAntNo );
        OvroSubsystemPrewrite prewriter( ovroMonSubsys );
        
        ovroMonSubsys.monitorPointSet().installPrewriteMethod( prewriter );

        if (clAutowrite) 
            ovroMonSubsys.startAutoWriter( clWriteDelay );
            
        // Create Ovro CAN Master.
        ::std::auto_ptr< OvroMaster > master;
        if (clEmulate) {
            master = auto_ptr< OvroMaster >( new OvroMaster( 
                clAntNo, 
                clSimulate, 
                ovroMonSubsys ) );

        } else if (parameterWasSpecified("canbus")) {
            master = auto_ptr< OvroMaster >( new OvroMaster( 
                clBoard, 
                clCanbus, 
                clAntNo, 
                clSimulate, 
                clReset, 
                clTerminate,
                ovroMonSubsys ) );
        } else {
            master = auto_ptr< OvroMaster >( new OvroMaster( 
                clBoard, 
                clAntNo, 
                clSimulate, 
                clReset, 
                clTerminate,
                ovroMonSubsys ) );
        }

        master->echoAll( clEcho ); 
        
        master->start(); // Kick off the CAN Master - doesn't block.
        
        sleep( 1 );
        
        ControlServer control( *( master.get( ) ),  
                               clAntNo, 
                               ovroMonSubsys, 
                               getCorbaServer(),
                               getConfDir(),
                               clSimulate );
        
        control.runServer(); // Blocks on Corba control server.
        
    } catch (...) {

        logCaughtAsError( );
        
        return 1;
    }

    return 0;

} // End Program::main
