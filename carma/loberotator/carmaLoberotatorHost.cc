
/** 
 * @file
 * Server process for Loberotator control boards.
 * This program follows the model of the Wideband Downconverter server
 * in that it handles control of the CANbus master for all CANBus messages
 *
 * @author Colby Kraybill, Steve Scott
 * $Revision: 1.40 $
 * $Date: 2014/06/24 21:46:00 $
 * $Id: carmaLoberotatorHost.cc,v 1.40 2014/06/24 21:46:00 scott Exp $
 *
 * $CarmaCopyright$
 */

/** 
 *
 * @version $Revision: 1.40 $
 * @key emulate false b Emulate Janz hardware
 * @key board  -1    i The modulbus number of the Janz CAN/DIO card
 * @key canbus -1    i The slow number of the canbus being controlled
 * @key holdoff 250  i Delay in msec from integral halfsec to send cmds
 * @key sim    false  b Simulate monitor data if hardware is not present
 * @key phaseswitchfile    "/full/path/to/filename" s 
 *    File from which to read phase switch table.
 * @key awdelay 0.160 d Monitor system autowrite delay in seconds.
 * @key syawdelay 0.280 d Switchyard monitor system autowrite delay in seconds.
 * @usage carmaLoberotatorHost [emulate=f][board=-1][bus=-1][sim=false]
 *
 * @logger INTERFEROMETRY_FACILITY carma.Loberotator
 */

// Carma includes
#include "carma/corba/corba.h"
#include "carma/corba/Server.h"
#include "carma/loberotator/LoberotatorControl.h"
#include "carma/loberotator/LoberotatorControl_skel.h"
#include "carma/loberotator/LoberotatorControl_skel_tie.h"
#include "carma/loberotator/LoberotatorMaster.h"
#include "carma/switchyard/SwitchyardControl.h"
#include "carma/switchyard/SwitchyardControl_skel.h"
#include "carma/switchyard/SwitchyardControl_skel_tie.h"
#include "carma/switchyard/SwitchyardControlImpl.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/Trace.h"

using namespace carma;
using namespace carma::loberotator;
using namespace carma::switchyard;
using namespace carma::util;
using namespace std;

namespace POA_clr = POA_carma::loberotator;
namespace POA_csy = POA_carma::switchyard;

bool areParametersValid( bool emulate, int board, int bus );

/** @mainpage
 *
 * The function of the Lobe Rotator subsystem is to control and
 * monitor the lobe rotator hardware.  This system is interconnected with
 * the DelayEngine (commands flow from the DelayEngine to this subsystem)
 * and the correlator (phases switches on the RF to allow synchronous 
 * detection of the astronomical signal and rejection of systematics).  
 * The lobe rotator provides continuous fringe tracking as the
 * geometry between the source being observed and the antennas changes over
 * time.
 *
 * Related Documentation:
 * - <A HREF="http://www.mmarray.org/project/WP/LobeRotator/sw/current.pdf">
 *   LobeRotatorSoftware Design</A>
 */

int Program::main ()
{
    carma::loberotator::LoberotatorMaster *lrMaster; // CAN Master
    PortableServer::POA_var masterPOA;

    try {
        bool emulate = getBoolParameter( "emulate" );
        int  board   = getIntParameter( "board" );
        int  bus     = getIntParameter( "canbus" );
        int  holdoff = getIntParameter( "holdoff" );
        string phaseSwitchFileName = getStringParameter("phaseswitchfile");
        const double autoWriteDelayInS = getDoubleParameter( "awdelay" );
        const double switchyardAutoWriteDelayInS = 
            getDoubleParameter( "syawdelay" );

        if (!areParametersValid(emulate, board, bus)) {
                ostringstream os ;
                os << "Invalid input parameters. Usage: " << getUsageString();
                const string usage = os.str();
                cerr << usage << endl;
                programLogCriticalIfPossible( usage );
                return EXIT_FAILURE;
        }
        
        if ( board == -1 ) board = 0;

        if ( bus == -1 ) bus = 0;

        // Create the CanMaster.
        if (emulate) {
            // this constructor is just for emulation
            lrMaster = new LoberotatorMaster(holdoff,
                                             autoWriteDelayInS,
                                             switchyardAutoWriteDelayInS);
        }
        else if (( board != -1 ) && ( bus != -1 )) {
            bool sim = getBoolParameter("sim");
            lrMaster = new LoberotatorMaster(board, bus, holdoff,
                                             sim, autoWriteDelayInS,
                                             switchyardAutoWriteDelayInS);
        }
        else {
            throw CARMA_ERROR("Non-emulated LRmaster needs board and bus");
        }
        
        // Create the corba::Server 
        SwitchyardControlImpl loSwitchControl( lrMaster->getLoSwitchyard( ) );
        SwitchyardControlImpl llSwitchControl( lrMaster->getLlSwitchyard( ) );
        
        corba::Server & server = getCorbaServer();

        server.addServant< POA_clr::LoberotatorControl_tie >( 
            lrMaster->getGlobalLoberotator(), LOBEROTATOR_NAME );
        server.addServant< POA_csy::SwitchyardControl_tie >( 
            loSwitchControl, LOSWITCHYARDCONTROL_NAME );
        server.addServant< POA_csy::SwitchyardControl_tie >( 
            llSwitchControl, LLSWITCHYARDCONTROL_NAME );

        const ::timespec amillisecond = { 0, 1000000 };
        while ( !imrTerminationRequested() && !lrMaster->isDone() ) {
            server.work();
            nanosleep( &amillisecond, 0 ); // Sleep to not monopolize the CPU.
        }

    } catch ( util::ErrorException & eex ) {
        ostringstream os;
        os << "Exception in Program::main: " << eex.getMessage();
        programLogCriticalIfPossible( os.str() );
        cerr << eex << endl;
        return EXIT_FAILURE;
    } catch ( ... ) {
        ostringstream os;
        os << "Unclassified exception in Program::main: " << getStringForCaught();
        const string err( os.str() );
        programLogCriticalIfPossible( err );
        cerr << err << endl;
        return EXIT_FAILURE;
    }

    return 0;
}

// ***************************************************************************
bool areParametersValid ( bool emulate, int board, int bus )
{
    if (emulate && (board != -1 || bus != -1)) {
        const string msg( "Cannot specify board and bus parameters if emulating ." );
        programLogCriticalIfPossible( msg );
        cerr << msg << endl;
        return false;
    } else if ((!emulate) && (board == -1) && (bus == -1)) {
        const string msg( "Must specify board and bus parameters if not emulating." );
        cerr << msg << endl;
        programLogCriticalIfPossible( msg );
        return false;
    } else if ((board == -1 && bus != -1)) {
        const string msg( "board must be specified with bus." );
        programLogCriticalIfPossible( msg );
        cerr << msg << endl;
        return false;
    } else {
        // Parameters are correct.
        return true;
    }
}
