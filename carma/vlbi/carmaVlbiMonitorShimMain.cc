#include <iostream>
using namespace std;

#include <carma/corba/Server.h>

#include <carma/vlbi/VlbiMonitorShim.h>
#include <carma/vlbi/VlbiMonitorShim_skel.h>
#include <carma/vlbi/VlbiMonitorShim_skel_tie.h>
#include <carma/vlbi/VlbiMonitorShimImpl.h>
using namespace carma::vlbi;

#include <carma/util/Trace.h>
#include <carma/util/Program.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/programLogging.h>
using namespace carma::util;


/** @version $Revision: 1.3 $
 *
 * @usage carmaVlbiShim [imr=imr] [-- [-ORBDefaultInitRef defref] [-ORBInitRef initref] [-ORBServerId serverid]]
 *
 * @key awdelay  0.100  d Monitor system autowrite delay in seconds.
 *
 * @logger MONITOR_FACILITY carma.vlbi.vlbiShim
 */

int
Program::main()
{
    CARMA_CPTRACE(Trace::TRACEALL, "carmaVlbiShim starting.");

    // Get keyword values
    const double autoWriteDelayInS = getDoubleParameter( "awdelay" );

    try {
        VlbiMonitorShimImpl servant(autoWriteDelayInS);
        carma::corba::Server &server = carma::util::Program::getCorbaServer();
        server.addServant<POA_carma::vlbi::MonitorShim_tie>(servant, carma::vlbi::MONITOR_SHIM_NAME);

        /* run the CORBA ORB (blocks) */
        programLogInfoIfPossible("Blocking on ORB");
        server.run(false);
        programLogInfoIfPossible("Finished blocking on ORB");

        return EXIT_SUCCESS;
    } catch (...) {
        std::ostringstream oss;
        oss << "Exception: " << getStringForCaught();
        programLogErrorIfPossible(oss.str());
        return EXIT_FAILURE;
    }

    // Should "never" get here, so if we do, return failure
    return EXIT_FAILURE;
}

// vim: set expandtab sw=4 ts=4 :
