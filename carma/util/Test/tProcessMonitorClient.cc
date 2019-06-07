#include "carma/corba/Server.h"
#include "carma/util/ProcessMonitorClient.h"
#include "carma/util/Program.h"

using namespace carma::util;

/**
 * @version $Revision: 1.1 $
 *
 * @description
 * Simple test program to run ProcessMonitorClient while hammering the CPU
 * with corba::Server::work(). This uncovered a TAO ORB::perform_work thread
 * safety bug when an orb is shared amongst client and server.  Running this
 * would cause work() to hang deep inside ACE select after at most 60s. 
 * Note you must specify -ORBServerId on extra command line.
 *
 * @usage tProcessMonitorClient imr=<imr host> -- -ORBServerId <unique id>
 *
 * @noKeys
 *
 * @logger TEST_FACILITY carma.test.tProcessMonitorClient
 */
int Program::main( ) 
{
    while ( !imrTerminationRequested() ) {
        Program::getCorbaServer().work();
    }
    return 0;
}
