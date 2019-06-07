#include <iostream>

#include "carma/util/Program.h"
#include "carma/canbus/Test/RemoteMasterTest/RemoteCanMaster.h"

using namespace carma::util;
using namespace carma::canbus::test;
using namespace std;

/**
 * @version $Revision: 1.3 $
 *
 * @usage tRemoteMaster hostname=canoveripserver
 *
 * @description Simple test program to demonstrate a remote CanMaster.
 *
 * @key hostname @mandatory s Host of canOverIp server.
 *
 * @logger TEST_FACILITY carma.test.canbus.RemoteMasterTest.tRemoteMaster
 */
int Program::main()
{
    string hostname = getStringParameter("hostname");

    RemoteCanMaster rcm(hostname);

    rcm.run();

    return EXIT_SUCCESS;
}

