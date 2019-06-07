#include <carma/util/Program.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/programLogging.h>
#include <carma/environment/DPS.h>

#include <iostream>
#include <sstream>
#include <cmath>

using namespace std;
using namespace carma::util;
using namespace carma::environment;

//
// @version     $Revision: 1.12 $ $Date: 2011/05/04 20:36:23 $
//
// @usage       testing Dew Point Sensor
//
// @description
//      weather readings are displayed to stdout. In emulation
//      mode the overhead is about 6900 calls/sec on a P1.6
//      or almost 12,000 cps on a P4/3.0
//      For no reuse this drops to about 3500 cps, a combination
//      of syslogging the bad device and instantiating new objects.
//
// @key  dev    /dev/ttyUSB1 string Serial line to open
// @key  reopen   false   b   reopen the device after each read
// @key  emulate  true    b   emulate the device
// @key  repeat   1       i   Number of frames to get
// @logger ENVIRONMENT_FACILITY carma.environment.Test.tDPS

int Program::main()
{
    const std::string device = getStringParameter("dev");
    const bool reopen = getBoolParameter("reopen");
    const bool emulate = getBoolParameter("emulate");
    const int repeat = getIntParameter("repeat");

    try {
        DPS dps(device, reopen, emulate);
        dps.startReaderThread();

        for (int i = 0; i < repeat; i++) {
            // sleep for one 500ms "frame"
            usleep(500 * 1000);

            // get the latest data
            DPSData data;
            dps.getLatestData(data);

            // print it
            std::cout << "DPS:"
                      << " i=" << repeat
                      << " Ta=" << data.ambientTemperature
                      << " Td=" << data.dewpointTemperature
                      << " RH=" << data.humidity
                      << std::endl;

        }
    } catch (...) {
        std::ostringstream oss;
        oss << "Exception: " << getStringForCaught();

        std::cerr << oss.str() << std::endl;
        programLogErrorIfPossible(oss.str());

        // FAIL
        return 1;
    }

    // SUCCESS
    return 0;
}

/* vim: set ts=4 sts=4 sw=4 et: */
