#include <carma/util/Program.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/programLogging.h>
using namespace carma::util;

#include <carma/environment/WS.h>
using namespace carma::environment;

#include <iostream>
#include <sstream>
#include <cmath>
using namespace std;

//
// @version     $Revision: 1.14 $ $Date: 2013/08/23 20:16:27 $
//
// @usage       testing Weather Station
//
// @description
//      weather readings are displayed to stdout.In emulation
//      mode the overhead is about 4400 calls/sec on a P1.6
//
// @key  dev    /dev/ttyUSB0 string Serial line to open
// @key  reopen   false   b   Close and Reopen device for each reading
// @key  emulate  true    b   Emulate the device
// @key  repeat   1       i   Number of frames to get
//
// @logger ENVIRONMENT_FACILITY carma.environment.Test.tWS

static void printWSData(const int repeat, const WSData &data)
{
    std::cout << "WS: i=" << repeat
              << std::endl
              << " Ta=" << data.ambientTemperature
              << " Td=" << data.dewpointTemperature
              << " RH=" << data.humidity
              << " Pressure=" << data.pressure
              << std::endl
              << " windSpeed=" << data.windSpeed
              << " windDirection=" << data.windDirection
              << " peakWindSpeed=" << data.peakWindSpeed
              << std::endl
              << " avgWindSpeed=" << data.averageWindSpeed
              << " avgWindDirection=" << data.averageWindDirection
              << std::endl
              << " battery=" << data.battery
              << " waterVaporDensity=" << data.waterVaporDensity
              << " waterColumn=" << data.waterColumn
              << std::endl;
}

int Program::main()
{
    const std::string device = getStringParameter("dev");
    const bool reopen = getBoolParameter("reopen");
    const bool emulate = getBoolParameter("emulate");
    const int repeat = getIntParameter("repeat");

    try {
        WS ws(device, reopen, emulate);
        ws.startThreads();

        for (int i = 0; i < repeat; i++) {
            // sleep for one 500ms "frame"
            usleep(500 * 1000);

            // get the data and print it
            WSData data;
            ws.getLatestData(data);
            printWSData(repeat, data);
        }
    } catch (...) {
        std::ostringstream oss;
        oss << "Exception: " << getStringForCaught();

        std::cerr << oss.str() << std::endl;
        programLogErrorIfPossible(oss.str());

        // FAIL
        return 1;
    }

    return 0;
}

/* vim: set ts=4 sts=4 sw=4 et: */
