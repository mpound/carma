/**
 * @version $Revision: 1.1 $
 *
 * @usage @autogen
 * @key device "/dev/comedi0" string National Instruments PXI-6031E
 *
 * @logger ENVIRONMENT_FACILITY carma.linelength.centralif
 */
#include <sstream>
#include <string>

#include <carma/util/Time.h>
#include <carma/util/Program.h>
#include <carma/util/StartPthread.h>
#include <carma/util/programLogging.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/ErrorException.h>
using namespace carma::util;

#include <carma/monitor/CentralIfSubsystem.h>
using carma::monitor::CentralIfSubsystem;

#include <carma/linelength/FiberIFThread.h>
using namespace carma::linelength;

static void writeMonitorPoints(CentralIfSubsystem &centralIf, FiberIFDataPtr data)
{
    // sanity check data length
    const unsigned int antennaCount = CentralIfSubsystem::antennaCount();
    if (data->pol1.size() != antennaCount || data->pol2.size() != antennaCount) {
        std::ostringstream oss;
        oss << "CentralIfSubsystem and FiberIF number of antennas do not match";
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    // online and timestamp monitor points
    centralIf.timestamp().setValue(Time::MJD());
    centralIf.online().setValue(true);

    // polarization data
    for (unsigned int i = 0; i < antennaCount; i++) {
        centralIf.antenna(i).pol(0).opticalPower().setValue(data->pol1.at(i));
        centralIf.antenna(i).pol(1).opticalPower().setValue(data->pol2.at(i));
    }
}

int Program::main()
try {
    const std::string device = getStringParameter("device");

    CentralIfSubsystem centralIf;
    centralIf.startAutoWriter(0.100);

    FiberIFThread fiber(device);
    StartPthreadWithRef(FiberIFThread::thread, fiber);

    carma::util::frameType dataRxFrame = 0;
    FiberIFDataPtr data;

    while (!imrTerminationRequested()) {
        // receive data or sleep
        if (!fiber.empty()) {
            dataRxFrame = Time::computeCurrentFrame();
            data = fiber.getData();
        } else {
            usleep(10000); // 10 ms
        }

        // basic monitor points
        centralIf.timestamp().setValue(Time::MJD());
        centralIf.online().setValue(true);

        // do not write data if it is very stale
        const frameType currentFrame = Time::computeCurrentFrame();
        const long difference = dataRxFrame - currentFrame;
        if (std::abs(difference) > 5)
            continue;

        // write out the data
        writeMonitorPoints(centralIf, data);
    }

    return EXIT_SUCCESS;
} catch (...) {
    std::ostringstream oss;
    oss << "EXCEPTION: " << getStringForCaught();
    programLogErrorIfPossible(oss.str());
    return EXIT_FAILURE;
}

// vim: set expandtab ts=4 sts=4 sw=4:
