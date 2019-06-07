#include <sstream>
#include <string>
using namespace std;

#include <carma/util/Program.h>
#include <carma/util/StartPthread.h>
#include <carma/util/Trace.h>
#include <carma/util/programLogging.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/ErrorException.h>
#include <carma/util/AutoPthreadQuitAndJoinGroup.h>
using namespace carma::util;

#include <carma/linelength/LineLengthControlImpl.h>
#include <carma/linelength/LLManagerThread.h>
using namespace carma::linelength;

#include <carma/corba/Server.h>
#include <carma/linelength/LineLengthControl.h>
#include <carma/linelength/LineLengthControl_skel.h>
#include <carma/linelength/LineLengthControl_skel_tie.h>

#define CPTRACEALL(args...) CARMA_CPTRACE(Trace::TRACEALL, ##args)

/**
 * @version $Revision: 1.16 $
 * @usage @autogen
 *
 * @key ni0 "/dev/comedi0" s First NI card device file.
 * @key ni1 "/dev/comedi1" s Second NI card device file.
 * @key ni0enable true b Enable reading the First NI card.
 * @key ni1enable true b Enable reading the Second NI card.
 * @key emulate false b Emulate hardware
 * @key awdelay  0.200  d Monitor system autowrite delay in seconds.
 *
 * @logger ENVIRONMENT_FACILITY carma.linelength.LineLength
 */

int Program::main()
{
    CPTRACEALL("carmaLineLengthHost starting...");

    try {
        const std::string ni0 = getStringParameter("ni0");
        const std::string ni1 = getStringParameter("ni1");
        const double autoWriteDelayInS = getDoubleParameter("awdelay");

        LLManagerThread llManager(ni0, ni1, autoWriteDelayInS);

        // enable board0 worker if requested
        if (getBoolParameter("ni0enable"))
            llManager.startWorkerBoard0();

        // enable board1 worker if requested
        if (getBoolParameter("ni1enable"))
            llManager.startWorkerBoard1();

        // start CORBA + monitor data writer
        AutoPthreadQuitAndJoinGroup group;
        group.insert(StartPthreadWithRef(LLManagerThread::thread, llManager));

        // Create a Device servant
        CPTRACEALL("Creating LineLengthControlImpl servant");
        LineLengthControlImpl impl(llManager);
        CPTRACEALL("Servant was successfully created");

        // Add the servant to the internal CORBA server object
        CPTRACEALL("Adding servant to CORBA Server object");
        carma::corba::Server &server = carma::util::Program::getCorbaServer();
        server.addServant<POA_carma::linelength::LineLengthControl_tie>(impl, carma::linelength::LINELENGTH_NAME);
        CPTRACEALL("Servant added to CORBA Server object");

        // Block on orb forever
        programLogInfoIfPossible("Blocking on ORB forever - type ^C to terminate");
        server.run(false);
        CPTRACEALL("ORB stopped blocking");
    } catch (...) {
        std::ostringstream oss;
        oss << "Exception in main: " << getStringForCaught();
        programLogErrorIfPossible(oss.str());
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

// vim: set expandtab ts=4 sts=4 sw=4:
