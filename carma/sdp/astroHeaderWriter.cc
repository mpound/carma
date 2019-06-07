/**
 * @logger DEFAULT_FACILITY carma.sdp.astroHeaderWriter
 *
 * @version $Id: astroHeaderWriter.cc,v 1.10 2014/06/04 17:09:29 mpound Exp $
 *
 * @usage @autogen
 *
 * @description
 * This application reads the monitor data flat files written by the
 * monitorAverageWriter and extracts the science data needed to form
 * the astronomical headers. These headers, in conjunction with the
 * VisBrick visibility files written by the correlator pipeline, are
 * used to create the science output format by the filler.
 *
 * @key dbconffile dbms/dbms.conf string RDBMS configuration file.
 * @key sdpconffile sdp/sdp.conf string SDP configuration file.
 * @key reset false bool Hard reset on start.
 * @key stopfile /tmp/astroHeaderWriter.stop string Exit gracefully if this file exists.
 * @key controlfile sdp/astroheaderwriter.conf string AHW control file
 * @key sleeptime 30 int amount of time to sleep between processing cycles
 *
 * @author Athol Kemball
 * @author Ira W. Snyder
 */

// Carma includes
#include <carma/sdp/AHW_Output.h>
#include <carma/sdp/AHW_Processor.h>

#include <carma/dbms/DBConfigurator.h>

#include <carma/corba/Server.h>

#include <carma/util/KeyValueConfigFile.h>
#include <carma/util/NotFoundException.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/programLogging.h>
#include <carma/util/TimedBenchmark.h>
#include <carma/util/ScopedLogNdc.h>
#include <carma/util/Program.h>
#include <carma/util/Trace.h>

// Std C++ includes
#include <exception>
#include <iostream>
#include <string>
#include <vector>
#include <map>

// Boost includes
#include <boost/shared_ptr.hpp>
#include <boost/filesystem.hpp>
#include <boost/foreach.hpp>

// namespace directives
using carma::dbms::DBConfigurator;
using namespace carma::util;
using namespace carma::sdp;

typedef boost::shared_ptr<AHW_Processor> AHW_Processor_Ptr;

#define CPTRACE6(args...) CARMA_CPTRACE(carma::util::Trace::TRACE6, ##args)

// Create an instance of the AstroHeaderWriter for a single correlator type
static AHW_Processor_Ptr createAHW(
        const carma::dbms::MonitorAverageType avgType,
        std::map<std::string, std::string> &sdpconf,
        std::map<std::string, std::string> &dbmsconf,
        const std::vector<AHW_Output> &outputs,
        carma::monitor::DataflowSubsystem &dataflow)
{
    using carma::monitor::DataflowSubsystem;

    boost::filesystem::path id;
    boost::filesystem::path od(sdpconf["astroHeaderDir"]);
    boost::filesystem::path rd(sdpconf["recycleDir"]);
    carma::util::CorrelatorType corlType = CORR_NONE;

    DataflowSubsystem::AstroheaderWriter &ahw = dataflow.astroheaderWriter();
    DataflowSubsystem::Correlator *monitor = NULL;

    od /= carma::dbms::toString(avgType);
    rd /= carma::dbms::toString(avgType);

    if (avgType == carma::dbms::SLCORREL_AVG) {
        id = dbmsconf["sdpSLCorrelDir"];
        corlType = CORR_SPECTRAL;
        monitor = &ahw.spectralLineCorrelator();
    } else if (avgType == carma::dbms::WBCORREL_AVG) {
        id = dbmsconf["sdpWBCorrelDir"];
        corlType = CORR_WIDEBAND;
        monitor = &ahw.widebandCorrelator();
    } else {
        std::ostringstream oss;
        oss << "ERROR: unknown MonitorAverageType: " << avgType;
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    programLogDebugIfPossible("createAHW: id=" + id.string() + " od=" + od.string() + " rd=" + rd.string());

    AHW_Processor_Ptr p(new AHW_Processor(corlType, id.string(), od.string(), rd.string(), outputs, monitor));
    return p;
}

// Check if the program was requested to stop execution
static bool stopRequested(const std::string &stopfile)
{
    carma::util::Program &program = carma::util::Program::getProgram();
    boost::filesystem::path p(stopfile);

    // check for stop file existence
    if (boost::filesystem::exists(p))
        return true;

    // check for CORBA stop request
    if (program.haveImrHostname() && program.imrTerminationRequested())
        return true;

    return false;
}

/*
 * Check for CORBA stop requests at regular intervals while sleeping. We
 * do not check for stop file existence to reduce the load on the filesystem.
 *
 * This function will exit early and return true if a CORBA stop request
 * was received while sleeping.
 */
static bool ahw_sleep(const unsigned int seconds)
{
    carma::util::Program &program = carma::util::Program::getProgram();
    const unsigned int ms_requested = seconds * 1000;
    unsigned int ms_elapsed = 0;

    const unsigned int ms = 50;
    const ::timespec ts = { 0, ms * 1000 * 1000 };

    while (ms_elapsed < ms_requested) {
        // check for CORBA stop request
        if (program.haveImrHostname() && program.imrTerminationRequested()) {
            programLogInfoIfPossible("CORBA stop request received");
            return true;
        }

        nanosleep(&ts, NULL);
        ms_elapsed += ms;
    }

    return false;
}

// Main
int carma::util::Program::main()
try {
    // Get input parameter values
    const std::string dbconffile  = getConfFile(getStringParameter("dbconffile"));
    const std::string sdpconffile = getConfFile(getStringParameter("sdpconffile"));
    const bool reset              = getBoolParameter("reset");
    const std::string stopfile    = getStringParameter("stopfile");
    const int sleeptime           = getIntParameter("sleeptime");

    carma::monitor::DataflowSubsystem dataflow;

    // daemon mode
    if (haveImrHostname()) {
        // corba::Server run thread so we can service termination requests
        getCorbaServer().run(true);

        // autowriter
        dataflow.startAutoWriter(0.100);
    }

    // Read configuration file parameters
    std::map<std::string, std::string> sdpconf = KeyValueConfigFile::load(sdpconffile);

    // Obtain a map of monitor point input directories using the
    // DBConfigurator class (see use in monitorDataLoader.cc).
    std::map<std::string, std::string> dbmsconf;
    try {
        typedef boost::shared_ptr<DBConfigurator> DBConfiguratorPtr;
        DBConfiguratorPtr dbconf(new DBConfigurator(dbconffile));
        dbmsconf = dbconf->getConfiguration();
    } catch (const NotFoundException& exc) {
        std::ostringstream oss;
        oss << "Unable to read configuration file " << dbconffile << std::endl;
        std::cerr << oss.str() << std::endl;
        programLogErrorIfPossible(oss.str());
        exc.report();
        return EXIT_FAILURE;
    } catch (...) {
        std::ostringstream oss;
        oss << "EXCEPTION: " << carma::util::getStringForCaught();
        std::cerr << oss.str() << std::endl;
        programLogErrorIfPossible(oss.str());
        return EXIT_FAILURE;
    }

    // Retrieve the recycle and output astronomical header directories
    // from the science data products sub-system configuration file.
    const std::string recycleDir = sdpconf["recycleDir"];
    const std::string outputDir = sdpconf["astroHeaderDir"];
    CPTRACE6("astroHeaderDir= " << outputDir << ", recycleDir= " << recycleDir);

    // Parse the AstroHeaderWriter output configuration / control file
    const std::string &file = getConfFile(getStringParameter("controlfile"));
    std::vector<AHW_Output> outputs;
    try {
        outputs = parseAHWControlFile(file);
    } catch (...) {
        std::ostringstream oss;
        oss << "Unable to parse configuration file: " << file
            << ". Error: " << carma::util::getStringForCaught();
        std::cerr << oss.str() << std::endl;
        programLogErrorIfPossible(oss.str());
        return EXIT_FAILURE;
    }

    // Create an AHW_Processor instance for each correlator
    std::vector<AHW_Processor_Ptr> ahws;
    ahws.push_back(createAHW(carma::dbms::SLCORREL_AVG, sdpconf, dbmsconf, outputs, dataflow));
    ahws.push_back(createAHW(carma::dbms::WBCORREL_AVG, sdpconf, dbmsconf, outputs, dataflow));

    // reset if requested
    if (reset) {
        CPTRACE6("Reset on start");
        BOOST_FOREACH(const AHW_Processor_Ptr ahw, ahws) {
            const ScopedLogNdc ndc(ahw->getCorrelatorType());
            ahw->reset();
        }
    }

    // elapsed time measurement
    carma::util::TimedBenchmark timer;
    double lastSleepTime = 0.0;
    timer.start();

    // run until a stop request is received
    while (!stopRequested(stopfile)) {

        timer.stop();
        const double elapsedTime = timer.milliseconds() / 1000.0;
        timer.start();

        unsigned int numIntegrations = 0;

        // run a single iteration
        BOOST_FOREACH(const AHW_Processor_Ptr ahw, ahws) {
            const ScopedLogNdc ndc(ahw->getCorrelatorType());
            try {
                CPTRACE6("START AHW: " << ahw->getCorrelatorType());
                numIntegrations += ahw->run_single_iteration(elapsedTime, lastSleepTime);
                CPTRACE6("FINISH AHW: " << ahw->getCorrelatorType());
            } catch (...) {
                std::ostringstream oss;
                oss << "EXCEPTION: " << ahw->getCorrelatorType()
                    << ": " << carma::util::getStringForCaught();
                programLogErrorIfPossible(oss.str());
            }
        }

        // sleep until next processing cycle
        if (numIntegrations <= 1) {
            lastSleepTime = sleeptime;
            ahw_sleep(sleeptime);
        } else {
            lastSleepTime = 0.0;
        }
    }

    // Exit
    programLogInfoIfPossible("Exiting normally");
    return EXIT_SUCCESS;
} catch (...) {
    std::ostringstream oss;
    oss << "EXCEPTION: " << carma::util::getStringForCaught();
    programLogErrorIfPossible(oss.str());
    return EXIT_FAILURE;
}

/* vim: set ts=4 sts=4 sw=4 et: */
