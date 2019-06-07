/**
 *
 * @version $Id: sdpFiller.cc,v 1.20 2013/04/16 14:57:15 friedel Exp $
 *
 * @usage sdpFiller sdpconffile=sdp/sdp.conf reset={true|false}
 *           infile=<input astro hdr file> outfile=<output SDP file>
 *           stopfile=/tmp/sdpFiller.stop 
 *
 * @description
 * This application reads the input astronomical header files created
 * by the astroHeaderWriter, in conjunction with their associated
 * visibility data files (visbrick files) produced by the correlator
 * pipeline sub-system, to create an output dataset in the canonical
 * science data products format, here chosen to be Miriad.
 *
 * @key	sdpconffile sdp/sdp.conf s SDP configuration file.
 * @key reset       false        b Hard reset on start.
 * @key starttime   "start"      s Start time in \"'format' %H:%M:%s\" format.
 * @key endtime     "end"        s End time in \"'format' %H:%M:%s\" format.
 * @key timezone    "PST"        s Time zone (UTC, PST, PDT)
 * @key format      "%Y-%b-%d"   s Date format (default: YYYY-Jan-DD).
 * @key corrtype    "acxc"       s Correlation type (xc, ac, acxc)
 * @key infile      ""           s Input astronomical header file
 * @key outfile     ""           s Output science data product file.
 * @key append      false        b Set true to append to output files (else rewrite).
 * @key rt          false        b Real-time mode (include .write astro hdrs incrementally).
 * @key pdb         false        b Set true to update project database.
 * @key sleep       120          i Sleep interval (seconds).
 * @key	stopfile    /tmp/sdpFiller.stop s Exit gracefully if this file exists.
 * @key	float       false         b If true, write floating point instead of scaled integer. This option should be set to true if the dynamic range of the data is greater than 32000.
 * @key getScript   true         b If true add the observing script to the output miriad file
 * @key force       false        b If true force a full refill, else just update the pdb
 *
 * @logger DEFAULT_FACILITY carma.sdp.sdpFiller
 *
 * @author Athol Kemball & Harold Ravlin
 */

// Carma includes
#include <carma/sdp/SDPFiller.h>
#include <carma/correlator/lib/CorrelatorConfigChecker.h>
#include <carma/util/Program.h>
#include <carma/util/Time.h>
#include <carma/util/KeyValueConfigFile.h>
#include <carma/util/ErrorException.h>
#include <carma/util/Trace.h>

// Carma tools includes
#include <log4cpp/Priority.hh>

// Std C++ includes
#include <iostream>
#include <string>
#include <map>
#include <exception>
#include <memory>
#include <climits>
#include <cstdlib>

// namespace directives
using carma::sdp::SDPFiller;
using carma::util::Program;

// Main
int carma::util::Program::main() {

  try {
    // Get input parameter values
    const std::string sdpconffile = 
      getConfFile(getStringParameter("sdpconffile"));
    const bool reset = getBoolParameter("reset");
    const std::string starttime = getStringParameter("starttime");
    const std::string endtime = getStringParameter("endtime");
    const std::string timezone = getStringParameter("timezone");
    const std::string format = getStringParameter("format");
    const std::string corrtype = getStringParameter("corrtype");
    const std::string infile = getStringParameter("infile");
    const std::string outfile = getStringParameter("outfile");
    const bool append = getBoolParameter("append");
    const bool rt = getBoolParameter("rt");
    const bool pdb = getBoolParameter("pdb");
    const int sleep = std::abs(getIntParameter("sleep"));
    const std::string stopfile = getStringParameter("stopfile");
    bool writeFloats     = getBoolParameter("float");
    const bool getScript       = getBoolParameter("getScript");
    const bool force           = getBoolParameter("force");
    

    // Set time zone enum
    carma::util::Time::TimeZone tz;
    if (timezone.find("PST") != std::string::npos) {
      tz = carma::util::Time::PST;
    } else if (timezone.find("PDT") != std::string::npos) {
      tz = carma::util::Time::PDT;
    } else if (timezone.find("UTC") != std::string::npos) {
      tz = carma::util::Time::UTC;
    } else {
      tz = carma::util::Time::LOCAL;
    };

    // Convert start and end input times to start and end frame counts
    carma::util::frameType startFrame = 0;
    carma::util::frameType endFrame = UINT_MAX;
    if (starttime.find("start") == std::string::npos) {
      startFrame = 
	carma::util::Time::computeClosestFrame(starttime, 
					       format+" %H:%M:%S", tz);
    };
    if (endtime.find("end") == std::string::npos) {
      endFrame = 
	carma::util::Time::computeClosestFrame(endtime, format+" %H:%M:%S", 
					       tz);

    };
    CARMA_CPTRACE(carma::util::Trace::TRACE6, "Start frame:" << startFrame
		  << ", end frame:" << endFrame);

    // Restrict allowed project database update to the case when
    // running in real-time mode, without time selection.
    bool projectDB = (pdb && rt && (startFrame == 0) && (endFrame = UINT_MAX));

    // Read configuration file parameters
    std::map<std::string, std::string> sdpconf = 
      carma::util::KeyValueConfigFile::load(sdpconffile);

    // Retrieve the recycle, astronomical header, visbrick and 
    // science data product format directories from the science data 
    // products sub-system configuration file.
    std::string astroHeaderDir = sdpconf["astroHeaderDir"];
    std::string visBrickDir = sdpconf["visBrickDir"];
    std::string scienceDataFormatDir = sdpconf["scienceDataFormatDir"];
    std::string recycleDir = sdpconf["recycleDir"];

    // Construct an SDPFiller object
    SDPFiller* sdpFiller = new SDPFiller(astroHeaderDir, visBrickDir, 
					 scienceDataFormatDir, 
					 recycleDir, sleep, stopfile,
					 writeFloats, getScript, force);

    // Perform reset on start-up as requested
    if (reset) {
      sdpFiller->reset();
      CARMA_CPTRACE(carma::util::Trace::TRACE6, "Reset on start");
    };

    // Initialize the correlator configuration
    std::string cconf = "conf/correlator/correlator.conf";
    carma::correlator::lib::CorrelatorConfigChecker* ccc =
      carma::correlator::lib::CorrelatorConfigChecker::getInstance(cconf);
    ccc->start();

    // Processing options:
    // infile	outfile	Action
    // Y	Y	Process infile to outfile.
    // N	Y	Concatenate all infiles in frame range to outfile.
    // Y	N	Process specified infile to matching outfile.
    // N	N	Process all infiles in frame range to matching outfiles
    std::vector<std::string> infileList(1, infile);
    if (!outfile.empty()) {
      if (!infile.empty()) {
	sdpFiller->processFiles(infileList, startFrame, endFrame, corrtype,
				outfile, append, projectDB);
      } else {
	sdpFiller->processFrameCountRange(startFrame, endFrame, corrtype, 
					  outfile, append, rt, projectDB);
      };
    } else {
      if (!infile.empty()) {
	sdpFiller->processFiles(infileList, startFrame, endFrame, corrtype,
				append, projectDB);
      } else {
	sdpFiller->processAll(startFrame, endFrame, corrtype, rt, projectDB);
      };
    };

    // Exit
    delete(sdpFiller);
    delete(ccc);
    Program::getLogger() << log4cpp::Priority::INFO << "carma::sdp::sdpFiller - "
			 << " Program exiting normally";
    return EXIT_SUCCESS;

  } catch (const carma::util::ErrorException& exc) {
    Program::getLogger() << log4cpp::Priority::ERROR << "carma::sdp::sdpFiller - "
			 << "std exception caught " << exc.what();
    CARMA_CPTRACE(carma::util::Trace::TRACE3, "ErrorException" << exc);
    return EXIT_FAILURE;

  } catch (const std::exception& exc) {
    Program::getLogger() << log4cpp::Priority::ERROR << "carma::sdp::sdpFiller - "
			 << "std exception caught " << exc.what();
    CARMA_CPTRACE(carma::util::Trace::TRACE3, "std exception" << exc.what());
    return EXIT_FAILURE;
  } catch (...) {
    Program::getLogger() << log4cpp::Priority::ERROR << "carma::sdp::sdpFiller - "
			 << "unknown exception caught";
    CARMA_CPTRACE(carma::util::Trace::TRACE3, "unknown exception");
    return EXIT_FAILURE;
  };
};
  



