/*
 * @version $Revision: 1.2 $
 * @usage @autogen
 *
 * @key file "sdp/astroheaderwriter.conf" string
 * The AstroHeaderWriter configuration and control filename
 *
 * @logger TEST_FACILITY carma.test.sdp.AstroHeaderWriter.ahwTest
 *
 * @description
 * AstroHeaderWriter Unit Test
 *
 * This unit test checks many various parts of the AstroHeaderWriter.
 *
 * 1) Check the configuration file parser
 *
 * This checks that the configuration file parses correctly. Any parsing
 * errors will be caught before the code goes into production.
 *
 * 2) Check historical monitor points
 *
 * This ensures that all monitor points used by the configuration file have
 * existed at some time in the CARMA Monitor System, even if they are not
 * present today.
 *
 * This means that both historical monitor points and current monitor
 * points are searched.
 *
 * 3) Check current monitor points
 *
 * This ensures that all monitor points used by the configuration file for
 * outputs that will be generated at the current frame exist in the
 * C++ CARMA Monitor System.
 *
 * This ensures that currently used monitor points have not been removed.
 */

#include <carma/sdp/AHW_Utils.h>
#include <carma/sdp/AHW_Output.h>
#include <carma/sdp/AHW_Evaluator.h>

#include <carma/dbms/TagIDAuthority.h>

#include <carma/monitor/MonitorSystem.h>
#include <carma/monitor/MonitorSystemSelector.h>

#include <carma/util/Time.h>
#include <carma/util/Program.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/ErrorException.h>
#include <carma/util/programLogging.h>

#include <iostream>
#include <string>
#include <sstream>
#include <vector>

#include <boost/algorithm/string/predicate.hpp>
#include <boost/foreach.hpp>
#include <boost/shared_ptr.hpp>

using namespace carma::sdp;
using namespace carma::util;

typedef boost::shared_ptr<carma::monitor::CarmaMonitorSystem> CmsPtr;

/* ========================================================================== */
/* Main Program                                                               */
/* ========================================================================== */

static void checkMonitorPoints(const StringVector &mps)
{
	using namespace carma::dbms;
	const TagIDAuthority &authority = TagIDAuthority::getAuthority();

	// check that each monitor point exists
	BOOST_FOREACH(const std::string &mp, mps) {
		try {
			authority.lookupID(mp);
		} catch (...) {
			std::ostringstream oss;
			oss << "MP NAME LOOKUP FAILED: MP=" << mp
				<< " EXCEPTION: " << getStringForCaught();
			throw CARMA_ERROR(oss.str());
		}
	}
}

static void checkCurrentMonitorPoints(CmsPtr cms, const StringVector &mps)
{
	using namespace carma::monitor;

	// check that each monitor point exists
	BOOST_FOREACH(const std::string &mp, mps) {
		const MonitorPoint * const ptr = cms->getMonitorPointPtr(mp, true);

		// check that the monitor point was found (case sensitive)
		if (ptr == NULL) {
			std::ostringstream oss;
			oss << "MONITOR POINT DOES NOT EXIST IN CURRENT MONITOR SYSTEM: " << mp;
			throw CARMA_ERROR(oss.str());
		}

		// check that the monitor point is marked vital
		if (ptr->getArchivePriority() != MonitorComponent::VITAL) {
			std::ostringstream oss;
			oss << "MONITOR POINT DOES NOT HAVE VITAL PRIORITY: " << mp;
			throw CARMA_ERROR(oss.str());
		}
	}
}

static void test2(const std::vector<AHW_Output> &outputs)
{
	AHW_Evaluator evaluator;
	evaluator.prepareAll();

	// check each monitor point expansion
	BOOST_FOREACH(const AHW_Output &ao, outputs) {
		std::cout << "... processing output: " << ao.outputName() << std::endl;
		const StringVector mps = evaluator.getMPExpansion(ao);
		checkMonitorPoints(mps);
	}

	// check internal monitor points
	{
		std::cout << "... processing internally used monitor points" << std::endl;
		const StringVector mps = evaluator.getMPInternal();
		checkMonitorPoints(mps);
	}
}

static void test3(const std::vector<AHW_Output> &outputs, const carma::util::CorrelatorType type)
{
	using namespace carma::monitor;

	std::cout << "Checking correlator type " << type << std::endl;
	const frameType currentFrame = Time::computeClosestFrame();

	// prepare for the given correlator type and the current frame number
	const MPValueMap map;
	AHW_Evaluator evaluator;
	evaluator.prepare(type, currentFrame, map);

	// get a carma monitor system pointer
	const CmsSelector selector = convertStringToCmsSelector("raw");
	std::string unused;
	CmsPtr cms(makeCms(selector, unused).release());

	BOOST_FOREACH(const AHW_Output &ao, outputs) {
		// if the output does not apply to the current frame time, skip it
		if (currentFrame < ao.frameCountStart() || currentFrame > ao.frameCountEnd())
			continue;

		std::cout << "... processing output: " << ao.outputName() << std::endl;

		// remove any mp expansions which are invalid spm mappings
		const StringVector mps = evaluator.getMPExpansion(ao, true);
		checkCurrentMonitorPoints(cms, mps);
	}

	// check internal monitor points
	{
		std::cout << "... processing internally used monitor points" << std::endl;
		const StringVector mps = evaluator.getMPInternal();
		checkCurrentMonitorPoints(cms, mps);
	}
}

int carma::util::Program::main()
try {
	const std::string file = getConfFile(getStringParameter("file"));

	std::cout << "TEST 1: parse configuration / control file" << std::endl;
	const std::vector<AHW_Output> outputs = parseAHWControlFile(file);
	std::cout << "PASS" << std::endl;

	std::cout << "TEST 2: check historical monitor point existence" << std::endl;
	test2(outputs);
	std::cout << "PASS" << std::endl;

	std::cout << "TEST 3: check current monitor point existence (WB)" << std::endl;
	test3(outputs, CORR_WIDEBAND);
	std::cout << "PASS" << std::endl;

	std::cout << "TEST 4: check current monitor point existence (SL)" << std::endl;
	test3(outputs, CORR_SPECTRAL);
	std::cout << "PASS" << std::endl;

	return EXIT_SUCCESS;
} catch (...) {
	std::ostringstream oss;
	oss << "EXCEPTION: " << getStringForCaught();
	std::cerr << oss.str() << std::endl;
	programLogErrorIfPossible(oss.str());
	return EXIT_FAILURE;
}

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
