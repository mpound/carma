/*
 * @version $Revision: 1.2 $
 * @usage @autogen
 *
 * @key file "sdp/astroheaderwriter.conf" string
 * The AstroHeaderWriter configuration and control filename
 *
 * @key framecount "CURRENT" string
 * The framecount to emulate.
 *
 * @key corltype "ALL" string
 * The correlator to emulate. Valid values: WB, SL, ALL.
 *
 * @key output "ALL" string
 * The output to expand. Valid values are the first column in the configuration
 * file, as well as the special keyword ALL.
 *
 * @logger TEST_FACILITY carma.test.sdp.AstroHeaderWriter.ahwExpander
 *
 * @description
 * AstroHeaderWriter Configuration Expander
 *
 * This will expand the configuration as much as possible using the same
 * infrastructure as the AstroHeaderWriter. It is helpful when modifying
 * the configuration file.
 */

//@TODO Support C3G modes

#include <carma/sdp/AHW_Utils.h>
#include <carma/sdp/AHW_Output.h>
#include <carma/sdp/AHW_Evaluator.h>

#include <carma/util/Time.h>
#include <carma/util/Program.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/ErrorException.h>
#include <carma/util/programLogging.h>

#include <iostream>
#include <string>
#include <sstream>
#include <vector>

#include <boost/foreach.hpp>

using namespace carma::sdp;
using namespace carma::util;


static carma::util::CorrelatorType convertCorlType(const std::string &corl)
{
	if (corl == "ALL")
		return CORR_ALL;

	if (corl == "WB")
		return CORR_WIDEBAND;

	if (corl == "SL")
		return CORR_SPECTRAL;

	throw CARMA_ERROR("unknown correlator type specified: " + corl);
}

static unsigned int convertFramecount(const std::string &fc)
{
	if (fc == "CURRENT")
		return Time::computeClosestFrame();

	if (fc == "FIRST")
		return NEWAHWVERSIONDATE;

	if (fc == "SECOND")
		return SECONDNEWAHWVERSIONDATE;

	if (fc == "THIRD")
		return THIRDNEWAHVERSIONDATE;

	if (fc == "FOURTH")
		return FOURTHNEWAHVERSIONDATE;

	std::istringstream iss;
	unsigned int num;

	iss.str(fc);
	if (!(iss >> num)) {
		std::ostringstream oss;

		oss << "could not parse \"" << fc << "\" as an int";
		throw CARMA_ERROR(oss.str());
	}

	return num;
}

/* ========================================================================== */
/* Main Program                                                               */
/* ========================================================================== */

int carma::util::Program::main()
try {
	const std::string &file = getConfFile(getStringParameter("file"));
	const std::string &corl_str = getStringParameter("corltype");
	const std::string &output_var = getStringParameter("output");
	const std::string &fc_str = getStringParameter("framecount");

	// convert strings to native types
	const carma::util::CorrelatorType corl = convertCorlType(corl_str);
	const unsigned int framecount = convertFramecount(fc_str);

	const std::vector<AHW_Output> outputs = parseAHWControlFile(file);
	AHW_Evaluator evaluator;

	// prepare the evaluator
	if (corl == CORR_ALL) {
		evaluator.prepareAll();
	} else {
		// empty map means simulate
		const MPValueMap map;
		evaluator.prepare(corl, framecount, map);
	}

	// internal monitor points requested
	if (output_var == "ALL" || output_var == "INTERNAL") {

		std::cout << "-------------------------------------" << std::endl;
		std::cout << "EXPANSION FOR INTERNAL MONITOR POINTS" << std::endl;
		std::cout << "-------------------------------------" << std::endl;

		const StringVector mps = evaluator.getMPInternal();
		BOOST_FOREACH(const std::string &mp, mps) {
			std::cout << mp << "\n";
		}
	}

	// run for each monitor point expansion
	BOOST_FOREACH(const AHW_Output &ao, outputs) {

		// skip unwanted variables
		if (output_var != "ALL" && output_var != ao.outputName())
			continue;

		std::cout << "--------------------------------------------------" << std::endl;
		std::cout << "EXPANSION FOR OUTPUT VARIABLE: " << ao.outputName() << std::endl;
		std::cout << "--------------------------------------------------" << std::endl;

		const StringVector mps = evaluator.getMPExpansion(ao);
		BOOST_FOREACH(const std::string &mp, mps) {
			std::cout << mp << "\n";
		}
	}

	return EXIT_SUCCESS;
} catch (...) {
	std::ostringstream oss;
	oss << "EXCEPTION: " << getStringForCaught();
	std::cerr << oss.str() << std::endl;
	programLogErrorIfPossible(oss.str());
	return EXIT_FAILURE;
}

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
