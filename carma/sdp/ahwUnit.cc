/*
 * @version $Revision: 1.2 $
 * @usage @autogen
 *
 * @key file "sdp/astroheaderwriter.conf" string
 * The AstroHeaderWriter configuration and control filename
 *
 * @key inputDir "/tmp/ahwInput" string
 * The AstroHeaderWriter input directory
 *
 * @key outputDir "/tmp/ahwOutput" string
 * The AstroHeaderWriter output directory
 *
 * @key recycleDir "/tmp/ahwRecycle" string
 * The AstroHeaderWriter recycle directory
 *
 * @key corl "SL" string
 * The Correlator Type to emulate
 *
 * @key reset true bool
 * Reset the input and output directories (start from scratch)
 *
 * @key process true bool
 * Run a single iteration of the AstroHeaderWriter processing loop
 *
 * @logger TEST_FACILITY carma.test.sdp.AstroHeaderWriter.ahwUnit
 *
 * @description
 * AstroHeaderWriter Unit Test
 *
 * This is a simple unit test suite building block for the AstroHeaderWriter.
 * It will reset the directories given, and then run a single iteration of
 * the main AstroHeaderWriter event loop, then exit. It does not deal with
 * changing sets of files.
 */

#include <carma/sdp/AHW_Utils.h>
#include <carma/sdp/AHW_Output.h>
#include <carma/sdp/AHW_Processor.h>

#include <carma/util/Program.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/ErrorException.h>
#include <carma/util/programLogging.h>

#include <iostream>
#include <sstream>
#include <string>
#include <vector>

using namespace carma::sdp;

/* ========================================================================== */
/* Main Program                                                               */
/* ========================================================================== */

static carma::util::CorrelatorType convertCorlType(const std::string &corl)
{
	if (corl == "WB")
		return carma::util::CORR_WIDEBAND;

	if (corl == "SL")
		return carma::util::CORR_SPECTRAL;

	throw CARMA_ERROR("unknown correlator type specified: " + corl);
}

int carma::util::Program::main()
try {
	using carma::monitor::DataflowSubsystem;

	const std::string file = getConfFile(getStringParameter("file"));
	const std::string inputDir = getStringParameter("inputDir");
	const std::string outputDir = getStringParameter("outputDir");
	const std::string recycleDir = getStringParameter("recycleDir");
	const carma::util::CorrelatorType corlType = convertCorlType(getStringParameter("corl"));
	const std::vector<AHW_Output> outputs = parseAHWControlFile(file);

	AHW_Processor ahw(corlType, inputDir, outputDir, recycleDir, outputs, NULL);

	if (getBoolParameter("reset"))
		ahw.reset();

	if (getBoolParameter("process"))
		ahw.run_single_iteration(0, 0);

	return EXIT_SUCCESS;
} catch (...) {
	std::ostringstream oss;
	oss << "EXCEPTION: " << carma::util::getStringForCaught();
	std::cerr << oss.str() << std::endl;
	programLogErrorIfPossible(oss.str());
	return EXIT_FAILURE;
}

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
