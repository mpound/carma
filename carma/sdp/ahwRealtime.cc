/*
 * @version $Revision: 1.2 $
 * @usage @autogen
 *
 * @key file "sdp/astroheaderwriter.conf" string
 * The AstroHeaderWriter configuration and control filename
 *
 * @key corltype "SL" string
 * The correlator to emulate. Valid values: WB, SL.
 *
 * @key inputCMS "final" string
 * Monitor system to use for input. One of { raw, intermediate, final }
 *
 * @logger TEST_FACILITY carma.test.sdp.AstroHeaderWriter.ahwRealtime
 *
 * @description
 * AstroHeaderWriter Realtime Debugging Tool
 *
 * This will run the guts of the AstroHeaderWriter in realtime, against the
 * CarmaMonitorSystem running on the local machine. It is helpful when
 * changing code in the AstroHeaderWriter and monitor points that it depends
 * upon.
 */

#include <carma/sdp/AHW_Utils.h>
#include <carma/sdp/AHW_Output.h>
#include <carma/sdp/LineBuffer.h>
#include <carma/sdp/AHW_Evaluator.h>
#include <carma/sdp/AstroHdrElement.h>
#include <carma/sdp/MonitorPointValue.h>

#include <carma/monitor/MonitorPoint.h>
#include <carma/monitor/MonitorSystem.h>
#include <carma/monitor/MonitorPointIterator.h>
#include <carma/monitor/MonitorSystemSelector.h>

#include <carma/util/Program.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/ErrorException.h>
#include <carma/util/programLogging.h>

#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include <boost/foreach.hpp>

using namespace carma::sdp;
using namespace carma::util;
using namespace carma::monitor;

typedef boost::shared_ptr<CarmaMonitorSystem> CmsPtr;

static carma::util::CorrelatorType convertCorlType(const std::string &corl)
{
	if (corl == "WB")
		return CORR_WIDEBAND;

	if (corl == "SL")
		return CORR_SPECTRAL;

	throw CARMA_ERROR("unknown correlator type specified: " + corl);
}

static MPValueMap createMPValueMap(const CmsPtr cms, const MPWantedMap &mpWanted)
{
	const carma::util::frameType framecount = cms->getFrameCount();
	MonitorPointIterator mpIter(*cms.get());
	MPValueMap mpValues;

	while (++mpIter) {
		const MonitorPoint &mp = mpIter.getMonitorPoint();
		const tagIDType tagID = mp.getTagID();

		// Determine if this monitor point is wanted
		const MPWantedMap::const_iterator it = mpWanted.find(tagID);

		// Skip monitor points not in the wanted map
		if (it == mpWanted.end())
			continue;

		// Construct monitor point value
		MonitorPointValuePtr mpv(new MonitorPointValue(framecount, mp));

		// Update the map of current monitor point values
		const std::string &mpName = it->second;
		mpValues[mpName] = mpv;
	}

	return mpValues;
}

/* ========================================================================== */
/* Main Program                                                               */
/* ========================================================================== */

int carma::util::Program::main()
try {
	const std::string &file = getConfFile(getStringParameter("file"));
	const std::string &corl_str = getStringParameter("corltype");
	const std::string &inputCMS = getStringParameter("inputCMS");

	// convert strings to native types
	const CorrelatorType corl = convertCorlType(corl_str);

	// parse the control file
	const std::vector<AHW_Output> outputs = parseAHWControlFile(file);

	// create the map of wanted monitor points
	const MPWantedMap mpWanted = createWantedMap(outputs);

	// attach to CARMA monitor system
	std::string unused;
	const CmsSelector selector = convertStringToCmsSelector(inputCMS);
	CmsPtr cms(makeCms(selector, unused).release());

	// check that the monitor system is receiving updates
	if (!cms->isActive()) {
		std::cerr << "Monitor System is not receiving data updates" << std::endl;
		std::cerr << "Make sure you are running this program on the ACC" << std::endl;
		return EXIT_FAILURE;
	}

	while (true) {
		// wait until next carma monitor system update
		cms->read();
		const carma::util::frameType framecount = cms->getFrameCount();

		// construct the mp value map
		const MPValueMap mpValues = createMPValueMap(cms, mpWanted);

		// get the input for one integration
		AstroHeaderElementMap astroHdrMap = createAstroHeaderRecord(corl, framecount, outputs, mpValues);

		// Output to stdout
		LineBuffer buf(std::cout.rdbuf());
		astroHdrMap.dumpTable(buf, framecount);
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
