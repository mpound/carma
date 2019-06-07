/*
 * @version $Revision: 1.2 $
 * @usage @autogen
 *
 * @key type "sl" string
 * The Pipeline type to emulate (sl, wb, c3g8, c3g23)
 *
 * @logger TEST_FACILITY carma.test.pipeline.tPipelineSHM
 *
 * @description
 * Pipeline shared memory emulator and test program
 *
 * This program emulates pipeline by writing a pattern to the shared memory
 * transport used for blank/flag data in RTD.
 */

#include <carma/correlator/lib/CorrelatorSideband.h>
using carma::correlator::lib::CorrelatorSideband;

#include <carma/pipeline/pipelineUtils.h>
#include <carma/pipeline/PipelineTransport.h>
using namespace carma::pipeline;

#include <carma/util/Program.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/ErrorException.h>
#include <carma/util/programLogging.h>
using namespace carma::util;

#include <iostream>
#include <string>
#include <sstream>
#include <vector>

#include <boost/foreach.hpp>
#include <boost/shared_ptr.hpp>

/* -------------------------------------------------------------------------- */
/* Main Program                                                               */
/* -------------------------------------------------------------------------- */

int carma::util::Program::main()
try {
	const std::string type = getStringParameter("type");
	const enum PipelineType pt = stringToPipelineType(type);

	// choose number of antennas to connect based on correlator type
	int nAnts = 8;
	if (pt == WB)
		nAnts = 8;
	else if (pt == SL)
		nAnts = 15;
	else if (pt == C3G23)
		nAnts = 23;
	else if (pt == C3G8)
		nAnts = 8;
	else
		throw CARMA_ERROR("unknown correlator type!");

	const AstrobandRange range = getAstrobandRange(pt);
	PipelineTransportWriter writer(pt);
	int antOffset = 0;

	while (true) {
		writer.clear();

		// back to the beginning
		if (antOffset > (23 - nAnts))
			antOffset = 0;

		for (int i = 0; i < nAnts; i++) {
			for (int j = 0; j < nAnts; j++) {
				const int antNo1 = i + antOffset + 1;
				const int antNo2 = j + antOffset + 1;

				for (unsigned int band = range.first; band <= range.second; band++) {
					// auto baselines will be "flagged"
					if (antNo1 == antNo2) {
						const uint32_t flags = CorrelatorSideband::A1_SHADOWED | CorrelatorSideband::A2_SHADOWED;
						writer.setBaseline(band, antNo1, antNo2, false, flags);
						continue;
					}

					// usb baselines will be "blanked"
					{
						const uint32_t flags = CorrelatorSideband::MANUAL_FLAG | CorrelatorSideband::A1_CALSTATE;
						writer.setBaseline(band, antNo1, antNo2, false, flags);
					}

					// lsb baselines will be "ok"
					{
						const uint32_t flags = CorrelatorSideband::NO_REASON;
						writer.setBaseline(band, antNo1, antNo2, true, flags);
					}
				}
			}
		}

		// move the block of antennas around inside the window
		antOffset++;

		writer.write();
		usleep(500 * 1000);
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
