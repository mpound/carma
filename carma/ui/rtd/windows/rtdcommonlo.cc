/*
 * @file
 * Antenna common Calibrator monitor points.
 *
 * $Id: rtdcommonlo.cc,v 1.3 2013/11/19 03:41:14 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <vector>

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
using namespace carma::ui::rtd;

#include "carma/monitor/AntennaCommon.h"
using namespace carma::monitor;

#include "carma/util/Program.h"

static std::string makeHelp()
{
	return "Common Antenna LO Help\n"
		   "\n"
		   "Status of the Common Antenna LO\n";
}

int carma::util::Program::main()
{
	const std::string title = "Common Antenna Local Oscillator Status";
	const std::string helpTitle = "COMMON ANTENNA LO";

	// Create a dislay
	MonitorDisplay display(title);
	display.setSpecificHelp(helpTitle, makeHelp());

	const int width = 8;  // width of the columns
	std::vector<std::string> columnLabel;
	std::vector<MonitorContainer*> container1;

	// 23 here refers to the number of OVRO+BIMA+SZA antennas
	for (int carmaAntNo = 1; carmaAntNo <= 23; ++carmaAntNo) {
		AntennaCommon &common = display.cms().antennaCommon(carmaAntNo - 1);
		std::ostringstream oss;

		oss << "C" << carmaAntNo;
		columnLabel.push_back(oss.str());
		container1.push_back(&common.lO());
	}

	// Create folders with tables
	MonitorSingleTableFolderPtr folder1(new MonitorSingleTableFolder(
				"LO", columnLabel, container1, width));

	// Add the folders to the display
	display.add(folder1);

	// Loop forever serving data to the client
	while (display.serveData()) {
	}

	return EXIT_SUCCESS;
}
