/*
 * @file
 *
 * RTD definition for BIMA telemetry window
 *
 * @author Chul Gwon (from Steve Scott)
 * $Id: rtdbimatelemetry.cc,v 1.7 2013/11/19 03:41:14 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <vector>

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
using namespace carma::ui::rtd;

#include "carma/monitor/BimaSubsystem.h"
using namespace carma::monitor;

#include "carma/util/Program.h"

static std::string makeHelp()
{
	return "BIMA Telemetry Help\n"
		   "\n"
		   "Status of the BIMA telemetry subsystem\n";
}

int carma::util::Program::main()
{
	const std::string title = "BIMA Telemetry";
	const std::string helpTitle = "BIMA Telemetry";

	// Create a dislay
	MonitorDisplay display(title);
	display.setSpecificHelp(helpTitle, makeHelp());

	const int width = 11;  // column width
	std::vector<std::string> columnLabel;
	std::vector<MonitorContainer*> container1;
	for (int i=0; i < 9; i++) {
		BimaSubsystem &bima = display.cms().bima(i);
		std::ostringstream oss;

		oss << "C" << i+7;
		columnLabel.push_back(oss.str());
		container1.push_back(&bima.bimaSpecific().telemetry());
	}

	// Create folders with tables
	MonitorSingleTableFolderPtr folder1(new MonitorSingleTableFolder(
				"telemetry", columnLabel, container1, width));

	// Add the folders to the display
	display.add(folder1);

	// Loop forever serving data to the client
	while (display.serveData()) {
		/* none */
	}

	return EXIT_SUCCESS;
}
