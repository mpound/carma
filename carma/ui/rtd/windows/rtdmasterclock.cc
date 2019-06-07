/*
 * @file
 *
 * Display masterclock status
 *
 * @author Steve Scott
 * $id: $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <vector>
using namespace std;

#include "carma/monitor/MasterClockSubsystem.h"
using namespace carma::monitor;

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
using namespace carma::ui::rtd;

#include "carma/util/Program.h"

static std::string makeHelp()
{
	ostringstream oss;
	oss << "	   CARMA MASTERCLOCK HELP\n\n"
		<< "Put the help here, "
		<< "describing all non-obvious parameters and how it works... "
		<< "";
	return oss.str();
}

int carma::util::Program::main()
{
	// Create a dislay
	MonitorDisplay display("Masterclock");
	display.setSpecificHelp("MasterClock Help", makeHelp());

	const int width = AUTO_SIZE;

	// Create a folder with a single table inside
	MonitorSingleTableFolderPtr folder1(new MonitorSingleTableFolder("Clock", "Values", display.cms().masterclock().clock(), width));

	// Add the folder to the display
	display.add(folder1);

	// Create another folder with a single table inside
	MonitorSingleTableFolderPtr folder2(new MonitorSingleTableFolder("XAC", "Values", display.cms().masterclock().clock().xac(), width));

	// Add the folder to the display
	display.add(folder2);

	// Loop forever, serving data on request
	while (display.serveData()) {
	}

	return EXIT_SUCCESS;
}
