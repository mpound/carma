/*
 * @file
 *
 * A template to be used for realtime windows.
 * Replace the strings and edit the commented out example section
 *
 * @author Original: somebody
 * $Id: rtdtemplate.cc,v 1.8 2012/02/21 23:45:37 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <vector>

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
using namespace carma::ui::rtd;

#include "carma/monitor/OvroSubsystem.h" // REPLACE with your subsystem header
using namespace carma::monitor;

#include "carma/util/Program.h"

// REPLACE these example strings with ones appropriate for your window
static std::string title = "OVRO Antenna Cryo Compressor";
static std::string helpTitle = "OVRO Cryo Compressor";
static std::string helpPageHeading = "OVRO ANTENNA CRYO HELP\n\n";
static std::string helpSummary = "Status of the OVRO antenna cryo subsystem. ";

static std::string makeHelp()
{
	std::ostringstream oss;
	oss << helpPageHeading
		<< helpSummary
		<< "Fill in more info here... "
		;
	return oss.str();
}

int carma::util::Program::main()
{
	// Create a dislay
	MonitorDisplay display(title);
	display.setSpecificHelp(helpTitle, makeHelp());

#if 0
/* -----Example of a two folders, each with a single table...
	const int width = 11;  // width of the columns
	std::vector<std::string> columnLabel;

	// need multiple containers if you want multiple tabs
	// in the same window window (o/w you just need one)
	std::vector<MonitorContainer*> container1;
	std::vector<MonitorContainer*> container2;

	// REPLACE "6" with appropriate number of columns
	// (6 here refers to the number of OVRO antennas)
	for (int i=0; i < 6; i++) {
		ostringstream oss;
		oss << "Ovro" << i+1;
		columnLabel.push_back(oss.str());
		container1.push_back(&display.cms().ovro(i).cryo().compressor());
		container2.push_back(&display.cms().ovro(i).cryo().compressor().xac());
	}

	// Put information in tabs for the RTD window
	MonitorSingleTableFolder folder1("compressor", columnLabel, container1, width);
	MonitorSingleTableFolder folder2("XAC", columnLabel, container2, width);

	// Add the folders to the display
	display.add(&folder1);

	// Add the folders to the display
	display.add(&folder2);
#endif

	// Loop forever serving data to the client
	while (display.serveData()) {
	}

	return EXIT_SUCCESS;
}

/* vim: set ts=4 sts=4 sw=4 noet: */
