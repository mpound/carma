/*
 *
 * Display status of ovro antenna tracking.
 *
 * @author Original: Steve Scott
 * $Id: rtdovrotrack.cc,v 1.4 2013/11/19 03:41:14 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <vector>

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/util/Program.h"

using namespace std;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui::rtd;

static std::string makeHelp()
{
	std::ostringstream oss;
	oss << "       OVRO ANTENNA DRIVE TRACKING HELP\n\n"
		<< "Status of the OVRO antenna drive subsystem tracking. "
		<< "This information is shipped directly from the microvax controller. ";
	return oss.str();
}

int carma::util::Program::main()
{
	// Create a dislay
	MonitorDisplay display("OVRO Antenna Tracking Status");
	display.setSpecificHelp("OVRO Tracking Help", makeHelp());

	const int width = 11;
	std::vector<std::string> columnLabel;
	std::vector<MonitorContainer*> container1;
	for (int i=0; i < 6; i++) {
		std::ostringstream oss;
		oss << "Ovro#" << i+1;
		columnLabel.push_back(oss.str());
		container1.push_back(&display.cms().ovro(i).drive().track());
	}

	// Create folders with tables
	MonitorSingleTableFolderPtr folder1(new MonitorSingleTableFolder(
				"tracking", columnLabel, container1, width));

	// Add the folders to the display
	display.add(folder1);

	while (display.serveData()) {
	}

	return 0;
}
