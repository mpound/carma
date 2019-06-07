/*
 *
 * Display status of ovro antenna pointing.
 *
 * @author Original: Steve Scott
 * $Id: rtdovropoint.cc,v 1.5 2013/11/19 03:41:14 iws Exp $
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
	oss << "       OVRO ANTENNA POINTING HELP\n\n"
		<< "Status of the OVRO antenna pointing, part of the drive subsystem. "
		<< "This information is shipped directly from the microvax controller. ";
	return oss.str();
}

int carma::util::Program::main()
{
    // Create a dislay
    MonitorDisplay display("OVRO Antenna Pointing Status");
    display.setSpecificHelp("OVRO Pointing Help", makeHelp());

    int width = 11;
    vector<string> columnLabel;
    vector<MonitorContainer*> container1;
    vector<MonitorContainer*> container2;
    for (int i=0; i < 6; i++) {
        ostringstream o;
        o << "Ovro#" << i+1;
        columnLabel.push_back(o.str());
        container1.push_back(&display.cms().ovro(i).drive().point());
        container2.push_back(&display.cms().ovro(i).drive().point().constants());
    }

    // Create folders with tables
    MonitorSingleTableFolderPtr folder1(new MonitorSingleTableFolder(
                "pointing", columnLabel, container1, width));
    MonitorSingleTableFolderPtr folder2(new MonitorSingleTableFolder(
                "pointingConstants", columnLabel, container2, width));

    // Add the folders to the display
    display.add(folder1);
    display.add(folder2);

    while (display.serveData()) {
    }

    return 0;
}
