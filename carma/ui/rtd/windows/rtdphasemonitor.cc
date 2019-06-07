/*
 * @author Colby Gutierrez-Kraybill
 * @author Chul Gwon
 * $Id: rtdphasemonitor.cc,v 1.6 2013/11/19 03:41:14 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <vector>
using namespace std;

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
using namespace carma::ui::rtd;

#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/PhaseMonitorSubsystem.h"
using namespace carma::monitor;

#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"
using namespace carma::util;

int carma::util::Program::main()
{
    const string file = Program::getConfFile("rtd/help/phasemonitor.html");
	const std::string title = "Phase Monitor";

	// Create a dislay
	MonitorDisplay display(title);
    display.setSpecificHelpFromTextFile( "Phase Monitor Window Help", file);
	PhaseMonitorSubsystem &pms = display.cms().phaseMonitor();

	// Phases tab
	MonitorSingleTableFolderPtr pmsFolder(new MonitorSingleTableFolder(
				"Phases", MonitorColumnInfo("Phases", pms)));
	display.add(pmsFolder);

	// Instrument tab
	MonitorSingleTableFolderPtr imcFolder(new MonitorSingleTableFolder(
				"Instrument", MonitorColumnInfo("Instrument", pms.instrument())));
	display.add(imcFolder);

    // Arms tab
	std::vector<MonitorColumnInfo> armsVec;
    armsVec.push_back(MonitorColumnInfo("NE Arm", pms.neArm()));
    armsVec.push_back(MonitorColumnInfo("SW Arm", pms.swArm()));
    MonitorSingleTableFolderPtr armsFolder(new MonitorSingleTableFolder(
                "Arms", armsVec));
    display.add(armsFolder);

	while (display.serveData()) {
		// none
	}

	return 0;
}
