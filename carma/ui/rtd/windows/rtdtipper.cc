/*
 * rtdtipper.cc
 *
 * Gets and displays data about the Opacity Monitor (aka Tipper)
 *
 * @author Colby Gutierrez-Kraybill
 * $Id: rtdtipper.cc,v 1.6 2013/11/19 03:41:15 iws Exp $
 *
 * $CarmaCopyright$
 */

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
using namespace carma::ui::rtd;

#include "carma/monitor/OpacityMonitorSubsystem.h"
using namespace carma::monitor;

#include "carma/util/Program.h"

static std::string makeHelp()
{
	return "Opacity Monitor Help\n"
		   "\n"
		   "Status of the OpacityMonitor subsystem\n";
}

int carma::util::Program::main()
{
	const std::string title = "Opacity Monitor";

	// Create a dislay
	MonitorDisplay display(title);
	display.setSpecificHelp(title, makeHelp());

	OpacityMonitorSubsystem &oms = display.cms().opacityMonitor();
	MonitorSingleTableFolderPtr omsFolder(new MonitorSingleTableFolder(
				"Opacity", MonitorColumnInfo("Opacity", oms)));
	display.add(omsFolder);

	while (display.serveData()) {
		// none
	}

	return 0;
}
