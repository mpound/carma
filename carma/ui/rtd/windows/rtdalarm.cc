
/*
 * rtdalarm.cc
 *
 * Gets and displays data about the Alarm subsystem
 *
 * @author Colby Gutierrez-Kraybill
 * $Id: rtdalarm.cc,v 1.5 2013/11/19 03:41:14 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <vector>
using namespace std;

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
using namespace carma::ui::rtd;

#include "carma/monitor/AlarmSubsystem.h"
using namespace carma::monitor;

#include "carma/util/Program.h"

static std::string makeHelp() {
	return "Alarm Subsystem Help\n"
		   "\n"
		   "Displays information about the alarm subsystem.\n";
}

int carma::util::Program::main()
{
	const std::string subsystemName("Alarm Subsystem");
	const std::string name("Alarm");

	// Create a dislay
	MonitorDisplay display(subsystemName);
	display.setSpecificHelp(subsystemName, makeHelp());

	MonitorContainer &alarm = display.cms().alarm();
	MonitorColumnInfo info(name, alarm);

	MonitorSingleTableFolderPtr mstf(new MonitorSingleTableFolder(name, info, AUTO_SIZE_EACH_COLUMN));

	display.add(mstf);

	while (display.serveData()) {
		/* nothing */
	}

	return 0;
}

