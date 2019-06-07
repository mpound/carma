/*
 * @file
 *
 * Display monitor points for Weather Subsystem
 *
 * @author Original: Chul Gwon (from Steve Scott)
 * $Id: rtdweather.cc,v 1.6 2013/11/19 03:41:15 iws Exp $
 *
 * $CarmaCopyright$
 */

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
using namespace carma::ui::rtd;

#include "carma/monitor/WeatherSubsystem.h"
using namespace carma::monitor;

#include "carma/util/Program.h"

static std::string makeHelp()
{
	return "Weather Help\n"
		   "\n"
		   "Status of the CARMA Weather Station\n";
}

int carma::util::Program::main()
{
	const std::string title = "Weather Station";
	const std::string helpTitle = "Weather Station Subsystem";

	// Create a dislay
	MonitorDisplay display(title);
	display.setSpecificHelp(helpTitle, makeHelp());

	WeatherSubsystem &weather = display.cms().weather();

	// Weather Folder
	MonitorSingleTableFolderPtr summaryFolder(new MonitorSingleTableFolder(
				"Summary", MonitorColumnInfo("Summary", weather)));
	display.add(summaryFolder);

	// Weather Station Folder
	MonitorSingleTableFolderPtr wsFolder(new MonitorSingleTableFolder(
				"WeatherStation", MonitorColumnInfo("WeatherStation", weather.weatherStation())));
	display.add(wsFolder);

	// Dew Point Sensor Folder
	MonitorSingleTableFolderPtr dpsFolder(new MonitorSingleTableFolder(
				"DewPointSensor", MonitorColumnInfo("DewPointSensor", weather.dewPointSensor())));
	display.add(dpsFolder);

	// Loop forever serving data to the client
	while (display.serveData()) {
	}

	return EXIT_SUCCESS;
}
