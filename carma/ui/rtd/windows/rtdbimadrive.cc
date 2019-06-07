/*
 * rtdbimadrive.cc
 *
 * Gets and displays data about the BIMA drives
 *
 * @author Colby Gutierrez-Kraybill
 * @author Chul Gwon
 * $Id: rtdbimadrive.cc,v 1.17 2013/11/19 03:41:14 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <vector>

#include <boost/foreach.hpp>

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/ZebraVisitor.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
using namespace carma::ui::rtd;

#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/BimaSubsystem.h"
using namespace carma::monitor;

#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"

typedef std::vector<MonitorContainer *> MCVector;
typedef std::map<std::string, MCVector> TabDataType;

static std::string makeHelp()
{
	return "BIMA Antenna Drives Help\n"
		   "\n"
		   "Status of the BIMA Antenna Drives subsystem\n";
}

static MonitorContainer *
appendContainer(const MonitorContainer &a, const MonitorContainer &b)
{
	const std::string name = a.getName() + "+" + b.getName();
	MonitorContainer *c = new MonitorContainer(name);

	MonitorPointIterator ampi(a, 1);
	while (ampi++) {
			c->add(ampi.getMonitorPoint());
	}

	MonitorPointIterator bmpi(b, 1);
	while (bmpi++) {
			c->add(bmpi.getMonitorPoint());
	}

	return c;
}

static void
dataHelper(MCVector &vec, const MonitorContainer &common, const MonitorContainer &bima, ZebraVisitorPtr visitor)
{
	MonitorContainer *c = appendContainer(common, bima);
	vec.push_back(c);
	visitor->addMonitorContainer(c);
}

int carma::util::Program::main()
{
	const std::string title = "BIMA Antenna Drives";
	const std::string helpTitle = "BIMA Antenna Drives";

	// Create a dislay
	MonitorDisplay display(title);
	display.setSpecificHelp(helpTitle, makeHelp());

	// initialize tabs (names are in display order!)
	std::vector<std::string> tabNames;
	tabNames.push_back("Summary");
	tabNames.push_back("Tracking");
	tabNames.push_back("Pointing");
	tabNames.push_back("Constants");
	tabNames.push_back("Limits");

	TabDataType tabData;
	BOOST_FOREACH(const std::string &tabName, tabNames)
		tabData[tabName] = MCVector();

	// for each BIMA antenna, add the data to each vector
	ZebraVisitorPtr zebra(new ZebraVisitor());
	const std::vector<std::string> colLabels = RtTable::getBimaAntNames();
	for (unsigned int i = 0; i < colLabels.size(); i++) {
		BimaSubsystem &bima = display.cms().bima(i);
		AntennaCommon::Drive &commonDrive = bima.antennaCommon().drive();
		BimaSubsystem::Drive &specDrive = bima.bimaSpecific().drive();

		dataHelper(tabData["Summary"], commonDrive, specDrive, zebra);
		dataHelper(tabData["Tracking"], commonDrive.track(), specDrive.track(), zebra);
		dataHelper(tabData["Pointing"], commonDrive.point(), specDrive.point(), zebra);
		dataHelper(tabData["Constants"], commonDrive.point().constants(), specDrive.point().constants(), zebra);
		dataHelper(tabData["Limits"], commonDrive.limit(), specDrive.limit(), zebra);
	}

	// create a tab for each container, add it to the display
	BOOST_FOREACH(const std::string &tabName, tabNames) {
		const std::vector<MonitorContainer *> &tabContainers = tabData[tabName];

		MonitorSingleTableFolderPtr folder(new MonitorSingleTableFolder(tabName,
				colLabels, tabContainers, 0, 1, 0, true, zebra));

		display.add(folder);
	}

	while (display.serveData()) {
		/* none */
	}

	return 0;
}
