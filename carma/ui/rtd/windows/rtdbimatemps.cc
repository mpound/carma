/*
 * Gets and displays data about misc BIMA temperatures
 *
 * @author Colby Gutierrez-Kraybill
 * $Id: rtdbimatemps.cc,v 1.5 2013/11/19 03:41:14 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <vector>
using namespace std;

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/ZebraVisitor.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
using namespace carma::ui::rtd;

#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/BimaSubsystem.h"
using namespace carma::monitor;

#include "carma/util/Program.h"

static std::string makeHelp()
{
	return "BIMA Antenna Temps Help\n"
		   "\n"
		   "Status of misc BIMA Antenna Temps\n";
}

int carma::util::Program::main()
{
	const std::string title = "BIMA Antenna Temps";
	const std::string helpTitle = "BIMA Antenna Temps";
	ZebraVisitorPtr zebraVisitor(new ZebraVisitor());

	// Create a dislay
	MonitorDisplay display(title);
	display.setSpecificHelp(helpTitle, makeHelp());

	RtFolder folder("BIMA Temps");

	std::vector<std::string> colLabels = RtTable::getBimaAntNames();
	std::vector<MonitorContainer*> temperatures;

	for (unsigned int i = 0; i < colLabels.size(); i++) {
		BimaSubsystem &bima = display.cms().bima(i);

		temperatures.push_back(&bima.bimaSpecific().temperatures());
		zebraVisitor->addMonitorContainer(bima.bimaSpecific().temperatures());
	}

	MonitorSingleTableFolderPtr mstf(new MonitorSingleTableFolder(
				"Tempertures", colLabels, temperatures, 0, 1, 0, true, zebraVisitor));

	display.add(mstf);

	while (display.serveData()) {
		/* none */
	}

	return 0;
}
