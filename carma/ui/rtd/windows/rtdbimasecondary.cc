/*
 * rtdbimasecondary.cc
 *
 * Gets and displays data about the BIMA Secondary
 *
 * @author Colby Gutierrez-Kraybill
 * $Id: rtdbimasecondary.cc,v 1.5 2013/11/19 03:41:14 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <vector>

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
	return "BIMA Antenna Secondary Help\n"
		   "\n"
		   "BIMA Antenna Secondary of mm mixers\n";
}

int carma::util::Program::main()
{
	const std::string title = "BIMA Antenna Secondary";
	const std::string helpTitle = "BIMA Antenna Secondary";
	ZebraVisitorPtr zebraVisitor(new ZebraVisitor());

	// Create a dislay
	MonitorDisplay display(title);
	display.setSpecificHelp(helpTitle, makeHelp());

	std::vector<std::string> colLabels = RtTable::getBimaAntNames();
	std::vector<MonitorContainer*> secondary;

	for (unsigned int i = 0; i < colLabels.size(); i++) {
		BimaSubsystem &bima = display.cms().bima(i);

		secondary.push_back(&bima.antennaCommon().secondary());
		zebraVisitor->addMonitorContainer(bima.antennaCommon().secondary());
	}

	MonitorSingleTableFolderPtr mstf(new MonitorSingleTableFolder(
				"Secondary", colLabels, secondary, 0, 1, 0, true, zebraVisitor));

	display.add( mstf );

	while (display.serveData()) {
		/* none */
	}

	return 0;
}
