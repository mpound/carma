/*
 * rtdbimastatus.cc
 *
 * Gets and displays data about the BIMA status control bits
 *
 * @author Colby Gutierrez-Kraybill
 * $Id: rtdbimastatus.cc,v 1.5 2013/11/19 03:41:14 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <vector>

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/ZebraVisitor.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
using namespace carma::ui::rtd;

#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/BimaSubsystem.h"
using namespace carma::monitor;

#include "carma/util/Program.h"

static std::string makeHelp()
{
	return "BIMA Antenna Control Status Bits Help\n"
		   "\n"
		   "BIMA Antenna Control Status Bits in the control box at the base\n";
}

int carma::util::Program::main()
{
	const std::string title = "BIMA Antenna Control Status Bits";
	const std::string helpTitle = "BIMA Antenna Control Status Bits";
	ZebraVisitorPtr zebraVisitor(new ZebraVisitor());

	// Create a dislay
	MonitorDisplay display(title);
	display.setSpecificHelp(helpTitle, makeHelp());

	RtFolder folder("BIMA Control Status Bits");

	std::vector<std::string> colLabels = RtTable::getBimaAntNames();
	std::vector<MonitorContainer *> statusBits;

	for (unsigned int i = 0; i < colLabels.size(); i++) {
		BimaSubsystem &bima = display.cms().bima(i);

		statusBits.push_back(&bima.bimaSpecific().statusBits());
		zebraVisitor->addMonitorContainer(bima.bimaSpecific().statusBits());
	}


	MonitorSingleTableFolderPtr mstf(new MonitorSingleTableFolder(
				"Status bits", colLabels, statusBits, 0, 1, 0, true, zebraVisitor));

	display.add(mstf);

	while (display.serveData()) {
		/* none */
	}

	return 0;
}
