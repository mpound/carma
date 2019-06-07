/*
 * rtdbimadewarreg.cc
 *
 * Gets and displays data about the BIMA Dewar Regulation
 *
 * @author Colby Gutierrez-Kraybill
 * $Id: rtdbimadewarreg.cc,v 1.4 2013/11/19 03:41:14 iws Exp $
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
	return "BIMA Antenna Dewar Temp Regulation Help\n"
		   "\n"
		   "BIMA Antenna Dewar Temp Regulation of mm mixers\n";
}

int carma::util::Program::main()
{
	const std::string title = "BIMA Antenna Dewar Temp Regulation";
	const std::string helpTitle = "BIMA Antenna Dewar Temp Regulation";
	ZebraVisitorPtr zebra(new ZebraVisitor());

	// Create a dislay
	MonitorDisplay display(title);
	display.setSpecificHelp(helpTitle, makeHelp());

	std::vector<std::string> colLabels = RtTable::getBimaAntNames();
	std::vector<MonitorContainer*> dewarReg;

	for (unsigned int i = 0; i < colLabels.size(); i++) {
		BimaSubsystem &bima = display.cms().bima(i);

		dewarReg.push_back(&bima.bimaSpecific().dewar().regulation());
		zebra->addMonitorContainer(bima.bimaSpecific().dewar().regulation());
	}

	MonitorSingleTableFolderPtr mstf(new MonitorSingleTableFolder(
				"Dewar Regulation", colLabels, dewarReg, 0, 1, 0, true, zebra));

	display.add(mstf);

	while (display.serveData()) {
		/* none */
	}

	return 0;
}
