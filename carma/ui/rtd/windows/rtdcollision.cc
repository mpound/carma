/** @file
 * Gets data from the CARMA collisionable antennas page
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.3 $
 * $Date: 2013/11/19 03:41:14 $
 * $Id: rtdcollision.cc,v 1.3 2013/11/19 03:41:14 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <vector>

#include "carma/monitor/BimaSubsystem.h"
using namespace carma::monitor;

#include "carma/services/Global.h"
using namespace carma::services;

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
using namespace carma::ui::rtd;

#include "carma/util/Program.h"

static std::string makeHelp()
{
	std::ostringstream oss;

	oss << "10M ANTENNA TILTMETER HELP\n\n"
		<< "Status of the 10m antenna tiltmeter CAN module. "
		<< "Fill in more info here... ";
	return oss.str();
}

int carma::util::Program::main()
{
	// Create a dislay
	MonitorDisplay display("Collisionable Antenna Status");
	display.setSpecificHelp("Collisionable Status Help", makeHelp());

	const int width = AUTO_SIZE;
	std::vector<std::string> columnLabel = RtTable::getBimaAntNames();
	std::vector<MonitorContainer *> ants;

	for (unsigned int i = 0; i < Global::nBimaAntennas(); ++i) {
		MonitorContainer *info = new MonitorContainer("dumb"); // Dummy to hold state
		BimaSubsystem &bima = display.cms().bima(i);

		info->add(bima.antennaCommon().drive().safeState());
		info->add(bima.antennaCommon().drive().safeAzLow());
		info->add(bima.antennaCommon().drive().safeAzHigh());
		info->add(bima.antennaCommon().drive().safeElLow());
		info->add(bima.antennaCommon().drive().safeElHigh());

		info->add(bima.antennaCommon().drive().track().actualAzimuth());
		info->add(bima.antennaCommon().drive().track().actualElevation());

		info->add(bima.bimaSpecific().statusBits().collision());
		info->add(bima.bimaSpecific().statusBits().collisionOff());

		ants.push_back(info);
	}

	MonitorSingleTableFolderPtr folder1(new MonitorSingleTableFolder(
				"CollisionInfo", columnLabel, ants, width, 1));

	// Add the folders to the display
	display.add(folder1);

	// Loop forever serving data to the client
	while (display.serveData()) {
		/* none */
	}

	return 0;
}
