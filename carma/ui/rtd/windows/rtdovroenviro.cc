/*
 * @file
 *
 * Gets data from the carma test subsystem and displays.
 *
 * @author Original: Andy Beard (based on code from Steve Scott)
 * $id: $
 *
 * $CarmaCopyright$
 */


#include <sstream>
#include <vector>

#include "carma/monitor/OvroSubsystem.h"
#include "carma/services/Global.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/util/Program.h"

using namespace std;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::util;
using namespace carma::ui::rtd;

static std::string makeHelp()
{
    ostringstream oss;
    oss << "       10M ENVIRONMENTAL MONITOR HELP\n\n"
        << "Status of the 10m Sidecab Environmental Monitor CAN module. "
        << "Fill in more info here... "
        ;
    return oss.str();
}

int
Program::main() {

    // Create a dislay
    MonitorDisplay display("10m Antenna Environmental Monitor Status");
    display.setSpecificHelp("10m Environmental Monitor Help", makeHelp());

    vector<string> columnLabel = RtTable::getOvroAntNames();
    vector<MonitorContainer*> enviromons; // Environmental monitors
    vector<MonitorContainer*> xacs;
    MonitorContainer * xac;
    for (unsigned int i = 0; i < Global::nOvroAntennas(); ++i) {
        enviromons.push_back(&display.cms().ovro(i).environmentalMonitor());
        xac = new MonitorContainer("dumb"); // Dummy to hold state and xac info.
        xac->add( display.cms().ovro(i).environmentalMonitor().state() );
        xac->add( display.cms().ovro(i).environmentalMonitor().xac() );
        xacs.push_back(xac);
    }

    MonitorSingleTableFolderPtr folder1(new MonitorSingleTableFolder(
                "environment", columnLabel, enviromons, AUTO_SIZE, 1));
    MonitorSingleTableFolderPtr folder2(new MonitorSingleTableFolder(
                "environment (xac)", columnLabel, xacs, AUTO_SIZE, 2));

    // Add the folders to the display
    display.add(folder1);
    display.add(folder2);

    // Loop forever serving data to the client
    while (display.serveData()) {
    }

    return 0;
}
