
/*
 * @file
 *
 * Gets data from the carma test subsystem and displays.
 *
 * @author Original: Steve Scott
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
    oss << "       10M SECONDARY MIRROR HELP\n\n"
        << "Status of the 10M antenna secondary mirror CAN module. "
        << "Fill in more info here... "
        ;
    return oss.str();
}

int
Program::main()
{

    // Create a dislay
    MonitorDisplay display("10m Secondary Mirror Status");
    display.setSpecificHelp("10m Secondary Mirror Help", makeHelp());

    int width = AUTO_SIZE;
    vector<string> columnLabel = RtTable::getOvroAntNames();
    vector<MonitorContainer*> secondarys;
    vector<MonitorContainer*> xacs;
    MonitorContainer* xac;
    for (unsigned int i = 0; i < Global::nOvroAntennas(); ++i) {
        secondarys.push_back(&display.cms().ovro(i).secondary());
        xac = new MonitorContainer("dumb"); // Dummy for state and xac info.
        xac->add( display.cms().ovro(i).secondary().state() );
        xac->add( display.cms().ovro(i).secondary().xac() );
        xacs.push_back(xac);
    }

    MonitorSingleTableFolderPtr folder1(new MonitorSingleTableFolder(
                "secondary", columnLabel, secondarys, width, 1));
    MonitorSingleTableFolderPtr folder2(new MonitorSingleTableFolder(
                "secondary (xac)", columnLabel, xacs, width, 2));

    // Add the folders to the display
    display.add(folder1);
    display.add(folder2);

    // Loop forever serving data to the client
    while (display.serveData()) {
    }

    return 0;
}
