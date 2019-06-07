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
    oss << "       10M LO REFERENCE MONITOR HELP\n\n"
        << "Status of the LO Reference Monitor CAN module. "
        << "Fill in more info here... "
        ;
    return oss.str();
}

int
Program::main()
{

    // Create a dislay
    MonitorDisplay display("10m Antenna LO Reference Monitor Status");
    display.setSpecificHelp("10m LO Reference Monitor Help", makeHelp());

    vector<string> columnLabel = RtTable::getOvroAntNames();
    vector<MonitorContainer*> lorefs;
    vector<MonitorContainer*> xacs;
    MonitorContainer * loref;
    MonitorContainer * xac;
    for (unsigned int i = 0; i < Global::nOvroAntennas(); ++i) {
        loref = new MonitorContainer("loref");
        loref->add( display.cms().ovro(i).loReferenceContainer().state() );
        loref->add( display.cms().ovro(i).loReferenceContainer().loReference());
        lorefs.push_back( loref );

        xac = new MonitorContainer("dumb"); // Dummy to hold state and xac info.
        xac->add( display.cms().ovro(i).loReferenceContainer().state() );
        xac->add( display.cms().ovro(i).loReferenceContainer().xac() );
        xacs.push_back(xac);
    }

    MonitorSingleTableFolderPtr folder1(new MonitorSingleTableFolder(
                "loref", columnLabel, lorefs, AUTO_SIZE, 2));
    MonitorSingleTableFolderPtr folder2(new MonitorSingleTableFolder(
                "loref (xac)", columnLabel, xacs, AUTO_SIZE, 2));

    // Add the folders to the display
    display.add(folder1);
    display.add(folder2);

    // Loop forever serving data to the client
    while (display.serveData()) {
    }

    return 0;
}
