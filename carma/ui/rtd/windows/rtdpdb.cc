/*
 * Displays Project Database Information
 */

#include <sstream>
#include <string>
#include <vector>

#include <carma/ui/rtd/common/MonitorDisplay.h>
#include <carma/ui/rtd/common/MonitorSingleTableFolder.h>
using namespace carma::ui::rtd;

#include <carma/monitor/ProjectDatabaseManagerSubsystem.h>
using namespace carma::monitor;

#include <carma/util/Program.h>
using namespace carma::util;

void setupAndRunDisplay(MonitorDisplay &display)
{
    ProjectDatabaseManagerSubsystem &subsys = display.cms().projectDatabaseManager();

    MonitorSingleTableFolderPtr generalTab(new MonitorSingleTableFolder("General", "", subsys));

    std::vector<MonitorContainer *> serverContainer;
    std::vector<std::string> serverHeading;

    for (int i = 0; i < subsys.serverCount(); i++) {
        std::ostringstream oss;
        oss << "Server " << i + 1;

        serverHeading.push_back(oss.str());
        serverContainer.push_back(&subsys.server(i));
    }

    MonitorSingleTableFolderPtr serverTab(new MonitorSingleTableFolder("Server Info", serverHeading, serverContainer));

    display.add(generalTab);
    display.add(serverTab);

    /* loop forever serving data to client */
    while (display.serveData()) {
        /* none */
    }
}

int Program::main()
{
    /* create a display */
    MonitorDisplay display("Project Database Information");

    /* add the help text */
    const std::string hfile = Program::getConfFile("rtd/help/pdb.html");
    display.setSpecificHelpFromTextFile(display.getTitle() + " Help", hfile);

    setupAndRunDisplay(display);
    return 0;
}

/* vim: set ts=4 sts=4 sw=4 tw=112 et: */
