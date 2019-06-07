/*
 * @file
 *
 * Displays Fault System Timing and Diagnostic Data
 *
 * $id: $
 *
 * $CarmaCopyright$
 */


#include <sstream>
#include <vector>

#include <carma/ui/rtd/common/MonitorDisplay.h>
#include <carma/ui/rtd/common/MonitorSingleTableFolder.h>
using namespace carma::ui::rtd;

#include <carma/monitor/FaultSubsystem.h>
#include <carma/monitor/MonitorPointIterator.h>
using namespace carma::monitor;

#include <carma/util/Program.h>
#include <carma/util/IllegalArgumentException.h>
using namespace carma::util;

typedef std::vector<MonitorContainer *> MonitorContainerVector;
typedef std::vector<std::string> StringVector;

void setupAndRunDisplay(MonitorDisplay &display)
{
    FaultSubsystem &fault = display.cms().fault();
    const int count = fault.subarrayCount();

    /* create the all tab */
    MonitorSingleTableFolderPtr allTab(new MonitorSingleTableFolder("General", "", fault));

    MonitorSingleTableFolderPtr alarmTab(new MonitorSingleTableFolder("Alarm", "", fault.alarm()));
    MonitorSingleTableFolderPtr blankTab(new MonitorSingleTableFolder("Blank/Flag", "", fault.blankFlag()));

    /* create the subarrays tab */
    MonitorContainerVector subarrayContainer;
    StringVector subarrayHeading;

    for (int i = 0; i < count; i++) {
        std::ostringstream oss;
        oss << "SA" << i + 1;

        subarrayHeading.push_back(oss.str());
        subarrayContainer.push_back(&fault.subarray(i));
    }

    MonitorSingleTableFolderPtr subarrayTab(new MonitorSingleTableFolder("Transients", subarrayHeading, subarrayContainer));

    /* add the tabs to the display */
    display.add(allTab);
    display.add(alarmTab);
    display.add(blankTab);
    display.add(subarrayTab);

    /* loop forever serving data to client */
    while (display.serveData())
        /* none */;
}

static std::string makeHelpText()
{
    std::ostringstream oss;

    oss << "        Fault System Status\n\n"
        << "Status of the Fault System, mostly containing "
        << " debugging and timing information.";

    return oss.str();
}

int Program::main()
{
    const std::string string1Param = getStringParameter( "string1" );

    /* create a display */
    MonitorDisplay display("Fault System Stats");

    /* add the help text */
    display.setSpecificHelp("Fault System Stats Help", makeHelpText());

    setupAndRunDisplay(display);
    return 0;
}

/* vim: set ts=4 sts=4 sw=4 tw=112 et: */
