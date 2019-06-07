/*
 * @file
 *
 * Displays SubarrayControl WorkerPool Statistics
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

#include <carma/monitor/ControlSubsystem.h>
#include <carma/monitor/ControlSubsystemExt.h>
#include <carma/monitor/MonitorPointIterator.h>
using namespace carma::monitor;

#include <carma/util/Program.h>
#include <carma/util/IllegalArgumentException.h>
using namespace carma::util;

#include <boost/shared_ptr.hpp>

void setupAndRunDisplay(MonitorDisplay &display)
{
    const int numSubarrays = ControlSubsystemBase::subarrayCount();

    for (int i = 0; i < numSubarrays; i++) {
        ControlSubsystemBase::ThreadStats &ts = display.cms().control().subarray(i).threadStats();

        std::ostringstream oss;
        oss << "Subarray " << i + 1;

        MonitorSingleTableFolderPtr p(new MonitorSingleTableFolder(oss.str(), "", ts));
        display.add(p);
    }

    /* loop forever serving data to client */
    while (display.serveData())
        /* none */;
}

int Program::main()
{
    const std::string string1Param = getStringParameter( "string1" );

    /* create a display */
    MonitorDisplay display("SubarrayControl WorkerPool Statistics");

    /* add the help text */
    const std::string hfile = Program::getConfFile("rtd/help/workerpool.html");
    display.setSpecificHelpFromTextFile(display.getTitle() + " Help", hfile);

    setupAndRunDisplay(display);
    return 0;
}

/* vim: set ts=4 sts=4 sw=4 tw=112 et: */
