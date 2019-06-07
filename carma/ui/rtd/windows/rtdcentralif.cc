/*
 * rtdcentralif.cc
 *
 * RTD window for CentralIf subsystem
 *
 * @author: Ira W. Snyder
 */

#include <sstream>
#include <vector>

#include <carma/ui/rtd/common/MonitorDisplay.h>
#include <carma/ui/rtd/common/MonitorSingleTableFolder.h>
#include <carma/ui/rtd/common/MonitorCell.h>
#include <carma/ui/rtd/common/ZebraVisitor.h>
#include <carma/monitor/MonitorPointIterator.h>
#include <carma/monitor/CentralIfSubsystem.h>
#include <carma/util/Program.h>

using namespace std;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui::rtd;

static std::string makeHelp()
{
    std::ostringstream oss;
    oss << "CentralIf Subsystem Help\n"
        << "\n"
        << "The CentralIf Subsystem monitors the optical power on the media\n"
        << "converters from fiber to coaxial cable.";
    return oss.str();
}

static std::vector<std::string> createLabels(const int first, const int last)
{
    std::vector<std::string> ret;
    for (int i = first; i <= last; i++) {
        std::ostringstream oss;
        oss << "IF" << i;
        ret.push_back(oss.str());
    }

    return ret;
}

static std::vector<MonitorContainer *>
createContainers(const int first, const int last,
                 MonitorDisplay &display, ZebraVisitorPtr visitor)
{
    std::vector<MonitorContainer *> ret;
    for (int i = first; i <= last; i++) {
        const int index = i - 1;
        CentralIfSubsystem &centralif = display.cms().centralIf();
        MonitorContainer *c = new MonitorContainer("Antenna");

        centralif.antenna(index).pol(0).opticalPower().setShortName("opticalPower Pol1");
        centralif.antenna(index).pol(1).opticalPower().setShortName("opticalPower Pol2");

        c->add(centralif.antenna(index).pol(0).opticalPower());
        c->add(centralif.antenna(index).pol(1).opticalPower());

        visitor->addMonitorContainer(*c);
        ret.push_back(c);
    }

    return ret;
}

int carma::util::Program::main()
{
    // Create a display
    const std::string subsystemName("CentralIf Subsystem");
    MonitorDisplay display(subsystemName);
    display.setSpecificHelp(subsystemName, makeHelp());

    ZebraVisitorPtr visitor(new ZebraVisitor());

    // IF 1-15
    {
        std::vector<std::string> labels = createLabels(1, 15);
        std::vector<MonitorContainer *> containers = createContainers(1, 15, display, visitor);
        display.add(MonitorSingleTableFolderPtr(new MonitorSingleTableFolder("IF 1-15", labels, containers, 0, 1, 0, true, visitor)));
    }

    // IF 16-24
    {
        std::vector<std::string> labels = createLabels(16, 24);
        std::vector<MonitorContainer *> containers = createContainers(16, 24, display, visitor);
        display.add(MonitorSingleTableFolderPtr(new MonitorSingleTableFolder("IF 16-24", labels, containers, 0, 1, 0, true, visitor)));
    }

    while (display.serveData()) {
        /* nothing */
    }

    return 0;
}

/* vim: set ts=4 sts=4 sw=4 et: */
