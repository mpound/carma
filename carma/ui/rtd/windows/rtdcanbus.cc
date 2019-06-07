/**
 * @file
 *
 * RTD window for Ovro antenna CANbusses.
 *
 * @author Original: Andrew Beard
 * $Id: rtdcanbus.cc,v 1.8 2013/11/19 03:41:14 iws Exp $
 *
 * $CarmaCopyright$
 */

// STL includes
#include <sstream>
#include <vector>

// Carma includes
#include "carma/monitor/WbdcSubsystem.h"
#include "carma/monitor/SldcSubsystem.h"
#include "carma/monitor/LoberotatorSubsystem.h"
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

namespace { // Anonymous

string title           = " CAN Bus Status";
string helpTitle       = " CAN Bus Info";
string helpPageHeading = " CANBUS HELP\n\n";
string helpSummary     = "Status of the canbus subsystem.";

typedef vector< vector<MonitorContainer*> > BusContainerType;

enum SubsystemType {
    WBDC, // The wideband downconverter
    SLDC, // The spectral line downconverter
    LR,   // Loberotator
    OVRO  // Ovro subsystem
    // INSERT ADDITIONAL SUBSYSTEMS HERE
};

} // End anonymous namespace

static std::string makeHelp()
{
    ostringstream oss;
    oss << helpPageHeading
        << helpSummary
        << "Fill in more info here... "
        ;
    return oss.str();
}

int
Program::main()
{
    enum SubsystemType subsys;
    string subsysName;

    if (getStringParameter("string1").find("wbdc") != string::npos ) {
        subsys = WBDC;
        subsysName = "Wbdc";
    } else if (getStringParameter("string1").find("sldc") != string::npos) {
        subsys = SLDC;
        subsysName = "Sldc";
    } else if (getStringParameter("string1").find("loberotator") !=
                string::npos) {
        subsys = LR;
        subsysName = "Loberotator";
    } else if (getStringParameter("string1").find("ovro") != string::npos) {
        subsys = OVRO;
        subsysName = "10 Meter Antenna ";
    } else {
        return EXIT_FAILURE;
    }

    // Create a dislay
    MonitorDisplay display( (subsysName + title) );
    display.setSpecificHelp(helpTitle, makeHelp());

    int width = AUTO_SIZE; // Tell MonitorSingleTableFolder to autoresize.
    vector<string> columnLabel;
    vector<MonitorContainer *> hostContainer;
    BusContainerType::size_type nBusses;

    // Depending on which subsystem this display is for, add the host data
    // to the host container and determine the number of busses.
    switch (subsys) {
        case WBDC:
            hostContainer.push_back(&display.cms().wbdc().can().host());
            columnLabel.push_back("Wbdc");
            nBusses = WbdcSubsystem::Can::busCount();
            break;
        case SLDC:
            hostContainer.push_back(&display.cms().sldc().can().host());
            columnLabel.push_back("Sldc");
            nBusses = SldcSubsystem::Can::busCount();
            break;
        case LR:
            hostContainer.push_back(&display.cms().loberotator().can().host());
            columnLabel.push_back("Loberotator");
            nBusses = 1;
            break;
        case OVRO:
            for (unsigned i = 0; i < Global::nOvroAntennas(); ++i) {
                hostContainer.push_back(&display.cms().ovro(i).can().host());
            }
            columnLabel = RtTable::getOvroAntNames();
            nBusses = OvroSubsystem::Can::busCount();
            break;
        default:
            return EXIT_FAILURE;
    }


    // Now do the same for the bus data - note that there can be differing
    // numbers of busses depending on which subsystem we are interested in.
    BusContainerType busContainers(nBusses);
    BusContainerType::iterator bi = busContainers.begin();

    int busIndex = 0;
    while ( bi != busContainers.end() )
    {
        switch (subsys) {
            case WBDC:
                bi->push_back(&display.cms().wbdc().can().bus(busIndex));
                break;
            case SLDC:
                bi->push_back(&display.cms().sldc().can().bus(busIndex));
                break;
            case LR:
                bi->push_back(&display.cms().loberotator().can().bus());
                break;
            case OVRO:
                for (unsigned i = 0; i < Global::nOvroAntennas(); ++i) {
                    bi->push_back(&display.cms().ovro(i).can().bus(busIndex));
                }
                break;
            default:
                break;
        }
        bi++;
        busIndex++;
    }

    // Create folders with tables
    MonitorSingleTableFolderPtr folder1(new MonitorSingleTableFolder(
                "CAN Master Status", columnLabel, hostContainer, width));

    // Add the first folder to the display
    display.add(folder1);

    // Add the next X folders dynamicaly.
    for ( unsigned int b = 0; b < nBusses; b++ ) {
        ostringstream os;
        os << "Bus " << b << " Health";
        MonitorSingleTableFolderPtr mstf(new MonitorSingleTableFolder(
                    os.str(), columnLabel, busContainers[b], width));
        display.add(mstf);
    }

    // Loop forever serving data to the client
    while (display.serveData()) {
    }

    return EXIT_SUCCESS;
}
