/*
 * @file
 *
 * Retrieves data from the Ovro subsystem and displays.
 *
 * @author Original: Chul Gwon (based on code from Steve Scott)
 * $Id: rtdovrotemperature.cc,v 1.7 2013/11/19 03:41:14 iws Exp $
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
    oss
    << "       Rx Temperature Control HELP\n\n"
    << "The 10-m Electronics Temperature Controller CAN module controls"
    << " and monitors the temperature of the electronics modules in the"
    << " thermal enclosure on the receiver plate.  There are two control"
    << " loops, one for each end of the component mounting plate.  Loop 1"
    << " is at the left end of the plate and loop 2 is at the right end when"
    << " viewing the receiver plate with the antenna at zenith.  "
    << " Temperatures are sensed using DS18B20 1-wire thermometers and "
    << " the temperature is controlled by applying current to power "
    << " resistors on the plate.  The currents are generated with a pulse"
    << " width modulated (PWM) H-bridge for high efficiency.";
    return oss.str();
}

int
Program::main()
{
    // Create a dislay
    MonitorDisplay display("10m Rx Thermal Control");
    display.setSpecificHelp("10m Rx Thermal Control Help", makeHelp());

    int width = AUTO_SIZE;
    vector<string> columnLabel = RtTable::getOvroAntNames();
    vector<MonitorContainer*> thermals;
    vector<MonitorContainer*> xacs;
    MonitorContainer * xac;
    for (unsigned int i = 0; i < Global::nOvroAntennas(); ++i) {
        thermals.push_back( &display.cms().ovro(i).rxThermalControl() );
        xac = new MonitorContainer("dumb"); // Dummy for state and xac info.
        xac->add( display.cms().ovro(i).rxThermalControl().state() );
        xac->add( display.cms().ovro(i).rxThermalControl().xac() );
        xacs.push_back(xac);
    }

    MonitorSingleTableFolderPtr folder1(new MonitorSingleTableFolder(
                "rxthermal", columnLabel, thermals, width, 1));
    MonitorSingleTableFolderPtr folder2(new MonitorSingleTableFolder(
                "rxthermal (xac)", columnLabel, xacs, width, 2));

    // Add the folders to the display
    display.add(folder1);
    display.add(folder2);

    // Loop forever serving data to the client
    while (display.serveData()) {
    }

    return 0;
}
