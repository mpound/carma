/*
 * @file
 *
 * Retrieves data from the bima subsystem and displays.
 *
 * @author Dick Plambeck (based on ovro code written by Andy Beard)
 * $Id: rtdbimasisrx.cc,v 1.5 2013/11/19 03:41:14 iws Exp $
 *
 * $CarmaCopyright$
 */


#include <sstream>
#include <vector>

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/BimaSubsystem.h"
#include "carma/services/Global.h"
#include "carma/util/Program.h"

using namespace std;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::util;
using namespace carma::ui::rtd;

static std::string makeHelp()
{
	std::ostringstream oss;
	oss << "       BIMA SIS CAN HELP\n\n"
		<< "Status of the Bima SIS CAN module. "
		<< "This module handles the 1mm RCP receiver "
		;
	return oss.str();
}

int
Program::main()
{
    // Create a dislay
    MonitorDisplay display("BIMA SIS CAN");
    display.setSpecificHelp("BIMA SIS CAN", makeHelp());

    const int width = AUTO_SIZE;
    vector<string> columnLabel = RtTable::getBimaAntNames();
    vector<MonitorContainer*> rx1mmRightPolContainer;
    vector<MonitorContainer*> rx1mmRightPolXacContainer;
    MonitorContainer * rx1mm;
    MonitorContainer * xac1mm;

    for (unsigned int i = 0; i < Global::nBimaAntennas(); ++i) {
        // All deliberately leaked.

        rx1mm = new MonitorContainer( "rx1mmRight" );
        rx1mm->add( display.cms().bima(i).rx1mm().state());
        rx1mm->add( display.cms().bima(i).rx1mm().sisReceiver());
        rx1mmRightPolContainer.push_back( rx1mm );

        xac1mm = new MonitorContainer("rx1mmRightXac");
        xac1mm->add(display.cms().bima(i).rx1mm( ).state());
        xac1mm->add(display.cms().bima(i).rx1mm( ).xac());
        rx1mmRightPolXacContainer.push_back(xac1mm);

    }

    MonitorSingleTableFolderPtr rx1mmRightFolder(new MonitorSingleTableFolder(
                                              "sis rx (1mm - right pol)",
                                              columnLabel,
                                              rx1mmRightPolContainer,
                                              width, 2));

    MonitorSingleTableFolderPtr xac1mmRightFolder(new MonitorSingleTableFolder(
                                                "xac (1mm - right pol)",
                                                columnLabel,
                                                rx1mmRightPolXacContainer,
                                                width, 2 ));

    display.add(rx1mmRightFolder);

    display.add(xac1mmRightFolder);

    while ( display.serveData( ) ); // Serve data forever

    return 0;
}
