/**
 * @file
 *
 * Downconverter display windows.
 * This program creates either the downconverter or the associated XAC window
 * depending on the input paramater.
 *
 * @author Steve Scott
 * @author Andy Beard
 * $Id: rtddownconverter.cc,v 1.11 2013/11/19 03:41:14 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <vector>

#include "carma/monitor/WbdcSubsystem.h"
#include "carma/monitor/SldcSubsystem.h"
#include "carma/services/Global.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/util/Program.h"

using namespace std;
using namespace log4cpp;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::util;
using namespace carma::ui::rtd;

static std::string makeHelp()
{
    ostringstream oss;
    oss << "       DOWNCONVERTER SUBSYSTEM HELP\n\n"
        << "Put the help here... "
        << ""
        ;
    return oss.str();
}

int
Program::main() {

    const bool xac =
        (getStringParameter("string1").find("xac") != string::npos);
    const bool sldc =
        (getStringParameter("string1").find("sldc") != string::npos);
    const int bandsPerBank  = (sldc ? Global::nSpectralLineBands() :
                                      Global::nWidebandBands());
    const int inputsPerBand = (sldc ? Global::nSpectralStations() :
                                      Global::nWidebandStations());

    {
        string logname = getLogname() + (sldc ? ".sldc" : ".wbdc");

        if ( xac )
            logname += ".xac";

        setInstanceLogname( logname );
    }

    // Create a dislay
    ostringstream title;
    title << ( sldc ? "Spectral Line" : "Wideband") << " Downconverter Module";
    title << ( xac ? " XAC" : "") << " Status";
    MonitorDisplay display(title.str());
    display.setSpecificHelp("Downconverter Help", makeHelp());
    CarmaMonitorSystem & cms = display.cms(); // Shorthand

    int width = AUTO_SIZE;  // Tell SingleMonitorTable to autoresize.
    int depth = (xac ? 2 : 1);

    // Folder for each band, each containing a table with all inputs
    for (int b = 0; b < bandsPerBank; b++) {
        ostringstream os;
        os << "Band " << (b + 1);

        vector<string> columnLabel;
        vector<MonitorContainer*> moncon;
        for (int i=0; i < inputsPerBand; i++) {
            ostringstream inputs;
            inputs << "Input " << i+1;
            columnLabel.push_back(inputs.str());
            if (sldc) { // SLDC
                if ( xac ) { // XAC
                    MonitorContainer * xacCont = new MonitorContainer("dumb");
                    xacCont->add( cms.sldc().band(b).input(i).state() );
                    xacCont->add( cms.sldc().band(b).input(i).xac() );
                    moncon.push_back(xacCont);
                } else {
                    moncon.push_back( &cms.sldc().band(b).input(i) );
                }
            } else {   // WBDC
                if ( xac ) {
                    MonitorContainer * xacCont = new MonitorContainer("dumber");
                    xacCont->add( cms.wbdc().band(b).input(i).state() );
                    xacCont->add( cms.wbdc().band(b).input(i).xac() );
                    moncon.push_back(xacCont);
                } else {
                    moncon.push_back( &cms.wbdc().band(b).input(i) );
                }
            }
        } // End loop over inputs

        MonitorSingleTableFolderPtr folder(new MonitorSingleTableFolder(
                os.str(),
                columnLabel,
                moncon,
                width,
                depth ));

        // Add the folder to the display
        display.add(folder);

    } // End loop over bands.

    // Serve the data out....
    while (display.serveData());

    return 0;

} // End Program::main()
