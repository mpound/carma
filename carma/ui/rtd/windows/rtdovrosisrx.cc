/*
 * @file
 *
 * Retrieves data from the Ovro subsystem and displays.
 *
 * @author Andy Beard (based on code from Steve Scott)
 * $Id: rtdovrosisrx.cc,v 1.12 2013/11/19 03:41:14 iws Exp $
 *
 * $CarmaCopyright$
 */


#include <sstream>
#include <vector>

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/services/Global.h"
#include "carma/util/Program.h"

using namespace std;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::util;
using namespace carma::ui::rtd;

namespace {

std::string makeHelp()
{
    ostringstream oss;
    oss << "       OVRO SIS RECEIVER MODULE HELP\n\n"
        << "Status of the OVRO SIS RECEIVER CAN module. "
        << "Fill in more info here... "
        ;
    return oss.str();
}


MonitorContainer *
buildRx1CmContainer( const unsigned int i, MonitorDisplay & display )
{
    // The 1cm receiver display requires special treatment in order to display
    // the stage temperatures in the same rows as the other antennas.
    MonitorContainer * rx1cm = new MonitorContainer( "rx1cm" );

    // Iterate over rxbias until we reach the board temperature - when
    // we do, add the temperature block afterwards.
    MonitorPointIterator mpi( display.cms().ovro(i).rxBias(), 1 );
    while ( mpi++ ) { 
        carma::monitor::MonitorPoint & mp = mpi.getMonitorPoint();
        rx1cm->add( mp );
        if ( mp.getName() == "boardTemperature" ) {
            rx1cm->add( display.cms().ovro(i).rxBiasTemps() );
        }
    }

    return rx1cm;

} // buildRx1CmContainer

} // namespace unnamed

int
Program::main()
{
    // Create a dislay
    MonitorDisplay display("OVRO SIS Receiver Module Status");
    display.setSpecificHelp("OVRO SIS Receiver Module Help", makeHelp());

    const int width = AUTO_SIZE;
    vector<string> columnLabel = RtTable::getOvroAntNames();
    vector<MonitorContainer*> rx1mmLeftPolContainer;
    vector<MonitorContainer*> rx1mmRightPolContainer;
    vector<MonitorContainer*> rx3mmContainer;
    vector<MonitorContainer*> rx1cmContainer;
    vector<MonitorContainer*> rx1mmLeftPolXacContainer;
    vector<MonitorContainer*> rx1mmRightPolXacContainer;
    vector<MonitorContainer*> rx3mmXacContainer;
    vector<MonitorContainer*> rx1cmXacContainer;
    MonitorContainer * rx1mm;
    MonitorContainer * rx3mm;
    MonitorContainer * xac1mm;
    MonitorContainer * xac3mm;
    MonitorContainer * xac1cm;

    for (unsigned int i = 0; i < Global::nOvroAntennas(); ++i) {
        // All deliberately one time leaked.
        rx1mm = new MonitorContainer( "rx1mmLeft" );
        rx1mm->add( display.cms().ovro(i).rx1mm( 0 ).state());
        rx1mm->add( display.cms().ovro(i).rx1mm( 0 ).sisReceiver());
        rx1mmLeftPolContainer.push_back( rx1mm );

        rx1mm = new MonitorContainer( "rx1mmRight" );
        rx1mm->add( display.cms().ovro(i).rx1mm( 1 ).state());
        rx1mm->add( display.cms().ovro(i).rx1mm( 1 ).sisReceiver());
        rx1mmRightPolContainer.push_back( rx1mm );

        rx3mm = new MonitorContainer( "rx3mm" );
        rx3mm->add( display.cms().ovro(i).rx3mm().state());
        rx3mm->add( display.cms().ovro(i).rx3mm().sisReceiver());
        rx3mmContainer.push_back( rx3mm );

        rx1cmContainer.push_back( buildRx1CmContainer( i, display ) );

        xac1mm = new MonitorContainer("rx1mmLeftXac");
        xac1mm->add(display.cms().ovro(i).rx1mm( 0 ).state());
        xac1mm->add(display.cms().ovro(i).rx1mm( 0 ).xac());
        rx1mmLeftPolXacContainer.push_back(xac1mm);

        xac1mm = new MonitorContainer("rx1mmRightXac");
        xac1mm->add(display.cms().ovro(i).rx1mm( 1 ).state());
        xac1mm->add(display.cms().ovro(i).rx1mm( 1 ).xac());
        rx1mmRightPolXacContainer.push_back(xac1mm);

        xac3mm = new MonitorContainer("rx3mmLeftXac");
        xac3mm->add(display.cms().ovro(i).rx3mm().state());
        xac3mm->add(display.cms().ovro(i).rx3mm().xac());
        rx3mmXacContainer.push_back(xac3mm);

        xac1cm = new MonitorContainer("rx1cmXac");
        xac1cm->add(display.cms().ovro(i).rxBias().state());
        xac1cm->add(display.cms().ovro(i).rxBias().xac());
        rx1cmXacContainer.push_back(xac1cm);
    }

    MonitorSingleTableFolderPtr rx1mmLeftFolder(new MonitorSingleTableFolder(
                "sis rx (1mm - left pol)", columnLabel, rx1mmLeftPolContainer, width, 2 ));
    MonitorSingleTableFolderPtr rx1mmRightFolder(new MonitorSingleTableFolder(
                "sis rx (1mm - right pol)", columnLabel, rx1mmRightPolContainer, width, 2));
    MonitorSingleTableFolderPtr rx3mmFolder(new MonitorSingleTableFolder(
                "sis rx (3mm)", columnLabel, rx3mmContainer, width, 2 ));
    MonitorSingleTableFolderPtr rx1cmFolder(new MonitorSingleTableFolder(
                "rx (1cm)", columnLabel, rx1cmContainer, width, 2 ));
    MonitorSingleTableFolderPtr xac1mmLeftFolder(new MonitorSingleTableFolder(
                "xac (1mm - left pol)", columnLabel, rx1mmLeftPolXacContainer, width, 2 ));
    MonitorSingleTableFolderPtr xac1mmRightFolder(new MonitorSingleTableFolder(
                "xac (1mm - right pol)", columnLabel, rx1mmRightPolXacContainer, width, 2 ));
    MonitorSingleTableFolderPtr xac3mmFolder(new MonitorSingleTableFolder(
                "xac (3mm)", columnLabel, rx3mmXacContainer, width, 2 ));
    MonitorSingleTableFolderPtr xac1cmFolder(new MonitorSingleTableFolder(
                "xac (1cm)", columnLabel, rx1cmXacContainer, width, 2 ));

    display.add(rx3mmFolder);
    display.add(rx1mmLeftFolder);
    display.add(rx1mmRightFolder);
    display.add(rx1cmFolder);

    display.add(xac3mmFolder);
    display.add(xac1mmLeftFolder);
    display.add(xac1mmRightFolder);
    display.add(xac1cmFolder);

    while ( display.serveData( ) ); // Serve data forever

    return 0;
}
