// $Id: rtdmonitorstats.cc,v 1.10 2014/07/31 18:28:04 iws Exp $

/*
 * @file
 *
 * Realtime display window for monitor system statistics.
 *
 * @author: Andy Beard
 * $Id: rtdmonitorstats.cc,v 1.10 2014/07/31 18:28:04 iws Exp $
 *
 * $CarmaCopyright$
 */

#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/MonitorStats.h"
#include "carma/monitor/SystemStatus.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorTable.h"
#include "carma/util/Program.h"

#include <memory>

using namespace std;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui::rtd;

int
Program::main()
{
    MonitorDisplay display( "Monitor System Transport" );

    const std::string hfile = Program::getConfFile("rtd/help/monitorstats.html");
    display.setSpecificHelpFromTextFile(display.getTitle() + " Help", hfile);

    const MonitorSystem& ms = display.cms();

    vector< string > statsHeaders;
    vector< MonitorContainer * > statsContainers;

    for ( int ss = 0; ss < ms.getActualNumSubsystems(); ++ss ) {

        try {
            MonitorSubsystem & mss = ms.getChildSubsystem( ss );
            MonitorComponent * mc = mss.getComponentPtr(
                "MonitorSubsystemStats",
                false );
            MonitorContainer * stats = dynamic_cast< MonitorContainer * >( mc );
            if ( stats != 0 ) {
                statsHeaders.push_back( mss.getName() );
                statsContainers.push_back( stats );
            }

        } catch (...) {
            // Most likely this subsystem doesn't have MonitorStats - stifle
        }
    }

    RtFolderPtr fcFolder(new RtFolder( "Summary" ));
    RtFolderPtr ssFolder(new RtFolder( "Subsystems" ));

    MonitorTablePtr ssTablePtr(
        MonitorTable::makeTable( statsHeaders,
                                 statsContainers,
                                 AUTO_SIZE,
                                 1,     // hierarchy depth
                                 0,     // sample number
                                 false,  // units in rows and labels
                                 true ) // switch rows and columns
                                     );

    MonitorTablePtr fcTablePtr(
        MonitorTable::makeTable( "Monitor System",
                                 ms.systemStatus( ).monitorSystemStats( ),
                                 AUTO_SIZE,
                                 1,     // hierarchy depth
                                 0,     // sample number
                                 true,  // units in rows and labels
                                 false ) // switch rows and columns
                                 );

    RtSpacerPtr spacer3pixels(new RtSpacer(3));
    RtSpringPtr spring(new RtSpring(10.0));

    fcFolder->add(spacer3pixels);
    fcFolder->add(fcTablePtr);
    fcFolder->add(spring);
    ssFolder->add(spacer3pixels);
    ssFolder->add(ssTablePtr);

    display.add(fcFolder);
    display.add(ssFolder);

    while ( display.serveData( ) );

    return 0;
}
