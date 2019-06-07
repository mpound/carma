
/*
 * rtdvlbi.cc
 *
 * Gets data from the carma vlbi subsystem and displays.
 *
 * @author Steve Scott
 * $id: $
 *
 * $CarmaCopyright$
 */


#include <sstream>
#include <vector>
#include <cerrno>
#include <cstring>

#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/VlbiSubsystem.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

#include "carma/ui/rtd/common/ZebraVisitor.h"
#include "carma/ui/rtd/common/CompositeMonitorTableVisitor.h"
#include "carma/ui/rtd/common/DisplayTimeOnlyVisitor.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"

using namespace std;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui::rtd;

namespace { // anonymous

const int RELATIVE_LABEL_FONT_SIZE = 4; // 4 points larger than default
const int DEFAULT_COLUMN_WIDTH = 12;

// Better alternative to this is an html help file in conf/rtd/help
string
makeHelp()
{
    ostringstream ost;
    ost
    << "       VLBI SUBSYSTEM HELP\n\n"
    << "The vlbi subsystem is a subsystem used for VLBI. "
    << "The XX monitor point measures blah, blah, blah... "
    ;
    return ost.str();
}

// -------------------
// Make delays folder  - Borrowed from rtdcorrelator.cc
// -------------------
template <typename BandSubsystem>
RtFolderPtr
makeDelaysFolder( int band, BandSubsystem & bs )
{
    ostringstream label;
    label << " Band " << band << " Delays ";

    //RtFolderPtr delaysFolder(new RtFolder(" Delays "));
    RtFolderPtr delaysFolder(new RtFolder( label.str() ));
    RtLabelPtr delaysLabel(new RtLabel( label.str() ));
    MonitorTablePtr delaysTable;

    DisplayTimeOnlyVisitorPtr timeVisitor(new DisplayTimeOnlyVisitor());
    ZebraVisitorPtr zebra(new ZebraVisitor());

    vector< ::std::string > columnNames;
    vector< MonitorContainer * > monitorDelays;

    delaysLabel->setRelFontSize( RELATIVE_LABEL_FONT_SIZE );

    // Form up columnNames and delays vectors.
    for ( int i = 0; i < bs.interpolatorSamplesCount(); ++i ) {

        const string empty("");
        columnNames.push_back( empty );

        CorMonitorInterpSamps & corMonitorInterpSamps =
            bs.interpolatorSamples(i);

        monitorDelays.push_back( &corMonitorInterpSamps );
        // Form up visitor
        for ( int j = 0; j < CorMonitorInterpSamps::inputDelaysCount(); ++j ) {
            timeVisitor->addToEffectedSet(
                        corMonitorInterpSamps.inputDelays(j).timestamp( ) );
        }

        zebra->addMonitorContainer(monitorDelays.back());
    }

    vector< MonitorTableVisitorPtr > visitors;
    visitors.push_back( timeVisitor );
    visitors.push_back( zebra );
    CompositeMonitorTableVisitorPtr compositeVisitor(new CompositeMonitorTableVisitor(visitors));

    delaysTable = MonitorTable::makeTable(
                                columnNames,
                                monitorDelays,
                                DEFAULT_COLUMN_WIDTH,
                                2, 0, true, true, compositeVisitor );

    // Build the folder
    delaysFolder->add(RtSpacerPtr(new RtSpacer( 5, 10, 2 )));
    delaysFolder->add( delaysLabel );
    delaysFolder->add(RtSpacerPtr(new RtSpacer( 3, 5, 0 )));
    delaysFolder->add( delaysTable );
    delaysFolder->add(RtSpacerPtr(new RtSpacer( 5, 10, 6 )));

    return delaysFolder;

} // End makeDelaysFolder.

} // end anonymous namespace

int
Program::main()
{
    // Create a dislay
    MonitorDisplay display("VLBIsubsystem status");
    display.setSpecificHelp("VLBI Help", makeHelp());
    VlbiSubsystem& vlbi = display.cms().vlbi();

    vector<MonitorContainer*> allbands;
    vector<MonitorContainer*> allcontrols;
    vector<string> columnLabels;

    for (int i=0; i< vlbi.bandCount(); i++) {
        ostringstream o;
        o << "Band" << (i+1);
        columnLabels.push_back(o.str());
        VlbiSubsystem::Band& b = vlbi.band(i);
        allbands.push_back(&b);
        allcontrols.push_back(&b.control());
    }

    MonitorSingleTableFolderPtr mt(new MonitorSingleTableFolder(
                "Control", columnLabels, allcontrols));
    display.add(mt);

    vector<MonitorContainer*>::iterator band_iter = allbands.begin();
    for(int bn=1; band_iter != allbands.end(); ++band_iter, ++bn) {
        display.add( makeDelaysFolder( bn, *(VlbiSubsystem::Band*)(*band_iter) ) );
    }

    // This actually listens and responds to requests for the display data
    while (display.serveData()) {
    }

    return 0;
}
