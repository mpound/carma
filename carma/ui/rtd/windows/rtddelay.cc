/*
 * @file
 *
 * Gets data from the carma delay and displays.
 *
 * @author Marc Pound
 * $Id: rtddelay.cc,v 1.25 2013/11/19 03:41:14 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <vector>
using namespace ::std;

#include "carma/util/Program.h"
using namespace carma::util;

#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/DelayEngineSubsystem.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/MonitorPointIterator.h"
using namespace carma::monitor;

#include "carma/ui/rtd/common/ZebraVisitor.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
using namespace carma::ui::rtd;

static std::string makeHelp()
{
	ostringstream oss;
	oss << "	   DELAY ENGINE SUBSYSTEM HELP\n\n"
		<< "This window displays delay information for carma. "
		<< "The folder shows the the most recent of the time-tagged triplet of "
		<< "delays which have been calculated and sent to the Lobe Rotator "
		<< "and Correlator.  ";
	return oss.str();
}

int carma::util::Program::main()
{
    const int NANTS = 23;

    // Create a dislay
    MonitorDisplay display("Delay Status");
    display.setSpecificHelp("Delay Engine Help", makeHelp());

    vector<string> columnHeadings;
    vector< MonitorContainer* > container;
    ZebraVisitorPtr stripeVisitor(new ZebraVisitor());
    DelayEngineSubsystem & delay = display.cms().delay();
    MonitorContainer * mc;
    ControlSubsystem & ac1 = display.cms().control();
    for (int i=0; i < NANTS; i++) {
        ostringstream o;
        o << "C"  << i+1;
        columnHeadings.push_back(o.str());
        mc = &delay.delayData(i);
        mc->add( ac1.antenna(i).rxDelay3mmPol2() );
        mc->add( ac1.antenna(i).rxDelay1mmPol1() );
        mc->add( ac1.antenna(i).rxDelay1mmPol2() );
        mc->add( ac1.antenna(i).rxDelay1cmPol1() );
        mc->add( ac1.antenna(i).rxDelay1cmPol2() );

        stripeVisitor->addMonitorContainer(*mc);
        container.push_back( mc );
    }

    //const int width = AUTO_SIZE; /*Too wide!*/

    //fixed column width, hierarchy depth 1, average samples, use units
    const int width = 10;
    const bool useUnits = true;
    const int hierarchy = 1;
    const int sampleNo  = 0;
    // Create 3 folders, each one containing one of the sample numbers
    MonitorSingleTableFolderPtr folder1(new MonitorSingleTableFolder(
            "Current Delays", columnHeadings, container,
             width, hierarchy, sampleNo, useUnits , stripeVisitor));

    // Add the folders to the display
    display.add( folder1 );

    // set some custom formats
    for (int i=0; i < NANTS; i++) {
    // Make sure the timestamps are formatted to fit in the cell.
        delay.delayData(i).calculatedAt().setFormat(MonitorPointAbstime::TIME);
        delay.delayData(i).calculatedFor().setFormat(MonitorPointAbstime::TIME);
        delay.delayData(i).validUntil().setFormat(MonitorPointAbstime::TIME);
    // these delays are typically picoseconds so give more
    // digits to the right of the decimal point
        delay.delayData(i).heightDelay().setPrecision(6);
        delay.delayData(i).troposphericDelay().setPrecision(6);
    }

    {
        ControlSubsystem & ac = display.cms().control();
        vector<string> columnLabel;
        vector<MonitorContainer*> container;
        // Add zebra striping
        ZebraVisitorPtr zebraVisitor(new ZebraVisitor());
        for (int i=0; i < NANTS; i++) {
           ostringstream o;
           o << "C"  << i+1;
           const string antName = o.str() ;
           columnLabel.push_back( antName );
           MonitorContainer * theColumn = new MonitorContainer( antName );
           theColumn->add( delay.delayData(i).u() );
           theColumn->add( ac.antenna(i).interpU() );
           theColumn->add( delay.delayData(i).v() );
           theColumn->add( ac.antenna(i).interpV() );
           theColumn->add( delay.delayData(i).w() );
           theColumn->add( ac.antenna(i).interpW() );

           zebraVisitor->addMonitorContainer(*theColumn);
           container.push_back( theColumn );
        }
        MonitorTablePtr uvwTable =
            MonitorTable::makeTable(columnLabel, container,
                width, hierarchy, sampleNo , useUnits,
                false,  zebraVisitor);
        RtFolderPtr folder2(new RtFolder( "UVW interpolation" ));
        folder2->add( uvwTable );
        display.add( folder2 );
    }

    while ( display.serveData() ) {
    }

    return 0;
}
