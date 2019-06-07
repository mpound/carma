/*
 * @file
 *
 * Display antenna & pad location information
 *
 * @author Marc Pound
 * $Id: rtdpad.cc,v 1.11 2013/11/19 03:41:14 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <iostream>
#include <sstream>
#include <vector>

#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ExceptionUtils.h"

#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/DelayEngineSubsystem.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/MonitorPointIterator.h"

#include "carma/ui/rtd/common/RowVisitor.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorTable.h"

using namespace ::std;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui::rtd;

static std::string makeHelp()
{
	return "       PAD INFORMATION HELP\n\n" ;
}

int carma::util::Program::main()
try {

    const int NANTS = 23;
    //fixed column width, hierarchy depth 1, average samples, use units
    const bool useUnits = true;
    const int hierarchy = 1;
    const int sampleNo  = 0;

    // Create a dislay
    MonitorDisplay display("Pad Information");
    display.setSpecificHelp("Pad Information Help", makeHelp());

    vector<string> columnLabel;
    vector< MonitorContainer* > container;
    DelayEngineSubsystem & delay = display.cms().delay();
    ControlSubsystem & ac = display.cms().control();

    // Add zebra striping and use long mp names for row labels
    RowVisitor * zebraVisitor = new RowVisitor( LIGHT_GRAY_CELL_COLOR );
    MonitorCellPtr arLon = MonitorCell::makeCell(
		                ac.subarray(0).arrayReference().longitude() );
    MonitorCellPtr arLat = MonitorCell::makeCell(
		                ac.subarray(0).arrayReference().latitude() );
    MonitorCellPtr arAlt = MonitorCell::makeCell(
		                ac.subarray(0).arrayReference().altitude() );

    RtAreaPtr arrayRef(new RtArea("array reference"));
    RtHBoxPtr bottomBox(new RtHBox("top line"));
    RtLabelPtr label(new RtLabel("Array Reference:"));
    const int antposCellWidth( 40 );
    arrayRef->addItem( "Antpos Filename",
                       MonitorCell::makeCell( antposCellWidth,
                                              ac.antPosFilename() ) );
    arrayRef->add( label );
    arrayRef->addItem( "Latitude (radians)", arLat);
    arrayRef->addItem( "Longitude (radians)", arLon);
    arrayRef->addItem( "Altitude (m)", arAlt);
    bottomBox->add( arrayRef );
    // squish it to the left
    bottomBox->add(RtSpringPtr(new RtSpring(5.0)));

    for (int i=0; i < NANTS; i++) {
	ostringstream o;
	o << "C"  << i+1;
	const string antName = o.str() ;
	columnLabel.push_back( antName );
	MonitorContainer * theColumn = new MonitorContainer( antName );

	theColumn->add( ac.antenna(i).location().latitude() );
	theColumn->add( ac.antenna(i).location().longitude() );
	theColumn->add( ac.antenna(i).location().altitude() );
	theColumn->add( ac.antenna(i).padNumber() );
	theColumn->add( ac.antenna(i).padOffset().east() );
	theColumn->add( ac.antenna(i).padOffset().north() );
	theColumn->add( ac.antenna(i).padOffset().up() );
	theColumn->add( ac.antenna(i).antennaOffset().east() );
	theColumn->add( ac.antenna(i).antennaOffset().north() );
	theColumn->add( ac.antenna(i).antennaOffset().up() );
	theColumn->add( ac.antenna(i).totalENU().east() );
	theColumn->add( ac.antenna(i).totalENU().north() );
	theColumn->add( ac.antenna(i).totalENU().up() );
	theColumn->add( delay.delayData(i).x() );
	theColumn->add( delay.delayData(i).y() );
	theColumn->add( delay.delayData(i).z() );
	theColumn->add( delay.delayData(i).delayOffset() );
	theColumn->add( delay.delayData(i).padDelay() );
	theColumn->add( delay.delayData(i).antennaDelay() );
	theColumn->add( delay.delayData(i).adjustableDelay() );
	int zebra = 0;
	MonitorPointIterator columnmpi( *theColumn, 1 );
	while ( columnmpi++ )
	    if ( ++zebra % 2 )
		zebraVisitor->addToEffectedSet( columnmpi.getMonitorPoint() );

        container.push_back( theColumn );
    }

    RtFolderPtr folder(new RtFolder( "Pad Information" ));

    MonitorTablePtr table = MonitorTable::makeTable( columnLabel,
                                                    container,
                                                    AUTO_SIZE,
                                                    hierarchy,
                                                    sampleNo,
                                                    useUnits,
                                                    zebraVisitor );


    // Add the folders to the display
    folder->add(RtSpringPtr(new RtSpring( 0.5 )));
    folder->add( bottomBox );
    folder->add(RtSpringPtr(new RtSpring( 0.5 )));
    folder->add( table );
    folder->add(RtSpringPtr(new RtSpring( 5.0 )));
    display.add( folder );

    while ( display.serveData() ) {
	}

    return EXIT_SUCCESS;

} catch ( ... ) {
	std::ostringstream oss;
	oss << "rtdpad: exception: " << carma::util::getStringForCaught();
	std::cerr << oss.str() << std::endl;
	programLogErrorIfPossible(oss.str());
	return EXIT_FAILURE;;
}
