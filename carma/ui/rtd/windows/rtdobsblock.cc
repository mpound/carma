/*
 * @file
 *
 * @author Steve Scott
 * $Id: rtdobsblock.cc,v 1.9 2013/11/19 03:41:14 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <vector>

#include "carma/util/Program.h"
#include "carma/util/Trace.h"
#include "carma/util/programLogging.h"
#include "carma/util/ExceptionUtils.h"

#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/SignalPathSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"

#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorTable.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/ui/rtd/common/RtDisplay.h"


using namespace ::std;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui;
using namespace carma::ui::rtd;

static std::string makeHelp()
{
    return "       OBSBLOCK SUBSYSTEM HELP\n\n";
}

int carma::util::Program::main()
try {
    // Create a display
    MonitorDisplay display("Project & Obsblock Information");
    display.setSpecificHelp("Project & Obsblock Help", makeHelp());

    ControlSubsystem& ac = display.cms().control();
    const int width = 15;

    // Create 2 folders, one for each subarray

    //for (unsigned int i = 0 ; i < 1; i++ ) {
    vector<string> columnHeadings1;
    vector<MonitorContainer*> container1;
    MonitorCellPtr arrayName1 = MonitorCell::makeCell( 10, ac.spectralLineCorrelator().controllingSubarray());
    MonitorCellPtr project1 =
      MonitorCell::makeCell( 12, ac.spectralLineCorrelator().project() );
    MonitorCellPtr obsblock1 =
      MonitorCell::makeCell( 24 , ac.spectralLineCorrelator().obsBlockId() );
    MonitorCellPtr currobject1 =
	    MonitorCell::makeCell( 5 ,
		ac.spectralLineCorrelator().obsblock().currentObsObject() );
    int index1 = ac.spectralLineCorrelator().obsblock().currentObsObject().getValue();
	// NB index value changes with time this won't update properly!
	// See below.
    CellPtr currname1;
    string nameRef1;
    if ( index1 >= 0 ) {
      nameRef1 =
	ac.spectralLineCorrelator().obsblock().obsObject( index1 ).name().getValue();
      currname1 = CellPtr(new CellString ( "10.1.8", nameRef1));
    } else {
      nameRef1 =
	ac.spectralLineCorrelator().obsblock().defaultName().getValue();
      currname1 = CellPtr(new CellString ( "10.1.8", nameRef1));
    }

    RtBoxPtr verticalSourceBox1(new RtVBox("general info"));
    RtAreaPtr projectArea1(new RtArea ( "project info" ));
    projectArea1->addItem("Subarray", arrayName1 );
    projectArea1->addItem("Project", project1 );
    projectArea1->addItem("Obsblock", obsblock1 );
    projectArea1->addItem("Active ObsObject ", currname1);
    projectArea1->addItem("Active OO#", currobject1);
    RtBoxPtr line1(new RtHBox("line 1"));
    line1->add(projectArea1);
    line1->add(RtSpringPtr(new RtSpring(5.0)));
    verticalSourceBox1->add( line1 );
    RtBoxPtr topBox1(new RtHBox("top info"));
    //topBox->setBorder(ONE_PIXEL_BELOW_BORDER);
    topBox1->add( verticalSourceBox1 );
    RtFolderPtr folder1(new RtFolder("Spectral Line Correlator"));
    folder1->add( topBox1 );

    MonitorContainer * constraints1 =
      & ac.spectralLineCorrelator().obsblock().constraints();

    MonitorTablePtr constraintsTable1 =
      MonitorTable::makeTable("Constraints", * constraints1,
			      width, 1, 0, false, true , MonitorTableVisitorPtr());
    RtBoxPtr middleBox1(new RtHBox("middle info"));
    //middleBox->setBorder(ONE_PIXEL_BELOW_BORDER);
    middleBox1->add( constraintsTable1 );
    folder1->add( middleBox1 );

    for (int j=0; j < 32; j++) {
      ostringstream o;
      o << "ObsObject #" << j+1;
      columnHeadings1.push_back(o.str());
      container1.push_back( & ac.spectralLineCorrelator().obsblock().obsObject(j) );
    }
    // @todo would be way cool to have the active obs object row
    // highlighted.

    MonitorTablePtr obsObjTable1 =
      MonitorTable::makeTable(columnHeadings1, container1,
			      width, 1, 0, false, true, MonitorTableVisitorPtr());
    RtBoxPtr bottomBox1(new RtHBox("bottom info"));

    // add the row for default intents
    MonitorCellPtr defaultName1 =
      MonitorCell::makeCell( width,
			     ac.spectralLineCorrelator().obsblock().defaultName()
			     );
    MonitorCellPtr defaultPurpose1 =
      MonitorCell::makeCell( width,
			     ac.spectralLineCorrelator().obsblock().defaultPurpose()
			     );
    MonitorCellPtr defaultSelfcal1=
      MonitorCell::makeCell( width,
			     ac.spectralLineCorrelator().obsblock().defaultSelfcal()
			     );
    MonitorCellPtr defaultFastswitch1=
      MonitorCell::makeCell( width,
			     ac.spectralLineCorrelator().obsblock().defaultFastswitch()
			     );

    RtRowPtr defI1 = RtRow::makeRow("Default Intent");
    obsObjTable1->addRow( defI1 ) ;
    obsObjTable1->add( defaultName1 );
    obsObjTable1->add( defaultPurpose1 );
    obsObjTable1->add( defaultSelfcal1 );
    obsObjTable1->add( defaultFastswitch1 );

    bottomBox1->add( obsObjTable1 );
    folder1->add( bottomBox1 );

    vector<string> columnHeadings2;
    vector<MonitorContainer*> container2;

    MonitorCellPtr arrayName2 = MonitorCell::makeCell( 10 , ac.widebandCorrelator().controllingSubarray() );

    MonitorCellPtr project2 =
      MonitorCell::makeCell( 12, ac.widebandCorrelator().project() );
    MonitorCellPtr obsblock2 =
      MonitorCell::makeCell( 24 , ac.widebandCorrelator().obsBlockId() );
    MonitorCellPtr currobject2 =
      MonitorCell::makeCell( 5 ,
			     ac.widebandCorrelator().obsblock().currentObsObject() );
    int index2 = ac.widebandCorrelator().obsblock().currentObsObject().getValue();

    // NB index value changes with time this won't update properly!
    // See below.
    CellPtr currname2;
    string nameRef2;
    if ( index2 >= 0 ) {
      nameRef2 =
	ac.widebandCorrelator().obsblock().obsObject( index2 ).name().getValue();
      currname2 = CellPtr(new CellString ( "10.1.8", nameRef2));
    } else {
      nameRef2 =
	ac.widebandCorrelator().obsblock().defaultName().getValue();
      currname2 = CellPtr(new CellString ( "10.1.8", nameRef2));
    }

    RtBoxPtr verticalSourceBox2(new RtVBox("general info"));

    RtAreaPtr projectArea2(new RtArea ( "project info" ));
    projectArea2->addItem("Subarray", arrayName2 );
    projectArea2->addItem("Project", project2 );
    projectArea2->addItem("Obsblock", obsblock2 );
    projectArea2->addItem("Active ObsObject ", currname2);
    projectArea2->addItem("Active OO#", currobject2);
    RtBoxPtr line2(new RtHBox("line 2"));
    line2->add(projectArea2);
    line2->add(RtSpringPtr(new RtSpring(5.0)));
    verticalSourceBox2->add( line2 );
    RtBoxPtr topBox2(new RtHBox("top info"));


	//topBox->setBorder(ONE_PIXEL_BELOW_BORDER);
    topBox2->add( verticalSourceBox2 );
    RtFolderPtr folder2(new RtFolder("Wideband Correlator"));
    folder2->add( topBox2 );

    MonitorContainer * constraints2 =
      & ac.widebandCorrelator().obsblock().constraints();

    MonitorTablePtr constraintsTable2 =
      MonitorTable::makeTable("Constraints", * constraints2,
			      width, 1, 0, false, true , MonitorTableVisitorPtr());
    RtBoxPtr middleBox2(new RtHBox("middle info"));
    //middleBox->setBorder(ONE_PIXEL_BELOW_BORDER);
    middleBox2->add( constraintsTable2 );
    folder2->add( middleBox2 );

    for (int j=0; j < 32; j++) {
      ostringstream o;
      o << "ObsObject #" << j+1;
      columnHeadings2.push_back(o.str());
      container2.push_back( & ac.widebandCorrelator().obsblock().obsObject(j) );
    }
    // @todo would be way cool to have the active obs object row
    // highlighted.

    MonitorTablePtr obsObjTable2 =
      MonitorTable::makeTable(columnHeadings2, container2,
			      width, 1, 0, false, true, MonitorTableVisitorPtr());
    RtBoxPtr bottomBox2(new RtHBox("bottom info"));

    // add the row for default intents
    MonitorCellPtr defaultName2 =
      MonitorCell::makeCell( width,
			     ac.widebandCorrelator().obsblock().defaultName()
			     );
    MonitorCellPtr defaultPurpose2 =
      MonitorCell::makeCell( width,
			     ac.widebandCorrelator().obsblock().defaultPurpose()
			     );
    MonitorCellPtr defaultSelfcal2=
      MonitorCell::makeCell( width,
			     ac.widebandCorrelator().obsblock().defaultSelfcal()
			     );
    MonitorCellPtr defaultFastswitch2=
      MonitorCell::makeCell( width,
			     ac.widebandCorrelator().obsblock().defaultFastswitch()
			     );

    RtRowPtr defI2 = RtRow::makeRow("Default Intent");
    obsObjTable2->addRow( defI2 );
    obsObjTable2->add( defaultName2 );
    obsObjTable2->add( defaultPurpose2 );
    obsObjTable2->add( defaultSelfcal2 );
    obsObjTable2->add( defaultFastswitch2 );

    bottomBox2->add( obsObjTable2 );
    folder2->add( bottomBox2 );

	// Add the folders to the display
    display.add( folder1 );
    display.add( folder2 );

    while ( display.serveData() ) {
      int index1 = ac.spectralLineCorrelator().obsblock().currentObsObject().getValue();
      int index2 = ac.widebandCorrelator().obsblock().currentObsObject().getValue();
      if ( index1 >= 0 ) {
	nameRef1 =
	  ac.spectralLineCorrelator().obsblock().obsObject( index1 ).name().getValue();
      } else {
	nameRef1 =
	  ac.spectralLineCorrelator().obsblock().defaultName().getValue();
      }
      if ( index2 >= 0 ) {
	nameRef2 =
	  ac.widebandCorrelator().obsblock().obsObject( index2 ).name().getValue();
      } else {
	nameRef2 =
	  ac.widebandCorrelator().obsblock().defaultName().getValue();
      }
    }

    return EXIT_SUCCESS;

} catch (...) {
	std::ostringstream oss;
	oss << "rtdobsblock: exception: " << carma::util::getStringForCaught();
	std::cerr << oss.str() << std::endl;
	programLogCriticalIfPossible(oss.str());
	return EXIT_FAILURE;
}
