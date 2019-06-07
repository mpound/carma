#include "carma/util/Program.h"
//#include "carma/util/programLogging.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/SignalPathCommonMonitorPoints.h"
#include "carma/monitor/SignalPathSubsystem.h"
#include "carma/ui/rtd/common/MonitorTable.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorTable.h"

#include <iostream>
#include <set>

using namespace std;

using namespace carma::monitor;
using namespace carma::ui::rtd;
using namespace carma::util;

vector<MonitorTablePtr>
makeAstroBandTable( SignalPathSubsystem::Astroband * ab ) 
{
    const int columnWidth = 9;
    const int hierarchyDepth = 1;
    const int sampleNo = 1;
    const bool unitsInRowLabels=false;
    const bool switchRowsAndColumns = false;

    vector<MonitorTablePtr> mtable;
    vector< MonitorContainer * > containers; 
    vector< string > columnHeadings;
    const unsigned int numInputs = ab->inputCount();

    // split into two tables for better use of screen real estate
    for( unsigned int j=0;j<16;++j) {
        containers.push_back( &ab->input(j) );
        ostringstream os;
        os << ( j < 10 ? "Input  " : "Input ") 
           << j+1;
        columnHeadings.push_back( os.str() );
    }
    mtable.push_back(
            MonitorTable::makeTable( 
                columnHeadings, containers,
                columnWidth, hierarchyDepth, sampleNo
                , unitsInRowLabels, switchRowsAndColumns, MonitorTableVisitorPtr() )
            );

    columnHeadings.clear();
    containers.clear();
    for( unsigned int j=16;j<numInputs;++j) {
        containers.push_back( &ab->input(j) );
        ostringstream os;
        os << "Input " << j+1;
        columnHeadings.push_back( os.str() );
    }
    mtable.push_back(
            MonitorTable::makeTable( 
                columnHeadings, containers,
                columnWidth, hierarchyDepth, sampleNo
                , unitsInRowLabels, switchRowsAndColumns, MonitorTableVisitorPtr() )
            );
    return mtable;
}


int Program::main() {

    MonitorDisplay display( "Astroband Status" );
    display.setSpecificHelp( "Astroband Help", "Help yourself!" );
    const string string1Param = getStringParameter( "string1" );
    const int abNo = atoi( string1Param.c_str() );

    SignalPathSubsystem & signalPath = display.cms().signalPath();
    const unsigned int abIdx = abNo - 1;
    const unsigned int numAstrobands = signalPath.mapping().astrobandCount();
    if ( abIdx >= numAstrobands ) 
        return 1;

    SignalPathSubsystem::Astroband * ab = 
        &signalPath.mapping().astroband(abIdx);
    ostringstream os;
    os << "Astroband " << ab->getAstrobandNo();
    const string bandLabel = os.str();
    //programLogNoticeIfPossible( bandLabel );
    RtVBoxPtr vbox(new RtVBox( bandLabel ));
    RtFolderPtr folder(new RtFolder( bandLabel ));
    RtHBoxPtr hbox(new RtHBox("line 1"));
    MonitorCellPtr confName = MonitorCell::makeCell( 25, ab->confName() );
    MonitorCellPtr confTag  = MonitorCell::makeCell( 15, ab->confTag() );
    hbox->add(RtLabelPtr(new RtLabel(ab->confName().getShortName())));
    hbox->add(RtSpacerPtr(new RtSpacer(5)));
    hbox->add( confName );
    hbox->add(RtSpacerPtr(new RtSpacer(15)));
    hbox->add(RtLabelPtr(new RtLabel(ab->confTag().getShortName())));
    hbox->add(RtSpacerPtr(new RtSpacer(5)));
    hbox->add( confTag );
    hbox->add(RtSpringPtr(new RtSpring(5)));
    vector<MonitorTablePtr> abTable = makeAstroBandTable( ab );
    vbox->add( hbox );
    vbox->add( abTable[0] );
    vbox->add( abTable[1] );
    folder->add( vbox );
    display.add( folder );

    while ( display.serveData() );

    return 0;
}
