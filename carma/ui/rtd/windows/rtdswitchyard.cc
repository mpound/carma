#include "carma/util/Program.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/ui/rtd/common/MonitorTable.h"
#include "carma/monitor/SignalPathSubsystem.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorTable.h"

#include <iostream>
#include <set>

using namespace std;

using namespace carma::monitor;
using namespace carma::ui::rtd;
using namespace carma::util;

void
populateModuleContainer( CM::Switchyard & switchyard,
                         MonitorContainer & container ) 
{
    // Create a MonitorContainer which doesn't contain the switch positions.
    std::set< tagIDType > tagsToOmit; 
    for ( int i = 0; i < switchyard.getNumSwitchPosition(); ++i ) {
        tagsToOmit.insert( switchyard.switchPosition( i ).getTagID() );
    }

    MonitorPointIterator mpi( switchyard, 1 );
    while ( ++mpi ) {
        MonitorPoint & mp = mpi.getMonitorPoint();
        if ( tagsToOmit.find( mp.getTagID() ) == tagsToOmit.end() ) 
            container.add( mp );
    }
}

MonitorTablePtr
makeModuleTable( SignalPathSubsystem & signalPath ) 
{
    MonitorContainer * ifCont = new MonitorContainer( "IFSwitchyard" );
    MonitorContainer * loCont = new MonitorContainer( "LOSwitchyard" );
    MonitorContainer * llCont = new MonitorContainer( "LLSwitchyard" );
    MonitorContainer * dcloCont = new MonitorContainer( "DCLOSwitchyard" );

    ifCont->add( signalPath.iFSwitchyard().state() );
    loCont->add( signalPath.lOSwitchyard().state() );
    llCont->add( signalPath.lLSwitchyard().state() );
    dcloCont->add( signalPath.dCLOSwitchyard().state() );

    populateModuleContainer( signalPath.iFSwitchyard().switchyard(), *ifCont );
    populateModuleContainer( signalPath.lOSwitchyard().switchyard(), *loCont );
    populateModuleContainer( signalPath.lLSwitchyard().switchyard(), *llCont );
    populateModuleContainer( signalPath.dCLOSwitchyard().switchyard(), 
                             *dcloCont );


    vector< MonitorContainer * > containers; 
    containers.push_back( ifCont );
    containers.push_back( loCont );
    containers.push_back( llCont );
    containers.push_back( dcloCont );

    vector< string > columnHeadings;
    columnHeadings.push_back( "Ant IF" );
    columnHeadings.push_back( "LO Ref" );
    columnHeadings.push_back( "Linelength" );
    columnHeadings.push_back( "2nd LO" );

    return MonitorTable::makeTable( columnHeadings, containers );
}

MonitorTablePtr
makeXacTable( SignalPathSubsystem & signalPath ) 
{
    MonitorContainer * ifXac = new MonitorContainer( "IFSwitchyard" );
    MonitorContainer * loXac = new MonitorContainer( "LOSwitchyard" );
    MonitorContainer * llXac = new MonitorContainer( "LLSwitchyard" );
    MonitorContainer * dcloXac = new MonitorContainer( "DCLOSwitchyard" );

    ifXac->add( signalPath.iFSwitchyard().state() );
    ifXac->add( signalPath.iFSwitchyard().xac() );
    loXac->add( signalPath.lOSwitchyard().state() );
    loXac->add( signalPath.lOSwitchyard().xac() );
    llXac->add( signalPath.lLSwitchyard().state() );
    llXac->add( signalPath.lLSwitchyard().xac() );
    dcloXac->add( signalPath.dCLOSwitchyard().state() );
    dcloXac->add( signalPath.dCLOSwitchyard().xac() );

    vector< MonitorContainer * > xacContainers;
    xacContainers.push_back( ifXac );
    xacContainers.push_back( loXac );
    xacContainers.push_back( llXac );
    xacContainers.push_back( dcloXac );

    vector< string > columnHeadings;
    columnHeadings.push_back( "Ant IF" );
    columnHeadings.push_back( "LO Ref" );
    columnHeadings.push_back( "LL Ref" );
    columnHeadings.push_back( "2nd LO" );

    return MonitorTable::makeTable( columnHeadings,  xacContainers, AUTO_SIZE, 2);
}

RtTablePtr
makeSwitchTable( CM::Switchyard & switchyard )
{
    const int numSwitches = switchyard.getNumSwitchPosition();
    const int cols = 8;
    const int rows = (numSwitches%8) ? (numSwitches/8) + 1: (numSwitches/8);

    RtTablePtr table(new RtTable("Switch Pos Table"));
    for ( int c = 0; c < cols; ++c ) {
        ostringstream colLabel;
        colLabel << c + 1;
        table->addCol( RtColumn::makeColumn( colLabel.str() ) );
    }

    for ( int r = 0; r < rows; ++r ) {
        ostringstream rowLabel;
        rowLabel << "Switches " << r*cols + 1 << "-";
        const int end = r*cols + cols;
        if ( end > numSwitches ) 
            rowLabel << numSwitches;
        else 
            rowLabel << end;
        table->addRow( RtRow::makeRow( rowLabel.str() ) );
    }

    table->setReverseOrder();
    table->noColLabels();

    const int cellWidth = 3;
    ostringstream fmtOss; 
    fmtOss << cellWidth << ".0.0";
    for ( int cellIdx = 0; cellIdx < rows * cols; ++cellIdx ) {
        if ( cellIdx < numSwitches ) 
            table->addCell( MonitorCell::makeCell( 
                cellWidth, switchyard.switchPosition( cellIdx ) ) );
        else
            table->addCell(CellPtr(new CellEmpty( fmtOss.str().c_str( ) )));
    }

    return table;
}



int Program::main() {

    MonitorDisplay display( "Switchyard Module Status" );
    display.setSpecificHelp( "Switchyard Module Help", "Help yourself!" );

    RtFolderPtr folder(new RtFolder( "Switchyard" ));
    folder->add(RtSpacerPtr(new RtSpacer( 20 )));

    SignalPathSubsystem & signalPath = display.cms().signalPath();

    const string tab = "             ";
    
    RtVBoxPtr ifBox(new RtVBox( "IF Switch Positions" ));
    RtLabelPtr ifLabel (new RtLabel( tab + "IF Switch Positions" ));
    ifBox->add( ifLabel );
    RtTablePtr ifTable = makeSwitchTable(signalPath.iFSwitchyard().switchyard());
    ifBox->add( ifTable );
    folder->add( ifBox );

    folder->add(RtSpacerPtr(new RtSpacer( 20 )));

    RtVBoxPtr loBox(new RtVBox( "LO Switch Positions" ));
    RtLabelPtr loLabel (new RtLabel( tab + "LO Switch Positions" ));
    loBox->add( loLabel );
    RtTablePtr loTable = makeSwitchTable(signalPath.lOSwitchyard().switchyard());
    loBox->add( loTable );
    folder->add( loBox );
    folder->add(RtSpacerPtr(new RtSpacer( 20 )));

    RtVBoxPtr llBox(new RtVBox( "LL Switch Positions" ));
    RtLabelPtr llLabel (new RtLabel( tab + "LL Switch Positions" ));
    llBox->add( llLabel );
    RtTablePtr llTable = makeSwitchTable(signalPath.lLSwitchyard().switchyard());
    llBox->add( llTable);
    folder->add( llBox );
    folder->add(RtSpacerPtr(new RtSpacer( 20 )));

    RtVBoxPtr dcloBox(new RtVBox( "2nd LO Switch Positions" ));
    RtLabelPtr dcloLabel (new RtLabel( tab + "2nd LO Switch Positions" ));
    dcloBox->add( dcloLabel );
    RtTablePtr dcloTable = makeSwitchTable( signalPath.dCLOSwitchyard().switchyard());
    dcloBox->add( dcloTable);
    folder->add( dcloBox );
    folder->add(RtSpacerPtr(new RtSpacer( 20 )));

    MonitorTablePtr modTable = makeModuleTable( signalPath );
    folder->add( modTable );
    
    RtFolderPtr xacFolder(new RtFolder( "Switchyard (xac)" ));
    MonitorTablePtr xacTable = makeXacTable( signalPath );
    xacFolder->add( xacTable );

    display.add( folder );
    display.add( xacFolder );

    while ( display.serveData() );

    return 0;
}
