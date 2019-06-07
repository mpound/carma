/**
 * @file RTD for block downconverters.
 * @author Andy Beard
 */

#include "carma/util/Program.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/monitor/MonitorContainer.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/monitor/SldcSubsystem.h"

#include <iostream>
#include <vector>

using namespace carma;
using namespace carma::monitor;
using namespace carma::ui::rtd;
using namespace carma::util;
using namespace std;

int
Program::main( ) {

    MonitorDisplay display( "Block Downconverters" );

    display.setSpecificHelp( "Block Downconverter Help", "" );

    vector< string > columnLabels;

    typedef vector< MonitorContainer * > MonitorContainerVec;
    MonitorContainerVec blockDcContainerVec;
    MonitorContainerVec blockDcXacContainerVec;

    const int numBdcs = SldcSubsystem::blockDownconverterContainerCount( );
    for ( int i = 0; i < numBdcs; ++i ) {
        ostringstream columnLabel;
        columnLabel << "Input " << ( i + 1 );
        columnLabels.push_back( columnLabel.str() );

        MonitorContainer * blockDcMc =
            new MonitorContainer( "BlockDc" + columnLabel.str() );
        MonitorContainer * blockDcMcXac =
            new MonitorContainer( "BlockDcXac" + columnLabel.str() );

        SldcSubsystem::BlockDownconverterContainer & bdcMonContainer =
            display.cms().sldc().blockDownconverterContainer( i );

        blockDcMc->add( bdcMonContainer.state( ) );
        blockDcMc->add( bdcMonContainer.blockDownconverter( ) );
        blockDcContainerVec.push_back( blockDcMc );

        blockDcMcXac->add( bdcMonContainer.state( ) );
        blockDcMcXac->add( bdcMonContainer.xac( ) );
        blockDcXacContainerVec.push_back( blockDcMcXac );
    }

    MonitorSingleTableFolderPtr blockDcFolder(new MonitorSingleTableFolder(
                                            "Block Downconverter",
                                            columnLabels,
                                            blockDcContainerVec,
                                            AUTO_SIZE,
                                            2 ));

    MonitorSingleTableFolderPtr blockDcXacFolder(new MonitorSingleTableFolder(
                                               "Block Downconverter (XAC)",
                                               columnLabels,
                                               blockDcXacContainerVec,
                                               AUTO_SIZE,
                                               2 ));

    display.add( blockDcFolder );
    display.add( blockDcXacFolder );

    while ( display.serveData( ) );

    return 0;

} // Program::main
