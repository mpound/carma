/*
 *
 * Display status of the dataflow systems and processes
 *
 * @author Original: Steve Scott
 * $Id: rtddataflow.cc,v 1.12 2013/11/19 03:41:14 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <vector>

#include "carma/monitor/MonitorSystem.h"

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/monitor/DataflowSubsystem.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/services/Global.h"
#include "carma/util/Program.h"


using namespace std;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::util;
using namespace carma::ui::rtd;
using namespace carma::ui;

static void createMPDFolder(const string& type, const DataflowSubsystem::MonitorPointDatabase& mpd, RtFolderPtr folder)
{
    RtLabelPtr header;

    RtTablePtr table(new RtTable(type));

    table->addCol( RtColumn::makeColumn( "Oldest Date" ) );
    table->addCol( RtColumn::makeColumn( "Newest Date" ) );
    table->addCol( RtColumn::makeColumn( "Oldest Frame" ) );
    table->addCol( RtColumn::makeColumn( "Newest Frame" ) );

    table->addRow( RtRow::makeRow( "Numeric" ) );
    table->addRow( RtRow::makeRow( "String" ) );
    table->addRow( RtRow::makeRow( "Short" ) );
    table->addRow( RtRow::makeRow( "Complex" ) );

    unsigned int column = 0;

    if(type == "Frame"){
        header = RtLabelPtr(new RtLabel("Frame Data"));
        column = 0;
    }
    else if(type == "Minute"){
        header = RtLabelPtr(new RtLabel("Minute Data"));
        column = 1;
    }
    else if(type == "WbCorrel"){
        header = RtLabelPtr(new RtLabel("WbCorrel Data"));
        column = 2;
    }
    else{
        header = RtLabelPtr(new RtLabel("SlCorrel Data"));
        column = 3;
    }

    for(unsigned int i = 0; i < 4; i++){
        table->add(MonitorCell::makeCell(20,mpd.databaseFiles(column).mPSubTypes(i).oldestTableDate()));
    }
    for(unsigned int i = 0; i < 4; i++){
        table->add(MonitorCell::makeCell(20,mpd.databaseFiles(column).mPSubTypes(i).newestTableDate()));
    }
    for(unsigned int i = 0; i < 4; i++){
        table->add(MonitorCell::makeCell(10,mpd.databaseFiles(column).mPSubTypes(i).oldestTableFrame()));
    }
    for(unsigned int i = 0; i < 4; i++){
        table->add(MonitorCell::makeCell(10,mpd.databaseFiles(column).mPSubTypes(i).newestTableFrame()));
    }

    const int numRows = table->getNumRows();

    folder->add(header);
    folder->add(table);
    RtSpringPtr spring(new RtSpring( 4, 1.0 ));

    folder->add( spring );
    table->setMinRows( numRows );
    table->setPrefRows( numRows );
}

static void createMDLTable(const string& type, const DataflowSubsystem::MonitorDataLoader& mdl, RtFolderPtr folder)
{
    RtLabelPtr header;

    RtTablePtr table(new RtTable(type));

    table->addCol( RtColumn::makeColumn( "Numeric" ) );
    table->addCol( RtColumn::makeColumn( "String" ) );
    table->addCol( RtColumn::makeColumn( "Short" ) );
    table->addCol( RtColumn::makeColumn( "Complex" ) );

    table->addRow( RtRow::makeRow( "Frame" ) );
    table->addRow( RtRow::makeRow( "Minute" ) );
    table->addRow( RtRow::makeRow( "WbCorrel" ) );
    table->addRow( RtRow::makeRow( "SlCorrel" ) );

    if(type == "Loaded"){
        header = RtLabelPtr(new RtLabel( " Loaded Files " ));
        folder->add(header);
        for(unsigned int i = 0; i < 4; i++){
            for(unsigned int j = 0; j < 4; j++){
                table->add(MonitorCell::makeCell(8,mdl.loadedFiles(j).mDLSubTypes(i).numberLoaded()));
            }
        }
    }
    else if(type == "Run Time"){
        header = RtLabelPtr(new RtLabel( " Run Times (min)" ));
        folder->add(header);
        for(unsigned int i = 0; i < 4; i++){
            for(unsigned int j = 0; j < 4; j++){
                table->add( MonitorCell::makeCell(8,mdl.loadedFiles(i).mDLSubTypes(j).lastRunTime()));
            }
        }
    }
    else{
        header = RtLabelPtr(new RtLabel( " Files To Be Loaded " ));
        folder->add(header);
        for(unsigned int i = 0; i < 4; i++){
            for(unsigned int j = 0; j < 4; j++){
                table->add( MonitorCell::makeCell(8,mdl.loadedFiles(i).mDLSubTypes(j).numberToLoad()));
            }
        }
    }

    const int numRows = table->getNumRows();

    folder->add(table);
    RtSpringPtr spring(new RtSpring( 4, 1.0 ));

    folder->add( spring );
    table->setMinRows( numRows );
    table->setPrefRows( numRows );
}

static RtFolderPtr createAHW(const std::string label, DataflowSubsystem::Correlator &corl)
{
    RtFolderPtr ahwFolder(new RtFolder(label));

    RtVBoxPtr topbox(new RtVBox(ONE_PIXEL_BELOW_BORDER));
    RtVBoxPtr botbox(new RtVBox(NO_BORDER));
    RtHBoxPtr topboxh(new RtHBox("TH"));
    RtHBoxPtr botboxh(new RtHBox("BH"));
    RtSpringPtr spr(new RtSpring);

    ahwFolder->add(topbox);
    ahwFolder->add(botbox);
    topbox->add(topboxh);
    botbox->add(botboxh);
    topboxh->add(spr);

    RtLabelPtr topHeading(new RtLabel("Last Processing Cycle"));
    topboxh->add(topHeading);
    topboxh->add(spr);
    botboxh->add(spr);

    RtLabelPtr botHeading(new RtLabel("File Status"));
    botboxh->add(botHeading);
    botboxh->add(spr);

    MonitorTablePtr ptable  = MonitorTable::makeTable(string(""), corl.processing());
    ptable->noColLabels();
    topbox->add(ptable);
    topbox->add(spr);

    RtSpacerPtr sp(new RtSpacer(4));
    ahwFolder->add(sp);

    vector<MonitorContainer*> mpContainers;
    vector<string> headings;
    for (int i=0; i<DataflowSubsystem::Correlator::inputFilesCount(); i++) {
        headings.push_back(string(""));
        mpContainers.push_back(&corl.inputFiles(i));
    }

    MonitorTablePtr ftable = MonitorTable::makeTable(headings, mpContainers);
    ftable->noColLabels();

    botbox->add(ftable);
    botbox->add(spr);

    return ahwFolder;
}

int Program::main()
{
    // Create a dislay
    MonitorDisplay display("Dataflow Status");
    const string hfile = Program::getConfFile("rtd/help/dataflow.html");
    display.setSpecificHelpFromTextFile( "Dataflow Window Help", hfile);

    DataflowSubsystem& df = display.cms().dataflow();
    DataflowSubsystem::AstroheaderWriter& ahw = df.astroheaderWriter();
    DataflowSubsystem::MonitorDataDeleter& mdd = df.monitorDataDeleter();
    DataflowSubsystem::MonitorDataLoader& mdl = df.monitorDataLoader();
    DataflowSubsystem::MonitorPointDatabase& mpd = df.monitorPointDatabase();

    // Define folders
    RtFolderPtr mdlFolder(new RtFolder("MonitorDataLoader"));
    RtFolderPtr dtFolder(new RtFolder("DataTransfer"));
    RtFolderPtr mddFolder(new RtFolder("MonitorDataDeleter"));
    RtFolderPtr mpdFolder(new RtFolder("MonitorPointDatabase"));
    RtSpringPtr spr(new RtSpring);

    // MDD folder
    RtVBoxPtr mddtopbox(new RtVBox(ONE_PIXEL_BELOW_BORDER));
    RtVBoxPtr mddbotbox(new RtVBox(NO_BORDER));
    RtHBoxPtr mddtopboxh(new RtHBox("TH"));
    RtHBoxPtr mddbotboxh(new RtHBox("BH"));
    mddFolder->add(mddtopbox);
    mddFolder->add(mddbotbox);
    mddtopbox->add(mddtopboxh);
    mddbotbox->add(mddbotboxh);
    mddtopboxh->add(spr);
    RtLabelPtr mddtopHeading(new RtLabel("Last Run Cycle"));
    mddtopboxh->add(mddtopHeading);
    mddtopboxh->add(spr);
    mddbotboxh->add(spr);
    RtLabelPtr mddbotHeading(new RtLabel("File Status"));
    mddbotboxh->add(mddbotHeading);
    mddbotboxh->add(spr);

    MonitorTablePtr  mddptable = MonitorTable::makeTable(string(""), mdd.lastRun());
    mddptable->noColLabels();
    mddtopbox->add(mddptable);
    mddtopbox->add(spr);
    RtSpacerPtr mddsp(new RtSpacer(4));
    mddFolder->add(mddsp);
    vector<MonitorContainer*> mddContainers;
    vector<string> mddheadings;
    for (int i=0; i<4; i++) {
        mddheadings.push_back(string(""));
        mddContainers.push_back(&(mdd.deletedFiles(i)));
    }
    MonitorTablePtr mddftable = MonitorTable::makeTable(mddheadings, mddContainers);
    mddftable->noColLabels();
    mddbotbox->add(mddftable);
    mddbotbox->add(spr);

    //MDL folder

    createMDLTable("Loaded",mdl,mdlFolder);
    createMDLTable("To Be Loaded",mdl, mdlFolder);
    createMDLTable("Run Time",mdl, mdlFolder);

    //MPD folder
    createMPDFolder("Frame",mpd,mpdFolder);
    createMPDFolder("SlCorrel",mpd,mpdFolder);
    createMPDFolder("WbCorrel",mpd,mpdFolder);
    createMPDFolder("Minute",mpd,mpdFolder);

    // Add the folders to the display
    display.add(createAHW("AHW Spectral", ahw.spectralLineCorrelator()));
    display.add(createAHW("AHW Wideband", ahw.widebandCorrelator()));
    display.add(mdlFolder);
    display.add(dtFolder);
    display.add(mddFolder);
    display.add(mpdFolder);

    while (display.serveData()) {
        /* nothing */
    }

    return 0;
}
