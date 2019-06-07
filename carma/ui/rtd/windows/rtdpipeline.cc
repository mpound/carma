// $Id: rtdpipeline.cc,v 1.41 2013/11/19 03:41:14 iws Exp $

/*
 * @file
 * 
 * Realtime display window for the wideband and spectral line correlator
 * pipelines. A command line arg determines which pipeline is used.
 *
 * @author Original: Steve Scott 
 * $id: $
 *
 * $CarmaCopyright$
 */


#include "carma/monitor/PipelineSubsystem.h"
#include "carma/monitor/PipelineSubsystemSL.h"
#include "carma/monitor/PipelineSubsystemWB.h"
#include "carma/monitor/SlPipelineSubsystem.h"
#include "carma/monitor/WbPipelineSubsystem.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"

#include <sstream>
#include <vector>

using namespace std;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui::rtd;

namespace {

    const int width = 45;

    const string helpTitle       = "Pipeline Help";
    const string helpPageHeading = "PIPELINE HELP\n\n";
    const string helpSummary     = "Correlator pipeline processing stateus"; 

    string makeHelp( ) 
    {
        ostringstream ost;
        ost << helpPageHeading
            << helpSummary
            << "The correlator pipelines catch data originating in the "
            << "correlator crates and put them together as a single unit. "
            << "Fill in more info here... ";    
        return ost.str();
    }

} // namespace <unnamed>

int
Program::main() 
{
    auto_ptr<PipelineSubsystem> pl;
    auto_ptr<MonitorDisplay> display;

    // Based on which pipeline this rtd is for, instantiate a different 
    // version of the monitor system and MonitorDisplay.
    string string1 = getStringParameter( "string1" );
    if ( string1.compare( "wb" ) == 0 ) {
        display = auto_ptr<MonitorDisplay>( 
            new MonitorDisplay( "Wideband Pipeline" ) );
        pl = auto_ptr<PipelineSubsystem>( 
            new PipelineSubsystemWB( display->cms( ).wbPipeline( ) ) );
    } else if ( string1.compare( "sl" ) == 0 ) {
        display = auto_ptr<MonitorDisplay>( 
            new MonitorDisplay( "Spectral Pipeline" ) );
        pl = auto_ptr<PipelineSubsystem>( 
            new PipelineSubsystemSL( display->cms( ).slPipeline( ) ) );
    } else {
        programLogErrorIfPossible( "Invalid input parameter - exiting." );
        return 1;
    }
        
    display->setSpecificHelp( helpTitle, makeHelp( ) );

    // Create folders with tables
    // First form up the special cases - pipeline status and decimation 
    // Pipeline Status
    RtFolderPtr pipelineFolder(new RtFolder( "Pipeline Status" ));

    vector<MonitorContainer*> stageStats;
    vector<string> stageLabels;
    {
        stageStats.push_back( &pl->getCoherenceStageStats( ) );
        stageLabels.push_back( "Coherence" );
        stageStats.push_back( &pl->getDecimationStageStats( ) );
        stageLabels.push_back( "Decimator" );
        stageStats.push_back( &pl->getTsysStageStats( ) );
        stageLabels.push_back( "Tsys" );
        stageStats.push_back( &pl->getBlankFlagStageStats( ) );
        stageLabels.push_back( "BlankFlag" );
        stageStats.push_back( &pl->getIntegratorStageStats( ) );
        stageLabels.push_back( "Integrator" );
        stageStats.push_back( &pl->getSelfCalStageStats( ) );
        stageLabels.push_back( "SelfCal" );
        stageStats.push_back( &pl->getCorrelatorPublisherStageStats( ) );
        stageLabels.push_back( "Publisher" );
        stageStats.push_back( &pl->getVisBrickStageStats( ) );
        stageLabels.push_back( "VisBrick" );
    }
    
    MonitorTablePtr stageStatTable = MonitorTable::makeTable( 
                                           stageLabels,
                                           stageStats,
                                           AUTO_SIZE,
                                           1,
                                           0,
                                           true,
                                           true,
                                           MonitorTableVisitorPtr() );
    
    MonitorContainer frameSummary( "Summary" );
    frameSummary.add( pl->getPipelineStatus( ).startOffset( ) );
    frameSummary.add( pl->getPipelineStatus( ).stopOffset( ) );
    frameSummary.add( pl->getPipelineStatus( ).totalProcTime( ) );
    frameSummary.add( pl->getPipelineStatus( ).dataFrame( ) );

    MonitorTablePtr framePipelineTable = MonitorTable::makeTable(
                                           "Summary",
                                           frameSummary,
                                           AUTO_SIZE,
                                           1,
                                           0,
                                           true,
                                           true,
                                           MonitorTableVisitorPtr() );
    
    MonitorContainer maxSummary( "Maximums" );
    maxSummary.add( pl->getPipelineStatus( ).maxStartOffset( ) );
    maxSummary.add( pl->getPipelineStatus( ).maxStopOffset( ) );
    maxSummary.add( pl->getPipelineStatus( ).maxTotalProcTime( ) );

    MonitorTablePtr maxPipelineTable = MonitorTable::makeTable(
                                           "Maximums",
                                           maxSummary,
                                           AUTO_SIZE,
                                           1,
                                           0,
                                           true,
                                           true,
                                           MonitorTableVisitorPtr() );

    MonitorContainer miscStats( "Misc" );
    miscStats.add( pl->getPipelineStatus( ).missedFrames( ) );
    miscStats.add( pl->getPipelineStatus( ).missedMonitorFrames( ) );

    MonitorTablePtr miscStatsTable = MonitorTable::makeTable(
                                           "Misc",
                                           miscStats,
                                           AUTO_SIZE,
                                           1,
                                           0,
                                           true,
                                           true,
                                           MonitorTableVisitorPtr() );

    pipelineFolder->add( stageStatTable );
    pipelineFolder->add( framePipelineTable );
    pipelineFolder->add( maxPipelineTable );
    pipelineFolder->add( miscStatsTable );

    vector<MonitorContainer*> decimationBands;
    vector<MonitorContainer*> catchDataBands;
    vector<string> bandLabels;
    vector<string> emptyLabels( pl->getBandCount() );

    const int numBands = pl->getBandCount( );
    for ( int bandIdx = 0; bandIdx < numBands; ++bandIdx ) {
        ostringstream os;
        os << "Band " << (bandIdx + 1 );
        bandLabels.push_back( os.str( ) );
        decimationBands.push_back( &( pl->getDecimation( bandIdx ) ) );
        catchDataBands.push_back( &( pl->getCatchDataBand( bandIdx ) ) );
    }   

    RtFolderPtr decimationFolder(new RtFolder("Decimator ->"));
    MonitorTablePtr decimatorBandsTable = MonitorTable::makeTable(
            bandLabels,
            decimationBands,
            AUTO_SIZE,
            1,
            0,
            true,
            false,
            MonitorTableVisitorPtr() );

    const int catchDataBandsWidth = 12; // AUTO_SIZE
    MonitorTablePtr catchDataBandsTable = MonitorTable::makeTable(
            bandLabels,
            catchDataBands,
            catchDataBandsWidth,
            1,
            0,
            true,
            false,
            MonitorTableVisitorPtr() );

    decimationFolder->add( decimatorBandsTable );

    MonitorSingleTableFolderPtr catchDataFolder(new MonitorSingleTableFolder(
                "CatchData ->", "Parameter", pl->getCatchDataStage( ), width));
    catchDataFolder->add( catchDataBandsTable );

    const int depth = 1;
    const int sample = 0;
    const bool unitsInRows = true;
    const bool switchRowsAndColumns = false;
    const MonitorTableVisitorPtr visitor;

    MonitorTablePtr tsysTable = MonitorTable::makeTable(
            "Tsys", 
            pl->getTsysStage( ),
            width, depth, sample, unitsInRows, 
            switchRowsAndColumns, visitor );

    MonitorTablePtr blankFlagTable = MonitorTable::makeTable(
            "Blanking & Flagging", 
            pl->getBlankFlagStage( ),
            width, depth, sample, unitsInRows, 
            switchRowsAndColumns, visitor );

    RtFolderPtr calFolder(new RtFolder( "Calibration & Blank/Flag ->" ));
    calFolder->add( tsysTable );
    calFolder->add( blankFlagTable );

    vector<MonitorContainer*> selfCalUsbIntegBands;
    vector<MonitorContainer*> selfCalLsbIntegBands;

    for ( int bandIdx = 0; bandIdx < numBands; ++bandIdx ) {
        selfCalUsbIntegBands.push_back(
            &( pl->getSelfCal( bandIdx, true ) ) );
        selfCalLsbIntegBands.push_back(
            &( pl->getSelfCal( bandIdx, false ) ) );
    }

    const int selfCalWidth = 15;

    MonitorTablePtr selfCalUsbIntegTable = MonitorTable::makeTable(
        emptyLabels,
        selfCalUsbIntegBands,
        selfCalWidth, depth, sample, unitsInRows,
        switchRowsAndColumns, visitor );
    
    MonitorTablePtr selfCalLsbIntegTable = MonitorTable::makeTable(
        emptyLabels,
        selfCalLsbIntegBands,
        selfCalWidth, depth, sample, unitsInRows,
        switchRowsAndColumns, visitor );

    MonitorSingleTableFolderPtr integratorFolder(new MonitorSingleTableFolder(
                "Integrator ->", "Parameter", pl->getIntegratorStage( ), width));

    RtLabelPtr usbLabel(new RtLabel( "Usb" ));
    RtLabelPtr lsbLabel(new RtLabel( "Lsb" ));
    RtFolderPtr selfCalIntegFolder(new RtFolder( "Self Cal ->" ));
    selfCalIntegFolder->add(RtLabelPtr( new RtLabel( "" ) ));
    selfCalIntegFolder->add( usbLabel );
    selfCalIntegFolder->add( selfCalUsbIntegTable );
    selfCalIntegFolder->add(RtSpacerPtr(new RtSpacer( 30 )));
    selfCalIntegFolder->add( lsbLabel );
    selfCalIntegFolder->add( selfCalLsbIntegTable );
    selfCalIntegFolder->add(RtSpacerPtr(new RtSpacer( 30 )));
    selfCalIntegFolder->add(RtSpacerPtr(new RtSpacer( 30 )));

    MonitorSingleTableFolderPtr visBrickFolder(new MonitorSingleTableFolder(
                "VisBrick Writer", "Parameter", pl->getVisBrickStage( ), width));
    MonitorSingleTableFolderPtr publisherFolder(new MonitorSingleTableFolder(
                "Publisher ->", "Parameter", pl->getCorrelatorPublisherStage( ), width));

    // Add the folders to the display
    display->add( catchDataFolder );
    display->add( decimationFolder );
    display->add( calFolder );
    display->add( integratorFolder );
    display->add( selfCalIntegFolder );
    display->add( publisherFolder );
    display->add( visBrickFolder );
    display->add( pipelineFolder );

    // Loop forever serving data to the client
    while ( display->serveData( ) );

    return 0;
}


