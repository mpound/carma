/*
 *
 * Composite observer's window
 *
 * @author Marc Pound
 * $Id: compositeFolders.cc,v 1.90 2014/06/04 17:09:50 mpound Exp $
 *
 * $CarmaCopyright$
 */


#include <iostream>
#include <sstream>
#include <vector>

#include "carma/monitor/AlarmSubsystem.h"
#include "carma/monitor/AntennaCommon.h"
#include "carma/monitor/AstroSubsystemExt.h"
#include "carma/monitor/BimaSubsystem.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/ControlCorrelEnum.h"
#include "carma/monitor/CorrDesignation.h"
#include "carma/monitor/FaultSubsystem.h"
#include "carma/monitor/ImrSubsystem.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/monitor/PhaseMonitorSubsystem.h"
#include "carma/monitor/OpacityMonitorSubsystem.h"
#include "carma/monitor/SignalPathSubsystem.h"
#include "carma/monitor/SlPipelineSubsystemExt.h"
#include "carma/monitor/SldcSubsystem.h"
#include "carma/monitor/WbPipelineSubsystemExt.h"
#include "carma/monitor/C3gMax8PipelineSubsystemExt.h"
#include "carma/monitor/C3gMax23PipelineSubsystemExt.h"
#include "carma/monitor/WbdcSubsystem.h"
#include "carma/monitor/WeatherSubsystem.h"
#include "carma/monitor/PipelineSubsystem.h"
#include "carma/monitor/PipelineSubsystemSL.h"
#include "carma/monitor/PipelineSubsystemWB.h"
#include "carma/monitor/SlPipelineSubsystem.h"
#include "carma/monitor/WbPipelineSubsystem.h"

#include "carma/ui/rtd/common/ZebraVisitor.h"
#include "carma/ui/rtd/common/AngleCell.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorTable.h"
#include "carma/ui/rtd/common/RtDisplay.h"
#include "carma/ui/rtd/common/CorrModeCell.h"

#include "carma/ui/rtd/windows/compositeFolders.h"
#include "carma/ui/rtd/windows/AzelPlotManager.h"
#include "carma/ui/rtd/windows/CompositeSubarrayDisplay.h"

#include "carma/util/CorrelatorSet.h"
#include "carma/util/IllegalStateException.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/Trace.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui;
using namespace carma::ui::rtd;

// subarray convenience enumerations
static const int SCI1  = 1;
static const int SCI2  = 2;
static const int ENG1  = 3;
static const int ENG2  = 4;
static const int MAINT = 5;

static const ui::rtd::CellColor HILITED ( ORANGE_CELL_COLOR );
static const string LONG_FORMAT("96.2.92");
static const string NOTIFY_FORMAT("80.1.78");
static const string MEDIUM_FORMAT("61.3.55");
static const string NO_ANT_FORMAT("44.3.38");
static const string SHORT_FORMAT("16.2.12");
static const string HEY_YOU(
        "To control antennas in the OFFLINE subarray, add them to another subarray with addAntenna()"
        );
static const string NO_ANTENNAS("NO ANTENNAS ASSIGNED TO THIS SUBARRAY");

    carma::ui::rtd::RtFolderPtr
carma::ui::rtd::makeFolderForSubarray(const int saNo,
        CompositeSubarrayDisplay * display,
        const SubarrayStatus * saStatus)
{
    try {

        const string saName = ControlSubsystem::getSubarrayAlphanumericName( saNo );
        RtFolderPtr folder(new RtFolder( saName ));

        RtHBoxPtr topBox = makeSourceInfoBox( saNo, display, saStatus ) ;
        folder->add( topBox );

        if ( saNo != MAINT ) {
            ostringstream labos;
            labos << "azel plot " << saNo << " will go here";
            RtHBoxPtr azelBox(new RtHBox( labos.str() ));
            azelBox->setBorder(TWO_PIXELS_ALL_SIDES_BORDER);
            RtAzelPlotPtr azelPlot = display->getAzelPlot( saNo );
            azelBox->add( azelPlot );
            RtSpringPtr spring1(new RtSpring( 1.0 ));
            azelBox->add( spring1 );
            topBox->add( azelBox );
            RtSpringPtr spring2(new RtSpring( 5.0 ));
            topBox->add( spring2 );
            RtBoxPtr antTable = makeAntTableBox( saNo, display, saStatus );
            folder->add( antTable );
            RtSpringPtr spring3(new RtSpring( 5.0 ));
            folder->add( spring3 );
            /*
               RtLabel * testLabel = new RtLabel("My God, it's full of stars!");
               folder->add( testLabel );
               RtSpring * spring4 = new RtSpring( 5.0 );
               folder->add( spring4 );
               */
        }

        //os.str("");
        //os << "Exiting makeFolderForSubarray "
        //    << " Subarray #"<<saNo;
        //programLogNoticeIfPossible( os.str() );
        return folder;

    } catch ( ... ) {
        programLogErrorIfPossible("caught error in makeFolderForSubarry");
        throw;
    }

}

carma::ui::rtd::RtHBoxPtr
carma::ui::rtd::makeSourceInfoBox ( const int saNo,
        CompositeSubarrayDisplay * display,
        const SubarrayStatus * saStatus )
{

    try {
        const int saIdx = saNo - 1;
        const ControlSubsystem& ac = display->cms().control();
        const WeatherSubsystem& wc = display->cms().weather();
        const ControlSubsystem::SpectralLineCorrelator & slc =
            display->cms().control().spectralLineCorrelator();
        const ControlSubsystem::WidebandCorrelator & wbc =
            display->cms().control().widebandCorrelator();
        const ControlSubsystem::C3gMax8Correlator & c3gmax8 =
            display->cms().control().c3gMax8Correlator();
        const ControlSubsystem::C3gMax23Correlator & c3gmax23 =
            display->cms().control().c3gMax23Correlator();

        const ControlSubsystemBase::Subarray & sa = ac.subarray(saIdx);

        RtVBoxPtr verticalSourceBox(new RtVBox("general info"));


        // Cells common to all subarrays (except OFFLINE == SA#5)
        MonitorCellPtr arrayName = MonitorCell::makeCell(10, sa.name());
        ///
        // measured weather params
        ///
        MonitorCellPtr atmTemp    =
            MonitorCell::makeCell( 4, wc.ambientTemperature() );
        MonitorCellPtr atmTempF   = MonitorCell::makeCell(4, wc.ambientTempF());
        MonitorCellPtr relhumid   = MonitorCell::makeCell( 4, wc.humidity() );
        //MonitorCell* h2odensity = MonitorCell::makeCell( 4, wc.waterDensity() );
        MonitorCellPtr h2omm      = MonitorCell::makeCell( 4, wc.precipWater() );

        //tObjsToDelete.push_back( h2odensity );

        // Areas & boxes for above.

        RtAreaPtr subarrayArea(new RtArea( "subarray info" ));
        RtAreaPtr projectArea2(new RtArea( "project info 2" ));
        subarrayArea->addItem("Subarray", arrayName );

        RtAreaPtr projectArea(new RtArea( "project info" ));
        RtAreaPtr scriptArea(new RtArea("script info"));
        bool haveProject = false;
        bool haveProject2 = false;
        if ( saNo != MAINT ) {
            MonitorCellPtr running   = MonitorCell::makeCell(6, sa.controllerRunning());
            MonitorCellPtr initialized = MonitorCell::makeCell(6, sa.controllerInitialized());
            running->setGoodColor(GREEN_CELL_COLOR);
            running->setWarnColor(YELLOW_CELL_COLOR);
            running->setErrorColor(RED_CELL_COLOR);
            initialized->setGoodColor(GREEN_CELL_COLOR);
            initialized->setWarnColor(YELLOW_CELL_COLOR);
            initialized->setErrorColor(RED_CELL_COLOR);
            subarrayArea->addItem("Running", running );
            subarrayArea->addItem("Initialized", initialized );
            CellPtr saStatusMode(new CellString( SHORT_FORMAT.c_str() , saStatus->mode() ));
            // TrackMode cell tooltip text. This cell is not created
            // from a monitor point (i.e. a MonitorCell), so make
            // a custom tooltip for it.
            saStatusMode->setCellName("Carma.AntennaN.trackMode");
            ostringstream os;
            os << "Subarray pointing mode. One of IDLE, AZ/EL, RA/DEC, MIXED."
                << "  This cell is a combination of all the antenna tracking "
                << "mode monitor points.";
            saStatusMode->setToolTipText( os.str() );
            saStatusMode->setValidity( true );
            subarrayArea->addItem( "Track Mode", saStatusMode );
            MonitorCellPtr config = MonitorCell::makeCell(5, sa.configName());
            subarrayArea->addItem( "Config", config );
            MonitorCellPtr sProject =
                MonitorCell::makeCell( 12, slc.project() );
            MonitorCellPtr sObsblock =
                MonitorCell::makeCell( 48, slc.obsBlockId() );
            MonitorCellPtr wProject =
                MonitorCell::makeCell( 12, wbc.project() );
            MonitorCellPtr wObsblock =
                MonitorCell::makeCell( 48, wbc.obsBlockId() );
            MonitorCellPtr runtime =
                MonitorCell::makeCell(5 , sa.trackRuntime() );

            CellPtr noneProject(new CellString("12.1.10","NONE"));
            CellPtr noneObsblock(new CellString("48,2,44", "NONE"));
            if( saNo == SCI1 || saNo == SCI2 ) {

                if(display->cms().signalPath().mapping().subarray(saIdx).CORRELATOR_DESIGNATION_MP().isValid()) {

                    CorrelatorSet corrSet(display->cms().signalPath().mapping().subarray(saIdx).CORRELATOR_DESIGNATION_MP().getValue());

                    if(corrSet.includesSpectral()) {
                        projectArea->addItem("Project", sProject );
                        projectArea->addItem("Obsblock ID", sObsblock );
                        projectArea->addItem("TrackRuntime", runtime, "hrs");
                        haveProject = true;
                    }


                    if(corrSet.includesWideband()) {

                        if(!haveProject){
                            projectArea->addItem("Project", wProject );
                            projectArea->addItem("Obsblock ID", wObsblock );
                            projectArea->addItem("TrackRuntime", runtime, "hrs");
                            haveProject = true;
                        } else{
                            projectArea2->addItem("                      Wideband Obsblock ID", wObsblock );
                            haveProject2 = true;
                        }
                    }
                }

                if(!haveProject && !haveProject2){
                    projectArea->addItem("Project", noneProject );
                    projectArea->addItem("Obsblock ID", noneObsblock );
                    projectArea->addItem("TrackRuntime", runtime, "hrs");
                }

            }

            MonitorCellPtr scriptName = MonitorCell::makeCell( 57, sa.scriptName() );
            scriptArea->addItem("Script", scriptName);
            MonitorCellPtr scriptState = MonitorCell::makeCell( 10, sa.scriptState() );
            scriptState->setGoodColor( GREEN_CELL_COLOR );
            scriptState->setWarnColor( HILITED );
            scriptArea->addItem("Script Status", scriptState);
            MonitorCellPtr ucat = MonitorCell::makeCell( 32, sa.userCatalog() );
            scriptArea->addItem("UCat",ucat);
        } else {
            CellPtr maintLabel(new CellString ( LONG_FORMAT.c_str(), HEY_YOU, HILITED ));
            maintLabel->setValidity( true );

            projectArea->add( maintLabel );
        }

        RtHBoxPtr line0(new RtHBox("line 0"));
        line0->add(subarrayArea);
        RtSpringPtr spring4(new RtSpring( 5.0 ));
        RtSpringPtr spring5(new RtSpring( 5.0 ));
        RtSpringPtr spring6(new RtSpring( 5.0 ));
        line0->add( spring4 );
        RtHBoxPtr line1(new RtHBox("line 1"));
        RtHBoxPtr line99(new RtHBox("line 99"));
        verticalSourceBox->add( line0 );
        if(haveProject){
            line1->add(projectArea);
            line1->add( spring5 );
            verticalSourceBox->add( line1 );
        }
        if(haveProject2){
            line99->add( projectArea2 );
            line99->add( spring6 );
            verticalSourceBox->add( line99 );
        }


        ///
        // Cells common to Sci & Eng subarrays
        ///
        if (saNo != MAINT ) {
            RtHBoxPtr line1a(new RtHBox("line 1a"));
            line1a->add( scriptArea );
            RtSpringPtr spring5a(new RtSpring( 5.0 ));
            line1a->add( spring5a );
            verticalSourceBox->add( line1a );
            MonitorCellPtr source = MonitorCell::makeCell( 9, sa.source() );
            MonitorCellPtr dopplerSource = MonitorCell::makeCell( 9, sa.dopplerSource() );
            MonitorCellPtr dopplerVel = MonitorCell::makeCell( 6, sa.velObservatory() );
            AngleCellPtr ra = AngleCell::makeCell( 10, sa.phaseCenterRa() );
            ra->setAngleFormat( FORMAT_HMS );
            ra->setModulus( MOD_TWO_PI );
            AngleCellPtr dec = AngleCell::makeCell( 11, sa.phaseCenterDec() );
            dec->setAngleFormat( FORMAT_DMS );
            dec->setModulus( MOD_PI );
            MonitorCellPtr dra = MonitorCell::makeCell(5, sa.commands().phaseCenterOffset().ra());
            MonitorCellPtr ddec = MonitorCell::makeCell(5, sa.commands().phaseCenterOffset().dec());
            MonitorCellPtr vel = MonitorCell::makeCell(5, sa.velocity());
            MonitorCellPtr loFreq = MonitorCell::makeCell( 6, sa.loFreq() );
            RtAreaPtr sourceArea1(new RtArea("source info top line"));
            sourceArea1->addItem( "Source", source);
            sourceArea1->addItem( "RA", ra);
            sourceArea1->addItem( "dRA(p.c.)", dra);
            sourceArea1->addItem( "DEC", dec);
            sourceArea1->addItem( "dDEC(p.c.)", ddec);

            RtHBoxPtr line2(new RtHBox( "line 2" ));
            line2->add( sourceArea1 );
            RtSpringPtr spring6(new RtSpring(5.0));
            line2->add( spring6 );
            verticalSourceBox->add(line2);


            if ( saNo < ENG2 ) {
                // Sci1, Sci2, Eng1 have independent LOs, so could
                // have doppler. Eng2 does not.
                RtAreaPtr sourceArea2(new RtArea( "source info 2nd line" ));
                sourceArea2->addItem( "Doppler Src", dopplerSource);
                sourceArea2->addItem( "Catalog Vel", vel, "km/s");
                sourceArea2->addItem( "Doppler Vel", dopplerVel, "km/s");
                sourceArea2->addItem( "LO1", loFreq, "GHz" );
                RtHBoxPtr line2a(new RtHBox("line 2a"));
                line2a->add( sourceArea2 );
                line2a->add(RtSpringPtr(new RtSpring(5.0)));

                verticalSourceBox->add(line2a);

                RtAreaPtr weatherArea(new RtArea( "weather info" ));
                weatherArea->addItem( "T(amb)", atmTemp, "C" );
                weatherArea->addItem( "", atmTempF, "F" );
                weatherArea->addItem( "RH", relhumid, "%" );
                //weatherArea->addItem( "rho(H2O)", h2odensity, "gm/m^3" );
                weatherArea->addItem( "pwv(H2O)", h2omm, "mm" );
                const PhaseMonitorSubsystem & pms = display->cms().phaseMonitor();
                MonitorCellPtr skyRms = MonitorCell::makeCell( 4, pms.skyRMS() );
                weatherArea->addItem( "skyRMS", skyRms, "um" );
                const OpacityMonitorSubsystem & oms = display->cms().opacityMonitor();
                MonitorCellPtr tau225 = MonitorCell::makeCell( 4, oms.tau225() );
                weatherArea->addItem( "Tau225", tau225 );
                RtHBoxPtr line3(new RtHBox("weather"));
                line3->add( weatherArea );
                RtSpringPtr wspring(new RtSpring(5.0));
                line3->add( wspring );
                verticalSourceBox->add( line3 );

                //auto_ptr<PipelineSubsystem> SLpl = auto_ptr<PipelineSubsystem>(
                //        new PipelineSubsystemSL( display->cms( ).slPipeline( ) ) );
                //auto_ptr<PipelineSubsystem> WBpl = auto_ptr<PipelineSubsystem>(
                //        new PipelineSubsystemWB( display->cms( ).wbPipeline( ) ) );
                //MonitorCell * pctCaughtSL = MonitorCell::makeCell(6, SLpl->getCatchDataStage().pctCaughtTotal() );
                //MonitorCell * pctCaughtWB = MonitorCell::makeCell(6, WBpl->getCatchDataStage().pctCaughtTotal() );

                // Since we can now mark astrobands off line, the corrmode
                // cell should go on both science subarrays
                // @TODO RETHINK THIS LAYOUT!
                if ( saNo == SCI1 || saNo == SCI2 ) {
                    // add the spectral line correlator mode
                    CellPtr sModeCell(new CorrModeCell( 70, slc.modeDesc() ));
                    CellPtr wModeCell(new CorrModeCell( 70, wbc.modeDesc() ));
                    CellPtr c3gmax8ModeCell(new CorrModeCell( 70, c3gmax8.modeDesc() ));
                    CellPtr c3gmax23ModeCell(new CorrModeCell( 70, c3gmax23.modeDesc() ));

                    if(display->cms().signalPath().mapping().subarray(saNo - 1).CORRELATOR_DESIGNATION_MP().isValid()) {

                        CorrelatorSet corrSet(display->cms().signalPath().mapping().subarray(saNo - 1).CORRELATOR_DESIGNATION_MP().getValue());

                        if(corrSet.includesSpectral()) {
                            RtAreaPtr sourceAreaCorrS(new RtArea( "correlator info line" ));
                            sourceAreaCorrS->addItem( "Spectral CorrMode ", sModeCell );
                            RtHBoxPtr lineCorrS(new RtHBox("line corrS"));
                            lineCorrS->add( sourceAreaCorrS );
                            lineCorrS->add(RtSpringPtr(new RtSpring(5.0)));
                            verticalSourceBox->add(lineCorrS);
                        }

                        if(corrSet.includesWideband()) {
                            RtAreaPtr sourceAreaCorrW(new RtArea( "correlator info line" ));
                            sourceAreaCorrW->addItem( "Wideband CorrMode", wModeCell );
                            RtHBoxPtr lineCorrW(new RtHBox("line corrW"));
                            lineCorrW->add( sourceAreaCorrW );
                            lineCorrW->add(RtSpringPtr(new RtSpring(5.0)));
                            verticalSourceBox->add(lineCorrW);
                        }

                        if(corrSet.includesC3gMax8()) {
                            RtAreaPtr sourceAreaCorrC3gMax8(new RtArea( "correlator info line" ));
                            sourceAreaCorrC3gMax8->addItem( "C3GMax8 CorrMode", c3gmax8ModeCell );
                            RtHBoxPtr lineCorrC3gMax8(new RtHBox("line corrC3gmax8"));
                            lineCorrC3gMax8->add( sourceAreaCorrC3gMax8 );
                            lineCorrC3gMax8->add(RtSpringPtr(new RtSpring(5.0)));
                            verticalSourceBox->add(lineCorrC3gMax8 );
                        }

                        if(corrSet.includesC3gMax23()) {
                            RtAreaPtr sourceAreaCorrC3gMax23(new RtArea( "correlator info line" ));
                            sourceAreaCorrC3gMax23->addItem( "C3GMax23 CorrMode", c3gmax8ModeCell );
                            RtHBoxPtr lineCorrC3gMax23(new RtHBox("line corrC3gmax23"));
                            lineCorrC3gMax23->add( sourceAreaCorrC3gMax23 );
                            lineCorrC3gMax23->add(RtSpringPtr(new RtSpring(5.0)));
                            verticalSourceBox->add(lineCorrC3gMax23 );
                        }
                    }
                }


            }
        } else {
            CellPtr availableAnts(new CellString(
                    MEDIUM_FORMAT.c_str(),
                    display->saAntNameListRef ( saNo - 1).current,
                    WHITE_CELL_COLOR
                    ));
            availableAnts->setValidity(true);
            RtAreaPtr avail(new RtArea("offline ants"));
            avail->addItem( "OFFLINE Antennas:",availableAnts);
            RtHBoxPtr offlineBox(new RtHBox("offline box "));
            offlineBox->add( avail );
            offlineBox->add(RtSpringPtr(new RtSpring( 5.0 )));
            verticalSourceBox->add( offlineBox );
        }


        ///
        // Cells common only to Sci subarrays
        ///
        if ( saNo == SCI1 || saNo == SCI2 ) {
            IntegratorStage * isc = 0;
            IntegratorStage * isc2 = 0;
            IntegratorStage * isc3 = 0;

            if(display->cms().signalPath().mapping().subarray(saNo - 1).CORRELATOR_DESIGNATION_MP().isValid()) {

                CorrelatorSet corrSet(display->cms().signalPath().mapping().subarray(saNo - 1).CORRELATOR_DESIGNATION_MP().getValue());

                if(corrSet.includesSpectral()) {
                    const SlPipelineSubsystem & slpc = display->cms().slPipeline();
                    isc = &(slpc.integratorStageContainer().integratorStage());
                    MonitorCellPtr integrationNo = MonitorCell::makeCell( 10, isc->integrationNumber());
                    // requested integration time
                    MonitorCellPtr reqIntTime = MonitorCell::makeCell( 10, isc->desiredIntegTime());
                    // number of requested records
                    MonitorCellPtr numRecs = MonitorCell::makeCell( 10, isc->numberOfRecords());
                    // number of records completed
                    MonitorCellPtr elapsedRecs = MonitorCell::makeCell( 10, isc->recordCount());
                    // are we integrating?
                    MonitorCellPtr integrating = MonitorCell::makeCell( 10, isc->integrating());
                    integrating->setGoodColor(GREEN_CELL_COLOR);
                    integrating->setWarnColor(YELLOW_CELL_COLOR);
                    integrating->setErrorColor(WHITE_CELL_COLOR);

                    RtAreaPtr integArea(new RtArea("integration info"));
                    integArea->addItem("SL Integrating", integrating);
                    integArea->addItem("Int #", integrationNo);
                    integArea->addItem("Int Time", reqIntTime);
                    integArea->addItem("Record", elapsedRecs);
                    integArea->addItem("of", numRecs);

                    RtHBoxPtr line4(new RtHBox("line 4"));
                    line4->add( integArea );
                    line4->add(RtSpringPtr(new RtSpring(5.0)));
                    verticalSourceBox->add(line4);
                }

                if(corrSet.includesWideband()) {
                    const WbPipelineSubsystem & wbpc = display->cms().wbPipeline();
                    isc2 = &(wbpc.integratorStageContainer().integratorStage());

                    MonitorCellPtr integrationNo2 = MonitorCell::makeCell( 10, isc2->integrationNumber());
                    // requested integration time
                    MonitorCellPtr reqIntTime2 = MonitorCell::makeCell( 10, isc2->desiredIntegTime());
                    // number of requested records
                    MonitorCellPtr numRecs2 = MonitorCell::makeCell( 10, isc2->numberOfRecords());
                    // number of records completed
                    MonitorCellPtr elapsedRecs2 = MonitorCell::makeCell( 10, isc2->recordCount());
                    // are we integrating?
                    MonitorCellPtr integrating2 = MonitorCell::makeCell( 10, isc2->integrating());
                    integrating2->setGoodColor(GREEN_CELL_COLOR);
                    integrating2->setWarnColor(YELLOW_CELL_COLOR);
                    integrating2->setErrorColor(WHITE_CELL_COLOR);

                    RtAreaPtr integArea2(new RtArea("integration info"));
                    integArea2->addItem("WB Integrating", integrating2);
                    integArea2->addItem("Int #", integrationNo2);
                    integArea2->addItem("Int Time", reqIntTime2);
                    integArea2->addItem("Record", elapsedRecs2);
                    integArea2->addItem("of", numRecs2);

                    RtHBoxPtr line45(new RtHBox("line 4.5"));
                    line45->add( integArea2 );
                    line45->add(RtSpringPtr(new RtSpring(5.0)));
                    verticalSourceBox->add(line45);
                }
                if(corrSet.includesC3gMax8()) {
                    const C3gMax8PipelineSubsystem & wbpc = display->cms().c3gMax8Pipeline();
                    isc3 = &(wbpc.integratorStageContainer().integratorStage());

                    MonitorCellPtr integrationNo2 = MonitorCell::makeCell( 10, isc3->integrationNumber());
                    // requested integration time
                    MonitorCellPtr reqIntTime2 = MonitorCell::makeCell( 10, isc3->desiredIntegTime());
                    // number of requested records
                    MonitorCellPtr numRecs2 = MonitorCell::makeCell( 10, isc3->numberOfRecords());
                    // number of records completed
                    MonitorCellPtr elapsedRecs2 = MonitorCell::makeCell( 10, isc3->recordCount());
                    // are we integrating?
                    MonitorCellPtr integrating2 = MonitorCell::makeCell( 10, isc3->integrating());
                    integrating2->setGoodColor(GREEN_CELL_COLOR);
                    integrating2->setWarnColor(YELLOW_CELL_COLOR);
                    integrating2->setErrorColor(WHITE_CELL_COLOR);

                    RtAreaPtr integArea2(new RtArea("integration info"));
                    integArea2->addItem("C3G-8 Integrating", integrating2);
                    integArea2->addItem("Int #", integrationNo2);
                    integArea2->addItem("Int Time", reqIntTime2);
                    integArea2->addItem("Record", elapsedRecs2);
                    integArea2->addItem("of", numRecs2);

                    RtHBoxPtr line45(new RtHBox("line 4.5"));
                    line45->add( integArea2 );
                    line45->add(RtSpringPtr(new RtSpring(5.0)));
                    verticalSourceBox->add(line45);
                }
                if(corrSet.includesC3gMax23()) {
                    const C3gMax23PipelineSubsystem & wbpc = display->cms().c3gMax23Pipeline();
                    isc3 = &(wbpc.integratorStageContainer().integratorStage());

                    MonitorCellPtr integrationNo2 = MonitorCell::makeCell( 10, isc3->integrationNumber());
                    // requested integration time
                    MonitorCellPtr reqIntTime2 = MonitorCell::makeCell( 10, isc3->desiredIntegTime());
                    // number of requested records
                    MonitorCellPtr numRecs2 = MonitorCell::makeCell( 10, isc3->numberOfRecords());
                    // number of records completed
                    MonitorCellPtr elapsedRecs2 = MonitorCell::makeCell( 10, isc3->recordCount());
                    // are we integrating?
                    MonitorCellPtr integrating2 = MonitorCell::makeCell( 10, isc3->integrating());
                    integrating2->setGoodColor(GREEN_CELL_COLOR);
                    integrating2->setWarnColor(YELLOW_CELL_COLOR);
                    integrating2->setErrorColor(WHITE_CELL_COLOR);

                    RtAreaPtr integArea2(new RtArea("integration info"));
                    integArea2->addItem("C3G-23 Integrating", integrating2);
                    integArea2->addItem("Int #", integrationNo2);
                    integArea2->addItem("Int Time", reqIntTime2);
                    integArea2->addItem("Record", elapsedRecs2);
                    integArea2->addItem("of", numRecs2);

                    RtHBoxPtr line45(new RtHBox("line 4.5"));
                    line45->add( integArea2 );
                    line45->add(RtSpringPtr(new RtSpring(5.0)));
                    verticalSourceBox->add(line45);
                }
            }
        }

        // add relevant alarm cells and make set their color schemes
        const AlarmSubsystem & alarm = display->cms().alarm();
        const FaultSubsystem & fault = display->cms().fault();
        RtHBoxPtr alarmBox(new RtHBox("alarm cells"));
        RtAreaPtr alarmArea(new RtArea("alarm"));
        MonitorCellPtr alarmEnable = MonitorCell::makeCell( fault.subarray(saNo - 1).alarmEnable() );
        MonitorCellPtr alarmOn     = MonitorCell::makeCell( alarm.alarmOn() );

        alarmOn->setGoodColor( GREEN_CELL_COLOR );
        alarmOn->setWarnColor( RED_CELL_COLOR );
        alarmOn->setErrorColor( RED_CELL_COLOR );

        alarmEnable->setGoodColor( GREEN_CELL_COLOR );
        alarmEnable->setWarnColor( RED_CELL_COLOR );
        alarmEnable->setErrorColor( RED_CELL_COLOR );


        if ( saNo < ENG1 ) {
            typedef CM::NoiseStatusMonitorPointEnum NoiseStatusEnum;

            NoiseStatusEnum * noiseStatus = 0;
            NoiseStatusEnum * noiseStatus2 = 0;

            if(display->cms().signalPath().mapping().subarray(saNo - 1).CORRELATOR_DESIGNATION_MP().isValid()) {

                CorrelatorSet corrSet(display->cms().signalPath().mapping().subarray(saNo - 1).CORRELATOR_DESIGNATION_MP().getValue());

                if(corrSet.includesSpectral()) {
                    noiseStatus = &( display->cms().
                            sldc().noiseSourceContainer().noiseSource().noiseStatus() );

                    MonitorCellPtr noiseState = MonitorCell::makeCell( *noiseStatus );
                    alarmArea->addItem("SL Noise Source", noiseState);

                }

                if(corrSet.includesWideband()) {
                    noiseStatus2 = &( display->cms().
                            wbdc().noiseSourceContainer().noiseSource().noiseStatus() );
                    MonitorCellPtr noiseState2 = MonitorCell::makeCell( *noiseStatus2 );
                    alarmArea->addItem("WB Noise Source", noiseState2);
                }

            }
        }

        alarmArea->addItem("Alarm Enable", alarmEnable);
        alarmArea->addItem("Alarm On", alarmOn);

        if ( saNo < ENG1 ) {
            MonitorCellPtr alarm1mm    = MonitorCell::makeCell( sa.alarm1mm() );
            alarm1mm->setGoodColor( GREEN_CELL_COLOR );
            alarm1mm->setWarnColor( ORANGE_CELL_COLOR );
            alarm1mm->setErrorColor( ORANGE_CELL_COLOR );
            alarmArea->addItem("1mm Alarm", alarm1mm);
        }

        alarmBox->add( alarmArea );
        alarmBox->add(RtSpringPtr(new RtSpring(5.0)));
        verticalSourceBox->add( alarmBox );

        RtHBoxPtr processes(new RtHBox("miscellaneous "));
        RtAreaPtr pArea(new RtArea("various"));

        const ImrSubsystem & mon = display->cms().imr();
        pArea->addItem( "Critical Processes NOT Running",
                MonitorCell::makeCell( mon.numNotRunningCriticalServers( ) )
                );
        pArea->addItem( "Processes NOT Running",
                MonitorCell::makeCell( mon.numNotRunningServers( ) )
                );
        processes->add( pArea );
        processes->add(RtSpringPtr(new RtSpring(5.0)));
        verticalSourceBox->add( processes );


        RtHBoxPtr commentBox(new RtHBox("obs comments"));
        RtAreaPtr commentArea(new RtArea("comment"));
        // use persistent version of comment monitor point
        MonitorCellPtr commentCell = MonitorCell::makeCell( 90, sa.notice() );
        commentArea->addItem("Comment", commentCell);
        commentBox->add( commentArea );
        commentBox->add(RtSpringPtr(new RtSpring(5.0)));
        verticalSourceBox->add( commentBox );

        //squish it up.
        verticalSourceBox->add(RtSpringPtr(new RtSpring(5.0)));

        // @todo add "last command", but need implementation in control first.

        RtHBoxPtr topBox(new RtHBox("top info"));
        // 2 pixel border all sides
        topBox->setBorder(ONE_PIXEL_BELOW_BORDER);
        topBox->add( verticalSourceBox );


        /*
           os.str("");
           os << "Exiting makeSourceInfoBox "
           << " Subarray #"<<saNo;
           programLogNoticeIfPossible( os.str() );
           */

        return topBox;
    } catch ( ... ) {
        programLogErrorIfPossible("caught error in makeSourceInfoBox");
        throw;
    }
}

carma::ui::rtd::RtBoxPtr
carma::ui::rtd::makeAntTableBox(const int saNo,
        CompositeSubarrayDisplay* display,
        const SubarrayStatus* saStatus)
{

    try {
        /*
           ostringstream os;
           os << "Entering makeAntTableBox "
           << " Subarray #"<<saNo;
           programLogNoticeIfPossible( os.str() );
           */
        vector<string> columnLabel;
        vector<string> blLabel;
        // Vector of monitorContainers, one for each table column, each containing
        // refs to all MPs in the column.
        vector<MonitorContainer*> container;
        RtHBoxPtr middleBox(new RtHBox("middle info"));
        middleBox->setBorder(ONE_PIXEL_BELOW_BORDER);
        const ControlSubsystem& ac = display->cms().control();
        const ControlSubsystemBase::Subarray & sa = ac.subarray(saNo - 1);
        int nants = sa.numberOfAntennas().getValue();

        if ( nants == 0 ) {
            CellPtr emptyTable(new CellString( NO_ANT_FORMAT.c_str(),
                    NO_ANTENNAS,
                    YELLOW_CELL_COLOR ));
            emptyTable->setValidity( true );
            RtSpringPtr spring1(new RtSpring( 5.0 ));
            RtSpringPtr spring2(new RtSpring( 5.0 ));
            middleBox->add ( spring1 );
            middleBox->add (emptyTable);
            middleBox->add ( spring2 );

            return  middleBox;
        }


        // Add zebra striping
        ZebraVisitorPtr zebraVisitor(new ZebraVisitor());

        const string dtemp("Dewar Temp");
        const string arcmin("arcmin");
        const string arcsec("arcsec");
        const string kelvin("K");

        for (int i=0; i < nants ; i++) {
            CompositeSubarrayDisplay::AntNameInfo aInfo
                = display->saAntNameInfoRef( saNo - 1, i );

            const string antName = aInfo.shortName;
            const int antNo      = aInfo.carmaAntNo;
            const int antIndex   = antNo - 1;
            columnLabel.push_back( antName );
            MonitorContainer* theColumn = new MonitorContainer( antName );

            const AntennaCommon& antCommon =
                display->cms().antennaCommon( antIndex );

            theColumn->add( antCommon.initialized() );

            if ( saNo <= SCI2 ) {
                const AstroSubsystem & astro = display->cms().astro();
                theColumn->add( astro.antenna( antIndex ).maxCoherence( ) );
            }

            theColumn->add( antCommon.drive().state() );

            // use sky error for azimuth, not coordinate error
            const AntennaCommon::Track& track = antCommon.drive().track();
            MonitorPointDouble& mpactaz = track.actualAzimuth();
            MonitorPointDouble& mpactel = track.actualElevation();
            mpactaz.setPrecision(1);
            mpactel.setPrecision(1);
            mpactaz.setWidth(6);
            mpactel.setWidth(6);
            theColumn->add(mpactaz);
            theColumn->add(mpactel);
            MonitorPointFloat& mpa = track.errorAzimuthSky() ;
            //mpa.setUnits(arcsec);
            theColumn->add( mpa );
            MonitorPointFloat & mpe = track.errorElevation() ;
            //mpe.setUnits(arcsec);
            theColumn->add( mpe );
            theColumn->add( antCommon.receivers().rxState() );

            // add the dewar temperatures from antenna-specific
            // monitor points. make sure they share a common name
            // and unit.
            MonitorPointDouble& mpdewar = antCommon.receivers().dewarTemp();
            theColumn->add( mpdewar );

            //System temperatures, amplitudes, phases, and SNR
            if ( saNo <= SCI2 ) {

                const AstroSubsystem & astro = display->cms().astro();

                theColumn->add( astro.antenna( antIndex ).medianTdsb( ) );
                theColumn->add( astro.antenna( antIndex ).medianAmp( ) );
                theColumn->add( astro.antenna( antIndex ).medianSnr( ) );
                theColumn->add( astro.antenna( antIndex ).shadowed( ) );
            }

            theColumn->add( ac.antenna(antIndex).padNumber() );
            //theColumn->add( antCommon.calibrator().calState() );

            zebraVisitor->addMonitorContainer(*theColumn);
            container.push_back( theColumn );
        }

        // tighten up columns for CARMA23 display
        const int colWidth = 8;
        //hierarchy depth 1, average samples, use units
        const bool useUnits = true;
        const int hierarchy = 1;
        const int sampleNo  = 0;
        MonitorTablePtr statusTable = MonitorTable::makeTable(
                    columnLabel, container,
                    colWidth, hierarchy, sampleNo , useUnits,
                    false, zebraVisitor);

        // Turn on audio on all  dewar  temperature cells
        const string plate( "plate4kTemp");
        const string stage3( "stage3");
        int numContainers = container.size();
        int cellCount = 0;
        for (int di=0; di< numContainers; di++ ) {
            MonitorContainer * mc = container.at(di);
            MonitorPointIterator mpi(*mc, 1 );
            while( mpi++ ) {
                const string name = mpi.getMonitorPoint().getName();
                // the MP container and tables both count down columns and
                // across rows, so this cellCount indexing works.
                Cell& c = statusTable->getCell(cellCount);
                if (name == plate || name == stage3 ) c.setAudio('E');
                cellCount++;
            }
        }

        // Replace tracking error cells with new that use other cell for bckgrnd clr
        MonitorContainer* mc = container.at(0);
        int numRows = statusTable->getNumRows();
        MonitorPointIterator mpi(*mc, 1);
        int azerrIndex = 0;
        while( mpi++ ) {
            const string name = mpi.getMonitorPoint().getName();
            if (name == "errorAzimuthSky") break;
            azerrIndex++;
        }
        int elerrIndex = 0;
        MonitorPointIterator mpit(*mc, 1);
        while( mpit++ ) {
            const string name = mpit.getMonitorPoint().getName();
            if (name == "errorElevation") break;
            elerrIndex++;
        }
        for (int i=0; i < nants ; i++) {
            CompositeSubarrayDisplay::AntNameInfo aInfo =
                display->saAntNameInfoRef( saNo - 1, i );
            const int antNo      = aInfo.carmaAntNo;
            const int antIdx     = antNo - 1;
            const AntennaCommon& antCommon = display->cms().antennaCommon(antIdx);
            const AntennaCommon::Track& track = antCommon.drive().track();
            MonitorPointFloat& mpa      = track.errorAzimuthSky() ;
            MonitorPoint&      mpastate = track.azimuthAxisState() ;
            MonitorPointFloat& mpe      = track.errorElevation() ;
            MonitorPoint&      mpestate = track.elevationAxisState() ;
            CellPtr ca = MonitorCell::makeCell(mpa, mpastate);
            statusTable->replaceCell(i*numRows+azerrIndex, ca);
            CellPtr ce = MonitorCell::makeCell(mpe, mpestate);
            statusTable->replaceCell(i*numRows+elerrIndex, ce);
        }

        const string saName =
            ControlSubsystem::getSubarrayAlphanumericName( saNo );
        ostringstream tableTitle;
        tableTitle << saName << " Antenna Status";
        statusTable->setTitle( tableTitle.str() );
        middleBox->add( statusTable );
        /*
           os.str("");
           os << "Exiting makeAntTableBox "
           << " Subarray #"<<saNo;
           programLogNoticeIfPossible( os.str() );
           */
        return middleBox;
    } catch ( const BaseException & bex ) {
        programLogErrorIfPossible("caught BASE_EXCEPTION in makeAntTableBox:");
        programLogErrorIfPossible( bex.what() );
        throw;
    } catch ( const std::exception & ex ) {
        programLogErrorIfPossible("caught STD_EXCEPTION in makeAntTableBox:");
        programLogErrorIfPossible( ex.what() );
        throw;
    } catch ( ... ) {
        programLogErrorIfPossible("caught UNKNOWN error in makeAntTableBox");
        throw;
    }
}

