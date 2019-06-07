
/*
 * 
 * Display status of the LO and spectral correlator setup.
 *
 * @author Original: Steve Scott 
 * $Id: rtdspecsetup.cc,v 1.25 2014/06/04 17:09:50 mpound Exp $
 *
 * $CarmaCopyright$
 */


#include <sstream>
#include <vector>

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/CorrModeCell.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/util/CorrelatorSet.h"
#include "carma/util/Program.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/StringUtils.h"
//#include "carma/util/programLogging.h"


using namespace std;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui::rtd;
   

namespace { 

template < typename CorrMonContainer >
void setupBoxes( RtVBoxPtr topbox,
                 RtVBoxPtr botbox,
                 RtFolderPtr folder,
                 const CorrMonContainer & cor )
{
    topbox->setBorder(ONE_PIXEL_BELOW_BORDER);
    folder->add(topbox);
    folder->add(botbox);
    RtLabelPtr tblab(new RtLabel("LO1 Setup"));
    tblab->setRelFontSize(4);
    topbox->add(tblab);
    RtLabelPtr bblab(new RtLabel("Band Setups"));
    bblab->setRelFontSize(4);
    botbox->add(bblab);
    RtHBoxPtr modeBox (new RtHBox( "mode description" ));
    RtAreaPtr modeArea(new RtArea( "mode Area" ));

    const int numBands = cor.numBands().getValue();
    const int modeCellWidth = numBands * 7;
    CellPtr modeCell(new CorrModeCell( modeCellWidth, cor.modeDesc() ));
    modeArea->addItem( "CorrMode", modeCell );
    modeBox->add( modeArea );
    botbox->add( modeBox );
}

template < typename CorrMonContainer >
MonitorContainer * 
createLoSetupMonitorContainer( const  CorrMonContainer & corrMonContainer, const MonitorDisplay & display )
{
    MonitorPoint * mp = &corrMonContainer.controllingSubarray();
    string subarray = mp->getAverageToString();
    const string SCI1("Sci#1");
    const string SCI2("Sci#2");
    int subarrayIndex = -1;
    if (subarray.compare(SCI1) == 0 ) 
        subarrayIndex = 0;
    else if ( subarray.compare(SCI2) == 0 ) 
        subarrayIndex = 1;

    MonitorContainer * loSetup = new MonitorContainer("losetup"); 
    loSetup->add(*mp);

    if ( subarrayIndex != -1 ) {
        ControlSubsystem::Subarray & sa = display.cms().control().subarray(subarrayIndex);

        MonitorPoint * m = 0;

        m = &sa.loFreq(); 
        m->setWidth(10);
        m->setPrecision(6);
        loSetup->add(*m); 
        
        const ControlSubsystem::Freq & fcmd = sa.commands().freq();

        m = &fcmd.restFreq(); 
        m->setWidth(10);
        m->setPrecision(6);
        loSetup->add(*m); 

        loSetup->add(fcmd.sideband()); 

        m = &fcmd.ifFreq(); 
        m->setWidth(10);
        m->setPrecision(6);
        loSetup->add(*m); 

        loSetup->add(sa.dopplerSource()); 

        m = &sa.velocity(); 
        m->setWidth(8);
        m->setPrecision(2);
        loSetup->add(*m); 

        loSetup->add(sa.velFrame()); 

        m = &sa.velObservatory(); 
        m->setWidth(8);
        m->setPrecision(2);
        loSetup->add(*m); 
    }

    return loSetup;
}

MonitorContainer *
createBandMonitorContainer( const ControlBandPoints & cbp, carma::util::CorrelatorSet corrset )
{
    MonitorContainer* band = new MonitorContainer("Band");  
    
    MonitorPoint* m = 0;

    band->add(cbp.configuration());
    band->add(cbp.online());
    band->add(cbp.astrobandConf());
    band->add(cbp.fpgaMode());
    m = &cbp.lo2Freq();
    m->setPrecision(6);
    m->setWidth(8);
    band->add(*m);
    band->add(cbp.lo2Sideband());
    m = &cbp.ifFreq();
    m->setPrecision(6);
    m->setWidth(8);
    band->add(*m);
    m = &cbp.bandwidth();
    m->setPrecision(1);
    m->setWidth(6);
    band->add(*m);
    band->add( cbp.corrBits() );
    if ( ! corrset.isC3g() ) {
        band->add( cbp.bdcEnabled() );
        band->add(cbp.blockDCpolarization());
    }
    band->add(cbp.decimation());
    band->add(cbp.keepEndChans());
    m = &cbp.centerFreq();
    m->setPrecision(6);
    m->setWidth(10);
    band->add(*m);
    for (int s=0; s<2; s++) {
        ControlBandPoints::Sideband& sb = cbp.sideband(s);
        string  sbstring = (s==1 ? "LSB" : "USB");
        m = &sb.bandCenter().skyFreq();
        m->setShortName(sbstring + " SkyFreq");
        m->setPrecision(6);
        m->setWidth(10);
        band->add(*m);
        m = &sb.restFreq();
        m->setShortName(sbstring + " RestFreq");
        m->setPrecision(6);
        m->setWidth(10);
        band->add(*m);
        m = &sb.transition();
        m->setShortName(sbstring + " Transition");
        m->setWidth(10);
        band->add(*m);
    }
    return band;
}

void
populateBandMonitorContainerVec( vector<MonitorContainer*> & container,
                                 const ControlSubsystem::SpectralLineCorrelator & slc )
{
    const int nBands = slc.numBands().getValue();
    carma::util::CorrelatorSet corrset(carma::util::CORR_SPECTRAL);
    for (int b=0; b<nBands; ++b) {  
        const ControlBandPoints & cbp = slc.slcBand(b).controlBandPoints();
        MonitorContainer * band = createBandMonitorContainer(cbp,corrset);
        container.push_back(band);
    }
}

void
populateBandMonitorContainerVec( vector<MonitorContainer*> & container,
                                 const ControlSubsystem::WidebandCorrelator & wbc )
{
    const int nBands = wbc.numBands().getValue();
    carma::util::CorrelatorSet corrset(carma::util::CORR_WIDEBAND);
    for (int b=0; b<nBands; ++b) {  
        const ControlBandPoints & cbp = wbc.wbcBand(b).controlBandPoints();
        MonitorContainer * band = createBandMonitorContainer(cbp,corrset);
        container.push_back(band);
    }
}

void
populateBandMonitorContainerVec( vector<MonitorContainer*> & container,
                                 const ControlSubsystem::C3gMax8Correlator & corr)
{ 
    const int nBands = corr.numBands().getValue();
    carma::util::CorrelatorSet corrset(carma::util::CORR_C3GMAX8);
    for (int b=0; b<nBands; ++b) {  
        const ControlBandPoints & cbp = corr.c3gMax8Band(b).controlBandPoints();
        MonitorContainer * band = createBandMonitorContainer(cbp,corrset);
        container.push_back(band);
    }
}

void
populateBandMonitorContainerVec( vector<MonitorContainer*> & container,
                                 const ControlSubsystem::C3gMax23Correlator & corr)
{
    const int nBands = corr.numBands().getValue();
    carma::util::CorrelatorSet corrset(carma::util::CORR_C3GMAX23);
    for (int b=0; b<nBands; ++b) {  
        const ControlBandPoints & cbp = corr.c3gMax23Band(b).controlBandPoints();
        MonitorContainer * band = createBandMonitorContainer(cbp,corrset);
        container.push_back(band);
    }
}
    
    
template < typename CorrMonContainer >
void
populateColumnLabels( vector<string> & columnLabel,
                      const CorrMonContainer & corrMon ) 
{
    int startBand;
    const int nBands = corrMon.numBands().getValue();
    string name = corrMon.name().getValue();
    if ( StringUtils::equalsIgnoreCase(name,"SpectralLine") )
        startBand=1;
    else if ( StringUtils::equalsIgnoreCase(name,"Wideband") )
        startBand=9;
    else if ( StringUtils::equalsIgnoreCase(name,"C3gMax23") )
        startBand=25;
    else if ( StringUtils::equalsIgnoreCase(name,"C3gMax8") )
        startBand=33;
    else 
        startBand=99; //error!
    
    for (int i=startBand; i < (nBands+startBand); i++) {
        ostringstream o;
        o << "Band#" << i;
        columnLabel.push_back(o.str());
    }
}

string makeHelp() {
    ostringstream ost;
    ost
    << "       CORRELATOR and LO Setup\n\n" 
    << "The rest, sky, and IF frequencies are for the center of the band.\n"
    << "The transition and the transition rest frequency are header "
    << "labels that are used to aid display and reduction software. "
    ;    
    return ost.str();

}

template < typename CorrMonContainer >
void 
runDisplay( const CorrMonContainer & corrMonContainer,
            MonitorDisplay & display )
{
    display.cms().readNewest();
    display.setSpecificHelp("Correlator/LO Help", makeHelp());

    RtFolderPtr folder(new RtFolder("main"));
    display.add(folder);
    
    RtVBoxPtr topbox(new RtVBox("topbox"));
    RtVBoxPtr botbox(new RtVBox("botbox"));

    setupBoxes( topbox, botbox, folder, corrMonContainer );
     
    MonitorContainer * loSetup 
        = createLoSetupMonitorContainer( corrMonContainer, display ); 
    MonitorTablePtr losetupTable = MonitorTable::makeTable( string(""), *loSetup,
                                                          AUTO_SIZE, 1, 
                                                          0, true, false, MonitorTableVisitorPtr());
    topbox->add(losetupTable);
    
    vector<string> columnLabels ;
    populateColumnLabels( columnLabels, corrMonContainer );
    
    // Create Band Specific Table
    vector<MonitorContainer*> container1; 
    populateBandMonitorContainerVec( container1, corrMonContainer );
    MonitorTablePtr table = MonitorTable::makeTable( columnLabels, container1,
                                                   AUTO_SIZE, 1, 
                                                   0, true, false, MonitorTableVisitorPtr());
                 
    // Add table to the folder
    botbox->add(table);
    folder->add(RtSpringPtr(new RtSpring( 4, 1.0 )));

    //string subarrayString = corrMonContainer.controllingSubarray().getAverageToString();
    //string subarrayStringLatest;
    //bool needsReinitialization = false;
    //MonitorTablePtr newObj;
    while (display.serveData( )){}
    //while (display.serveData( needsReinitialization )) {
        /** Some sort of concurrency exception if I use this code...
        subarrayStringLatest = 
            corrMonContainer.controllingSubarray().getAverageToString();

        needsReinitialization = 
            ( subarrayString.compare(subarrayStringLatest)!=0 );
        if ( needsReinitialization ) {
            { // debug
                ostringstream os;
                os << " MWP RTD subarrayString = "
                   << subarrayString
                   << " subarrayStringLatest = " << subarrayStringLatest
                   << " needs Reinit = " << boolalpha << needsReinitialization;
                programLogNotice(os.str());
            }
           display.cms().readNewest();
           loSetup = createLoSetupMonitorContainer( corrMonContainer, display ); 
           newObj = MonitorTable::makeTable( string(""), 
                   *loSetup,
                   AUTO_SIZE, 1, 
                   0, true, false, MonitorTableVisitorPtr());
           display.replace(losetupTable,newObj);
           losetupTable = newObj;
        }
    
        display.cms().readNewest();
        subarrayString = subarrayStringLatest;
        */
    //}
}


} // namespace unnamed

int Program::main() {    
    
    const string string1Param = getStringParameter( "string1" );
    util::CorrelatorType ctype = util::CORR_NONE;
    string corrTypeString;
    if ( StringUtils::equalsIgnoreCase(string1Param,"SL" ) ) {
            ctype = util::CORR_SPECTRAL;
            corrTypeString = "Spectral";
    }
    if ( StringUtils::equalsIgnoreCase(string1Param,"WB" ) ) {
            ctype = util::CORR_WIDEBAND;
            corrTypeString = "Wideband";
    }
    if ( StringUtils::equalsIgnoreCase(string1Param,"C3GMAX8" ) ) {
            ctype = util::CORR_C3GMAX8;
            corrTypeString = "C3G Max8";

    }
    if ( StringUtils::equalsIgnoreCase(string1Param,"C3GMAX23" ) ) {
            ctype = util::CORR_C3GMAX23;
            corrTypeString = "C3G Max23";
    }

    if ( ctype == util::CORR_NONE )
        throw CARMA_EXCEPTION( IllegalArgumentException, "Unrecognized correlator type, string1 must be sl, wb, c3gmax8, or c3gmax23" );

    // Create a dislay
    MonitorDisplay display( corrTypeString + "Correlator & LO Status");

    ControlSubsystem & control = display.cms().control();
    display.cms().readNewest();

    switch ( ctype ) {
        case util::CORR_SPECTRAL:
            {
                ControlSubsystem::SpectralLineCorrelator& corr 
                    = control.spectralLineCorrelator();
                runDisplay( corr, display );
                return EXIT_SUCCESS;
            }
        break;
        case util::CORR_WIDEBAND:
            {
                ControlSubsystem::WidebandCorrelator & corr 
                    = control.widebandCorrelator();
                runDisplay( corr, display );
                return EXIT_SUCCESS;
            }
        case util::CORR_C3GMAX8:
            {
                ControlSubsystem::C3gMax8Correlator & corr 
                    = control.c3gMax8Correlator();
                runDisplay( corr, display );
                return EXIT_SUCCESS;
            }
        break;
        case util::CORR_C3GMAX23:
            {
                ControlSubsystem::C3gMax23Correlator & corr 
                    = control.c3gMax23Correlator();
                runDisplay( corr, display );
                return EXIT_SUCCESS;
            }
        break;
        default:
            return EXIT_FAILURE;
            break;
    }
        
    return EXIT_FAILURE;
}
