/** @file
 * Retrieves and displays data for the spectral line correlator subsystems.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.21 $
 * $Date: 2014/06/20 15:55:47 $
 * $Id: rtdcorrelator.cc,v 1.21 2014/06/20 15:55:47 mpound Exp $
 * 
 * $CarmaCopyright$
 */

// C++ Standard Library includes
#include <iostream>
#include <sstream>
#include <vector>

// Carma includes
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/CarmaSlcBandSubsystem.h"
#include "carma/monitor/Carma3GSubsystem.h"
#include "carma/monitor/WbcBandSubsystem.h"
#include "carma/monitor/Carma3GSubsystem.h"
#include "carma/util//corrUtils.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/ui/rtd/common/ZebraVisitor.h"
#include "carma/ui/rtd/common/CompositeMonitorTableVisitor.h"
#include "carma/ui/rtd/common/DisplayTimeOnlyVisitor.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorTable.h"
#include "carma/ui/rtd/common/RtDisplay.h"


using namespace std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::ui;
using namespace carma::ui::rtd;
using namespace carma::util;
    
// Basic Window Layout
// ,-----------, ,------, ,------, ,---------, ,----------,
// | Status Tab| |Delays| |Common| |Digitizer| |Correlator|
// |--------------------------------------------------------+
// |                                                        |
// |                                                        |
// |   Status|Delays|Common|Digitizer|Correlator info       |
// |                                                        |
// |                                                        |
// |________________________________________________________|

namespace { // anonymous 

const int RELATIVE_LABEL_FONT_SIZE = 4; // 4 points larger than default
const int DEFAULT_COLUMN_WIDTH = 12; 
const int STATUS_COLUMN_WIDTH = 15; 

typedef enum {
    CORRELATOR,
    DIGITIZER
} SubsystemType;

string
makeHelp( void ) 
{
    ostringstream os;
    os <<"     SPECTRAL LINE CORRELATOR HELP\n\n";
    return os.str();
}

template < typename BandSubsystem >
string getHwTypeString( const BandSubsystem & bs )
{
    const string subsystemName = bs.getName();
    if ( subsystemName.find( "CarmaSlcBand" ) != string::npos )
        return "CARMA";
    if ( subsystemName.find( "WbcBand" ) != string::npos )
        return "COBRA";
    if ( subsystemName.find( "Carma3GBand" ) != string::npos )
        return "C3G";

    return "<INVALID HW>";
}

// -------------------
// Make status folder  - Templatized (generic) COBRA/CARMA SL and CARMA WB.
// -------------------

template < typename MC >
MonitorContainer* getHeaderMonitorContainer(MC &mon, const std::string &name)
{
    // intentionally leaks memory
    MonitorContainer *headerContainer = new MonitorContainer(name);

    // add all monitor points which are part of the header
    // to the virtual header container
    headerContainer->add(mon.received());
    headerContainer->add(mon.timestamp());
    headerContainer->add(mon.bandNumber());
    headerContainer->add(mon.mode());
    headerContainer->add(mon.sourceName());
    headerContainer->add(mon.numberOfPhaseSwitchesIntegrated());
    headerContainer->add(mon.phaseSwitchPeriod());
    headerContainer->add(mon.phaseSwitchIntegrationTime());
    headerContainer->add(mon.totalTimeSpan());
    headerContainer->add(mon.totalIntegrationTime());
    headerContainer->add(mon.bandwidth());
    headerContainer->add(mon.centerFrequency());
    headerContainer->add(mon.dconFrequency());
    headerContainer->add(mon.dconSideband());
    headerContainer->add(mon.lobeResidualFrequency());
    headerContainer->add(mon.lobeTrackSideband());
    headerContainer->add(mon.bdcEnabled());
    headerContainer->add(mon.scaling());
    headerContainer->add(mon.bitMode());
    headerContainer->add(mon.astroBandMode());
    headerContainer->add(mon.corrStatus());
    headerContainer->add(mon.corrSeqNo());
    headerContainer->add(mon.commandStatusBitPattern());
    headerContainer->add(mon.commandStatus());

    return headerContainer;
}

template < typename BandSubsystem >
RtFolderPtr
makeStatusFolder( BandSubsystem & bs, SubsystemType subsys, unsigned bandNo) 
{
    ostringstream label;

    RtLabelPtr statusLabel;
    RtFolderPtr statusFolder;
    MonitorTablePtr statusTable;

    ZebraVisitorPtr stripeVisitor(new ZebraVisitor());
    DisplayTimeOnlyVisitorPtr timeVisitor(new DisplayTimeOnlyVisitor());

    vector< ::std::string > columnNames;
    vector< MonitorContainer * > monitorHeaders;

    // First band info
    columnNames.push_back( "Band" );
    monitorHeaders.push_back( getHeaderMonitorContainer(bs, "Band") );
    timeVisitor->addToEffectedSet( bs.timestamp( ) );
    stripeVisitor->addMonitorContainer(monitorHeaders.back());

    const string hwTypeString = getHwTypeString( bs );
    
    switch (subsys) {
        case DIGITIZER:
            label << " Band " << bandNo  << " " << hwTypeString 
                  << " Digitizer Board Status ";
            statusLabel = RtLabelPtr(new RtLabel( label.str() ));
            statusFolder = RtFolderPtr(new RtFolder( hwTypeString + " Digitizer Status "));
            for ( int i = 0; i < bs.digitizerCount(); ++i ) {
                ::std::ostringstream os;
                os << "Dig" << ( i + 1 );
                columnNames.push_back( os.str() );
                monitorHeaders.push_back(getHeaderMonitorContainer(bs.digitizer(i), os.str()));
                timeVisitor->addToEffectedSet( bs.digitizer(i).timestamp( ) );
                stripeVisitor->addMonitorContainer(monitorHeaders.back());
            }
            break;
        case CORRELATOR:
            label << " Band " << bandNo << " " << hwTypeString 
                  << " Correlator Board Status ";
            statusLabel = RtLabelPtr(new RtLabel( label.str() ));
            statusFolder = RtFolderPtr(new RtFolder("Correlator Status"));
            for ( int i = 0; i < bs.correlatorCount(); ++i ) {
                ::std::ostringstream os;
                os << "Corr" << (i + 1);
                columnNames.push_back( os.str() );
                monitorHeaders.push_back(getHeaderMonitorContainer(bs.correlator(i), os.str()));
                timeVisitor->addToEffectedSet( bs.correlator(i).timestamp( ) );
                stripeVisitor->addMonitorContainer(monitorHeaders.back());
            } 
            break;
        default:
            ostringstream os;
            os << "1 Invalid subsystem type: " << subsys ;
            throw CARMA_EXCEPTION( ErrorException, os.str().c_str());
            break;
    }

    statusLabel->setRelFontSize(RELATIVE_LABEL_FONT_SIZE);

    vector < MonitorTableVisitorPtr > visitors;
    visitors.push_back( timeVisitor );
    visitors.push_back( stripeVisitor );

    CompositeMonitorTableVisitorPtr compositeVisitor(new CompositeMonitorTableVisitor(visitors));

    statusTable = MonitorTable::makeTable( 
                        columnNames,
                        monitorHeaders,
                        STATUS_COLUMN_WIDTH, 
                        1, 0, true, false, compositeVisitor );

    // Form up the status folder
    statusFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 2 ) ));
    statusFolder->add( statusLabel );
    statusFolder->add( RtSpacerPtr(new RtSpacer( 3, 5, 0 ) ));
    statusFolder->add( statusTable );
    statusFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 6 ) ));

    return statusFolder;

} // End makeStatusFolder - generic 

// -------------------
// Make delays folder  - COBRA
// -------------------
template <typename BandSubsystem>
RtFolderPtr
makeDelaysFolder( BandSubsystem & bs, unsigned bandNo )
{
    const string hwTypeString = getHwTypeString( bs );
    
    ostringstream label;
    label << hwTypeString << " Band " << bandNo << " Delays ";

    RtFolderPtr delaysFolder = RtFolderPtr(new RtFolder(" Delays "));
    RtLabelPtr delaysLabel = RtLabelPtr(new RtLabel( label.str() ));
    MonitorTablePtr delaysTable;

    DisplayTimeOnlyVisitorPtr timeVisitor(new DisplayTimeOnlyVisitor());
    ZebraVisitorPtr stripeVisitor(new ZebraVisitor());

    vector< ::std::string > columnNames;
    vector< MonitorContainer * > monitorDelays;
    
    delaysLabel->setRelFontSize( RELATIVE_LABEL_FONT_SIZE );

    // Form up columnNames and delays vectors.
    for ( int i = 0; i < bs.interpolatorSamplesCount(); ++i ) {

        const string empty("");
        columnNames.push_back( empty );
        
        CorMonitorInterpSamps & corMonitorInterpSamps =
            bs.interpolatorSamples(i);
        
        monitorDelays.push_back( &corMonitorInterpSamps );
        // Form up visitor
        for ( int j = 0; j < CorMonitorInterpSamps::inputDelaysCount(); ++j ) {
            timeVisitor->addToEffectedSet(
                        corMonitorInterpSamps.inputDelays(j).timestamp( ) );
        }
        stripeVisitor->addMonitorContainer(monitorDelays.back());
    }

    vector< MonitorTableVisitorPtr > visitors;
    visitors.push_back( timeVisitor );
    visitors.push_back( stripeVisitor );
    CompositeMonitorTableVisitorPtr compositeVisitor(new CompositeMonitorTableVisitor(visitors));

    delaysTable = MonitorTable::makeTable(
                                columnNames,
                                monitorDelays,
                                DEFAULT_COLUMN_WIDTH,
                                2, 0, true, true, compositeVisitor ); 

    // Build the folder
    delaysFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 2 ) ));
    delaysFolder->add( delaysLabel );
    delaysFolder->add( RtSpacerPtr(new RtSpacer( 3, 5, 0 ) ));
    delaysFolder->add( delaysTable );
    delaysFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 6 ) ));

    return delaysFolder;

} // End makeDelaysFolder.


// -------------------
// Make common folder  - Wideband & Cobra
// -------------------
template < typename BandSubsystem >
RtFolderPtr
makeCommonFolder( BandSubsystem & bs, SubsystemType subsys, unsigned bandNo ) 
{
    ostringstream label;
    RtFolderPtr commonFolder;
    RtLabelPtr commonLabel;
    MonitorTablePtr commonTable;
    
    DisplayTimeOnlyVisitorPtr timeVisitor(new DisplayTimeOnlyVisitor());
    ZebraVisitorPtr stripeVisitor(new ZebraVisitor());

    vector< ::std::string > columnNames;
    vector< MonitorContainer * > monitorCommon;
    
    const string hwTypeString = getHwTypeString( bs );

    switch (subsys) {
        case DIGITIZER:
            label << " Band " << bandNo << " Digitizer Common Data";
            commonLabel = RtLabelPtr(new RtLabel( label.str() ));
            commonFolder = RtFolderPtr(new RtFolder( hwTypeString + " Digitizer Common "));
            for ( int i = 0; i < bs.digitizerCount(); ++i ) {
                ::std::ostringstream os;
                os << "Dig" << ( i + 1 );
                columnNames.push_back( os.str() );
                MonitorContainer * mc = new MonitorContainer( os.str() );
                // Customize what points from the container are shown
                mc->add( bs.digitizer(i).boardNumber() );
                mc->add( bs.digitizer(i).slotNumber() );
                mc->add( bs.digitizer(i).boardRevision() );
                mc->add( bs.digitizer(i).boardType() );
                mc->add( bs.digitizer(i).boardSerialNumber() );
                mc->add( bs.digitizer(i).moduleRevision() );
                mc->add( bs.digitizer(i).moduleSerialNumber() );
                mc->add( bs.digitizer(i).versionDspSoftware() );
                mc->add( bs.digitizer(i).versionSysHdl() );
                mc->add( bs.digitizer(i).versionFpgaHdl() );
                mc->add( bs.digitizer(i).versionModuleHdl() );
                mc->add( bs.digitizer(i).osCpuUsage() );
                mc->add( bs.digitizer(i).osContextSwitchCounter() );
                mc->add( bs.digitizer(i).osTaskCounter() );
                mc->add( bs.digitizer(i).timeReferenceStatus() );
                mc->add( bs.digitizer(i).phaseReferenceStatus() );
                mc->add( bs.digitizer(i).dspStatus() );
                mc->add( bs.digitizer(i).timeOfLastUpdate() );
                mc->add( bs.digitizer(i).uptime() );
                monitorCommon.push_back( mc );
                timeVisitor->addToEffectedSet(
                    bs.digitizer(i).timeOfLastUpdate( ) );
                stripeVisitor->addMonitorContainer(*mc);
            }
            break;
        case CORRELATOR:
            label << " Band " << bandNo << " Correlator Common Data";
            commonLabel = RtLabelPtr(new RtLabel( label.str() ));
            commonFolder = RtFolderPtr(new RtFolder( hwTypeString + " Correlator Common "));
            for ( int i = 0; i < bs.correlatorCount(); ++i ) {
                ::std::ostringstream os;
                os << "Corr" << (i + 1);
                columnNames.push_back( os.str() );
                MonitorContainer * mc = new MonitorContainer( os.str() );
                // Customize what points from the container are shown
                mc->add( bs.correlator(i).boardNumber() );
                mc->add( bs.correlator(i).slotNumber() );
                mc->add( bs.correlator(i).boardRevision() );
                mc->add( bs.correlator(i).boardType() );
                mc->add( bs.correlator(i).boardSerialNumber() );
                mc->add( bs.correlator(i).versionDspSoftware() );
                mc->add( bs.correlator(i).versionSysHdl() );
                mc->add( bs.correlator(i).versionFpgaHdl() );
                mc->add( bs.correlator(i).osCpuUsage() );
                mc->add( bs.correlator(i).osContextSwitchCounter() );
                mc->add( bs.correlator(i).osTaskCounter() );
                mc->add( bs.correlator(i).timeReferenceStatus() );
                mc->add( bs.correlator(i).phaseReferenceStatus() );
                mc->add( bs.correlator(i).dspStatus() );
                mc->add( bs.correlator(i).timeOfLastUpdate() );
                mc->add( bs.correlator(i).uptime() );
                monitorCommon.push_back( mc );
                timeVisitor->addToEffectedSet(
                    bs.correlator(i).timeOfLastUpdate( ) );
                stripeVisitor->addMonitorContainer(*mc);
            } 
            break;
        default:
            ostringstream os;
            os << "2 Invalid subsystem type: " << subsys ;
            throw CARMA_EXCEPTION( ErrorException, os.str().c_str());
            break;
    }

    vector< MonitorTableVisitorPtr > visitors;
    visitors.push_back( timeVisitor );
    visitors.push_back( stripeVisitor );
    CompositeMonitorTableVisitorPtr compositeVisitor(new CompositeMonitorTableVisitor(visitors));

    commonLabel->setRelFontSize( RELATIVE_LABEL_FONT_SIZE );
    commonTable = MonitorTable::makeTable(
                                columnNames,
                                monitorCommon,
                                DEFAULT_COLUMN_WIDTH, 
                                1, 0, true, false, compositeVisitor ); 

    commonFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 2 ) ));
    commonFolder->add( commonLabel );
    commonFolder->add( RtSpacerPtr(new RtSpacer( 3, 5, 0 ) ));
    commonFolder->add( commonTable );
    commonFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 6 ) ));

    return commonFolder;

} // End makeCommonFolder. - Generic 

// -------------------
// Make common folder - CARMA 
// -------------------
RtFolderPtr
makeCommonFolder( CarmaSlcBandSubsystem & slc, 
                  SubsystemType subsys, unsigned bandNo ) 
{
    ostringstream label;
    RtFolderPtr commonFolder;
    RtLabelPtr commonLabel;
    MonitorTablePtr commonTable;
    ZebraVisitorPtr visitor(new ZebraVisitor());
    vector< ::std::string > columnNames;
    vector< MonitorContainer * > monitorCommon;
    
    switch (subsys) {
        case DIGITIZER:
            label << " Band " << bandNo << " Digitizer Common Data";
            commonLabel = RtLabelPtr(new RtLabel( label.str() ));
            commonFolder = RtFolderPtr(new RtFolder(" CARMA Digitizer Common "));
            for ( int i = 0; i < slc.digitizerCount(); ++i ) {
                ::std::ostringstream os;
                os << "Dig" << ( i + 1 );
                columnNames.push_back( os.str() );
                MonitorContainer * mc = new MonitorContainer( os.str() );
                // Customize what points from the container are shown
                mc->add( slc.digitizer(i).boardNumber() );
                mc->add( slc.digitizer(i).slotNumber() );
                mc->add( slc.digitizer(i).boardRevision() );
                mc->add( slc.digitizer(i).boardType() );
                mc->add( slc.digitizer(i).boardSerialNumber() );
                mc->add( slc.digitizer(i).versionDspSoftware() );
                mc->add( slc.digitizer(i).versionSysHdl() );
                mc->add( slc.digitizer(i).versionFpgaHdl() );
                mc->add( slc.digitizer(i).osCpuUsage() );
                mc->add( slc.digitizer(i).osLoadAverage() );
                mc->add( slc.digitizer(i).osFreeRam() );
                mc->add( slc.digitizer(i).osTaskCounter() );
                mc->add( slc.digitizer(i).osContextSwitchCounter() );
                mc->add( slc.digitizer(i).osEccErrors() );
                mc->add( slc.digitizer(i).timeReferenceStatus() );
                mc->add( slc.digitizer(i).phaseReferenceStatus() );
                mc->add( slc.digitizer(i).uptime() );
                monitorCommon.push_back( mc );
                visitor->addMonitorContainer(*mc);
            }
            break;
        case CORRELATOR:
            label << " Band " << bandNo << " Correlator Common Data";
            commonLabel = RtLabelPtr(new RtLabel( label.str() ));
            commonFolder = RtFolderPtr(new RtFolder(" CARMA Correlator Common "));
            for ( int i = 0; i < slc.correlatorCount(); ++i ) {
                ::std::ostringstream os;
                os << "Corr" << (i + 1);
                columnNames.push_back( os.str() );
                MonitorContainer * mc = new MonitorContainer( os.str() );
                mc->add( slc.correlator(i).boardNumber() );
                mc->add( slc.correlator(i).slotNumber() );
                mc->add( slc.correlator(i).boardRevision() );
                mc->add( slc.correlator(i).boardType() );
                mc->add( slc.correlator(i).boardSerialNumber() );
                mc->add( slc.correlator(i).versionDspSoftware() );
                mc->add( slc.correlator(i).versionSysHdl() );
                mc->add( slc.correlator(i).versionFpgaHdl() );
                mc->add( slc.correlator(i).osCpuUsage() );
                mc->add( slc.correlator(i).osLoadAverage() );
                mc->add( slc.correlator(i).osFreeRam() );
                mc->add( slc.correlator(i).osTaskCounter() );
                mc->add( slc.correlator(i).osContextSwitchCounter() );
                mc->add( slc.correlator(i).osEccErrors() );
                mc->add( slc.correlator(i).timeReferenceStatus() );
                mc->add( slc.correlator(i).phaseReferenceStatus() );
                mc->add( slc.correlator(i).uptime() );
                monitorCommon.push_back( mc );
                visitor->addMonitorContainer(*mc);
            } 
            break;
        default:
            ostringstream os;
            os << "3 Invalid subsystem type: " << subsys ;
            throw CARMA_EXCEPTION( ErrorException, os.str().c_str());
            break;
    }

    commonLabel->setRelFontSize( RELATIVE_LABEL_FONT_SIZE );
    commonTable = MonitorTable::makeTable(
                                columnNames,
                                monitorCommon,
                                DEFAULT_COLUMN_WIDTH, 
                                1, 0, true, false , visitor ); 

    commonFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 2 ) ));
    commonFolder->add( commonLabel );
    commonFolder->add( RtSpacerPtr(new RtSpacer( 3, 5, 0 ) ));
    commonFolder->add( commonTable );
    commonFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 6 ) ));

    return commonFolder;

} // End makeCommonFolder.

// -------------------
// Make common folder - CARMA3G
// -------------------
RtFolderPtr
makeCommonFolder( Carma3GBandSubsystem & slc, 
                  SubsystemType subsys, unsigned bandNo ) 
{
    ostringstream label;
    RtFolderPtr commonFolder;
    RtLabelPtr commonLabel;
    MonitorTablePtr commonTable;
    ZebraVisitorPtr visitor(new ZebraVisitor());
    vector< ::std::string > columnNames;
    vector< MonitorContainer * > monitorCommon;
    
    switch (subsys) {
        case DIGITIZER:

            label << " Band " << bandNo << " Digitizer Common Data";
            commonLabel = RtLabelPtr(new RtLabel( label.str() ));
            commonFolder = RtFolderPtr(new RtFolder(" CARMA3G Digitizer Common "));
            for ( int i = 0; i < slc.digitizerCount(); ++i ) {
                ::std::ostringstream os;
                os << "Dig" << ( i + 1 );
                columnNames.push_back( os.str() );
                MonitorContainer * mc = new MonitorContainer( os.str() );
                // Customize what points from the container are shown
                mc->add( slc.digitizer(i).boardNumber() );
                mc->add( slc.digitizer(i).slotNumber() );
                mc->add( slc.digitizer(i).boardRevision() );
                mc->add( slc.digitizer(i).boardType() );
                mc->add( slc.digitizer(i).boardSerialNumber() );
                mc->add( slc.digitizer(i).versionDspSoftware() );
                mc->add( slc.digitizer(i).versionSysHdl() );
                mc->add( slc.digitizer(i).versionFpgaHdl() );
                mc->add( slc.digitizer(i).osCpuUsage() );
                mc->add( slc.digitizer(i).osLoadAverage() );
                mc->add( slc.digitizer(i).osFreeRam() );
                mc->add( slc.digitizer(i).osTaskCounter() );
                mc->add( slc.digitizer(i).osContextSwitchCounter() );
                mc->add( slc.digitizer(i).osEccErrors() );
                mc->add( slc.digitizer(i).timeReferenceStatus() );
                mc->add( slc.digitizer(i).phaseReferenceStatus() );
                mc->add( slc.digitizer(i).uptime() );
                monitorCommon.push_back( mc );
                visitor->addMonitorContainer(*mc);
            }
            break;
        case CORRELATOR:
            label << " Band " << bandNo << " Correlator Common Data";
            commonLabel = RtLabelPtr(new RtLabel( label.str() ));
            commonFolder = RtFolderPtr(new RtFolder(" CARMA3G Correlator Common "));
            for ( int i = 0; i < slc.correlatorCount(); ++i ) {
                ::std::ostringstream os;
                os << "Corr" << (i + 1);
                columnNames.push_back( os.str() );
                MonitorContainer * mc = new MonitorContainer( os.str() );
                mc->add( slc.correlator(i).boardNumber() );
                mc->add( slc.correlator(i).slotNumber() );
                mc->add( slc.correlator(i).boardRevision() );
                mc->add( slc.correlator(i).boardType() );
                mc->add( slc.correlator(i).boardSerialNumber() );
                mc->add( slc.correlator(i).versionDspSoftware() );
                mc->add( slc.correlator(i).versionSysHdl() );
                mc->add( slc.correlator(i).versionFpgaHdl() );
                mc->add( slc.correlator(i).osCpuUsage() );
                mc->add( slc.correlator(i).osLoadAverage() );
                mc->add( slc.correlator(i).osFreeRam() );
                mc->add( slc.correlator(i).osTaskCounter() );
                mc->add( slc.correlator(i).osContextSwitchCounter() );
                mc->add( slc.correlator(i).osEccErrors() );
                mc->add( slc.correlator(i).timeReferenceStatus() );
                mc->add( slc.correlator(i).phaseReferenceStatus() );
                mc->add( slc.correlator(i).uptime() );
                monitorCommon.push_back( mc );
                visitor->addMonitorContainer(*mc);
            } 
            break;
        default:
            ostringstream os;
            os << "4 Invalid subsystem type: " << subsys ;
            throw CARMA_EXCEPTION( ErrorException, os.str().c_str());
            break;
    }

    commonLabel->setRelFontSize( RELATIVE_LABEL_FONT_SIZE );
    commonTable = MonitorTable::makeTable(
                                columnNames,
                                monitorCommon,
                                DEFAULT_COLUMN_WIDTH, 
                                1, 0, true, false , visitor ); 

    commonFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 2 ) ));
    commonFolder->add( commonLabel );
    commonFolder->add( RtSpacerPtr(new RtSpacer( 3, 5, 0 ) ));
    commonFolder->add( commonTable );
    commonFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 6 ) ));

    return commonFolder;

} // End makeCommonFolder.
// -------------------
// Make Digitizer folder - Generic  
// -------------------

MonitorContainer*
getDigitizerMonitorContainer(WbcBandSubsystem::Digitizer &dig, const std::string &name)
{
    // intentionally leaks
    MonitorContainer *cont = new MonitorContainer(name);

    // add all COBRA digitizer specific monitor points
    cont->add(dig.digATemperature());
    cont->add(dig.digBTemperature());
    cont->add(dig.fpga1Temperature());
    cont->add(dig.fpga3Temperature());
    cont->add(dig.phase());
    cont->add(dig.vcc());
    cont->add(dig.vcc3());
    cont->add(dig.vint_sys());
    cont->add(dig.vint_fpga());
    cont->add(dig.vecl());
    cont->add(dig.vtt());
    cont->add(dig.quantizationStatesValid());

    for (int i = 0; i < dig.getNumCorrelations(); i++)
        cont->add(dig.correlations(i));

    for (int i = 0; i < dig.getNumSpectralDelta(); i++)
        cont->add(dig.spectralDelta(i));

    return cont;
}

template <typename BandSubsystem>
RtFolderPtr
makeDigitizerFolder( BandSubsystem & bs, unsigned bandNo ) 
{
    ostringstream label;
    label << " Band " << bandNo << " Digitizers ";
    RtFolderPtr digitizerFolder = RtFolderPtr(new RtFolder(" COBRA Digitizers "));
    RtLabelPtr digitizerLabel = RtLabelPtr(new RtLabel( label.str() ));
    MonitorTablePtr digitizerTable;
    vector< ::std::string > columnNames;
    vector< MonitorContainer * > monitorDigitizers;
    ZebraVisitorPtr visitor(new ZebraVisitor());
    
    digitizerLabel->setRelFontSize( RELATIVE_LABEL_FONT_SIZE );
    
    // Form up columnNames and digitizer monitor vector.
    for ( int i = 0; i < bs.digitizerCount(); ++i ) {
        ::std::ostringstream os;
        os << "Dig" << ( i + 1 );
        columnNames.push_back( os.str() );

        MonitorContainer *digitizerContainer = getDigitizerMonitorContainer(bs.digitizer(i), os.str());
        monitorDigitizers.push_back( digitizerContainer );
        visitor->addMonitorContainer(digitizerContainer);
    }

    digitizerTable = MonitorTable::makeTable(
                                    columnNames,
                                    monitorDigitizers, 
                                    11, 1, 0, true, false,
                                    visitor );

    digitizerFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 2 ) ));
    digitizerFolder->add( digitizerLabel );
    digitizerFolder->add( RtSpacerPtr(new RtSpacer( 3, 5, 0 ) ));
    digitizerFolder->add( digitizerTable );
    digitizerFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 6 ) ));

    return digitizerFolder;

} // End makeDigitizerFolder

// -------------------
// Make Digitizer folder  - CARMA specialization
// -------------------

RtFolderPtr
makeDigitizerFolder( CarmaSlcBandSubsystem & slc, unsigned bandNo ) 
{
    ostringstream label;
    label << " Band " << bandNo << " Digitizer";
    RtFolderPtr digitizerFolder = RtFolderPtr(new RtFolder(" CARMA Digitizers "));
    MonitorTablePtr digitizerTable;
    vector< ::std::string > columnNames;
    vector< MonitorContainer * > monitorDigitizers;
    ZebraVisitorPtr visitor(new ZebraVisitor());
    
    // Voltages, currents and status of power supplies
    RtLabelPtr digitizerLabel = RtLabelPtr(new RtLabel( label.str() + " Voltages/Currents/Supplies" ));
    digitizerLabel->setRelFontSize( RELATIVE_LABEL_FONT_SIZE );
    
    // Form up columnNames and digitizer MonitorCarmaCommon vector.
    for ( int i = 0; i < slc.digitizerCount(); ++i ) {
        ::std::ostringstream os;
        os << "Dig" << ( i + 1 );
        columnNames.push_back( os.str() );
        monitorDigitizers.push_back( &slc.digitizer(i).power() );
        visitor->addMonitorContainer(slc.digitizer(i).power());
    }


    digitizerTable = MonitorTable::makeTable(
                                    columnNames,
                                    monitorDigitizers, 
                                    11, 1, 0, true, false, visitor);
    // Now customize the labels for the voltages.
    unsigned rowNo = 0;
    digitizerTable->setRowLabel(rowNo++,"CPCI 3.3V (V)");
    digitizerTable->setRowLabel(rowNo++,"CPCI   5V (V)");
    digitizerTable->setRowLabel(rowNo++,"CPCI  12V (V)");

    digitizerTable->setRowLabel(rowNo++,"CPCI 3.3V (A)");
    digitizerTable->setRowLabel(rowNo++,"CPCI   5V (A)");
    digitizerTable->setRowLabel(rowNo++,"CPCI  12V (A)");

    digitizerTable->setRowLabel(rowNo++,"SYS 1.2V (V)");
    digitizerTable->setRowLabel(rowNo++,"SYS 2.5V (V)");
    digitizerTable->setRowLabel(rowNo++,"SYS 1.2V (A)");
    digitizerTable->setRowLabel(rowNo++,"SYS 2.5V (A)");

    digitizerTable->setRowLabel(rowNo++,"DIG  1.8V (V)");
    digitizerTable->setRowLabel(rowNo++,"DIG 2.25V (V)");
    digitizerTable->setRowLabel(rowNo++,"DIG  3.3V (V)");

    digitizerTable->setRowLabel(rowNo++,"FPGA  12V (V)");
    digitizerTable->setRowLabel(rowNo++,"FPGA 1.2V (V)");
    digitizerTable->setRowLabel(rowNo++,"FPGA 2.5V (V)");
    digitizerTable->setRowLabel(rowNo++,"FPGA 3.3V (V)");
    digitizerTable->setRowLabel(rowNo++,"FPGA  12V (A)");
    digitizerTable->setRowLabel(rowNo++,"FPGA 1.2V (A)");
    digitizerTable->setRowLabel(rowNo++,"FPGA 2.5V (A)");
    digitizerTable->setRowLabel(rowNo++,"FPGA 3.3V (A)");

    digitizerFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 2 ) ));
    digitizerFolder->add( digitizerLabel );
    digitizerFolder->add( RtSpacerPtr(new RtSpacer( 3, 5, 0 ) ));
    digitizerFolder->add( digitizerTable );
    digitizerFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 6 ) ));

    monitorDigitizers.clear();
    columnNames.clear();
    RtLabelPtr temperatureLabel  = RtLabelPtr(new RtLabel( label.str()+" Temperatures" ));
    temperatureLabel->setRelFontSize( RELATIVE_LABEL_FONT_SIZE );
    // Now do temperatures.
    for ( int i = 0; i < slc.digitizerCount(); ++i ) {
        ::std::ostringstream os;
        os << "Dig" << ( i + 1 );
        columnNames.push_back( os.str() );
        monitorDigitizers.push_back( &slc.digitizer(i).temperature() );
        visitor->addMonitorContainer(slc.digitizer(i).temperature());
    }

    digitizerTable = MonitorTable::makeTable(
                                    columnNames,
                                    monitorDigitizers, 
                                    11, 1, 0, true, false, visitor);

    digitizerFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 2 ) ));
    digitizerFolder->add( temperatureLabel);
    digitizerFolder->add( RtSpacerPtr(new RtSpacer( 3, 5, 0 ) ));
    digitizerFolder->add( digitizerTable );
    digitizerFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 6 ) ));

    return digitizerFolder;
}

RtFolderPtr
makeDigitizerFolder( Carma3GBandSubsystem & slc, unsigned bandNo ) 
{
    ostringstream label;
    label << " C3GBand " << bandNo << " Digitizer";
    RtFolderPtr digitizerFolder = RtFolderPtr(new RtFolder(" CARMA Digitizers "));
    MonitorTablePtr digitizerTable;
    vector< ::std::string > columnNames;
    vector< MonitorContainer * > monitorDigitizers;
    ZebraVisitorPtr visitor(new ZebraVisitor());
    
    // Voltages, currents and status of power supplies
    RtLabelPtr digitizerLabel = RtLabelPtr(new RtLabel( label.str() + " Voltages/Currents/Supplies" ));
    digitizerLabel->setRelFontSize( RELATIVE_LABEL_FONT_SIZE );
    
    // Form up columnNames and digitizer MonitorCarmaCommon vector.
    for ( int i = 0; i < slc.digitizerCount(); ++i ) {
        ::std::ostringstream os;
        os << "Dig" << ( i + 1 );
        columnNames.push_back( os.str() );
        monitorDigitizers.push_back( &slc.digitizer(i).power() );
        visitor->addMonitorContainer(slc.digitizer(i).power());
    }


    digitizerTable = MonitorTable::makeTable(
                                    columnNames,
                                    monitorDigitizers, 
                                    11, 1, 0, true, false, visitor);
    // Now customize the labels for the voltages.
    unsigned rowNo = 0;
    digitizerTable->setRowLabel(rowNo++,"CPCI 3.3V (V)");
    digitizerTable->setRowLabel(rowNo++,"CPCI   5V (V)");
    digitizerTable->setRowLabel(rowNo++,"CPCI  12V (V)");

    digitizerTable->setRowLabel(rowNo++,"CPCI 3.3V (A)");
    digitizerTable->setRowLabel(rowNo++,"CPCI   5V (A)");
    digitizerTable->setRowLabel(rowNo++,"CPCI  12V (A)");

    digitizerTable->setRowLabel(rowNo++,"SYS 1.2V (V)");
    digitizerTable->setRowLabel(rowNo++,"SYS 2.5V (V)");
    digitizerTable->setRowLabel(rowNo++,"SYS 1.2V (A)");
    digitizerTable->setRowLabel(rowNo++,"SYS 2.5V (A)");

    digitizerTable->setRowLabel(rowNo++,"DIG  1.8V (V)");
    digitizerTable->setRowLabel(rowNo++,"DIG 2.25V (V)");
    digitizerTable->setRowLabel(rowNo++,"DIG  3.3V (V)");

    digitizerTable->setRowLabel(rowNo++,"FPGA  12V (V)");
    digitizerTable->setRowLabel(rowNo++,"FPGA 1.2V (V)");
    digitizerTable->setRowLabel(rowNo++,"FPGA 2.5V (V)");
    digitizerTable->setRowLabel(rowNo++,"FPGA 3.3V (V)");
    digitizerTable->setRowLabel(rowNo++,"FPGA  12V (A)");
    digitizerTable->setRowLabel(rowNo++,"FPGA 1.2V (A)");
    digitizerTable->setRowLabel(rowNo++,"FPGA 2.5V (A)");
    digitizerTable->setRowLabel(rowNo++,"FPGA 3.3V (A)");

    digitizerFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 2 ) ));
    digitizerFolder->add( digitizerLabel );
    digitizerFolder->add( RtSpacerPtr(new RtSpacer( 3, 5, 0 ) ));
    digitizerFolder->add( digitizerTable );
    digitizerFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 6 ) ));

    monitorDigitizers.clear();
    columnNames.clear();
    RtLabelPtr temperatureLabel  = RtLabelPtr(new RtLabel( label.str()+" Temperatures" ));
    temperatureLabel->setRelFontSize( RELATIVE_LABEL_FONT_SIZE );
    // Now do temperatures.
    for ( int i = 0; i < slc.digitizerCount(); ++i ) {
        ::std::ostringstream os;
        os << "Dig" << ( i + 1 );
        columnNames.push_back( os.str() );
        monitorDigitizers.push_back( &slc.digitizer(i).temperature() );
        visitor->addMonitorContainer(slc.digitizer(i).temperature());
    }

    digitizerTable = MonitorTable::makeTable(
                                    columnNames,
                                    monitorDigitizers, 
                                    11, 1, 0, true, false, visitor);

    digitizerFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 2 ) ));
    digitizerFolder->add( temperatureLabel);
    digitizerFolder->add( RtSpacerPtr(new RtSpacer( 3, 5, 0 ) ));
    digitizerFolder->add( digitizerTable );
    digitizerFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 6 ) ));

    return digitizerFolder;
}

template <typename BandSubsystem>
RtFolderPtr
makeStatisticsFolder( BandSubsystem & bs, unsigned bandNo ) 
{
    ostringstream label;
    label << " Band " << bandNo << " Digitizer Statistics";
    RtFolderPtr digitizerFolder = RtFolderPtr(new RtFolder("Digitizers Stats"));

    MonitorTablePtr digitizerATable;
    MonitorTablePtr digitizerBTable;
    vector< ::std::string > columnNames;
    vector< MonitorContainer * > monitorADigitizers;
    vector< MonitorContainer * > monitorBDigitizers;

    RtLabelPtr quantALabel = RtLabelPtr(new RtLabel( label.str() + " DigA Quantization" ));
    RtLabelPtr quantBLabel = RtLabelPtr(new RtLabel( label.str() + " DigB Quantization"));

    quantALabel->setRelFontSize( RELATIVE_LABEL_FONT_SIZE );
    quantBLabel->setRelFontSize( RELATIVE_LABEL_FONT_SIZE );

    ZebraVisitorPtr visitorA(new ZebraVisitor());
    ZebraVisitorPtr visitorB(new ZebraVisitor());

    for ( int i = 0; i < bs.digitizerCount(); ++i ) {
        ::std::ostringstream os;
        os << "Dig" << ( i + 1 );
        columnNames.push_back( os.str() );

        monitorADigitizers.push_back( &bs.digitizer(i).digA() );
        visitorA->addMonitorContainer(bs.digitizer(i).digA());

        monitorBDigitizers.push_back( &bs.digitizer(i).digB() );
        visitorB->addMonitorContainer(bs.digitizer(i).digB());
    }

    digitizerATable = MonitorTable::makeTable(
                                    columnNames,
                                    monitorADigitizers,
                                    11, 1, 0, true, false, visitorA);

    digitizerBTable = MonitorTable::makeTable(
                                    columnNames,
                                    monitorBDigitizers,
                                    11, 1, 0, true, false, visitorB);

    digitizerFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 2 ) ));
    digitizerFolder->add( quantALabel );
    digitizerFolder->add( RtSpacerPtr(new RtSpacer( 3, 5, 0 ) ));
    digitizerFolder->add( digitizerATable );
    digitizerFolder->add( RtSpacerPtr(new RtSpacer( 3, 5, 0 ) ));
    digitizerFolder->add( quantBLabel );
    digitizerFolder->add( RtSpacerPtr(new RtSpacer( 3, 5, 0 ) ));
    digitizerFolder->add( digitizerBTable );
    digitizerFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 6 ) ));

    return digitizerFolder;

} // End makeStatisticsFolder

// -------------------
// Make Correlator folder  - Generic 
// -------------------

MonitorContainer*
getCorrelatorMonitorContainer(WbcBandSubsystem::Correlator &cor, const std::string &name)
{
    // intentionally leaks
    MonitorContainer *cont = new MonitorContainer(name);

    // add all COBRA correlator specific monitor points
    cont->add(cor.fpga1Temperature());
    cont->add(cor.fpga7Temperature());
    cont->add(cor.phase0());
    cont->add(cor.phase1());
    cont->add(cor.phase2());
    cont->add(cor.phase3());
    cont->add(cor.vcc());
    cont->add(cor.vcc3());
    cont->add(cor.vint_sys());
    cont->add(cor.vint_fpga());
    cont->add(cor.clockDelay());

    for (int i = 0; i < cor.getNumCorrelations(); i++)
        cont->add(cor.correlations(i));

    for (int i = 0; i < cor.getNumSpectralDelta(); i++)
        cont->add(cor.spectralDelta(i));

    return cont;
}

template <typename BandSubsystem>
RtFolderPtr
makeCorrelatorFolder( BandSubsystem & bs, unsigned bandNo ) 
{
    ostringstream label;
    label << " Band " << bandNo << " Correlators ";
    RtFolderPtr correlatorFolder = RtFolderPtr(new RtFolder(" COBRA Correlators "));
    RtLabelPtr correlatorLabel = RtLabelPtr(new RtLabel( label.str() ));
    MonitorTablePtr correlatorTable;
    vector< ::std::string > columnNames;
    vector< MonitorContainer * > monitorCorrelators;
    ZebraVisitorPtr visitor(new ZebraVisitor());
    
    correlatorLabel->setRelFontSize( RELATIVE_LABEL_FONT_SIZE );
    
    // Form up columnNames and digitizer monitor vector.
    for ( int i = 0; i < bs.correlatorCount(); ++i ) {
        ::std::ostringstream os;
        os << "Corr" << ( i + 1 );
        columnNames.push_back( os.str() );

        MonitorContainer *correlatorContainer = getCorrelatorMonitorContainer(bs.correlator(i), os.str());
        monitorCorrelators.push_back( correlatorContainer );
        visitor->addMonitorContainer(correlatorContainer);
    }

    correlatorTable = MonitorTable::makeTable(
                                    columnNames,
                                    monitorCorrelators,
                                    11, 1, 0, true, false, visitor);

    correlatorFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 2 ) ));
    correlatorFolder->add( correlatorLabel );
    correlatorFolder->add( RtSpacerPtr(new RtSpacer( 3, 5, 0 ) ));
    correlatorFolder->add( correlatorTable );
    correlatorFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 6 ) ));

    return correlatorFolder;

} // End makeCorrelatorFolder

// -------------------
// Make Correlator folder  - Carma specialization 
// -------------------
RtFolderPtr
makeCorrelatorFolder( CarmaSlcBandSubsystem & slc, unsigned bandNo ) 
{
    ostringstream label;
    label << " Band " << bandNo << " Correlator";
    RtFolderPtr correlatorFolder = RtFolderPtr(new RtFolder(" CARMA Correlators "));
    MonitorTablePtr correlatorTable;
    vector< ::std::string > columnNames;
    vector< MonitorContainer * > monitorCorrelators;
    ZebraVisitorPtr visitor(new ZebraVisitor());
    
    // Voltages, currents and status of power supplies
    RtLabelPtr correlatorLabel = RtLabelPtr(new RtLabel( label.str() + " Voltages/Currents/Supplies" ));
    correlatorLabel->setRelFontSize( RELATIVE_LABEL_FONT_SIZE );
    
    // Form up columnNames and digitizer monitor vector.
    for ( int i = 0; i < slc.correlatorCount(); ++i ) {
        ::std::ostringstream os;
        os << "Corr" << ( i + 1 );
        columnNames.push_back( os.str() );
        monitorCorrelators.push_back( 
                                &slc.correlator(i).power() );
        visitor->addMonitorContainer(slc.correlator(i).power());
    }

    correlatorTable = MonitorTable::makeTable(
                                    columnNames,
                                    monitorCorrelators,
                                    11, 1, 0, true, false, visitor);

    // Now customize the labels for the voltages.
    unsigned rowNo = 0;
    correlatorTable->setRowLabel(rowNo++,"CPCI 3.3V (V)");
    correlatorTable->setRowLabel(rowNo++,"CPCI   5V (V)");
    correlatorTable->setRowLabel(rowNo++,"CPCI  12V (V)");

    correlatorTable->setRowLabel(rowNo++,"CPCI 3.3V (A)");
    correlatorTable->setRowLabel(rowNo++,"CPCI   5V (A)");
    correlatorTable->setRowLabel(rowNo++,"CPCI  12V (A)");

    correlatorTable->setRowLabel(rowNo++,"SYS 1.2V (V)");
    correlatorTable->setRowLabel(rowNo++,"SYS 2.5V (V)");
    correlatorTable->setRowLabel(rowNo++,"SYS 1.2V (A)");
    correlatorTable->setRowLabel(rowNo++,"SYS 2.5V (A)");

    correlatorTable->setRowLabel(rowNo++,"DIG  1.8V (V)");
    correlatorTable->setRowLabel(rowNo++,"DIG 2.25V (V)");
    correlatorTable->setRowLabel(rowNo++,"DIG  3.3V (V)");

    correlatorTable->setRowLabel(rowNo++,"FPGA  12V (V)");
    correlatorTable->setRowLabel(rowNo++,"FPGA 1.2V (V)");
    correlatorTable->setRowLabel(rowNo++,"FPGA 2.5V (V)");
    correlatorTable->setRowLabel(rowNo++,"FPGA 3.3V (V)");
    correlatorTable->setRowLabel(rowNo++,"FPGA  12V (A)");
    correlatorTable->setRowLabel(rowNo++,"FPGA 1.2V (A)");
    correlatorTable->setRowLabel(rowNo++,"FPGA 2.5V (A)");
    correlatorTable->setRowLabel(rowNo++,"FPGA 3.3V (A)");

    correlatorFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 2 ) ));
    correlatorFolder->add( correlatorLabel );
    correlatorFolder->add( RtSpacerPtr(new RtSpacer( 3, 5, 0 ) ));
    correlatorFolder->add( correlatorTable );
    correlatorFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 6 ) ));

    // Temperatures
    monitorCorrelators.clear();
    columnNames.clear();
    RtLabelPtr temperatureLabel  = RtLabelPtr(new RtLabel( label.str()+" Temperatures" ));
    temperatureLabel->setRelFontSize( RELATIVE_LABEL_FONT_SIZE );
    // Form up columnNames and digitizer monitor vector.
    for ( int i = 0; i < slc.correlatorCount(); ++i ) {
        ::std::ostringstream os;
        os << "Corr" << ( i + 1 );
        columnNames.push_back( os.str() );
        monitorCorrelators.push_back( &slc.correlator(i).temperature() );
        visitor->addMonitorContainer(slc.correlator(i).temperature());
    }

    correlatorTable = MonitorTable::makeTable(
                                    columnNames,
                                    monitorCorrelators,
                                    11, 1, 0, true, false, visitor);

    correlatorFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 2 ) ));
    correlatorFolder->add( temperatureLabel );
    correlatorFolder->add( RtSpacerPtr(new RtSpacer( 3, 5, 0 ) ));
    correlatorFolder->add( correlatorTable );
    correlatorFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 6 ) ));

    return correlatorFolder;

} // End makeCorrelatorFolder

// -------------------
// Make Correlator folder  - Carma3g specialization 
// -------------------
RtFolderPtr
makeCorrelatorFolder( Carma3GBandSubsystem & slc, unsigned bandNo ) 
{
    ostringstream label;
    label << " C3GBand " << bandNo << " Correlator";
    RtFolderPtr correlatorFolder = RtFolderPtr(new RtFolder(" CARMA Correlators "));
    MonitorTablePtr correlatorTable;
    vector< ::std::string > columnNames;
    vector< MonitorContainer * > monitorCorrelators;
    ZebraVisitorPtr visitor(new ZebraVisitor());
    
    // Voltages, currents and status of power supplies
    RtLabelPtr correlatorLabel = RtLabelPtr(new RtLabel( label.str() + " Voltages/Currents/Supplies" ));
    correlatorLabel->setRelFontSize( RELATIVE_LABEL_FONT_SIZE );
    
    // Form up columnNames and digitizer monitor vector.
    for ( int i = 0; i < slc.correlatorCount(); ++i ) {
        ::std::ostringstream os;
        os << "Corr" << ( i + 1 );
        columnNames.push_back( os.str() );
        monitorCorrelators.push_back( 
                                &slc.correlator(i).power() );
        visitor->addMonitorContainer(slc.correlator(i).power());
    }

    correlatorTable = MonitorTable::makeTable(
                                    columnNames,
                                    monitorCorrelators,
                                    11, 1, 0, true, false, visitor);

    // Now customize the labels for the voltages.
    unsigned rowNo = 0;
    correlatorTable->setRowLabel(rowNo++,"CPCI 3.3V (V)");
    correlatorTable->setRowLabel(rowNo++,"CPCI   5V (V)");
    correlatorTable->setRowLabel(rowNo++,"CPCI  12V (V)");

    correlatorTable->setRowLabel(rowNo++,"CPCI 3.3V (A)");
    correlatorTable->setRowLabel(rowNo++,"CPCI   5V (A)");
    correlatorTable->setRowLabel(rowNo++,"CPCI  12V (A)");

    correlatorTable->setRowLabel(rowNo++,"SYS 1.2V (V)");
    correlatorTable->setRowLabel(rowNo++,"SYS 2.5V (V)");
    correlatorTable->setRowLabel(rowNo++,"SYS 1.2V (A)");
    correlatorTable->setRowLabel(rowNo++,"SYS 2.5V (A)");

    correlatorTable->setRowLabel(rowNo++,"DIG  1.8V (V)");
    correlatorTable->setRowLabel(rowNo++,"DIG 2.25V (V)");
    correlatorTable->setRowLabel(rowNo++,"DIG  3.3V (V)");

    correlatorTable->setRowLabel(rowNo++,"FPGA  12V (V)");
    correlatorTable->setRowLabel(rowNo++,"FPGA 1.2V (V)");
    correlatorTable->setRowLabel(rowNo++,"FPGA 2.5V (V)");
    correlatorTable->setRowLabel(rowNo++,"FPGA 3.3V (V)");
    correlatorTable->setRowLabel(rowNo++,"FPGA  12V (A)");
    correlatorTable->setRowLabel(rowNo++,"FPGA 1.2V (A)");
    correlatorTable->setRowLabel(rowNo++,"FPGA 2.5V (A)");
    correlatorTable->setRowLabel(rowNo++,"FPGA 3.3V (A)");

    correlatorFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 2 ) ));
    correlatorFolder->add( correlatorLabel );
    correlatorFolder->add( RtSpacerPtr(new RtSpacer( 3, 5, 0 ) ));
    correlatorFolder->add( correlatorTable );
    correlatorFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 6 ) ));

    // Temperatures
    monitorCorrelators.clear();
    columnNames.clear();
    RtLabelPtr temperatureLabel  = RtLabelPtr(new RtLabel( label.str()+" Temperatures" ));
    temperatureLabel->setRelFontSize( RELATIVE_LABEL_FONT_SIZE );
    // Form up columnNames and digitizer monitor vector.
    for ( int i = 0; i < slc.correlatorCount(); ++i ) {
        ::std::ostringstream os;
        os << "Corr" << ( i + 1 );
        columnNames.push_back( os.str() );
        monitorCorrelators.push_back( &slc.correlator(i).temperature() );
        visitor->addMonitorContainer(slc.correlator(i).temperature());
    }

    correlatorTable = MonitorTable::makeTable(
                                    columnNames,
                                    monitorCorrelators,
                                    11, 1, 0, true, false, visitor);

    correlatorFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 2 ) ));
    correlatorFolder->add( temperatureLabel );
    correlatorFolder->add( RtSpacerPtr(new RtSpacer( 3, 5, 0 ) ));
    correlatorFolder->add( correlatorTable );
    correlatorFolder->add( RtSpacerPtr(new RtSpacer( 5, 10, 6 ) ));

    return correlatorFolder;

} // End makeCorrelatorFolder

template <typename BandSubsystem>
void doDisplay(
        MonitorDisplay &display,
        BandSubsystem &bs,
        unsigned bandNo )
{
    // Make our various folders and add them to the display
    string errMsg;
    try {
        programLogNoticeIfPossible("entering doDisplay");
        errMsg = "makeStatusFolder(DIG)";
        programLogNoticeIfPossible( errMsg );
        display.add( makeStatusFolder( bs, DIGITIZER, bandNo ) );

        errMsg = "makeStatusFolder(COR)";
        programLogNoticeIfPossible( errMsg );
        display.add( makeStatusFolder( bs, CORRELATOR, bandNo ) );

        errMsg = "makeDelaysFolder";
        programLogNoticeIfPossible( errMsg );
        display.add( makeDelaysFolder( bs, bandNo ) );

        errMsg = "makeCommonFolder(DIG)";
        programLogNoticeIfPossible( errMsg );
        display.add( makeCommonFolder( bs, DIGITIZER, bandNo ) );

        errMsg = "makeCommonFolder(COR)";
        programLogNoticeIfPossible( errMsg );
        display.add( makeCommonFolder( bs, CORRELATOR, bandNo) );

        errMsg = "makeDigitizerFolder";
        programLogNoticeIfPossible( errMsg );
        display.add( makeDigitizerFolder( bs, bandNo ) );

        errMsg = "makeCorrelatorFolder";
        programLogNoticeIfPossible( errMsg );
        display.add( makeCorrelatorFolder( bs, bandNo ) );


        const string hwTypeString = getHwTypeString( bs );
        // C3G bands don't have digitizer statistics monitor data
        if ( hwTypeString.compare("C3G") != 0 ) {
            errMsg = "makeStatisticsFolder";
            programLogNoticeIfPossible( errMsg );
            display.add( makeStatisticsFolder( bs, bandNo ) );
        }
        programLogNoticeIfPossible("returning from doDisplay");


    } catch ( const ::std::out_of_range & ex ) {
        ostringstream os;
        os << " out_of_range caught in " << errMsg;
        throw CARMA_EXCEPTION (ErrorException, os.str() );
    } catch ( const ErrorException & eex ) {
        ostringstream os;
        os << eex.getMessage() << " in " << errMsg;
        throw CARMA_EXCEPTION (ErrorException, os.str() );
    } catch ( ... ) {
        programLogNoticeIfPossible("caught something bad");
        throw CARMA_EXCEPTION (ErrorException, "Unclassified exception" );
    }
}

} // end anonymous namespace

int 
Program::main() 
{
    try {
        ScopedLogNdc ndc("rtdcorrelator::main()");

        // Retrieve band from input options (no command line for rtds)
        const string typeString = Program::getStringParameter("string1");
        const string bandString = Program::getStringParameter("string2");
        const unsigned short band = atoi( bandString.c_str() );
        const bool slc = ( typeString.find("slc") != string::npos );
        const bool wbc = ( typeString.find("wbc") != string::npos );
        //const bool c3g = ( typeString.find("c3g") != string::npos );

        const string corrTypeString = ( slc ? "Spectral Line" : ( wbc ? "Wideband"  : "CARMA3G" ) );

        // Create a display
        ostringstream titlebar;
        titlebar << "Band " << band << " " << corrTypeString 
                  << " Correlator Status";
        MonitorDisplay display( titlebar.str() );
        display.setSpecificHelp( corrTypeString + " Correlator Help",
                                 makeHelp() );

        if ( wbc ) {

            WbcBandSubsystem & wbcb = display.cms().wbcBand( band - 9 );
            doDisplay( display, wbcb, band );

        } else { 

            const hardwareType hType  = util::hwType( band );

            switch (hType) {
                case  HARDWARE_TYPE_CARMA:
                    {
                        CarmaSlcBandSubsystem & slcb
                          = display.cms().carmaSlcBand( band - 1 );
                        doDisplay( display, slcb, band );
                    }
                    break;
                case  HARDWARE_TYPE_C3G:
                    {
                        ostringstream os;
                        os << "Starting C3G display for band " << band
                            <<" Corrstring = " << corrTypeString;
                        programLogNoticeIfPossible(os.str());
                        Carma3GBandSubsystem & c3gb
                          = display.cms().carma3gBand( band - 25 );
                        doDisplay( display, c3gb, band );
                    }
                     break;
                default:
                case  HARDWARE_TYPE_UNKNOWN:
                case  HARDWARE_TYPE_COBRA:
                     {
                     ostringstream os;
                     os << "Invalid hardware type: "
                        << getStringForHardwareType(hType)
                        << " typeString = " << typeString
                        << " bandString = " << bandString
                        << " bandNo = " << band; 
                     throw CARMA_EXCEPTION( ErrorException,os.str().c_str());
                     }
                     break;
            }
        }

        // Serve the data out.
        while ( display.serveData() ) { }

        return EXIT_SUCCESS;

    } catch ( const ErrorException & ex ) {
        programLogErrorIfPossible( ex.getMessage());
        cerr << "Caught exception: " << ex.getMessage() << endl;
        return EXIT_FAILURE;
    } catch ( ... ) {
        const string err = " Caught unclassified exception";
        logCaughtBacktraceAsErrorIfPossible( err );
        //cerr << getCaughtBacktraceAsString() << endl;
        return EXIT_FAILURE;
    }

} // End main

