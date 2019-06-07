/**
 * Real Time Display for tsys and flux calibration values.
 *
 * @author Andy Beard
 * $Id: rtdcalibration.cc,v 1.18 2014/06/04 17:09:50 mpound Exp $
 *
 * $CarmaCopyright$
 */

#include "carma/monitor/AstroSubsystemExt.h"
#include "carma/monitor/SignalPathMapping.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/Program.h"

#include <boost/shared_ptr.hpp>
#include <boost/foreach.hpp>
#include <iostream>
#include <vector>

using namespace std;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui::rtd;

namespace {

    typedef boost::shared_ptr< MonitorContainer > MonitorContainerPtr;

    string
    makeHelp() {
        ostringstream os;
        os << "CARMA Calibration Window Help. " << endl
           << "This window contains tsys calibration values for CARMA. " 
           << "The columns are the inputs to the correlator downconverters "
           << "and digitizers. "
           << "The calibration procedure assigns a value of 10000K whenever "
           << " a calibration  fails or if the computed value exceeds "
           << " 10000K." << endl 
           << "Effective Tcal values are listed on the Eff Tcal tab.  This "
           << "is the effective system temperature multiplied by the Y factor "
           << "to obtain the double sideband system temperature.  It consists "
           << "of the (ambient) load temperature plus additional components "
           << "for atmospheric and ground spillover contributions all minus "
           << "the CMB temperature component." << endl;
        return os.str();
    }

    enum TsysTypes {
        TDSB,
        TUSB,
        TLSB,
        TEFF };

    string
    tsysTypeToString( const enum TsysTypes type ) 
    {
        switch ( type ) {
            case TDSB: return "Tdsb";
            case TUSB: return "Tusb";
            case TLSB: return "Tlsb";
            case TEFF: return "Teff";
            default: return "<error>";
        }
    }
    
    MonitorPoint &
    getTsysMP( const AstroSubsystem & astro,
               const int antennaNo,
               const BandPolPair & bandpol,
               const TsysTypes tsysType )
    {
        const AstroSubsystem::Antenna & antenna = astro.antenna( antennaNo - 1);
        const AstroSubsystem::Band & band = antenna.band( bandpol.first - 1 );

        const AstroSubsystem::Rx * rx;
        if ( bandpol.second == PolMPE::L ) {
            rx = &( band.leftPol() );
        } else if ( bandpol.second == PolMPE::R ) {
            rx = &( band.rightPol() );
        } else {
            throw CARMA_EXCEPTION( IllegalArgumentException, "Invalid pol!" );
        }

        switch ( tsysType ) {
            case TDSB:
                return rx->tdsb();
            case TUSB:
                return rx->usb().tsys();
            case TLSB:
                return rx->lsb().tsys();
            case TEFF:
                return rx->effectiveTcal();
            default:
                throw CARMA_EXCEPTION( IllegalArgumentException, 
                                       "Invalid Tsys type!" );
        }
    }

    MonitorContainerPtr
    makeTsysContainer( const AstroSubsystem & astro, 
                       const int antennaNo,
                       const vector< BandPolPair > & mappedBandPols,
                       const enum TsysTypes type ) 
    {
        ostringstream name;
        name << tsysTypeToString( type ) << "-C" << antennaNo;
        MonitorContainerPtr answer( new MonitorContainer( name.str( ) ) );

        BOOST_FOREACH( const BandPolPair & bandpol, mappedBandPols ) {

            MonitorPoint & mp =  getTsysMP( astro, 
                                            antennaNo,
                                            bandpol,
                                            type );

            ostringstream rowName;
            rowName << "Astroband " << bandpol.first 
                << PolMPE::valueToString( bandpol.second ) << " ";
            mp.setShortName(rowName.str());
            answer->add( mp );
        }
        return answer;
    }

    vector< string >
    makeColumnLabels( const vector< int > & mappedAntNos ) {
        vector< string > labels;
        BOOST_FOREACH( const int ant, mappedAntNos ) {
            ostringstream os;
            os << "C" << ant;
            labels.push_back( os.str( ) );
        }
        return labels;
    }
    
    void 
    makeContainersAndLabels( 
        const  AstroSubsystem & astroSs,
        const vector< int > & mappedAntNos,
        const vector< BandPolPair > & mappedBandPols,
        vector< string > & columnLabels,
        vector< MonitorContainerPtr > & tdsbContainers, 
        vector< MonitorContainerPtr > & tlsbContainers,
        vector< MonitorContainerPtr > & tusbContainers,
        vector< MonitorContainerPtr > & tcalContainers )
    {
        columnLabels = makeColumnLabels( mappedAntNos );

        BOOST_FOREACH( const int antNo, mappedAntNos ) {
            tdsbContainers.push_back( 
                makeTsysContainer( astroSs, antNo, mappedBandPols, TDSB ) );
            tlsbContainers.push_back(
                makeTsysContainer( astroSs, antNo, mappedBandPols, TLSB ) );
            tusbContainers.push_back( 
                makeTsysContainer( astroSs, antNo, mappedBandPols, TUSB ) );
            tcalContainers.push_back( 
                makeTsysContainer( astroSs, antNo, mappedBandPols, TEFF ) );
        }
    }

    // Convert a vector of shared_ptrs to normal ptrs until downstream 
    // supports proper memory management.
    vector< MonitorContainer * >
    convertToNonShared( const vector< MonitorContainerPtr > & shared )
    {
        vector< MonitorContainer * > answer;

        BOOST_FOREACH( MonitorContainerPtr sharedPtr, shared )
        {
            answer.push_back( sharedPtr.get() );
        }
        return answer;
    }

} // End namespace <unnamed> 

int 
Program::main() {
    
    const string string1Param = getStringParameter( "string1" );
    const bool sldc = ( string1Param.find("sldc") != string::npos );

    const string title = ( sldc ? string( "Spectral Line" ) :
                                  string( "Wideband" ) )  + 
                                  " System Temperatures";
    const MonitorCorrelatorDesignation corrDes( sldc ? 
            monitor::corrTypeToCorrDes(CORR_SPECTRAL) :
            monitor::corrTypeToCorrDes(CORR_WIDEBAND) );

    MonitorDisplay display( title );
    display.setSpecificHelp( "CARMA Calibration Help", makeHelp( ) );
    
    SignalPathMapping spm( display.cms(), corrDes );
    display.cms().readNewest();
    spm.update();

    const vector< int > mappedAntNos = spm.getMappedAntennaNumbers();
    const vector< BandPolPair > mappedBandPols = spm.getMappedBandPolPairs();
    
    if ( mappedBandPols.size() == 0 ) {
        RtFolderPtr folder(new RtFolder( "" ));
        RtVBoxPtr top(new RtVBox( "Top" ));
        RtVBoxPtr mid(new RtVBox( "Mid", TWO_PIXELS_ALL_SIDES_BORDER ));
        RtVBoxPtr bot(new RtVBox( "Bot" ));
        RtLabelPtr label(new RtLabel("There is no signal path defined for any astrobands.\n"
            "Please morph this window after configuring the correlator.", 5 ));
        mid->add( label );

        folder->add( top );
        folder->add( mid );
        folder->add( bot );
        display.add( folder );
        while ( display.serveData() );
        return 0;
    }

    vector< string > columnLabels;
    vector< MonitorContainerPtr > tdsbContainers; 
    vector< MonitorContainerPtr > tlsbContainers;
    vector< MonitorContainerPtr > tusbContainers;
    vector< MonitorContainerPtr > tcalContainers;
    
    const string notifyFormat( "80.1.78" );
    const string altString("Signal path has changed."
        "  Morph this window to redisplay.");
    const string emptyString;
    const int altLen = 80;
    CellPtr notifyCell(new CellString( notifyFormat.c_str(),  emptyString, altLen ));
    notifyCell->addAlternative( altString, ORANGE_CELL_COLOR );
    notifyCell->setValidity( true );
    RtHBoxPtr notifyBox(new RtHBox( "Notify" ));
    notifyBox->add( notifyCell );
    
    const AstroSubsystem & astro = display.cms().astro();
    
    makeContainersAndLabels( astro, 
                             mappedAntNos,
                             mappedBandPols,
                             columnLabels,
                             tdsbContainers,
                             tlsbContainers,
                             tusbContainers,
                             tcalContainers );

    MonitorSingleTableFolderPtr tdsbFolder(new MonitorSingleTableFolder(
                                         "Tsys DSB",
                                         columnLabels,
                                         convertToNonShared( tdsbContainers ),
                                         5,
                                         1 ));
    MonitorSingleTableFolderPtr tlsbFolder(new MonitorSingleTableFolder(
                                         "Tsys LSB",
                                         columnLabels,
                                         convertToNonShared( tlsbContainers ),
                                         5,
                                         1 ));
    MonitorSingleTableFolderPtr tusbFolder(new MonitorSingleTableFolder(
                                         "Tsys USB",
                                         columnLabels,
                                         convertToNonShared( tusbContainers ),
                                         5,
                                         1 ));
    MonitorSingleTableFolderPtr tcalFolder(new MonitorSingleTableFolder(
                                         "Eff Tcal",
                                         columnLabels,
                                         convertToNonShared( tcalContainers ),
                                         5,
                                         1 ));

    tdsbFolder->add( notifyBox );
    tlsbFolder->add( notifyBox );
    tusbFolder->add( notifyBox );
    tcalFolder->add( notifyBox );

    display.add( tdsbFolder );
    display.add( tlsbFolder );
    display.add( tusbFolder );
    display.add( tcalFolder );

    bool signalPathModified = false;
    while ( display.serveData( ) ) {
        signalPathModified = spm.update();
        if ( signalPathModified ) {
            notifyCell->setActiveAlternative( 0 );
            notifyCell->setColor( YELLOW_CELL_COLOR );
        }
    }
    return 0;
}
