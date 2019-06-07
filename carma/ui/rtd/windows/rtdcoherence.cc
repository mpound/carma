/**
 * Real Time Display for coherence monitor.
 *
 * @author Andy Beard
 * $Id: rtdcoherence.cc,v 1.5 2014/06/30 22:02:42 scott Exp $
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
        os << "CARMA Coherence Monitor Window Help. " << endl;
        return os.str();
    }

    MonitorPoint &
    getCoherenceMP( const AstroSubsystem & astro,
                    const int antennaNo,
                    const BandPolPair & bandpol,
                    const bool usb )
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

        if ( usb ) {
            return rx->usb().coherence();
        } else {
            return rx->lsb().coherence();
        }
    }

    MonitorContainerPtr
    makeCoherenceContainer( const AstroSubsystem & astro, 
                            const int antennaNo,
                            const vector< BandPolPair > & mappedBandPols,
                            const bool usb )
    {
        ostringstream name;
        name << ( usb ? "USB" : "LSB" ) << "-C" << antennaNo;
        MonitorContainerPtr answer( new MonitorContainer( name.str( ) ) );

        BOOST_FOREACH( const BandPolPair & bandpol, mappedBandPols ) {

            MonitorPoint & mp =  getCoherenceMP( astro, 
                                                 antennaNo,
                                                 bandpol,
                                                 usb );

            mp.setPrecision(2);
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
        vector< MonitorContainerPtr > & usbContainers, 
        vector< MonitorContainerPtr > & lsbContainers )
    {
        columnLabels = makeColumnLabels( mappedAntNos );

        BOOST_FOREACH( const int antNo, mappedAntNos ) {
            usbContainers.push_back( 
                makeCoherenceContainer( astroSs, antNo, mappedBandPols, true ) );
            lsbContainers.push_back(
                makeCoherenceContainer( astroSs, antNo, mappedBandPols, false ) );
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


//@ TODO Support C3G modes
int 
Program::main() {
    
    const string string1Param = getStringParameter( "string1" );
    const bool sldc = ( string1Param.find("sldc") != string::npos );

    const string title = ( sldc ? string( "Spectral Line" ) :
                                  string( "Wideband" ) )  + 
                                  " Coherence Monitor";
    const MonitorCorrelatorDesignation corrDes( sldc ? 
            monitor::corrTypeToCorrDes(CORR_SPECTRAL) :
            monitor::corrTypeToCorrDes(CORR_WIDEBAND) );

    MonitorDisplay display( title );
    display.setSpecificHelp( "CARMA Coherence Monitor Help", makeHelp( ) );
    
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
    vector< MonitorContainerPtr > lsbContainers;
    vector< MonitorContainerPtr > usbContainers;
    
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
                             usbContainers,
                             lsbContainers );

    MonitorSingleTableFolderPtr usbFolder(new MonitorSingleTableFolder(
                                         "Coherence USB",
                                         columnLabels,
                                         convertToNonShared( usbContainers ),
                                         5,
                                         1 ));
    MonitorSingleTableFolderPtr lsbFolder(new MonitorSingleTableFolder(
                                         "Coherence LSB",
                                         columnLabels,
                                         convertToNonShared( lsbContainers ),
                                         5,
                                         1 ));

    usbFolder->add( notifyBox );
    lsbFolder->add( notifyBox );

    display.add( usbFolder );
    display.add( lsbFolder );

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
