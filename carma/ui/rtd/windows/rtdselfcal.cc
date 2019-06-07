/**
 * Real Time Display for self cal.
 *
 * @author Andy Beard
 * $Id: rtdselfcal.cc,v 1.19 2014/06/04 17:09:50 mpound Exp $
 *
 * $CarmaCopyright$
 */

#include "carma/monitor/AstroSubsystemExt.h"
#include "carma/monitor/CorrDesignation.h"
#include "carma/monitor/SignalPathMapping.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"

#include <boost/foreach.hpp>
#include <boost/shared_ptr.hpp>
#include <iostream>

using namespace std;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui::rtd;

namespace {

    typedef ::boost::shared_ptr< MonitorContainer > MonitorContainerPtr;
    typedef ::std::pair< int, PolType > BandPolPair;

    const int COLUMN_WIDTH = 6;
    const int PRECISION = 1;

    AstroSubsystem::SelfCal &
    getSelfCal( const AstroSubsystem & astro,
                const int antennaNo,
                const int bandNo,
                const PolType pol,
                const bool usb )
    {
        const int aidx = antennaNo - 1;
        const int bidx = bandNo - 1;
        const AstroSubsystem::Band & band = astro.antenna( aidx ).band( bidx );
        if ( pol == PolMPE::L ) {
            if ( usb )
                return band.leftPol( ).usb( ).selfCal( );
            else
                return band.leftPol( ).lsb( ).selfCal( );
        } else if ( pol == PolMPE::R ) {
            if ( usb )
                return band.rightPol( ).usb( ).selfCal( );
            else
                return band.rightPol( ).lsb( ).selfCal( );
        } else {
            throw CARMA_EXCEPTION( IllegalArgumentException,
                                   "Unsupported polarization type." );
        }
    }
        
    MonitorContainerPtr
    makeComplexContainer( AstroSubsystem & astro,
                          const int antennaNo,
                          vector< BandPolPair > mappedBandPols,
                          const bool isUsb, // Otherwise lsb
                          const bool displayAmp, // Otherwise display phase
                          const bool isError )
    {
        MonitorContainerPtr complexContainer( new MonitorContainer( "Dumb" ) );

        BOOST_FOREACH( const BandPolPair & bpp, mappedBandPols )
        {
            AstroSubsystem::SelfCal & selfCal = getSelfCal( astro,
                                                            antennaNo,
                                                            bpp.first,
                                                            bpp.second,
                                                            isUsb );

            MonitorPointComplex & mp = isError ? selfCal.antVisErr() : 
                                                 selfCal.antVis();

            if ( displayAmp ) {
                mp.setStringRepresentation( MonitorPointComplex::AMP );
                mp.setUnits( "Jy" );
                mp.setPrecision( PRECISION );
            } else {
                mp.setStringRepresentation( MonitorPointComplex::PHASE );
                mp.setUnits( "Deg" );
                mp.setPrecision( PRECISION );
            }

            ostringstream rowName;
            rowName << "Astroband " << bpp.first 
                    << PolMPE::valueToString( bpp.second ) << " ";
            mp.setShortName( rowName.str( ) );
            complexContainer->add( mp );
        }

        return complexContainer;

    } // makeComplexContainer

    void
    populateMonitorContainerVector(
                          vector< MonitorContainerPtr > & monitorContainerVec,
                          const vector< int > & mappedAntNos,
                          const vector< BandPolPair > & mappedBandPols,
                          AstroSubsystem & astro,
                          const bool isUsb, // Otherwise lsb
                          const bool displayAmp, // Otherwise display phase
                          const bool isError )
    {

        BOOST_FOREACH( const int antNo, mappedAntNos ) {
            monitorContainerVec.push_back( 
                makeComplexContainer( astro,
                                      antNo,
                                      mappedBandPols,
                                      isUsb,
                                      displayAmp,
                                      isError ) );
        }

    } // populateMonitorContainerVector

    MonitorContainerPtr
    makeSnrContainer( AstroSubsystem & astro, 
                      const int antennaNo,
                      const vector< BandPolPair > & mappedBandPols,
                      const bool isUsb ) // Otherwise lsb
    {
        MonitorContainerPtr snrContainer( new MonitorContainer( "Dumb" ) );
        BOOST_FOREACH( const BandPolPair & bpp, mappedBandPols ) 
        {
            AstroSubsystem::SelfCal & selfCal = getSelfCal( astro,
                                                            antennaNo,
                                                            bpp.first,
                                                            bpp.second,
                                                            isUsb );

            MonitorPointFloat & mp = selfCal.snr();

            mp.setPrecision( PRECISION );

            ostringstream rowName;
            rowName << "Astroband " << bpp.first 
                    << PolMPE::valueToString( bpp.second ) << " ";
            mp.setShortName( rowName.str( ) );
            snrContainer->add( mp );
        }

        return snrContainer;

    } // makeSnrContainer

    void
    populateSnrMonitorContainerVector(
                    vector< MonitorContainerPtr > & monitorContainerVec,
                    const vector< int > & mappedAntNos,
                    const vector< BandPolPair > & mappedBandPols,
                    AstroSubsystem & astro,
                    const bool isUsb )    // Otherwise lsb
    {
        BOOST_FOREACH( const int antNo, mappedAntNos ) {
            monitorContainerVec.push_back( 
                makeSnrContainer( astro,
                                  antNo, 
                                  mappedBandPols, 
                                  isUsb ) );
        }
    } // populateSnrMonitorContainerVector

    vector< string >
    makeColumnLabels( const vector< int > & mappedAntNos )
    {
        vector< string > labels;
        BOOST_FOREACH( const int antNo, mappedAntNos ) {
            ostringstream os;
            os << "C" << antNo;
            labels.push_back( os.str( ) );
        }
        return labels;
    } // makeColumnLabels

    bool
    isSlMode( )
    {
        const string modeString =
            Program::getProgram( ).getStringParameter( "string2" );

        if ( modeString == "SL" ) {
            return true;
        } else if ( modeString == "WB" ) {
            return false;
        } else {
            throw CARMA_EXCEPTION( IllegalArgumentException,
                                   "string2 must be either SL or WB." );
        }
    }

    string
    createTitle( const bool slMode )
    {
        ostringstream heading;

        if ( slMode )
            heading << "Spectral Line ";
        else
            heading << "Wideband ";

        heading << "Self Calibration Results for Integrated Data";

        return heading.str( );
    }

// Convert a vector of shared_ptrs to normal ptrs until downstream supports 
// proper memory management.
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

void
runDisplay( const bool sl )
{
    MonitorDisplay display( createTitle( sl ) );
    display.setSpecificHelp( "Self Calibration Help", "Help" );
    
    SignalPathMapping spm( 
        display.cms(),  
        sl ?  corrTypeToCorrDes(CORR_SPECTRAL):
        corrTypeToCorrDes(CORR_WIDEBAND) );

    display.cms().readNewest();
    spm.update();

    AstroSubsystem & astro = display.cms().astro();
    const vector< int > mappedAnts = spm.getMappedAntennaNumbers( );
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
        return;
    }

    // Window format is 6 tabs (USB & LSB Amp, Phase and SNR)
    // Each tab has a table of rows=bands, cols=ants
    vector< string > columnLabels = makeColumnLabels( mappedAnts );

    vector< MonitorContainerPtr > usbSNR;
    vector< MonitorContainerPtr > lsbSNR;
    vector< MonitorContainerPtr > usbAmp;
    vector< MonitorContainerPtr > lsbAmp;
    vector< MonitorContainerPtr > usbPhase;
    vector< MonitorContainerPtr > lsbPhase;
    vector< MonitorContainerPtr > usbAmpError;
    vector< MonitorContainerPtr > lsbAmpError;
    vector< MonitorContainerPtr > usbPhaseError;
    vector< MonitorContainerPtr > lsbPhaseError;

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

    populateSnrMonitorContainerVector( usbSNR, 
                                       mappedAnts,
                                       mappedBandPols,
                                       astro,
                                       true );
    MonitorSingleTableFolderPtr usbSnrFolder(new MonitorSingleTableFolder(
                "SNR USB", columnLabels, convertToNonShared( usbSNR ), COLUMN_WIDTH, 1 ));
    usbSnrFolder->add( notifyBox );

    populateSnrMonitorContainerVector( lsbSNR,
                                       mappedAnts,
                                       mappedBandPols,
                                       astro,
                                       false );
    MonitorSingleTableFolderPtr lsbSnrFolder(new MonitorSingleTableFolder(
                "SNR LSB", columnLabels, convertToNonShared( lsbSNR ), COLUMN_WIDTH, 1 ));
    lsbSnrFolder->add( notifyBox );

    populateMonitorContainerVector( usbAmp,
                                    mappedAnts,
                                    mappedBandPols,
                                    astro,
                                    true,
                                    true,
                                    false );
    MonitorSingleTableFolderPtr usbAmpFolder(new MonitorSingleTableFolder(
                "Amps USB", columnLabels, convertToNonShared( usbAmp ), COLUMN_WIDTH, 1 ));
    usbAmpFolder->add( notifyBox );
                          

    populateMonitorContainerVector( lsbAmp,
                                    mappedAnts,
                                    mappedBandPols,
                                    astro,
                                    false,
                                    true,
                                    false );
    MonitorSingleTableFolderPtr lsbAmpFolder(new MonitorSingleTableFolder(
                "Amps LSB", columnLabels, convertToNonShared( lsbAmp ), COLUMN_WIDTH, 1 ));
    lsbAmpFolder->add( notifyBox );

    populateMonitorContainerVector( usbPhase,
                                    mappedAnts,
                                    mappedBandPols,
                                    astro,
                                    true,
                                    false,
                                    false );
    MonitorSingleTableFolderPtr usbPhaseFolder(new MonitorSingleTableFolder(
                "Phase USB", columnLabels, convertToNonShared( usbPhase ), COLUMN_WIDTH, 1 ));
    usbPhaseFolder->add( notifyBox );

    populateMonitorContainerVector( lsbPhase,
                                    mappedAnts,
                                    mappedBandPols,
                                    astro,
                                    false,
                                    false,
                                    false );
    MonitorSingleTableFolderPtr lsbPhaseFolder(new MonitorSingleTableFolder(
                "Phase LSB", columnLabels, convertToNonShared( lsbPhase ), COLUMN_WIDTH, 1 ));
    lsbPhaseFolder->add( notifyBox );

    populateMonitorContainerVector( usbAmpError,
                                    mappedAnts,
                                    mappedBandPols,
                                    astro,
                                    true,
                                    true,
                                    true );
    MonitorSingleTableFolderPtr usbAmpErrorFolder(new MonitorSingleTableFolder(
                "Amp Errors USB", columnLabels, convertToNonShared(usbAmpError), COLUMN_WIDTH, 1 ));
    usbAmpErrorFolder->add( notifyBox );

    populateMonitorContainerVector( lsbAmpError,
                                    mappedAnts,
                                    mappedBandPols,
                                    astro,
                                    false,
                                    true,
                                    true );
    MonitorSingleTableFolderPtr lsbAmpErrorFolder(new MonitorSingleTableFolder(
                "Amp Errors LSB", columnLabels, convertToNonShared(lsbAmpError), COLUMN_WIDTH, 1 ));
    lsbAmpErrorFolder->add( notifyBox );

    populateMonitorContainerVector( usbPhaseError,
                                    mappedAnts,
                                    mappedBandPols,
                                    astro,
                                    true,
                                    false,
                                    true );
    MonitorSingleTableFolderPtr usbPhaseErrorFolder(new MonitorSingleTableFolder(
                "Phase Errors USB", columnLabels, convertToNonShared( usbPhaseError ), COLUMN_WIDTH, 1 ));
    usbPhaseErrorFolder->add( notifyBox );

    populateMonitorContainerVector( lsbPhaseError,
                                    mappedAnts,
                                    mappedBandPols,
                                    astro,
                                    false,
                                    false,
                                    true );
    MonitorSingleTableFolderPtr lsbPhaseErrorFolder(new MonitorSingleTableFolder(
                "Phase Errors LSB", columnLabels, convertToNonShared( lsbPhaseError ), COLUMN_WIDTH, 1 ));
    lsbPhaseErrorFolder->add( notifyBox );

    display.add( usbSnrFolder );
    display.add( lsbSnrFolder );
    display.add( usbAmpFolder );
    display.add( lsbAmpFolder );
    display.add( usbPhaseFolder );
    display.add( lsbPhaseFolder );
    display.add( usbAmpErrorFolder );
    display.add( lsbAmpErrorFolder );
    display.add( usbPhaseErrorFolder );
    display.add( lsbPhaseErrorFolder );

    bool signalPathModified = false;
    while ( display.serveData( ) ) {
        signalPathModified = spm.update();
        if ( signalPathModified ) {
            notifyCell->setActiveAlternative( 0 );
            notifyCell->setColor( YELLOW_CELL_COLOR );
        }
    }
}

} // namespace <unnamed>

int
Program::main( ) try {

    const bool sl = isSlMode( );

    runDisplay( sl );

    return 0;

} catch (...) {
    logCaughtAsError( );
    return 1;
} // Program::main
