/**
 * @file
 *
 * RTD window to monitor CARMA processes.
 *
 * @author: Andrew Beard
 * $Id: rtdprocessmonitor.cc,v 1.18 2013/11/26 20:35:20 mpound Exp $
 *
 * $CarmaCopyright$
 */

#include <map>

#include "carma/monitor/ImrSubsystem.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ImrConfigHandlers.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/Trace.h"

using namespace std;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui::rtd;

namespace { // Anonymous for local riffraff.

    const string TITLE( "  Process Status" );
    const string helpTitle( "  Carma Process Status Info" );
    const string helpPageHeading( "  CARMA PROCESS HELP\n\n" );
    const string helpSummary( "  Status of Carma processes." );

    const unsigned int MAX_FRAMES_TO_WAIT_FOR_CONFIG = 5;

    ::std::string
    makeSpecificHelp( ) {
        ostringstream os;
        os << " Process Status Help \n\n"
            << "This display provides the status of all processes which "
            << "comprise the CARMA real-time system. Processes are organized "
            << "according to system type (e.g. data pipeline).  The displayed "
            << "name is the name of the process as registered with the IMR. "
            << "Likewise the states of each process are IMR server states ("
            << "described in detail below). \n\n"
            << "Here are some common process name abbreviations:\n"
            << " FSP: FrameScriberPublisher - publishes monitor data to acc.\n"
            << " SL: Spectral-Line.\n"
            << " WB: WideBand.\n"
            << " TH, CH, MH: Telemetry, Control and Monitor Hosts.\n"
            << " OT: Optical Telescope.\n"
            << " RM, DM: Receiver and Drive Manager.\n\n"
            << "IMR Server States\n"
            << " RUNNING: The process is running and functioning normally.\n"
            << " FORKED: An attempt has been made to start the process, "
            << "however, the process has not internally transitioned to "
            << "the RUNNING state.  This could be caused by a flaw in the "
            << "application or a misconfigured application.  Note that "
            << "in some cases monitor data may still come from the app. \n"
            << " STARTING: The process is in the process of starting up.\n"
            << " STOPPED: The process is not running probably because of "
            << "a bug in the application or a misconfigured xml server "
            << "entry.\n"
            << " UNKNOWN: I know nothing about this process!  This likely "
            << "means that the process exists in the xml but somehow "
            << "didn't get registered with the IMR - try a system "
            << "restart and file a bug.\n" 
            << "When a process is in an error state (e.g. Anything but "
            << "RUNNING), an attempt should be made to remedy the problem "
            << "via the IMR console (command line - imrConsole). If the "
            << "problem persists please file a bug report on the app."
            << " \n\n"
            << "Bugs: www.mmarray.org/bugzilla/\n"
            << "Requests, Comments or Donations: abeard@ovro.caltech.edu\n"
            << "\n\n";
        return os.str();
    };

    ::std::string 
    systemTypeToString( const SystemType sysType ) {
        switch ( sysType ) {
            case DATA: return "Data Pipeline";
            case MONITOR: return "Monitor System";
            case CONTROL: return "Control System";
            case INTERFEROMETRY: return "Control Building Electronics (Interferometry)";
            case CORRELATOR: return "Correlator System";
            case OVRO: return "Ovro Antenna";
            case BIMA: return "Bima Antenna";
            case SZA: return "Sza Antenna";
            case MISC: return "Miscellaneous";
        }
        return "Unknown";
     }

     int 
     getMaxServersPerLine( const SystemType sysType ) {
        switch ( sysType ) {
            case OVRO: return 6;
            case BIMA: return 9;
            case CORRELATOR: return 6;
            default:
                return 5;
       }
       return 5;
    }

    int 
    getServersPerLine( const SystemType sysType,
                       domainType & domain ) {
        int maxspl = getMaxServersPerLine( sysType );
        int serversPerLine = 0;
        if ( domain.nServersByType[sysType] > maxspl ) {
            serversPerLine = domain.nServersByType[sysType] / maxspl;
        } else {
            serversPerLine = maxspl;
        }
        return serversPerLine;
    }

    RtBoxPtr
    createSummaryBox ( domainType & domain,
                       const ImrSubsystem & mon ) { 
        RtAreaPtr area(new RtArea( "Summary Area" ));
        RtBoxPtr box(new RtVBox( " Summary Box" ));
        RtLabelPtr header(new RtLabel( " Summary " ));
        area->addItem(
            mon.numServers().getShortName( ),
            MonitorCell::makeCell( mon.numServers( ) ) );
        area->addItem(
            mon.numRunningServers().getShortName( ), 
            MonitorCell::makeCell( mon.numRunningServers( ) ) );
        area->addItem(
            mon.numNotRunningServers().getShortName( ), 
            MonitorCell::makeCell( mon.numNotRunningServers( ) ) );
        area->addItem( mon.configFilename().getShortName( ), 
                       MonitorCell::makeCell( mon.configFilename( ) ) );

        RtBoxPtr hbox(new RtHBox( "Crit Box" ));
        hbox->add(RtSpringPtr(new RtSpring( 1.0 )));
        hbox->add(RtLabelPtr(new RtLabel( mon.numCriticalServers( ).getShortName( ) )));
        hbox->add(RtSpacerPtr(new RtSpacer( 5 )));
        hbox->add( MonitorCell::makeCell( mon.numCriticalServers( ) ) ); 
        hbox->add(RtSpacerPtr(new RtSpacer( 40 ))); // Min 40 pixel spacer
        hbox->add(RtLabelPtr(new RtLabel( 
            mon.numNotRunningCriticalServers( ).getShortName( ) ) ));
        hbox->add(RtSpacerPtr(new RtSpacer( 5 )));
        hbox->add( MonitorCell::makeCell( mon.numNotRunningCriticalServers( )));
        hbox->add(RtSpringPtr(new RtSpring( 1.0 )));
        box->add( header );
        box->add( area );
        box->add( hbox );
        box->setBorder( ONE_PIXEL_BELOW_BORDER );

        return box;
    }
    
    RtBoxPtr
    createSystemBox ( domainType &         domain,
                      const ImrSubsystem & mon, 
                      const SystemType     sysType ) {
        string systemTypeName = systemTypeToString( sysType );
        RtAreaPtr area(new RtArea( systemTypeName ));
        int serversPerLine = getServersPerLine( sysType, domain );
        int sidx = 0; // Server index into monitor system
        int cidx = 0; // Column index 
        OadVector::const_iterator o = domain.oads.begin( );
        for ( ; o != domain.oads.end( ); ++o ) {
            ServerVector::const_iterator s = o->servers.begin( );
            for ( ; s != o->servers.end( ); ++s, ++sidx ) {
                // Fundamental predicate: Make sure servername matches xml.
                const string imrSubsysServerName =
                    mon.server( sidx ).name().getValue();
                
                if ( imrSubsysServerName != s->name ) {
                    throw CARMA_EXCEPTION( ErrorException, 
                            "ImrSubsystem server name \"" +
                            imrSubsysServerName +
                            "\" and xml config server name \"" + s->name +
                            "\" do not match!" );
                }
                
                if ( s->system == sysType ) {
                    MonitorPoint & mp = mon.server( sidx ).state();
                    MonitorCellPtr cell = MonitorCell::makeCell( mp );
                    area->addItem( mon.server( sidx ).name().getValue(), cell );
                    cell->setGoodColor( GREEN_CELL_COLOR );
                    ++cidx;
                    if ( cidx % serversPerLine == 0 ) 
                        cell->setLayout( EOL_CENTERED_LAYOUT );
                }
            }
        }
       
        if ( cidx > 0 ) {
            RtBoxPtr vb(new RtVBox( systemTypeName ));
            RtLabelPtr header(new RtLabel( systemTypeName + " Processes " ));
            vb->add(RtSpacerPtr(new RtSpacer( 5, 5, 3.0 )));
            vb->add( header );
            vb->add(RtSpacerPtr(new RtSpacer( 3, 5, 0 )));
            vb->add( area );
            vb->add(RtSpacerPtr(new RtSpacer( 5, 5, 8.0 )));
            vb->setBorder( ONE_PIXEL_BELOW_BORDER );
            return vb;
        } else {
            return RtBoxPtr();
        }
    } // End createSystemBox
    
    void 
    addBoxToFolder( RtBoxPtr box, 
                           RtFolderPtr folder ) {
        if ( box.get() != 0 )
            folder->add( box );
        
    } // End addBoxToFolderIfValid
        
    // Create the entire display from parsed XML config domain
    void
    layoutDisplay( domainType & domain,
                   MonitorDisplay &   display ) {

        const ImrSubsystem & mon = display.cms( ).imr( );
        
        RtFolderPtr mainFolder(new RtFolder( " Main " ));
        RtFolderPtr antFolder(new RtFolder( " Antennas " ));
        RtFolderPtr corrFolder(new RtFolder( " Correlator " ));
        addBoxToFolder( createSummaryBox( domain, mon ), mainFolder );
        addBoxToFolder( createSystemBox( domain, mon, DATA ), mainFolder );
        addBoxToFolder( createSystemBox( domain, mon, CONTROL ), mainFolder );
        addBoxToFolder( createSystemBox( domain, mon, MONITOR ), mainFolder );
        addBoxToFolder( createSystemBox( domain, mon, INTERFEROMETRY ), mainFolder );
        addBoxToFolder( createSystemBox( domain, mon, MISC ), mainFolder );

        addBoxToFolder( createSystemBox( domain, mon, OVRO ), antFolder );
        addBoxToFolder( createSystemBox( domain, mon, BIMA ), antFolder );
        addBoxToFolder( createSystemBox( domain, mon, SZA ), antFolder );
        addBoxToFolder( createSystemBox( domain, mon, CORRELATOR ), corrFolder );
        display.add( mainFolder );
        display.add( corrFolder );
        display.add( antFolder );
        
    }; // End layoutDisplay
        
} // End namespace anonymous

int
Program::main( ) {
    // Create monitor system and try to get the first valid config filename. If 
    // we don't get a valid conf filename within MAX_FRAMES_TO_WAIT_FOR_CONFIG
    // exit with an error.
    MonitorDisplay display( TITLE );
    display.setSpecificHelp( helpPageHeading, makeSpecificHelp() );
    display.cms( ).readNewest( );
    carma::monitor::ImrSubsystem & imrmon = display.cms( ).imr( );
    MonitorPoint::VALIDITY confValid = imrmon.configFilename( ).getValidity( );
    unsigned int frame = 0;
    while ( confValid < MonitorPoint::VALID && 
            frame < MAX_FRAMES_TO_WAIT_FOR_CONFIG ) {
        confValid = imrmon.configFilename( ).getValidity();
        ++frame;
        display.cms( ).read( );
    }

    if ( confValid < MonitorPoint::VALID ) {
        programLogErrorIfPossible( " Unable to retrieve valid conf filename -" 
            " double check that both the processMonitor server and FSP are"
            " alive and well." );
        return 1;
    } // Else we're good

    const string conffile = imrmon.configFilename( ).getValue( ); 

    CARMA_CPTRACE( Trace::TRACE3, "Configuration filename: " << conffile );

    // Parse - THE PARSED XML DETERMINES THE STATIC STRUCTURE OF THE RTD PAGE!
    domainType domain = parseXmlConfig( conffile, false, false );

    // Based on system type (i.e. DATA, MONITOR, CONTROL, etc), layout rtd.
    layoutDisplay( domain, display );

    // Serve data indefinitely.
    while ( display.serveData( ) );

    return 0;

} // End Program::main
