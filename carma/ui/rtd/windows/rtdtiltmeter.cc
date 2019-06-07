/** @file
 * Gets data from the carma ovro tiltmeter subsystem and displays.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.4 $
 * $Date: 2013/11/19 03:41:15 $
 * $Id: rtdtiltmeter.cc,v 1.4 2013/11/19 03:41:15 iws Exp $
 *
 * $CarmaCopyright$
 */
         

#include <sstream>
#include <vector>

#include "carma/monitor/BimaSubsystem.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/monitor/SzaSubsystem.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/Program.h"

using namespace std;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui::rtd;
using namespace carma::services;

namespace {

typedef enum {
    BIMA,
    OVRO,
    SZA } AntType;

AntType
getAntType( const string & antString ) 
{
    if ( antString == "bima" ) return BIMA;
    if ( antString == "ovro" ) return OVRO;
    if ( antString == "sza" ) return SZA;

    throw CARMA_EXCEPTION( IllegalArgumentException, 
                           "Invalid ant designator." );
}

string
getAntString( const AntType antType ) 
{
    switch ( antType ) {
        case BIMA: return "6m";
        case OVRO: return "10m";
        case SZA: return "3.5m";
        default: return "<error>";
    }
    return "<error>";
}

vector< string >
getAntNames( const AntType antType ) 
{
    switch ( antType ) {
        case BIMA: return RtTable::getBimaAntNames();
        case OVRO: return RtTable::getOvroAntNames();
        case SZA: return RtTable::getSzaAntNames();
        default: break;
    }

    throw CARMA_EXCEPTION( IllegalArgumentException, 
                           "Illegal ant type." );
}

string 
makeHelp( const string & antString ) {
    ostringstream ost;
    ost
    << "       " << antString << " ANTENNA TILTMETER HELP\n\n" 
    << "Status of the " << antString << " antenna tiltmeter CAN module. "
    << "Fill in more info here... "
    ;    
    return ost.str();
}

template < typename S >
void 
populateMonitorContainerVecs( vector< MonitorContainer * > & tiltmeters,
                              vector< MonitorContainer * > & xacs,
                              S & monitorSubsystem )
{
    // Deliberately leaks
    MonitorContainer * tiltmeter = new MonitorContainer("Tiltmeter");
    tiltmeter->add( monitorSubsystem.tiltmeter() );
    tiltmeters.push_back( tiltmeter );

    // Deliberately leaks
    MonitorContainer * xac = new MonitorContainer("Xac"); 
    xac->add( monitorSubsystem.tiltmeter().state() );
    xac->add( monitorSubsystem.tiltmeter().xac() );
    xacs.push_back(xac);
}

} // namespace <unnamed>


int
Program::main() 
{
    const string string1Param = getStringParameter( "string1" );

    const AntType antType = getAntType( string1Param );
    const string antString = getAntString( antType );
    
    // Create a dislay
    MonitorDisplay display( antString + " Antenna Tiltmeter Status" );
    const string file = Program::getConfFile( "rtd/help/tiltmeter.html" );
    display.setSpecificHelpFromTextFile( antString + " Tiltmeter Help", file );

    int width = AUTO_SIZE;  
    vector<string> columnLabel = getAntNames( antType );
    vector<MonitorContainer*> tiltmeters;  
    vector<MonitorContainer*> xacs;  
    
    switch ( antType ) {
        case BIMA:
            {
                for ( int i = 0; i < BimaSubsystem::COUNT; ++i) {
                    BimaSubsystem & bima = display.cms().bima(i);
                    populateMonitorContainerVecs( tiltmeters, xacs, bima );
                }
            }
            break;
        case OVRO:
            {
                for ( int i = 0; i < OvroSubsystem::COUNT; ++i) {
                    OvroSubsystem & ovro = display.cms().ovro(i);
                    populateMonitorContainerVecs( tiltmeters, xacs, ovro );
                }
            }
            break;
        /* case SZA:
            {
                for ( int i = 0; i < SzaSubsystem::COUNT; ++i) {
                    SzaSubsystem & sza = display.cms().sza(i);
		            populateMonitorContainerVecs( tiltmeters, xacs, sza );
                }
            }
            break;
         */
        default:
            throw CARMA_EXCEPTION( IllegalArgumentException, 
                                   "Illegal ant type." );
            break;
    }

    MonitorSingleTableFolderPtr folder1(new MonitorSingleTableFolder(
                "tiltmeter", columnLabel, tiltmeters, width, 2));
    MonitorSingleTableFolderPtr folder2(new MonitorSingleTableFolder(
                "tiltmeter (xac)", columnLabel, xacs, width, 2));

    // Add the folders to the display
    display.add(folder1);
    display.add(folder2);

    while ( display.serveData( ) ) {} // Loop forever serving data to the client

    return 0;
}
