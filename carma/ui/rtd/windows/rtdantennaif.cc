/*
 * @file
 * 
 * Retrieves data from the Ovro subsystem and displays.
 *
 * @author Original: Andy Beard (based on code from Steve Scott)
 * $id: $
 *
 * $CarmaCopyright$
 */


#include <sstream>
#include <vector>

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/BimaSubsystem.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/monitor/SzaSubsystem.h"
#include "carma/util/Program.h"
#include "carma/util/IllegalArgumentException.h"

using namespace std;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::util;
using namespace carma::ui::rtd;

namespace {

typedef enum {
    BIMA,
    OVRO } AntType;

AntType
getAntType( const string & antString ) 
{
    if ( antString == "bima" ) return BIMA;
    if ( antString == "ovro" ) return OVRO;

    throw CARMA_EXCEPTION( IllegalArgumentException, 
                           "Invalid ant designator: " + antString + "." );
}

string
getAntString( const AntType antType ) 
{
    switch ( antType ) {
        case BIMA: return "6m";
        case OVRO: return "10m";
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
        default: break;
    }

    throw CARMA_EXCEPTION( IllegalArgumentException, 
                           "Illegal ant type." );
}

string 
makeHelp() 
{
    ostringstream ost;
    ost
    << "       ANTENNA IF MODULE HELP\n\n" 
    << "Status of the Antenna IF CAN module. "
    << "Fill in more info here... "
    ;    
    return ost.str();
}

template < typename S >
void
populateMonitorContainerVecs( vector< MonitorContainer * > & if1s,
                              vector< MonitorContainer * > & if2s,
                              vector< MonitorContainer * > & xac1s,
                              vector< MonitorContainer * > & xac2s,
                              S & monitorSubsystem )
{
    // Deliberately leaks
    MonitorContainer * antif1 = new MonitorContainer("Antenna If1");
    antif1->add( monitorSubsystem.antennaIfContainer(0).state() );
    antif1->add( monitorSubsystem.antennaIfContainer(0).antennaIF() );
    if1s.push_back( antif1 );
    
    // Deliberately leaks
    MonitorContainer * antif2 = new MonitorContainer("Antenna If2");
    antif2->add( monitorSubsystem.antennaIfContainer(1).state() );
    antif2->add( monitorSubsystem.antennaIfContainer(1).antennaIF() );
    if2s.push_back( antif2 );

    // Deliberately leaks
    MonitorContainer * xac1 = new MonitorContainer("Xac1"); 
    xac1->add( monitorSubsystem.antennaIfContainer(0).state() );
    xac1->add( monitorSubsystem.antennaIfContainer(0).xac() );
    xac1s.push_back(xac1);
    
    MonitorContainer * xac2 = new MonitorContainer("Xac2"); 
    xac2->add( monitorSubsystem.antennaIfContainer(1).state() );
    xac2->add( monitorSubsystem.antennaIfContainer(1).xac() );
    xac2s.push_back(xac2);
}

void 
setupAndRunDisplay( MonitorDisplay & display, const AntType & antType ) 
{
    // const int width = AUTO_SIZE;
    vector<string> columnLabel;
    vector<MonitorContainer*> antennaIf1s;
    vector<MonitorContainer*> antennaIf2s;
    vector<MonitorContainer*> xac1s;
    vector<MonitorContainer*> xac2s;

    int count = 0;
    if ( antType == BIMA ) {
        columnLabel = getAntNames( BIMA );
        count = BimaSubsystem::COUNT;
    } else if ( antType == OVRO ) {
        columnLabel = getAntNames( OVRO );
        count = OvroSubsystem::COUNT;
    }

    for ( int i = 0; i < count; ++i) {
        
        if ( antType == BIMA ) {
            populateMonitorContainerVecs( antennaIf1s,
                    antennaIf2s,
                    xac1s,
                    xac2s,
                    display.cms().bima( i ) );
        } else if ( antType == OVRO ) {
            populateMonitorContainerVecs( antennaIf1s,
                    antennaIf2s,
                    xac1s,
                    xac2s,
                    display.cms().ovro( i ) );
        }
    }
        

    MonitorSingleTableFolderPtr if1Folder(new MonitorSingleTableFolder(
                "   IF1   ", columnLabel, antennaIf1s, AUTO_SIZE, 2 ));
    MonitorSingleTableFolderPtr if2Folder(new MonitorSingleTableFolder(
                "   IF2   ", columnLabel, antennaIf2s, AUTO_SIZE, 2 ));
    MonitorSingleTableFolderPtr xac1Folder(new MonitorSingleTableFolder(
                "IF1 (xac)", columnLabel, xac1s, AUTO_SIZE, 2 ));
    MonitorSingleTableFolderPtr xac2Folder(new MonitorSingleTableFolder(
                "IF2 (xac)", columnLabel, xac2s, AUTO_SIZE, 2 ));
    
    display.add(if1Folder);
    display.add(if2Folder);
    display.add(xac1Folder);
    display.add(xac2Folder);
    
    while (display.serveData( ) ); // Loop forever serving data to the client
}


} // namespace <unnamed>

int
Program::main() {
    
    const string string1Param = getStringParameter( "string1" );

    const AntType antType = getAntType( string1Param );
    const string antString = getAntString( antType );

    // Create a dislay
    MonitorDisplay display( antString + " Antenna IF Module Status" );
    display.setSpecificHelp( antString + " Antenna IF Module Help", 
                             makeHelp( ) );

    switch ( antType ) {
        case OVRO:
        case BIMA:
            setupAndRunDisplay( display, antType );
            break;
        default:
            throw CARMA_EXCEPTION( IllegalArgumentException,
                                   "Invalid ant type." );
            break;
    }

    return 0;
}
