
/**
 *
 * Outputs (to stdout) any part of a monitor subsystem tree,
 * just the canonical name and the archive priority.
 *
 * @author: Steve Scott
 *
 * $Id: dumpArchivePriority.cc,v 1.1 2011/05/10 00:35:20 scott Exp $
 *
 * $CarmaCopyright$
 *
 */

// Keyword setup...
// @version     $Revision: 1.1 $ $Date: 2011/05/10 00:35:20 $
// @usage dumpArchivePriority [component=c] 
// @key component  all    string  Monitor system component to dump; hierarchical name, case insensitive, *=root of system.  Can be a list of components separated by a space or comma. 
// @key levels     -1     int     Number of levels under component to dump
// @key mode       final  string  \"raw\" or \"final\" - reads \"raw\" IPQ or \"final\" IPQ
// @description
//      Simple program to read and dump the arhive priority of the
//      carma monitor system.

//
// @logger MONITOR_FACILITY carma.monitor.dumpMonitor

#include <iomanip>
#include <iostream>

#include "carma/util/IllegalArgumentException.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/MonitorSubsystemMaster.h"
#include "carma/monitor/MonitorPointIterator.h"


using namespace std;
using namespace carma;
using namespace carma::util;
using namespace carma::monitor;

namespace {



void
removeLeadingWhitespaceAndCommas( string & str ) 
{
    if ( str.empty( ) ) return;

    const string delims( ", \t" );
    const string::size_type firstNonDelIdx = str.find_first_not_of( delims );
    if ( firstNonDelIdx != string::npos ) {
        str = str.substr( firstNonDelIdx );
    } else {
        // String is all delims, force to empty 
        str = "";
    }
}
    
typedef vector< string > StringVec;

StringVec 
parseCompParamLine( string components )
{
    StringVec result;

    removeLeadingWhitespaceAndCommas( components ); 

    // Components can be in a comma, space or tab separated list.  
    // Strip off individual components until there are no more.
    while ( components.size( ) > 0 ) {

        string::size_type delimIdx = components.find_first_of( ", \t");
        if ( delimIdx == string::npos ) {
            result.push_back( components );
            components = "";
        } else {
            result.push_back( components.substr( 0, delimIdx ) );
            components = components.substr( delimIdx );
        }
        removeLeadingWhitespaceAndCommas( components ); 
    }

    return result;
}

typedef vector< const MonitorComponent * > MonitorComponentVec;

}
    
int Program::main() 
{
    cout.setf(ios::fixed);    
    
    int  levels = getIntParameter("levels");

     // Define monitor system
    MonitorSystemContainer* msc;
    // Use ACC shared memory
    const string modeString( getStringParameter( "mode" ) );
    if ( modeString == "raw" ) {
        msc = new RawCarmaMonitorSystem();
    } else if ( modeString == "final" ) {
        msc = new FinalCarmaMonitorSystem();
    } else {
        msc = new CarmaMonitorSystem();
    }
       
    // Get component(s) to dump...
    MonitorComponentVec mVec;

    if (parameterWasSpecified("component") == false) {
            cout << "Must specify a component; exiting..." << endl;
            return 0;
    }  

    const string compParamLine = getStringParameter("component");
    const StringVec components = parseCompParamLine( compParamLine );

    StringVec::const_iterator compName = components.begin( );
    const StringVec::const_iterator compNameEnd = components.end( );
    for ( ; compName != compNameEnd; ++compName ) {
        const MonitorComponent *m = msc->getComponentPtr(*compName, false );
        if ( m == 0 ) {
            cerr << "Could not find requested component(" << *compName
                 << ") underneath " << msc->getName() << ";\n"
                 << "  try moving higher up the tree - exiting..." 
                 << endl;
             return 0;
         }

         mVec.push_back( m );
    }
  
    msc->read();

    MonitorComponentVec::const_iterator monComp = mVec.begin( ); 
    const MonitorComponentVec::const_iterator monCompEnd = mVec.end( );
    for ( ; monComp != monCompEnd; ++monComp ) {

        // It is convenient to use a reference for the top of the tree
        const MonitorComponent &tree = *(*monComp);

        MonitorPointIterator mpi((MonitorContainer&)tree, levels);

        while (++mpi) {
            MonitorPoint& mp = mpi.getMonitorPoint();
            cout << setw(40) << left << mp.getCanonicalName();
            cout << "    " << mp.archivePriorityToString() << endl;
         }

    }

    return EXIT_SUCCESS;
}
