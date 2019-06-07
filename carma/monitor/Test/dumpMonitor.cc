
/**
 *
 * Outputs (to stdout) any part of a monitor subsystem tree,
 * including component names, values, and validity.
 * The tree can be from the ACC or (optionally) the subsystem.
 * The output in continuous, with each subsequent frame being dumped
 * until the requested number of repetitions is complete.
 * The orgin of the data is the monitor system shared memory, and this
 * requires a running frameCollator (for the ACC) or a frameScriberPublisher
 * if run in the subsystem. If no data is available because these programs
 * have never been run then this program will exit.
 *
 * This program will also optionally dump the 
 *  o names of the subsystems if run on the ACC.
 *  o dump statistics about monitor data transport, either in the subsystem,
 *    or in the ACC.
 * 
 * The output of this program can be redirected to a file to create 
 * an instant logger; or it can be piped to grep for a quick monitor.
 *
 * @author: Steve Scott
 *
 * $Id: dumpMonitor.cc,v 1.47 2012/08/21 23:31:54 eml Exp $
 *
 * $CarmaCopyright$
 *
 */

// Keyword setup...
// @version     $Revision: 1.47 $ $Date: 2012/08/21 23:31:54 $
// @usage dumpMonitor [ave=t/f] [verbose=f/t] [subsystem=s] [list=f/t] [component=c] [frames=f] [mode=[\"carma\"]|\"raw\"|\"final\"] [usedb=[false]] [dbconffile=[dbms/dbms.conf]]
// @key verbose    false  bool    Verbose output (all monitor point attributes)
// @key ave        true   bool    Dump average value or first sample
// @key list       false  bool    List subsystem names at start
// @key frames     100    int     Number of frames to dump
// @key levels     -1     int     Number of levels under component to dump
// @key subsystem  ""     string  Optional subsystem name (case insensitive) - used only when run in subsystem
// @key mode       carma  string  \"raw\", \"carma\", or \"final\" - reads \"raw\", \"carma\", or \"final\" IPQ
// @key component  all    string  Monitor system component to dump; hierarchical name, case insensitive, *=root of system.  Can be a list of components separated by a space or comma. 
// @key stats      no     string  If yes, y, t, true, or subsystem dumps stats about end-to-end transport, with times in msecs
// @key maxstats   false  bool    If yes, y, t, or true dumps maximum end-to-end transport, with times in msecs
// @key usedb      false  bool    configure TagIDAuthority to retrieve tagIDs from a database
// @key dbconffile dbms/dbms.conf  string   file from which to get db configuration information if usedb=t, with the file location interpreted by Program::getConfFile()
// @key iterator   false  bool    gives timing for iterator if true
// @description
//      Simple program to read and dump the carma monitor system.
//      It can read all of the system or just a hierarchical piece of it 
//      (as small as a single monitor point). This program works off of
//      shared memory, not CORBA, and loops reading new frames and dumping
//      them until the requested number of frames has been dumped. 
//      By default the average values and validity are output,
//      but a command line parameter will direct the 1st sample to
//      be output instead. The validity is indicated right after the
//      values with a single character (g=good, b=bad).
//      A listing of subsystem names can optionally be selected with a keyword.
//      The output of this program can be redirected to a file to create 
//      an instant logger; or it can be piped to grep for a quick monitor.
//      You do not need to specify an imr.
//      The program operates in three basic modes: 
//        Subsystem, ACC, and Stats 
//      SUBSYSTEM MODE:
//      This mode is triggered when the subsystem is specified by the
//      subsystem keyword and statistics=false (default).
//      For this mode to work correctly you must be running on the machine
//      that is the host for the subsystem. The component keyword is then
//      used to specify a path relative to the subsystem, and defaults 
//      to all of the subsystem.
//      ACC MODE:
//      When the subsystem keyword is left as the default of \"carma\"
//      the program runs in ACC mode. In this case you must run on the
//      ACC and input a hierarchical component name (with the component keyword),
//      beginning with the subsystem. The component name defaults to \"all\",
//      which is all of carma, which is very large! The dump is hierarchical,
//      including and below your requested component. 
//      STATISTICS MODE:
//      This mode is initiated with stats keyword with values of:
//        stats=yes, y, true, t  dump requested subsystem from ACC shared memory
//        stats=subsystem       dump requested subsystem from subsys shared mem
//        stats=no, n, false, f  no stats
//      This mode can be  run on the ACC or a subsystem and it 
//      supplies information about the time taken to transport monitor 
//      data from the producers of the data to the ACC/subsystem as well as 
//      the various delay settings in the transport agents that influence 
//      these transport times. If no subsystem is specified then the times 
//      for all are dumped;
//      if a specific subsystem is requested with the subsystem keyword
//      then just the times for that subsystem are printed as gotten from
//      from the subsystem (not ACC) shared memory. The component
//      keyword is ignored in this mode.
//      Levels: levels=0 (default) dumps all levels
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

void dumpSubsystems() 
{
    MonitorSystem&   carma = *new CarmaMonitorSystem();
    for (int i=0; i<carma.getNumChildren(); i++) {
        cout << "Subsystem:"
             << carma.getChild(i).componentRef().toString(false, false, false)
             << endl;
    }
    delete &carma;
}

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
    // Control variables for hierarchyToString()
    bool canonical        = true;
    bool verbose          = getBoolParameter("verbose");
    bool value            = true;
    bool doStats          = ((getStringParameter("stats") != "no")
                             &&  (getStringParameter ("stats") != "n")
                             &&  (getStringParameter ("stats") != "f")
                             &&  (getStringParameter ("stats") != "false"));
    bool subsystemStats   = (getStringParameter("stats") == "subsystem");    
    bool doAverage        = getBoolParameter("ave");
    bool doIteratorTiming = getBoolParameter("iterator");
    int  levels           = getIntParameter("levels");
    bool doMaxStats       = getBoolParameter("maxstats");

    cout.setf(ios::fixed);    
    
    const bool doingASubsystem = parameterWasSpecified("subsystem");

    if (doStats  &&  (getStringParameter ("stats") != "yes") 
                 &&  (getStringParameter ("stats") != "y")
                 &&  (getStringParameter ("stats") != "t")
                 &&  (getStringParameter ("stats") != "true"))  {
        std::cerr << "stats can only be \"yes\", \"y\", \"true\", \"t\""
                  << ", \"n\", \"no\", \"false\", \"f\", or \"subsystem\"."
                  << "\nExiting..."
                  << std::endl;
        return EXIT_FAILURE;
    }
    if (subsystemStats && (doingASubsystem == false)) {
        cerr << "If stats=subsystem, then a subsystem name must be entered"
             << " with the subsystem keyword. Exiting..." << endl;
        return EXIT_FAILURE;
    }

    // Dump of subsystem names    
    if (Program::getBoolParameter("list")) {
        // Define monitor system
        cout << "---------- List of carma subsystems ------------" << endl;
        dumpSubsystems();
        cout << "------------------------------------------------" << endl;
        return EXIT_SUCCESS;
    }
     
        
     // Define monitor system
    MonitorSystemContainer* msc;
    
    if ((doStats && !subsystemStats) || (doingASubsystem == false)) { 
        // Use ACC shared memory
        const string modeString( getStringParameter( "mode" ) );
        if ( modeString == "raw" ) {
            msc = new RawCarmaMonitorSystem();
        } else if ( modeString == "final" ) {
            msc = new FinalCarmaMonitorSystem();
        } else {
            msc = new CarmaMonitorSystem();
        }
    }
    else {
        const string subsys = getStringParameter("subsystem");
        try {
            msc = &MonitorSubsystemMaster::makeSubsystem(subsys);
        } catch ( const util::IllegalArgumentException & ex ) {
            cerr << "Subsystem " << subsys << " doesn't exist." << endl;
            cerr << "List of subsystems:" << endl;
            dumpSubsystems();
            return EXIT_FAILURE;
        }
    }
    
    //    bool alive = msc->isActive();
    bool alive = true;
    if (alive == false) {
      if (doingASubsystem) {
        cerr << "This subsystem is not receiving data updates." << endl;
        cerr << "Make sure that you are running this program"
             << " on the machine that hosts the subsystem." << endl;
      } 
      else {
        cerr << "This system is not receiving data updates." << endl;
        cerr << "Make sure that you are running this program "
             << "on the ACC computer!" << endl;
      }
      
      if (doStats) 
        return EXIT_FAILURE;
        
      // We continue so that the ss can be dumped - might just
      // be interested in seeing its structure...
    }
       
    // Get component(s) to dump...
    MonitorComponentVec mVec;

    {
        if (parameterWasSpecified("component") == false) {
            // If no component is specified, then dump the whole container
            mVec.push_back( msc );
        }  else  {

            const string compParamLine = getStringParameter("component");

            const StringVec components = parseCompParamLine( compParamLine );

            StringVec::const_iterator compName = components.begin( );
            const StringVec::const_iterator compNameEnd = components.end( );
            for ( ; compName != compNameEnd; ++compName ) {
                const MonitorComponent *m = msc->getComponentPtr( *compName, 
                                                                  false );
                if ( m == 0 ) {
                    cerr << "Could not find requested component(" << *compName
                        << ") underneath " << msc->getName() << ";\n"
                        << "  try moving higher up the tree - exiting..." 
                        << endl;
                    return 0;
                }

                mVec.push_back( m );
            }
        }
    }
    
    if (!alive) {
        cout << "Data feed is not active -- exiting..." << endl;
        return EXIT_FAILURE;
    }
    
    
    if (doStats)  {
        cout << "All times are in milliseconds, measured as offsets from\n"
             << "the half-second for the monitor frame.\n"
             << endl;
        cout << MonitorSubsystem::transportHeaderToString();
    }
    
    if (doIteratorTiming) {
        cout << "Output: frame count, num MPs, time, time per 10k MPs, ave time/10k\n"
             <<" with times in milliseconds" << endl;
    }
  
    double deltasum = 0;  // For iterator timing
    // Loop
    int frames = getIntParameter("frames");   
    for (int frame=0; frame < frames; frame++) {
        msc->read();
        if (!doIteratorTiming && !doMaxStats) {
            frameType f = Time::computeClosestFrame();
            cout << "frame#=" << frame+1 << "    " 
                 << Time::getTimeString(f, 1) 
                 << ", dataframe=" << msc->getFrameCount() << endl; 
        }  
        if (doStats)  {
               cout << msc->transportStatisticsToString(canonical);
        }
        else if (doMaxStats)  {
            double maxReceiveTime = 0;
            for (int i=0; i<msc->getNumChildren(); i++) {
                const MonitorSubsystem * const ms =
                    msc->getChild(i).subsystemPtr();

                if ( ms != 0 )
                    maxReceiveTime = max(ms->getReceiveTime(), maxReceiveTime);
            }
            frameType f = Time::computeClosestFrame();
            double t0 = Time::MJD(msc->getFrameCount()+1);
            cout << "frame=" << frame+1 << "    " 
                 << Time::getTimeString(f, 1) 
                 << "   " 
                 << static_cast< int >(1000*Time::SECONDS_PER_DAY*(maxReceiveTime-t0))
                 << endl; 
        }
        else if (doIteratorTiming) {
            MonitorPointIterator mpi(*msc);
            int count = 0;
            double start = Time::MJD();
            while (mpi++) {
                count++;
            }
            double delta = 1000*Time::SECONDS_PER_DAY*(Time::MJD()-start);
            deltasum += delta;
            cout << setw(4) << frame+1 << "   "
                 << count << "   " << setprecision(2) << delta << "   " 
                 << 10000*delta/count << "  "
                 << 10000*deltasum/(count*(frame+1)) 
                 << endl;
        }
        else {

            MonitorComponentVec::const_iterator monComp = mVec.begin( ); 
            const MonitorComponentVec::const_iterator monCompEnd = mVec.end( );
            for ( ; monComp != monCompEnd; ++monComp ) {

                // It is convenient to use a reference for the top of the tree
                const MonitorComponent &tree = *(*monComp);

                if (doAverage) {
                    cout << tree.hierarchyToStringAverage(canonical, 
                            verbose, value,0,levels);
                }
                else {
                    cout << tree.hierarchyToString(canonical, 
                            verbose, value,0,0,levels);
                }
            }
        }

    }

    return EXIT_SUCCESS;
}
