
/**
 *
 * Outputs (to stdout) a brief desciption of the full carma tree.
 * Redundant (mutilple count) containers and monitor points are suppressed.
 *

 *
 * @author: Steve Scott
 *
 * $Id: briefDump.cc,v 1.11 2010/07/21 17:58:28 abeard Exp $
 *
 * $CarmaCopyright$
 *
 */

// Keyword setup...
// @version     $Revision: 1.11 $ $Date: 2010/07/21 17:58:28 $
// @usage dumpMonitor [ave=t/f] [verbose=f/t]
// @key verbose    false  bool    Verbose output (all monitor point attributes)

// @description
//      Simple program to dump the carma tree to stdout.

//
// @logger TEST_FACILITY carma.test.monitor.briefDump

//#include <exception>

#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/MonitorSubsystemMaster.h"
#include "carma/monitor/MonitorPointIterator.h"

#include <iostream>

using namespace ::std;
using namespace carma::util;
using namespace carma::monitor;

vector<string> getSubsystemNames() 
{
    vector<string> v;
    v.push_back("Bima1");
    v.push_back("Ovro1");
    v.push_back("WbcBand1");
    v.push_back("Sldc");
    v.push_back("CentralIf");
    v.push_back("Control");
    //v.push_back("DelayEngine");
    v.push_back("LineLength");
    v.push_back("Loberotator");
    v.push_back("LoRef");
    v.push_back("MasterClock");
    v.push_back("WbPipeline");
    v.push_back("Weather");
    return v;
}

string toString(const MonitorComponent& c, const int indent) 
{
    static int multipleInstances = 0;
    ostringstream o;
    string name = c.getName();
    int len = name.size();
    bool isMultipleInstance = false;

    // Check to see if it ends in a digit that is not '1'
    if (((name[len-1] >= '2') && (name[len-1] <= '9')) 
            ||  (name[len-1] == '0') ) {
        isMultipleInstance = true;
        multipleInstances++;
    }
    // If it ends in a '1' and is preceeded by a digit
    if ((name[len-1] == '1') && (name[len-2] >= '0') && (name[len-2] <= '9')){ 
        isMultipleInstance = true;
        multipleInstances++;
    }
    if (!isMultipleInstance) multipleInstances = 0;
    if (isMultipleInstance) {
        const MonitorPoint* mp = dynamic_cast<const MonitorPoint*>(&c);   
        if (mp != 0) {
            ostringstream os;
            //os << "****SKIP**** " <<  name << endl;
            //return os.str();
            if (multipleInstances == 1) return "...\n"; 
            return ""; 
        }  
    }
    o << name << "   /" << c.getDescription() << "/" << "\n";
    if (isMultipleInstance) {
        // Just return and don't traverse farther down the tree
        return o.str();   
    }
    if (name == "Xac") return o.str();
    if (name == "Can") return o.str();
    {
        const MonitorContainer * const container =
            dynamic_cast< const MonitorContainer * >( &c );
            
        if ( container != 0 ) {
            const int childIndent = indent + 2;

            for (int i=0; i < container->getNumChildren(); i++) {
                o << setw(childIndent) << "" 
                  << toString( container->getChild(i).componentRef(),
                               childIndent );
            }
        }
    }
    return o.str();
}


int Program::main() 
{
    // Control variables for hierarchyToString()
    //bool canonical        = true;
    //bool verbose          = getBoolParameter("verbose");

    cout.setf(ios::fixed);      

    cout << "Created by carma/monitor/Test/briefDump program\n" << endl;        

    // Define monitor system
    MonitorSystem* ms = new CarmaMonitorSystem();
    
    vector<string> ssnames = getSubsystemNames();
    
    for (size_t i=0; i < ssnames.size(); i++) {
        MonitorComponent * c = ms->getComponentPtr(ssnames[i], true);
        if (c == 0) {
            cout << "Trouble, couldn't find: " << ssnames[i] << endl;
            return EXIT_FAILURE;
        }
        //cout << "SS:" << ssnames[i] << endl;
        try {
            cout << toString(*c, 0);
        }
        catch (exception& e) {
            cerr << "Caught std::exception in toString(): \n" 
                 << e.what() << endl;
        }  
        catch (...) {
            cerr << "Caught exception in toString()" << endl;
        }  
    }

    return EXIT_SUCCESS;
}
