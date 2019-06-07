
/**
 *
 * Simple test for the acc monitor hierarchy.
 * Command line input:
 *  1) Which component to dump - defaults to everything but can be a 
 *     single monitor point. Hierarchical, case matters.
 *  2) Verbose or succint (default)
 *  3) Dump names of all subsystems (no=default); convenient if you
 *     need to know exact name of ss.
 * Action:
 *  1) Optionally dumps subsystem names
 *  2) Dumps hierarchy as described by the library
 *  3) Loops, reading new frames and dumping them out. 
 *     If a FrameCollator has never been run on this machine to define the
 *     data then the program exits.
 *
 * @author: Steve Scott
 *
 * $Id: carmaTest.cc,v 1.22 2012/01/18 18:48:43 mpound Exp $
 *
 * $CarmaCopyright$
 *
 */

// Keyword setup...
// @version     $Revision: 1.22 $ $Date: 2012/01/18 18:48:43 $
// @usage       carmaTest [verbose=t/f] [comp=c] [ssNames=t/f] 
// @key verbose false bool Verbose output
// @key ssNames false bool Dump subsystem names at start
// @key comp    Carma string Monitor system component to dump; hierarchical name, case matters 
// @description
//      Simple test program for reading the carma monitor system in the ACC.
//      It can read all of the system or just a piece of it 
//      (as small as a single monitor point). The first thing it does is
//      to optionally dump all subsystem names. Then it dumps the
//      hierarchy including and below your requested component.
//      Then it reads new frames as they are written and dumps them. 
//      Program will loop forever until you kill it.
//      A little test of numerics in appended to the end.
//
// @logger TEST_FACILITY carma.test.monitor.carmaTest
//

#include <iostream>
#include <iomanip>

#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/SldcSubsystem.h"
#include "carma/monitor/MonitorPointIterator.h"


using namespace std;
using namespace carma::util;
using namespace carma::monitor;

int Program::main() 
{
    // Control variables for hierarchyToString()
    bool canonical        = true;
    bool verbose          = Program::getBoolParameter("verbose");
    bool value            = true;
    bool initialDump      = false;
    bool infiniteDump     = false;
    bool testAveraging    = true;

    // Define monitor system
    MonitorSystem&   carma = *new CarmaMonitorSystem();
    
    // Control of subsystem dump from command line
    bool dumpSubsystemNames = Program::getBoolParameter("ssNames");
    // Dump of subsystem names    
    if (dumpSubsystemNames) {
        cout << "------------ Quick def of carma subsystems--------------"
             << endl;
        for (int i=0; i<carma.getNumChildren(); i++) {
            cout << "Subsystem:"
                 << carma.getChild(i).componentRef().toString( false,
                                                               false,
                                                               false )
                 << endl;
        }
    }
    
    // Get component to dump...
    string input = Program::getStringParameter("comp");;    
    MonitorComponent* m;
    if (input == "Carma") {
        m = &carma;
    }
    else {
         m = carma.getComponentPtr(input, true);
    }
    if (m == 0) {
         cout << "Could not find requested component(" << input
               << "), exiting..." << endl;
         exit(1);
    }
    cout << "Dumping: " << input << endl;
    
    // It is convenient to use a reference for the top of the tree
    MonitorComponent& tree = *m;

    //cout.setf(ios::fixed);

    cout << "-----------------Initial hierarchy-----------------" << endl;
    if (initialDump)cout << tree.hierarchyToString(canonical, verbose, value);


    cout << "----------------Read loop, dump values---------------" << endl;
    // Setup for top of queue...
    bool haveData = carma.readNewest(); // get top of queue
    if (!haveData) {
        cout << "There is no data in the queue - exiting..." << endl;
        exit(1);
    }
    
    // Loop
    int rep = 0;   
    while (infiniteDump) {

        cout << "rep=" << ++rep << endl;   
        carma.read();
        cout << tree.hierarchyToString(canonical, verbose, value);

    }

    cout << setiosflags(ios::fixed);
    while (testAveraging) {
        carma.read();
        SldcSubsystem::Input& inp = carma.sldc().band(0).input(1);
        MonitorPointFloat& psys = inp.psys();
        double sum =0;
        double samps = 0;
        for (int i=0; i<psys.getNumSamples(); i++) {
            if (psys.isValid(i)) {
                sum += psys.getValue(i);
                samps++;
            }
        }
        double handAve = 0;
        if (samps)handAve = sum/samps;
        const double aveErr = psys.getAve() - handAve;
        const double aveNumErr = psys.getAveNumeric() - handAve;
        cout << "aveErr:" << setw(7) << setprecision(4) << aveErr
             << "  aveNumErr:" << setw(7) << setprecision(4) << aveNumErr
             << "   samps=" << setprecision(0) << samps << endl;
    }
    
    // A little test of the MPnumeric    
    carma.read();
    SldcSubsystem::Input& inp = carma.sldc().band(0).input(1);
    MonitorPointFloat& psys = inp.psys();
    ostringstream d, n;
    d << setiosflags(ios::fixed);
    n << setiosflags(ios::fixed);

    d << "Float:  Ave=" << setprecision(3) << psys.getAve() << " (";
    n << "Numeric: Ave=" << setprecision(3) << psys.getAveNumeric() 
      << " (";
    for (int i=0; i<5; i++) {
        d << setprecision(3) << psys.getValue(i);
        n << setprecision(3) << psys.getValueNumeric(i);
        if (i < 4) {
             d << ", ";
             n << ", ";
        }
        else {
             d << ")";
             n << ")";
        }
    }   
    cout << d.str() << endl;
    cout << n.str() << endl; 
    
    return 0;
}












