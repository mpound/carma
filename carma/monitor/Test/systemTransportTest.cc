
/**
 *
 * Does timing tests for data transport for a single subsystem
 * to the system (ACC).
 *
 * @author: Steve Scott
 *
 * $Id: systemTransportTest.cc,v 1.11 2012/01/18 18:48:43 mpound Exp $
 * $CarmaCopyright$
 *
 */

// Keyword setup...
// @version     $Revision: 1.11 $ $Date: 2012/01/18 18:48:43 $
// @usage     see keywords
// @key subsys  Test  string  subsystem name
// @key reps    100   int     Number of frames to do in test
// @key quiet   true  bool    Silence the output
// @description
// Test program for monitor subsystem transport times.
// Run only on ACC machine (needs full carma monitor system).
// Subsystem name is case insensitive.
//
// @logger TEST_FACILITY carma.test.monitor.systemTransportTest
//

#include <iostream>
#include <iomanip>

#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "carma/util/CommonExceptions.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/MonitorSubsystemMaster.h"

#include "carma/monitor/Test/utils.cc"


using namespace std;
using namespace carma::util;
using namespace carma::monitor;

// --------------------------- Main ------------------------

int Program::main() 
{
    
    const double MSEC_PER_DAY = 1000*Time::SECONDS_PER_DAY;
       
    // Flag to control output
    bool quiet = Program::getBoolParameter("quiet");

    // Get number of reps from cmd line
    int nReps = getIntParameter("reps");
    
    //Subsystem name
    string subsystemName = getStringParameter("subsys");
         
    // Variables to sum over full run
    int    nBadFrames         = 0;
    
    Stats readDelay;
    Stats pubDelay;
    Stats rxDelay;
    Stats tranTime;
    
    // carma monitor system (ACC) 
    CarmaMonitorSystem c;

    // subsystem within the monitor system    
    MonitorComponent * mc  = c.getComponentPtr( subsystemName, false );
    if ( mc == 0 ) {
        ostringstream o;
        o << "Can't find subsystem with name=\'" << subsystemName << "'" ;
        throw CARMA_ERROR(o);
    }
    MonitorSubsystem & r = dynamic_cast< MonitorSubsystem & >( *mc );
    if ( &r == 0 ) {
        throw CARMA_ERROR("Couldn't cast to a MonitorSubsystem");
    }
    
   // Get the first carma monitor point to use to check status
    MonitorPoint& mp = r.getFirstMonitorPoint();

    // Print out a header
    if (!quiet) {
        cout << "NumMP="     << r.getNumMonitorPoints()
             << "  NumSamp=" << r.getNumSamples()
             << endl;
        cout << "Del: msec since last half sec for  "
             << "reading carma ipq" << endl;
        cout << "Tran: transport time (rx-pub)" << endl;
        cout << "stat: validity of first MP at carma frames"
             << endl;
        cout << " Reps rDel pDel rxDel Tran Stat"
             <<  "        Pub                   Rx" << endl;
    }
    
    // Synchronize startup
    sleepUntilNextFrame(20);
    c.readNewest();


    // ---------Loop------------------------------
    // The loop:  reading data
    for(int rep=0; rep < nReps; rep++) {

 
    double currentFrameTime = Time::MJD(Time::computeClosestFrame());
    
    // Now a blocking read on the system frame...
    c.read();
    double readTime = Time::MJD(); // Time of ss read
    // Time after half sec that data was read
    readDelay += MSEC_PER_DAY*(readTime - currentFrameTime);
    
    // If the frame is not received in time the rx/pub times are not
    // updated and are therefore off by -0.5s
    // In this case we want to ignore the data
    if (mp.isValid()) {
        rxDelay  += MSEC_PER_DAY*(r.getReceiveTime() - currentFrameTime);
        pubDelay += MSEC_PER_DAY*(r.getPublishTime() - currentFrameTime);
        tranTime += MSEC_PER_DAY*(r.getReceiveTime() - r.getPublishTime());
    }
    
    if (false) cout 
      << Time::getDateTimeString(currentFrameTime, 3) <<"/"
      << Time::getDateTimeString(r.getPublishTime(), 3) <<"/"
      << Time::getDateTimeString(r.getReceiveTime(), 3) <<"/"
      <<endl;

    string statString;
    if (mp.isValid()) {
        statString = "g";
    }
    else {
        nBadFrames++;
        statString = "B"; 
    }


    if (!quiet) {       
        cout<< setprecision(0)
            << setw(5) << rep + 1 
            << setw(5) << readDelay.last() 
            << setw(5) << pubDelay.last() 
            << setw(6) << rxDelay.last() 
            << setw(5) << tranTime.last() 
            << setw(5) << statString 
            << " " << Time::getDateTimeString(r.getPublishTime(), 3)
            << " " << Time::getDateTimeString(r.getReceiveTime(), 3)
            << endl;
    }
    
    // Now wait until the next frame before we go to the top
    sleepUntilNextFrame(20);
        
    }  // End of for loop



    double badFramesPercent = (100.0 * nBadFrames)/(nReps);
    
    cout<<"----------------Summary-------------------"<<endl;
    cout << "NumMP="     << r.getNumMonitorPoints();
    cout << "  NumSamp=" << r.getNumSamples();
    cout << endl;
    cout.setf(ios::fixed);
    cout << "Reps=" << nReps 
         << "  BadFrames=" << nBadFrames  
         << "(" << setprecision(1) << badFramesPercent << "%)"
         << endl;
    pubDelay.summary("Publish delay:");
    rxDelay.summary("Receive delay:");
    readDelay.summary("Read delay:");
    tranTime.summary("Transport time:");

    return EXIT_SUCCESS;
}
