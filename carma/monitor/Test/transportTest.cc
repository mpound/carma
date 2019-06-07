
/**
 *
 * Unit test for data tranport. Uses both the subsystem and system
 * frames, so both the FrameScriberPublisher and the FrameCollator
 * must be running.
 *
 * @author: Steve Scott
 *
 * $Id: transportTest.cc,v 1.21 2012/01/18 18:48:43 mpound Exp $
 * $CarmaCopyright$
 *
 */

// Keyword setup...
// @version     $Revision: 1.21 $ $Date: 2012/01/18 18:48:43 $
// @usage     See keywords
// @key subsys  Test   string  subsystem name
// @key delay   0.100  double  Autowriter delay in seconds
// @key reps    100    int     Number of frames to do in test
// @key quiet true bool Silence the output
// @description
// Test program for monitor subsystem transport times.
// Needs access to both the subsystem frame and the full
// carma monitor system. So this program needs to run on
// the same machine as a FrameScriberPublisher and a FrameCollator.
// Subsystem name is case insensitive
//
// @logger TEST_FACILITY carma.test.monitor.transportTest
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
    cout.setf(ios::fixed);

    // Control variables for hierarchyToString()
    //bool canonical        = true;
    //bool verbose          = false;
    //bool value            = true;
           
    // Flag to control output
    bool quiet = Program::getBoolParameter("quiet");

    // Flag to control use of autoWrite or manual writer
    bool autoWrite = true;
    double autoWriterDelay = Program::getDoubleParameter("delay");
    
    // Get number of reps from cmd line
    int nReps = getIntParameter("reps");
    
    //Subsystem name
    string subsystemName = getStringParameter("subsys");
         
    // Variables to sum over full run
    int    nBadSubsystem         = 0;
    int    nBadSystem            = 0;
    
    Stats writeDelay;
    Stats ssReadDelay;
    Stats sysReadDelay;
    Stats mpsetTranTime;
    Stats ssTranTime;
    Stats mpsetStartWriteDelay;
    Stats scriberStartWriteDelay;
    Stats mpsetEndWriteDelay;
    
    // carma monitor system (ACC) 
    CarmaMonitorSystem c;

    // Define the subsystem used for writing and reading
    MonitorSubsystem& w = MonitorSubsystemMaster::makeSubsystem(subsystemName);
    MonitorSubsystem& r = MonitorSubsystemMaster::makeSubsystem(subsystemName);
   
    MonitorComponent * mc  = c.getComponentPtr( subsystemName, false );
    if ( mc == 0 ) {
        ostringstream o;
        o << "Can't find subsystem with name=\'" << subsystemName << "'" ;
        throw CARMA_ERROR(o);
    }
    MonitorSubsystem & css = dynamic_cast< MonitorSubsystem & >( *mc );
    if (&css == 0) {
        throw CARMA_ERROR("Couldn't cast to a MonitorSubsystem");
    }
    
    // Get the timestamp (if available) to use to check status,
    // otherwise use first reader and carma monitor point
    MonitorPoint * mpts  = r.getMonitorPointPtr("timestamp", true);
    MonitorPoint * mpcts = css.getMonitorPointPtr("timestamp", true);
    bool haveTimeStamp = (mpts != 0);
    if (haveTimeStamp) {
        cout << "Have timestamp" << endl;

        if ( mpcts == 0 ) {
            cout << "mpcts is NULL" << endl;
            exit( 1 );
        }
    }
    MonitorPoint& mpr = haveTimeStamp? (*mpts)  : r.getFirstMonitorPoint();
    MonitorPoint& mpc = haveTimeStamp? (*mpcts) : css.getFirstMonitorPoint();

    // Print out a header
    if (!quiet) {
        cout << "NumMP="     << r.getNumMonitorPoints()
             << "  NumSamp=" << r.getNumSamples()
             << "    Autowriter:" << (autoWrite?"Yes":"No");
        if (autoWrite) {
            cout << "  delay=" 
                 << setprecision(3) << autoWriterDelay ;
        }
        cout << endl;
        cout << "*Del: msec since last half sec for writing, "
             << "reading subsys ipq, and carma ipq" << endl;
        cout << "tran: transport time (rx-pub)" << endl;
        cout << "ssStat and cStat: validity at subsystem and carma frames"
             << endl;
        // Column headings...     
        cout << "wDel ssDel cDel mpTran ssTran ssStat cStat"
             <<  "        Pub                   Rx" << endl;
    }
    
    // Synchronize startup
    sleepUntilNextFrame();
    r.readNewest();
    c.readNewest();
    if (autoWrite)w.startAutoWriter(autoWriterDelay); 
    r.read();
    c.read();
    sleepUntilNextFrame();


    // ---------Loop------------------------------
    // The loop: writing and reading data
    //bool firstTime = true;
    for(int rep=0; rep < nReps; rep++) {

     
    // -------------- Update values ----------------    
    // Mark every monitor point as good so that they are all transported
    // from the MPset to the scriber
    w.setValidity(MonitorPoint::VALID_NOT_CHECKED);
 
    double writeTime = Time::MJD();
    double currentFrameTime = Time::MJD(Time::computeClosestFrame());
    // Manual writing is an alternative to the autowriter...
    if(!autoWrite)w.write();
    
    // Now a blocking read in the subsystem...
    r.read();
    double ssRead = Time::MJD(); // Time of ss read
    const double MSEC_PER_DAY = 1000*Time::SECONDS_PER_DAY;

    // Time after half second that data was written
    writeDelay += MSEC_PER_DAY*(writeTime - currentFrameTime);
    // Time after half sec that data was read
    ssReadDelay += MSEC_PER_DAY*(ssRead - currentFrameTime);
    // Difference between writing and reading
    //double rwdelay = MSEC_PER_DAY*(ssRead-writeTime);
    
    // Now read the system frame...
    c.read();
    double cRead  = Time::MJD(); // Time of ss read
    sysReadDelay += MSEC_PER_DAY*(cRead - currentFrameTime);
    ssTranTime   += MSEC_PER_DAY*(css.getReceiveTime()-css.getPublishTime());
    double startWriteTime        = w.getStartWriteTime();
    double endWriteTime          = w.getEndWriteTime();
    double startScriberWriteTime = w.getStartScriberWriteTime();
    if (startWriteTime > 1)mpsetStartWriteDelay += 
        MSEC_PER_DAY*(startWriteTime - currentFrameTime);
    if (startScriberWriteTime > 1)scriberStartWriteDelay += 
        MSEC_PER_DAY*(startScriberWriteTime - currentFrameTime);
    if (endWriteTime > 1)mpsetEndWriteDelay += 
        MSEC_PER_DAY*(endWriteTime - currentFrameTime);
    if (endWriteTime > 1)mpsetTranTime += 
        MSEC_PER_DAY*(endWriteTime - startWriteTime);

    if (false) cout 
      << Time::getDateTimeString(currentFrameTime, 3) <<"/"
      << Time::getDateTimeString(startWriteTime, 3) <<"/"
      << Time::getDateTimeString(startScriberWriteTime, 3) <<"/"
      << Time::getDateTimeString(endWriteTime, 3) <<"/"
      <<endl;

    string ssStatString, cStatString;
    if (mpr.isValid()) {
        ssStatString = "g";
    }
    else {
        nBadSubsystem++;
        ssStatString = "B"; 
    }
    if (mpc.isValid()) {
        cStatString = "g";
    }
    else {
        nBadSystem++;
        cStatString = "B"; 
    }


    if (!quiet) {       
        cout<< setprecision(0)
            << setw(4) << writeDelay.last() 
            << setw(6) << ssReadDelay.last() 
            << setw(5) << sysReadDelay.last() 
            << setw(7) << mpsetTranTime.last() 
            << setw(7) << ssTranTime.last() 
            << setw(7) << ssStatString 
            << setw(6) << cStatString 
            << " " << Time::getDateTimeString(css.getPublishTime(), 3)
            << " " << Time::getDateTimeString(css.getReceiveTime(), 3)
            << endl;
    }
    
    // Now wait until the next frame before we go to the top
    sleepUntilNextFrame();

        
    }  // End of for loop
    double badSubsystemPercent = (100.0 * nBadSubsystem)/(nReps);
    double badSystemPercent    = (100.0 * nBadSystem)/(nReps);
    
    cout<<"----------------Summary-------------------"<<endl;
    cout << "NumMP="     << r.getNumMonitorPoints()
         << "  NumSamp=" << r.getNumSamples()
         << "    Autowriter:" << (autoWrite?"Yes":"No");
    if (autoWrite) {
        cout << "  delay=" 
             << setprecision(3) << autoWriterDelay ;
    }
    cout << endl;
    cout << "Reps=" << nReps 
         << "  BadSubsystemFrames=" << nBadSubsystem  
         << "(" << setprecision(1) << badSubsystemPercent << "%)"
         << "  BadSystemFrames="  << nBadSystem 
         << "(" << setprecision(1) << badSystemPercent << "%)"
         << endl;
    writeDelay.summary("Write delay:");
    ssReadDelay.summary("Subsystem read delay:");
    sysReadDelay.summary("System read delay:");
    mpsetStartWriteDelay.summary("MPset start write delay:");
    scriberStartWriteDelay.summary("Scriber rx MPset delay:");
    mpsetEndWriteDelay.summary("MPset end write delay:");
    mpsetTranTime.summary("MPset transport time:");
    ssTranTime.summary("Subsystem transport time:");

    return 0;
}












