  
/**
 *
 * Unit test for a client for the wideband downconverter monitor subsystem.
 * This is also an example program for any simple client.
 *
 * @author: Steve Scott
 *
 * $Id: dcTest.cc,v 1.31 2012/03/13 05:18:11 abeard Exp $
 * $CarmaCopyright$
 *
 */

// Keyword setup...
// @version     $Revision: 1.31 $ $Date: 2012/03/13 05:18:11 $
// @usage      See keywords
// @key verbose    false  bool   Verbose output
// @key transport  false  bool   Reports internal transport stats
// @key delay      0.100  double Autowriter delay in seconds
// @key reps       10     int    Number of frames to do in test
// @key quiet      true   bool   Silence the output
// @description
//      Simple test program for a client for the wideband downconverter 
//      monitor subsystem. This does not use the ACC but does need to
//      have a frameScriber running.
//
// @logger TEST_FACILITY carma.test.monitor.dcTest
//


#include <iostream>
#include <iomanip>

#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/WbdcSubsystem.h"

#include "carma/monitor/Test/utils.cc"

using namespace std;
using namespace carma::util;
using namespace carma::monitor;


int Program::main() 
{
    const double MSEC_PER_DAY = 1000*Time::SECONDS_PER_DAY;
    cout.setf(ios::fixed);
    
    // Control variables for hierarchyToString()
    //bool canonical        = true;
    bool verbose          = Program::getBoolParameter("verbose");
    //bool value            = true;
       
    // Flag to control use of autoWrite or manual writer
    bool autoWrite = true;
    // Autowriter delay, in seconds
    double autoWriterDelay = Program::getDoubleParameter("delay");
        
    // Output transport times?
    bool doTransport = Program::getBoolParameter("transport");
        
    // Flag to control output
    bool quiet = Program::getBoolParameter("quiet");

    // Flag to control reading local ss frame or acc frame
    bool useAcc = false;

    int testBandNo  = 1;  // BandNo selected for testing
    int testInputNo = 1;   // InputNo selected for testing

    // Get number of reps from cmd line
    int nReps = getIntParameter("reps");
    
    // carma monitor system (ACC) in case we want to use it
    CarmaMonitorSystem* carma;

    // Define the subsystem used for writing
    WbdcSubsystem&  w = *new WbdcSubsystem();

    // Define the subsystem used for reading
    WbdcSubsystem* ss;
    if (useAcc) {
        carma = new CarmaMonitorSystem();
        ss = &(carma->wbdc());
    }
    else {
        ss = new WbdcSubsystem();
    }
    
    //I like to use references, so here is one for the reader
    WbdcSubsystem&  r = *ss;

    // A particular downconverter chosen to do things to
    // For both the reader and writer	
    WbdcSubsystem::Input& wd = w.band(testBandNo - 1).input(testInputNo - 1);
    WbdcSubsystem::Input& rd = r.band(testBandNo - 1).input(testInputNo - 1);
  
    // Transport test variables and statistics
    int   nBadTransport = 0;    
    Stats writeDelay;
    Stats readDelay;
    Stats mpsetTranTime;
    Stats mpsetStartWriteDelay;
    Stats scriberStartWriteDelay;
    Stats mpsetEndWriteDelay;

    //cout<<"-----------------setPersistentValues-------------------"<<endl;
    wd.xac().serialNo().setValue(543);
    wd.bandNo().setValue(testBandNo);
    wd.inputNo().setValue(testInputNo);
    // Output format control
    wd.psys().setPrecision(2);
    rd.psys().setPrecision(2);
    w.timestamp().setPrecision(3);
    r.timestamp().setPrecision(3);
    w.timestamp().setWidth(w.timestamp().getWidth()+4);
    r.timestamp().setWidth(r.timestamp().getWidth()+4);

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
             << "reading subsys ipq" << endl;
        cout << "tran: transport time of mpSet to scriber" << endl;
        cout << "stat: validity or data read from ipq"
             << endl;
        cout << " reps wDel rDel tran stat"
             <<  "      start write              end write" << endl;
    }

    // Synchronize startup
    sleepUntilNextFrame();
    r.readNewest();
    if (autoWrite)w.startAutoWriter(autoWriterDelay); 
    r.read();
    sleepUntilNextFrame();    


    // .............................................
    // -------------------BEGIN LOOP----------------------
    // The loop: writing and reading data
    for(int rep=0; rep < nReps; rep++) {

    
    // -------------- Update values ----------------
    w.timestamp().setValue(Time::MJD());
    //double writeTime = w.timestamp().getValue();
    
    // Update values
    for (int b=0; b<16; b++) {
        for (int i=0; i<8; i++) {
            WbdcSubsystem::Input& x = w.band(b).input(i);
            x.ps5vAnalog().setValue(5.0 + rep/100.0);
            for (int s=0; s<5; s++) {
                MonitorPointFloat& m = x.psys();
                m.setValue(-25.0 + (s*10+rep)/100.0, s);
            }
        }
    }
    
    if(false) cout << setprecision(2)
         << wd.ps5vAnalog().toString(true,false, false) 
         << wd.ps5vAnalog().dumpSamples() << endl;
    
    //volatile double beforeMark = Time::MJD();    
    // Mark all the monitor points so they will be transported
    if (doTransport)w.setValidity(MonitorPoint::VALID_NOT_CHECKED);
    //double markElapsedTime = MSEC_PER_DAY*(Time::MJD() - beforeMark);    
     
    //r.readNewest();      // Set to top of queue
    double frameTime = Time::MJD(Time::computeClosestFrame());
    // Manual writing is an alternative to the autowriter...
    if(!autoWrite)w.write();
    writeDelay += MSEC_PER_DAY*(Time::MJD() - frameTime);
    
    // Now a blocking read...
    r.read();
    readDelay += MSEC_PER_DAY*(Time::MJD() - frameTime);
    bool valid = MonitorPoint::isValid(rd.psys().getValidity());
    if (!valid)nBadTransport++;


    // Now wait until 20msec before next frame so that we can get all stats
    sleepUntilNextFrame(-20);
    
    
    //cout<<wd.totalPower().toString(true, verbose, true) <<endl;
 
    double startWriteTime        = w.getStartWriteTime();
    double endWriteTime          = w.getEndWriteTime();
    double startScriberWriteTime = w.getStartScriberWriteTime();
    mpsetStartWriteDelay += 
         MSEC_PER_DAY*(startWriteTime - frameTime);
    scriberStartWriteDelay += 
         MSEC_PER_DAY*(startScriberWriteTime - frameTime);
    mpsetEndWriteDelay += 
         MSEC_PER_DAY*(endWriteTime - frameTime);
    mpsetTranTime += 
         MSEC_PER_DAY*(endWriteTime - startWriteTime);

    if (verbose) {
        bool fullDesc = false;
        cout << r.timestamp().toString(true, fullDesc, true) << endl;
        cout << setprecision(2)
             << rd.psys().toString(true,false, false) 
             << rd.psys().dumpSamples() 
             << endl;
        cout << setprecision(2)
             << rd.ps5vAnalog().toString(true,false, false) 
             << rd.ps5vAnalog().dumpSamples() << endl;
        //cout << rd.serialNo().toString(true, verbose, true) << endl;
    }


    if (!quiet ) {       
        cout<< setprecision(0)
            << setw(5) << rep+1
            << setw(5) << writeDelay.last() 
            << setw(5) << readDelay.last() 
            << setw(5) << mpsetTranTime.last() 
            << setw(5) << (valid?"GOOD":"BAD") 
            << " "  << Time::getDateTimeString(startWriteTime, 3)
            << "  " << Time::getDateTimeString(endWriteTime,   3)
            << endl;
    }
        
    }  // End of for loop


    double badTransportPercent = (100.0 * nBadTransport)/nReps;

    if (doTransport) {    
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
             << "  BadTransport=" << nBadTransport  
             << "(" << setprecision(1) << badTransportPercent << "%)"
             << endl;
        writeDelay.summary("Write delay:");
        readDelay.summary("Read delay:");
        mpsetStartWriteDelay.summary("MPset start write delay:");
        scriberStartWriteDelay.summary("Scriber rx MPset delay:");
        mpsetEndWriteDelay.summary("MPset end write delay:");
        mpsetTranTime.summary("MPset transport time:");
    }
    
    cout<<"----------------successful completion-------------------"<<endl;
    return 0;
}












