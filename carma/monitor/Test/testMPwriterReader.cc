/*
 * Integration test for the monitor frame transport.
 *
 * @author: N. S. Amarnath
 *
 * $Id: testMPwriterReader.cc,v 1.45 2012/01/18 18:48:43 mpound Exp $
 * $CarmaCopyright$
 *
 */

// Keyword setup...
// @version     $Revision: 1.45 $ $Date: 2012/01/18 18:48:43 $
// @usage    testMPwriterReader [iterations=n] [quiet=y] [verbose=n] [writeOnly=false]
// @description
//      Test program for end-to-end tracking of s test subsystem frame.
//      The program assumes thet the FrameScriberPublisher with the subsyhstem
//      name Test is running. The test program generates data for writing
//      to the Test subsystem frame monitor points. The generated values
//      are optionally dumped to stdout. The subsystem frame gets written to
//      shared memeory and gets sent to the Collator and to the carma system
//      shared memeory on the ACC, where this test program reads it and
//      compares it with the subsystem frame that was written. If the
//      two match, then the test passes. If the test fails, the values
//      read from carma system shared memory are also dumped.
//
//      The program does this for 'iterations' # of times, where
//      a value of 0 for 'iterations' means it does this forever.
//      The writeOnly options allows this program to be used to quietly
//      act as a simulated data feed.
//
// @key iterations 0 integer
//      number of writes - 0 means write forever
//
// @key quiet true bool
//      suppress print out of extra data
//
// @key verbose false bool
//      when not quiet, give full mp descriptions
//
// @key writeOnly false bool
//      only write, don't read
//
// @key mode final string
//      \"raw\" or \"final\" - reads \"raw\" IPQ or \"final\" IPQ
//
// @key tickleMonBug false bool
//      whether or not to try to tickle a monitor system bug
//
// @key awdelay      0.100 double 
//      Monitor system autowriter delay in seconds.
//
// @logger MONITOR_FACILITY carma.monitor.testMPwriterReader


#include <iostream>
#include <iomanip>
#include <values.h>

#include "carma/util/Program.h"
#include "carma/util/FrameAlignedTimer.h"
#include "carma/util/Time.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/monitorPointSpecializations.h"
#include "carma/monitor/TestSubsystemExt.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


namespace {


const string kPingPongStringA0 = "123";
const string kPingPongStringA1 = "abcdefghijklmnopqrstuvwxyz";

const string kPingPongStringB0 = "1111";
const string kPingPongStringB1 = "2222";


void
writeSampleValues( TestSubsystem &    testSubsystem,
                   const unsigned int i,
                   const bool         tickleMonBug )
{
    int ival = 23 + i;

    testSubsystem.timestamp().setValue (Time::MJD());

    TestSubsystem::Box& mc = testSubsystem.box();
    MonitorPointChar& mpc  = mc.mpchar();
    MonitorPointByte& mpb  = mc.mpbyte();
    MonitorPointShort& mps = mc.mpshort();
    MonitorPointInt& mpi   = mc.mpint();
    mpb.setNumSamples(3);
    mpi.setNumSamples(4);
    mps.setNumSamples(2);
    mpb.setValue(ival+0,  0);
    mpb.setValue(ival+10, 1);
    mpb.setValue(ival+2,  2);
    mpb.setValidity(MonitorPoint::INVALID_NO_HW,  1);
    mpb.setValidity(MonitorPoint::VALID_WARNING_LOW,  2);
    mps.setValue(ival);
    mps.setValue(ival+2, 1);
    mpi.setValue(ival);
    mpi.setValue(789+i+0,  1);
    mpi.setValue(789+i+1,  2);
    mpi.setValue(789+i+2,  3);
    mpi.setValidity(MonitorPoint::VALID_ERROR_LOW,    0);
    mpi.setValidity(MonitorPoint::VALID_WARNING_LOW,  1);
    mpi.setValidity(MonitorPoint::VALID_WARNING_HIGH, 2);
    mpi.setValidity(MonitorPoint::VALID_ERROR_HIGH,   3);

    double fval = 52.12345678 + static_cast< double >( i );
    complex<float> cxval(43.12345678+0.1*i, -2.54321 - 0.1*i);
    MonitorPointFloat& mpf     =  mc.mpfloat();
    MonitorPointDouble& mpd    =  mc.mpdouble();
    MonitorPointComplex& mpcx  =  mc.mpcomplex();
    MonitorPointAbstime& mpa   =  mc.mpmjd();
    mpd.setNumSamples(3);
    mpf.setValue(fval);
    mpd.setValue(fval);
    mpd.setValue(fval+2, 1);
    mpd.setValue(fval+4, 2);
    mpd.setValidity(MonitorPoint::VALID_ERROR,   0);
    mpd.setValidity(MonitorPoint::VALID_WARNING, 1);
    mpcx.setValue(cxval);
    mpa.setValue(Time::MJD());

    MonitorPointBool& mpl = mc.mpbool();
    bool lval = ((i%20) < 10)  ?  true : false;
    mpl.setValue(lval);

    char cval = 'a';
    mpc.setValue(cval);

    if ( (i % 30) < 15 )
        mc.mpstring().setValue( "abcdef" );
    else
        mc.mpstring().setValue( "zyxwvutsrqponmlkji" );

    if ( tickleMonBug ) {
        // Try to tickle a monitor system bug

        if ( (i % 16) < 8 ) {
            mc.canDevice(0).spstring().setValue( kPingPongStringA0 );
            mc.canDevice(1).spstring().setValue( kPingPongStringA1 );
        } else {
            mc.canDevice(0).spstring().setValue( kPingPongStringA1 );
            mc.canDevice(1).spstring().setValue( kPingPongStringA0 );
        }

        if ( (i % 8) < 4 ) {
            mc.canDevice(2).spstring().setValue( kPingPongStringB0 );
            mc.canDevice(3).spstring().setValue( kPingPongStringB1 );
        } else {
            mc.canDevice(2).spstring().setValue( kPingPongStringB1 );
            mc.canDevice(3).spstring().setValue( kPingPongStringB0 );
        }
    }

    MonitorPointSerialNo& mpsn = mc.mpserialno();
    int snval = 7531 + i;
    mpsn.setValue(snval);

    int colorPhase = (i%30)/10;
    switch (colorPhase) {
        case 0: mc.color().setValue(
            TestSubsystem::ColorMonitorPointEnum::RED);
            break;
        case 1: mc.color().setValue(
            TestSubsystem::ColorMonitorPointEnum::GREEN);
            break;
        case 2: mc.color().setValue(
            TestSubsystem::ColorMonitorPointEnum::BLUE);
            break;
    }


    TestSubsystem::CanDevice& t = mc.canDevice(0);

    ival += 13;
    fval += 13;
    cxval += complex<float>(+fval, -fval);
    t.spchar().setValue('b');
    t.spbyte().setValue(ival);
    t.spshort().setValue(ival);
    t.spint().setValue(ival);
    t.spfloat().setValue(fval);
    t.spdouble().setValue(fval);
    t.spmjd().setValue(fval);
    t.spcomplex().setValue(cval);
    t.spbool().setValue(i%2 == 1);
    t.spserialno().setValue(345+i);

    TestSubsystem::Fake& f = testSubsystem.fake();
    // Set the noise values with a random number
    for (int i = 0; i < f.digitizerCount(); i++) {
        for (int n = 0; n < f.digitizer(i).getNumNoise(); n++) {
            int v = static_cast<int>(round(100.0*rand()/RAND_MAX));
            f.digitizer(i).noise(n).setValue(v);
        }
    }

}


}  // namespace < anonymous >


int
Program::main( )
{
    const bool verbose      = getBoolParameter ("verbose");
    int numIterations       = getIntParameter ("iterations");
    const bool quiet        = getBoolParameter ("quiet");
    const bool writeOnly    = getBoolParameter ("writeOnly");
    const string mode       = getStringParameter ("mode");
    const bool tickleMonBug = getBoolParameter( "tickleMonBug" );

    const bool rawMode = (mode == string ("raw"));
    const bool canonical = true;
    const bool value = true;

    Time time;

    // Connection to test subsystem frame
    TestSubsystem testWriter;

    // Initialize a random number generator
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    srand(ts.tv_nsec);

    // Connection to carma system frame
    MonitorSystem & carma =
        (rawMode ? (*(new RawCarmaMonitorSystem)) :
                   (*(new CarmaMonitorSystem)));
        
    TestSubsystem & testReader = carma.test();

    if (numIterations == 0) numIterations = MAXINT;

    const double awDelayInS = getDoubleParameter( "awdelay" );
    testWriter.startAutoWriter( awDelayInS );

    if (writeOnly) {
        FrameAlignedTimer fat;
        fat.ResetNextFireTime();

        for (int  i = 0;  i < numIterations;  i++)  {
            fat.WaitForNextFireTime();
            //cout << "Fired!!" << endl;
            writeSampleValues( testWriter, i, tickleMonBug );
        }
        return EXIT_SUCCESS;
    }

    cout  
        << "If this program hangs, it may be caused by not running on\n"
        << "an ACC equivalent machine that is running a FrameCollator."
        << endl;

    // Start FORLOOP
    for (int  i = 0;  i < numIterations;  i++)  {

    bool dataMatches = true;
    writeSampleValues( testWriter, i, tickleMonBug );
    carma.readNewest();
    //testWriter.write(); // Use this instead of autoWriter
    cout << "New values written to monitor stream." << endl;
    do  {
        carma.read();
    }  while (testReader.timestamp().getValue() <
              testWriter.timestamp().getValue());

    int mpintSave = testWriter.box().mpint().getValue(1);
    if (testReader == testWriter)  {

        // The next line should cause the == test to fail; it is a test
        // of the == operator
        testWriter.box().mpint().setValue(6, 1);
        if (testReader == testWriter)  {
            cout << "Frames are equal when shouldn't be;"
                    << " CARMA MONITOR TRANSPORT TEST FAIL" << endl;
        }
        testWriter.box().mpint().setValue(mpintSave, 1);
        cout << "Frames are equal; CARMA MONITOR TRANSPORT TEST PASS"
                << endl;
    }
    else  {
        dataMatches = false;
        cout << "Frames are not equal; CARMA MONITOR TRANSPORT TEST FAILED"
                << endl;
        if (!quiet) {
            cout << "Written:: " << endl;
            cout << testWriter.hierarchyToString(canonical, false, true)
                    << endl;
            cout << "Read:: " << endl;
            cout << testReader.hierarchyToString(canonical, false, true)
                    << endl;
        }
    }
    if (!quiet && !dataMatches) {
        cout << testWriter.hierarchyToString(canonical, verbose, value)
                << endl;
        cout << "Values read from monitor stream." << endl;
        cout << testReader.hierarchyToString(canonical, verbose, value)
                << endl;
    }
    MonitorPointIterator iter1(testReader);
    MonitorPointIterator iter2(testWriter);
    while(iter1++) {
        iter2++;
        if (iter1.getMonitorPoint() != iter2.getMonitorPoint()) {
            //cout<<iter1.getMonitorPoint().toString()<<endl;
            //cout<<iter2.getMonitorPoint().toString()<<endl;
        }
    }

    } // End FORLOOP

    return EXIT_SUCCESS;
}
