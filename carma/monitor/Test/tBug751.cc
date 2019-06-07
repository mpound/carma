#include "carma/monitor/SubsystemFrameBuffer.h"
#include "carma/monitor/MonitorPointSet.h"
#include "carma/monitor/TestSubsystem.h"
#include "carma/monitor/TestSubsystemExt.h"
#include "carma/util/Program.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/StartPthread.h"
#include "carma/util/ScopedPthreadMutexLock.h"

#include <iostream>

using namespace carma::monitor;
using namespace carma::util;
using namespace std;

void 
setBox( TestSubsystem::Box & box ) 
{
    box.mpstring().setValue( "12345678 2345678" 
        "2345678 2345678 2345678 2345678 2345678 2345678" );
    box.mpchar().setValue('a');
    box.mpbyte().setValue(1);
    box.mpshort().setValue(2);
    box.mpint().setValue(3);
    box.mpbool().setValue( true );
    box.mpfloat().setValue(4.0);
    box.mpdouble().setValue(5.0);
    box.mpcomplex().setValue( std::complex< float >( 6.0, 7.0 ) );
}

void
setCanDevice( TestSubsystem::CanDevice & candev )
{
    candev.spchar().setValue('a');
    candev.spbyte().setValue(1);
    candev.spshort().setValue(2);
    candev.spint().setValue( 5 );
    candev.spbool().setValue( true );
    candev.spfloat().setValue(3.0);
    candev.spdouble().setValue(3.0);
    candev.spcomplex().setValue( std::complex< float >( 6.0, 7.0 ) );
    candev.spmjd().setValue( 1.000001 );

    candev.spstring().setValue( "spstring 2345678 2345678 2345678"
        "12345678 2345678 2345678 2345678 2345678 2345678" ); // 10 chunks
}

void
setFakeDigitizer( TestSubsystem::Digitizer & digitizer ) 
{
    digitizer.clockSpeed().setValue( 500.000 );
    digitizer.state().setValue( "12345678 2345678 2345678 2345678"
        "12345678 2345678 2345678 2345678 2345678 2345678" ); // 10 chunks
    for (int i = 0; i < digitizer.response().getNumSamples(); ++i )
        digitizer.response().setValue( i, i );

    for (int i = 0; i < digitizer.getNumNoise(); ++i ) 
        digitizer.noise(i).setValue( i );
}

void
setLocal( TestSubsystem::FileWithPermissions & file )
{ 
    file.fileHeader().filename().setValue( "12345678" ); //  2345678" );
    file.permissions().setValue( 002 );
}
    
void 
printStats( TestSubsystem & testSS ) 
{
    MonitorPointSet & mps = testSS.monitorPointSet();
    const SubsystemFrameBuffer & ssfb = mps.getBuffer();

    cout << "Test Subsystem Stats: " << endl
        << " MaxMonitorPoints = " << testSS.maxMonitorPoints() << endl
        << " MaxSamples = " << testSS.maxSamples() << endl
        << " Buff::maxNumMonPoints = " << ssfb.getMaxNumMonitorPoints() << endl
        << " Buff::maxNumSamples = " << ssfb.getMaxNumSamples() << endl
        << " (including Avgs) = " << ssfb.
            maxNumSamplesIncludingAverages ( ssfb.getMaxNumMonitorPoints(),
                                             ssfb.getMaxNumSamples() ) << endl
        << " ActualSamples = " << ssfb.getNumActualSamples() << endl;
//         << " Allocated samples = " << ssfb.getNumAllocatedSamples() << endl;
}

struct CheesyAutoWriterArgs {
    CheesyAutoWriterArgs( TestSubsystem & testSubsystem,
                          unsigned int writes );
    TestSubsystem & testSS;
    unsigned int numWrites; // # of times to write to the test subsystem.
    unsigned int numTriggers; // # of times a write triggers an exception.
    bool done;
    PthreadMutex doneMutex;
};

CheesyAutoWriterArgs::CheesyAutoWriterArgs( TestSubsystem & testSubsystem,
                                            const unsigned int writes ) :
    testSS( testSubsystem ),
    numWrites( writes ),
    numTriggers( 0 ),
    done( false ) { }
    

void cheesyAutowriter( CheesyAutoWriterArgs & args )
{
    cout << "Writing TestSubsystem " << args.numWrites << " times:" << endl;

    const int writesPerDot = args.numWrites / 80;

    // Write balls to the wall. 
    for ( unsigned i = 0; i < args.numWrites; ++i ) {
        try {
            args.testSS.write();
            if ( i % writesPerDot == 0 ) cout << "." << flush;
        } catch (...) {
            ++args.numTriggers;
        }
    }
    cout << endl;

    ScopedPthreadMutexLock lock( args.doneMutex );
    args.done = true;
}

//
// @version $Revision: 1.4 $
//
// @key reps 10000 int Number of write cycles to do in test.
//
// @usage use it
//
// @description
// Regression test for bug 751 - try to trigger by writing the test monitor
// subsystem as fast as possible from one thread while dynamically changing
// the number of monitor samples in another.
//
// @logger TEST_FACILITY carma.test.monitor.tBug751
int Program::main( ) {

    TestSubsystem testSS;

    setBox( testSS.box() );
    for ( int i = 0; i < TestSubsystem::Box::canDeviceCount(); ++i ) 
        setCanDevice( testSS.box().canDevice( i ) );

    for ( int i = 0; i < TestSubsystem::Fake::digitizerCount(); ++i ) {
        // setFakeDigitizer( testSS.fake().digitizer( i ) );
        // setLocal( testSS.fileWithPermissions() );
    }
    
    printStats( testSS );

    struct CheesyAutoWriterArgs args( testSS, getIntParameter( "reps" ) );
    
    StartPthreadWithRef( cheesyAutowriter, args, "CheesyAutoWriter Thread" );

    unsigned numSamples = 0;
    bool done = false;
    while ( !done ) {

        for ( unsigned i = 0; i < 10000; ++i ) { 
            unsigned ns = numSamples + 1;
            numSamples = ns % 20;
            testSS.box().mpint().setNumSamples( numSamples );
        }

        { 
            ScopedPthreadMutexLock lock( args.doneMutex );
            done = args.done;
        }
    }

    if ( args.numTriggers ) {
        cout << "Bug 751 may have triggered " << args.numTriggers 
            << " times - check the logs to verify.  If confirmed"
            << " please reopen the bug." << endl;
        return 1;
    } 
    
    return 0;

}
