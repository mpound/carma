#include "carma/util/Program.h"
#include "carma/corba/corba.h"

#include "carma/dbms/TagIDAuthority.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/StartPthread.h"
#include "carma/util/Trace.h"
#include <string>

using namespace ::std;
using namespace ::CORBA;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;

namespace {


struct ThreadArgs {
    const int             threadNo;
    const int             numCycles;
    const bool            stringTest;
    const bool            numTest;
    const bool            boolTest;
    const bool            enumTest;
    const int             sleeptime;
    CarmaMonitorSystem *  cms;
    
    explicit ThreadArgs( int                     inThreadNo,
                         int                     inNumCycles,
                         bool                    inStringTest,
                         bool                    inNumTest,
                         bool                    inBoolTest,
                         bool                    inEnumTest,
                         int                     inSleepTime,
                         CarmaMonitorSystem *    inCms );
};


ThreadArgs::ThreadArgs( const int              inThreadNo,
                        const int              inNumCycles,
                        const bool             inStringTest,
                        const bool             inNumTest,
                        const bool             inBoolTest,
                        const bool             inEnumTest,
                        const int              inSleepTime,
                        CarmaMonitorSystem *   inCms ):
threadNo( inThreadNo ),
numCycles( inNumCycles ),
stringTest( inStringTest ),
numTest( inNumTest ),
boolTest( inBoolTest ),
enumTest( inBoolTest ),
sleeptime( inSleepTime),
cms( inCms )
{
}


const char * const kSourceNames[] =
    {
        "GDRA",
        "AGEM",
        "ATAU",
        "AUMA",
        "GUMA",
        "BTAU",
        "BUMA",
        "BAUR",
        "EUMA"
    };

const int kNumSourceNames =
    (sizeof( kSourceNames ) / sizeof( kSourceNames[ 0 ] ));
    
const char * const kProjects[] =
    {
        "base.foo.bar.1",
        "flux.oria.13",
	"test.hello.1",
	"fringe.various.sources.22",
	"cx007.yomama.isugly.3",
	"lex.luthor.rulez.8"
    };

const int kNumProjects = 6;


void
stressStrings (const CarmaMonitorSystem * cms,
	       const int numCycles,
	       const int sleeptime,
	       const int threadNo
	)
{
    CARMA_CPTRACE( Trace::TRACE2, "Stressing strings #" <<threadNo + 1
	    << " numCycles = " << numCycles );

    for (int i = 0; i < numCycles; i++ ) {

	for ( int j = 0; j < kNumProjects; j++ ) {
	    cms->control().subarray(0).obsBlockId().setValue( kProjects[j] );
	    if (sleeptime>0) usleep(sleeptime);
	}

	for ( int k = 0; k < kNumSourceNames; k++ ) {
	    cms->control().subarray(0).commands()
		         .track().sourcename().setValue( kSourceNames[k] );
	}

        if (sleeptime>0) usleep(sleeptime);
    }
}

void
stressNumbers (const CarmaMonitorSystem * cms,
	       const int numCycles,
	       const int sleeptime,
	       const int threadNo
	)
{
    CARMA_CPTRACE( Trace::TRACE2, "Stressing numbers #" <<threadNo + 1
	    << " numCycles = " << numCycles );

    for (int i = 0; i < numCycles; i++ ) {
	for ( int j = 0; j < 100 ; j++ ) {
	    cms->control().subarray(0).phaseCenterRa()
		          .setValue( static_cast<double>(j) );
	    cms->control().subarray(0).phaseCenterDec()
		          .setValue( static_cast<double>(2*j) );
	    cms->control().subarray(0).meanEquinox()
		          .setValue( static_cast<double>(100*j) );
	    cms->control().subarray(0).planetTemperature()
		          .setValue( static_cast<double>(100*j) );
	    cms->control().antenna(4).delayOffset3mmRx()
		          .setValue( static_cast<float>(134*j) );
	    cms->control().antenna(4).padOffset().north()
		          .setValue( static_cast<float>(345*j) );

	    if (sleeptime>0) usleep(sleeptime);
	}

	if (sleeptime>0) usleep(sleeptime);
    }
}

void
stressBools (const CarmaMonitorSystem * cms,
	     const int numCycles,
	     const int sleeptime,
	     const int threadNo
	    )
{
    CARMA_CPTRACE( Trace::TRACE2, "Stressing bool #" <<threadNo + 1
	    << " numCycles = " << numCycles );

    bool val;
    for (int i = 0; i < numCycles; i++ ) {
	for ( int j = 0; j < 100 ; j++ ) {
	    val = cms->control().subarray(0).controllerRunning().getValue();
	    cms->control().subarray(0).controllerRunning().setValue( !val );
	    val = cms->control().subarray(0).controllerInitialized()
		                .getValue();
	    cms->control().subarray(0).controllerInitialized().setValue( !val );
	    val = cms->control().subarray(0).reachable().sldcSystem()
		                .getValue();
	    cms->control().subarray(0).reachable().sldcSystem()
		          .setValue( !val );
	    val = cms->control().subarray(0).reachable().loberotator()
		                .getValue();
	    cms->control().subarray(0).reachable().loberotator()
		          .setValue( !val );
	    val = cms->control().antenna(1)
		                .antennaReachable().cryo().getValue();
	    cms->control().antenna(1)
		          .antennaReachable().cryo().setValue( !val );
	    val = cms->control().antenna(1)
		                .antennaReachable().calibrator().getValue();
	    cms->control().antenna(1)
		          .antennaReachable().calibrator().setValue( !val );

	    if (sleeptime>0) usleep(sleeptime);
	}

        if (sleeptime>0) usleep(sleeptime);
    }
}

void
stressEnums (const CarmaMonitorSystem * cms,
	     const int numCycles,
	     const int sleeptime,
	     const int threadNo
	    )
{
    CARMA_CPTRACE( Trace::TRACE2, "Stressing bool #" <<threadNo + 1
	    << " numCycles = " << numCycles );

    typedef ControlSubsystemBase::VelDefMonitorPointEnum   VDE;
    typedef ControlSubsystemBase::VelFrameMonitorPointEnum VFE;
    typedef ControlSubsystemBase::SidebandMonitorPointEnum SBE;
    typedef VDE::VELDEF   VDEF;
    typedef VFE::VELFRAME VFRAME;
    typedef SBE::SIDEBAND SB;

    VFRAME velframe;
    VDEF   veldef;
    SB    sb;

    for (int i = 0; i < numCycles; i++ ) {
	for ( int j = 0; j < 100 ; j++ ) {

	    if ( j % 2 == 0 ) {
		velframe = VFE::OBSERV;
		veldef   = VDE::OPTICAL;
		sb       = SBE::USB;

	    } else {
		velframe = VFE::LSR;
		veldef   = VDE::RADIO;
		sb       = SBE::LSB;
	    }
		
	    cms->control().subarray(0).velFrame().setValue( velframe );
            cms->control().subarray(0).velDef().setValue( veldef );
            cms->control().subarray(0).commands()
		          .freq().sideband().setValue( sb );

	    if (sleeptime>0) usleep(sleeptime);
	}

        if (sleeptime>0) usleep(sleeptime);
    }
}

void
entryPoint( const ThreadArgs & args )
try {
    CARMA_CPTRACE( Trace::TRACE2, "Entered." );
    
    if ( args.stringTest )
        stressStrings( args.cms, args.numCycles, 
		       args.sleeptime, args.threadNo );

    if ( args.numTest )
        stressNumbers( args.cms, args.numCycles, 
		       args.sleeptime, args.threadNo );
    
    if ( args.boolTest )
        stressBools( args.cms, args.numCycles, 
		     args.sleeptime, args.threadNo );

    if ( args.enumTest )
        stressEnums( args.cms, args.numCycles, 
		     args.sleeptime, args.threadNo );
    
} catch ( ... ) {
    logCaughtAsError( );
    
    throw;
}


void
testIt( const int   numThreads,
        const int   numCycles,
        const bool  stringTest,
        const bool  numTest,
        const bool  boolTest,
        const bool  enumTest,
        const int   sleeptime,
        CarmaMonitorSystem * cms
      )
{
    CARMA_CPTRACE( Trace::TRACE2,
                   "Creating " << numThreads << " threads." );

    vector< ::pthread_t > threads;

    {
        for ( int i = 0; i < numThreads; ++i ) {
            string threadNdc;

            {
                ostringstream oss;
                oss << "thread #" << (i + 1);
                threadNdc = oss.str();
            }

            const ThreadArgs args( i,
                                   numCycles,
                                   stringTest,
                                   numTest,
                                   boolTest,
                                   enumTest,
                                   sleeptime,
                                   cms
                                 );

            threads.push_back( StartPthreadWithCopy( entryPoint,
                                                     args,
                                                     threadNdc ) );
            
        }
    }

    CARMA_CPTRACE( Trace::TRACE2,
                   "Joining to " << threads.size() << " threads." );

    {
        for ( ::size_t i = 0; i < threads.size(); ++i ) {
            void * threadResult = 0;

            ::pthread_join( threads[ i ], &threadResult );
        }
    }

    CARMA_CPTRACE( Trace::TRACE2, "All threads completed." );
}


}  // namespace < anonymous >


//
// @version $Revision: 1.5 $
//
// @usage 
//   Stress test the Monitor system against multithreaded writes.
//
// @description
//   A test binary that stress tests the Monitor system by instantiating
//   several threads and using them to simultaneously modify monitor
//   point values.
//   On Marc's laptop, the following will expose a monitor system
//   bug:
//   stressMonsys sleep=1 cycles=1000 stringTest=t
//   Get errors about number of samples which is probably an overflow
//   akin to the 'validity bug'.
//
// @key threads 5 int
//      Number of threads to use in the multithreaded testing. 0 or less means
//      no multithreaded testing.
//
// @key stringTest true bool
//      Whether or not to test string monitor points 
//
// @key numberTest true bool
//      Whether or not to test numeric monitor points 
//
// @key boolTest true bool
//      Whether or not to test boolean monitor points 
//
// @key enumTest true bool
//      Whether or not to test enum monitor points 
//
// @key cycles 100 int
//      number of cycles in the test, per thread
//
// @key sleep   1 int
//      microseconds to sleep at end of write loops
//
// @logger TEST_FACILITY carma.test.control.stressMonSys
//

int
Program::main( )
{
    try {
        const int threadsParam = getIntParameter( "threads" );
        const bool stringTest  = getBoolParameter( "stringTest" );
        const bool numTest     = getBoolParameter( "numberTest" );
        const bool boolTest    = getBoolParameter( "boolTest" );
        const bool enumTest    = getBoolParameter( "enumTest" );
        const int  numCycles   = getIntParameter( "cycles" );
        const int  sleepTime   = getIntParameter( "sleep" );

            carma::monitor::CarmaMonitorSystem * cms = new CarmaMonitorSystem;

        testIt( threadsParam, numCycles, 
            stringTest, numTest, boolTest, enumTest,
            sleepTime, cms );
        
        programLogInfoIfPossible( "shutting down the tag ID authority" );
        dbms::TagIDAuthority::closeAuthority();
    } catch ( ... ) {
        cerr << getArg0() << " exiting with failure!" << endl;
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
