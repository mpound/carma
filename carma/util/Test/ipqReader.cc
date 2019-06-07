//
// @version $Revision: 1.27 $ $Date: 2008/02/12 22:23:06 $
//
// @usage   use it
//
// @description
//  Test program for IPQ reading. Run in conjunction with
//  ipqWriter
//
// @key dump    1   i   dump every nth element write (0 = none)
// @key interactive t   b   whether to run in interactive mode or not
// @key num 0   i   number of reads to wait for (0 = infinite)
// @key verbose t   b   debug output
// @key to      0   i   test offset for queues
//
// @logger TEST_FACILITY carma.test.util.ipqReader
//

#include <fstream>
#include <sstream>

#include <unistd.h>

#include "carma/util/Test/ipqWriterReaderData.h"
#include "carma/util/ErrorException.h"
#include "carma/util/IPQreader.h"
#include "carma/util/IPQfileReader.h"
#include "carma/util/ScopedSingleCharIoMode.h"
#include "carma/util/Time.h"
#include "carma/util/Program.h"
#include "carma/util/ThreadQuit.h"
#include "carma/util/StartPthread.h"
#include "carma/util/AutoPthreadQuitAndJoinGroup.h"
#include "carma/util/VmSize.h"


using namespace ::std;
using namespace carma;
using namespace carma::util;
using namespace carma::util::test;


namespace {


struct Params {
    unsigned long dumpEveryNth;
    unsigned long num;
    bool verbose;
    unsigned long to;
};

void
ReaderThreadEntryPoint( const Params& params ) {

    const bool doMemoryUsage = false;
    {
        //bool timing = false; 
        cout.setf( ios::fixed );

        // Create the data queue
        IPQreader< Element > ipq( fname, false, 0, params.to );

        // Memory usage test
        if (doMemoryUsage) {
            IPQreader<Element>* ipp[100000];
            for (int k=0; k<100000; k++) {
                try {
                    ipp[k] = new IPQreader<Element>(fname, false, 0, params.to);
                }
                catch (ErrorException e) {
                    cout << e.getMessage() << endl;
                    throw(e);
                }    
                catch (exception e) {
                    cout << e.what() << endl;
                    throw(e);
                }  
                VmSize v = VmSize();
                
                cout << "Count=" << k+2 << "  " 
                     << v.getSizeKB() << " KB   " 
                     << v.getStringHuman(3) << endl;
            }
        }
        // End memory usage test

        unsigned long long numThusFar = 0;
        // Loop, dumping data
        
        int idx = -1;
        while ( (params.num == 0) || (numThusFar < params.num) ) {
            // Blocking read; wait for new data to be available
            try {
                if (params.verbose) {
                    cout 
                     <<  "putOff:"   << ipq.getPutOffset() 
                     << " getOff:"   << ipq.getGetOffset() 
                     << " maxOff:"   << ipq.getMaxOffset() 
                     << " queSize:"  << ipq.getQueueSize()
                     << " eleSize:"  << ipq.getElementSize()
                     << " numAvail:" << ipq.getNumAvailable() 
                     << " lost:"     << ipq.getLostElementCount() 
                     << " idx:"      << idx
                     << endl;
                }
                
                int lost = ipq.read( );
                idx = ipq.i;
                
                ++numThusFar;
                            
                if ( lost > 0 )
                    cout << "Read lost:" << lost << endl;

                if ( (params.dumpEveryNth != 0) && ((numThusFar % params.dumpEveryNth) == 0) ) {
                    cout << "read #" <<  numThusFar << ": ";               
                    ipq.dump( );
                }

#if 0
                {
                    Element aa; 
                    Element aaa;

                    double t1 = Time::MJD( );
                    bool hasData = ipq.readNewest( );
                    double t2 = Time::MJD( );
                    double delta = Time::MILLISECONDS_PER_DAY * (t2 - t1);

                    if ( timing ) {
                        cout << "readNewestLocked:" 
                                  << (delta * 1000)
                                  << "  " << ipq.getElementSize() 
                                  << "  msec/MB:" 
                                  << setprecision(2) << ((1.0e+6 * delta) / ipq.getElementSize( ))
                                  << endl;
                    }
                    t1 = Time::MJD();
                    aa = aaa;
                    t2 = Time::MJD();
                    delta = Time::MILLISECONDS_PER_DAY*(t2-t1);
                    if (timing)cout << "objcopy:" 
                                  << delta*1000 
                                  << "  " << ipq.getElementSize() 
                                  << "  msec/MB:" 
                                  << setprecision(2) << 1e6*delta/ipq.getElementSize() 
                                  << endl;
                    t1 = Time::MJD();
                    memcpy(&aa, &aaa, sizeof(aa));
                    t2 = Time::MJD();
                    delta = Time::MILLISECONDS_PER_DAY*(t2-t1);
                    if (timing)cout << "memcopy:" 
                                  << delta*1000 
                                  << "  " << ipq.getElementSize() 
                                  << "  msec/MB:" 
                                  << setprecision(2) << 1e6*delta/ipq.getElementSize() 
                                  << endl;
                    hasData = ipq.readNewestNoLock();
                    hasData = ipq.readNewest();
                    hasData = ipq.readNewestNoLock();
                }
#endif
            } catch ( ... ) {
                RethrowCaughtExceptionIfThreadQuitRequestedError( );
                
                cerr << "Caught exception on IPQ read operation";
                
                try {
                    throw;
                } catch ( const ::std::exception & e ) {
                    cerr << ":\n    " << e.what( );
                } catch ( ... ) {
                    // Do nothing
                }
                
                cerr << "\n    Exiting reader thread..." << endl;
                
                throw;
            }
        }
    }
    
    cout << "Read thread is done" << endl;
}


}  // anonymous namespace


int
Program::main( )
{
    int result = EXIT_FAILURE;
    
    try {
        const int dumpEveryNth = getIntParameter( "dump" );
        const bool interactiveMode = getBoolParameter( "interactive" );
        const int num = getIntParameter( "num" );
        const int to  = getIntParameter( "to" );
        
        if ( interactiveMode ) cout << "Enter \"q\" to exit" << endl;
        
        {
            struct Params params;

            params.verbose = getBoolParameter("verbose");
            if ( dumpEveryNth < 0 )
                throw runtime_error( "dump cannot be negative" );
        
            params.dumpEveryNth = dumpEveryNth;

            if ( num < 0 )
                throw runtime_error( "num cannot be negative" );

            params.num = num;
            params.to  = to;
            
            const ::pthread_t thread =
                StartPthreadWithCopy( ReaderThreadEntryPoint, params );

            const AutoPthreadQuitAndJoinGroup
                autoJoin( thread,
                          AutoPthreadQuitAndJoinGroup::JOIN_ONLY_ACTION );

            if ( interactiveMode ) {
                const ScopedSingleCharIoMode singleCharIoMode;
                
                while ( true ) {
                    char s;
                    
                    cin >> s;
                    
                    if ( (s == 'q') || (s == 'Q') )
                        break;
                }
                
                try {
                    RequestThreadQuit( thread );
                } catch ( ... ) {
                    // just stifle any exception
                }
            }
        }
        
        cout << "Exiting ipq test reader" << endl;
        
        result = EXIT_SUCCESS;
    } catch ( ... ) {
        result = EXIT_FAILURE;  
    }

    return result;
}
