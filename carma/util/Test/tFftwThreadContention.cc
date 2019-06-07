#include "carma/util/AutoPthreadQuitAndJoinGroup.h"
#include "carma/util/FftwRealToRealPlan.h"
#include "carma/util/OneShotBarrier.h"
#include "carma/util/Program.h"
#include "carma/util/StartPthread.h"

#include <unistd.h>
#include <iostream>
#include <sstream>

using namespace carma::util;
using namespace std;

namespace {


struct ThreadArgs {
    ThreadArgs( const FftwRealVector::size_type maxFftwSize, 
                OneShotBarrier & oneShotBarrier );
    FftwRealVector::size_type maxFftwSize_;
    OneShotBarrier & oneShotBarrier_;
};

ThreadArgs::ThreadArgs( const FftwRealVector::size_type maxFftwSize, 
                        OneShotBarrier & oneShotBarrier ) :
    maxFftwSize_( maxFftwSize ),
    oneShotBarrier_( oneShotBarrier ) 
{
    // Nothing
}

void createAndDestroyFftws( ThreadArgs & args ) 
{
    const FftwRealVector::size_type maxFftwSize = args.maxFftwSize_;

    FftwRealVector data( maxFftwSize );

    args.oneShotBarrier_.wait();

    for ( FftwRealVector::size_type size = 1; size < maxFftwSize; ++size ) {
        FftwRealToRealPlan makeAndBreak( 
            size, 
            data, 
            FftwRealToRealPlan::REAL_TO_HALFCOMPLEX );

        makeAndBreak.execute();
        if ( size % 50 == 0 ) cout << "." << flush;
    }

}

}

/**
 * @version $Revision: 1.1 $
 *
 * @description
 * Test to exercise FftwRealToRealPlan creation and destruction in a high 
 * thread contention environment.
 *
 * @usage tFftwThreadContention
 *
 * @noKeys
 *
 * @logger TEST_FACILITY carma.test.tFftwThreadContention
 */
int Program::main( ) 
{
    {
        const FftwRealVector::size_type maxFftwSize = 128; 
        const int nThreads = 15;

        cout << "Testing simultaneous FftwRealToRealPlan creation, "
             << "execution and destruction." << endl;
        
        cout << "   Creating " << nThreads << " threads with data sizes "
             << "ranging from 1 to " << maxFftwSize 
             << " in each thread." << endl;

        AutoPthreadQuitAndJoinGroup quitter;

        OneShotBarrier oneShotBarrier( nThreads + 1 );
        ThreadArgs threadArgs( maxFftwSize, oneShotBarrier );

        for ( int thread = 0; thread < nThreads; ++thread ) {
            ostringstream threadIdOss;
            threadIdOss << "thread # " << thread;
            const ::pthread_t threadId = StartPthreadWithRef(
                    createAndDestroyFftws, 
                    threadArgs,
                    threadIdOss.str() );
            quitter.insert( threadId, 
                            AutoPthreadQuitAndJoinGroup::JOIN_ONLY_ACTION );
        }
        cout << "   Waiting for threads to block on OneShotBarrier." << endl;
        oneShotBarrier.wait();
        cout << "   Joining on all threads and waiting for exit." << flush;
    } // Join on all threads via ~AutoPthreadQuitAndJoinGroup 
    
    cout << endl << "   Test completed successfully." << endl;
    return 0;
} 
