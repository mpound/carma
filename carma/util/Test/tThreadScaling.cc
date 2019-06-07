#include <iostream>
#include <vector>

#include <sys/time.h>
#include <unistd.h>

#include "carma/util/Program.h"
#include "carma/util/StartPthread.h"
#include "carma/util/ThreadQuit.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedLock.h"


using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


struct ThreadParams {
    PthreadMutex *   startBarrier;
    long long        iterations;
    long long        startMicroseconds;
    long long        endMicroseconds;
    vector< double > values;
    double           average;
    long long        iterationsUntilDeliberateSegv;
    
    explicit ThreadParams( PthreadMutex &           startBarrierIn,
                           const vector< double > & valuesIn,
                           const long long          iterationsUntilDeliberateSegvIn ) :
    startBarrier( &startBarrierIn ),
    iterations( 0 ),
    startMicroseconds( 0 ),
    endMicroseconds( 0 ),
    values( valuesIn ),
    average( 0.0 ),
    iterationsUntilDeliberateSegv( iterationsUntilDeliberateSegvIn ) {
    }
};


void
WorkLoop( ThreadParams & params ) {
    struct ::timeval startTime;

    const long long iterationsUntilDeliberateSegv =
        params.iterationsUntilDeliberateSegv;
        
    long long iterations = 0;

    try {
        {
            ScopedLock< PthreadMutex >
                barrierLock( *(params.startBarrier) );
        }
        
        ::gettimeofday( &startTime, 0 );
        
        while ( true ) {
            vector< double >::const_iterator i = params.values.begin( );
            const vector< double >::const_iterator iEnd = params.values.end( );
            
            double total = 0.0;
            
            while ( i != iEnd ) {
                total += *i;
                
                ThreadQuitTestSelf( );
                
                ++i;
            }               
            
            params.average = (total / params.values.size( ));
                
            ++iterations;

            if ( iterations == iterationsUntilDeliberateSegv ) {
                int * const foo = reinterpret_cast< int * >( 0x000000c3 );
                
                *foo = 19;
            }
        }
    } catch ( ... ) {
        struct ::timeval endTime;
        ::gettimeofday( &endTime, 0 );

        params.iterations = iterations;

        const long long microsecondsPerSecond = 1000LL * 1000LL;
        
        params.startMicroseconds =
            startTime.tv_sec * microsecondsPerSecond + startTime.tv_usec;

        params.endMicroseconds =
            endTime.tv_sec * microsecondsPerSecond + endTime.tv_usec;
            
        throw;
    }
}


void
TestIt( const int numThreads,
        const int segvIters ) {
    cout << numThreads << " threads:" << endl;
    
    PthreadMutex startBarrier;
    
    vector< ::pthread_t > threads;
    vector< ThreadParams > params;

    vector< double > values;
    
    values.push_back( 3.1416 );
    values.push_back( -3.1416 );
    values.push_back( 22.3 );
    values.push_back( 11.0 );
    values.push_back( 121.0 );
    values.push_back( 84632874673.0 );
    values.push_back( -9347239472.0 );
    values.push_back( 1.27345605 );

    for ( int i = 0; i < numThreads; ++i )
        params.push_back( ThreadParams( startBarrier, values, segvIters ) );

    {
        ScopedLock< PthreadMutex >
            barrierLock( startBarrier );
            
        for ( int i = 0; i < numThreads; ++i )           
            threads.push_back( StartPthreadWithRef( WorkLoop, params[ i ] ) );
            
        ::sleep( 2 );
    }
    
    ::sleep( 5 );
    
    for ( int i = 0; i < numThreads; ++i )
        RequestThreadQuit( threads[ i ] );
    
    for ( int i = 0; i < numThreads; ++i ) {
        void * threadResult = 0;
    
        ::pthread_join( threads[ i ], &threadResult );
    }
    
    long long totalIterations = 0;

    long long minStartMicroseconds = 0x7FFFffffFFFFffffLL;
    long long maxStartMicroseconds = -(0x7FFFffffFFFFfffeLL);

    long long minEndMicroseconds = 0x7FFFffffFFFFffffLL;
    long long maxEndMicroseconds = -(0x7FFFffffFFFFfffeLL);
    
    for ( int i = 0; i < numThreads; ++i ) {
        cout << "    thread #"
                  << (i + 1)
                  << ": "
                  << params[ i ].iterations
                  << " iterations in "
                  << ((params[ i ].endMicroseconds - params[ i ].startMicroseconds) / 1000.0)
                  << " milliseconds of wall time"
                  << endl;
        
        minStartMicroseconds = ::std::min( minStartMicroseconds, params[ i ].startMicroseconds );
        maxStartMicroseconds = ::std::max( maxStartMicroseconds, params[ i ].startMicroseconds );

        minEndMicroseconds = ::std::min( minEndMicroseconds, params[ i ].endMicroseconds );
        maxEndMicroseconds = ::std::max( maxEndMicroseconds, params[ i ].endMicroseconds );

        totalIterations += params[ i ].iterations;
    }
    
    const long long startWindowMicroseconds = (maxStartMicroseconds - minStartMicroseconds);

    cout << "    start wall time window of "
              << (startWindowMicroseconds / 1000.0)
              << " milliseconds"
              << endl;

    const long long endWindowMicroseconds = (maxEndMicroseconds - minEndMicroseconds);

    cout << "    end wall time window of "
              << (endWindowMicroseconds / 1000.0)
              << " milliseconds"
              << endl;

    const long long windowMicroseconds = (maxEndMicroseconds - minStartMicroseconds);
    
    cout << "    "
              << totalIterations
              << " total iterations over a wall time window of "
              << (windowMicroseconds / 1000.0)
              << " milliseconds"
              << endl;
              
    cout << "    "
              << (totalIterations / (windowMicroseconds / 1000.0))
              << " iterations/millisecond"
              << endl;
}


}  // anonymous namespace


//
// @key minThreads 1 int
//      Minimum number of threads to test
//
// @key maxThreads 8 int
//      Maximum number of threads to test
//
// @key segvIters @noDefault int
//      Iterations before deliberately causing a seg fault 
//
// @logger TEST_FACILITY carma.test.util.tThreadScaling
//

int
Program::main( ) {
    const int minThreads = getIntParameter( "minThreads" );
    const int maxThreads = getIntParameter( "maxThreads" );
    
    if ( minThreads > maxThreads )
        throw runtime_error( "minThreads > maxThreads" );

    const int segvIters = 
        (parameterWasSpecified( "segvIters" ) ?
            getIntParameter( "segvIters" ) :
            0);

    for ( int i = minThreads; i <= maxThreads; ++i )
        TestIt( i, segvIters );
    
    return 0;
}
