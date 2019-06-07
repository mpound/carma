//
// @version $Revision: 1.5 $ $Date: 2006/02/23 19:16:24 $
//
// @usage use it
//
// @description
//  Test program for benchmarking the performance of various allocators in
//  a multithreaded application.
//
// @key alloc   mp   string Choose one of {mp, malloc, gcc}.
// @key threads 4    int    Number of worker threads to spawn.
// @key frames  60   int    Number of frames for each thread to work.
// @key reps    750  int    Number of reps per thread per frame.
// @key clone   t    bool   Whether or not to clone strings to avoid contention.
//
// @logger TEST_FACILITY carma.test.util.benchmarkAllocators
//

#include <vector>
#include <iostream>
#include <stdexcept>

#include <pthread.h>

#include "carma/util/Program.h"
#include "carma/util/FrameAlignedTimer.h"
#include "carma/util/StartPthread.h"
#include "carma/util/MpAllocator.h"
#include "carma/util/MallocAllocator.h"

using namespace carma::util;

namespace {


template < typename T >
void
forceClone( T & s ) {
    ::std::vector< char > sAsCString( s.begin( ), s.end( ) );
    
    {
        T temp;
        
        s.swap( temp );
    }

    s = T( sAsCString.begin( ), sAsCString.end( ) );
}


template < typename T >
void
verifyWork( T       base,
            const T foo ) {
    const T suffixA( "_a_suffix" );

    const T basePlusSuffixA = base + suffixA;
    
    const T suffixB( "_b_suffix" );

    T mutableVersion = base + suffixB;
    
    if ( mutableVersion == basePlusSuffixA ) {
        ::std::cerr << "mistake #1.\n";
        
        throw ::std::runtime_error( "mistake #1" );
    }
    
    mutableVersion[ base.size( ) + 1 ] = 'a';

    if ( mutableVersion != basePlusSuffixA ) {
        ::std::cerr << "mistake #2.\n";
        
        throw ::std::runtime_error( "mistake #2" );
    }
    
    T fooCopy = foo;
    
    fooCopy[ 3 ] = '#';
    
    if ( mutableVersion == fooCopy ) {
        ::std::cerr << "mistake #3.\n";
        
        throw ::std::runtime_error( "mistake #3" );
    }
}


template < typename T >
void
doWork( T              base,
        const T        foo,
        const ::size_t numReps ) {
    char c = 'a';
    
    for ( ::size_t i = 0; i < numReps; ++i ) {
        T moddedBase = base;
        
        moddedBase[ 7 ] = c;
        
        verifyWork( moddedBase, foo );
        
        if ( c == 'z' )
            c = 'a';
        else
            ++c;
    }
}


template < typename T >
struct ThreadParams {
    ::size_t id;
    ::size_t numFrames;
    ::size_t numReps;
    bool     cloneStrings;
    T base;
    T foo;
};


template < typename T >
void
threadEntryPoint( const ThreadParams< T > & threadParams ) {
    ::std::cout << "thread #" << threadParams.id << " starting...\n";

    FrameAlignedTimer timer;
    
    T base = threadParams.base;
    T foo = threadParams.foo;

    if ( threadParams.cloneStrings ) {
        {
            if ( base.data( ) != threadParams.base.data( ) )
                ::std::cout << "Base copy was not a ref count copy before forcing a clone.\n";

            forceClone( base );
            
            if ( base.data( ) == threadParams.base.data( ) )
                ::std::cout << "Base copy was a ref count copy after forcing a clone.\n";
        }

        {
            if ( foo.data( ) != threadParams.foo.data( ) )
                ::std::cout << "Foo copy was not a ref count copy before forcing a clone.\n";

            forceClone( foo );
            
            if ( foo.data( ) == threadParams.foo.data( ) )
                ::std::cout << "Foo copy was a ref count copy after forcing a clone.\n";
        }
    }
    
    for ( ::size_t i = 0; i < threadParams.numFrames; ++i ) {
        if ( i == 0 )
            timer.ResetNextFireTimeAndWait( 2 );
        else
            timer.WaitForNextFireTime( );
            
        doWork( base, foo, threadParams.numReps );
    }
    
    ::std::cout << "thread #" << threadParams.id << " done.\n";
}


template < typename T >
void
doIt( const ::size_t numThreads,
      const ::size_t numFrames,
      const ::size_t numReps,
      const bool     cloneStrings ) {
    ::std::cout << "Starting "
                << numThreads
                << " threads each doing "
                << numReps
                << " reps per frame for "
                << numFrames
                << " frames "
                << (cloneStrings ? "with" : "without")
                << " string cloning.\n";
                
    ::std::vector< ::pthread_t > threads;

    threads.resize( numThreads );

    for ( ::size_t i = 0; i < numThreads; ++i )
        threads[ i ] = 0;

    T masterBase = "base_x_y_string";
    T masterFoo = "check_yoself_befo_you_wreck_yoself";

    try {
        for ( ::size_t i = 0; i < numThreads; ++i ) {
            ThreadParams< T > threadParams;
            
            threadParams.id = i;            
            threadParams.numFrames = numFrames;
            threadParams.numReps = numReps;
            threadParams.cloneStrings = cloneStrings;
            threadParams.base = masterBase;            
            threadParams.base[ 5 ] = static_cast< char >( 'a' + i );
            threadParams.foo = masterFoo;
            
            threads[ i ] = StartPthreadWithCopy( threadEntryPoint, threadParams );
        }
    } catch ( ... ) {
        ::std::cerr << "Error spawning threads.\n";
    }
    
    for ( ::size_t i = 0; i < numThreads; ++i ) {
        void * threadResult = 0;
        
        ::pthread_join( threads[ i ], &threadResult );
    }
}


typedef ::std::basic_string< char, ::std::char_traits< char >, carma::util::MpAllocator< char > > MpString;
typedef ::std::basic_string< char, ::std::char_traits< char >, carma::util::MallocAllocator< char > > MallocString;

}  // anonymous namespace


int
carma::util::Program::main( ) {
    const ::std::string allocType = getStringParameter( "alloc" );
    
    if ( allocType == "mp" ) {
        ::std::cout << "Using carma::util::MpAllocator.\n";
        
        doIt< MpString >( getIntParameter( "threads" ),
                          getIntParameter( "frames" ),
                          getIntParameter( "reps" ),
                          getBoolParameter( "clone" ) );
    } else if ( allocType == "malloc" ) {
        ::std::cout << "Using carma::util::MallocAllocator.\n";
        
        doIt< MallocString >( getIntParameter( "threads" ),
                              getIntParameter( "frames" ),
                              getIntParameter( "reps" ),
                              getBoolParameter( "clone" ) );
    } else if ( allocType == "gcc" ) {
        ::std::cout << "Using gcc's default allocator.\n";

        doIt< ::std::string >( getIntParameter( "threads" ),
                               getIntParameter( "frames" ),
                               getIntParameter( "reps" ),
                               getBoolParameter( "clone" ) );
    } else {
        ::std::cerr << "ERROR: Unknown allocator type\n";
        
        return 1;
    }
    
    return 0;
}
