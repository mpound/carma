#include <iostream>
#include <vector>
#include <stdexcept>

#include <unistd.h>
#include <pthread.h>
#include <sys/time.h>

#include "carma/util/Program.h"
#include "carma/util/PthreadMutex.h"

static ::size_t gMinLockCount;
static ::size_t gNumThreads;

static ::pthread_mutex_t gNumThreadsDoneGuard = PTHREAD_MUTEX_INITIALIZER;
static ::size_t gNumThreadsDone;

static ::pthread_cond_t gAllThreadsDoneCond = PTHREAD_COND_INITIALIZER;

static carma::util::PthreadMutex gMutex;
static ::size_t gLockCounter;

static void *
EntryPoint( void * arg ) {
  void * result = &arg;

  try {
    if ( arg == 0 )
      throw std::invalid_argument( "arg is NULL" );

    ::size_t & myCounter(*(static_cast< ::size_t * >( arg )));

    myCounter = 0;

    bool done = false;

    do {      
      gMutex.Lock( );
      
      const ::size_t lockCount = ++gLockCounter;

      gMutex.Unlock( );

      ++(myCounter);
      
      done = (lockCount >= gMinLockCount);
    } while ( done == false );
    
    ::pthread_mutex_lock( &gNumThreadsDoneGuard );
    
    const ::size_t numThreadsDone = ++gNumThreadsDone;
    
    ::pthread_mutex_unlock( &gNumThreadsDoneGuard );

    if ( numThreadsDone == gNumThreads )
        ::pthread_cond_signal( &gAllThreadsDoneCond );

    result = 0;
  } catch ( ... ) {
    result = &arg;

    std::cerr << "Exception caught trying to escape EntryPoint." << std::endl;
  }

  return result;
}


static void
Test( ) {
  std::cout << gNumThreads << " threads: " << std::flush;

  struct ::timeval startTime;
  struct ::timeval endTime;

  gNumThreadsDone = 0;
  gLockCounter = 0;

  std::vector< ::size_t > threadCounters;

  for ( ::size_t threadNo = 0; threadNo < gNumThreads; ++threadNo )
    threadCounters.push_back( 0 );

  gMutex.Lock( );

  std::vector< pthread_t > threads;

  for ( ::size_t threadNo = 1; threadNo < gNumThreads; ++threadNo ) {
    ::pthread_t thread;
    
    ::pthread_create( &thread, 0, EntryPoint, &(threadCounters[ threadNo ]) );
    
    threads.push_back( thread );
  }

  ::sleep( 2 );

  ::gettimeofday( &startTime, 0 );

  gMutex.Unlock( );

  EntryPoint( &(threadCounters[ 0 ]) );

  ::pthread_mutex_lock( &gNumThreadsDoneGuard );
    
  while ( true ) {
    if ( gNumThreadsDone == gNumThreads ) {
      ::gettimeofday( &endTime, 0 );
      break;
    }
    
    ::pthread_cond_wait( &gAllThreadsDoneCond, &gNumThreadsDoneGuard );
  }
  
  ::pthread_mutex_unlock( &gNumThreadsDoneGuard );
  
  {
    std::vector< pthread_t >::const_iterator i = threads.begin( );
    const std::vector< pthread_t >::const_iterator iEnd = threads.end( );

    for ( ; i != iEnd; ++i ) {
      void * threadResult = &i;

      ::pthread_join( *i, &threadResult );
    }
  }

  const double ellapsed = (endTime.tv_sec + 1.0e-6 * endTime.tv_usec) - 
                          (startTime.tv_sec + 1.0e-6 * startTime.tv_usec);

  const double rate = (gLockCounter / ellapsed);

  if ( rate >= 1.0e+9 )
    std::cout << (rate * 1.0e-9) << " billion";
  else if ( rate >= 1.0e+6 )
    std::cout << (rate * 1.0e-6) << " million";
  else if ( rate >= 1.0e+3 )
    std::cout << (rate * 1.0e-3) << " thousand";
  else
    std::cout << rate;

  std::cout << " locks/second." << std::endl;

  if ( true ) {
    for ( ::size_t threadNo = 0; threadNo < gNumThreads; ++threadNo ) {
      std::cout << "Thread #" << threadNo << " locked " << threadCounters[ threadNo ] << " times.\n";
    }
    std::cout << "Final lock count: " << gLockCounter << "\n" << std::endl;
  }
}


//
// @noKeys
//
// @logger TEST_FACILITY carma.test.util.PthreadMutexContention
//
int
carma::util::Program::main( ) {
  int result = -1;

  try {
    gMinLockCount = 10UL * 1000UL * 1000UL;

    for ( gNumThreads = 1; gNumThreads <= 10; ++gNumThreads )
      Test( );

    result = 0;
  } catch ( ... ) {
    result = -2;
  }

  return result;
}
