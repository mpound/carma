
/**
 * $Id: SemaphoreOperator.cc,v 1.2 2007/11/28 07:57:12 colby Exp $
 *
 */

#include "carma/util/Trace.h"
#include "carma/util/ErrorException.h"
#include "carma/util/SemaphoreException.h"
#include "carma/util/SemaphoreTimedOutException.h"

#include "carma/util/SemaphoreOperator.h"

#include <iostream>

#include <unistd.h>
#include <sys/sem.h>

using namespace std;
using namespace carma::util;

const key_t SemaphoreOperator::_key = 0xCEADA4F5;

SemaphoreOperator::SemaphoreOperator( int semnum )
  :
  _semnum( semnum )
{
  int cflags = S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH|IPC_CREAT;
  ostringstream errMsg;

  CPTRACE( Trace::TRACE6, "  c'tor attempting to create semaphore" );
  _semid = semget( _key, MAX_SEMS, cflags );

  // if _semid remains -1, and errno is EEXIST, then the sem
  // has already been created.  That's great!  Lets go
  // ahead and open it regular like then...
  if ( _semid == -1 && errno == EEXIST )
  {                                    
    CPTRACE( Trace::TRACE6, "   c'tor semaphore exists, acquiring" );
    _semid = semget( _key, 0, 0 );

    if ( _semid == -1 )
    {
      // Uh oh!  DAMN IT!  DAMN!  IT!  TO!  HEEEELLLLL!
      errMsg << "Unable to acquire semaphore: "
	<< strerror( errno );
      CPTRACE( Trace::TRACE6, "    c'tor could not create, nor acquire semaphore: "
	 << errMsg.str() );
      throw CARMA_EXCEPTION( SemaphoreException, errMsg.str() );
    }
  }
  else if ( _semid != -1 )
  {

    CPTRACE( Trace::TRACE6, "   c'tor created, reseting" );

    // We just created the semaphore, set some parameters on it
    _semop.sem_num = _semnum;
    _semop.sem_op = 1;
    _semop.sem_flg = IPC_NOWAIT;

    if ( semop( _semid, &_semop, 1 ) < 0 )
    {
      // Only bother if error is other than EAGAIN
      // This means some other process has beat us
      // to the reset.  If that happens, it's possible
      // Lock/Unlocks will not work properly..? TODO
      if ( errno != EAGAIN )
      {
	errMsg << "Unable to reset semaphore: "
	  << strerror( errno );
	CPTRACE( Trace::TRACE6, "    c'tor unable to reset: "
	    << errMsg.str() );
	throw CARMA_EXCEPTION( SemaphoreException, errMsg.str() );
      }
    }
     
    // Now set up relatively constant parameters
    _semop.sem_num = semnum;
    _semop.sem_flg = SEM_UNDO;
  }
  else
  {
    // Something else went wrong
    errMsg << "Unable to create semaphore: " 
	<< strerror( errno );
    CPTRACE( Trace::TRACE6, "    c'tor unable to create: "
	<< errMsg.str() );
    throw CARMA_EXCEPTION( SemaphoreException, errMsg.str() );
  }
}


void SemaphoreOperator::Lock( struct timespec *timeout )
{
  int semstat;

  CPTRACE( Trace::TRACE6,
      "  SO::Lock:_semid: " << _semid << " semnum: " << _semnum );

  _semop.sem_op = -1;

  semstat = semtimedop( _semid, &_semop, 1, timeout );

  if ( semstat < 0 && errno != EAGAIN )
  {
    ostringstream errMsg;
    errMsg << "Unable to lock _semid: "
      << _semid << " semnum: " << _semnum
      << " : " << strerror(errno);
    CPTRACE( Trace::TRACE6, "    SO::Lock error: "
	<< errMsg.str() );
    throw CARMA_EXCEPTION( SemaphoreException, string( errMsg.str() ) );
  }
  else if ( semstat < 0 && errno == EAGAIN )
  {
    throw CARMA_EXCEPTION( SemaphoreTimedOutException, "timed out" );
  }

}

void SemaphoreOperator::Unlock( struct timespec *timeout )
{
  int semstat;

  CPTRACE( Trace::TRACE6,
      "     SO::Unlock:_semid: " << _semid << " semnum: " << _semnum );

  _semop.sem_op = 1; 

  semstat = semtimedop( _semid, &_semop, 1, timeout );

  if ( semstat < 0 && errno != EAGAIN )
  {
    ostringstream errMsg;
    errMsg << "Unable to unlock _semid: "
      << _semid << " semnum: " << _semnum
      << " : " << strerror(errno);
    CPTRACE( Trace::TRACE6, "    SO::Unlock error: "
	<< errMsg.str() );
    throw CARMA_EXCEPTION( SemaphoreException, string( errMsg.str() ) );
  }
  else if ( semstat < 0 && errno == EAGAIN )
  {
    throw CARMA_EXCEPTION( SemaphoreTimedOutException, "timed out" );
  }
}
