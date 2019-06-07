

/**@file
 * Helper class for semaphore operations
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill </dl>
 * $Revision: 1.2 $
 * $Date: 2007/11/28 07:57:12 $
 * $Id: SemaphoreOperator.h,v 1.2 2007/11/28 07:57:12 colby Exp $
 */


#ifndef CARMA_UTIL_SEMAPHORE_OPS_H
#define CARMA_UTIL_SEMAPHORE_OPS_H

#include <sstream>

#include <fcntl.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <errno.h>

#define MAX_SEMS 250

namespace carma
{
  namespace util
  {
    class SemaphoreOperator
    {
      public:
	// TODO, have a static definition of semnums associated
	// with particular classes of Lock/Unlocks?
	SemaphoreOperator( int semnum );

	// TODO, Create framework to remove semaphore when not in use
	~SemaphoreOperator() {};

	void Lock( struct timespec *timeout = NULL );
	void Unlock( struct timespec *timeout = NULL );

      private:

	static const key_t _key;
	int _semid; 
	int _semnum;

	struct sembuf _semop;

#if defined(__GNU_LIBRARY__) && !defined(_SEM_SEMUN_UNDEFINED)
	/* union semun is defined by including <sys/sem.h> */
#else
	/* according to X/OPEN we have to define it ourselves */
	union semun
	{
	  int val;                  /* value for SETVAL */
	  struct semid_ds *buf;     /* buffer for IPC_STAT, IPC_SET */
	  unsigned short *array;    /* array for GETALL, SETALL */
	  /* Linux specific part: */
	  struct seminfo *__buf;    /* buffer for IPC_INFO */
	} _arg;
#endif // semun undefined
    };
  }
}



#endif // CARMA_UTIL_SEMAPHORE_OPS_H
