

/**@file
 * Class definition for TelemetryClient on the BIMA antennas.
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill </dl>
 * $Revision: 1.5 $
 * $Date: 2011/08/24 15:52:47 $
 * $Id: SemaphoreOperator.h,v 1.5 2011/08/24 15:52:47 abeard Exp $
 */


#ifndef CARMA_BIMA_SEMAPHORE_OPS_H
#define CARMA_BIMA_SEMAPHORE_OPS_H

#include <map>
#include <sstream>

#include <fcntl.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <errno.h>

#define MAX_SEMS 250

namespace carma
{
  namespace antenna
  {
    namespace bima
    {
      class SemaphoreOperator
      {
       public:
        SemaphoreOperator( bool server = false );
        ~SemaphoreOperator();

        void semOpIdToSemIdAndNum( int semOpId,
                                   int &semid,
                                   unsigned short &semnum );

        void update( const std::string & name );
        void updateStale();
        void waitOn( const std::string & name );
        void checkAndUpdate( const std::string & name );
        void addNameAndId( const std::string & name, unsigned short id );
        unsigned short getSemNum( const std::string & name );
	      
       private:
        std::map<std::string, unsigned short> _nameToSemOpId;

        std::map<std::string, int> _staleList;
        int _staleCheck;

        // Unique sem keys follow
        static const key_t _key[4];
        // 1000 semnums default (4 x 250 semnums on 2.4.25 )
        int _semid[4]; 

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
}



#endif // CARMA_BIMA_SEMAPHORE_OPS_H
