#ifndef SZA_UTIL_CONDVAR_H
#define SZA_UTIL_CONDVAR_H

/**
 * @file CondVar.h
 * 
 * Tagged: Thu Oct 18 13:20:31 PDT 2007
 * 
 * @author SZA data acquisition
 */
#include "carma/szautil/Mutex.h"

#include <pthread.h>

namespace sza {
  namespace util {
    
    class CondVar {
    public:
      
      /**
       * Constructor.
       */
      CondVar();
      
      /**
       * Destructor.
       */
      virtual ~CondVar();
      
      void lock();
      void waitNoLock();

      void wait();
      void broadcast();

    private:

      Mutex mutex_;
      pthread_cond_t cond_;
      bool condVarIsReady_;

    }; // End class CondVar
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_CONDVAR_H
