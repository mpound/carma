#ifndef SZA_UTIL_MUTEX_H
#define SZA_UTIL_MUTEX_H

/**
 * @file Mutex.h
 * 
 * Tagged: Sat Mar 27 16:28:13 PST 2004
 * 
 * @author Erik Leitch
 */
#include <pthread.h>

namespace sza {
  namespace util {
    
    class Mutex {
    public:
      
      /**
       * Constructor.
       */
      Mutex();
      
      /**
       * Destructor.
       */
      virtual ~Mutex();
      
      void lock();

      void unlock();

      /**
       * Return true if the mutex was successfully locked by this call.
       */
      bool tryLock();

      /**
       * Somewhat redundant with tryLock(), except that the mutex is
       * unlocked on exit.  Used for checking if someone else has
       * locked a mutex without actually wanting to lock it ourselves.
       */
      bool isLocked();

      inline pthread_mutex_t getPthreadVar() {
	return mutex_;
      }

      inline pthread_mutex_t* getPthreadVarPtr() {
	return &mutex_;
      }

      bool isItMe();

    private:

      pthread_mutex_t mutex_;
      bool mutexIsReady_;
      pthread_t who_;     // The identity of the thread who currently has the lock

    }; // End class Mutex
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_MUTEX_H
