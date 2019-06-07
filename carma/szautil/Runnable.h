// $Id: Runnable.h,v 1.2 2011/02/25 01:23:45 eml Exp $

#ifndef SZA_UTIL_RUNNABLE_H
#define SZA_UTIL_RUNNABLE_H

/**
 * @file Runnable.h
 * 
 * Tagged: Tue Dec 21 19:19:59 CST 2004
 * 
 * @version: $Revision: 1.2 $, $Date: 2011/02/25 01:23:45 $
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Thread.h"

#define RUN_FN(fn) void* (fn)(void* arg)

namespace sza {
  namespace util {

    class Runnable {
    public:

      /**
       * Constructor.
       */
      Runnable(bool spawnThread, RUN_FN(*runFn));

      /**
       * Destructor.
       */
      virtual ~Runnable();

      /**
       * A startup function for the spawned thread.
       */
      static THREAD_START(startUp);

      static void blockForever();

    protected:

      /**
       * If this object is spawned in its own thread, we will use a
       * Thread container to manage it.
       */
      Thread* spawnedThread_;

      /**
       * True if this object is spawned in a separate thread.
       */
      bool spawned_; 

      // A pointer to a function which will be called on startup

      RUN_FN(*runFn_);
      
      void spawn(void* arg);

    public:

      virtual void spawn();

    }; // End class Runnable

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_RUNNABLE_H
