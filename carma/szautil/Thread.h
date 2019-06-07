#ifndef thread_h
#define thread_h

/**
 * @file Thread.h
 * 
 * Tagged: Fri Nov 14 12:39:37 UTC 2003
 * 
 * @author Erik Leitch
 */
#include <iostream>
#include <pthread.h>
#include <string>
#include <unistd.h>

// A thread start-up function

#define THREAD_START(fn) void* (fn)(void *arg)

// A thread shutdown function

#define THREAD_STOP(fn) void (fn)(void *arg)

// A thread cleanup function

#define THREAD_CLEAN(fn) void (fn)(void *arg)

// A method by which we can ping this thread

#define THREAD_PING(fn) void (fn)(void *arg)

// A pair of macros which we will use to safely lock/unlock a mutex.
// Also includes a fix to work-around the bug under kernel 2.4.20 that
// CORBA calls scramble the cancellation type.

#define INSTALL_MUTEX_CLEANUP(mutex, logStream) \
{\
  int oldtype;\
  if(pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, &oldtype) != 0) \
    logStream.appendSysError(true, "pthread_setcanceltype");\
  pthread_cleanup_push(&Thread::unlockMutex, (void *) (&mutex));\
  if(oldtype != PTHREAD_CANCEL_DEFERRED && oldtype != PTHREAD_CANCEL_ASYNCHRONOUS) ;\
    oldtype = PTHREAD_CANCEL_ASYNCHRONOUS
 
#define UNINSTALL_MUTEX_CLEANUP(logStream) \
  pthread_cleanup_pop(0);\
  if(pthread_setcanceltype(oldtype, NULL) != 0) \
    logStream.appendSysError(true, "pthread_setcanceltype");\
}

namespace sza {
  namespace util {
    
    /**
     * Define a class to encapsulate thread handling.
     */      
    class Thread {
      
    public:
      
      /**
       * Function by which a caller can start up this thread 
       *
       * ***NB: this method blocks until the startup function
       * (passed as void* arg) calls broadcastReady() below, to
       * allow synchronization.  You MUST make sure your thread
       * startup function calls broadcastReady() or the thread
       * calling run() will never unblock.
       *
       * @throws Exception
       */
      void start(void* arg);
      
      /**
       * Calls pthread_cancel() for this thread
       *
       * @throws Exception
       */
      void cancel();
      
      /**
       * Function by which the control thread can ping this thread
       *
       * @throws Exception
       */
      void ping(void* arg);
      
      /**
       * Let other threads know we are ready
       *
       * @throws Exception
       */
      void broadcastReady();
      
      /**
       * Let other threads know we are done
       *
       * @throws Exception
       */
      void broadcastDone();
      
      /**
       * Function by which we can safely alter the running state of
       * this thread
       *
       * @throws Exception
       */
      void setRunState(bool state);
      
      /**
       * Return true if the passed argument matches the name of this
       * thread.
       */
      bool matchName(std::string compname);
      
      /**
       * Print the name of this thread
       */
      std::string strName();
      
      /**
       * Constructor method.
       *
       * @param startFn A user-supplied startup function for this thread.
       *                Obviously, this cannot be NULL.
       *
       * @param cleanFn A user-supplied startup function for this thread.
       *                Can be NULL, in which case the default cleanup
       *                handler is a no-op.
       *
       * @param pingFn  A user-supplied method of pinging this thread.
       *                Can be NULL, in which case isPingable() returns 
       *                false and ping() is a no-op.
       *
       * @param name    A name to associate with this thread.
       *
       */
      Thread(THREAD_START(*startFn), THREAD_CLEAN(*cleanFn), 
	     THREAD_PING(*pingFn), std::string name);
      
      /**
       * Same as above, but allows the user to explicitly control the
       * order in which this thread is started and cancelled.
       */
      Thread(THREAD_START(*startFn), THREAD_CLEAN(*cleanFn), 
	     THREAD_PING(*pingFn), std::string name, 
	     unsigned startOrder, unsigned cancelOrder);

      /**
       * Destructor method
       */
      ~Thread();
      
      /**
       * Return true once this thread is running.
       */
      bool isRunning();
      
      /**
       * Return true if a ping function has been installed.
       */
      bool isPingable();
      
      /**
       * Raise a signal to this thread.
       */
      void raise(int sigNo);
      
      /**
       * A wrapper around pthread_mutex_unlock() suitable for
       * passing to pthread_cleanup_push()
       */
      static THREAD_CLEAN(unlockMutex);
      
      /**
       * A value to be returned if an error occurs on startup
       */
      bool wasError_;

      inline unsigned startOrder() {
	return startOrder_;
      }

      inline unsigned cancelOrder() {
	return cancelOrder_;
      }

      void waitUntilReady();

      inline std::string name() {
	return name_;
      };

    private:
      
      unsigned startOrder_;
      unsigned cancelOrder_;

      /**
       * Keep a pointer to the arguments to be passed to the
       * user-supplied thread startup function for this thread.
       */
      void* startupArg_;
      
      /**
       * A variable which will be set to true once this thread is up
       * and running.  
       */
      bool running_;
      
      /**
       * A guard mutex for the running variable
       */
      pthread_mutex_t runningGuard_; 
      
      /**
       * A handle to the thread, returned by pthread_create()
       */
      pthread_t id_; 
      
      /**
       * Define a structure which will associate a guard mutex with
       * a condition variable
       */
      struct CondVar {
	
	/**
	 * A guard mutex for the variable
	 */
	pthread_mutex_t guard;
	
	/**
	 * A variable which we will use to signal other threads
	 */
	pthread_cond_t cond;
      };
      
      /**
       * A variable we will use to indicate that the thread has
       * allocated its resources and is ready for communication
       */
      CondVar ready_;
      
      /**
       * A variable we will use to indicate that the thread has
       * deallocated its resources and is ready to exit
       */
      CondVar done_;
      
      /**
       * A name by which this thread will be known
       */
      std::string name_;
      
      /**
       * Thread initializer function
       */
      void privateConstructor(THREAD_START(*startFn), 
			      THREAD_CLEAN(*cleanFn),
			      THREAD_PING(*pingFn), 
			      std::string name, 
			      unsigned startOrder, unsigned cancelOrder);

      /**
       * A static startup function to be passed to pthread_create().
       */
      static THREAD_START(startThread);
      
      /**
       * A static cleanup function to be passed to pthread_cleanup_push().
       */
      static THREAD_CLEAN(cleanThread);
      
      /**
       * User-supplied startup function for this thread.  This
       * gets passed in as the function argument to
       * pthread_create()
       */
      THREAD_START(*userStartFn_);
      
      /**
       * User-supplied cleanup function for this thread.  This
       * gets passed in as the function argument to
       * pthread_create()
       */
      THREAD_CLEAN(*userCleanFn_);
      
      /**
       * A user-supplied method to send this thread a heartbeat
       * message.
       */
      THREAD_PING(*userPingFn_);
      
      /**
       * A private function to change the internal running state of
       * this thread
       */
      bool setRunStatePrivate(bool state);
      
    }; // End class Thread
    
  }; // End namespace util
}; // End namespace sza

#endif









