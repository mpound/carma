#ifndef SIGNALTASK_H
#define SIGNALTASK_H

/**
 * @file SignalTask.h
 * 
 * Tagged: Fri Nov 14 12:39:36 UTC 2003
 * 
 * @author Erik Leitch
 */
#include <string>
#include <vector>

#include "carma/szautil/AbsTimer.h"
#include "carma/szautil/GenericTask.h"
#include "carma/szautil/SignalTaskMsg.h"

#include <pthread.h>

namespace sza {
  namespace util {
    
    class Thread;

    /**
     * A generic base-class task for handling timers & signals.
     *
     * Context: 
     *
     * Under pthreads, there is no guarantee which thread a signal
     * will be delivered to, so traditional use of signal handlers
     * in multiple threads is unsafe.
     *
     * The safe way of handling signals under pthreads is to block
     * signals in all threads but one, which is dedicated to
     * servicing all signals.
     *
     * The default run() method of this class handles signals in
     * normal code, by blocking in a loop on sigwait() until any
     * catchable signal has arrived.  On receipt of a signal, if a
     * handler has been installed for that signal using either
     * installTimer() or installSignal() methods below, that
     * handler is called.  Note that all signals are blocked
     * between calls to sigwait(), so that we are guaranteed not
     * to be interrupted by another signal while servicing one
     * that has just arrived.
     *
     * Each pass of the loop atomically unblocks a set of signals
     * specified by a sigset_t* argument passed to sigwait().
     * This set is modified by calls to installTimer() and
     * installSignal(), below.
     *
     * SignalTask installs
     */
    class SignalTask :
      public sza::util::
      GenericTask<sza::util::SignalTaskMsg> {
      
      public:
      
      /**
       * Constructor for SignalTask.  The thread id is set in this
       * constructor to the id returned by pthread_self().  Calling
       * our internal send*Msg() methods will therefore cause an I/O
       * signal to be raised to the thread which instantiated this
       * object, causing the message queue to be checked.
       */
      SignalTask(bool spawnThread = false);
      
      /**
       * Making the destructor virtual ensures that the right
       * destructor will be called for classes which inherit from
       * SignalTask.
       */
      virtual ~SignalTask();
      
      /**
       * Send a message to install a timer.
       */
      void sendInstallTimerMsg(std::string name,
			       int sigNo, 
			       unsigned long initSec,
			       unsigned long initNsec,
			       unsigned long intervalSec,
			       unsigned long intervaNsec,
			       SIGNALTASK_HANDLER_FN(*handler));
      
      /**
       * Send a message to attach a handler to an existing timer.
       */
      void sendAddHandlerMsg(std::string name,
			     SIGNALTASK_HANDLER_FN(*handler), 
			     bool add);

      /**
       * Send a message to install a signal.
       */
      void sendInstallSignalMsg(int sigNo, SIGNALTASK_HANDLER_FN(*handler),
				void* arg=NULL);
      
      /**
       * Send a message to attach a handler to a signal.
       */
      void sendAddHandlerMsg(int sigNo,
			     SIGNALTASK_HANDLER_FN(*handler), 
			     bool add);

      /**
       * Send a message to enable/disable a timer.
       */
      void sendEnableTimerMsg(std::string name, bool enable);
      
      /**
       * Stop all timers.
       */
      void stopTimers();
      
      /**
       * Query what signal this object uses for I/O events.
       */
      int getIoSig();
      
      /**
       * Run, handling signals.
       */
      virtual void run();
      
      /**
       * Service our message queue.
       */
      void serviceMsgQ();
      
      pthread_t getThreadId();

      protected:
      
      class TimerInfo {
      public:
	/**
	 * Constructor.
	 */
	TimerInfo(std::string name, 
		  int sigNo, 
		  unsigned long initSec, 
		  unsigned long initNsec,
		  unsigned long intervalSec, 
		  unsigned long intervalNsec);
	
	/**
	 * Destructor
	 */
	~TimerInfo();
	
	/**
	 * Re-arm a periodic timer.
	 */
	void reArm();
	
      private:
	
	friend class SignalTask;
	
	int sigNo_;       // The signal to handle.
	unsigned long initSec_;      // The intial delay, in seconds,
				     // of this timer.
	unsigned long initNsec_;     // The initial delay, in
				     // nanoseconds, of this timer.
	unsigned long intervalSec_;  // The interval, in seconds, of
				     // this timer.
	unsigned long intervalNsec_; // The interval, in nanoseconds,
				     // of this timer.
	std::string name_;     // A name for this timer
	bool enabled_;    // True when this timer is enabled
	AbsTimer* timer_; // The timer.
      };
      
      /**
       * A class to manage signal handlers for all handled signals.
       */
      class SignalHandler {
      public:
	
	/**
	 * Constructors.
	 */
	SignalHandler(int sigNo, SIGNALTASK_HANDLER_FN(*handler), void* arg=NULL);
	
	/**
	 * Constructor with a pointer to the timer associated with
	 * this signal.
	 */
	SignalHandler(int sigNo,
		      SIGNALTASK_HANDLER_FN(*handler), 
		      TimerInfo* timer,
		      void* arg=NULL); 
	
	/**
	 * Re-arm any periodic timer associated with this handler.
	 */
	void reArm();
	
	/** 
	 * Destructor
	 */
	~SignalHandler();

	/**
	 * Add a handler to the set attached to this signal
	 */
	void addHandler(SIGNALTASK_HANDLER_FN(*handler), void* arg=NULL);
	  
	/**
	 * Remove a handler to the set attached to this signal
	 */
	void removeHandler(SIGNALTASK_HANDLER_FN(*handler));

      private:
	
	friend class SignalTask;
	
	/**
	 *  The signal to handle
	 */
	int sigNo_;      
	
	struct HandlerPair {

	  HandlerPair(SIGNALTASK_HANDLER_FN(*handler), void* args) {
	    handler_ = handler;
	    args_ = args;
	  }

	  SIGNALTASK_HANDLER_FN(*handler_);
	  void* args_;
	};

	/**
	 * A vector of handlers to call when this signal is received.
	 */
	std::vector<HandlerPair> handlers_;
	
	/**
	 * A TimerInfo object (if any) associated with this signal.
	 */
	TimerInfo* timer_;
      };
      
      protected:
      
      /**
       * The set of signals handled by this task.  We make this
       * protected so that inheritors of this class can redefine
       * run(), which may require access to handledSignals_.
       */
      sigset_t handledSignals_;
      
      private:
      
      /**
       * A private initialization method which is called by all
       * constructors.
       */
      void privateConstructor();
      
      /**
       * A signal we will watch to indicate that an I/O event has
       * arrived on our message queue.
       */
      int sigIo_;  
      
      /**
       * The id of the thread which instantiated this object.
       */
      pthread_t id_;
      
      /**
       * A static pointer, for use in static functions.
       */
      static SignalTask* signaltask_;
      
      /**
       * A vector of timer information.  
       *
       * NB: Do not naively change this to be of type <TimerInfo>,
       * or you will completely break this class!  The reason is
       * explained in notes for installTimer(string name...) in
       * SignalTask.cc.
       */
      std::vector<TimerInfo*> timers_;

      // Members for managing a spawned thread

      /**
       * If this object is spawned in a separate thread, we will use a
       * Thread container to manage it.
       */
      Thread* spawnedThread_;

      /**
       * True if this object is spawned in a separete thread.
       */
      bool spawned_; 
      
      /**
       * A startup function for the spawned thread.
       */
      static THREAD_START(startUp);

      /**
       * A vector of SignalHandler objects, which stores functions
       * to be called on receipt of a signal.
       */
      std::vector<SignalHandler*> signalHandlers_;
      
      /**
       * Respond to a message to install a timer.
       */
      void installTimer(SignalTaskMsg* msg);
      
      /**
       * Create a new timer, and install a handler for its signal.
       */
      void installTimer(std::string name, 
			int sigNo, 
			unsigned long initSec, 
			unsigned long initNsec,
			unsigned long intervalSec, 
			unsigned long intervalNsec,
			SIGNALTASK_HANDLER_FN(*handler));
      
      /**
       * Add a handler to the set of handlers for the named timer.
       */
      void addHandler(std::string name, SIGNALTASK_HANDLER_FN(*handler), void* args=NULL);

      /**
       * Remove a handler from the set of handlers for the named timer.
       */
      void removeHandler(std::string name, SIGNALTASK_HANDLER_FN(*handler));

      /**
       * Return a pointer to the SignalHandler for the requested
       * signal.
       */
      SignalHandler* getHandler(int sigNo);

      /**
       * Return a pointer to the TimerInfo object for the requested
       * signal
       */
      TimerInfo* getTimer(int sigNo);

      /**
       * Return a pointer to the TimerInfo object for the requested
       * name
       */
      TimerInfo* getTimer(std::string name);

      /**
       * Respond to a message to install a signal.
       */
      void installSignal(SignalTaskMsg* msg);
      
      /**
       * Install a handler for a given signal.
       */
      void installSignal(int sigNo, SIGNALTASK_HANDLER_FN(*handler), void* arg=NULL);
      
      /**
       * Add a handler to the set of handlers for the named timer.
       */
      void addHandler(int sigNo, SIGNALTASK_HANDLER_FN(*handler), void* args=NULL);

      /**
       * Remove a handler from the set of handlers for the named timer.
       */
      void removeHandler(int sigNo, SIGNALTASK_HANDLER_FN(*handler));

      /**
       * Respond to a message to enable/disable a timer.
       */
      void enableTimer(SignalTaskMsg* msg);
      
      /**
       * Start a timer by name.
       */
      void startTimer(std::string name);
      
      /**
       * Stop a timer by name.
       */
      void stopTimer(std::string name);
      
      /**
       * Start all timers.
       */
      void startTimers();
      
      /**
       * Process a message received on our message queue.
       */
      void processTaskMsg(bool* stop);
      
      /**
       * A callback function which will cause this task to check its
       * message queue.
       */
      static SIGNALTASK_HANDLER_FN(checkMsgQ);
      
      /**
       * Override GenericTask::fwdTaskMsg
       */
      void fwdTaskMsg(SignalTaskMsg* msg);
      
    }; // End class SignalTask
  }; // End namespace util
}; // End namespace sza

#endif // End #ifndef 
