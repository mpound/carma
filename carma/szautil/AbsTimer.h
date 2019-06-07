#ifndef ABSTIMER_H
#define ABSTIMER_H

/**
 * @file AbsTimer.h
 * 
 * Tagged: Fri Nov 14 12:39:30 UTC 2003
 * 
 * @author Erik Leitch
 */
// C++ includes

#include <list>
#include <algorithm>  // Needed for find_if()
#include <functional> // Needed for predicates

// C includes

#include <unistd.h>
#include <signal.h>
#include <time.h>

#define DEFAULT_CLOCK CLOCK_REALTIME

namespace sza {
  namespace util {
    
#if !HAVE_RT

    // If we don't have realtime libs, this declaration will prevent
    // the compiler from complaining about it.

    #define timer_t unsigned int

    #define SIGRTMIN 31

    enum {
      CLOCK_REALTIME
    };

#endif

    /**
     * Class used to set up repetitive or one-shot timers on
     * integral boundries relative to the system clock. For example,
     * use this class if you want to fire events every 500 msec
     * starting at a specific absolute time.
     *
     * Author: Erik Leitch, extended from Rick Hobbs' Abstimer class
     * Version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:28 $
     */
    class AbsTimer {
      
    public:
      
      /**
       * A class for storing information about a known Timer
       */
      class TimerId {
      public:
	
	/**
	 * Pointer to the timer managed by this object
	 */
	timer_t* timer_;
	
	/**
	 * Signal associated with this timer
	 */
	int sigNo_;
	
	/**
	 * Constructor for TimerId
	 */
	TimerId(timer_t* timer, int sigNo) : timer_(timer), sigNo_(sigNo) {}
      };
      
      /**
       * A predicate for testing if a timer's id matches a
       * requested id
       */
      class TimerId_eq : public std::unary_function<TimerId, bool> {
	timer_t* timer_;
      public:
	explicit TimerId_eq(timer_t* timer) : timer_(timer) {}
	bool operator() (const TimerId& t) const {return t.timer_ == timer_;}
      };
      
      /**
       * A predicate for testing if a timer's signal matches a
       * requested signal
       */
      class TimerSig_eq : public std::unary_function<TimerId, bool> {
	int sigNo_;
      public:
	explicit TimerSig_eq(int sigNo) : sigNo_(sigNo) {}
	bool operator() (const TimerId& t) const {return t.sigNo_ == sigNo_;}
      };
      
      /**
       * A static list of timers.  This list is added to by
       * addTimer() each time a timer gets created for a unique signal
       */
      static std::list<TimerId> timerList_;
      
      /**
       * AbsTimer constructor
       *
       * @throws AntException
       */
      AbsTimer(int signo, void (*handler)(int));
      
      /**
       * Constructor with no handler.
       */
      AbsTimer(int signo);
      
      /**
       * AbsTimer destructor
       */
      ~AbsTimer();
      
      /**
       * Returns the resolution of the clock in nsec
       * 
       * @throws AntException
       */
      unsigned long getResolution();
      
      /**
       *  Set initial delay when timer is to start from the moment
       *  its start() method is called.  Remember, sec is in
       *  absolute time from start of epoch.
       */
      void setInitialDelay(unsigned long sec, unsigned long nsec);
      
      /**
       *  Set interval delay for timer. 
       */
      void setIntervalDelay(unsigned long sec, unsigned long nsec);
      
      /**
       *  Re-arm a periodic timer which we want to fire relative to
       *  absolute second boundaries.
       */
      void reArm();
      
      /**
       * Start the Timer
       */
      void start();
      
      /**
       * Stop the Timer
       *
       * @throws AntException
       */
      void stop();
      
      /**
       * Query if this timer is running.  Returns true after start()
       * has been called, false before start() has been called, and
       * false after a call to stop();
       */
      bool isRunning();
      
      /**
       * Set whether this clock should run on integral second
       * boundaries relative to the current time.
       */
      void setIntegral(bool integral);
      
      /**
       * A debugging function, to print out the signals of all know timers
       */
      void checkTimer();
      
    private:
      
      /**
       * A private method which is called under the hood by both
       * public constructors.
       */
      void privateConstructor(int signo, void (*handler)(int));
      
      /**
       * True if this timer is running
       */
      bool isRunning_;
      
      /**
       * True if this timer should start on an integral second boundary
       */
      bool integral_;
      
      /**
       * True if this timer is a periodic timer.
       */
      bool periodic_;
      
      /**
       * The signal associated with this timer.
       */
      int sigNo_;
      
      /**
       * The id of this timer
       */
      timer_t timer_; 
      
      /**
       * Container for managing the action to take on expiry of this
       * timer
       */
      struct sigevent evp_; 
      
      /**
       * Number of seconds before the initial expiry of the timer
       */
      unsigned long initSec_;  
      /**
       * Number of nano-seconds before the initial expiry of the timer
       */
      unsigned long initNanoSec_;
      
      /**
       * Number of seconds in the timer interval
       */
      unsigned long intervalSec_;
      
      /**
       * Number of nano-seconds in the timer interval
       */
      unsigned long intervalNanoSec_;
      
      /**
       * Add a timer to the static list of known timers
       *
       * @throws AntException
       */
      void addTimer(timer_t* timer, int signo);
      
      /**
       * Remove a timer from the static list of known timers.
       * Currently called by AbsTimer() destructor function
       */
      void remTimer(timer_t* timer);
      
      /**
       * Return true if a timer for this signal already exists
       */
      bool timerAlreadyExists(int sigNo);
      
      /**
       * Set up a timer to go off at a future time
       *
       * @throws AntException
       */
      void setFutureTime(unsigned long initSec, 
			 unsigned long initNanoSec, 
			 unsigned long intervalSec, 
			 unsigned long intervalNanoSec);
      
    }; // End class AbsTimer
    
  }; // End namespace util
}; // End namespace sza

#endif // ABSTIMER_H
