// $Id: PeriodicTimer.h,v 1.2 2011/11/14 22:42:03 eml Exp $

#ifndef SZA_UTIL_PERIODICTIMER_H
#define SZA_UTIL_PERIODICTIMER_H

/**
 * @file PeriodicTimer.h
 * 
 * Tagged: Tue Feb  2 14:45:04 NZDT 2010
 * 
 * @version: $Revision: 1.2 $, $Date: 2011/11/14 22:42:03 $
 * 
 * @author username: Command not found.
 */
#include <iostream>

#include "carma/szautil/GenericTask.h"
#include "carma/szautil/GenericTaskMsg.h"
#include "carma/szautil/SpawnableTask.h"
#include "carma/szautil/TimeOut.h"

#define PERIODIC_TIMER_HANDLER(fn) void (fn)(void* args)

namespace sza {
  namespace util {

    //------------------------------------------------------------
    // A utility class for sending messages to the ModemPager task
    //------------------------------------------------------------

    class PeriodicTimerMsg : public GenericTaskMsg {
    public:

      enum MsgType {
	TIMER,
	ADD_HANDLER,
	REM_HANDLER
      };
      
      union {

	struct {
	  bool enable;
	  unsigned intervalInSeconds;
	} timer;

	struct {
	  PERIODIC_TIMER_HANDLER(*fn);
	  void* args;
	} addHandler;

	struct {
	  PERIODIC_TIMER_HANDLER(*fn);
	} remHandler;

      } body;
      
      // A type for this message

      MsgType type;
    };

    //------------------------------------------------------------
    // Main class definition
    //------------------------------------------------------------

    class PeriodicTimer : 
      public SpawnableTask<PeriodicTimerMsg> {
    public:

	//------------------------------------------------------------
	// A utility class used to store handlers
	//------------------------------------------------------------
	
	class Handler {
	public:
	  PERIODIC_TIMER_HANDLER(*fn_);
	  void* args_;
	};

      /**
       * Constructor.
       */
      PeriodicTimer();

      /**
       * Destructor.
       */
      virtual ~PeriodicTimer();

      // Add a callback function to be called whenever the timer expires

      void addHandler(PERIODIC_TIMER_HANDLER(*handler), void* args=0);

      // Remove a callback function from the list to be called

      void removeHandler(PERIODIC_TIMER_HANDLER(*handler));

      // Enable/Disable the timer

      void enableTimer(bool enable, unsigned intervalInSeconds=0);

    private:

      TimeOut timeOut_;

      // A list of handlers to be called when the ephemeris is updated

      std::vector<Handler> handlers_; 

      //-----------------------------------------------------------------------
      // Methods called in response to messages received on our message queue
      //-----------------------------------------------------------------------

      void executeEnableTimer(bool enable, unsigned intervalInSeconds);
      void executeAddHandler(PERIODIC_TIMER_HANDLER(*handler), void* args=0);
      void executeRemoveHandler(PERIODIC_TIMER_HANDLER(*handler));

      //-----------------------------------------------------------------------
      // Run methods used by this class
      //-----------------------------------------------------------------------

      void serviceMsgQ();
      void processMsg(PeriodicTimerMsg* msg);

      // React to a timeout in select

      void registerTimeOut();

      void callHandlers();

    }; // End class PeriodicTimer

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_PERIODICTIMER_H
