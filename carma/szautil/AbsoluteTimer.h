// $Id: AbsoluteTimer.h,v 1.2 2011/11/14 22:42:02 eml Exp $

#ifndef SZA_UTIL_ABSOLUTETIMER_H
#define SZA_UTIL_ABSOLUTETIMER_H

/**
 * @file AbsoluteTimer.h
 * 
 * Tagged: Tue Feb  2 14:45:04 NZDT 2010
 * 
 * @version: $Revision: 1.2 $, $Date: 2011/11/14 22:42:02 $
 * 
 * @author username: Command not found.
 */
#include <iostream>

#include "carma/szautil/GenericTask.h"
#include "carma/szautil/GenericTaskMsg.h"
#include "carma/szautil/SpawnableTask.h"
#include "carma/szautil/TimeOut.h"
#include "carma/szautil/Time.h"

#define ABSOLUTE_TIMER_HANDLER(fn) void (fn)(void* args)

namespace sza {
  namespace util {

    //------------------------------------------------------------
    // A utility class for sending messages to the ModemPager task
    //------------------------------------------------------------

    class AbsoluteTimerMsg : public GenericTaskMsg {
    public:

      enum MsgType {
	TIMER,
	ADD_HANDLER,
	REM_HANDLER
      };
      
      union {

	struct {
	  bool enable;

	  unsigned delayInSeconds;
	  unsigned delayInNanoSeconds;

	  unsigned offsetInNanoSeconds;

	  unsigned intervalInSeconds;
	  unsigned intervalInNanoSeconds;

	} timer;

	struct {
	  ABSOLUTE_TIMER_HANDLER(*fn);
	  void* args;
	} addHandler;

	struct {
	  ABSOLUTE_TIMER_HANDLER(*fn);
	} remHandler;

      } body;
      
      // A type for this message

      MsgType type;
    };

    //------------------------------------------------------------
    // Main class definition
    //------------------------------------------------------------

    class AbsoluteTimer : 
      public SpawnableTask<AbsoluteTimerMsg> {
    public:

	//------------------------------------------------------------
	// A utility class used to store handlers
	//------------------------------------------------------------
	
	class Handler {
	public:
	  ABSOLUTE_TIMER_HANDLER(*fn_);
	  void* args_;
	};

      /**
       * Constructor.
       */
      AbsoluteTimer();

      /**
       * Destructor.
       */
      virtual ~AbsoluteTimer();

      // Add a callback function to be called whenever the timer expires

      void addHandler(ABSOLUTE_TIMER_HANDLER(*handler), void* args=0);

      // Remove a callback function from the list to be called

      void removeHandler(ABSOLUTE_TIMER_HANDLER(*handler));

      // Enable/Disable the timer

      void enableTimer(bool enable, 
		       unsigned delayInSeconds=0,    unsigned delayInNanoSeconds=0,
		       unsigned offsetInNanoSeconds=0,
		       unsigned intervalInSeconds=0, unsigned intervalInNanoSeconds=0);
    private:

      Time offset_;
      Time initialDelay_;
      Time interval_;

      bool firstTimeoutSinceReconfiguration_;
      double firstTimeToFireInSeconds_;

      TimeOut timeOut_;

      // A list of handlers to be called when the ephemeris is updated

      std::vector<Handler> handlers_; 

      //-----------------------------------------------------------------------
      // Methods called in response to messages received on our message queue
      //-----------------------------------------------------------------------

      void executeEnableTimer(bool enable, 
			      unsigned delayInSeconds,    unsigned delayInNanoSeconds,
			      unsigned offsetInNanoSeconds,
			      unsigned intervalInSeconds, unsigned intervalInNanoSeconds);
      void executeAddHandler(ABSOLUTE_TIMER_HANDLER(*handler), void* args=0);
      void executeRemoveHandler(ABSOLUTE_TIMER_HANDLER(*handler));

      //-----------------------------------------------------------------------
      // Run methods used by this class
      //-----------------------------------------------------------------------

      void serviceMsgQ();
      void processMsg(AbsoluteTimerMsg* msg);

      // React to a timeout in select

      void registerTimeOut();

      void callHandlers();

      void computeNextTimeout();

    }; // End class AbsoluteTimer

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_ABSOLUTETIMER_H
