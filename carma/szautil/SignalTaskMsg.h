#ifndef SIGNALTASKMSG_H
#define SIGNALTASKMSG_H

/**
 * @file SignalTaskMsg.h
 * 
 * Tagged: Fri Nov 14 12:39:36 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Exception.h"
#include "carma/szautil/GenericTaskMsg.h"
#include <cstring>

namespace sza {
  namespace util {
    
    class SignalTaskMsg :
      public sza::util::GenericTaskMsg {
      
      public:
      
      enum MsgType {
	ADD_SIGNAL_HANDLER,
	ADD_TIMER_HANDLER,
	SIG_ENABLE_TIMER,
	SIG_INSTALL_TIMER,
	SIG_INSTALL_SIGNAL,
      };
      
      MsgType type;
      
      /**
       * A union of message bodies.
       */
      union {
	
	struct {
	  char name[SIGNAL_NAME_LEN+1];
	  int sigNo;
	  unsigned long initSec;
	  unsigned long initNsec;
	  unsigned long intervalSec;
	  unsigned long intervalNsec;
	  SIGNALTASK_HANDLER_FN(*handler);
	} installTimer;
	
	struct {
	  char name[SIGNAL_NAME_LEN+1];
	  bool enable;
	} enableTimer;
	
	struct {
	  int sigNo;
	  SIGNALTASK_HANDLER_FN(*handler);
	  void* arg;
	} installSignal;

	struct {
	  int sigNo;
	  SIGNALTASK_HANDLER_FN(*handler);
	  bool add;
	} addSignalHandler;
	
	struct {
	  char name[SIGNAL_NAME_LEN+1];
	  SIGNALTASK_HANDLER_FN(*handler);
	  bool add;
	} addTimerHandler;
	
      } body;
      
      //------------------------------------------------------------
      // Methods for packing signal messages 
      //------------------------------------------------------------
      
      /**
       * Pack a message to install a timer.
       */
      inline void packInstallTimerMsg(std::string name, 
				      int sigNo,
				      unsigned long initSec,
				      unsigned long initNsec,
				      unsigned long intervalSec,
				      unsigned long intervalNsec,
				      SIGNALTASK_HANDLER_FN(*handler)) 
	{
	  
	  if(name.size() > SIGNAL_NAME_LEN)
	    ThrowError("Name string is too long");
	  
	  genericMsgType_ = 
	    sza::util::GenericTaskMsg::TASK_SPECIFIC;
	  
	  type = SIG_INSTALL_TIMER;
	  
	  strncpy(body.installTimer.name, name.c_str(), SIGNAL_NAME_LEN);
	  body.installTimer.sigNo        = sigNo;
	  body.installTimer.initSec      = initSec;
	  body.installTimer.initNsec     = initNsec;
	  body.installTimer.intervalSec  = intervalSec;
	  body.installTimer.intervalNsec = intervalNsec;
	  body.installTimer.handler      = handler;
	}
      
      /**
       * Pack a message to install a signal.
       */
      inline void packInstallSignalMsg(int sigNo, 
				       SIGNALTASK_HANDLER_FN(*handler),
				       void* arg)
	{
	  genericMsgType_ = 
	    sza::util::GenericTaskMsg::TASK_SPECIFIC;
	  
	  type = SIG_INSTALL_SIGNAL;
	  
	  body.installSignal.sigNo   = sigNo;
	  body.installSignal.handler = handler;
	  body.installSignal.arg     = arg;
	}
      
      /**
       * Pack a message to enable/disable a timer.
       */
      inline void packEnableTimerMsg(std::string name, 
				     bool enable)
	{
	  genericMsgType_ = 
	    sza::util::GenericTaskMsg::TASK_SPECIFIC;
	  
	  type = SIG_ENABLE_TIMER;
	  
	  if(name.size() > SIGNAL_NAME_LEN)
	    ThrowError("Name string is too long");
	  
	  strncpy(body.enableTimer.name, name.c_str(), 
		  SIGNAL_NAME_LEN);
	  
	  body.enableTimer.enable = enable;
	}

      /**
       * Pack a message to add a handler to a signal
       */
      inline void packAddHandlerMsg(std::string name, 
				    SIGNALTASK_HANDLER_FN(*handler),
				    bool add)
	{
	  genericMsgType_ = 
	    sza::util::GenericTaskMsg::TASK_SPECIFIC;
	  
	  type = ADD_TIMER_HANDLER;
	  
	  if(name.size() > SIGNAL_NAME_LEN)
	    ThrowError("Name string is too long");
	  
	  strncpy(body.addTimerHandler.name, name.c_str(), 
		  SIGNAL_NAME_LEN);

	  body.addTimerHandler.handler = handler;
	  body.addTimerHandler.add     = add;
	}

      /**
       * Pack a message to add a handler to a signal
       */
      inline void packAddHandlerMsg(int sigNo, 
				    SIGNALTASK_HANDLER_FN(*handler),
				    bool add)
	{
	  genericMsgType_ = 
	    sza::util::GenericTaskMsg::TASK_SPECIFIC;
	  
	  type = ADD_SIGNAL_HANDLER;
	  
	  body.addSignalHandler.sigNo   = sigNo;
	  body.addSignalHandler.handler = handler;
	  body.addSignalHandler.add     = add;
	}
      
      
    }; // End class SignalTaskMsg
    
  }; // End namespace util
}; // End namespace sza

#endif // End #ifndef 
