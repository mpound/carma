#ifndef GENERICMASTERTASKMSG_H
#define GENERICMASTERTASKMSG_H

/**
 * @file GenericMasterTaskMsg.h
 * 
 * Tagged: Fri Nov 14 12:39:34 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/GenericTaskMsg.h"
#include "carma/szautil/LogStream.h"

#define SIGNALTASK_HANDLER_FN(fn) void (fn)(int sigNo, void* args)
#define SIGNAL_NAME_LEN 10

namespace sza {
  namespace util {
    
    /**
     * A class to encapsulate message types for a generic task.
     *
     * Classes which extend from this class should simply add
     * whatever members are required to process additional
     * task-specific messages, for instance a union of task-specific
     * messages.
     *
     * NB: There is no explicit constructor for this class, since
     * the compiler won't allow classes with constructors to be
     * included as members of unions.  This means that we cannot
     * construct unions using objects which inherit from
     * GenericMasterTaskMsg unless they also don't have constructors.
     */
    class GenericMasterTaskMsg : public GenericTaskMsg {
      
    public:
      
      /**
       * Enumerate supported generic message types.
       */
      enum GenericMasterMsgType {
	ADD_HANDLER = GenericTaskMsg::LAST+1,    // Add a signal handler
	ENABLE_TIMER,   // Enable/Disable a timer
	INSTALL_TIMER,  // Install a timer
	INSTALL_SIGNAL, // Install a signal
	TASK_SPECIFIC   // A task-specific message
      };
      
      /**
       * A message body
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
	  int sigNo;
	  SIGNALTASK_HANDLER_FN(*handler);
	  void* args;
	} installSignal;
	
	struct {
	  char name[SIGNAL_NAME_LEN+1];
	  bool enable;
	} enableTimer;
	
	struct {
	  char name[SIGNAL_NAME_LEN+1];
	  SIGNALTASK_HANDLER_FN(*handler);
	  void* args;
	  bool add;
	} addHandler;
	
      } genericMasterBody;
      
      /**
       * Pack a message to install a timer.  Allows separately
       * setting the initial delay and the interval
       */
      inline void packInstallTimerMsg(std::string name, 
				      int sigNo, 
				      unsigned long initSec, 
				      unsigned long initNsec, 
				      unsigned long intervalSec, 
				      unsigned long intervalNsec, 
				      SIGNALTASK_HANDLER_FN(*handler)) 
	{
	  genericMsgType_ = (GenericTaskMsg::GenericMsgType)
	    sza::util::GenericMasterTaskMsg::INSTALL_TIMER;
	  
	  if(name.size() > SIGNAL_NAME_LEN)
	    ThrowError("name string is too long");
	  
	  strncpy(genericMasterBody.installTimer.name, name.c_str(), 
		  SIGNAL_NAME_LEN);
	  genericMasterBody.installTimer.sigNo        = sigNo;
	  genericMasterBody.installTimer.initSec      = initSec;
	  genericMasterBody.installTimer.initNsec     = initNsec;
	  genericMasterBody.installTimer.intervalSec  = intervalSec;
	  genericMasterBody.installTimer.intervalNsec = intervalNsec;
	  genericMasterBody.installTimer.handler      = handler;
	}
      
      /**
       * Pack a message to install a timer.  Sets the initial delay
       * and the interval to the same.
       */
      inline void packInstallTimerMsg(std::string name, 
				      int sigNo, 
				      unsigned long intervalSec, 
				      unsigned long intervalNsec, 
				      SIGNALTASK_HANDLER_FN(*handler)) 
	{
	  genericMsgType_ = (GenericTaskMsg::GenericMsgType)
	    sza::util::GenericMasterTaskMsg::INSTALL_TIMER;
	  
	  if(name.size() > SIGNAL_NAME_LEN)
	    ThrowError("Name string is too long");
	  
	  strncpy(genericMasterBody.installTimer.name, name.c_str(), 
		  SIGNAL_NAME_LEN);
	  genericMasterBody.installTimer.sigNo        = sigNo;
	  genericMasterBody.installTimer.initSec      = intervalSec;
	  genericMasterBody.installTimer.initNsec     = intervalNsec;
	  genericMasterBody.installTimer.intervalSec  = intervalSec;
	  genericMasterBody.installTimer.intervalNsec = intervalNsec;
	  genericMasterBody.installTimer.handler      = handler;
	}
      
      /**
       * Pack a message to install a signal.
       */
      inline void packInstallSignalMsg(int sigNo, 
				       SIGNALTASK_HANDLER_FN(*handler))
	{
	  genericMsgType_ = (GenericTaskMsg::GenericMsgType)
	    sza::util::GenericMasterTaskMsg::INSTALL_SIGNAL;
	  
	  genericMasterBody.installSignal.sigNo   = sigNo;
	  genericMasterBody.installSignal.handler = handler;
	}
      
      /**
       * Pack a message to enable/disable a timer.
       */
      inline void packEnableTimerMsg(std::string name, 
				     bool enable)
	{
	  genericMsgType_ = (GenericTaskMsg::GenericMsgType)
	    sza::util::GenericMasterTaskMsg::ENABLE_TIMER;
	  
	  if(name.size() > SIGNAL_NAME_LEN)
	    ThrowError("name std::string is too long");
	  
	  strncpy(genericMasterBody.enableTimer.name, name.c_str(), 
		  SIGNAL_NAME_LEN);
	  
	  genericMasterBody.enableTimer.enable = enable;
	}
      
      /**
       * Pack a message to enable/disable a timer.
       */
      inline void packAddHandlerMsg(std::string name, 
				    SIGNALTASK_HANDLER_FN(*handler),
				    bool add)
	{
	  genericMsgType_ = (GenericTaskMsg::GenericMsgType)
	    sza::util::GenericMasterTaskMsg::ADD_HANDLER;
	  
	  if(name.size() > SIGNAL_NAME_LEN)
	    ThrowError("name std::string is too long");
	  
	  strncpy(genericMasterBody.addHandler.name, name.c_str(), 
		  SIGNAL_NAME_LEN);
	  
	  genericMasterBody.addHandler.handler = handler;
	  genericMasterBody.addHandler.add     = add;
	}
      
    }; // End class GenericMasterTaskMsg
    
  }; // End namespace util
}; // End namespace sza

#endif // End #ifndef 
