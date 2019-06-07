#ifndef PMACCOMMSMSG_H
#define PMACCOMMSMSG_H

/**
 * @file PmacCommsMsg.h
 * 
 * Tagged: Thu Nov 13 16:53:44 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Exception.h"
#include "carma/szautil/GenericTaskMsg.h"

#include "carma/antenna/sza/antenna/control/PmacCommand.h"

namespace sza {
  namespace util {
    
    
    class PmacCommsMsg :
      public sza::util::GenericTaskMsg {
      
      public:
      
      /**
       * Enumerate supported message types.
       */
      enum MsgType {
	CONNECT,
	DISCONNECT,
	SETMEM
      };
      
      /**
       * A type for this message
       */
      MsgType type;
      
      union {
	
	struct {
	  unsigned short offset;
	  unsigned short length;
	  unsigned char data[PMAC_DATA_MAX_LEN];
	} setmem;
	
      } body;
      
      /**
       * Pack a message to connect to the pmac.
       */
      inline void packConnectMsg()
	{
	  genericMsgType_ = 
	    sza::util::GenericTaskMsg::TASK_SPECIFIC;
	  
	  type = CONNECT;
	}
      
      /**
       * Pack a message to disconnect from the pmac.
       */
      inline void packDisconnectMsg()
	{
	  genericMsgType_ = 
	    sza::util::GenericTaskMsg::TASK_SPECIFIC;
	  
	  type = DISCONNECT;
	}
      
      /**
       * Pack a setmem command.
       */
      inline void packSetMemMsg(unsigned short offset,
				unsigned short length,
				unsigned char* data)
	{
	  if(length > PMAC_DATA_MAX_LEN)
	    throw sza::util::
	      sza::util::Error("PmacCommsMsg::packSetMemMsg: "
			       "data array too large.\n");
	  
	  genericMsgType_ = 
	    sza::util::GenericTaskMsg::TASK_SPECIFIC;
	  
	  type = SETMEM;
	  
	  body.setmem.offset = offset;
	  body.setmem.length = length;
	  
	  for(unsigned i=0; i < length; i++)
	    body.setmem.data[i] = data[i];
	}
      
    }; // End class PmacCommsMsg
    
  }; // End namespace util
}; // End namespace sza

#endif // End #ifndef 
