#ifndef SZA_ANTENNA_ANTENNACONTROLMSG_H
#define SZA_ANTENNA_ANTENNACONTROLMSG_H

/**
 * @file AntennaControlMsg.h
 * 
 * Started: Thu Feb 26 14:19:05 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Exception.h"
#include "carma/szautil/GenericTaskMsg.h"
#include "carma/szautil/NetMsg.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      class AntennaControlMsg : 
	public sza::util::GenericTaskMsg {
	
	public:
	
	/**
	 * Enumerate supported message types.
	 */
	enum MsgType {
	  CONNECT, // Attempt to connect to the control host.
	  NETMSG,  // A network message intended for the controller.
	  WRITE_CARMA_SEQ_NO,   // Message to write a carma sequence number
	  WRITE_CARMA_MONITORS, // Message to write carma monitor points
	};
	
	/**
	 * A type for this message
	 */
	MsgType type;
	
	union {
	  sza::util::NetMsg networkMsg;

	  struct {
	    unsigned long seq_;
	    CarmaSeqNoType type_;
	    bool success_;
	  } carmaSeqNo;

	} body;

	/**
	 * Method to get a pointer to the NetMsg
	 */
	inline sza::util::NetMsg* getNetMsg()
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    type = NETMSG;
	    return &body.networkMsg;
	  }

	//------------------------------------------------------------
	// Methods for packing message to the communications task.
	//
	// We explicitly initialize genericMsgType_ in each method,
	// since we cannot do this in a constructor, since objects
	// with explicit constructors apparently can't be union
	// members.
	//------------------------------------------------------------
	
	/**
	 * Pack a message to connect to the pmac.
	 */
	inline void packConnectMsg() {
	  genericMsgType_ = 
	    sza::util::GenericTaskMsg::TASK_SPECIFIC;
	  
	  type = CONNECT;
	}
	
	inline void packCarmaSeqNoMsg(unsigned long seq, 
				      CarmaSeqNoType carmaType, bool success) {
	  genericMsgType_ = 
	    sza::util::GenericTaskMsg::TASK_SPECIFIC;
	  
	  body.carmaSeqNo.seq_     = seq;
	  body.carmaSeqNo.type_    = carmaType;
	  body.carmaSeqNo.success_ = success;
	  
	  type = WRITE_CARMA_SEQ_NO;
	}

	inline void packWriteCarmaMonitorsMsg() {
	  genericMsgType_ = 
	    sza::util::GenericTaskMsg::TASK_SPECIFIC;
	  
	  type = WRITE_CARMA_MONITORS;
	}

	private:
      }; // End class AntennaControlMsg
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 


