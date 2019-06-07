#ifndef ANTENNAMONITORMSG_H
#define ANTENNAMONITORMSG_H

/**
 * @file AntennaMonitorMsg.h
 * 
 * Tagged: Thu Nov 13 16:53:29 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/GenericTaskMsg.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      
      /**
       * A class to encapsulate messages sent to the Monitor Task.
       */
      class AntennaMonitorMsg :
	public sza::util::GenericTaskMsg {
	
	public:
	
	/**
	 * Enumerate supported AntennaMonitor messages
	 */
	enum MsgType {
	  CONNECT,        // Connect to the archiver
	  FLAG_BOARD,
	  PACK_DATAFRAME, // Pack a data frame.
	  DISPATCH_DATAFRAME  // Send a data frame.
	}; 
	
	/**
	 * The type of this message
	 */
	MsgType type;
	
	/**
	 * A union of message bodies.
	 */
	union {
	  
	  /**------------------------------------------------------------
	   * Flag a board.
	   */
	  struct {
	    unsigned short board; // The register map index of the
	    // board to un/flag
	    bool flag;            // True to flag, false to unflag
	  } flagBoard;
	  
	} body;
	
	//------------------------------------------------------------
	// Methods for packing messages
	//------------------------------------------------------------
	
	inline void packFlagBoardMsg(unsigned short board, bool flag)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    type = FLAG_BOARD;
	    body.flagBoard.board = board;
	    body.flagBoard.flag = flag;
	  }
	
	inline void packPackDataFrameMsg()
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    type = PACK_DATAFRAME;
	  }

	inline void packDispatchDataFrameMsg()
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    type = DISPATCH_DATAFRAME;
	  }
	
	/**
	 * Pack a message to connect to the pmac.
	 */
	inline void packConnectMsg() {
	  genericMsgType_ = 
	    sza::util::GenericTaskMsg::TASK_SPECIFIC;
	  
	  type = CONNECT;
	}

      }; // End class AntennaMonitorMsg
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
