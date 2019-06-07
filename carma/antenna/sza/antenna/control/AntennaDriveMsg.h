#ifndef ANTENNADRIVEMSG_H
#define ANTENNADRIVEMSG_H

/**
 * @file AntennaDriveMsg.h
 * 
 * Tagged: Thu Nov 13 16:53:28 UTC 2003
 * 
 * @author Erik Leitch
 */
#include <string>

#include "carma/szautil/Exception.h"
#include "carma/szautil/GenericTaskMsg.h"

#include "carma/antenna/sza/antenna/control/TrackerMsg.h"

#include "carma/szaarrayutils/szaregs.h"

namespace sza {
  namespace antenna {
    namespace control {      
      
      /**
       * A container for messages sent to the Drive Task.
       */
      class AntennaDriveMsg :
	public sza::util::GenericTaskMsg {
	
	public:
	
	/**
	 * Enumerate supported messages for this task
	 */
	enum MsgType {
	  FLAG_BOARD,        // A message to flag a board.
	  PMAC_CONNECTED,    // A message from the Tracker task that
	  // the pmac is dis/connected.
	  TRACKER_MSG // A message for the tracker thread.
	};
	
	/**
	 * The type of this message
	 */
	MsgType type;
	
	/**
	 * A union of supported messages
	 */
	union {
	  
	  /**
	   * Flag a board.
	   */
	  struct {
	    unsigned short board; // The register map index of the
	    // board to un/flag
	    bool flag;            // True to flag, false to unflag
	  } flagBoard;
	  
	  /**
	   * A message that the pmac is dis/connected
	   */
	  struct {
	    bool connected;
	  } pmacConnected;
	  
	  /**
	   * A message for the Tracker thread.
	   */
	  TrackerMsg trackerMsg;
	  
	} body;
	
	//------------------------------------------------------------
	// Methods for accessing messages for tasks managed by this
	// class.
	//------------------------------------------------------------
	
	inline TrackerMsg* getTrackerMsg()
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    type = TRACKER_MSG;

	    return &body.trackerMsg;
	  }
	
	//------------------------------------------------------------
	// Methods for packing messsages to the drive task.
	//------------------------------------------------------------
	
	inline void packFlagBoardMsg(unsigned short board, bool flag) {
	  genericMsgType_ = 
	    sza::util::GenericTaskMsg::TASK_SPECIFIC;
	  
	  type = FLAG_BOARD;
	  body.flagBoard.board = board;
	  body.flagBoard.flag = flag;
	}
	
	inline void packPmacConnectedMsg(bool connected)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    type = PMAC_CONNECTED;
	    
	    body.pmacConnected.connected = connected;
	  }
	
      }; // End AntennaDriveMsg class
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
