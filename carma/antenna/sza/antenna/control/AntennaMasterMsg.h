#ifndef ANTENNAMASTERMSG_H
#define ANTENNAMASTERMSG_H

/**
 * @file AntennaMasterMsg.h
 * 
 * Tagged: Thu Nov 13 16:53:29 UTC 2003
 * 
 * @author Erik Leitch
 */
#include <string>

#include "carma/szautil/SignalTask.h"
#include "carma/szautil/GenericMasterTaskMsg.h"

#include "carma/antenna/sza/antenna/control/AntennaControlMsg.h"
#include "carma/antenna/sza/antenna/control/AntennaDriveMsg.h"
#include "carma/antenna/sza/antenna/control/AntennaMonitorMsg.h"
#include "carma/antenna/sza/antenna/control/AntennaRxMsg.h"
#include "carma/antenna/sza/antenna/control/UmacControlMsg.h"
#include "carma/antenna/sza/antenna/control/AntennaTask.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      /**
       * A class to manage messages used to communicate with an
       * AntennaMaster thread.
       */
      class AntennaMasterMsg : 
	public sza::util::GenericMasterTaskMsg {
	
	public:
	
	/**
	 * Enumerate supported AntennaMaster messages
	 */	
	enum MsgType {
	  ADOPT_BOARD,    // Record a board adoption request from
			  // another task
	  CONTROL_MSG,    // A message for the communications task
	  CONTROL_CONNECTED, // We are dis/connected from/to the control host
	  DRIVE_MSG,      // A message for the drive task
	  FLAG_BOARD, 
	  SCANNER_CONNECTED, // We are dis/connected from/to the archiver
	  MONITOR_MSG,    // A message for the monitor task
	  PMAC_CONNECTED, // The pmac is dis/connected
	  RX_MSG,         // A message for the rx task
	  STRIP_CONTROL_MSG,// A message for the strip control task
	  SEND_HEARTBEAT, // Initiate a heartbeat.
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
	   * Specify which task is responsible for flagging a board.
	   */
	  struct {
	    unsigned short board;
	    AntennaTask::Id taskId;
	  } adoptBoard;
	  
	  /**
	   * Message to flag a board.
	   */
	  struct {
	    unsigned short board;
	    bool flag;
	  } flagBoard;
	  
	  /**
	   * A message that the control task is dis/connected from the
	   * control host
	   */
	  struct {
	    bool connected;
	  } controlConnected;
	  
	  /**
	   * A message that the scanner is dis/connected from the
	   * archiver host
	   */
	  struct {
	    bool connected;
	  } scannerConnected;
	  
	  /**
	   * A message that the pmac is dis/connected
	   */
	  struct {
	    bool connected;
	  } pmacConnected;
	  
	  //------------------------------------------------------------
	  // Message bodies for threads managed by the AntennaMaster
	  //------------------------------------------------------------
	  
	  /**
	   * A message for the AntennaControl task
	   */
	  AntennaControlMsg    controlMsg;   
	  
	  /**
	   * A message for the AntennaDrive task
	   */
	  AntennaDriveMsg    driveMsg;   
	  
	  /**
	   * A message for the AntennaMonitor task
	   */
	  AntennaMonitorMsg  monitorMsg; 
	  
	  /**
	   * A message for the AntennaRx task
	   */
	  AntennaRxMsg       rxMsg;      
	  
	  /**
	   * A message for the UmacControl task
	   */
	  UmacControlMsg    umacControlMsg;      

	} body;
	
	//------------------------------------------------------------
	// Methods for accessing task messages managed by this class.
	//------------------------------------------------------------
	
	inline AntennaControlMsg* getControlMsg()
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    type = CONTROL_MSG;
	    return &body.controlMsg;
	  }
	
	inline AntennaDriveMsg* getDriveMsg()
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    type = DRIVE_MSG;
	    return &body.driveMsg;
	  }
	
	inline AntennaMonitorMsg* getMonitorMsg()
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    type = MONITOR_MSG;
	    return &body.monitorMsg;
	  }
	
	inline AntennaRxMsg* getRxMsg()
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    type = RX_MSG;
	    return &body.rxMsg;
	  }
	
	inline UmacControlMsg* getUmacControlMsg()
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    type = STRIP_CONTROL_MSG;
	    return &body.umacControlMsg;
	  }

	//------------------------------------------------------------
	// Methods for packaging messages intended for this task.
	//------------------------------------------------------------
	
	/**............................................................
	 * Pack a message to send a heartbeat.
	 */
	inline void packSendHeartBeatMsg()
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    type = SEND_HEARTBEAT;
	  }
	
	/**............................................................
	 * Pack a message to adopt flagging operations for a board.
	 */
	inline void packAdoptBoardMsg(unsigned short board, 
				      AntennaTask::Id taskId)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    type = ADOPT_BOARD;
	    body.adoptBoard.board = board;
	    body.adoptBoard.taskId = taskId;
	  } 
	
	inline void packFlagBoardMsg(unsigned short board, bool flag)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    type = FLAG_BOARD;
	    body.flagBoard.board = board;
	    body.flagBoard.flag = flag;
	  }
	
	inline void packControlConnectedMsg(bool connected)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    type = CONTROL_CONNECTED;
	    
	    body.controlConnected.connected = connected;
	  }
	
	inline void packScannerConnectedMsg(bool connected)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    type = SCANNER_CONNECTED;
	    
	    body.scannerConnected.connected = connected;
	  }
	
	inline void packPmacConnectedMsg(bool connected)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    type = PMAC_CONNECTED;
	    
	    body.pmacConnected.connected = connected;
	  }

      }; // End class AntennaMasterMsg
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
