#ifndef SZA_ANTENNA_CONTROL_ANTENNACONTROL_H
#define SZA_ANTENNA_CONTROL_ANTENNACONTROL_H

/**
 * @file AntennaControl.h
 * 
 * Started: Wed Feb 25 16:40:05 PST 2004
 * 
 * @author Erik Leitch
 */
#include <string>
#include <sys/socket.h>

// Shared control code includes

#include "carma/szautil/Directives.h"
#include "carma/szautil/GenericTask.h"
#include "carma/szautil/Logger.h"
#include "carma/szautil/LogMsgHandler.h"
#include "carma/szautil/NetCommHandler.h"
#include "carma/szautil/TcpClient.h"

#include "carma/antenna/sza/antenna/control/AntennaControlMsg.h"
#include "carma/antenna/sza/antenna/control/AntNetCmdForwarder.h"
#include "carma/antenna/sza/antenna/control/SzaTask.h"

namespace sza {
  namespace antenna {
    namespace corba {
      class AntennaCorba;
    }
  }
}

namespace sza {
  namespace antenna {
    namespace control {

      /**
       * Incomplete type specifications.
       */
      class AntennaMaster;

      class AntennaControl :
	public SzaTask,
	public sza::util::GenericTask<AntennaControlMsg> {
	
	public:
	
	/**
	 * Constructor.
	 */
	AntennaControl(AntennaMaster* parent);
	
	/**
	 * Destructor.
	 */
	virtual ~AntennaControl();
	
	SzaShare* getShare();

	AntennaMaster* parent() {
	  return parent_;
	}

      private:

	/**
	 * We declare AntennaMaster a friend because its
	 * forwardCommsMsg() method will call our sendTaskMsg() method
	 */
	friend class AntennaMaster;

	/**
	 * An antennaCorba because it will access our parent_
	 */
	friend class sza::antenna::corba::AntennaCorba;

	/**
	 * A static pointer to ourselves for use in static functions.
	 */
	static AntennaControl* control_;

	sza::antenna::corba::AntennaCorba* antennaCorba_;

	/**
	 * The parent of this task.
	 */
	AntennaMaster* parent_;

	/**
	 * An object encapsulating conversion between net commands and
	 * task messages.
	 */
	AntNetCmdForwarder* forwarder_;

	/**
	 * A send stream for handling messages to be sent back to the
	 * control program
	 */
	sza::util::NetCommHandler netCommHandler_;
	
	/**
	 * An object for managing our connection to the host machine
	 */
	sza::util::TcpClient client_;

	/**
	 * An object for managing log messages
	 */
	sza::util::LogMsgHandler logMsgHandler_;

	//------------------------------------------------------------
	// Declare startup functions for threads managed by this class.
	//------------------------------------------------------------
	
	/**
	 * The antenna control task.
	 */
	static THREAD_START(startAntennaCorba);
	
	//------------------------------------------------------------
	// Declare cleanup handlers for threads managed by this class.
	//------------------------------------------------------------
	
	/**
	 * The antenna control task.
	 */
	static THREAD_CLEAN(cleanAntennaCorba);

	/**
	 * Attempt to open a connection to the control host.
	 */
	bool connect();

	/**
	 * True when connected to the RTC control port
	 */
	bool isConnected(); 

	/**
	 * Close a connection to the control host.
	 */
	void disconnect();

	/**
	 * Attempt to connect to the host
	 */
	void connectControl(bool reEnable);
	  
	/**
	 * Attempt to connect to the host
	 */
	void disconnectControl();
	  
	/**
	 * Overwrite the base-class method.
	 */
	void serviceMsgQ();

	/**
	 * Send a message to the parent about the connection status of
	 * the host
	 */
	void sendControlConnectedMsg(bool connected);

	/**
	 * Send a string to be logged back to the control program.
	 */
	static LOG_HANDLER_FN(sendLogMsg);
	
	/**
	 * Send an error string to be logged back to the control
	 * program.
	 */
	static LOG_HANDLER_FN(sendErrMsg);
	
	static void sendNetMsg(std::string& logStr, bool isErr);

	/**
	 * Overwrite the base-class method.
	 */
	void processMsg(AntennaControlMsg* msg);

	/**
	 * Pack a network message intended for the ACC
	 */
	void packNetMsg(AntennaControlMsg* msg);

	/**
	 * Send a greeting message
	 */
	void sendGreeting();

	/**
	 * A handler called when a command has been read from the
	 * control program.
	 */
	static NET_READ_HANDLER(netCmdReadHandler);
	
	/**
	 * A handler called when a message has been read from the
	 * control program.
	 */
	static NET_SEND_HANDLER(netMsgReadHandler);

	/**
	 * A handler called when a message has been sent to the
	 * control program.
	 */
	static NET_SEND_HANDLER(netMsgSentHandler);
	
	/**
	 * Call this function if an error occurs while communicating
	 * with the ACC
	 */
	static NET_ERROR_HANDLER(netErrorHandler);
	
	/**
	 * Send an antenna ID message to the control program
	 * decide what to do
	 */
	void sendAntennaIdMsg();

	/**
	 * Read a greeting message from the control program, and
	 * decide what to do
	 */
	void parseGreetingMsg(sza::util::NetMsg* msg);

	/**
	 * Write a sequence number expected by the CARMA control system
	 */
	void writeCarmaSeqNo(AntennaControlMsg* msg);

	/**.......................................................................
	 * Write CARMA monitor points
	 */
	void writeCarmaMonitorPoints();

      }; // End class AntennaControl

    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 



