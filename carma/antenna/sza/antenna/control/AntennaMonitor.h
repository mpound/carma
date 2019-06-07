#ifndef ANTENNAMONITOR_H
#define ANTENNAMONITOR_H

/**
 * @file AntennaMonitor.h
 * 
 * Tagged: Thu Nov 13 16:53:29 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/DataFrame.h"
#include "carma/szautil/GenericTask.h"
#include "carma/szautil/NetStr.h"
#include "carma/szautil/NetMsgHandler.h"
#include "carma/szautil/SignalTask.h"
#include "carma/szautil/TcpClient.h"

#include "carma/antenna/sza/antenna/control/AntennaMonitorMsg.h"
#include "carma/antenna/sza/antenna/control/Scanner.h"
#include "carma/antenna/sza/antenna/control/SzaTask.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      /**
       * Forward declaration of AntennaDrive.
       */
      class AntennaDrive;
      class AntennaMaster;

      /**
       * Define a class for sending snapshots of the antenna state
       * back to the control program for archiving.
       */      
      class AntennaMonitor : 
	public SzaTask,
	public sza::util::GenericTask<AntennaMonitorMsg> {
	
	private:
	
	
	/**
	 * An object which will handle transfer of data frames to the
	 * outside world
	 */
	Scanner* scanner_;
	
	/**
	 * We declare AntennaMaster a friend because its
	 * startAntennaMonitor() method will call serviceMsgQ() above
	 */
	friend class AntennaMaster;
	
	/**
	 * A pointer to the AntennaDrive resources
	 */
	AntennaMaster* parent_;
	
	/**
	 * Private constructor function prevents instantiation by
	 * anyone but friends of this class, i.e., AntennaMaster.
	 *
	 * @throws Exception
	 */
	AntennaMonitor(AntennaMaster* master);
	
	/**
	 * Destructor.
	 *
	 * @throws Exception
	 */
	~AntennaMonitor();
	
	//------------------------------------------------------------
	// Methods for communication with this task
	//
	
	/**
	 * Send a message to dispatch the next frame.
	 */
	void sendDispatchDataFrameMsg();
	
	/**
	 * Process messages received from the AntennaMonitor message queue
	 *
	 * @param taskMsg AntennaMsg* The message received from the
	 * message queue
	 *
	 * @throws Exception
	 */
	void processMsg(AntennaMonitorMsg* taskMsg);
	
	/**
	 * The IP address of the control host
	 */
	std::string host_;           
	
	/**
	 * An object to manage our connection to the control program
	 */
	sza::util::TcpClient client_;
	
	/**
	 * A network stream handler for sending a greeting to the archiver
	 * program
	 */
	sza::util::NetMsgHandler netMsgHandler_;

	/**
	 * True when a greeting is waiting to be dispatched.
	 */
	bool connectionPending_;

	/**
	 * The TCP/IP network handler we will use to send data frames
	 * to the archiver.
	 */
	sza::util::NetHandler netDataFrameHandler_;       
	
	/**
	 * True when a frame is waiting to be dispatched.
	 */
	bool dispatchPending_;

	/**
	 * Attempt to open a connection to the control host.
	 */
	bool connect();

	/**
	 * Close a connection to the control host.
	 */
	void disconnect();

	/**
	 * Attempt to connect to the scanner
	 */
	void connectScanner(bool reEnable);
	  
	/**
	 * Attempt to connect to the scanner
	 */
	void disconnectScanner();
	  
	/**
	 * Overwrite the base-class method.
	 */
	void serviceMsgQ();

	// Handlers to be called while communicating with the
	// translator

	static NET_READ_HANDLER(netMsgReadHandler);
	static NET_SEND_HANDLER(netMsgSentHandler);
	static NET_SEND_HANDLER(netDataFrameSentHandler);

	/**
	 * A handler to be called when an error occurs in communication
	 */
	static NET_ERROR_HANDLER(netErrorHandler);

	/**
	 * If there is room in the circular frame buffer, record another
	 * data frame and push it onto the event channel.
	 */
	void packNextFrame();

	/**
	 * If there is room in the circular frame buffer, record another
	 * data frame and push it onto the event channel.
	 */
	void dispatchNextFrame();

	/**
	 * Install the frame buffer as the network buffer and pre-format the
	 * register frame output message.
	 */
	void packFrame(sza::util::DataFrame* frame);

	/**
	 * Send a message to the parent about the connection status of the
	 * host
	 */
	void sendScannerConnectedMsg(bool connected);

	/**
	 * Pack a greeting message to be sent to the controller.
	 */
	void sendAntennaIdMsg();

	/**
	 * Read a greeting message from the control program, and
	 * decide what to do
	 */
	void parseGreetingMsg(sza::util::NetMsg* msg);

      }; // End class AntennaMonitor
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif
