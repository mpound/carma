#ifndef SZA_ANTENNA_CANBUS_CANDEVICE_H
#define SZA_ANTENNA_CANBUS_CANDEVICE_H

/**
 * @file CanDevice.h
 * 
 * Started: Fri Nov 21 15:46:44 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/canbus/CanMonitor.h"
#include "carma/antenna/sza/antenna/canbus/CanMonitorPoint.h"

#include "carma/antenna/sza/antenna/control/Board.h"

#include "carma/szautil/Rx.h"

#include "carma/canbus/Device.h"

#include <string>
#include <vector>

// The maximum number of characters which can be returned from a CAN
// node in an ASCII character std::string.

#define CAN_MAX_CHAR 8

// The maximum number of error log entries maintained by a node.

#define CAN_MAX_ERROR_LOG_ENTRY 64

// Define a handler for a CAN status message

#define CAN_STATUS_MSG_HANDLER(fn) void (fn)(void* arg1, unsigned arg2)

namespace sza {
  namespace antenna {

    // Forward declaration of SzaShare lets us use it without defining
    // it

    namespace control {
      class SzaShare;
    }

    namespace canbus {
      
      /**
       * A class for encapsulating devices for the SZA which will be
       * attached to a CAN bus.
       *
       * CanDevice inherits the public interface of Device and the
       * protected members of Board (SzaShare* share_ and
       * RegMapBoard* board_)
       */
      class CanDevice : 
	public carma::canbus::Device,
	public sza::antenna::control::Board {
	
	public:
	
	/**
	 * Constructor.  
	 *
	 * @param share Pointer to the shared memory object used for
	 * monitoring
	 *
	 * @param boardName The name of the board in the shared
	 * memory object which this device will control.
	 *
	 * @param api An identifier for the API version of
	 * inheritors of this class.
	 *
	 * @param node The CAN node id
	 *
	 * @param io Reference to CanOutput class - should be
	 * obtained from downcasting the canbus::Master reference in
	 * inherited canbus::Master class.
	 */
	CanDevice(sza::antenna::control::SzaShare* share,
		  std::string boardName,
		  carma::canbus::apiType api,
		  carma::canbus::nodeType node, 
		  carma::canbus::CanOutput& io);
	
	/**
	 * Destructor.  Make this virtual so that inheritors'
	 * destructors will be properly called.
	 */
	virtual ~CanDevice();
	
	/**
	 * Enable/Disable monitor packets.  We make this public so
	 * that tasks can call it on start up.
	 */
	virtual void enableMonitorPackets(bool timeStampEnable, 
					  bool blankingFrameEnable,
					  bool slowMonitorEnable);
	
	/**
	 * Send the current mjd
	 */
	virtual void sendTimeStamp();

	/**
	 * Post a fully constructed message to this device
	 */
	void postMessage(carma::canbus::Message msg);

	CanMonitorPoint* findMonitorPoint(char* name);

	// Maintain information about the receiver currently selected
	
	virtual void setRxId(sza::util::Rx::Id rxId);
	
	std::vector<carma::canbus::Message> sendDummyMsg(bool send);

	protected:
	
	// All SZA CAN devices will have these members.
	
	CanMonitor monitor_;

	sza::util::Rx::Id rxId_;

	// A struct to encapsulate a status-message/handler
	// association

	struct HandlerInfo 
	{
	  CAN_STATUS_MSG_HANDLER(*handler_);
	  carma::canbus::msgType mid_;
	  void* arg1_;
	  unsigned arg2_;

	  HandlerInfo(carma::canbus::msgType mid,
		      CAN_STATUS_MSG_HANDLER(*handler),
		      void* arg1, unsigned arg2)
	  {
	    mid_     = mid;
	    handler_ = handler;
	    arg1_    = arg1;
	    arg2_    = arg2;
	  }
	};

	// A vector of handlers for status messages

	std::vector<HandlerInfo> handlers_;

	// Install a handler for a given message id

	void installHandler(carma::canbus::msgType mid,
			    CAN_STATUS_MSG_HANDLER(*handler),
			    void* arg1,
			    unsigned arg2);

	/**
	 * Init registers common to all CAN devices
	 */
	void initRegs();

	/**
	 * Register receipt of this monitor packet
	 */
	void registerReceived(unsigned id, bool isSim);

	//------------------------------------------------------------
	// Overloaded methods from the base class.
	//------------------------------------------------------------
	
	//------------------------------------------------------------
	// Device control

	/**
	 * Return a map of device controls.
	 */
	std::map<carma::canbus::msgType, std::string> 
	  getControls();
	
	/**
	 * A map of controls common to all devices
	 */
	std::map<carma::canbus::msgType, std::string> 
	  getCommonControls();

	/**
	 * Get module-specific controls
	 */
	virtual std::map<carma::canbus::msgType, std::string> 
	  getSpecificControls() const;
	
	/**
	 * Return an inheritor-specific prefix to use when
	 * constructing monitor points
	 */
	virtual std::string controlPrefix();

	/**
	 * Return a map of devices half second monitor points.
	 */ 
	virtual std::map<carma::canbus::msgType, std::string> 
	  getHalfSecMonitors() const;
	
	/**
	 * Return a map of slow monitor points.
	 */
	virtual std::map<carma::canbus::msgType, std::string> 
	  getSlowMonitors() const;

	/**
	 * Return a map of engineering monitor points.
	 */
	virtual std::map<carma::canbus::msgType, std::string> 
	  getEngineeringMonitors() const;
	
	/**
	 * Return a map of status messages
	 */
	virtual std::map<carma::canbus::msgType, std::string> 
	  getStatusMessages() const;

	/**
	 * Return a CAN message ID constructed from the api, node
	 * and command ID
	 */
	carma::canbus::idType createId(bool host, 
				       carma::canbus::msgType mid);
	
	carma::canbus::idType createDummyId();

	/**
	 * Return a CAN message ID constructed from the api, node
	 * and command ID
	 */
	carma::canbus::idType createEngId(bool host, 
					  carma::canbus::msgType mid);

	/**
	 * Return a CAN bus ID suitable for passing to Message
	 * constructor
	 */
	carma::canbus::busIdType getBusId();
	
	/**
	 * Post a message for this device.
	 */
	void postMessage(carma::canbus::msgType mid,
			 std::vector<carma::canbus::byteType>& data);
	
	/**
	 * Post a message with no intrinsic payload.
	 */
	void postMessage(carma::canbus::msgType mid);
	void postDummyMessage();
	
  	/**
	 * Post a message for this device.
	 */
	void postEngMessage(carma::canbus::msgType mid,
			    std::vector<carma::canbus::byteType>& data);
	
	/**
	 * Get an initialized node message.
	 */
	carma::canbus::Message 
	  nodeMessage(carma::canbus::msgType mid,
		      std::vector<carma::canbus::byteType>& data);
	
	/**
	 * Get an initialized node message with no intrinsic
	 * payload.
	 */
	carma::canbus::Message nodeMessage(carma::canbus::msgType mid);
	carma::canbus::Message dummyMessage();
	
	/**
	 * Get an initialized engineering node message.
	 */
	carma::canbus::Message 
	  nodeEngMessage(carma::canbus::msgType mid,
			 std::vector<carma::canbus::byteType>& data);

	/**
	 * Get an initialized host message.
	 */
	carma::canbus::Message 
	  hostMessage(carma::canbus::msgType mid,
		      std::vector<carma::canbus::byteType>& data);
	
	/**
	 * Get an initialized host message with no intrinsic
	 * payload.
	 */
	carma::canbus::Message hostMessage(carma::canbus::msgType mid);
	
	//------------------------------------------------------------
	// Host commands common to all CAN devices
	//------------------------------------------------------------

	enum commonHostCommands
	  {
	    HOSTCMD_RESET         = 0x000, // Reset
	    HOSTCMD_SET_TIME      = 0x001, // Set the time (MJD)
	    HOSTCMD_STOP_CHAN1    = 0x002, // Stop channel 1 fast sampling
	    HOSTCMD_STOP_CHAN2    = 0x003, // Stop channel 2 fast sampling
	    HOSTCMD_START_CHAN1   = 0x004, // Start channel 1 fast sampling
	    HOSTCMD_START_CHAN2   = 0x005, // Start channel 2 fast sampling
	  };

	//------------------------------------------------------------
	// Engineering commands common to all CAN devices.
	//------------------------------------------------------------
	
	enum commonEngineeringCommands 
	  {
	    ENGCMD_ERROR_LOG      = 0x3F0,
	    ENGCMD_ERROR_CLEAR    = 0x3F4,
	    ENGCMD_MONITOR_ENABLE = 0x3FB,
	    ENGCMD_ID_REQUEST     = 0x3FC,
	    ENGCMD_START_DOWNLOAD = 0x3FD,
	    ENGCMD_ASCII_STRING   = 0x3FF
	  };
	
	/**
	 * Retrieve a numbered error log entry.
	 */
	virtual void retrieveErrorLogEntry(unsigned short recordNo);
	
	/**
	 * Clear the error log.
	 */
	virtual void clearErrorLog();
	
	/**
	 * Request the module ID.
	 */
	virtual void requestId();
	
	/**
	 * Start downloading a new program.
	 */
	virtual void startDownLoad();
	
	/**
	 * Send a std::string of up to 8 characters to the CAN node.
	 */
	virtual void sendAsciiString(std::string sendStr);
	
	//============================================================
	// Message processing methods.
	//============================================================
	
	/**
	 * Process a message for this device.
	 */
	virtual void processMsg(carma::canbus::msgType messageId, 
				std::vector<carma::canbus::byteType>& data,
				bool isSim);
	
	/**
	 * Simulate messages to this device
	 */
	virtual carma::canbus::Message simulateMsg(carma::canbus::msgType mid);

	//------------------------------------------------------------
	// Blanking frame (2 Hz) monitor packets
	//------------------------------------------------------------

	/**
	 * Blanking frame monitor packets are generally the same for
	 * all CAN nodes.
	 */
	enum defaultHalfSecMonitors {
	  MONITOR_PACKET_1    = 0x0E0,
	  MONITOR_PACKET_2    = 0x0E1,
	  MONITOR_PACKET_3    = 0x0E2,
	  MONITOR_PACKET_4    = 0x0E3,
	  MONITOR_PACKET_5    = 0x0E4,
	  MONITOR_PACKET_6    = 0x0E5,
	  MONITOR_PACKET_7    = 0x0E6,
	  MONITOR_PACKET_8    = 0x0E7,
	  MONITOR_PACKET_9    = 0x0E8,
	  MONITOR_PACKET_10   = 0x0E9,
	  MONITOR_PACKET_11   = 0x0EA,
	};

	/**
	 * Return true if this is a valid blanking frame monitor
	 * packet.
	 */
	bool isBlankingFrameMonitor(carma::canbus::msgType mid);
	
	/**
	 * Return a standard enumerator for monitor packets
	 */
	unsigned idOfBlankingFrameMonitorMsgType(carma::canbus::msgType mid);
	
	/**
	 * Return the message id corresponding to a standard enumerator
	 */
	carma::canbus::msgType msgTypeOfBlankingFrameMonitorId(unsigned int id);

	/**
	 * Process a blanking frame monitor packet.
	 */
	virtual void 
	  processBlankingFrameMonitor(carma::canbus::msgType mid, 
				      std::vector<carma::canbus::byteType>& 
				      data, bool isSim);
	
	/**
	 * Stubs for processing up to 10 monitor packets.
	 */
	virtual void 
	  processBlankingFrameMonitor1(std::vector<carma::canbus::byteType>& 
				       data, bool isSim) {};
	virtual void 
	  processBlankingFrameMonitor2(std::vector<carma::canbus::byteType>& 
				       data, bool isSim) {};
	virtual void 
	  processBlankingFrameMonitor3(std::vector<carma::canbus::byteType>& 
				       data, bool isSim) {};
	virtual void 
	  processBlankingFrameMonitor4(std::vector<carma::canbus::byteType>& 
				       data, bool isSim) {};
	virtual void 
	  processBlankingFrameMonitor5(std::vector<carma::canbus::byteType>& 
				       data, bool isSim) {};
	virtual void 
	  processBlankingFrameMonitor6(std::vector<carma::canbus::byteType>& 
				       data, bool isSim) {};
	virtual void 
	  processBlankingFrameMonitor7(std::vector<carma::canbus::byteType>& 
				       data, bool isSim) {};
	virtual void 
	  processBlankingFrameMonitor8(std::vector<carma::canbus::byteType>& 
				       data, bool isSim) {};
	virtual void 
	  processBlankingFrameMonitor9(std::vector<carma::canbus::byteType>& 
				       data, bool isSim) {};
	virtual void 
	  processBlankingFrameMonitor10(std::vector<carma::canbus::byteType>& 
					data, bool isSim) {};
	virtual void 
	  processBlankingFrameMonitor11(std::vector<carma::canbus::byteType>& 
					data, bool isSim) {};

	/**
	 * Simulate blanking frame monitor packets
	 */
	virtual carma::canbus::Message 
	  simulateBlankingFrameMonitor(carma::canbus::msgType mid);

	virtual carma::canbus::Message simulateBlankingFrameMonitor1();
	virtual carma::canbus::Message simulateBlankingFrameMonitor2();
	virtual carma::canbus::Message simulateBlankingFrameMonitor3(); 
	virtual carma::canbus::Message simulateBlankingFrameMonitor4(); 
	virtual carma::canbus::Message simulateBlankingFrameMonitor5(); 
	virtual carma::canbus::Message simulateBlankingFrameMonitor6(); 
	virtual carma::canbus::Message simulateBlankingFrameMonitor7(); 
	virtual carma::canbus::Message simulateBlankingFrameMonitor8(); 
	virtual carma::canbus::Message simulateBlankingFrameMonitor9(); 
	virtual carma::canbus::Message simulateBlankingFrameMonitor10(); 
	virtual carma::canbus::Message simulateBlankingFrameMonitor11(); 
 
	/**
	 * Simulate a generic monitor packet
	 */
	virtual carma::canbus::Message 
	  simulateGenericBlankingFrameMonitor(carma::canbus::msgType mid);

	//------------------------------------------------------------
	// Slow monitor packets
	//------------------------------------------------------------

	/**
	 * Slow monitor packets will be the same for all CAN nodes.
	 */
	enum slowMonitors {
	  SLOW_MONITOR_1  = 0x120,
	  SLOW_MONITOR_2  = 0x121,
	  SLOW_MONITOR_3  = 0x122,
	  SLOW_MONITOR_4  = 0x123,
	  SLOW_MONITOR_5  = 0x124
	};
	
	//------------------------------------------------------------
	// Members to do with slow monitors common to all CAN
	// devices.
	//------------------------------------------------------------
	
	unsigned short moduleSerialNo_;

	unsigned int initRequest_;
	unsigned int rxErrors_;
	unsigned int txErrors_;
	unsigned int memoryErrors_;
	unsigned int systemErrors_;
	unsigned short schedulerOverflowCount_;
	unsigned short timedSchedulerOverflowCount_;

	unsigned char softwareMajorVersion_;
	unsigned char softwareMinorVersion_;
	unsigned char softwareTestVersion_;
	unsigned int swVersionStr_[3];

	unsigned short communicationErrorCount_;
	unsigned short timeErrorCount_;
	unsigned short softwareErrorCount_;
	unsigned short hardwareErrorCount_;
	short timeOffset_;
	short timeStampInterval_;
	short timeStampDelta_;
	
	/**
	 * Return true if this is a slow monitor packet. 
	 */
	bool isSlowMonitor(carma::canbus::msgType mid);
	
	/**
	 * Process a slow monitor packet.
	 */
	virtual void 
	  processSlowMonitor(carma::canbus::msgType mid, 
			     std::vector<carma::canbus::byteType>& data,
			     bool isSim);
	
	/**
	 * Process the first slow monitor packet.
	 */
	virtual void 
	  processSlowMonitor1(std::vector<carma::canbus::byteType>& data, bool isSim);
	
	/**
	 * Process the second slow monitor packet.
	 */
	virtual void 
	  processSlowMonitor2(std::vector<carma::canbus::byteType>& data, bool isSim);
	
	/**
	 * Process the third slow monitor packet.
	 */
	virtual void 
	  processSlowMonitor3(std::vector<carma::canbus::byteType>& data, bool isSim);
	
	/**
	 * Process the fourth slow monitor packet.
	 */
	virtual void 
	  processSlowMonitor4(std::vector<carma::canbus::byteType>& data, bool isSim);
	
	/**
	 * Process the fifth slow monitor packet.
	 */
	virtual void 
	  processSlowMonitor5(std::vector<carma::canbus::byteType>& data, bool isSim);

	//============================================================
	// Engineering monitor packets
	//============================================================

	/**
	 * Engineering monitor packets.  Same for all CAN devices.
	 */
	enum engineeringMonitors {
	  ENG_MONITOR_1 = 0x1F0, // Error record 1
	  ENG_MONITOR_2 = 0x1F1, // Error record 2
	  ENG_MONITOR_3 = 0x3FC, // ID request
	  ENG_MONITOR_4 = 0x3FF  // ASCII string
	};
	
	//------------------------------------------------------------
	// Members to do with engineering monitor members common to
	// all CAN devices.
	//------------------------------------------------------------
	
	unsigned int errLog_;
	unsigned int mjd_;
	unsigned int mjdTime_;
	unsigned int errCode_;
	unsigned int errCnt_;
	unsigned int serialNo_;
	unsigned int errData_;
	unsigned int moduleType_;

	unsigned short apiNo_;
	unsigned short dongleId_;

	unsigned char asciiString_[CAN_MAX_CHAR+1];

	/**
	 * Return true if this is a valid Engineering packet.
	 */
	bool isEngineeringMonitor(carma::canbus::msgType mid);

	/**
	 * Process an engineering monitor packet.
	 */
	virtual void 
	  processEngineeringMonitor(carma::canbus::msgType mid, 
				    std::vector<carma::canbus::byteType>& data,
				    bool isSim);
	
	// Engineering monitor process methods
	
	virtual void 
	  processEngineeringMonitor1(std::vector<carma::canbus::byteType>& data, bool isSim);
	
	virtual void 
	  processEngineeringMonitor2(std::vector<carma::canbus::byteType>& data, bool isSim);
	
	virtual void 
	  processEngineeringMonitor3(std::vector<carma::canbus::byteType>& data, bool isSim);
	
	virtual void 
	  processEngineeringMonitor4(std::vector<carma::canbus::byteType>& data, bool isSim);
	
	//------------------------------------------------------------
	// Status messages
	//------------------------------------------------------------

	/**
	 * Return true if this is a valid status message for this
	 * device
	 */
	bool isStatusMessage(carma::canbus::msgType mid);
	
	/**.......................................................................
	 * Return a standard id corresponding to this status frame id
	 */
	unsigned idOfStatusMessageMsgType(carma::canbus::msgType mid);

	/**
	 * Process an status message
	 */
	virtual void 
	  processStatusMessage(carma::canbus::msgType mid, bool isSim);

	// Stub out methods for processing up to three status messages

	virtual void processStatusMessage1(bool isSim);
	virtual void processStatusMessage2(bool isSim);
	virtual void processStatusMessage3(bool isSim);
	virtual void processGenericStatusMessage(bool isSim);

	//------------------------------------------------------------
	// Utilities.
	//------------------------------------------------------------
	
	/**
	 * Skip a number of bytes in the data array.
	 */
	void skipByte(std::vector<carma::canbus::byteType>& data, 
		      unsigned short nByte);

	std::string monthDayYearToString(unsigned int month, unsigned int day, unsigned year);


      }; // End class CanDevice
      
    }; // End namespace canbus
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 


