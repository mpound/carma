// $Id: GpibUsbController.h,v 1.1 2010/12/13 21:06:30 eml Exp $

#ifndef SZA_UTIL_GPIBUSBCONTROLLER_H
#define SZA_UTIL_GPIBUSBCONTROLLER_H

/**
 * @file GpibUsbController.h
 * 
 * Tagged: Tue Oct 16 13:01:01 PDT 2007
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:30 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/CondVar.h"
#include "carma/szautil/SerialClient.h"
#include "carma/szautil/SpawnableTask.h"
#include "carma/szautil/GenericTaskMsg.h"
#include "carma/szautil/TimeOut.h"

#include <sstream>

#define GPIB_RESPONSE_HANDLER(fn) bool (fn)(void* arg)

namespace sza {
  namespace util {
    
    class GpibUsbDevice;

    // A utility class for sending messages to the GpibUsbController task
      
    class GpibUsbControllerMsg : public GenericTaskMsg {
    public:
      
      enum MsgType {
	GPIB_CNT_MSG,
	GPIB_DEV_MSG,
	CONNECT,
      };

      unsigned char cmd_[100];
      bool expectsResponse_;
      GPIB_RESPONSE_HANDLER(*responseHandler_);

      CondVar* condVar_;
      void* retVal_;

      // A type for this message
      
      MsgType type_;
    };
      
    class GpibUsbController : public SerialClient, public SpawnableTask<GpibUsbControllerMsg> {
    public:
      
      // Constructor.

      GpibUsbController(bool doSpawn=false);
      GpibUsbController(std::string port, bool doSpawn=false);
      
      // Destructor.

      virtual ~GpibUsbController();
      
      void stop();

      // Open the psuedo-serial port

      int connect();
      int connectAndClear();

      //------------------------------------------------------------
      // Controller commands
      //------------------------------------------------------------

      // Set whether or not to auto-listen after a command

      void setAuto(bool doAuto);

      // Select the operational mode for the controller

      enum Mode {
	MODE_CONTROLLER = 0,
	MODE_DEVICE     = 1,
      };

      void setMode(Mode mode);

      // Set or get the GPIB address

      virtual void setAddress(unsigned address);
      virtual unsigned getAddress();

      // Set or query if we are sending EOI

      void setEoi(bool sendEoi);
      std::string getEoi();

      // Get the version

      std::string getVersion();

      // Get the help output

      std::string getHelp();

      // Set or query the EOS characters

      enum {
	EOS_CRLF = 0,
	EOS_CR   = 1,
	EOS_LF   = 2,
	EOS_NONE = 3,
      };

      void setEos(unsigned);
      std::string getEos();

      // Clear the interfacae

      void clearInterface();

      // Reset the controller

      void resetController();

      // This command enables or disables the appending of a user
      // specified character (specified by setEotChar()) to network
      // output whenever EOI is detected while reading a character
      // from the GPIB port

      void enableEot(bool enable);

      // Thie command sets the character that should be appended when
      // EOI is asserted

      void setEotChar(char eot);

      //------------------------------------------------------------
      // Common Device commands
      //------------------------------------------------------------

      // Get the device identification string

      std::string getDevice();

      // Reset the device

      void resetDevice();

      // Issue a GPIB clear command

      void clearDevice();

      // Send the device the 'clear interface' command

      void clearDeviceInterface();

      // Query the status of a self test

      std::string getSelfTest();

      //------------------------------------------------------------
      // Other members
      //------------------------------------------------------------

      std::ostringstream response_;
      void* retVal_;

      // Wait until all overlapping commands have been executed

      void wait();

      // Response handlers

      static GPIB_RESPONSE_HANDLER(checkString);

    protected:

      friend class GpibUsbDevice;

      enum Receiver {
	DEVICE     = 0,
	CONTROLLER = 1
      };

      //------------------------------------------------------------
      // Data members
      //------------------------------------------------------------

      TimeOut cmdTimeOut_;

      std::string devName_;
      std::string lastCmd_;
      bool expectingResponse_;
      GPIB_RESPONSE_HANDLER(*responseHandler_);
      CondVar* condVar_;

      bool eotEnabled_;

      //------------------------------------------------------------
      // Private methods
      //------------------------------------------------------------

      void initialize(bool doSpawn);
      void serviceMsgQ(void);
      void processMsg(GpibUsbControllerMsg* msg);
      
      // Suspend or resume processing messages

      void suspendProcessingMessages(bool suspend);
      
      // Watch the serial port for a response, and set a timeout
      
      void watchForResponse(bool watch);

      // Respond to a timeout while waiting for a response

      void respondToTimeOut();

      // Read/continue reading a response

      void readResponse();

      // Respond to unexpected input from the controller

      void respondToUnexpectedInput();

      void sendDeviceCommand(std::string cmd, bool expectsResponse=false, 
			     GPIB_RESPONSE_HANDLER(*handler)=0, bool block=false, 
			     void* retVal=0);

      void sendControllerCommand(std::string cmd, bool expectsResponse=false, 
				 GPIB_RESPONSE_HANDLER(*handler)=0, bool block=false, void* retVal=0);

      virtual void sendCommand(std::string cmd, Receiver rx, bool expectsResponse=false, 
			       GPIB_RESPONSE_HANDLER(*handler)=0, bool block=false, void* retVal=0);

      void executeCommand(std::string cmd, bool expectsResponse, 
			  GPIB_RESPONSE_HANDLER(*handler), CondVar* condVar, void* retVal);

      // Private execute methods for specific commands

      void executeConnect(void* retVal);

      static GPIB_RESPONSE_HANDLER(checkHandler);

      static GPIB_RESPONSE_HANDLER(checkAddress);
      static GPIB_RESPONSE_HANDLER(checkDevice);
      static GPIB_RESPONSE_HANDLER(checkCompletion);

    }; // End class GpibUsbController

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_GPIBUSBCONTROLLER_H
