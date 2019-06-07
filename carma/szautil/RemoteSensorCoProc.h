#ifndef SZA_UTIL_REMOTESENSORCOPROC_H
#define SZA_UTIL_REMOTESENSORCOPROC_H

/**
 * @file RemoteSensorCoProc.h
 * 
 * Tagged: Mon Jul 19 14:47:35 PDT 2004
 * 
 * @author Erik Leitch
 */
#include <list>
#include <string>
#include <sstream>

#include "carma/szautil/CoProc.h"
#include "carma/szautil/Communicator.h"
#include "carma/szautil/FdSet.h"
#include "carma/szautil/Port.h"
#include "carma/szautil/RemoteSensor.h"
#include "carma/szautil/String.h"

namespace sza {
  namespace util {
    
    //-----------------------------------------------------------------------
    // RemoteSensorCoProc class is defined here
    //-----------------------------------------------------------------------

    class RemoteSensorCoProc : public Communicator, public RemoteSensor {
    public:

      //------------------------------------------------------------
      // Methods for use of this object in a stand-alone thread
      //------------------------------------------------------------

      // Constructor for internal use of this class (ie, run in its
      // own thread).  If timeOutIntervalInSeconds is non-zero, this
      // object will automatically retrieve the device status on the 
      // specified interval.  Else it will do nothing until told to
      // retrieve it.

      RemoteSensorCoProc(std::string host, 
			 unsigned timeoutIntervalInSeconds=0);
      
      RemoteSensorCoProc(std::string host, 
			 SystemStatusSubsystemMutex*         ms,
			 carma::monitor::MonitorPointBool&   mpIsReachable,
			 carma::monitor::MonitorPointDouble& mpCurrentTemp,
			 unsigned timeoutIntervalInSeconds=0);
      
      RemoteSensorCoProc(std::string host, 
			 SystemStatusSubsystemMutex*            ms,
			 carma::monitor::MonitorPointBool&      mpHostIsReachable,
			 carma::monitor::MonitorPointBool&      mpHostIsOk,
			 carma::monitor::MonitorPointBool&      mcPlaceIsOk,
			 carma::monitor::MonitorPointEnum&      mcPlaceStatus,
			 carma::monitor::MonitorPointAbstime&   mpSampleTime,
			 carma::monitor::MonitorPointDouble&    mpCurrentTemp,
			 sza::util::Temperature&                minTemp,
			 sza::util::Temperature&                maxTemp,
			 unsigned timeoutIntervalInSeconds=0);

      // Destructor.

      virtual ~RemoteSensorCoProc();

      protected:

      //------------------------------------------------------------
      // Methods that should be overloaded by inheritors wanting to
      // use the CoProc class for processing feedback from generic
      // types of commands
      //------------------------------------------------------------

      void executeReadSensor();

      // Compile the list of communication/responses needed for
      // retrieving the device status from the remote server

      virtual void compileGetDeviceStatusStateMachine() = 0;

      // Assemble the command string to be issued

      virtual std::string getCommandString() = 0;

      // Define what it means to parse the return message

      virtual void processDeviceStatus() = 0;

      static COMM_PARSER_FN(parseDeviceStatus);

      private:
      
      //------------------------------------------------------------
      // Generic methods
      //------------------------------------------------------------

      // Initialize pertinent members of this class to sensible
      // defaults

      void initialize();

      // Initiate the comms sequence to retrieve the tipper log from
      // the remote server

      void initiateGetDeviceStatusCommSequence();

      // Process a status message recieved from the server

      void connect();
      void disconnect();

      // Return the read fd associated with the server connection

      int getFd();

      // Write a string to the server

      void writeString(std::string);

      // Concatenate a string received from the server

      void concatenateString(std::ostringstream& os);

      //------------------------------------------------------------
      // Methods for use of this object in a stand-alone thread
      //------------------------------------------------------------

      void serviceMsgQ();

      //------------------------------------------------------------
      // Generic methods
      //------------------------------------------------------------

      // React to a timeout during communication

      void registerTimeOut();

      // Terminate a command sequence to the

      void terminateCommSequence(bool error);
      
      // The process that will communicate with the telnet server

      CoProc* coProc_;

      sza::util::Port errPort_;
      sza::util::Port readPort_;
      sza::util::Port writePort_;

      bool connected_;

    }; // End class RemoteSensorCoProc
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_REMOTESENSORCOPROC_H
