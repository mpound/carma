#ifndef SZA_UTIL_CONTROL_H
#define SZA_UTIL_CONTROL_H

/**
 * @file Control.h
 * 
 * Tagged: Tue Mar 23 06:23:03 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/FdSet.h"
#include "carma/szautil/NetStr.h"
#include "carma/szautil/SignalTask.h"
#include "carma/szautil/SzaPorts.h"
#include "carma/szautil/TcpClient.h"

namespace sza {
  namespace util {
    
    class Control {
    public:
      
      /**
       * Constructor.
       */
      Control(std::string host, bool log);

      // Non-interactive constructor

      Control(std::string host);
      void sendCommand(std::string command);
      void disconnect();
      
      /**
       * Run this object.
       */
      void processCommands();
      void processMessages();
      void readMessage();

      /**
       * Destructor.
       */
      virtual ~Control();

    private:

      // A static pointer to ourselves for use in handlers
      
      static Control* control_;

      // An object for handling signals

      SignalTask signalTask_;

      // A client object for managing our connection to the control program

      TcpClient client_;

      // A network buffer for communication with the control program

      NetStr netStr_;

      // A set of file descriptors

      FdSet fdSet_;

      // True when we should exit.

      bool stop_;

      // Thread management objects

      Thread* readThread_;

      static THREAD_START(startRead);

      Thread* sendThread_;

      static THREAD_START(startSend);

      /**
       * Read a command from stdin
       */
      void readCommand();

      /**
       * Send a command to the control program
       */
      void sendCommand();
      
      /**
       * A handler to be called to exit
       */
      static SIGNALTASK_HANDLER_FN(exitHandler);

      /**
       * Method called when a command has been sent.
       */
      static NET_SEND_HANDLER(sendHandler);

      /**
       * Method called when a command has been sent.
       */
      static NET_READ_HANDLER(readHandler);

      /**
       * Method called when an error occurs communicating with the
       * control program
       */
      static NET_SEND_HANDLER(errorHandler);

    }; // End class Control
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_CONTROL_H
