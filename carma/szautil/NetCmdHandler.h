#ifndef SZA_UTIL_NETCMDHANDLER_H
#define SZA_UTIL_NETCMDHANDLER_H

/**
 * @file NetCmdHandler.h
 * 
 * Started: Thu Feb 26 16:21:06 UTC 2004
 * 
 * @author Erik Leitch
 */

// Shared control code includes

#include "carma/szaarrayutils/rtcnetcoms.h"

#include "carma/szautil/NetHandler.h"
#include "carma/szautil/NetCmd.h"

namespace sza {
  namespace util {

    class NetCmdHandler : public NetHandler {
    public:
      
      /**
       * Constructor.
       */
      NetCmdHandler();
      
      /**
       * Destructor.
       */
      virtual ~NetCmdHandler();
      
      /**
       * Pack a network command into our send buffer.;
       */
      void packNetCmd(sza::util::NetCmd* rtc);

      /**
       * Pack a command into our send buffer.
       */
      void packNetCmd(sza::array::RtcNetCmd* rtc, 
		      sza::array::NetCmdId opcode);

      /**
       * Return the network message we just read.
       */
      sza::util::NetCmd* getLastReadNetCmd();

      /**
       * Of the one we just sent
       */
      sza::util::NetCmd* getLastSentNetCmd();

      /**
       * Overload the base-class methods to install user-defined handlers
       */
      void installReadHandler(NET_READ_HANDLER(*handler), void* arg);
      void installSendHandler(NET_SEND_HANDLER(*handler), void* arg);
      void installErrorHandler(NET_ERROR_HANDLER(*handler), void* arg);

    private:
      
      /**
       * The last read command.
       */
      sza::util::NetCmd lastReadNetCmd_;

      /**
       * The last sent command.
       */
      sza::util::NetCmd lastSentNetCmd_;

      /**
       * Read a NetCmd out of the network buffer
       */
      void readNetCmd();

      /**
       * A handler to be called when a message has been completely read.
       */
      static NET_READ_HANDLER(readHandler);

      // A pointer to a user-defined handler

      NET_READ_HANDLER(*userReadHandler_);
      void* userReadArg_;

      /**
       * A handler to be called when a message has been completely sent.
       */
      static NET_SEND_HANDLER(sendHandler);

      // A pointer to a user-defined handler

      NET_SEND_HANDLER(*userSendHandler_);
      void* userSendArg_;

      /**
       * A handler to be called when an error has occurred
       */
      static NET_ERROR_HANDLER(errorHandler);

      // A pointer to a user-defined handler

      NET_ERROR_HANDLER(*userErrorHandler_);
      void* userErrorArg_;

    }; // End class NetCmdHandler

  }; // End namespace util
}; // End namespace sza
  
#endif // End #ifndef 
  
  
