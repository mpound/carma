#ifndef SZA_UTIL_NETMSGHANDLER_H
#define SZA_UTIL_NETMSGHANDLER_H

/**
 * @file NetMsgHandler.h
 * 
 * Tagged: Mon Mar 15 18:24:26 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/NetHandler.h"
#include "carma/szautil/NetMsg.h"

namespace sza {
  namespace util {

    class NetMsg;
    
    class NetMsgHandler : public NetHandler {
    public:
      
      /**
       * Constructor.
       */
      NetMsgHandler();
      
      /**
       * Destructor.
       */
      virtual ~NetMsgHandler();
      
      /**
       * Send a message to a socket described by a previously attached
       * fd.
       */
      sza::util::NetSendStr::NetSendId 
	sendNetMsg(sza::util::NetMsg* msg);

      /**
       * Send a message to a socket
       */
      sza::util::NetSendStr::NetSendId 
	sendNetMsg(int fd, sza::util::NetMsg* msg);

      /**
       * Pack a network message into the network buffer.
       */
      void packNetMsg(sza::util::NetMsg* msg);

      /**
       * Pack a greeting message
       */
      void packGreetingMsg(unsigned int antenna);

      /**
       * Pack an antenna id message
       */
      void packAntennaIdMsg(unsigned int antenna);

      /**
       * Read a net message out of the network buffer
       */
      void readNetMsg();

      /**
       * Return the last message read.
       */
      sza::util::NetMsg* getLastReadNetMsg();
      sza::util::NetMsg* getLastSentNetMsg();

      /**
       * Overload the base-class methods to install user-defined handlers
       */
      void installReadHandler(NET_READ_HANDLER(*handler), void* arg);
      void installSendHandler(NET_SEND_HANDLER(*handler), void* arg);
      void installErrorHandler(NET_ERROR_HANDLER(*handler), void* arg);

    private:

      /**
       * The SZA array map
       */
      ArrayMap* arraymap_;     

      /**
       * The last read message
       */
      sza::util::NetMsg lastReadNetMsg_;
      sza::util::NetMsg lastSentNetMsg_;

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

    }; // End class NetMsgHandler
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_NETMSGHANDLER_H
