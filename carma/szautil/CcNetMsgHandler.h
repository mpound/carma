#ifndef SZA_UTIL_CCNETMSGHANDLER_H
#define SZA_UTIL_CCNETMSGHANDLER_H

/**
 * @file CcCcNetMsgHandler.h
 * 
 * Tagged: Tue Mar 23 15:43:10 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/CcNetMsg.h"
#include "carma/szautil/NetReadStr.h"
#include "carma/szautil/NetSendStr.h"

namespace sza {
  namespace util {

    class CcNetMsg;
    
    class CcNetMsgHandler {
    public:
      
      /**
       * Constructor.
       */
      CcNetMsgHandler();
      
      /**
       * Destructor.
       */
      virtual ~CcNetMsgHandler();
      
      /**
       * Attach the network I/O stream to a socket.
       */
      void attachReadStream(int fd);

      /**
       * Attach the network I/O stream to a socket.
       */
      void attachSendStream(int fd);

      /**
       * Send a message packed into our network buffer to a socket
       * described by a previously attached fd.
       */
      sza::util::NetSendStr::NetSendId send();
      
      /**
       * Send a message to a socket described by a previously attached
       * fd.
       */
      sza::util::NetSendStr::NetSendId 
	send(sza::util::CcNetMsg* msg);

      /**
       * Send a message packed into our network buffer to the
       * specified socket.  
       */
      sza::util::NetSendStr::NetSendId send(int fd);

      /**
       * Send a message to a socket
       */
      sza::util::NetSendStr::NetSendId 
	send(int fd, sza::util::CcNetMsg* msg);

      /**
       * Read a message into our network buffer from a socket
       * described by a previously attached fd.
       */
      sza::util::NetReadStr::NetReadId read();
      
      /**
       * Read a message into our network buffer from the specified
       * socket.
       */
      sza::util::NetReadStr::NetReadId read(int fd);

      /**
       * Pack a network message.
       */
      void packCcNetMsg(sza::util::CcNetMsg* msg);

      /**
       * Return the last message read.
       */
      sza::util::CcNetMsg* getCcNetMsg();

    private:

      sza::util::CcNetMsg netMsg_;
      sza::util::NetSendStr* nss_;
      sza::util::NetReadStr* nrs_;

    }; // End class CcNetMsgHandler
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_CCNETMSGHANDLER_H
