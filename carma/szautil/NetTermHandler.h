#ifndef SZA_UTIL_NETTERMHANDLER_H
#define SZA_UTIL_NETTERMHANDLER_H

/**
 * @file NetTermHandler.h
 * 
 * Tagged: Tue May 11 14:38:31 PDT 2004
 * 
 * @author Erik Leitch
 */
#include <string>
#include "carma/szautil/NetHandler.h"

namespace sza {
  namespace util {
    
    /**
     * A handler for network messages between terminal servers and
     * clients
     */
    class NetTermHandler : public NetHandler {
    public:
      
      enum MsgType {
	NONE,       // An initializer
	LINE,       // A line of text has been sent
	SHUTDOWN,   // A message from the server that it is shutting
		    // down
	DISCONNECT, // A message from a client that it is
		    // disconnecting
      };

      /**
       * Constructor.
       */
      NetTermHandler();
      
      /**
       * Constructor.
       */
      NetTermHandler(int fd);

      /**
       * Destructor.
       */
      virtual ~NetTermHandler();
      
      /**
       * Return the last message type read into the network buffer
       */
      MsgType getLastMsgType();

      /**
       * Return a copy of the last line read into the network buffer.
       */
      std::string getLastLine();

      /**
       * Pack a line
       */
      void sendLine(std::string& line);

    private:

      static const unsigned LINE_LEN;
      static const unsigned BUF_SIZE;

      MsgType lastMsgType_;

      std::string lastLine_;

      /**
       * A handler to be called when a command has been completely read.
       */
      static NET_READ_HANDLER(readMsg);

    }; // End class NetTermHandler
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_NETTERMHANDLER_H
