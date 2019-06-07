#ifndef SZA_UTIL_NETHANDLER_H
#define SZA_UTIL_NETHANDLER_H

/**
 * @file NetHandler.h
 * 
 * Tagged: Sun Apr  4 22:36:40 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/NetStr.h"

namespace sza {
  namespace util {
    
    class NetHandler {
    public:
      
      /**
       * Constructor.
       */
      NetHandler();
      
      /**
       * Constructor.
       */
      NetHandler(int fd);

      /**
       * Destructor.
       */
      virtual ~NetHandler();
      
      /**
       * Attach the network I/O stream to a socket.
       */
      void attachReadStream(int fd);

      /**
       * Attach the network I/O stream to a socket.
       */
      void attachSendStream(int fd);

      /**
       * Attach all streams to the same socket.
       */
      void attach(int fd);

      /**
       * Overwritable method for returning the read fd.
       */
      virtual int getReadFd();

      /**
       * Overwritable method for returning the send fd.
       */
      virtual int getSendFd();

      /**
       * Method for returning a single fd.  This will be -1 until the
       * attach() method is used to attach all streams.
       */
      virtual int getFd();

      /**
       * Set the network buffer pointing to an external buffer, or
       * pass buffer=NULL to dynamically allocate it.
       */
      void setReadBuffer(void* buffer, unsigned int size);
      

      /**
       * Set the network buffer pointing to an external buffer, or
       * pass buffer=NULL to dynamically allocate it.
       */
      void setSendBuffer(void* buffer, unsigned int size);

      /**
       * Send a message packed into our network buffer to a socket
       * described by a previously attached fd.
       */
      sza::util::NetSendStr::NetSendId send();
      
      /**
       * Send a message packed into our network buffer to the
       * specified socket.  
       */
      sza::util::NetSendStr::NetSendId send(int fd);

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
       * Return a pointer to our read stream
       */
      inline NetReadStr* getReadStr() {
	return nrs_;
      }

      /**
       * Return a pointer to our read stream
       */
      inline NetSendStr* getSendStr() {
	return nss_;
      }

      /**
       * Methods to install user-defined handlers
       */
      virtual void installReadHandler(NET_READ_HANDLER(*handler), void* arg);
      virtual void installReadErrorHandler(NET_ERROR_HANDLER(*handler), 
					   void* arg);
      virtual void installSendHandler(NET_SEND_HANDLER(*handler), void* arg);
      virtual void installSendErrorHandler(NET_ERROR_HANDLER(*handler), 
					   void* arg);
      virtual void installErrorHandler(NET_ERROR_HANDLER(*handler), 
				       void* arg);

    protected:

      int fd_;
      NetReadStr* nrs_;
      NetSendStr* nss_;

    private:

      void initialize();

    }; // End class NetHandler
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_NETHANDLER_H
