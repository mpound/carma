#ifndef NETSTR_H
#define NETSTR_H

/**
 * @file NetStr.h
 * 
 * Started: Tue Mar  2 03:37:15 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/NetReadStr.h"
#include "carma/szautil/NetSendStr.h"

namespace sza {
  namespace util {
    
    /**
     * Class for managing a read buffer and a send buffer both
     * attached to the same file descriptor.
     */
    class NetStr {
    public:
      
      /**
       * Constructor.
       */
      NetStr();
      
      /**
       * Constructor.
       */
      NetStr(int fd, unsigned long readSize, unsigned long sendSize);
      
      /**
       * Destructor.
       */
      virtual ~NetStr();
      
      /**
       * Get a reference to our NetReadStr object.
       */
      NetReadStr* getReadStr();
      
      /**
       * Get a reference to our NetSendStr object.
       */
      NetSendStr* getSendStr();
      
      /**
       * Attach our network buffers to a file descriptor.
       */
      void attach(int fd);
      
      /**
       * Attach this network read buffer to a file descriptor.
       */
      void setReadBuffer(unsigned int* buffer, unsigned int size);
      
      /**
       * Attach this network send buffer to a file descriptor.
       */
      void setSendBuffer(unsigned int* buffer, unsigned int size);
      
      /**
       * Return the fd to which we are currently attached.
       */
      int getFd();
      
      /**
       * Send a message
       */
      NetSendStr::NetSendId send();
      
      /**
       * Read a message
       */
      NetReadStr::NetReadId read();
      
    private:
      
      int fd_;
      NetReadStr* netReadStr_;
      NetSendStr* netSendStr_;
      
    }; // End class NetStr
    
  }; // End namespace util
}; // End namespace sza

#endif // End #ifndef 


