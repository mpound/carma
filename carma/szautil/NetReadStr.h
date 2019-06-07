#ifndef NETREADSTR_H
#define NETREADSTR_H

/**
 * @file NetReadStr.h
 * 
 * Started: Sun Feb 29 15:29:37 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szaarrayutils/netbuf.h"
#include "carma/szaarrayutils/netobj.h"

#define NET_READ_HANDLER(fn) void (fn)(void* arg)

#ifdef NET_ERROR_HANDLER
#undef NET_ERROR_HANDLER
#endif

#define NET_ERROR_HANDLER(fn) void (fn)(void* arg)

namespace sza {
  namespace util {
    
    class NetReadStr {
    public:
      
      enum NetReadId {   
	
	// The message byte-count is being read 
	
	NET_READ_SIZE   = sza::array::NetReadStr::NET_READ_SIZE,
	
	// The message body is being read 
	
	NET_READ_DATA   = sza::array::NetReadStr::NET_READ_DATA,
	
	// The message has been completely read 
	
	NET_READ_DONE   = sza::array::NetReadStr::NET_READ_DONE,
	
	// Closed connection detected at start of message 
	
	NET_READ_CLOSED = sza::array::NetReadStr::NET_READ_CLOSED,
	
	// I/O error detected while reading 
	
	NET_READ_ERROR  = sza::array::NetReadStr::NET_READ_ERROR
      };
      
      /**
       * Constructor.
       */
      NetReadStr();
      
      /**
       * Constructor with file descriptor and size.
       */
      NetReadStr(int fd, unsigned long size);
      
      /**
       * Destructor.
       */
      virtual ~NetReadStr();
      
      /**
       * Attach this network buffer to a file descriptor.
       */
      void attach(int fd);
      
      /**
       * Set the network buffer pointing to an external buffer, or
       * pass buffer=NULL to dynamically allocate it.
       */
      void setBuffer(void* buffer, unsigned int size);
      
      /**
       * Read a message into our network buffer from a socket
       * described by a previously attached fd.
       */
      NetReadId read();
      
      /**
       * Read a message into our network buffer from the
       * specified socket.  
       */
      NetReadId read(int fd);
      
      /**
       * Return the last read state
       */
      NetReadId state();
      
      /**
       * Return the file descriptor to which we're currently
       * attached.
       */
      int getFd();
      
      /**
       * Start unpacking a message from a network buffer.
       */
      void startGet(int *opcode);
      
      /**
       * Finish unpacking a message from a network buffer.
       */
      void endGet();
      
      /**
       * Get a char from a network buffer.
       */
      void getChar(long ndata, unsigned char *data);
      
      /**
       * Get a short from a network buffer.
       */
      void getShort(long ndata, unsigned short *data);
      
      /**
       * Get a long from a network buffer.
       */
      void getLong(long ndata, unsigned long *data);
      
      /**
       * Get a float from a network buffer.
       */
      void getFloat(long ndata, float *data);
      
      /**
       * Get a double from a network buffer.
       */
      void getDouble(long ndata, double *data);
      
      /**
       * Read an object from a network buffer.
       */
      void getObj(const NetObjTable* types, int id, void *obj);
      
      /**
       * Incrementally read bytes from a network buffer.
       */
      void incNget(long nbytes);
      
      /**
       * Install a read handler.  
       */
      void installReadHandler(NET_READ_HANDLER(*handler), void* arg);

      /**
       * Install an error handler.  
       */
      void installErrorHandler(NET_ERROR_HANDLER(*handler), void* arg);

    private:
      
      /**
       * The substance of any constructor for this class.
       */
      void privateConstructor(int fd, unsigned long size);
      
      /**
       * The internal NetBuf object.
       */
      sza::array::NetReadStr* netStream_;
      
      /**
       * True when the network buffer has been allocated.
       */
      bool netBufAllocated_;
      
      /**
       * True when we are attached to a file descriptor.
       */
      bool attached_;
      
      /**
       * Read a message into our network buffer from the
       * specified socket.  
       */
      NetReadId privateRead(int fd);
      
      /**
       * Return the last read state
       */
      NetReadId privateState(sza::array::NetReadStr::NetReadId id);

      // Handler functions

      /**
       * A handler to be called when a read has finished
       */
      NET_READ_HANDLER(*readHandler_);

      /**
       * The argument to pass to the read handler.
       */
      void* readArg_;

      /**
       * A handler to be called if an error occurs while reading
       */
      NET_READ_HANDLER(*errorHandler_);

      /**
       * The argument to pass to the error handler.
       */
      void* errorArg_;

    }; // End class NetReadStr
    
  }; // End namespace util
}; // End namespace sza

#endif // End #ifndef 


