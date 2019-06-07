#ifndef SZA_UTIL_NETBUF_H
#define SZA_UTIL_NETBUF_H

/**
 * @file NetBuf.h
 * 
 * Tagged: Tue Jul 19 00:50:19 PDT 2005
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    class NetBuf {
    public:
      
      /**
       * Enumerate read states
       */
      enum NetReadState {
	NET_READ_PREFIX, // Reading msg prefix
	NET_READ_DATA,   // Reading data
	NET_READ_DONE,   // Read done
	NET_READ_ERROR,  // Error occurred reading
	NET_READ_CLOSED, // Connection closed while reading
      };

      /**
       * Enumerate send states
       */
      enum NetSendState {
	NET_SEND_DATA,   // Sending data
	NET_SEND_DONE,   // Send done
	NET_SEND_ERROR,  // Error occurred sending
	NET_SEND_CLOSED, // Connection closed while sending
      };

      /**
       * Constructor.
       */
      NetBuf();

      /**
       * Constructor with buffer
       */
      NetBuf(unsigned int size, unsigned char* extBuf=0);
      
      /**
       * Constructor with buffer and file descriptor
       */
      NetBuf(int fd, unsigned int size, unsigned char* extBuf=0);

      /**
       * Destructor.
       */
      virtual ~NetBuf();
      
      // Allocate an internal buffer, or set it to point to external
      // memory

      void setBuffer(unsigned int size, unsigned char* extBuf=0);

      // Attach this buffer to a file descriptor

      void attach(int fd);

      // Read data into the buffer

      NetReadState read(int fd=-1);

      // Send data in the buffer

      NetSendState send(int fd=-1);

      // The size of the message prefix

      virtual unsigned int msgPrefixSize();

      // Parse the message prefix, and return the byte count
      
      virtual unsigned int parseMsgPrefix();

      // Write a message prefix

      virtual void putMsgPrefix(unsigned int size);

      //------------------------------------------------------------
      // Utility methods for putting data into the buffer
      //------------------------------------------------------------

      void startPut();

      void endPut();

      virtual void putUchar(unsigned n, unsigned char* ptr);

      virtual void putChar(unsigned n, char* ptr);

      virtual void putUshort(unsigned n, unsigned short* ptr);

      virtual void putShort(unsigned n, short* ptr);

      virtual void putUint(unsigned n, unsigned int* ptr);

      virtual void putInt(unsigned n, int* ptr);

      virtual void putFloat(unsigned n, float* ptr);

      virtual void putDouble(unsigned n, double* ptr);

      static float floatToIeee(float fval);

      static double doubleToIeee(double dval);

      //------------------------------------------------------------
      // Utility methods for extracting data from the buffer
      //------------------------------------------------------------

      void startGet();

      void endGet();

      virtual void getUchar(unsigned n, unsigned char* ptr);

      virtual void getChar(unsigned n, char* ptr);

      virtual void getUshort(unsigned n, unsigned short* ptr);

      virtual void getShort(unsigned n, short* ptr);

      virtual void getUint(unsigned n, unsigned int* ptr);

      virtual void getInt(unsigned n, int* ptr);

      virtual void getFloat(unsigned n, float* ptr);

      virtual void getDouble(unsigned n, double* ptr);

      virtual double netToHost(double netDouble);

      static float ieeeToFloat(unsigned int ieee);

      static double ieeeToDouble(double ieee);

    protected:

      // The file descriptor to which this buffer is attached

      int fd_;

      // Maximum size, in bytes, of a message, exclusive of any message prefix

      unsigned int maxMsgSize_;

      // Length of the message to be sent/read

      unsigned int msgLen_;

      // An internal/external buffer which will be used for
      // reading/writing data

      unsigned char* buffer_;

      // True if the buffer is externally allocated

      bool external_;

      // The number of bytes extracted from the buffer

      unsigned nGet_;

      // The number of bytes put into the buffer

      unsigned nPut_;

      // The last read state

      NetReadState lastReadState_;

      // The last send state

      NetSendState lastSendState_;

      // Private constructor

      void privateConstructor(int fd, unsigned int size, unsigned char* extBuf);

      NetReadState setReadState(NetReadState state);
      NetSendState setSendState(NetSendState state);

    }; // End class NetBuf
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_NETBUF_H
