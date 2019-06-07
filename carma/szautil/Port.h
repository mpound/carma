#ifndef SZA_UTIL_PORT_H
#define SZA_UTIL_PORT_H

/**
 * @file Port.h
 * 
 * Tagged: Mon May 10 16:41:20 PDT 2004
 * 
 * @author Erik Leitch
 */
#include <string>
#include "carma/szautil/String.h"
#include "carma/szautil/Vector.h"

namespace sza {
  namespace util {
    
    class Port {
    public:

      static const unsigned int MAX_RCV_BUFFER = 300;
      
      // The default telnet port number

      static const unsigned TELNET_PORT_NO = 23;

      /**
       * Constructor
       */
      Port(int fd = -1);

      /**
       * Destructor
       */
      virtual ~Port();

      /**
       * Return the file descriptor associated with our port connection
       */
      inline int getFd() {
	return fd_;
      }

      void setFd(int fd) {
	fd_ = fd;
      }

      /**
       * Connect to the port
       */
      virtual int connect() 
      {
	return 0;
      };

      /**
       * Write a message to the port
       */
      void writeString(std::string& message, int fd=-1);
      void writeBytes(Vector<unsigned char>& buffer);
      static void writeBytes(Vector<unsigned char>& buffer, int fd);

      /**  
       * Read a message from the port
       */
      unsigned int readBytes(unsigned char *message, int fd=-1);
      unsigned int readBytes(Vector<unsigned char>& buffer);
      
      // Read tne next byte from the serial port 
      
      unsigned char getNextByte();

      /**  
       * Read a message from the port
       */
      std::string readString(int fd=-1);

      bool concatenateString(std::ostringstream& os, int fd=-1, bool cont=true);
      void concatenateChar(std::ostringstream& os, int fd=-1);
      int getNbyte(int fd=-1);

      /**
       * terminate a read when any of the following characters are
       * read from the port
       */
      void terminateAt(std::string strip);

      /**
       * Strip any of the following characters from data read from the
       * port
       */
      void strip(std::string strip);

      /**
       * Characters we mustn't strip.  Note that this will override
       * duplicate characters implied by stripUnprintable()
       */
      void dontStrip(std::string strip);

      /**
       * Strip any unprintable characters
       */
      void stripUnprintable(bool strip);

      /**
       * Append the passed string to the end of each line
       */
      void append(std::string append);

      void appendNewline();
      void appendLinefeed();
      void appendNull();
      void appendNothing();

      /**
       * Set no buffering for this stream
       */
      void setNoBuf();

      /**
       * Set line buffering for this stream
       */
      void setLineBuf();

      void print(unsigned char);

    protected:

      int fd_;
      
    private:
      
      std::vector<unsigned char> appendVec_;

      String termStr_;
      String stripStr_;
      String dontStripStr_;
      String appendStr_;
      bool stripUnprintable_;
    };
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_PORT_H
