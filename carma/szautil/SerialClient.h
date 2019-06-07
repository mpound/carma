#ifndef SZA_UTIL_SERIALCLIENT_H
#define SZA_UTIL_SERIALCLIENT_H

/**
 * @file SerialClient.h
 * 
 * Tagged: Mon May 10 15:32:08 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Port.h"

#include <termios.h>

namespace sza {
  namespace util {
    
    class SerialClient : public Port {
    public:
      
      /**
       * Constructor.
       */
      SerialClient(std::string port="/dev/ttyS0", int baudRate=9600, 
		   bool canonical=false, bool sevenBit=false);
      
      /**
       * Destructor.
       */
      virtual ~SerialClient();
      
      /**
       * Set the port
       */
      virtual void setPort(std::string port); 

      /**
       * Set the baud rate
       */
      void setBaudRate(int baudRate);

      /**
       * Open a connection to the serial port
       */
      virtual int connect();
      int connectWx();

      /**
       * Terminate the serial port connection to the TERM
       */
      void disconnect();

      std::string portName();

      void setFd(int fd);

    private:

      //------------------------------------------------------------
      // Static arrays
      //------------------------------------------------------------
      
      bool ownFd_;

      /**
       * Define a struct for encapsulating a baud rate
       */
      struct BaudRate {
	speed_t speed;
	int baudRate;
      };
      
      /**
       * A static array of baudrate enumerator-speed associations
       */
      static BaudRate baudRates_[];

      /**
       * The number of elements in the above array
       */
      static unsigned nBaudRates_;

    protected:
      /**
       * The serial port
       */
      std::string portName_;

    private:
      /**
       * The baud rate
       */
      BaudRate* baudRate_;

      /**
       * Canonical input character processing. Buffer until newline read.
       */
      bool canonical_;
      
      /**
       * Seven bit characters on input
       */
      bool sevenBit_;
      
    }; // End class SerialClient
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_SERIALCLIENT_H
