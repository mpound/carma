#ifndef SZA_UTIL_TERMINALSERVER_H
#define SZA_UTIL_TERMINALSERVER_H

/**
 * @file TerminalServer.h
 * 
 * Tagged: Mon May 10 15:01:45 PDT 2004
 * 
 * @author Erik Leitch
 */
#include <list>
#include <string>
#include <stdio.h>

#include "carma/szautil/FdSet.h"
#include "carma/szautil/LogFile.h"
#include "carma/szautil/NetTermHandler.h"
#include "carma/szautil/Runnable.h"
#include "carma/szautil/SignalTaskMsg.h"
#include "carma/szautil/Vector.h"

namespace sza {
  namespace util {
    
    class Port;
    class SignalTask;
    class SerialClient;
    class TcpClient;
    class TcpListener;

    class TerminalServer : public Runnable {

    public:
      
      enum {
	CR = '\r',
	LF = '\n',
      };

      /**
       * Constructor for serial connections
       */
      TerminalServer(unsigned baudRate, std::string serialPort, bool spawnThread=false);

      /**
       * Constructor for serial connections
       */
      TerminalServer(std::string ipAddress, unsigned port, bool spawnThread=false);

      /**
       * Destructor.
       */
      virtual ~TerminalServer();
      
      /**
       * Set the port number on which we should listen for connection
       * requests.  Also sets the queue length
       */
      void listen(unsigned port, unsigned nClients = 5);

      /**
       * Set whether or not to register stdin to be listened to
       */
      void listenToStdin(bool listen);

      /**
       * Set whether or not to log server traffic
       */
      void log(bool log);

      /**
       * Set a string of chars to strip from lines read from the port
       */
      void strip(std::string strip);

      /**
       * Set a string of chars never to strip from lines read from the
       * port
       */
      void dontStrip(std::string dontStrip);

      /**
       * Strip unprintable characters from received lines
       */
      void stripUnprintable(bool strip);

      /**
       * Set characters to append to lines written to the port
       */
      void append(std::string append);

      void setLogFilePrefix(const std::string& prefix);

      void setLogFileDirectory(const std::string& dir);

      static RUN_FN(runFn);

      /**
       * Block in select
       */
      void run();

    private:

      bool hammering_;
      bool waitingToHammer_;

      std::list<std::string> hammers_;

      std::list<std::string>::iterator hammerStep_;

      TimeVal timeVal_;

      struct timeval* timeOut_;

      bool pending_;

      /**
       * A private thread which will manage signal handling
       */
      SignalTask* signalTask_;

      /**
       * True if we should stop
       */
      bool stop_;

      /**
       * A string of characters we should strip from lines received
       * from the port.
       */
      std::string strip_;

      /**
       * A string we should append to lines written to the port.
       */
      std::string append_;

      /**
       * Object for listening for socket connection requests
       */
      TcpListener* listener_;

      /**
       * Clients
       */
      TcpClient* tcpipPort_;
      SerialClient* serialPort_;
      Port* port_;

      FILE* file_;

      /**
       * Logfile
       */
      LogFile logFile_;

      /**
       * The set of sockets to be watched
       */
      FdSet fdSet_;
      
      /**
       * True if listening to stdin
       */
      bool listeningToStdin_;

      /**
       * A vector of network buffers
       */
      std::list<NetTermHandler*> clients_;

      /**
       * Initialize members
       */
      void initMembers();

      /**
       * Set up for connecting to a TCP/IP port.
       */
      void setTcpIp(std::string host, unsigned port);

      /**
       * Set up for connecting to a serial port
       */
      void setSerial(unsigned baudRate, std::string portName);

      /**
       * Send a shutdown message to connected clients
       */
      void sendShutdownMsg();

      /**
       * Accept a client connection
       */
      void acceptConnection();

      /**
       * Read data from the port
       */
      void readFromPort();

      /**
       * Read data from stdin
       */
      void readFromStdin();

      /**
       * Write to stdin
       */
      void writeToStdout(std::string& line);

    public:
      /**
       * Write data to the port
       */
      void writeToPort(std::string& line);
      void writeToPort(Vector<unsigned char>& bytes);

    private:
      /**
       * Check clients for data to be read
       */
      void checkClients();

      /**
       * Read data from a client socket
       */
      void readFromClient(NetTermHandler* client);

      /**
       * A shutdown method
       */
      static SIGNALTASK_HANDLER_FN(shutDown);

      /**
       * Initialize the port
       */
      void initPort();

      /**
       * Open a file to download to the port
       */
      void openFile(std::string fileName);
      
      /**
       * Read from a file
       */
      void readFromFile();

      bool parseCommand(std::string line);

      /**
       * Begin a sequence to crack open the datalogger's head
       */
      void initHammer();
      
      /**
       * Called when the datalogger sends a response
       */
      void stepHammer();

    }; // End class TerminalServer
    
  } // End namespace util
} // End namespace sza


#endif // End #ifndef SZA_UTIL_TERMINALSERVER_H
