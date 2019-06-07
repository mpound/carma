#ifndef SZA_UTIL_SERVER_H
#define SZA_UTIL_SERVER_H

/**
 * @file Server.h
 * 
 * Tagged: Thu 02-Feb-06 17:13:21
 * 
 * @author Erik Leitch
 */
#include <list>
#include <string>
#include <stdio.h>

#include "carma/szautil/FdSet.h"
#include "carma/szautil/NetDat.h"
#include "carma/szautil/NetHandler.h"
#include "carma/szautil/PipeQueue.h"
#include "carma/szautil/Runnable.h"
#include "carma/szautil/SignalTaskMsg.h"
#include "carma/szautil/TimeOut.h"

namespace sza {
  namespace util {
    
    class Port;
    class SignalTask;
    class TcpListener;
    
    class Server : public Runnable {
      
    public:
      
      class ServerData {
      public:
	ServerData();
	virtual ~ServerData();
      };
      
      class ServerConnection {
      public:

	// A queue of messages to be send to this client

	PipeQueue::MsgQueue msgQueue_;

	// A Handler for sending/receiving messages to/from this client

	NetHandler handler_;

	// A temporary buffer for storing data received from this
	// client

	std::vector<unsigned char> bytes_;

	// A handle to our parent

	Server* parent_;

	// Client-specific data

	ServerData* data_;

	// True when this connection has been initialized, whatever
	// that means for a particular type of server

	bool initialized_;

	//------------------------------------------------------------
	// Methods of this class
	//------------------------------------------------------------

	ServerConnection(int fd, 
			 unsigned readBufSize, unsigned sendBufSize, 
			 Server* parent);

	~ServerConnection();

	void setSendBufferSize(unsigned size);
	void setReadBufferSize(unsigned size);

	void packClientData(NetDat& dat);
	void packClientData(unsigned char* buffer, unsigned datSize);
	void stageClientData(unsigned char* buffer, unsigned datSize);
	void checkMsgQueue();
      };
      
      /**
       * Constructor for serial connections
       */
      Server(bool spawnThread, int listenPort, unsigned readBufSize=0, 
	     unsigned sendBufSize=0);
      
      /**
       * Destructor.
       */
      virtual ~Server();
      
      /**
       * Block in select
       */
      virtual void run();

    protected:

      unsigned readBufSize_;

      unsigned sendBufSize_;

      TimeOut timeOut_;

      virtual void serviceSelect();

      // Method called when we time out in our select loop

      virtual void timeOutAction() {};

      // Method called when data have been completely read from a client

      virtual void readClientData(ServerConnection* conn) {};

      // Method called immediately after a client has connected

      virtual void acceptClientAction(ServerConnection* conn) {};
      void sendClientData(NetDat& dat, ServerConnection* client);

      void setReadBufSize(unsigned size);
      void setSendBufSize(unsigned size);

      void setTimeOutSeconds(unsigned int seconds);

    protected:

      /**
       * The set of sockets to be watched
       */
      FdSet fdSet_;

    protected:
      
      unsigned int timeOutSeconds_;

      /**
       * A private thread which will manage signal handling
       */
      SignalTask* signalTask_;
      
      /**
       * True if we should stop
       */
      bool stop_;
      
      /**
       * Object for listening for socket connection requests
       */
      TcpListener* listener_;
      
      /**
       * A vector of network buffers
       */
      std::list<ServerConnection*> clients_;
      
      /**
       * Initialize members
       */
      void initMembers(int listenPort, unsigned readBufSize, unsigned sendBufSize);
      
      /**
       * Set the port number on which we should listen for connection
       * requests.  Also sets the queue length
       */
      void listen(unsigned port, unsigned nClients = 5);
      
      /**
       * Send a shutdown message to connected clients
       */
      void sendShutdownMsg();
      
      /**
       * Accept a client connection
       */
      void acceptConnection();
      
    private:

      /**
       * Check clients for data to be read
       */
      void checkClients();
      
      /**
       * Read data from a client socket
       */
      void readFromClient(NetHandler* client);
      
      /**
       * A shutdown method
       */
      static SIGNALTASK_HANDLER_FN(shutDown);
      
      /**
       * A run mtehod to be called from pthread_start()
       */
      static RUN_FN(runFn);
      
      // Check clients for data to be read

      void checkClientsForReadableData();

      // Check clients for writability

      void checkClientsForWritability();

      static NET_READ_HANDLER(readHandler);
      static NET_SEND_HANDLER(sendHandler);
      static NET_ERROR_HANDLER(errHandler);

    }; // End class Server
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_SERVER_H
