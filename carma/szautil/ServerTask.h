#ifndef SZA_UTIL_SERVERTASK_H
#define SZA_UTIL_SERVERTASK_H

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
#include "carma/szautil/SignalTask.h"
#include "carma/szautil/SignalTaskMsg.h"
#include "carma/szautil/SpawnableTask.h"
#include "carma/szautil/TcpListener.h"
#include "carma/szautil/TimeOut.h"

namespace sza {
  namespace util {
    
    class Port;
    class SignalTask;
    class TcpListener;
    
    template <class Msg>
      class ServerTask : public sza::util::SpawnableTask<Msg> {
    public:
      
      //=======================================================================
      // Class for managing client data for this server
      //=======================================================================
      
      class ServerData {
      public:
	ServerData() {};
	virtual ~ServerData() {};
      };
      
      //=======================================================================
      // Class for managing client-specific connections
      //=======================================================================

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

	ServerTask<Msg>* parent_;

	// Client-specific data

	ServerData* data_;

	// True when this connection has been initialized, whatever
	// that means for a particular type of server

	bool initialized_;

	// True when a send is pending

	bool sendPending_;

	//------------------------------------------------------------
	// Methods of this class
	//------------------------------------------------------------

	ServerConnection(int fd, 
			 unsigned readBufSize, unsigned sendBufSize, 
			 ServerTask* parent) {
	  handler_.attach(fd);
	  handler_.setReadBuffer(0, readBufSize);
	  handler_.setSendBuffer(0, sendBufSize);
	  parent_ = parent;
	  initialized_ = false;
	  sendPending_ = false;
	  
	  handler_.installReadHandler(ServerTask::readHandler, (void*)this);
	  handler_.installSendHandler(ServerTask::sendHandler, (void*)this);
	  
	  handler_.installReadErrorHandler(ServerTask::errHandler, (void*)this);
	  handler_.installSendErrorHandler(ServerTask::errHandler, (void*)this);
	  
	  data_ = 0;
	};

	//------------------------------------------------------------
	// Destructor for ServerConnection class
	//------------------------------------------------------------

	~ServerConnection() {

	  // If any handlers were attached, detach them now
	  
	  if(handler_.getFd() > 0) {
	    close(handler_.getFd());
	    
	    handler_.attach(-1);
	    
	    ::close(handler_.getFd());
	  }
	  
	  // Delete any data associated with this connection too
	  
	  if(data_) {
	    delete data_;
	    data_ = 0;
	  }
	};

	void setSendBufferSize(unsigned size) {
	  handler_.setSendBuffer(0, size);
	};

	void setReadBufferSize(unsigned size) {
	  handler_.setReadBuffer(0, size);
	};

	//------------------------------------------------------------
	// Private method to pack data intended for a client
	//------------------------------------------------------------

	void packClientData(NetDat& dat) {
	  std::vector<unsigned char>& data = dat.getSerializedData();
	  unsigned datSize = data.size();
	  
	  packClientData(&data[0], datSize);
	};

	//------------------------------------------------------------
	// Private method to pack data intended for a client.  If our
	// message queue for this client is currently empty, stage it
	// directly into the network handler, else push it onto the
	// message queue for later sending.
	//------------------------------------------------------------

	void packClientData(unsigned char* buffer, unsigned nbyte) {
	  if(!sendPending_) {
	    stageClientData(buffer, nbyte);
	  } else {
	    msgQueue_.push(buffer, nbyte);
	  }
	};

	//------------------------------------------------------------
	// Private method to pack data intended for a client
	//------------------------------------------------------------
	
	void stageClientData(unsigned char* buffer, unsigned datSize) {

	  // Resize the send buffer if this object is larger than the
	  // current send buffer size
	  
	  if(datSize+8 > parent_->sendBufSize_) {
	    parent_->setSendBufSize(datSize + 8);
	    setSendBufferSize(datSize + 8);
	  }
	  
	  // Now send the data
	  
	  handler_.getSendStr()->startPut(datSize);
	  handler_.getSendStr()->putChar(datSize, buffer);
	  handler_.getSendStr()->endPut();
	  
	  // Set our pending flag to true

	  sendPending_ = true;

	  // And register this client fd to be watched for writability
	  
	  parent_->GenericTask<Msg>::fdSet_.registerWriteFd(handler_.getSendFd());
	};

	//------------------------------------------------------------
	// Check this client's message queue for pending messages.  If
	// the queue is now empty, stage the next message to be sent
	//------------------------------------------------------------

	void checkMsgQueue() {
	  if(!msgQueue_.empty()) {
	    PipeQueue::QueueNode& node = msgQueue_.front();
	    stageClientData(node.buffer_, node.nbyte_);
	    msgQueue_.pop();
	  }
	};

      };
      
      //=======================================================================
      // Methods of ServerTask class
      //=======================================================================

      /**
       * A vector of network buffers
       */
      std::list<ServerConnection*> clients_;

      //------------------------------------------------------------
      // Constructor for serial connections
      //------------------------------------------------------------
      
      ServerTask(bool spawnThread, int listenPort, unsigned readBufSize=0, 
		 unsigned sendBufSize=0) : SpawnableTask<Msg>(spawnThread) 
      {
	initMembers(listenPort, readBufSize, sendBufSize);
      };
	
      //------------------------------------------------------------
      // Destructor
      //------------------------------------------------------------
	
      virtual ~ServerTask() {
        // Shut down the signal task
       
	if(signalTask_ != 0) {
	  delete signalTask_;
	  signalTask_ = 0;
	}
	
	// Free any memory allocated in this class
	

	typename std::list<ServerConnection*>::iterator iClient;
	for(iClient=clients_.begin(); iClient != clients_.end(); iClient++) {
	  delete *iClient;
	}
	
	// Free the listener
	
	if(listener_ != 0) {
	  delete listener_;
	  listener_ = 0;
	}
      };
      
      //------------------------------------------------------------
      // Process task messages
      //------------------------------------------------------------

      virtual void processTaskMsg(bool* stop) {
	GenericTask<Msg>::processTaskMsg(stop);
      };

      //------------------------------------------------------------
      // Block in select
      //------------------------------------------------------------

     virtual void serviceMsgQ() {
       int nready=0;
       
       // On entry to the loop, timeout immediately
       
       timeOut_.setIntervalInSeconds(0);
       timeOut_.activate(true);
       
       do {
	 
	 // Block in select() until one or more file descriptors are readable
	 
	 if((nready=select(GenericTask<Msg>::fdSet_.size(), 
			   GenericTask<Msg>::fdSet_.readFdSet(), 
			   GenericTask<Msg>::fdSet_.writeFdSet(), 
			   NULL, 
			   timeOut_.tVal())) < 0) {
	   ThrowSysError("select()");
	 }
	 
	 if(nready > 0) {
	   
	   // Service a select()able event
	   
	   serviceSelect();

	 } else {
	   
	   // Do whatever it is we are supposed to do on timeout
	   
	   timeOutAction();
	   
	   // And reset the timeout
	   
	   timeOut_.setIntervalInSeconds(timeOutSeconds_);
	   timeOut_.reset();
	 }
	 
       } while(!stop_);
       
     };

    protected:

      unsigned readBufSize_;

      unsigned sendBufSize_;

      TimeOut timeOut_;

      //------------------------------------------------------------
      // Service a select()able event
      //------------------------------------------------------------

      virtual void serviceSelect() {

	// Check our message queue for task messages

	if(GenericTask<Msg>::fdSet_.isSetInRead(GenericTask<Msg>::msgq_.fd())) {
	  processTaskMsg(&stop_);
	}

	// Service requests received over the socket connection
	
	if(listener_ != 0 && GenericTask<Msg>::fdSet_.isSetInRead(listener_->getFd()))
	  acceptConnection();
	
	// Check connected clients for data
	
	checkClientsForReadableData();
	
	// Check connected clients for sendable data
	
	checkClientsForWritability();
      };
      
      //------------------------------------------------------------
      // Method called when we time out in our select loop
      //------------------------------------------------------------

      virtual void timeOutAction() {};

      //------------------------------------------------------------
      // Method called when data have been completely read from a client
      //------------------------------------------------------------

      virtual void readClientData(ServerConnection* conn) {};

      //------------------------------------------------------------
      // Method called immediately after a client has connected
      //------------------------------------------------------------

      virtual void acceptClientAction(ServerConnection* conn) {};

      //------------------------------------------------------------
      // Method to send data to all connected clients
      //------------------------------------------------------------

      void sendClientData(NetDat& dat, ServerConnection* client) {

	// If a client was specified, send only to that client.  
	
	if(client) {
	  client->packClientData(dat);
	  
	  // Else send to all clients, but only clients that are
	  // initialized.  This is to prevent a server from packing data to
	  // all clients that may overwrite the initialization data that a
	  // client is waiting for
	  
	} else {
	  typename std::list<ServerConnection*>::iterator iClient;
	  for(iClient=clients_.begin(); iClient != clients_.end(); iClient++) {
	    ServerConnection* client = *iClient;
	    
	    // Only send to this client if the client is initialized
	    
	    if(client->initialized_)
	      client->packClientData(dat);
	  }
	}
      };

      void setReadBufSize(unsigned size) {
	readBufSize_ = size;
      };

      void setSendBufSize(unsigned size) {
	sendBufSize_ = size;
      };

      void setTimeOutSeconds(unsigned int seconds) {
	timeOutSeconds_ = seconds;
      };

      // The timeout for our select loop

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
      
      //------------------------------------------------------------
      // Initialize members
      //------------------------------------------------------------

      void initMembers(int listenPort, unsigned readBufSize, unsigned sendBufSize) {
	listener_    =  0;
	signalTask_  =  0;
	stop_        =  false;
	readBufSize_ = readBufSize;
	sendBufSize_ = sendBufSize;
	
	// We will set the timeout to 1 second for our select() loop
	
	setTimeOutSeconds(1);
	
	// Finally, spawn a thread for managing signal handling
	
	signalTask_ = new SignalTask(true);
	
	// And install a signal handler for SIGINT
	
	signalTask_->sendInstallSignalMsg(SIGINT, &shutDown, this);
	
	// Start listening on the requested port
	
	listen(listenPort);
      };
      
      //------------------------------------------------------------
      // Set the port number on which we should listen for connection
      // requests.  Also sets the queue length
      //------------------------------------------------------------

      void listen(unsigned port, unsigned nClients = 5) {
	if(nClients > 0) {
	  listener_ = new TcpListener(port, nClients);
	  GenericTask<Msg>::fdSet_.registerReadFd(listener_->getFd());
	}
      };

      //------------------------------------------------------------
      // Accept a client connection
      //------------------------------------------------------------

      void acceptConnection() {
	int fd = -1;
	
	// Allow the caller to connect.  The fd returned will be configured
	// for blocking I/O.
	
	fd = listener_->acceptConnection(true);
	
	// Insert a new connection into the list
	
	ServerConnection* conn = new ServerConnection(fd, readBufSize_, sendBufSize_, this);
	
	// And register the descriptor to be watched for input
	
	GenericTask<Msg>::fdSet_.registerReadFd(fd);
	
	// Do anything inheritors define when a client connects
	
	CTOUT("About to call acceptClientAction with conn = " << conn);
	acceptClientAction(conn);
	
	// Only after acceptClientAction is done should we attempt to insert
	// the client in the list
	
	clients_.insert(clients_.begin(), conn);
      };

      
    private:

      //------------------------------------------------------------
      // A shutdown method
      //------------------------------------------------------------

      static SIGNALTASK_HANDLER_FN(shutDown) {
	ServerTask* server = (ServerTask*) args;
	server->sendStopMsg();
      };

      //------------------------------------------------------------
      // Check clients for data to be read
      //------------------------------------------------------------

      void checkClientsForReadableData() {
	std::vector<ServerConnection*> disconnectedClients_;

	typename std::list<ServerConnection*>::iterator iClient;
	for(iClient=clients_.begin(); iClient != clients_.end(); iClient++) {

	  ServerConnection* client = *iClient;
	  
	  if(GenericTask<Msg>::fdSet_.isSetInRead(client->handler_.getReadFd())) {
	    
	    client->handler_.read();
	    
	    // If after processing messages from this client, the client is
	    // disconnected, mark it for removal
	    
	    if(client->handler_.getReadFd() < 0)
	      disconnectedClients_.push_back(client);
	  }
	}
	
	// Finally, remove any clients that were disconnected after reading
	
	typename std::vector<ServerConnection*>::iterator iDisClient;
	for(iDisClient=disconnectedClients_.begin(); iDisClient != disconnectedClients_.end(); iDisClient++) {
	  ServerConnection* client = *iDisClient;
	  clients_.remove(client);
	  delete client;
	}
      };

      //------------------------------------------------------------
      // Check clients for writability
      //------------------------------------------------------------

      void checkClientsForWritability() {
	typename std::list<ServerConnection*>::iterator iClient;
	for(iClient=clients_.begin(); iClient != clients_.end(); iClient++) 
	{
	  ServerConnection* client = *iClient;
	  
	  if(GenericTask<Msg>::fdSet_.isSetInWrite(client->handler_.getSendFd()))
	    client->handler_.send();
	}
      };

      //------------------------------------------------------------
      // Static method to be called when a message is fully read froym
      // a client
      //------------------------------------------------------------

      static NET_READ_HANDLER(readHandler) {
	ServerConnection* conn = (ServerConnection*)arg;
	conn->parent_->readClientData(conn);
      };

      //------------------------------------------------------------      
      // Static method to be called when a message is fully sent to a
      // client
      //------------------------------------------------------------      

      static NET_SEND_HANDLER(sendHandler) {
	ServerConnection* conn = (ServerConnection*)arg;
	conn->parent_->GenericTask<Msg>::fdSet_.clearFromWriteFdSet(conn->handler_.getFd());
	
	// Only after the first send is completed can we consider the client
	// initialized
	
	conn->initialized_ = true;

	// Set our pending flag to false

	conn->sendPending_ = false;

	// Check msg queue for other queued messages
	
	conn->checkMsgQueue();
      };

      //------------------------------------------------------------
      // Static method to be called when a message is fully read from
      // a client
      //------------------------------------------------------------

      static NET_ERROR_HANDLER(errHandler) {
	ServerConnection* conn = (ServerConnection*)arg;
	conn->parent_->GenericTask<Msg>::fdSet_.clearFromReadFdSet(conn->handler_.getReadFd());
	conn->parent_->GenericTask<Msg>::fdSet_.clearFromWriteFdSet(conn->handler_.getSendFd());
	conn->handler_.attach(-1);

	// Set our pending flag to false

	conn->sendPending_ = false;
      };
      
    }; // End class Server
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_SERVERTASK_H
