#ifndef SZA_UTIL_CLIENTTASK_H
#define SZA_UTIL_CLIENTTASK_H

/**
 * @file ClientTask.h
 * 
 * Tagged: Wed 01-Feb-06 10:43:27
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Exception.h"
#include "carma/szautil/FdSet.h"
#include "carma/szautil/NetDat.h"
#include "carma/szautil/NetHandler.h"
#include "carma/szautil/Runnable.h"
#include "carma/szautil/SpawnableTask.h"
#include "carma/szautil/TcpClient.h"
#include "carma/szautil/TimeVal.h"

namespace sza {
  namespace util {
    
    template <class Msg>
      class ClientTask : public sza::util::SpawnableTask<Msg> {
    public:
      
      /**
       * Constructor.
       */
      ClientTask(bool spawn, std::string host, unsigned connectPort, 
		 unsigned readBufSize=0, unsigned sendBufSize=0) :
	SpawnableTask<Msg>(spawn) {
	initMembers(host, connectPort, readBufSize, sendBufSize);
      };

      	/**
	 * Destructor.
	 */
	virtual ~ClientTask() {
	  disconnect();
	};
	
	//-----------------------------------------------------------------------
	// Initialization method
	//-----------------------------------------------------------------------

	void initMembers(std::string host, unsigned port, 
			 unsigned readBufSize, unsigned sendBufSize) {
	  tcp_.setHost(host);
	  tcp_.setPort(port);
	  
	  handler_.installReadHandler(readHandler, (void*)this);
	  handler_.installSendHandler(sendHandler, (void*)this);
	  
	  handler_.installReadErrorHandler(errHandler, (void*)this);
	  handler_.installSendErrorHandler(errHandler, (void*)this);
	  
	  stop_ = false;
	  
	  timeOut_.setSeconds(0);
	  timeOutPtr_ = timeOut_.timeVal();
	};

	//-----------------------------------------------------------------------
	// Disconnect from server
	//-----------------------------------------------------------------------

	void disconnect() {

	  tcp_.disconnect();

	  GenericTask<Msg>::fdSet_.clearFromReadFdSet(handler_.getReadFd());
	  GenericTask<Msg>::fdSet_.clearFromWriteFdSet(handler_.getSendFd());
	  
	  handler_.attach(-1);
	  
	  // And set a timer to reconnect!
	  
	  timeOut_.setSeconds(1);
	  timeOutPtr_ = timeOut_.timeVal();
	};

	//-----------------------------------------------------------------------
	// Connect to server
	//-----------------------------------------------------------------------

	bool connect() {
	  if(tcp_.connectToServer(true) > 0) {
	    handler_.attach(tcp_.getFd());
	    GenericTask<Msg>::fdSet_.registerReadFd(handler_.getReadFd());
	    timeOutPtr_ = NULL;
	    return true;
	  } else {
	    timeOut_.setSeconds(1);
	    timeOut_.reset();
	    return false;
	  }
	};

	//-----------------------------------------------------------------------
	// Send data to the server
	//-----------------------------------------------------------------------

	void sendServerData(NetDat& dat) {
	  std::vector<unsigned char>& data = dat.getSerializedData();
	  unsigned datSize = data.size();
	  
	  // Resize the send buffer if this object is larger than the
	  // current send buffer size
	  
	  if(datSize+8 > sendBufSize_)
	    setSendBufSize(datSize + 8);
	  
	  handler_.getSendStr()->startPut(data.size());
	  handler_.getSendStr()->putChar(data.size(), &data[0]);
	  handler_.getSendStr()->endPut();
	  
	  // And register this client fd to be watched for writability
	  
	  GenericTask<Msg>::fdSet_.registerWriteFd(handler_.getSendFd());
	};

	//-----------------------------------------------------------------------
	// Static method to be called when a message is fully read
	// from the server
	//-----------------------------------------------------------------------

	static NET_READ_HANDLER(readHandler) {
	  ClientTask* client = (ClientTask*)arg;
	  client->readServerData(client->handler_);
	};

	//-----------------------------------------------------------------------
	// Static method to be called when a message is fully sent to
	// the server
	//-----------------------------------------------------------------------

	static NET_SEND_HANDLER(sendHandler) {
	  ClientTask* client = (ClientTask*)arg;
	  client->fdSet_.clearFromWriteFdSet(client->handler_.getSendFd());
	};

	//-----------------------------------------------------------------------
	// Static method to be called when an error occurs reading or
	// sending data
	//-----------------------------------------------------------------------

	static NET_ERROR_HANDLER(errHandler) {
	  ClientTask* client = (ClientTask*)arg;
	  client->disconnect();
	};

	//-----------------------------------------------------------------------
	// Block in select
	//-----------------------------------------------------------------------
	
	virtual void serviceMsgQ() {

	  int nready=0;
	  
	  do {
	    
	    // Block in select() until one or more file descriptors are readable
	    
	    if((nready=select(GenericTask<Msg>::fdSet_.size(), 
			      GenericTask<Msg>::fdSet_.readFdSet(), 
			      GenericTask<Msg>::fdSet_.writeFdSet(), 
			      NULL, timeOutPtr_)) < 0) 
	    {
	      ThrowSysError("select()");
	    }
	    
	    if(nready > 0) {
	      
	      // read data received over the socket connection
	      
	      if(GenericTask<Msg>::fdSet_.isSetInRead(handler_.getReadFd()))
		handler_.read();
	      
	      // send data over the socket connection
	      
	      if(GenericTask<Msg>::fdSet_.isSetInWrite(handler_.getSendFd()))
		handler_.send();
	      
	      // Else check our message queue for task messages

	      if(GenericTask<Msg>::fdSet_.isSetInRead(GenericTask<Msg>::msgq_.fd())) {
		processTaskMsg(&stop_);
	      }

	    } else 
	      connect();
	    
	  } while(!stop_);
	};
	
	void setReadBufSize(unsigned size) {
	  handler_.setReadBuffer(0, size);
	  readBufSize_ = size;
	};

	void setSendBufSize(unsigned size) {
	  handler_.setSendBuffer(0, size);
	  sendBufSize_ = size;
	};

	void readServerData(NetHandler& handler) {
	  int size;

	  handler.getReadStr()->startGet(&size);

	  // Changing this now to allow for variable network object sizes.
	  // The network buffer can change size if a message larger than the
	  // previously allocated buffer size is encountered, therefore our
	  // byte array will be resized accordingly.  
	  //
	  // Note that the network buffer only changes size to accomodate
	  // larger messages, so that we do not reallocate just because a
	  // shorter message was encountered.
	  
	  if(size > bytes_.size()) {
	    bytes_.resize(size);
	  }
	  
	  handler.getReadStr()->getChar(size, &bytes_[0]);
	  handler.getReadStr()->endGet();
	  
	  sizeInBytesOfLastMessage_ = size;
	  
	  processServerData();
	};

	virtual void processTaskMsg(bool* stop) {
	  GenericTask<Msg>::processTaskMsg(stop);
	}

	virtual void processServerData() {COUT("Inside base-class procesServerData");};

	TimeVal timeOut_;
	struct timeval* timeOutPtr_;
	
	// A byte array into which serialized data will be returned
	
	std::vector<unsigned char> bytes_;
	
	// The size in bytes of the last message read.  This is to allow
	// for the case where a message is smaller than the allocated
	// bytes_ array.
	
	unsigned sizeInBytesOfLastMessage_;
	
	unsigned sendBufSize_;
	unsigned readBufSize_;

	bool stop_;

	// The connection manager
	
	TcpClient tcp_;
	
	NetHandler handler_;
	
    }; // End class ClientTask
    
  } // End namespace util
} // End namespace sza


#endif // End #ifndef SZA_UTIL_CLIENTTASK_H
