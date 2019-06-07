#ifndef SZA_UTIL_CLIENT_H
#define SZA_UTIL_CLIENT_H

/**
 * @file Client.h
 * 
 * Tagged: Wed 01-Feb-06 10:43:27
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/FdSet.h"
#include "carma/szautil/NetDat.h"
#include "carma/szautil/NetHandler.h"
#include "carma/szautil/Runnable.h"
#include "carma/szautil/SignalTaskMsg.h"
#include "carma/szautil/TcpClient.h"
#include "carma/szautil/TimeVal.h"

namespace sza {
  namespace util {
    
    class SignalTask;

    class Client : public Runnable {
    public:
      
      /**
       * Constructor.
       */
      Client(bool spawn, std::string host, unsigned connectPort, 
	     unsigned readBufSize=0, unsigned sendBufSize=0, bool spawnSignalHandler=false);
      
      /**
       * Destructor.
       */
      virtual ~Client();
      
      /**
       * Block in select
       */
      void run();

      void setReadBufSize(unsigned size);
      void setSendBufSize(unsigned size);

    protected:
      
      // Method called when data have been completely read from the server

      void readServerData(NetHandler& handler);

      // Send data to the server

      void sendServerData(NetDat& dat);

      // Method called when data have been packed in our bytes array

      virtual void processServerData() {COUT("Inside base-class procesServerData");};

      TimeVal timeOut_;
      struct timeval* timeOutPtr_;

      // A byte array into which serialized data will be returned

      std::vector<unsigned char> bytes_;

      // The size in bytes of the last message read.  This is to allow
      // for the case where a message is smaller than the allocated
      // bytes_ array.

      unsigned sizeInBytesOfLastMessage_;

      /**
       * A private thread which will manage signal handling
       */
      SignalTask* signalTask_;
      bool spawnSignalTask_;

    private:

      unsigned sendBufSize_;
      unsigned readBufSize_;

      bool stop_;

      // The connection manager

      TcpClient tcp_;

      NetHandler handler_;

      FdSet fdSet_;

      void initMembers(std::string host, unsigned port, 
		      unsigned readBufSize, unsigned sendBufSize);

      // A run method to be called from pthread_start()

      static RUN_FN(runFn);

      // A shutdown method

      static SIGNALTASK_HANDLER_FN(shutDown);

      static NET_READ_HANDLER(readHandler);
      static NET_SEND_HANDLER(sendHandler);
      static NET_ERROR_HANDLER(errHandler);

      void connect();
      void disconnect();

    }; // End class Client
    
  } // End namespace util
} // End namespace sza


#endif // End #ifndef SZA_UTIL_CLIENT_H
