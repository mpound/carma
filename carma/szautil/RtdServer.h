// $Id: RtdServer.h,v 1.1 2013/07/10 15:37:57 eml Exp $

#ifndef SZA_UTIL_RTDSERVER_H
#define SZA_UTIL_RTDSERVER_H

/**
 * @file RtdServer.h
 * 
 * Tagged: Thu Mar  7 14:53:35 PST 2013
 * 
 * @version: $Revision: 1.1 $, $Date: 2013/07/10 15:37:57 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/RtdClientData.h"
#include "carma/szautil/Server.h"

#define REG_DATA_FN(fn) void (fn)(unsigned nByte, unsigned nEl, unsigned char* src, unsigned char* dest)

namespace sza {
  namespace util {

    class NetMonitorFrame;

    class RtdServer : public sza::util::Server {
    public:

      //------------------------------------------------------------
      // A class for maintaining per-register info
      //------------------------------------------------------------

      class MonitoredReg {
      public:

	MonitoredReg(RegMapBlock* block, unsigned char* srcPtr);

	// Information describing this register

	RegMapBlock* block_;
	unsigned nEl_;
	unsigned nByte_;

	// A pointer to the start of the data for this register in the
	// data frame

	unsigned char* srcPtr_; 

	// A function for processing the data for this register

	REG_DATA_FN(*convFn_);

	// Method to pack data for this register

	unsigned pack(unsigned char* destPtr);

	// Possible options for this conversion function

	static REG_DATA_FN(convertNone);
	static REG_DATA_FN(convertHtons);
	static REG_DATA_FN(convertHtonl);
      };
      
      //------------------------------------------------------------
      // A class for maintaining per-client data
      //------------------------------------------------------------

      class ClientData : 
      public Server::ServerData 
      {
      public:	

        ClientData() {};
	virtual ~ClientData();
	
	RtdClientData clientData_;
	
	void processMessage(RtdServer* server,
			    ServerConnection* client,
			    std::map<unsigned, RegMapBlock*>& regMap,
			    std::map<unsigned, unsigned char*>& srcPtrMap);
	
	void resize();

	void pack();

	std::map<unsigned, MonitoredReg*> monitoredRegs_;
      };

      //------------------------------------------------------------
      // Now method of the RtdServer class itself
      //------------------------------------------------------------

      /**
       * Constructor.
       */
      RtdServer(bool spawnThread, unsigned port, NetMonitorFrame* nmf);
      RtdServer(bool spawnThread, unsigned port, NetMonitorFrame* nmf, int fdRead);

      /**
       * Destructor.
       */
      virtual ~RtdServer();

    private:

      std::map<unsigned, RegMapBlock*> registerBlockMap_;
      std::map<unsigned, unsigned char*> srcPtrMap_;

      NetMonitorFrame* nmf_;
      int fdRead_;
      RtdClientData clientData_;
      bool haveRegMap_;

      void constructMapOfRegisters();

      void checkForServerData();
      void processClientData(ServerConnection* client);

      void sendRegisterMap(ServerConnection* client);
      void sendRegisterMap();

      void sendAddRegAck(ServerConnection* client);
      void sendRemRegAck(ServerConnection* client);

      void sendDataRegisters();
      void sendDataRegisters(ServerConnection* client);

      // Inherited interface from Server

      void run();
      void readClientData(ServerConnection* client);
      void acceptClientAction(ServerConnection* conn);

    }; // End class RtdServer

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_RTDSERVER_H
