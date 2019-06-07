// $Id: RtdServerTask.h,v 1.2 2013/11/19 22:55:28 eml Exp $

#ifndef SZA_UTIL_RTDSERVERTASK_H
#define SZA_UTIL_RTDSERVERTASK_H

/**
 * @file RtdServerTask.h
 * 
 * Tagged: Thu Mar  7 14:53:35 PST 2013
 * 
 * @version: $Revision: 1.2 $, $Date: 2013/11/19 22:55:28 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/BitMask.h"
#include "carma/szautil/GenericTaskMsg.h"
#include "carma/szautil/RtdClientData.h"
#include "carma/szautil/ServerTask.h"

#define REG_DATA_FN(fn) void (fn)(unsigned nByte, unsigned nEl, unsigned char* src, unsigned char* dest)

namespace sza {
  namespace util {

    class NetMonitorFrame;

    //------------------------------------------------------------
    // A message class for use with this task
    //------------------------------------------------------------

    class RtdServerMsg : public GenericTaskMsg {
    public:

      enum MsgType {
	ADD_REG = 0x1,
	REM_REG = 0x2,
      };
      
      union {
	unsigned regId_;
      } body;
      
      // A type for this message

      MsgType type;
    };

    class RtdServerTask : public sza::util::ServerTask<RtdServerMsg> {
    public:

      //------------------------------------------------------------
      // A class for maintaining per-register info
      //------------------------------------------------------------

      class MonitoredReg {
      public:

	MonitoredReg(RegMapBlock* block, unsigned char* srcPtr, BitMask* validityBitMask);

	// Information describing this register

	RegMapBlock* block_;
	unsigned nEl_;
	unsigned nByte_;

	// A pointer to the start of the data for this register in the
	// data frame

	unsigned char* srcPtr_; 
	BitMask* validityBitMask_;

	// A function for processing the data for this register

	REG_DATA_FN(*convFn_);

	// Method to pack data for this register

	unsigned pack(unsigned char* destPtr);

	// Possible options for this conversion function

	static REG_DATA_FN(convertNone);
	static REG_DATA_FN(convertHtons);
	static REG_DATA_FN(convertHtonl);	
	static REG_DATA_FN(convertDouble);
	static REG_DATA_FN(convertPairedFourByteType);
      };
      
      //------------------------------------------------------------
      // A class for maintaining per-client data
      //------------------------------------------------------------

      class ClientData : 
      public ServerTask<RtdServerMsg>::ServerData 
      {
      public:	

        ClientData() {};
	virtual ~ClientData();
	
	RtdClientData clientData_;
	
	void processMessage(RtdServerTask* server,
			    ServerConnection* client,
			    std::map<unsigned, RegMapBlock*>& regMap,
			    std::map<unsigned, unsigned char*>& srcPtrMap);
	
	void resize();

	void pack();

	std::map<unsigned, MonitoredReg*> monitoredRegsMap_;
	std::list<MonitoredReg*> monitoredRegsList_;
      };

      //------------------------------------------------------------
      // Now method of the RtdServerTask class itself
      //------------------------------------------------------------

      /**
       * Constructor.
       */
      RtdServerTask(bool spawnThread, unsigned port, NetMonitorFrame* nmf);
      RtdServerTask(bool spawnThread, unsigned port, NetMonitorFrame* nmf, int fdRead);

      /**
       * Destructor.
       */
      virtual ~RtdServerTask();

      void run();

    private:

      std::map<unsigned, RegMapBlock*> registerBlockMap_;
      std::map<unsigned, unsigned char*> srcPtrMap_;

      unsigned char* vRegPtr_;
      RegMapBlock*   vRegBlk_;
      BitMask validityBitMask_;

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

      void readClientData(ServerConnection* client);
      void acceptClientAction(ServerConnection* conn);

    }; // End class RtdServerTask

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_RTDSERVERTASK_H
