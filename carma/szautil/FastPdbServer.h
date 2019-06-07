// $Id: FastPdbServer.h,v 1.1 2014/05/05 22:51:57 eml Exp $

#ifndef SZA_UTIL_FASTPDBSERVER_H
#define SZA_UTIL_FASTPDBSERVER_H

/**
 * @file FastPdbServer.h
 * 
 * Tagged: Thu Mar  7 14:53:35 PST 2013
 * 
 * @version: $Revision: 1.1 $, $Date: 2014/05/05 22:51:57 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/FastPdb.h"
#include "carma/szautil/FastPdbData.h"
#include "carma/szautil/Server.h"

#define REG_DATA_FN(fn) void (fn)(unsigned nByte, unsigned nEl, unsigned char* src, unsigned char* dest)

namespace sza {
  namespace util {

    class NetMonitorFrame;

    class FastPdbServer : public sza::util::Server {
    public:

      //------------------------------------------------------------
      // A class for maintaining per-client data
      //------------------------------------------------------------

      class ClientData : 
      public Server::ServerData 
      {
      public:	

        ClientData() {};
	virtual ~ClientData();
	
	FastPdbData clientData_;
	
	void processMessage(FastPdbServer* server,
			    ServerConnection* client);
      };

      //------------------------------------------------------------
      // Now method of the FastPdbServer class itself
      //------------------------------------------------------------

      /**
       * Constructor.
       */
      FastPdbServer(bool spawnThread, unsigned port, std::string file);

      /**
       * Destructor.
       */
      virtual ~FastPdbServer();

      FastPdb pdb_;

      FastPdbData clientData_;

      void processClientData(ServerConnection* client);
      void sendResponse(ServerConnection* client);

      // Inherited interface from Server

      void run();
      void readClientData(ServerConnection* client);
      void acceptClientAction(ServerConnection* conn);

    }; // End class FastPdbServer

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_FASTPDBSERVER_H
