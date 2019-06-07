// $Id: NetMonitorStreamServer.h,v 1.1 2013/07/10 15:28:30 eml Exp $

#ifndef SZA_UTIL_NETMONITORSTREAMSERVER_H
#define SZA_UTIL_NETMONITORSTREAMSERVER_H

/**
 * @file NetMonitorStreamServer.h
 * 
 * Tagged: Mon May 23 16:22:59 PDT 2011
 * 
 * @version: $Revision: 1.1 $, $Date: 2013/07/10 15:28:30 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/Server.h"

namespace sza {
  namespace util {

    class NetMonitorFrame;

    class NetMonitorStreamServer : public sza::util::Server {
    public:

      /**
       * Constructor.
       */
      NetMonitorStreamServer(bool spawnThread, unsigned port, NetMonitorFrame* ndf);
      NetMonitorStreamServer(bool spawnThread, unsigned port, NetMonitorFrame* ndf, int fdRead);

      /**
       * Destructor.
       */
      virtual ~NetMonitorStreamServer();

    protected:

      NetMonitorFrame* nmf_;
      int fdRead_;
      bool haveFrame_; // True once a data frame has been received

      virtual void run();
      void acceptClientAction(ServerConnection* conn);
      void checkForDataFrames();
      void sendRegisterMap(ServerConnection* conn);
      void sendDataFrame();
      void forwardDataFrame();

    }; // End class NetMonitorStreamServer

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_NETMONITORSTREAMSERVER_H
