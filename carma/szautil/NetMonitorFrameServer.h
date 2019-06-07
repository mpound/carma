// $Id: NetMonitorFrameServer.h,v 1.4 2013/11/19 22:55:28 eml Exp $

#ifndef SZA_UTIL_NETMONITORFRAMESERVER_H
#define SZA_UTIL_NETMONITORFRAMESERVER_H

/**
 * @file NetMonitorFrameServer.h
 * 
 * Tagged: Mon May 23 16:22:59 PDT 2011
 * 
 * @version: $Revision: 1.4 $, $Date: 2013/11/19 22:55:28 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/Server.h"

namespace sza {
  namespace util {

    class NetMonitorFrame;

    class NetMonitorFrameServer : public sza::util::Server {
    public:

      /**
       * Constructor.
       */
      NetMonitorFrameServer(bool spawnThread, unsigned port, NetMonitorFrame* ndf);
      NetMonitorFrameServer(bool spawnThread, unsigned port, NetMonitorFrame* ndf, int fdRead);

      /**
       * Destructor.
       */
      virtual ~NetMonitorFrameServer();

      virtual void run();

    protected:

      NetMonitorFrame* nmf_;
      int fdRead_;
      bool haveFrame_; // True once a data frame has been received

      void acceptClientAction(ServerConnection* conn);
      void checkForDataFrames();
      void sendRegisterMap(ServerConnection* conn);
      void sendDataFrame();
      void forwardDataFrame();

    }; // End class NetMonitorFrameServer

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_NETMONITORFRAMESERVER_H
