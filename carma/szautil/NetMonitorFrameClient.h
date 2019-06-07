// $Id: NetMonitorFrameClient.h,v 1.2 2013/11/19 22:55:28 eml Exp $

#ifndef SZA_UTIL_NETMONITORFRAMECLIENT_H
#define SZA_UTIL_NETMONITORFRAMECLIENT_H

/**
 * @file NetMonitorFrameClient.h
 * 
 * Tagged: Mon May 23 16:00:06 PDT 2011
 * 
 * @version: $Revision: 1.2 $, $Date: 2013/11/19 22:55:28 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/Client.h"

namespace sza {
  namespace util {

    class NetMonitorFrame;

    class NetMonitorFrameClient : public sza::util::Client {
    public:

      /**
       * Constructor.
       */
      NetMonitorFrameClient(bool spawnThread, std::string host, unsigned port, NetMonitorFrame* nmf, int fdWrite=-1, bool spawnSignalHandler=false);

      /**
       * Destructor.
       */
      virtual ~NetMonitorFrameClient();

    protected:

      NetMonitorFrame* nmf_;
      int fdWrite_;

      void processServerData();
      void notify();

    }; // End class NetMonitorFrameClient

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_NETMONITORFRAMECLIENT_H
