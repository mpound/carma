// $Id: RtdMonitorFrameClient.h,v 1.1 2013/07/10 15:36:12 eml Exp $

#ifndef SZA_UTIL_RTDMONITORFRAMECLIENT_H
#define SZA_UTIL_RTDMONITORFRAMECLIENT_H

/**
 * @file RtdMonitorFrameClient.h
 * 
 * Tagged: Mon May 23 16:00:06 PDT 2011
 * 
 * @version: $Revision: 1.1 $, $Date: 2013/07/10 15:36:12 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/NetMonitorFrameClient.h"
#include "carma/szautil/RtdArrayTemplate.h"

// Define a class for obtaining data from a NetMonitorFrameServer.
// This client will ultimately get data for reserving to RTD clients

namespace sza {
  namespace util {

    class NetMonitorFrame;

    class RtdMonitorFrameClient : public sza::util::NetMonitorFrameClient {
    public:

      /**
       * Constructor.
       */
      RtdMonitorFrameClient(bool spawnThread, std::string host, unsigned port, NetMonitorFrame* nmf);
      RtdMonitorFrameClient(bool spawnThread, std::string host, unsigned port, NetMonitorFrame* nmf, int fdWrite);

      /**
       * Destructor.
       */
      virtual ~RtdMonitorFrameClient();

    protected:

      void processServerData();

      RtdArrayTemplate rtdTemplate_;
      
    }; // End class NetMonitorFrameClient

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_RTDMONITORFRAMECLIENT_H
