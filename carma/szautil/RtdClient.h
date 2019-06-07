// $Id: RtdClient.h,v 1.1 2013/07/10 15:32:25 eml Exp $

#ifndef SZA_UTIL_RTDCLIENT_H
#define SZA_UTIL_RTDCLIENT_H

/**
 * @file RtdClient.h
 * 
 * Tagged: Fri Mar  8 09:49:14 PST 2013
 * 
 * @version: $Revision: 1.1 $, $Date: 2013/07/10 15:32:25 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/Client.h"
#include "carma/szautil/RtdClientData.h"

namespace sza {
  namespace util {

    class RtdClient : public sza::util::Client {
    public:

      /**
       * Constructor.
       */
      RtdClient(bool spawn, std::string host, unsigned connectPort);

      /**
       * Destructor.
       */
      virtual ~RtdClient();

      virtual void processServerData();

      void sendAddRegMsg(unsigned id);
      void sendRemRegMsg(unsigned id);

    private:

      RtdClientData data_;

    }; // End class RtdClient

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_RTDCLIENT_H
