// $Id: FastPdbClient.h,v 1.1 2014/05/05 22:51:57 eml Exp $

#ifndef SZA_UTIL_FASTPDBCLIENT_H
#define SZA_UTIL_FASTPDBCLIENT_H

/**
 * @file FastPdbClient.h
 * 
 * Tagged: Tue Feb 18 16:15:46 PST 2014
 * 
 * @version: $Revision: 1.1 $, $Date: 2014/05/05 22:51:57 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/Client.h"
#include "carma/szautil/FastPdbData.h"

namespace sza {
  namespace util {

    class FastPdbClient : public sza::util::Client {
    public:

      /**
       * Constructor.
       */
      FastPdbClient(bool spawn, std::string host, unsigned connectPort);

      /**
       * Destructor.
       */
      virtual ~FastPdbClient();

      virtual void processServerData();

      void sendListSourceMsg(std::string src);

    private:

      FastPdbData data_;

    }; // End class FastPdbClient

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_FASTPDBCLIENT_H
