// $Id: FastPdbData.h,v 1.1 2014/05/05 22:52:38 eml Exp $

#ifndef SZA_UTIL_FASTPDBDATA_H
#define SZA_UTIL_FASTPDBDATA_H

/**
 * @file FastPdbData.h
 * 
 * Tagged: Tue Feb 18 13:08:16 PST 2014
 * 
 * @version: $Revision: 1.1 $, $Date: 2014/05/05 22:52:38 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/NetUnion.h"
#include "carma/szautil/Server.h"

namespace sza {
  namespace util {

    class FastPdbData : public NetUnion, public Server::ServerData {
    public:

      enum {
	MEM_SRCLIST  = 0x1,
	MEM_PROJLIST = 0x2,
	MEM_RESPONSE = 0x4,
      };

      std::string source_;
      std::string project_;
      std::string response_;

      /**
       * Constructor.
       */
      FastPdbData();

      /**
       * Destructor.
       */
      virtual ~FastPdbData();

      friend std::ostream& operator<<(std::ostream& os, FastPdbData& data);

    private:
    }; // End class FastPdbData

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_FASTPDBDATA_H
