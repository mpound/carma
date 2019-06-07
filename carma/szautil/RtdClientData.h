// $Id: RtdClientData.h,v 1.1 2013/07/10 15:33:55 eml Exp $

#ifndef SZA_UTIL_RTDCLIENTDATA_H
#define SZA_UTIL_RTDCLIENTDATA_H

/**
 * @file RtdClientData.h
 * 
 * Tagged: Thu Mar  7 15:26:50 PST 2013
 * 
 * @version: $Revision: 1.1 $, $Date: 2013/07/10 15:33:55 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/NetUnion.h"
#include "carma/szautil/RtdArrayTemplate.h"
#include "carma/szautil/Server.h"

namespace sza {
  namespace util {

    class RtdClientData : public NetUnion, public Server::ServerData {
    public:

      enum {
	MEM_TEMPLATE  = 0x1,
	MEM_DATAREGS  = 0x2,
	MEM_ADDREG    = 0x3,
	MEM_REMREG    = 0x4,
	MEM_ADDREGACK = 0x5,
	MEM_REMREGACK = 0x6,
      };

      // Constructor.

      RtdClientData();

      // Destructor.

      virtual ~RtdClientData();

      RtdArrayTemplate template_;
      unsigned addReg_;
      unsigned remReg_;
      std::vector<unsigned char> dataBytes_;

      friend std::ostream& operator<<(std::ostream& os, RtdClientData& data);

    }; // End class RtdClientData

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_RTDCLIENTDATA_H
