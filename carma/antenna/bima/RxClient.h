

/**@file
 * Class definition for RxClient on the BIMA antennas.
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill </dl>
 * $Revision: 1.1 $
 * $Date: 2006/02/09 18:08:36 $
 * $Id: RxClient.h,v 1.1 2006/02/09 18:08:36 colby Exp $
 */


#ifndef CARMA_BIMA_RXCLIENT_H
#define CARMA_BIMA_RXCLIENT_H

#include <iosfwd>

#include "carma/antenna/bima/Configuration.h"
#include "carma/antenna/bima/RxCommand.h"
#include "carma/antenna/bima/SharedMemory.h"
#include "carma/util/IPQwriter.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Logger.h"
#include "carma/util/Trace.h"

// NOTE, Matching _rxWriter in class definition
// below!
#define RWSET(N,V) RCSET(_rxWriter,N,V)

namespace carma
{
  namespace antenna
  {
    namespace bima
    {
      class RxClient : public SharedMemory
      {
	public:
	  RxClient( Configuration& config );
	  ~RxClient();

	  Configuration& getConfig() { return _config; }

	  void rxWrite();

	protected:
	  std::auto_ptr< carma::util::IPQwriter<RxCommand> > _rxWriter;

	private:
	  Configuration& _config;
      };
    }
  }
}



#endif // CARMA_BIMA_RXCLIENT_H
