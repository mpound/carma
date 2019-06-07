

/**@file
 * Class definition for TelemetryClient on the BIMA antennas.
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill </dl>
 * $Revision: 1.5 $
 * $Date: 2010/12/15 19:02:55 $
 * $Id: AntennaIFClient.h,v 1.5 2010/12/15 19:02:55 plambeck Exp $
 */


#ifndef CARMA_BIMA_ANTENNAIFCLIENT_H
#define CARMA_BIMA_ANTENNAIFCLIENT_H

#include <iostream>
#include <ostream>
#include <string>
#include <map>
#include <vector>

#include <sys/poll.h>

#include "carma/antenna/bima/TelemetryCommand.h"
#include "carma/antenna/bima/Configuration.h"
#include "carma/antenna/bima/SharedMemory.h"
#include "carma/util/IPQwriter.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Logger.h"
#include "carma/util/Trace.h"

namespace carma
{
  namespace antenna
    {
      namespace bima
	{
	  class AntennaIFClient : public SharedMemory
	    {
	    public:
	      AntennaIFClient( Configuration& config );
	      ~AntennaIFClient();
	      
              void command( const unsigned short command, short value );
              void command( const unsigned short command, float value );
              Configuration& getConfig() { return _config; }

              static const unsigned short SELECT_IF_BAND = 1;
              static const unsigned short SET_IF_LEVEL   = 2;
              static const unsigned short SET_IF_ATTEN   = 3;

              static const unsigned short SET_IF1_LEVEL   = 5;
              static const unsigned short SET_IF1_ATTEN   = 6;
              static const unsigned short SET_IF2_LEVEL   = 7;
              static const unsigned short SET_IF2_ATTEN   = 8;

              static const unsigned short START_IF1_FASTSAMP   = 9;
              static const unsigned short START_IF2_FASTSAMP   = 10;
              static const unsigned short STOP_IF1_FASTSAMP   = 11;
              static const unsigned short STOP_IF2_FASTSAMP   = 12;
	      
              static const unsigned short SISRX_SET_VDRAIN    = 13;
              static const unsigned short SISRX_SET_VGATE1    = 14;
              static const unsigned short SISRX_SET_VGATE2    = 15;
              static const unsigned short SISRX_SET_VJ        = 16;
              static const unsigned short SISRX_TUNE          = 17;
              static const unsigned short SISRX_GETVGAP       = 18;
              static const unsigned short SISRX_SCANV1        = 19;
              static const unsigned short SISRX_SCANV2        = 20;
              static const unsigned short SISRX_SCANDV        = 21;
              static const unsigned short SISRX_IVCURVE       = 22;
              static const unsigned short SISRX_SET_LOOP_MODE = 23;

	    private:
	      carma::util::IPQwriter<TelemetryCommand> *_aifWriter;
              Configuration& _config;
	      
	    };
	}
    }
}



#endif // CARMA_BIMA_ANTENNAIFCLIENT_H
