/**@file
 * Class definition for Stepper Motors for BIMA systems.
 * This class is derived from existing BIMA code to
 * generally describe motor parameters.
 * The original structure was defined in inc/nrcvr.h
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.7 $
 * $Date: 2005/09/26 20:56:55 $
 * $Id: LO.h,v 1.7 2005/09/26 20:56:55 colby Exp $
 */



#ifndef CARMA_ANTENNA_BIMA_LO_H
#define CARMA_ANTENNA_BIMA_LO_H

#include <vector>

// System includes
#include <string>
#include <unistd.h>

// CARMA includes
#include "carma/util/Program.h"
#include "carma/services/Table.h"
#include "carma/antenna/bima/TelemetryClient.h"
#include "carma/antenna/bima/Configuration.h"

namespace carma
{
  namespace antenna
    {
      namespace bima
	{
	  class LO : public TelemetryClient
	    {

	    public:
	      LO( Configuration& config );

              double xBandFreq();
              double xBandErrorVolts();
              double xBandIFLevel();
              bool xLockStatus();
              void setLockInfo( int value );
              int getLockInfo();
              void lockX( double xfreq );
              void setCommanded( double xfreq );
              double getCommanded();

	    private:
              std::string _name;
              const char *_dir;
              int _yigAnt;
            
	    }; // class LO
	} // namespace bima
    } // namespace antenna
} // namespace carma

#endif // CARMA_ANTENNA_BIMA_LO_H
