/**@file
 * Class definition for Stepper Motors for BIMA systems.
 * This class is derived from existing BIMA code to
 * generally describe motor parameters.
 * The original structure was defined in inc/nrcvr.h
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.2 $
 * $Date: 2005/08/02 23:00:57 $
 * $Id: IF.h,v 1.2 2005/08/02 23:00:57 colby Exp $
 */



#ifndef CARMA_ANTENNA_BIMA_IF_H
#define CARMA_ANTENNA_BIMA_IF_H

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
	  class IF : public TelemetryClient
	    {

	    public:
              IF( Configuration &config );

              float getTemp();
              float getHeater();

	    private:
              std::string _name;

              Configuration &_config;
            
	    }; // class IF
	} // namespace bima
    } // namespace antenna
} // namespace carma

#endif // CARMA_ANTENNA_BIMA_IF_H
