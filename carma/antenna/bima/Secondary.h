/**@file
 * Class definition for Stepper Motors for BIMA systems.
 * This class is derived from existing BIMA code to
 * generally describe motor parameters.
 * The original structure was defined in inc/nrcvr.h
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.3 $
 * $Date: 2006/03/31 04:28:23 $
 * $Id: Secondary.h,v 1.3 2006/03/31 04:28:23 colby Exp $
 */



#ifndef CARMA_ANTENNA_BIMA_SECONDARY_H
#define CARMA_ANTENNA_BIMA_SECONDARY_H

// C++ Includes
#include <cmath>
#include <vector>
#include <string>
#include <iosfwd>

// System includes
#include <unistd.h>

// CARMA includes
#include "carma/util/Program.h"
#include "carma/services/Table.h"
#include "carma/antenna/bima/TelemetryClient.h"
#include "carma/antenna/bima/Configuration.h"
#include "carma/antenna/bima/Motor.h"


namespace carma
{
  namespace antenna
    {
      namespace bima
	{
	  class Secondary : public TelemetryClient
	    {

	    public:
              Secondary( Configuration &config );

	      typedef enum { MOVING, ACQUIRED, FAILED } FocusState;

              float getFocus(); // returns mm
              short getRawFocus(); // returns counts
              void  setFocus( float mm ); // sets focus in mm
	      FocusState getState();
	      void setState( FocusState fs );

	    private:
              std::string _name;
              Motor *_focus;

              Configuration &_config;
            
	    }; // class Secondary
	} // namespace bima
    } // namespace antenna
} // namespace carma

#endif // CARMA_ANTENNA_BIMA_SECONDARY_H
