/**@file
 * Class definition for Stepper Motors for BIMA systems.
 * This class is derived from existing BIMA code to
 * generally describe motor parameters.
 * The original structure was defined in inc/nrcvr.h
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.10 $
 * $Date: 2012/02/07 02:24:40 $
 * $Id: CalWheel.h,v 1.10 2012/02/07 02:24:40 plambeck Exp $
 */



#ifndef CARMA_ANTENNA_BIMA_CALWHEEL_H
#define CARMA_ANTENNA_BIMA_CALWHEEL_H

#include <vector>

// System includes
#include <string>
#include <unistd.h>

// CARMA includes
#include "carma/util/Program.h"
#include "carma/services/Table.h"
#include "carma/antenna/bima/TelemetryClient.h"
#include "carma/antenna/bima/Motor.h"
#include "carma/antenna/bima/Configuration.h"

namespace carma
{
  namespace antenna
    {
      namespace bima
	{
	  class CalWheel : public TelemetryClient
	    {

	    public:
	      CalWheel( Configuration& config );

              typedef enum { SKY, AMB, FIXED, REFLEC, MOVING, ERROR, UNKNOWN } Positions;

              void setPosition( CalWheel::Positions position, int iband );
              void setPosition( std::string stringpos, int iband );
	      int getInstantPosition();
              Positions getPosition( int iband );

              double getAmbTemp();
          
	      void setCurSequenceNo( int seq );
	      void setNextSequenceNo( int seq );
	      int getCurSequenceNo();
	      int getNextSequenceNo();

	    private:
              static const int skytarg[5];
              static const int ambtarg[5];
              static const int hottarg[5];

              Motor *_calwheel;
              Motor *_pol;
	      bool _cmOpticsInstalled;

            
	    }; // class CalWheel
	} // namespace bima
    } // namespace antenna
} // namespace carma

#endif // CARMA_ANTENNA_BIMA_CALWHEEL_H
