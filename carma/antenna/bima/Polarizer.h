/**@file
 * Class definition for Stepper Motors for BIMA systems.
 * This class is derived from existing BIMA code to
 * generally describe motor parameters.
 * The original structure was defined in inc/nrcvr.h
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.2 $
 * $Date: 2007/03/03 01:29:21 $
 * $Id: Polarizer.h,v 1.2 2007/03/03 01:29:21 colby Exp $
 */



#ifndef CARMA_ANTENNA_BIMA_POLARIZER_H
#define CARMA_ANTENNA_BIMA_POLARIZER_H

#include <map>

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
      class Polarizer : public TelemetryClient
      {

	public:
	  Polarizer( Configuration& config );

	  typedef enum { MOVING, BAD, RCP, LCP, H, V, CLEAR } PolPos;
	  void loadConfFile();
	  PolPos getCanonicalPosition();
	  int getInstantPosition();


	private:
	  Motor *_polarizer;
          std::map<std::string, unsigned short * > _positions;

	  char _band;

	  static const unsigned short _tolerance = 250;
	  unsigned short *_rcp;
	  unsigned short *_lcp;
	  unsigned short *_clear;

      }; // class Polarizer
    } // namespace bima
  } // namespace antenna
} // namespace carma

#endif // CARMA_ANTENNA_BIMA_POLARIZER_H
