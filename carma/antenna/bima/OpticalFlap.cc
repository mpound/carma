/**@file
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.7 $
 * $Date: 2006/02/10 21:22:14 $
 * $Id: OpticalFlap.cc,v 1.7 2006/02/10 21:22:14 colby Exp $
 */


#include <math.h>

// CARMA includes
#include "carma/antenna/bima/OpticalFlap.h"


using namespace std;
using namespace carma::util;
using namespace carma::antenna::bima;


OpticalFlap::OpticalFlap( Configuration& config )
     : TelemetryClient( config ), _config( config )
{
}

void OpticalFlap::open()
{
  setbits( "BITSTO50", 0x08, 0x08 );
}

void OpticalFlap::close()
{
  setbits( "BITSTO50", 0x00, 0x08 );
}

OpticalFlap::OpticalFlapStatus OpticalFlap::getStatus()
{
  return CLOSED;
}

