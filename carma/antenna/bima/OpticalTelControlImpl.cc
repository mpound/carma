/**
 * @file
 * Definition for OpticalTelControlImpl class.
 *
 * @author Colby Gutierrez-Kraybill
 * $Revision: 1.13 $
 * $Date: 2012/02/29 16:22:53 $
 * $Id: OpticalTelControlImpl.cc,v 1.13 2012/02/29 16:22:53 abeard Exp $
 */

// Carma includes
#include "carma/antenna/bima/OpticalTelControlImpl.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

// Carma Tools includes
#include <log4cpp/Priority.hh>

using namespace carma::antenna::bima;
using namespace carma::antenna::common;
using namespace carma::util;
using namespace log4cpp;

// -----------------------------------------------------------------------------
OpticalTelControlImpl::OpticalTelControlImpl(
    carma::monitor::AntennaCommon::OpticalTel &mon,
    FrameGrabber &fg,
    Configuration &config,
    const float azFieldOfViewInArcminutes,
    const float elFieldOfViewInArcminutes,
    const float rotationInDegrees,
    const bool simulate ) :
    OpticalTelCommon( mon, fg, azFieldOfViewInArcminutes,
                      elFieldOfViewInArcminutes, rotationInDegrees, simulate ),
    _config( config )
{
// put in tracing
  _flap = new OpticalFlap( _config );
}

// -----------------------------------------------------------------------------
OpticalTelControlImpl::~OpticalTelControlImpl()
{
// put in tracing
}

// -----------------------------------------------------------------------------
void OpticalTelControlImpl::turn( SwitchState state )
{
  if ( state == ON )
    _flap->open();
  else if ( state == OFF )
    _flap->close();
  else
    throw CARMA_EXCEPTION( UserException, "Invalid State passed, must be ON or OFF" );
// implement for bima...
}
