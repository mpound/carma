/**
 * @file
 * Definition for OpticalTelControlImpl class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.21 $
 * $Date: 2014/02/06 23:39:38 $
 * $Id: OpticalTelControlImpl.cc,v 1.21 2014/02/06 23:39:38 control Exp $
 */

// Carma includes
#include "carma/antenna/ovro/control/OpticalTelControlImpl.h"
#include "carma/corba/Client.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/util/BaseException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/UserException.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

using namespace carma;
using namespace carma::antenna::ovro;
using namespace carma::antenna::common;
using namespace carma::monitor;
using namespace carma::util;
using namespace log4cpp;

#define COUT(statement)				\
  {						\
    std::ostringstream _macroOs;		\
    _macroOs << statement << std::endl;		\
    std::cout << _macroOs.str();		\
  }

// -----------------------------------------------------------------------------
// note: poa_ is inherited from OpticalTelCommon...
OpticalTelControlImpl::OpticalTelControlImpl(
    ::std::string antenna,
    FrameGrabber &fg,
    bool activate,
    AntennaCommon::OpticalTel & opticalTel,
    const float azFieldOfViewInArcminutes,
    const float elFieldOfViewInArcminutes,
    const float rotationInDegrees,
    const bool simulate,
    corba::Client & client ) :
    OpticalTelCommon(
        opticalTel,
        fg,
        azFieldOfViewInArcminutes, elFieldOfViewInArcminutes,
        rotationInDegrees, simulate ),
    enviroName_( "carma." + antenna + "." + ENVIRONMENT_NAME ),
    enviroControl_( EnvironmentalControl::_nil() ),
    client_( client )
{
  COUT("Here OP 0");
    CARMA_CPTRACE(Trace::TRACE6, "OpticalTelControlImpl() - "
        "Creating OpticalTelControl object.");

  COUT("Here OP 1");
    // Resolve Environmental Monitor DO - If this doesn't work just soldier on.
    try {
  COUT("Here OP 2");
        CARMA_CPTRACE(Trace::TRACE3, "Resolving enviroName_ " << enviroName_);
        enviroControl_ = 
            client_.resolveName< EnvironmentalControl >( enviroName_ );
  COUT("Here OP 3");
    } catch (...) {
        log_ << Priority::WARN << "OpticalTelControlImpl() - Unable to "
            "resolve EnvironmentalControl DO.";
        enviroControl_ = EnvironmentalControl::_nil();
	COUT("Here OP 4 ");
    }
}

// -----------------------------------------------------------------------------
OpticalTelControlImpl::~OpticalTelControlImpl()
{
    CARMA_CPTRACE(Trace::TRACE6, "~OpticalTelControlImpl() - "
        "Destroying OpticalTelControl object.");
}

// -----------------------------------------------------------------------------
void OpticalTelControlImpl::turn(SwitchState state)
{
    // If we have an invalid DO reference, try to resolve it.
    // If we get an exception with the current DO, throw it back to the client.
    // Basically, I don't do anything here above and beyond what an IMR
    // combined with a PERSISTENT POA would typically give me.
    ::CORBA::Boolean on = (state == carma::antenna::common::ON);
    try {
        log_ << Priority::INFO << "OpticalTelControlImpl::turn() - "
             << (on ? "En" : "Dis") << "abling optical telescope.";
        if ( CORBA::is_nil( enviroControl_ ) ) {
            try {
                CARMA_CPTRACE(Trace::TRACE3, "Enviro reference is nil, "
                    "attempting to re-resolve.");
                enviroControl_ = 
                    client_.resolveName<EnvironmentalControl>( enviroName_ );
            } catch (...) {
                enviroControl_ = EnvironmentalControl::_nil();
                throw;
            }
        }
        CARMA_CPTRACE(Trace::TRACE3, (on ? "En" : "Dis") << "abling camera.");
        enviroControl_->enableCamera(on);
    } catch (...) {
        logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
    }
}
