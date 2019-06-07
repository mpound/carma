/**
 * @file
 * Class definitions for the AntennaControlImpl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.9 $
 * $Date: 2012/02/15 21:05:00 $
 * $Id: AntennaControlImpl.cc,v 1.9 2012/02/15 21:05:00 abeard Exp $
 */

#include "carma/antenna/ovro/canbus/OvroMaster.h"
#include "carma/antenna/ovro/control/AntennaControlImpl.h"
#include "carma/corba/Server.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/Program.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/Trace.h"

#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

using namespace carma::antenna::common;
using namespace carma::antenna::ovro;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

// -----------------------------------------------------------------------------
AntennaControlImpl::AntennaControlImpl(
    carma::antenna::ovro::OvroMaster & master,
    carma::corba::Server & server ) :
        server_( server ),
        master_( master ),
        log_( Program::getLogger() )
{
    CARMA_CPTRACE( Trace::TRACE6, "AntennaControlImpl() - C'tor." );
}

// -----------------------------------------------------------------------------
AntennaControlImpl::~AntennaControlImpl()
{
    CARMA_CPTRACE( Trace::TRACE6, "AntennaControlImpl() - D'tor." );
}

// -----------------------------------------------------------------------------
void AntennaControlImpl::resetAllCanModules()
{
    try {
        log_ << Priority::INFO << "AntennaControlImpl::"
            "resetAllCanModules().";
        master_.reset();
    } catch (...) {
        logAndRethrowCaughtExceptionAsUserException(
            log_,
            Priority::ERROR );
    }
}

// -----------------------------------------------------------------------------
void AntennaControlImpl::setInitialization( const CORBA::Boolean state )
{
    master_.setInitialization( state );
}

// -----------------------------------------------------------------------------
void AntennaControlImpl::quit( )
try {
    server_.stop( );
} catch (...) {
    // Stifle.
}
