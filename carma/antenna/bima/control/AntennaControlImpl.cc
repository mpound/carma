/**
 * @file
 * Class definitions for the AntennaControlImpl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.4 $
 * $Date: 2012/02/21 21:06:58 $
 * $Id: AntennaControlImpl.cc,v 1.4 2012/02/21 21:06:58 abeard Exp $
 */

// Carma includes
#include "carma/antenna/bima/control/AntennaControlImpl.h"
#include "carma/antenna/bima/IFCanMaster.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/Trace.h"

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

using namespace carma::antenna::common;
using namespace carma::antenna::bima;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

// -----------------------------------------------------------------------------
AntennaControlImpl::AntennaControlImpl(
    carma::antenna::bima::IFCanMaster & master ) :
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
    CARMA_CPTRACE( Trace::TRACE1, " AntennaControlImpl()::resetAllCanModules()" );
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
try {
    log_ << Priority::INFO << "AntennaControlImpl::setInitialization().";
    master_.setInitialized( state );
} catch (...) {
    logAndRethrowCaughtExceptionAsUserException(
            log_,
            Priority::ERROR );
}
