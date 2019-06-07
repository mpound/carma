/**
 * @file
 * BIMA CryoControl implementation.
 *
 * @author Colby Gutierrez-Kraybill
 *
 * $Revision: 1.7 $
 * $Id: OpticsControlImpl.cc,v 1.7 2012/02/21 21:06:58 abeard Exp $
 */


// Carma includes
#include "carma/antenna/bima/control/OpticsControlImpl.h"

using namespace log4cpp;
using namespace carma::util;
using namespace carma::antenna::bima;

// -----------------------------------------------------------------------------
OpticsControlImpl::OpticsControlImpl
(
 Configuration &config
 )
: RxClient( config ),
  log_(Program::getLogger()),
  _config( config )
{
  // Nothing
}

// -----------------------------------------------------------------------------
OpticsControlImpl::~OpticsControlImpl()
{
    // Nothing
}

// -----------------------------------------------------------------------------
void OpticsControlImpl::selectRx()
{
  log_ << Priority::INFO
       << "selectRx()";
    // Nothing
}
