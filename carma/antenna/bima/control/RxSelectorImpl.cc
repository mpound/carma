/**
 * @file
 * RxSelectorImpl CORBA class definition.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.10 $
 * $Date: 2012/02/21 21:06:58 $
 * $Id: RxSelectorImpl.cc,v 1.10 2012/02/21 21:06:58 abeard Exp $
 */

// Carma includes
#include "carma/antenna/bima/control/RxControlImpl.h"
#include "carma/antenna/bima/control/RxSelectorImpl.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

// STL includes
#include <string>

using namespace carma::antenna::common;
using namespace carma::antenna::bima;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

// -----------------------------------------------------------------------------
RxSelectorImpl::RxSelectorImpl( Configuration &config, 
                                RxControl_ptr rxControlPtr ) :
    RxClient( config ),
    log_( Program::getLogger() ),
    rxControlPtr_( rxControlPtr ),
    config_( config )
{
    // Nothing
}


// -----------------------------------------------------------------------------
RxSelectorImpl::~RxSelectorImpl()
{
    // Nothing
}

// -----------------------------------------------------------------------------
carma::antenna::common::RxControl_ptr RxSelectorImpl::Rx( RxControl::Type type )
{
  try
  {

    log_ << Priority::INFO << "RxSelectorImpl::Rx( type= "
      << type << ")";

    // RxSelection is basically a no-op in bima antennas
    // Therefore, any sequence info is completing immediately
    // once an Rx is selected.
    RWSET( command, RxCommand::USENEXTOPTICSEQNO );
    this->rxWrite();

    return carma::antenna::common::RxControl::_duplicate( rxControlPtr_ );
  }
  catch (const std::exception &ex)
  {
    log_ << Priority::ERROR << "RxSelectorImpl::Rx() - "
      << "std::exception caught. " << ex.what();
    throw CARMA_EXCEPTION( UserException, ex.what() );
  } catch (...) {
    log_ << Priority::ERROR << "RxSelectorImpl::Rx() - "
      << "Unknown exception caught.";
    throw CARMA_EXCEPTION( UserException,
	"RxSelectorImpl::Rx() - Unknown exception caught." );
  }

  // We should never get here.
  return RxControl::_nil();
}
