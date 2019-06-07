#include "carma/antenna/sza/antenna/corba/RxSelector.h"

#include "carma/antenna/common/RxControl.h"
#include "carma/antenna/common/RxControl_skel.h"
#include "carma/antenna/common/RxControl_skel_tie.h"
#include "carma/antenna/common/RxSelector.h"
#include "carma/antenna/common/RxSelector_skel.h"
#include "carma/antenna/common/RxSelector_skel_tie.h"

#include "carma/antenna/sza/antenna/corba/Rx1cmProxy.h"
#include "carma/antenna/sza/antenna/corba/Rx3mmProxy.h"
#include "carma/antenna/sza/antenna/corba/Rx1mmProxy.h"
#include "carma/antenna/sza/antenna/corba/SzaShareCorba.h"

#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/antenna/sza/control/szaRxControl.h"
#include "carma/antenna/sza/control/szaRxControl_skel.h"
#include "carma/antenna/sza/control/szaRxControl_skel_tie.h"

#include "carma/corba/Server.h"

#include "carma/szautil/Exception.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::antenna::corba;
using namespace sza::util;


/**.......................................................................
 * Constructor.
 */
RxSelector::RxSelector(AntennaMaster* parent) : Proxy(parent)
{
  parent_ = 0;

  rx1cm_  = 0;
  rx3mm_  = 0;
  rx1mm_  = 0;

  // Sanity check

  if(parent == 0)
    ThrowError("NULL parent pointer received");

  parent_ = parent;

  // Initialize subsystem pointers

  rx1cm_ = new Rx1cmProxy(parent_);
  rx3mm_ = new Rx3mmProxy(parent_);
  rx1mm_ = new Rx1mmProxy(parent_);

}

/**.......................................................................
 * Destructor.
 */
RxSelector::~RxSelector()
{
  if(rx1cm_ != 0) {
    delete rx1cm_;
    rx1cm_ = 0;
  }

  if(rx3mm_ != 0) {
    delete rx3mm_;
    rx3mm_ = 0;
  }

  if(rx1mm_ != 0) {
    delete rx1mm_;
    rx1mm_ = 0;
  }
}

/**.......................................................................
 * Delegate responsibility for hosting the CORBA Rx object to
 * the antenna task
 */
carma::antenna::common::RxControl_ptr
RxSelector::Rx(carma::antenna::common::RxControl::Type type)
{
  // Set our internal pointer to the LO type specified by the band
  // (right now only the Yig is defined).

  switch(type) {
  case carma::antenna::common::RxControl::RX1CM:
    COUT("Returning 1cm object");
    return carma::antenna::common::RxControl::_duplicate( rx1cmPtr_ );
    break;
  case carma::antenna::common::RxControl::RX3MM:
    COUT("Returning 3mm object");
    return carma::antenna::common::RxControl::_duplicate( rx3mmPtr_ );
    break;
  case carma::antenna::common::RxControl::RX1MM:
    COUT("Returning 1mm object");
    return carma::antenna::common::RxControl::_duplicate( rx1mmPtr_ );
    break;
  default:
    ThrowError("Invalid receiver type: " << type);
    break;
  }
}

/**.......................................................................
 * Register this object with the corba::Server class 
 */
void RxSelector::registerObject( const std::string& name,
                                 carma::corba::Server & server )
{
  namespace POA_cac  = POA_carma::antenna::common;
  namespace POA_casc = POA_carma::antenna::sza::control;

  try {

    server.addServant< POA_cac::RxSelector_tie >( *this, name );

    if ( rx1cm_ ) {
      server.addServant< POA_casc::RxControl_tie >( *rx1cm_, rx1cmPtr_ );
      rx1cm_->registerObject( "Rx1cm", server );
    } 

    if ( rx3mm_ ) {
      server.addServant< POA_casc::RxControl_tie >( *rx3mm_, rx3mmPtr_ );
      rx3mm_->registerObject( "Rx3mm", server );
    }

    if ( rx1mm_ ) {
      server.addServant< POA_casc::RxControl_tie >( *rx1mm_, rx1mmPtr_ );
      rx1mm_->registerObject( "Rx1mm", server );
    }

  } catch (const CORBA::Exception& ex) {

    ThrowError("Unable to register new object: " << name << " (" << ex << ")");
  }
}

sza::antenna::corba::RxProxy* RxSelector::getRxProxy(sza::util::Rx::Id type)
{
  switch (type) {
  case Rx::RX1CM:
    return rx1cm_;
    break;
  case Rx::RX3MM:
    return rx3mm_;
    break;
  case Rx::RX1MM:
    return rx1mm_;
    break;
  default:
    return rx1cm_;
    break;
  }
}

void RxSelector::selectRx(sza::util::Rx::Id rxId)
{
  AntennaMasterMsg msg;
  unsigned long seq = sequenceNumber();

  COUT("selectRx seq = " << seq);

  msg.getRxMsg()->packSelectRxMsg(rxId, seq);
  msg.getRxMsg()->setCarmaRxSequenceNumber(0);
  parent_->fwdTaskMsg(&msg);
  share_->setRx(rxId);
}
