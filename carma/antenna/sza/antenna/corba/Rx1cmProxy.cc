#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/corba/Server.h"
#include "carma/antenna/common/IFControl.h"
#include "carma/antenna/common/IFControl_skel.h"
#include "carma/antenna/common/IFControl_skel_tie.h"
#include "carma/antenna/sza/antenna/corba/FrontEnd1cmProxy.h"
#include "carma/antenna/sza/antenna/corba/IF1cmProxy.h"
#include "carma/antenna/sza/antenna/corba/LO1cmProxy.h"
#include "carma/antenna/sza/antenna/corba/Optics1cmProxy.h"
#include "carma/antenna/sza/antenna/corba/Rx1cmProxy.h"
#include "carma/antenna/sza/control/szaLOControl.h"
#include "carma/antenna/sza/control/szaLOControl_skel.h"
#include "carma/antenna/sza/control/szaLOControl_skel_tie.h"
#include "carma/antenna/sza/control/szaRxControl.h"
#include "carma/antenna/sza/control/szaRxControl_skel.h"
#include "carma/antenna/sza/control/szaRxControl_skel_tie.h"

using namespace carma;
using namespace std;
using namespace sza::antenna::control;
using namespace sza::antenna::corba;
using namespace sza::util;

Rx1cmProxy::Rx1cmProxy(AntennaMaster* parent) :
  RxProxy(parent)
{
  lo_           = new LO1cmProxy(parent);
  ifSys_        = new IF1cmProxy(parent);
  frontEnd_     = new FrontEnd1cmProxy(parent);
  optics_       = new Optics1cmProxy(parent);
}

/**.......................................................................
 * Destructor function
 */
Rx1cmProxy::~Rx1cmProxy() {}

/**.......................................................................
 * Setup frequencies for this receiver
 */
void Rx1cmProxy::setFrequency(double yigFreq, double LOFreq,
			      bool endWithAbsorberInBeam,
			      bool optimizeReceiver,
			      bool forceRelock, // Not used by sza
			      CORBA::ULong seq)
{
  // Set the default yig frequency, so that the selectRx command gets
  // it right

  setDefaultYigFrequency(Rx::RX30GHZ, Frequency(Frequency::GigaHz(), yigFreq));
  RxProxy::setFrequency(Rx::RX30GHZ, seq);
};

void Rx1cmProxy::registerObject( const std::string & name, 
                                 carma::corba::Server & server )
{

  try {

    namespace POA_cac = POA_carma::antenna::common;
    namespace POA_casc = POA_carma::antenna::sza::control;

    if ( ifSys_ )
        server.addServant< POA_cac::IFControl_tie >( *ifSys_,
                                                     ifPtr_ );

    if ( lo_ )
        server.addServant< POA_casc::LOControl_tie >( *lo_, 
                                                      loPtr_ );

    if ( frontEnd_ )
        server.addServant< POA_cac::FrontEndControl_tie >( *frontEnd_, 
                                                           frontEndPtr_ );

    if ( optics_ )
        server.addServant< POA_cac::OpticsControl_tie >( *optics_, 
                                                         opticsPtr_ );

  } catch (const CORBA::Exception& ex) {

    COUT("Got an error while registering object: " << name);
    COUT("Got an error: " << ex);
    ThrowError("Unable to register new object: " << name << " (" << ex << ")");
  }
}
