#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/corba/Server.h"
#include "carma/antenna/common/IFControl.h"
#include "carma/antenna/common/IFControl_skel.h"
#include "carma/antenna/common/IFControl_skel_tie.h"
#include "carma/antenna/sza/antenna/corba/FrontEnd1mmProxy.h"
#include "carma/antenna/sza/antenna/corba/IF1mmProxy.h"
#include "carma/antenna/sza/antenna/corba/LO1mmProxy.h"
#include "carma/antenna/sza/antenna/corba/Optics1mmProxy.h"
#include "carma/antenna/sza/antenna/corba/Rx1mmProxy.h"
#include "carma/antenna/sza/control/szaLOControl.h"
#include "carma/antenna/sza/control/szaLOControl_skel.h"
#include "carma/antenna/sza/control/szaLOControl_skel_tie.h"
#include "carma/antenna/sza/control/szaRxControl.h"
#include "carma/antenna/sza/control/szaRxControl_skel.h"
#include "carma/antenna/sza/control/szaRxControl_skel_tie.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::antenna::corba;

Rx1mmProxy::Rx1mmProxy(AntennaMaster* parent) :
  RxProxy(parent)
{
  lo_           = new LO1mmProxy(parent);
  ifSys_        = new IF1mmProxy(parent);
  frontEnd_     = new FrontEnd1mmProxy(parent);
  optics_       = new Optics1mmProxy(parent);
}

/**.......................................................................
 * Destructor function
 */
Rx1mmProxy::~Rx1mmProxy() {}

/**.......................................................................
 * Setup frequencies for this receiver
 */
void Rx1mmProxy::setFrequency(double yigFreq, double LOFreq,
			      bool endWithAbsorberInBeam,
			      bool optimizeReceiver,
			      bool forceRelock, // Not used by sza
			      CORBA::ULong seq)
{
  std::ostringstream os;
  os << "SZA has no 1mm receiver to tune to" << std::endl;
  throw CARMA_EXCEPTION(carma::util::UserException, os.str().c_str());
};

void Rx1mmProxy::registerObject( const std::string & name,
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
