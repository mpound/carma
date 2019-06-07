#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/corba/Server.h"

#include "carma/antenna/common/AntennaControl.h"
#include "carma/antenna/common/AntennaControl_skel.h"
#include "carma/antenna/common/AntennaControl_skel_tie.h"
#include "carma/antenna/common/CryoControl.h"
#include "carma/antenna/common/CryoControl_skel.h"
#include "carma/antenna/common/CryoControl_skel_tie.h"
#include "carma/antenna/common/FocusControl.h"
#include "carma/antenna/common/FocusControl_skel.h"
#include "carma/antenna/common/FocusControl_skel_tie.h"
#include "carma/antenna/common/RxSelector.h"
#include "carma/antenna/common/RxSelector_skel.h"
#include "carma/antenna/common/RxSelector_skel_tie.h"
#include "carma/antenna/sza/antenna/corba/AntennaInitializer.h"
#include "carma/antenna/sza/antenna/corba/AntennaProxy.h"
#include "carma/antenna/sza/antenna/corba/CalibratorProxy.h"
#include "carma/antenna/sza/antenna/corba/Cryo.h"
#include "carma/antenna/sza/antenna/corba/DriveProxy.h"
#include "carma/antenna/sza/antenna/corba/Focus.h"
#include "carma/antenna/sza/antenna/corba/RxSelector.h"
#include "carma/antenna/sza/antenna/corba/SzaShareCorba.h"
#include "carma/antenna/sza/control/szaCalibratorControl.h"
#include "carma/antenna/sza/control/szaCalibratorControl_skel.h"
#include "carma/antenna/sza/control/szaCalibratorControl_skel_tie.h"
#include "carma/antenna/sza/control/szaDriveControl.h"
#include "carma/antenna/sza/control/szaDriveControl_skel.h"
#include "carma/antenna/sza/control/szaDriveControl_skel_tie.h"
#include "carma/antenna/sza/control/szaAntennaControl.h"
#include "carma/antenna/sza/control/szaAntennaControl_skel.h"
#include "carma/antenna/sza/control/szaAntennaControl_skel_tie.h"

#include "carma/szautil/Exception.h"


using namespace carma;
using namespace std;
using namespace sza::antenna::control;
using namespace sza::antenna::corba;
using namespace sza::util;

/**.......................................................................
 * Constructor
 */
AntennaProxy::AntennaProxy(AntennaMaster* parent) : Proxy(parent)
{
  cal_         = 0;
  cryo_        = 0;
  drive_       = 0;
  focus_       = 0;
  rxSelector_  = 0;
  initializer_ = 0;

  // Initialize subsystem pointers

  cal_         = new CalibratorProxy(parent);
  cryo_        = new Cryo(parent);
  drive_       = new DriveProxy(parent);
  focus_       = new Focus(parent);
  rxSelector_  = new RxSelector(parent);

  initializer_ = new AntennaInitializer(this);

};

/**.......................................................................
 * Destructor
 */
AntennaProxy::~AntennaProxy()
{
  if(cal_ != 0) {
    delete cal_;
    cal_ = 0;
  }

  if(cryo_ != 0) {
    delete cryo_;
    cryo_ = 0;
  }

  if(drive_ != 0) {
    delete drive_;
    drive_ = 0;
  }

  if(focus_ != 0) {
    delete focus_;
    focus_ = 0;
  }

  if(rxSelector_ != 0) {
    delete rxSelector_;
    rxSelector_ = 0;
  }
};


/**.......................................................................
 * Register this object with the CORBA nameserver
 */
void AntennaProxy::registerObject( const string& name, 
                                   carma::corba::Server & server)
{
  std::ostringstream os;
  os.str("");

  COUT("Inside AntennaProxy:: register object");

  namespace POA_cac = POA_carma::antenna::common;
  namespace POA_casc = POA_carma::antenna::sza::control;

  try {

    os << name << ".Antenna";
    server.addServant< POA_casc::AntennaControl_tie >(*this, os.str());

  } catch (const CORBA::Exception& ex) {

    COUT("Got an error: " << ex);
    ThrowError("Unable to register new object: " << name << " (" << ex << ")");
  }

  // Register other objects maintained by us

  COUT("Here 2");

  if(cryo_) {
    os.str("");
    os << name << ".Cryo";
    server.addServant< POA_cac::CryoControl_tie >(*cryo_, os.str());
  }

  COUT("Here 3");

  if(cal_) {
    os.str("");
    os << name << ".Calibrator";
    server.addServant< POA_casc::CalibratorControl_tie >(*cal_, os.str());
  }

  COUT("Here 4");

  if(drive_) {
    os.str("");
    os << name << ".Drive";
    server.addServant< POA_casc::DriveControl_tie >(*drive_, os.str());
  }

  COUT("Here 5");

  if(focus_) {
    os.str("");
    os << name << ".Focus";
    server.addServant< POA_cac::FocusControl_tie >(*focus_, os.str());
  }

  COUT("Here 5");

  if(rxSelector_) {
    os.str("");
    os << name << ".RxSelector";
    rxSelector_->registerObject( os.str(), server ); 
  }
}

void AntennaProxy::resetAllCanModules()
{
  AntennaMasterMsg msg;
  msg.getRxMsg()->packResetMsg(CanModule::ALL, false);
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

void AntennaProxy::setInitialization(bool initialized)
{
  CTOUT("Set Initialization called");
  share_->setInitialized(initialized);
}

AntNum* AntennaProxy::getAnt()
{
  return parent_->getAnt();
}

// Local method to initialize the antenna

void AntennaProxy::initializeAntenna()
{
  initializer_->initializeAntenna();
}

/**.......................................................................
 * CORBA method to reload bias tables (to allow reloading without
 * restarting antenna process)
 *
 */
void AntennaProxy::reloadBiasTables()
{
  initializer_->reloadBiasTables();
}
