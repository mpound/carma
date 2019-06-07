#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/antenna/sza/antenna/corba/LOProxy.h"
#include "carma/antenna/sza/antenna/corba/IFProxy.h"
#include "carma/antenna/sza/antenna/corba/FrontEndProxy.h"
#include "carma/antenna/sza/antenna/corba/OpticsProxy.h"
#include "carma/antenna/sza/antenna/corba/PolarizationProxy.h"
#include "carma/antenna/sza/antenna/corba/RxProxy.h"
#include "carma/antenna/sza/antenna/corba/SzaShareCorba.h"

using namespace carma::antenna::common;

using namespace std;

using namespace sza::antenna::control;
using namespace sza::antenna::corba;
using namespace sza::util;

RxProxy::RxProxy(AntennaMaster* parent) : Proxy(parent)
{
  // Initialize pointers to this receiver's subsystems

  lo_           = 0;
  ifSys_        = 0;
  frontEnd_     = 0;
  optics_       = 0;
  polarization_ = 0;

  // And allocate resources for generic proxies.  Specific ones (lo_,
  // ifSys_, frontEnd_) will be allocated by inheritors

  polarization_ = new PolarizationProxy(parent);
};

/**.......................................................................
 * Destructor function
 */
RxProxy::~RxProxy()
{

  if(lo_ != 0) {
    delete lo_;
    lo_ = 0;
  }

  if(ifSys_ != 0) {
    delete ifSys_;
    ifSys_ = 0;
  }

  if(frontEnd_ != 0) {
    delete frontEnd_;
    frontEnd_ = 0;
  }

  if(optics_ != 0) {
    delete optics_;
    optics_ = 0;
  }

  if(polarization_ != 0) {
    delete polarization_;
    polarization_ = 0;
  }

  cout << "Leaving RxProxy destructor" << endl;
};

/**.......................................................................
 * Return a pointer to the internal LO system object
 */
LOControl_ptr RxProxy::LO()
{
  return LOControl::_duplicate( loPtr_ );
};

/**.......................................................................
 * Return a pointer to the internal IF system object
 */
IFControl_ptr RxProxy::IF(carma::antenna::common::RxControl::IF_Type ifoutpu)
{
  return IFControl::_duplicate( ifPtr_ );
};

/**.......................................................................
 * Return a pointer to the internal FrontEnd object
 */
FrontEndControl_ptr RxProxy::FrontEnd(
    const carma::antenna::common::RxControl::Pol_Type pol )
{
  return FrontEndControl::_duplicate( frontEndPtr_ );
};

/**.......................................................................
 * Return a pointer to the internal Optics object
 */
OpticsControl_ptr RxProxy::Optics()
{
  return OpticsControl::_duplicate( opticsPtr_ );
};

/**.......................................................................
 * Return a pointer to the internal Polarization object
 */
PolarizationControl_ptr RxProxy::Polarization()
{
  return PolarizationControl::_duplicate( polarizationPtr_ );
};

/**.......................................................................
 * Setup frequencies for this receiver
 */
void RxProxy::setFrequency(double yigFreq, double LOFreq,
			   bool endWithAbsorberInBeam,
			   bool optimizeReceiver,
			   CORBA::ULong seq)
{
  COUT("Inside RxProxy::setFrequency stub");
};

/**.......................................................................
 * Setup frequencies for this receiver
 */
void RxProxy::setFrequency(sza::util::Rx::Id rxId, CORBA::ULong seq)
{
  AntennaMasterMsg msg;

  // Decide which receiver we are being told to invoke

  msg.getRxMsg()->packSelectRxMsg(rxId, sequenceNumber());
  msg.getRxMsg()->setCarmaRxSequenceNumber(seq);

  COUT("Inside setFrequency: type = " << msg.getRxMsg()->carmaSeqNoType_ << " seq = " << msg.getRxMsg()->carmaSeqNo_);

  parent_->fwdTaskMsg(&msg);

  // Record the change of receiver in the shared-memory object

  share_->setRx(rxId);

  // Now that we've commanded the receiver set-up, install any
  // stored collimation offsets for this receiver too

  setSkyOffsets();

  // And assert the stored flexure terms for this receiver (although
  // why the flexure should be different for different receivers is
  // beyond me)

  setFlexure();
};

void RxProxy::measureTotalPower(carma::antenna::common::CalibratorControl::Position position)
{
}

void RxProxy::setObservingFrequency(double obsFreq, CORBA::ULong seq)
{
  COUT("Inside RxProxy::setObservingFrequency");
}

/**.......................................................................
 * Setup offsets for this receiver
 */
void RxProxy::setOffset(double az, double el)
{
  COUT("Inside setOffset stub");
};

void RxProxy::measureTotalPower(carma::antenna::common::CalibratorControl::Position position, CORBA::ULong seq)
{
  COUT("Inside measureTotalPower stub");
}

/**.......................................................................
 * Turn fast sampling on/off
 */
void RxProxy::toggleFastSampling(CORBA::ULong channel,
				 bool start)
{
  AntennaMasterMsg msg;
  msg.getRxMsg()->packToggleFastSamplingMsg(channel, start);
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

void RxProxy::setIFPresetPower( )
{
  ifSys_->setPresetPower( );
}

void RxProxy::setIFAtten(CORBA::Float atten,
                carma::antenna::common::RxControl::IF_Type ifType)
{
  ifSys_->setAtten( atten );
}

void RxProxy::setIFPower( CORBA::Float power )
{
  ifSys_->setPower( power );
}

void RxProxy::storeEncoderPosition(sza::util::Rx::Id rxId, short position)
{
  AntennaMasterMsg msg;
  msg.getRxMsg()->packCalTertMsg(sza::array::CALTERT_STORE_ENCODER,
				 rxId,
				 position,
				 sza::util::CalPos::NONE, false,  // not used
				 sza::util::CalTertTypes::MODULE, // not used
				 sza::util::CalTertTypes::READ,   // not used
				 sequenceNumber());
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

sza::antenna::corba::LOProxy* RxProxy::getLOProxy()
{
  return lo_;
}

sza::antenna::corba::IFProxy* RxProxy::getIFProxy()
{
  return ifSys_;
}

sza::antenna::corba::FrontEndProxy* RxProxy::getFrontEndProxy()
{
  return frontEnd_;
}

void RxProxy::setDefaultYigFrequency(Rx::Id rxId, Frequency freq)
{
  Rx::setYigFrequency(freq, parent_->getAnt()->getId(), rxId);
}

void RxProxy::setDefaultGunnFrequency(Rx::Id rxId, Frequency freq)
{
  Rx::setGunnFrequency(freq, parent_->getAnt()->getId());
}

void RxProxy::resetCanModule(const char* moduleName)
{
  COUT("Inside RxProxy::resetCanModule");

  AntennaMasterMsg msg;

  std::string moduleStr(moduleName);

  sza::util::CanModule::Id modules = sza::util::CanModule::NONE;

  if(moduleStr == "all") {
    modules = sza::util::CanModule::ALL;
  } else if(moduleStr == "bias") {
    modules = sza::util::CanModule::BIAS;
  } else if(moduleStr == "btg") {
    modules = sza::util::CanModule::BIAS;
  } else if(moduleStr == "caltert") {
    modules = sza::util::CanModule::CALTERT;
  } else if(moduleStr == "ifmod") {
    modules = sza::util::CanModule::IFMOD;
  } else if(moduleStr == "intmod") {
    modules = sza::util::CanModule::INTMOD;
  } else if(moduleStr == "receiver") {
    modules = sza::util::CanModule::RECEIVER;
  } else if(moduleStr == "rx") {
    modules = sza::util::CanModule::RECEIVER;
  } else if(moduleStr == "thermal") {
    modules = sza::util::CanModule::THERMAL;
  } else if(moduleStr == "tiltmeter") {
    modules = sza::util::CanModule::TILTMETER;
  } else if(moduleStr == "varactor") {
    modules = sza::util::CanModule::VARACTOR;
  } else if(moduleStr == "yig") {
    modules = sza::util::CanModule::YIG;
  }

  // If a valid module was specified, send the command

  if(modules != sza::util::CanModule::NONE) {
    msg.getRxMsg()->packResetMsg(modules, false);
    msg.getRxMsg()->setCarmaSequenceNumber();
    parent_->fwdTaskMsg(&msg);
  }
}

void RxProxy::resetCanBus()
{
  AntennaMasterMsg msg;
  msg.getRxMsg()->packResetMsg(sza::util::CanModule::ALL, true);
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}
