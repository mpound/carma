#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/antenna/sza/antenna/corba/FrontEndProxy.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::antenna::corba;
using namespace sza::util;

/**.......................................................................
 * Constructor with a pointer to the parent AntennaMaster
 */
FrontEndProxy::FrontEndProxy(AntennaMaster* parent) : Proxy(parent) {}

/**.......................................................................
 * Destructor function
 */
FrontEndProxy::~FrontEndProxy() {}

//-----------------------------------------------------------------------
// CORBA methods
//-----------------------------------------------------------------------

/**.......................................................................
 * Set up tuning for this receiver
 */
void FrontEndProxy::setFrequency(double freqInGHz)
{
  AntennaMasterMsg msg;

  // Decide which receiver we are being told to invoke

  if(freqInGHz > 20 && freqInGHz < 40) {
    msg.getRxMsg()->packSelectRxMsg(sza::util::Rx::RX30GHZ, sequenceNumber());
  } else if(freqInGHz > 80 && freqInGHz < 110) {
    msg.getRxMsg()->packSelectRxMsg(sza::util::Rx::RX90GHZ, sequenceNumber());
  } else {
    std::ostringstream os;
    os << "freq = " << freqInGHz << " GHz doesn't correspond to any valid SZA receiver";
    throw CARMA_EXCEPTION(carma::util::UserException, os.str().c_str());
  }

  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
};

/**.......................................................................
 * Set an SIS junction voltage
 */
void FrontEndProxy::setSISVj(float voltage)
{
  std::cout << "FrontEnd: setSISVj() stub" << std::endl;
};

/**.......................................................................
 * Set an SIS junction current
 */
void FrontEndProxy::setSISIj(float current)
{
  std::cout << "FrontEnd: setSISIj() stub" << std::endl;
};

/**.......................................................................
 * Do an I-V curve for an SIS Rx
 */
void FrontEndProxy::doIVcurve()
{
  std::cout << "FrontEnd: doIVcurve() stub" << std::endl;
};

/**.......................................................................
 * Retrieve an I-V curve for an SIS Rx
 */
carma::antenna::common::IVCurve* FrontEndProxy::getIVCurve()
{
  COUT("Inside FrontEndProxy::getIVCurve stub");
  return 0;
}

/**.......................................................................
 * Set a gate voltage
 */
void FrontEndProxy::setVG(carma::antenna::common::FrontEndControl::Amp amp,
			  carma::antenna::common::FrontEndControl::Stage stage,
			  float voltage)
{
  std::cout << "FrontEnd: setVG() stub" << std::endl;
};

/**.......................................................................
 * Set a drain voltage
 */
void FrontEndProxy::setVD(carma::antenna::common::FrontEndControl::Amp amp,
			  carma::antenna::common::FrontEndControl::Stage stage,
			  float voltage)
{
  std::cout << "FrontEnd: setVD() stub" << std::endl;
};

/**.......................................................................
 * Set a drain current
 */
void FrontEndProxy::setID(carma::antenna::common::FrontEndControl::Amp amp,
			  carma::antenna::common::FrontEndControl::Stage stage,
			  float current)
{
  std::cout << "FrontEnd: setID() stub" << std::endl;
};

/**.......................................................................
 * Set a mixer bias
 */
void FrontEndProxy::setMixer(float voltage)
{
  std::cout << "FrontEnd: setMixer() stub" << std::endl;
};

/**.......................................................................
 * Do an IV curve
 */
void FrontEndProxy::doIVcurve(float startVjInMv,
			      float stopVjInMv,
			      float stepVjInMv,
			      unsigned short deltaInMs,
			      bool doPower,
			      CORBA::ULong seqNo)
{
  std::cout << "FrontEnd: doIVcurve() stub" << std::endl;
};

/**.......................................................................
 * This is the main method by which we will store default bias settings
 */
void FrontEndProxy::setDefaultBias(unsigned iBias, short bias)
{
  std::cout << "setDefaultBias: iBias = " << iBias << " bias = " << bias << std::endl;

  AntennaMasterMsg msg;
  msg.getRxMsg()->packSetBiasMsg(iBias, bias, sza::array::RX, sza::util::Rx::RXALL, sequenceNumber(), true);
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

/**.......................................................................
 * This is the main method by which we will set all biases
 */
void FrontEndProxy::setBias(sza::util::Rx::Id rxId,
			    carma::antenna::common::FrontEndControl::Amp amp,
			    carma::antenna::common::FrontEndControl::Stage stage,
			    sza::util::Rx::Stage szaStage,
			    short bias,
			    unsigned long seq,
			    bool isDefault)
{
  if(szaStage == Rx::AmpInvalid) {
    std::ostringstream os;

    os << "amp = " << amp << ", stage = " << stage
       << " is an invalid combination for SZA " << rxId << " receiver";

    throw CARMA_EXCEPTION(carma::util::UserException, os.str().c_str());
  }

  AntennaMasterMsg msg;
  msg.getRxMsg()->packSetBiasMsg((unsigned) szaStage, bias, sza::array::AMP, rxId, seq, isDefault);
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

/**.......................................................................
 * Return the appropriate bias number for the requested gate
 * voltage of this receiver.
 */
sza::util::Rx::Stage FrontEndProxy::getVgStage(carma::antenna::common::FrontEndControl::Amp amp,
					       carma::antenna::common::FrontEndControl::Stage stage)
{
  return sza::util::Rx::AmpInvalid;
}

/**.......................................................................
 * Return the appropriate bias number for the requested drain
 * voltage of this receiver.
 */
sza::util::Rx::Stage FrontEndProxy::getVdStage(carma::antenna::common::FrontEndControl::Amp amp,
					       carma::antenna::common::FrontEndControl::Stage stage)
{
  return sza::util::Rx::AmpInvalid;
}

/**.......................................................................
 * Return the appropriate bias number for the requested drain
 * current of this receiver.
 */
sza::util::Rx::Stage FrontEndProxy::getIdStage(carma::antenna::common::FrontEndControl::Amp amp,
					       carma::antenna::common::FrontEndControl::Stage stage)
{
  return sza::util::Rx::AmpInvalid;
}
