#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/antenna/sza/antenna/corba/FrontEnd3mmProxy.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::antenna::corba;
using namespace sza::util;

/**.......................................................................
 * Constructor with a pointer to the parent AntennaMaster
 */
FrontEnd3mmProxy::FrontEnd3mmProxy(AntennaMaster* parent) :
  FrontEndProxy(parent) {}

/**.......................................................................
 * Destructor function
 */
FrontEnd3mmProxy::~FrontEnd3mmProxy() {}

/**.......................................................................
 * Set a gate voltage.
 */
void FrontEnd3mmProxy::setVG(carma::antenna::common::FrontEndControl::Amp amp,
			     carma::antenna::common::FrontEndControl::
			     Stage stage,
			     float voltage)
{
  // Receiver card expects a voltage in units of mV

  signed short bias = (short)(voltage * 1000);
  sza::util::Rx::Stage szaStage = getVgStage(amp, stage);
  setBias(Rx::RX3MM, amp, stage, szaStage, bias, sequenceNumber(), false);
}

/**.......................................................................
 * Set a drain voltage.
 */
void FrontEnd3mmProxy::setVD(carma::antenna::common::FrontEndControl::Amp amp,
			     carma::antenna::common::FrontEndControl::
			     Stage stage,
			     float voltage)
{
  // Receiver card expects a voltage in units of mV

  signed short bias = (short)(voltage * 1000);
  sza::util::Rx::Stage szaStage = getVdStage(amp, stage);
  setBias(Rx::RX3MM, amp, stage, szaStage, bias, sequenceNumber(), false);
}

/**.......................................................................
 * Set a drain current
 */
void FrontEnd3mmProxy::setID(carma::antenna::common::FrontEndControl::Amp amp,
			     carma::antenna::common::FrontEndControl::
			     Stage stage,
			     float current)
{
}

/**.......................................................................
 * Return the appropriate bias number for the requested gate voltage
 * of this receiver.
 */
sza::util::Rx::Stage FrontEnd3mmProxy::getVgStage(carma::antenna::common::FrontEndControl::Amp amp,
						  carma::antenna::common::FrontEndControl::Stage stage)
{
  switch (amp) {

  case carma::antenna::common::FrontEndControl::RF:

    switch (stage) {
    case carma::antenna::common::FrontEndControl::FIRST:
      return Rx::Amp90GHzRF1Stage1Vg;
      break;
    case carma::antenna::common::FrontEndControl::SECOND:
      return Rx::Amp90GHzRF1Stage2Vg;
      break;
    case carma::antenna::common::FrontEndControl::THIRD:
      return Rx::Amp90GHzRF2Stage1Vg;
      break;
    case carma::antenna::common::FrontEndControl::FOURTH:
      return Rx::Amp90GHzRF2Stage2Vg;
      break;
    }

    break;

  case carma::antenna::common::FrontEndControl::IF:

    switch (stage) {
    case carma::antenna::common::FrontEndControl::FIRST:
      return Rx::Amp90GHzIFVg;
      break;
    default:
      return Rx::AmpInvalid;
      break;
    }

    break;
  }
}

/**.......................................................................
 * Return the appropriate bias number for the requested drain current
 * of this receiver.
 */
sza::util::Rx::Stage FrontEnd3mmProxy::getVdStage(carma::antenna::common::FrontEndControl::Amp amp,
						  carma::antenna::common::FrontEndControl::Stage stage)
{
  switch (amp) {

  case carma::antenna::common::FrontEndControl::RF:

    switch (stage) {
    case carma::antenna::common::FrontEndControl::FIRST:
      return Rx::Amp90GHzRF1Vd;
      break;
    case carma::antenna::common::FrontEndControl::THIRD:
      return Rx::Amp90GHzRF2Vd;
      break;
    default:
      return Rx::AmpInvalid;
      break;
    }

    break;

  case carma::antenna::common::FrontEndControl::IF:

    switch (stage) {
    case carma::antenna::common::FrontEndControl::FIRST:
      return Rx::Amp90GHzIFVd;
      break;
    default:
      return Rx::AmpInvalid;
      break;
    }

    break;
  }
}
