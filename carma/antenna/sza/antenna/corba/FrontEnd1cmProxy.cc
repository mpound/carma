#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/antenna/sza/antenna/corba/FrontEnd1cmProxy.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::antenna::corba;
using namespace sza::util;
using namespace carma::antenna::common;

/**.......................................................................
 * Constructor with a pointer to the parent AntennaMaster
 */
FrontEnd1cmProxy::FrontEnd1cmProxy(AntennaMaster* parent) :
  FrontEndProxy(parent) {}

/**.......................................................................
 * Destructor function
 */
FrontEnd1cmProxy::~FrontEnd1cmProxy() {}

/**.......................................................................
 * Set a gate voltage.
 */
void FrontEnd1cmProxy::setVG(carma::antenna::common::FrontEndControl::Amp amp,
			     carma::antenna::common::FrontEndControl::Stage stage,
			     float voltage)
{
  // Receiver card expects a voltage in units of mV

  signed short bias = (short)(voltage * 1000);
  sza::util::Rx::Stage szaStage = getVgStage(amp, stage);
  setBias(Rx::RX1CM, amp, stage, szaStage, bias, sequenceNumber(), false);
}

/**.......................................................................
 * Set a drain voltage.
 */
void FrontEnd1cmProxy::setVD(carma::antenna::common::FrontEndControl::Amp amp,
			     carma::antenna::common::FrontEndControl::Stage stage,
			     float voltage)
{
  // Do nothing -- no drain voltages are settable for SZA 1cm receiver
}

/**.......................................................................
 * Set a drain current
 */
void FrontEnd1cmProxy::setID(carma::antenna::common::FrontEndControl::Amp amp,
			     carma::antenna::common::FrontEndControl::
			     Stage stage,
			     float current)
{
  // Receiver card expects a current in units of 0.01 mA

  signed short bias = (short)(current * 100);
  sza::util::Rx::Stage szaStage = getIdStage(amp, stage);
  setBias(Rx::RX1CM, amp, stage, szaStage, bias, sequenceNumber(), false);
}

/**.......................................................................
 * Return the appropriate bias number for the requested gate voltage
 * of this receiver.
 */
sza::util::Rx::Stage FrontEnd1cmProxy::getVgStage(carma::antenna::common::FrontEndControl::Amp amp,
						  carma::antenna::common::FrontEndControl::Stage stage)
{
  switch (amp) {

  case carma::antenna::common::FrontEndControl::RF:

    switch (stage) {
    case carma::antenna::common::FrontEndControl::FIRST:
      return Rx::Amp30GHzRFStage1Vg;
      break;
    case carma::antenna::common::FrontEndControl::SECOND:
      return Rx::Amp30GHzRFStage2Vg;
      break;
    case carma::antenna::common::FrontEndControl::THIRD:
      return Rx::Amp30GHzRFStage3Vg;
      break;
    case carma::antenna::common::FrontEndControl::FOURTH:
      return Rx::Amp30GHzRFStage4Vg;
      break;
    }

    break;

  case carma::antenna::common::FrontEndControl::IF:

    switch (stage) {
    case carma::antenna::common::FrontEndControl::FIRST:
      return Rx::Amp30GHzIF1;
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
sza::util::Rx::Stage FrontEnd1cmProxy::getIdStage(carma::antenna::common::FrontEndControl::Amp amp,
						  carma::antenna::common::FrontEndControl::Stage stage)
{
  switch (amp) {

  case carma::antenna::common::FrontEndControl::RF:

    switch (stage) {
    case carma::antenna::common::FrontEndControl::FIRST:
      return Rx::Amp30GHzRFStage1Id;
      break;
    case carma::antenna::common::FrontEndControl::SECOND:
      return Rx::Amp30GHzRFStage2Id;
      break;
    case carma::antenna::common::FrontEndControl::THIRD:
      return Rx::Amp30GHzRFStage3Id;
      break;
    case carma::antenna::common::FrontEndControl::FOURTH:
      return Rx::Amp30GHzRFStage4Id;
      break;
    }

    break;

  case carma::antenna::common::FrontEndControl::IF:

    switch (stage) {
    case carma::antenna::common::FrontEndControl::FIRST:
      return Rx::Amp30GHzIF1;
      break;
    default:
      return Rx::AmpInvalid;
      break;
    }

  default:
    return Rx::AmpInvalid;
    break;

  }
}
