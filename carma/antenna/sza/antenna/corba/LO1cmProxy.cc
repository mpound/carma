#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/antenna/sza/antenna/corba/LO1cmProxy.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::antenna::corba;
using namespace sza::util;

LO1cmProxy::LO1cmProxy(AntennaMaster* parent) : LOProxy(parent) {}

// Destructor function

LO1cmProxy::~LO1cmProxy() {}

void LO1cmProxy::setYigFrequency(double yigFreq)
{
  LOProxy::setYigFrequency(Rx::RX1CM, yigFreq);
}

void LO1cmProxy::setLoFrequency(double frequency)
{
  std::cout << "Calling: LO1cmProxy::setLoFrequency stub" << std::endl;
}

void LO1cmProxy::toggleSweep(bool on)
{
  AntennaMasterMsg msg;
  msg.getRxMsg()->packLoMsg(sza::array::LO_TOGGLE,
			    sza::util::LoOsc::VARACTOR,
			    sza::util::LoStage::LO_SWEEP,
			    sza::util::Rx::RXALL, // ignored -- VARACTOR above determines the Rx
			    on, 0, 0, 0, // not used
			    0, // not used
			    ' ', ' ', ' ', ' ', ' ', 0.0, 0, 0, 0, 0); // not used
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

