#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/antenna/sza/antenna/corba/LO3mmProxy.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::antenna::corba;
using namespace sza::util;

LO3mmProxy::LO3mmProxy(AntennaMaster* parent) : LOProxy(parent) {}

// Destructor function

LO3mmProxy::~LO3mmProxy() {}

void LO3mmProxy::setYigFrequency(double yigFreq)
{
  LOProxy::setYigFrequency(Rx::RX3MM, yigFreq);
}

void LO3mmProxy::setLoFrequency(double frequency)
{
  std::cout << "Calling: LO3mmProxy::setLoFrequency stub" << std::endl;
}

void LO3mmProxy::toggleSweep(bool on)
{
  AntennaMasterMsg msg;
  msg.getRxMsg()->packLoMsg(sza::array::LO_TOGGLE,
			    sza::util::LoOsc::GUNN,
			    sza::util::LoStage::LO_SWEEP, // not used
			    sza::util::Rx::RXALL,
			    on, 0, 0, 0, // not used
			    0, // not used
			    ' ', ' ', ' ', ' ', ' ', 0.0, 0, 0, 0, 0); // not used
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

