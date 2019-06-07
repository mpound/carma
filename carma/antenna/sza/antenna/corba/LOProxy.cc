#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/antenna/sza/antenna/corba/LOProxy.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::antenna::corba;
using namespace sza::util;

LOProxy::LOProxy(AntennaMaster* parent) : Proxy(parent) {}

// Destructor function

LOProxy::~LOProxy() {}

//-----------------------------------------------------------------------
// CORBA methods
//-----------------------------------------------------------------------

void LOProxy::setYigFrequency(double yigFreq)
{
  std::cout << "Calling: LOProxy::setYigFrequency stub" << std::endl;
}

void LOProxy::setLoFrequency(double frequency)
{
  std::cout << "Calling: LOProxy::setLoFrequency stub" << std::endl;
}

void LOProxy::toggleSweep(bool on)
{
  std::cout << "Calling: LOProxy::toggleSweep stub" << std::endl;
}

void LOProxy::toggleYigSweep(bool on)
{
  std::cout << "Calling: LOProxy::toggleYigSweep" << std::endl;

  AntennaMasterMsg msg;
  msg.getRxMsg()->packLoMsg(sza::array::LO_TOGGLE,
			    sza::util::LoOsc::YIG,
			    sza::util::LoStage::LO_SWEEP,
			    sza::util::Rx::RXALL, // ignored -- VARACTOR above determines the Rx
			    on, 0, 0, 0, // not used
			    0, // not used
			    ' ', ' ', ' ', ' ', ' ', 0.0, 0, 0, 0, 0); // not used
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

void LOProxy::setLoTerminatorAttenuation(unsigned short attenInDb)
{
  AntennaMasterMsg msg;
  msg.getRxMsg()->packIntModMsg(sza::array::INTMOD_SET_ATTEN, attenInDb);
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

void LOProxy::setPresetPower()
{
  AntennaMasterMsg msg;
  msg.getRxMsg()->packIntModMsg(sza::array::INTMOD_PRESET_POWER);
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

void LOProxy::setYigVoltage(unsigned short voltage)
{
  AntennaMasterMsg msg;
  msg.getRxMsg()->packLoMsg(sza::array::LO_VOLTAGE,
			    sza::util::LoOsc::YIG,
			    sza::util::LoStage::LO_ALL, // not used
			    sza::util::Rx::RXALL,
			    false, 0, 0, 0, // not used
			    voltage,
			    ' ', ' ', ' ', ' ', ' ', 0.0, 0, 0, 0, 0); // not used
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

void LOProxy::setYigLoopGain(unsigned short gain)
{
  AntennaMasterMsg msg;
  msg.getRxMsg()->packLoMsg(sza::array::LO_LOOPGAIN,
			    sza::util::LoOsc::YIG,
			    sza::util::LoStage::LO_ALL, // Not used
			    sza::util::Rx::RXALL,       // Not used
			    false, // state -- not used
			    0,    // Damping resistance, not used
			    0,    // Frequency, not used
			    gain, // Loop gain
			    0,    // Voltage, not used
			    ' ', ' ', ' ', ' ', ' ', 0.0, 0, 0, 0, 0); // not used
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

void LOProxy::setGunnLoopGain(unsigned short gain)
{
  AntennaMasterMsg msg;
  msg.getRxMsg()->packLoMsg(sza::array::LO_LOOPGAIN,
			    sza::util::LoOsc::GUNN,
			    sza::util::LoStage::LO_ALL, // Not used
			    sza::util::Rx::RXALL,       // Not used
			    false, // state -- not used
			    0,    // Damping resistance, not used
			    0,    // Frequency, not used
			    gain, // Loop gain
			    0,    // Voltage, not used
			    ' ', ' ', ' ', ' ', ' ', 0.0, 0, 0, 0, 0); // not used
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

void LOProxy::setVaractorLoopGain(unsigned short gain)
{
  AntennaMasterMsg msg;
  msg.getRxMsg()->packLoMsg(sza::array::LO_LOOPGAIN,
			    sza::util::LoOsc::VARACTOR,
			    sza::util::LoStage::LO_ALL, // Not used
			    sza::util::Rx::RXALL,       // Not used
			    false, // state -- not used
			    0,    // Damping resistance, not used
			    0,    // Frequency, not used
			    gain, // Loop gain
			    0,    // Voltage, not used
			    ' ', ' ', ' ', ' ', ' ', 0.0, 0, 0, 0, 0); // not used
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

void LOProxy::setYigDampingResistance(unsigned short resistance)
{
  AntennaMasterMsg msg;
  msg.getRxMsg()->packLoMsg(sza::array::LO_DAMPGAIN,
			    sza::util::LoOsc::YIG,
			    sza::util::LoStage::LO_ALL, // Not used
			    sza::util::Rx::RXALL,       // Not used
			    false, // state -- not used
			    resistance, // Damping resistance
			    0,    // Frequency, not used
			    0,    // Loop gain
			    0,    // Voltage, not used
			    ' ', ' ', ' ', ' ', ' ', 0.0, 0, 0, 0, 0); // not used
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

void LOProxy::setGunnVoltage(unsigned short voltage)
{
  AntennaMasterMsg msg;
  msg.getRxMsg()->packLoMsg(sza::array::LO_VOLTAGE,
			    sza::util::LoOsc::GUNN,
			    sza::util::LoStage::LO_ALL, // not used
			    sza::util::Rx::RXALL,
			    false, 0, 0, 0, // not used
			    voltage,
			    ' ', ' ', ' ', ' ', ' ', 0.0, 0, 0, 0, 0); // not used
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

void LOProxy::setDefaultGunnVoltage(unsigned short voltage)
{
  Voltage volts;
  volts.setCentiVolts(voltage);
  setDefaultGunnVoltage(volts);
}

//-----------------------------------------------------------------------
// Local methods
//-----------------------------------------------------------------------

void LOProxy::setYigFrequency(sza::util::Rx::Id rxId, double yigFreqInGHz)
{
  // CAN API expects the frequency in MHz

  unsigned short frequency = (unsigned short)(yigFreqInGHz * 1000);

  AntennaMasterMsg msg;
  msg.getRxMsg()->packLoMsg(sza::array::LO_LO_FREQ,
			    sza::util::LoOsc::YIG,
			    sza::util::LoStage::LO_ALL, // not used
			    rxId,
			    false,  0, // not used
			    frequency,
			    0, 0, ' ', ' ', ' ', ' ', ' ', 0.0, 0, 0, 0, 0); // not used
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

void LOProxy::setDefaultYigFrequency(sza::util::Frequency& freq)
{
  unsigned short frequency = (unsigned short)(freq.MHz());

  AntennaMasterMsg msg;
  msg.getRxMsg()->packLoMsg(sza::array::LO_DEFAULT_FREQ,
			    sza::util::LoOsc::YIG,
			    sza::util::LoStage::LO_ALL, // not used
			    sza::util::Rx::RXALL,
			    false, 0, // not used
			    frequency,
			    0, 0, ' ', ' ', ' ', ' ', ' ', 0.0, 0, 0, 0, 0); // not used
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

void LOProxy::setLoopGainResistance(sza::util::LoOsc::Osc osc, int loopGain)
{
  AntennaMasterMsg msg;
  msg.getRxMsg()->packLoMsg(sza::array::LO_LOOPGAIN,
			    osc,
			    sza::util::LoStage::LO_IF,
			    sza::util::Rx::RXNONE, // not used
			    false,  0, 0,  // Not used
			    loopGain,
			    0, ' ', ' ', ' ', ' ', ' ', 0.0, 0, 0, 0, 0); // not used
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

void LOProxy::setDampingGainResistance(int dampGain)
{
  AntennaMasterMsg msg;
  msg.getRxMsg()->packLoMsg(sza::array::LO_DAMPGAIN,
			    sza::util::LoOsc::ALL,
			    sza::util::LoStage::LO_IF,
			    sza::util::Rx::RXNONE,
			    false,
			    dampGain,
			    0,
			    0,
			    0,
			    ' ',
			    ' ',
			    ' ',
			    ' ',
			    ' ',
			    0.0,
			    0,
			    0,
			    0,
			    0);
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

void LOProxy::setDefaultLOTermAtten(sza::util::Attenuation& atten)
{
  AntennaMasterMsg msg;
  msg.getRxMsg()->packIntModMsg(sza::array::INTMOD_SET_DEFAULT_ATTEN, (unsigned char)atten.dB());
  parent_->fwdTaskMsg(&msg);
}

void LOProxy::setDefaultGunnVoltage(sza::util::Voltage& volt)
{
  unsigned short voltage = (unsigned short)(volt.gunnUnits());

  AntennaMasterMsg msg;
  msg.getRxMsg()->packLoMsg(sza::array::LO_DEFAULT_VOLTAGE,
			    sza::util::LoOsc::GUNN,
			    sza::util::LoStage::LO_ALL, // not used
			    sza::util::Rx::RXALL,
			    false, 0, 0, 0, // not used
			    voltage,
			    ' ', ' ', ' ', ' ', ' ', 0.0, 0, 0, 0, 0); // not used
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}
