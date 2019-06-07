#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/Rx.h"

using namespace std;
using namespace sza::util;

// Rx LO frequencies

const Frequency Rx::rx30GHzLOFreq_   = Frequency(Frequency::MegaHz(), 10.0); 
const Frequency Rx::rx90GHzLOFreq_   = Frequency(Frequency::MegaHz(), 10.0); 
const Frequency Rx::rx230GHzLOFreq_  = Frequency(Frequency::MegaHz(), 10.0);

// Rx sky frequencies

// New sky frequency according to Mike, after fixing

const Frequency Rx::rx30GHzSkyFreq_  = Frequency(Frequency::GigaHz(),  35.938); 
const Frequency Rx::rx90GHzSkyFreq_  = Frequency(Frequency::GigaHz(),  89.780); 
const Frequency Rx::rx230GHzSkyFreq_ = Frequency(Frequency::GigaHz(), 230.000);

// Yig frequencies

const Frequency Rx::rx30GHzYigFreq_  = Frequency(Frequency::GigaHz(), 8.972);
const Frequency Rx::rx90GHzYigFreq_  = Frequency(Frequency::GigaHz(), 8.972);
const Frequency Rx::rx230GHzYigFreq_ = Frequency(Frequency::GigaHz(), 8.972);

Frequency Rx::rx30GHzYigFreqs_[AntNum::NANT];
Frequency Rx::rx90GHzYigFreqs_[AntNum::NANT];
Frequency Rx::rx230GHzYigFreqs_[AntNum::NANT];

Voltage   Rx::gunnVoltages_[AntNum::NANT];
Frequency Rx::gunnFrequencies_[AntNum::NANT];

//------------------------------------------------------------
// IF Attenuations
//------------------------------------------------------------

Attenuation Rx::rx30GHzIfTotalAttensSky_[AntNum::NANT];
Attenuation Rx::rx90GHzIfTotalAttensSky_[AntNum::NANT];
Attenuation Rx::rx230GHzIfTotalAttensSky_[AntNum::NANT];

Attenuation Rx::rx30GHzIfInputAttensSky_[AntNum::NANT];
Attenuation Rx::rx90GHzIfInputAttensSky_[AntNum::NANT];
Attenuation Rx::rx230GHzIfInputAttensSky_[AntNum::NANT];

Attenuation Rx::rx30GHzIfOutputAttensSky_[AntNum::NANT];
Attenuation Rx::rx90GHzIfOutputAttensSky_[AntNum::NANT];
Attenuation Rx::rx230GHzIfOutputAttensSky_[AntNum::NANT];

Attenuation Rx::rx30GHzIfTotalAttensLoad_[AntNum::NANT];
Attenuation Rx::rx90GHzIfTotalAttensLoad_[AntNum::NANT];
Attenuation Rx::rx230GHzIfTotalAttensLoad_[AntNum::NANT];

Attenuation Rx::rx30GHzIfInputAttensLoad_[AntNum::NANT];
Attenuation Rx::rx90GHzIfInputAttensLoad_[AntNum::NANT];
Attenuation Rx::rx230GHzIfInputAttensLoad_[AntNum::NANT];

Attenuation Rx::rx30GHzIfOutputAttensLoad_[AntNum::NANT];
Attenuation Rx::rx90GHzIfOutputAttensLoad_[AntNum::NANT];
Attenuation Rx::rx230GHzIfOutputAttensLoad_[AntNum::NANT];

/**.......................................................................
 * Constructors
 */
Rx::Rx(Rx::Id rxId) 
{
  rxId_  = rxId;
  antId_ = AntNum::ANTNONE;

  rxIsSet_  = true;
  antIsSet_ = false;
}

Rx::Rx(AntNum::Id antId, Rx::Id rxId) 
{
  rxId_  = rxId;
  antId_ = antId;

  rxIsSet_  = true;
  antIsSet_ = true;
}


/**.......................................................................
 * Destructor.
 */
Rx::~Rx() {}

/**.......................................................................
 * Public method to return the center sky RF frequency, in Hz,
 * corresponding to this receiver. 
 */
Frequency Rx::getSkyFrequency(Rx::Id id)
{
  LogStream errStr;

  switch (id) {
  case RX30GHZ:
    return rx30GHzSkyFreq_;
    break;
  case RX90GHZ:
    return rx90GHzSkyFreq_;
    break;
  case RX230GHZ:
    return rx230GHzSkyFreq_;
    break;
  default:
    errStr.appendMessage(true, "Invalid Rx Id");
    throw Error(errStr);
    break;
  }
}

Frequency Rx::getSkyFrequency()
{
  return getSkyFrequency(rxId_);
}

/**.......................................................................
 * Public method to return the center freqeuncy, in Hz
 * corresponding to this receiver. 
 */
Frequency Rx::getLOFrequency(Rx::Id id)
{
  LogStream errStr;

  switch (id) {
  case RX30GHZ:
    return rx30GHzLOFreq_;
    break;
  case RX90GHZ:
    return rx90GHzLOFreq_;
    break;
  case RX230GHZ:
    return rx230GHzLOFreq_;
    break;
  default:
    errStr.appendMessage(true, "Invalid Rx Id");
    throw Error(errStr);
    break;
  }
}

Frequency Rx::getLOFrequency()
{
  return getLOFrequency(rxId_);
}

/**.......................................................................
 * Public method to return the center freqeuncy, in Hz
 * corresponding to this receiver. 
 */
Frequency Rx::getYigFrequency(Rx::Id id)
{
  LogStream errStr;

  switch (id) {
  case RX30GHZ:
    return rx30GHzYigFreq_;
    break;
  case RX90GHZ:
    return rx90GHzYigFreq_;
    break;
  case RX230GHZ:
    return rx230GHzYigFreq_;
    break;
  default:
    errStr.appendMessage(true, "Invalid Rx Id");
    throw Error(errStr);
    break;
  }
}

/**.......................................................................
 * Public method to set the default yig frequency corresponding to
 * the specified receivers.
 */
void Rx::setYigFrequency(Frequency freq, AntNum::Id antId, Rx::Id id)
{
  LogStream errStr;
  AntNum antennas(antId);

  // Iterate over antennas, checking which ones were requested

  for(AntNum antiter(AntNum::ANT0); antiter <= AntNum(AntNum::ANTMAX); antiter++) {
    if(antennas.isSet(antiter)) {
      unsigned iant = antiter.getIntId();

      if(id & (unsigned int)RX30GHZ)
	rx30GHzYigFreqs_[iant]  = freq;
      
      if(id & (unsigned int)RX90GHZ)
	rx90GHzYigFreqs_[iant]  = freq;
      
      if(id & (unsigned int)RX230GHZ)
	rx230GHzYigFreqs_[iant] = freq;
    }
  }
}

/**.......................................................................
 * Public method to return the center frequency, in Hz
 * corresponding to this receiver. 
 */
Frequency Rx::getYigFrequency(AntNum::Id antId, Rx::Id id)
{
  LogStream errStr;
  unsigned iant = AntNum::idToInt(antId);

  switch (id) {
  case RX30GHZ:
    if(rx30GHzYigFreqs_[iant].Hz() > 0.0)
      return rx30GHzYigFreqs_[iant];
    else
      return rx30GHzYigFreq_;
    break;
  case RX90GHZ:
    if(rx90GHzYigFreqs_[iant].Hz() > 0.0)
      return rx90GHzYigFreqs_[iant];
    else
      return rx90GHzYigFreq_;
    break;
  case RX230GHZ:
    if(rx230GHzYigFreqs_[iant].Hz() > 0.0)
      return rx230GHzYigFreqs_[iant];
    else
      return rx230GHzYigFreq_;
    break;
  default:
    errStr.appendMessage(true, "Invalid Rx Id");
    throw Error(errStr);
    break;
  }
}

Frequency Rx::getYigFrequency()
{
  return getYigFrequency(rxId_);
}

/**.......................................................................
 * Return the switch position corresponding to a given rx
 */
Rx::Id Rx::switchPosToRx(unsigned char pos)
{
  switch (pos) {
  case RX30GHZ_IFSWITCHPOS:
    return RX30GHZ;
    break;
  case RX90GHZ_IFSWITCHPOS:
    return RX90GHZ;
    break;
  case RX230GHZ_IFSWITCHPOS:
    return RX230GHZ;
    break;
  default:
    return RXUNKNOWN;
    break;
  }
}

/**.......................................................................
 * Return the switch position corresponding to a given rx
 */
unsigned char Rx::rxToIFSwitchPos(Rx::Id rxId)
{
  switch (rxId) {
  case RX30GHZ:
    return RX30GHZ_IFSWITCHPOS;
    break;
  case RX90GHZ:
    return RX90GHZ_IFSWITCHPOS;
    break;
  case RX230GHZ:
    return RX230GHZ_IFSWITCHPOS;
    break;
  default:
    return UNUSED_IFSWITCHPOS;
    break;
  }
}

/**.......................................................................
 * Set the IF attenuations for each antenna
 */
void Rx::setIfTotalAtten(Attenuation atten, AntNum::Id antId, Rx::Id rxId, CalPos::Pos pos)
{
  AntNum antennas(antId);

  // Iterate over antennas, checking which ones were requested
  
  for(AntNum antiter(AntNum::ANT0); antiter <= AntNum(AntNum::ANTMAX); antiter++) {
    if(antennas.isSet(antiter)) {
      unsigned iant = antiter.getIntId();

      if(rxId & (unsigned int)RX30GHZ) {
	if(pos & (unsigned int)CalPos::SKY)
	  rx30GHzIfTotalAttensSky_[iant]  = atten;
	if(pos & (unsigned int)CalPos::HOTLOAD)
	  rx30GHzIfTotalAttensLoad_[iant]  = atten;
      }
      
      if(rxId & (unsigned int)RX90GHZ) {
	if(pos & (unsigned int)CalPos::SKY)
	  rx90GHzIfTotalAttensSky_[iant]  = atten;
	if(pos & (unsigned int)CalPos::HOTLOAD)
	  rx90GHzIfTotalAttensLoad_[iant]  = atten;
      }
      
      if(rxId & (unsigned int)RX230GHZ) {
	if(pos & (unsigned int)CalPos::SKY)
	  rx230GHzIfTotalAttensSky_[iant]  = atten;
	if(pos & (unsigned int)CalPos::HOTLOAD)
	  rx230GHzIfTotalAttensLoad_[iant]  = atten;
      }

    }

  }

}

void Rx::setIfInputAtten(Attenuation atten, AntNum::Id antId, Rx::Id rxId, CalPos::Pos pos)
{
  AntNum antennas(antId);
  
  // Iterate over antennas, checking which ones were requested
  
  for(AntNum antiter(AntNum::ANT0); antiter <= AntNum(AntNum::ANTMAX); antiter++) {
    if(antennas.isSet(antiter)) {
      unsigned iant = antiter.getIntId();

      if(rxId & (unsigned int)RX30GHZ) {
	if(pos & (unsigned int)CalPos::SKY)
	  rx30GHzIfInputAttensSky_[iant]  = atten;
	if(pos & (unsigned int)CalPos::HOTLOAD)
	  rx30GHzIfInputAttensLoad_[iant]  = atten;
      }
      
      if(rxId & (unsigned int)RX90GHZ) {
	if(pos & (unsigned int)CalPos::SKY)
	  rx90GHzIfInputAttensSky_[iant]  = atten;
	if(pos & (unsigned int)CalPos::HOTLOAD)
	  rx90GHzIfInputAttensLoad_[iant]  = atten;
      }
      
      if(rxId & (unsigned int)RX230GHZ) {
	if(pos & (unsigned int)CalPos::SKY)
	  rx230GHzIfInputAttensSky_[iant]  = atten;
	if(pos & (unsigned int)CalPos::HOTLOAD)
	  rx230GHzIfInputAttensLoad_[iant]  = atten;
      }

    }
  }
}

void Rx::setIfOutputAtten(Attenuation atten, AntNum::Id antId, Rx::Id rxId, CalPos::Pos pos)
{
  AntNum antennas(antId);

  // Iterate over antennas, checking which ones were requested
  
  for(AntNum antiter(AntNum::ANT0); antiter <= AntNum(AntNum::ANTMAX); antiter++) {
    if(antennas.isSet(antiter)) {
      unsigned iant = antiter.getIntId();

      if(rxId & (unsigned int)RX30GHZ) {
	if(pos & (unsigned int)CalPos::SKY)
	  rx30GHzIfOutputAttensSky_[iant]  = atten;
	if(pos & (unsigned int)CalPos::HOTLOAD)
	  rx30GHzIfOutputAttensLoad_[iant]  = atten;
      }
      
      if(rxId & (unsigned int)RX90GHZ) {
	if(pos & (unsigned int)CalPos::SKY)
	  rx90GHzIfOutputAttensSky_[iant]  = atten;
	if(pos & (unsigned int)CalPos::HOTLOAD)
	  rx90GHzIfOutputAttensLoad_[iant]  = atten;
      }
      
      if(rxId & (unsigned int)RX230GHZ) {
	if(pos & (unsigned int)CalPos::SKY)
	  rx230GHzIfOutputAttensSky_[iant]  = atten;
	if(pos & (unsigned int)CalPos::HOTLOAD)
	  rx230GHzIfOutputAttensLoad_[iant]  = atten;
      }

    }
  }
}

/**.......................................................................
 * Return the IF total attenuations for each antenna
 */
Attenuation Rx::getIfTotalAtten(AntNum::Id antId, Rx::Id rxId, CalPos::Pos pos)
{
  AntNum ant(antId);
  unsigned iant = ant.getIntId();

  switch(rxId) {
  case RX30GHZ:

    switch(pos) {
    case CalPos::SKY:
      return rx30GHzIfTotalAttensSky_[iant];
      break;
    case CalPos::HOTLOAD:
      return rx30GHzIfTotalAttensLoad_[iant];
      break;
    default:
      ThrowError("Unrecognized position: " << pos);
      break;
    }

    break;

  case RX90GHZ:

    switch(pos) {
    case CalPos::SKY:
      return rx90GHzIfTotalAttensSky_[iant];
      break;
    case CalPos::HOTLOAD:
      return rx90GHzIfTotalAttensLoad_[iant];
      break;
    default:
      ThrowError("Unrecognized position: " << pos);
      break;
    }

    break;

  case RX230GHZ:

    switch(pos) {
    case CalPos::SKY:
      return rx230GHzIfTotalAttensSky_[iant];
      break;
    case CalPos::HOTLOAD:
      return rx230GHzIfTotalAttensLoad_[iant];
      break;
    default:
      ThrowError("Unrecognized position: " << pos);
      break;
    }

    break;

  default:
    ThrowError("Unrecognized receiver");
    break;
  }
}

Attenuation Rx::getIfTotalAtten(CalPos::Pos pos)
{
  return getIfTotalAtten(antId_, rxId_, pos);
}

/**.......................................................................
 * Return the IF input attenuations for each antenna
 */
Attenuation Rx::getIfInputAtten(AntNum::Id antId, Rx::Id rxId, CalPos::Pos pos)
{
  AntNum ant(antId);
  unsigned iant = ant.getIntId();

  switch(rxId) {
  case RX30GHZ:

    switch(pos) {
    case CalPos::SKY:
      return rx30GHzIfInputAttensSky_[iant];
      break;
    case CalPos::HOTLOAD:
      return rx30GHzIfInputAttensLoad_[iant];
      break;
    default:
      ThrowError("Unrecognized position: " << pos);
      break;
    }

    break;

  case RX90GHZ:

    switch(pos) {
    case CalPos::SKY:
      return rx90GHzIfInputAttensSky_[iant];
      break;
    case CalPos::HOTLOAD:
      return rx90GHzIfInputAttensLoad_[iant];
      break;
    default:
      ThrowError("Unrecognized position: " << pos);
      break;
    }

    break;

  case RX230GHZ:

    switch(pos) {
    case CalPos::SKY:
      return rx230GHzIfInputAttensSky_[iant];
      break;
    case CalPos::HOTLOAD:
      return rx230GHzIfInputAttensLoad_[iant];
      break;
    default:
      ThrowError("Unrecognized position: " << pos);
      break;
    }

    break;

  default:
    ThrowError("Unrecognized receiver");
    break;
  }
}

Attenuation Rx::getIfInputAtten(CalPos::Pos pos)
{
  return getIfInputAtten(antId_, rxId_, pos);
}

/**.......................................................................
 * Return the IF output attenuations for each antenna
 */
Attenuation Rx::getIfOutputAtten(AntNum::Id antId, Rx::Id rxId, CalPos::Pos pos)
{
  AntNum ant(antId);
  unsigned iant = ant.getIntId();

  switch(rxId) {
  case RX30GHZ:

    switch(pos) {
    case CalPos::SKY:
      return rx30GHzIfOutputAttensSky_[iant];
      break;
    case CalPos::HOTLOAD:
      return rx30GHzIfOutputAttensLoad_[iant];
      break;
    default:
      ThrowError("Unrecognized position: " << pos);
      break;
    }

    break;

  case RX90GHZ:

    switch(pos) {
    case CalPos::SKY:
      return rx90GHzIfOutputAttensSky_[iant];
      break;
    case CalPos::HOTLOAD:
      return rx90GHzIfOutputAttensLoad_[iant];
      break;
    default:
      ThrowError("Unrecognized position: " << pos);
      break;
    }

    break;

  case RX230GHZ:

    switch(pos) {
    case CalPos::SKY:
      return rx230GHzIfOutputAttensSky_[iant];
      break;
    case CalPos::HOTLOAD:
      return rx230GHzIfOutputAttensLoad_[iant];
      break;
    default:
      ThrowError("Unrecognized position: " << pos);
      break;
    }

    break;

  default:
    ThrowError("Unrecognized receiver");
    break;
  }
}

Attenuation Rx::getIfOutputAtten(CalPos::Pos pos)
{
  return getIfOutputAtten(antId_, rxId_, pos);
}

/**.......................................................................
 * Public method to set the Gunn operating voltage corresponding
 * to this receiver.
 */
void Rx::setGunnVoltage(Voltage voltage, AntNum::Id antId)
{
  COUT("Inside setGunnVoltage: voltage = " << voltage);

  AntNum antennas(antId);

  // Iterate over antennas, checking which ones were requested
  
  for(AntNum antiter(AntNum::ANT0); antiter <= AntNum(AntNum::ANTMAX); antiter++) {
    if(antennas.isSet(antiter)) {
      unsigned iant = antiter.getIntId();
      COUT("Setting voltage for iant = " << iant << " to : " << voltage);
      gunnVoltages_[iant] = voltage;
    }
  }
}
      
/**.......................................................................
 * Public method to return the Gunn operating voltage
 * corresponding to this receiver.
 */
Voltage Rx::getGunnVoltage(AntNum::Id antId)
{
  unsigned iant = AntNum::idToInt(antId);
  return gunnVoltages_[iant];
}

/**.......................................................................
 * Public method to set the Gunn operating Frequency corresponding
 * to this receiver.
 */
void Rx::setGunnFrequency(Frequency frequency, AntNum::Id antId)
{
  COUT("Inside setGunnFrequency: frequency = " << frequency);

  AntNum antennas(antId);

  // Iterate over antennas, checking which ones were requested
  
  for(AntNum antiter(AntNum::ANT0); antiter <= AntNum(AntNum::ANTMAX); antiter++) {
    if(antennas.isSet(antiter)) {
      unsigned iant = antiter.getIntId();
      COUT("Setting Gunn frequency for iant = " << iant << " to : " << frequency);
      gunnFrequencies_[iant] = frequency;
    }
  }
}
      
/**.......................................................................
 * Public method to return the Gunn operating frequency
 * corresponding to this receiver.
 */
Frequency Rx::getGunnFrequency(AntNum::Id antId)
{
  unsigned iant = AntNum::idToInt(antId);
  return gunnFrequencies_[iant];
}
