#include <sstream>
#include <vector>

#include "carma/canbus/Utilities.h"

#include "carma/szautil/CanModule.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/YigBits.h"
#include "carma/szautil/YigFlags.h"

#include "carma/antenna/sza/antenna/canbus/Yig.h"
#include "carma/antenna/sza/antenna/control/SzaShare.h"

using namespace std;
using namespace sza::antenna::canbus;
using namespace sza::util;

using namespace carma::canbus;

/**.......................................................................
 * Constructor.
 */
Yig::Yig(sza::antenna::control::SzaShare* share, 
	 string boardName,
	 carma::canbus::nodeType node, 
	 carma::canbus::CanOutput& io) : 
  Oscillator(share, boardName, sza::util::CanModule::yigApiNo_, node, io) 
{
  monitor_.addMonitorPoint("lockState");
  monitor_.addMonitorPoint("lockStateMask");
  monitor_.addMonitorPoint("dataValid");
  monitor_.addMonitorPoint("current");
  monitor_.addMonitorPoint("frequency");
  monitor_.addMonitorPoint("loopGainResistance");
  monitor_.addMonitorPoint("dampingResistance");
  monitor_.addMonitorPoint("id");
  monitor_.addMonitorPoint("calDate");
  monitor_.addMonitorPoint("sweepStatus");
  monitor_.addMonitorPoint("refStatus");
  monitor_.addMonitorPoint("autoRelock");
  monitor_.addMonitorPoint("relockCount");
  monitor_.addMonitorPoint("lockBit");
  monitor_.addMonitorPoint("lockBitMask");
}

//-----------------------------------------------------------------------
// Commands from the host.
//-----------------------------------------------------------------------

/**.......................................................................
 * Send a message to set the frequency and start the lock procedure.
 */
std::vector<carma::canbus::Message>
Yig::setLoFrequency(unsigned short frequency, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;

  uShortToData(data, frequency);

  if(send)
    postMessage(HOSTCMD_SET_FREQUENCY, data);
  else
    msgs.push_back(nodeMessage(HOSTCMD_SET_FREQUENCY, data));

  return msgs;
}

/**.......................................................................
 * Send a message to download the tuning table _from_ the one-wire
 */
std::vector<carma::canbus::Message>
Yig::getTuningTable(bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;

  if(send)
    postMessage(HOSTCMD_GET_TUNING_TABLE, data);
  else
    msgs.push_back(nodeMessage(HOSTCMD_GET_TUNING_TABLE, data));

  return msgs;
}

/**.......................................................................
 * Return a map of controls.
 */
std::map<carma::canbus::msgType, std::string> 
Yig::getSpecificControls() const
{
  map<msgType, string> tmp; 
  tmp[HOSTCMD_TEST_MODE]     = "Yig::HOSTCMD_TEST_MODE";
  tmp[HOSTCMD_SET_FREQUENCY] = "Yig::HOSTCMD_SET_FREQUENCY";
  return tmp; 
}

/**.......................................................................
 * Return an inheritor-specific prefix to use when
 * constructing monitor points
 */
std::string Yig::controlPrefix()
{
  return std::string("Yig");
}

//-----------------------------------------------------------------------
// Engineering commands.
//-----------------------------------------------------------------------


/**.......................................................................
 * Download the YIG id
 */
std::vector<carma::canbus::Message>
Yig::downloadId(unsigned char id, unsigned char month,
		unsigned char day, unsigned char year, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  uByteToData(data, id);
  uByteToData(data, month);
  uByteToData(data, day);
  uByteToData(data, year);
  
  if(send)
    postMessage(ENGCMD_UPLOAD_YIGID, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_UPLOAD_YIGID, data));

  return msgs;
}

/**.......................................................................
 * Install a tuning table entry.
 */
std::vector<carma::canbus::Message>
Yig::downloadTuningTableEntry(unsigned short voltage, 
			      unsigned short frequency, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  uShortToData(data, voltage);  
  uShortToData(data, frequency);  
  
  if(send)
    postMessage(ENGCMD_DOWNLOAD_TUNING, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_DOWNLOAD_TUNING, data));

  return msgs;
}

/**.......................................................................
 * Install a tuning table entry.
 */
std::vector<carma::canbus::Message>
Yig::downloadTuningTableToOneWire(bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;

  if(send)
    postMessage(ENGCMD_DOWNLOAD_TO_ONEWIRE, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_DOWNLOAD_TO_ONEWIRE, data));

  return msgs;
}

/**.......................................................................
 * Install a tuning table entry.
 */
std::vector<carma::canbus::Message>
Yig::setOperatingVoltage(unsigned short voltage, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  uShortToData(data, voltage);  
  
  if(send)
    postMessage(ENGCMD_OPERATING_VOLTAGE, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_OPERATING_VOLTAGE, data));

  return msgs;
}

/**....................................................................... 
 * Set the loop gain resistance
 */
std::vector<carma::canbus::Message>
Yig::setLoopGainResistance(unsigned short voltage, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  uShortToData(data, voltage);  
  
  if(send)
    postMessage(ENGCMD_LOOP_GAIN_RESISTANCE, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_LOOP_GAIN_RESISTANCE, data));

  return msgs;
}

/**....................................................................... 
 * Set the damping gain resistance.
 */
std::vector<carma::canbus::Message>
Yig::setDampingResistance(unsigned short resistance, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  uShortToData(data, resistance);  
  
  if(send)
    postMessage(ENGCMD_DAMPING_RESISTANCE, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_DAMPING_RESISTANCE, data));

  return msgs;
}

/**....................................................................... 
 * Turn the sweep on/off
 */
std::vector<carma::canbus::Message>
Yig::turnSweepOn(bool on, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;

  uByteToData(data, on);
  
  if(send)
    postMessage(ENGCMD_SWEEP, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_SWEEP, data));

  return msgs;
}

/**....................................................................... 
 * Enable/Disable auto lock
 */
std::vector<carma::canbus::Message>
Yig::enableAutoLock(bool enable, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  uByteToData(data, enable);
  
  if(send)
    postMessage(ENGCMD_AUTOLOCK, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_AUTOLOCK, data));

  return msgs;
}

/**....................................................................... 
 * Set DAC calibation coefficient
 */
std::vector<carma::canbus::Message>
Yig::setDACCalibrationCoefficient(float coeff, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  floatToData(data, coeff);
  
  if(send)
    postMessage(ENGCMD_SET_DAC_COEFF, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_SET_DAC_COEFF, data));

  return msgs;
}

//-----------------------------------------------------------------------
// Monitor packet processing.
//-----------------------------------------------------------------------

/**.......................................................................
 * Process monitor packet 1.
 */
void Yig::processBlankingFrameMonitor1(vector<byteType>& data, bool isSim)
{
  // Extract the monitor points from the data vector.
  
  unsigned char lockState = dataToUbyte(data);

  monitor_.findMonitorPoint("lockState")->writeReg(isSim, lockState);
  monitor_.findMonitorPoint("dataValid")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("frequency")->writeReg(isSim, dataToUshort(data));
  monitor_.findMonitorPoint("loopGainResistance")->writeReg(isSim, dataToUshort(data));
  monitor_.findMonitorPoint("dampingResistance")->writeReg(isSim, dataToUshort(data));

  // Write an extended bitmask version of lockState

  monitor_.findMonitorPoint("lockStateMask")->writeReg(isSim, lockStateToBit(lockState));
}

/**.......................................................................
 * Convert from an integral lock state to an orthogonal bit so that
 * states can be unioned
 */
unsigned char Yig::lockStateToBit(unsigned char lockState)
{
  switch(lockState) {
  case UNLOCKED:
    return YigFlags::UNLOCKED;
    break;
  case SEARCHING:
    return YigFlags::SEARCHING;
    break;
  case REFINING:
    return YigFlags::REFINING;
    break;
  case LOCKED:
    return YigFlags::LOCKED;
    break;
  case RELOCK:
    return YigFlags::RELOCK;
    break;
  default:
    ThrowError("Unrecognized lock state");
    break;
  }
}

/**.......................................................................
 * Process monitor packet 2.
 */
void Yig::processBlankingFrameMonitor2(vector<byteType>& data, bool isSim)
{
  // Extract the monitor points from the data vector.
  
  monitor_.findMonitorPoint("ifLevel")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("errorVoltage")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("current")->writeReg(isSim, dataToUshort(data));
  monitor_.findMonitorPoint("noiseMeterVoltage")->writeReg(isSim, dataToShort(data));
}

/**.......................................................................
 * Process monitor packet 3
 */
void Yig::processBlankingFrameMonitor3(vector<byteType>& data, bool isSim)
{
  CoordRange range;
  
  // Extract the monitor points from the data vector.
  
  monitor_.findMonitorPoint("boardTemperature")->writeReg(isSim, dataToShort(data));
  
  for(unsigned int iChn=0; iChn <= 2; iChn++) {
    range.setIndex(iChn);
    monitor_.findMonitorPoint("maxChnl")->writeReg(isSim, dataToShort(data), &range);
  }
}

/**.......................................................................
 * Process monitor packet 4
 */
void Yig::processBlankingFrameMonitor4(vector<byteType>& data, bool isSim)
{
  // Extract the monitor points from the data vector.

  CoordRange range;
  for(unsigned int iChn=3; iChn <= 6; iChn++) {
    range.setIndex(iChn);
    monitor_.findMonitorPoint("maxChnl")->writeReg(isSim, dataToShort(data), &range);
  }
}

/**.......................................................................
 * Process monitor packet 5
 */
void Yig::processBlankingFrameMonitor5(vector<byteType>& data, bool isSim)
{
  // Extract the monitor points from the data vector.
  
  monitor_.findMonitorPoint("id")->writeReg(isSim, dataToUbyte(data));
  
  CoordRange range;
  for(unsigned int iChn=0; iChn <= 2; iChn++) {
    range.setIndex(iChn);
    monitor_.findMonitorPoint("calDate")->writeReg(isSim, dataToUbyte(data), &range); // month
  }
  
  // status of the hardware sweep
  
  monitor_.findMonitorPoint("sweepStatus")->writeReg(isSim, dataToUbyte(data)); 
  
  // Condition of the 10 MHz ref
  
  monitor_.findMonitorPoint("refStatus")->writeReg(isSim, dataToUbyte(data)); 
  
  // status of auto locking
  
  monitor_.findMonitorPoint("autoRelock")->writeReg(isSim, dataToUbyte(data)); 
  
  // Number of times the YIG has to relock
  
  monitor_.findMonitorPoint("relockCount")->writeReg(isSim, dataToUbyte(data)); 
}

/**.......................................................................
 * Process monitor packet 6
 */
void Yig::processBlankingFrameMonitor6(vector<byteType>& data, bool isSim)
{
  // Extract the monitor points from the data vector.
  
  unsigned char lockBit = dataToUbyte(data);

  monitor_.findMonitorPoint("lockBit")->writeReg(isSim, lockBit);

  // Write an extended bitmask version of lockBit

  monitor_.findMonitorPoint("lockBitMask")->writeReg(isSim, lockBitToBit(lockBit));
}

/**.......................................................................
 * Convert from an integral lock bit to an orthogonal bit so that
 * states can be unioned
 */
unsigned char Yig::lockBitToBit(unsigned char lockBit)
{
  switch(lockBit) {
  case BIT_UNLOCKED:
    return YigBits::UNLOCKED;
    break;
  case BIT_LOCKED:
    return YigBits::LOCKED;
    break;
  default:
    ThrowError("Unrecognized lock bit");
    break;
  }
}

/**.......................................................................
 * Return a map of half-second monitor packets.
 */ 
std::map<carma::canbus::msgType, std::string> 
Yig::getHalfSecMonitors() const 
{
  map<msgType, string> tmp; 
  
  tmp[MONITOR_PACKET_1] = "Yig::MONITOR_PACKET_1";
  tmp[MONITOR_PACKET_2] = "Yig::MONITOR_PACKET_2";
  tmp[MONITOR_PACKET_3] = "Yig::MONITOR_PACKET_3";
  tmp[MONITOR_PACKET_4] = "Yig::MONITOR_PACKET_4";
  tmp[MONITOR_PACKET_5] = "Yig::MONITOR_PACKET_5";
  tmp[MONITOR_PACKET_6] = "Yig::MONITOR_PACKET_6";
  
  return tmp; 
}

//-----------------------------------------------------------------------
// Status message processing
//-----------------------------------------------------------------------

/**.......................................................................
 * Return a map of half-second monitor points.
 */ 
std::map<carma::canbus::msgType, std::string>
Yig::getStatusMessages() const
{
  map<msgType, string> tmp; 
  
  tmp[STATUS_MSG_1] = "Yig::STATUS_MSG_1";
  
  return tmp;
}

/**.......................................................................
 * Process status message 1.
 */
void Yig::processStatusMessage1(bool isSim) 
{
}

