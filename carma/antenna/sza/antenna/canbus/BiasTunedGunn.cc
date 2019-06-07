#include <sstream>
#include <vector>

#include "carma/canbus/Utilities.h"

#include "carma/szautil/CanModule.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LoStage.h"

#include "carma/antenna/sza/antenna/canbus/BiasTunedGunn.h"

using namespace std;
using namespace sza::antenna::canbus;
using namespace sza::util;
using namespace carma::canbus;

/**.......................................................................
 * Constructor.
 */
BiasTunedGunn::BiasTunedGunn(sza::antenna::control::SzaShare* share, 
			     string boardName,
			     carma::canbus::nodeType node,
			     carma::canbus::CanOutput& io) :
Oscillator(share, boardName, sza::util::CanModule::biasApiNo_, node, io) 
{
  monitor_.addMonitorPoint("phaseLockState");
  monitor_.addMonitorPoint("hwLockStatus");
  monitor_.addMonitorPoint("refLockStatus");
  monitor_.addMonitorPoint("sweepStatus");
  monitor_.addMonitorPoint("gunnStatus");
  monitor_.addMonitorPoint("dataValid");
  monitor_.addMonitorPoint("autoRelock");
  monitor_.addMonitorPoint("relockCount");

  monitor_.addMonitorPoint("gunnId");
  monitor_.addMonitorPoint("gunnVoltage");
  monitor_.addMonitorPoint("multiplier");
  monitor_.addMonitorPoint("freqRangeCheck");
  monitor_.addMonitorPoint("ifMonState");

  monitor_.addMonitorPoint("calTableState");
  monitor_.addMonitorPoint("calMonth");
  monitor_.addMonitorPoint("calDay");
  monitor_.addMonitorPoint("calYear");

  monitor_.addMonitorPoint("calDate");

  monitor_.addMonitorPoint("numZabers");
  monitor_.addMonitorPoint("allZabers");

  monitor_.addMonitorPoint("gunnFrequency");
  monitor_.addMonitorPoint("loopGain");

  monitor_.addMonitorPoint("tunerPosition");
  monitor_.addMonitorPoint("backShortPosition");
  monitor_.addMonitorPoint("attenuatorPosition");

  monitor_.addMonitorPoint("gunnCurrent");
  monitor_.addMonitorPoint("pos24VAnalogVoltage");
  monitor_.addMonitorPoint("pos5VDigitalVoltage");
  monitor_.addMonitorPoint("pos15VAnalogVoltage");

  monitor_.addMonitorPoint("pos12VAnalogVoltage");
  monitor_.addMonitorPoint("pos5VAnalogVoltage");
  monitor_.addMonitorPoint("neg12VAnalogVoltage");
  monitor_.addMonitorPoint("pos6VAnalogVoltage");

  monitor_.addMonitorPoint("crowbarState");
  monitor_.addMonitorPoint("crowbarCount");

  // For monitoring only.  
 
  monitor_.addMonitorPoint("lockStatusError");

  // Intialize the current receiver id and lock status to unknown

  rxId_ = sza::util::Rx::RXUNKNOWN;
  hwLockStatus_ = 0;
}

//-----------------------------------------------------------------------
// Commands from the host.
//-----------------------------------------------------------------------

/**.......................................................................
 * Return a map of controls.
 */
std::map<carma::canbus::msgType, std::string> 
BiasTunedGunn::getSpecificControls() const
{
  map<msgType, string> tmp; 
  
  tmp[HOSTCMD_SET_LO_FREQ]        = "BiasTunedGunn::HOSTCMD_SET_LO_FREQ";
  tmp[HOSTCMD_SET_GUNN_VOLTAGE]   = "BiasTunedGunn::HOSTCMD_SET_GUNN_VOLTAGE";
  tmp[HOSTCMD_SET_LOOP_GAIN]      = "BiasTunedGunn::HOSTCMD_SET_LOOP_GAIN";

  tmp[HOSTCMD_SET_GUNN_STATE]     = "BiasTunedGunn::HOSTCMD_SET_GUNN_STATE";
  tmp[HOSTCMD_SET_SWEEP_STATE]    = "BiasTunedGunn::HOSTCMD_SET_SWEEP_STATE";
  tmp[HOSTCMD_SET_IFMON_STATE]    = "BiasTunedGunn::HOSTCMD_SET_IFMON_STATE";

  tmp[HOSTCMD_SET_TUNER_POS]      = "BiasTunedGunn::HOSTCMD_SET_TUNER_POS";
  tmp[HOSTCMD_SET_BACKSHORT_POS]  = "BiasTunedGunn::HOSTCMD_SET_BACKSHORT_POS";
  tmp[HOSTCMD_SET_ATTEN_POS]      = "BiasTunedGunn::HOSTCMD_SET_ATTEN_POS";

  tmp[HOSTCMD_JOG_TUNER]          = "BiasTunedGunn::HOSTCMD_JOG_TUNER";
  tmp[HOSTCMD_JOG_BACKSHORT]      = "BiasTunedGunn::HOSTCMD_JOG_BACKSHORT";
  tmp[HOSTCMD_JOG_ATTEN]          = "BiasTunedGunn::HOSTCMD_JOG_ATTEN";

  tmp[HOSTCMD_TOGGLE_AUTO_RELOCK] = "BiasTunedGunn::HOSTCMD_TOGGLE_AUTO_RELOCK";
  tmp[HOSTCMD_HOME_ACTUATORS]     = "BiasTunedGunn::HOSTCMD_HOME_ACTUATORS";
  
  return tmp; 
}

/**.......................................................................
 * Return an inheritor-specific prefix to use when
 * constructing monitor points
 */
std::string BiasTunedGunn::controlPrefix()
{
  return std::string("BiasTunedGunn");
}

//-----------------------------------------------------------------------
// Engineering commands.
//-----------------------------------------------------------------------

/**.......................................................................
 * Install a tuning table entry.
 */
std::vector<carma::canbus::Message>
BiasTunedGunn::downloadTuningTableEntry(unsigned int frequency,
					unsigned int tunerPos,
					unsigned int backshortPos,
					unsigned int attenPos,
					bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data1, data2;
  
  uShortToData(data1, frequency);
  uShortToData(data1, tunerPos);

  uShortToData(data2, backshortPos);
  uShortToData(data2, attenPos);
  
  if(send) {
    postMessage(ENGCMD_DL_TUNING_TABLE, data1);
    postMessage(ENGCMD_DL_TUNING_TABLE, data2);
  } else {
    msgs.push_back(nodeMessage(ENGCMD_DL_TUNING_TABLE, data1));
    msgs.push_back(nodeMessage(ENGCMD_DL_TUNING_TABLE, data2));
  }
  
  return msgs;
}

/**.......................................................................
 * Set the operating frequency.
 */
std::vector<carma::canbus::Message>
BiasTunedGunn::setLoFrequency(unsigned short frequency, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;

  // Stuff the data byte into the data packet

  uShortToData(data, frequency);

  COUT("Sending lo frequency: " << frequency);

  // Now send/store the message

  if(send)
    postMessage(HOSTCMD_SET_LO_FREQ, data);
  else
    msgs.push_back(nodeMessage(HOSTCMD_SET_LO_FREQ, data));
  
  return msgs;
}

/**.......................................................................
 * Set the Gunn voltage
 */
std::vector<carma::canbus::Message>
BiasTunedGunn::setVoltage(unsigned short voltage, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;

  // Stuff the data byte into the data packet

  uShortToData(data, voltage);

  COUT("Sending voltage: " << voltage);

  // Now send/store the message

  if(send)
    postMessage(HOSTCMD_SET_GUNN_VOLTAGE, data);
  else
    msgs.push_back(nodeMessage(HOSTCMD_SET_GUNN_VOLTAGE, data));
  
  return msgs;
}

/**.......................................................................
 * Set the Loop gain
 */
std::vector<carma::canbus::Message>
BiasTunedGunn::setLoopGainResistance(unsigned short gain, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;

  // Stuff the data byte into the data packet

  uShortToData(data, gain);

  COUT("Sending gain: " << gain);

  // Now send/store the message

  if(send)
    postMessage(HOSTCMD_SET_LOOP_GAIN, data);
  else
    msgs.push_back(nodeMessage(HOSTCMD_SET_LOOP_GAIN, data));
  
  return msgs;
}

/**....................................................................... 
 * Turn the gunn on/off
 */
std::vector<carma::canbus::Message>
BiasTunedGunn::setDeviceState(unsigned device, bool state, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;

  // Stuff the data byte into the data packet

  uByteToData(data, state);

  // Now see which message to send
  
  carma::canbus::msgType mid;

  COUT("Commanding device state = " << device << " state = " << state);

  switch (device) {
  case LoStage::LO_GUNN:
    mid = HOSTCMD_SET_GUNN_STATE;
    break;
  case LoStage::LO_SWEEP:
    mid = HOSTCMD_SET_SWEEP_STATE;
    break;
  case LoStage::LO_IFMONITOR:
    mid = HOSTCMD_SET_IFMON_STATE;
    break;
  default:
    ThrowError("Unrecognized LoStage id: " << device);
    break;
  }

  // Now send/store the message

  if(send)
    postMessage(mid, data);
  else
    msgs.push_back(nodeMessage(mid, data));

  COUT("Inside 2");  
  return msgs;
}

/** .......................................................................
 * Set the position for the requested device
 */
std::vector<carma::canbus::Message>
BiasTunedGunn::setDevicePosition(unsigned device, unsigned int position, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;

  // Stuff the data byte into the data packet

  uLongToData(data, position);

  // Now see which message to send
  
  carma::canbus::msgType mid;

  COUT("Commanding device position = " << device << " position = " << position);

  switch (device) {
  case LoStage::LO_TUNER:
    mid = HOSTCMD_SET_TUNER_POS;
    break;
  case LoStage::LO_BACKSHORT:
    mid = HOSTCMD_SET_BACKSHORT_POS;
    break;
  case LoStage::LO_ATTEN:
    mid = HOSTCMD_SET_ATTEN_POS;
    break;
  default:
    ThrowError("Unrecognized LoStage id: " << device);
    break;
  }

  // Now send/store the message

  if(send)
    postMessage(mid, data);
  else
    msgs.push_back(nodeMessage(mid, data));
  
  return msgs;
}

/** .......................................................................
 * Jog the position for the requested device
 */
std::vector<carma::canbus::Message>
BiasTunedGunn::jogDevicePosition(unsigned device, int step, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;

  // Stuff the data byte into the data packet

  sLongToData(data, step);

  // Now see which message to send
  
  carma::canbus::msgType mid;

  COUT("Jogging device position = " << device << " position = " << step);

  switch (device) {
  case LoStage::LO_TUNER:
    mid = HOSTCMD_JOG_TUNER;
    break;
  case LoStage::LO_BACKSHORT:
    mid = HOSTCMD_JOG_BACKSHORT;
    break;
  case LoStage::LO_ATTEN:
    mid = HOSTCMD_JOG_ATTEN;
    break;
  default:
    ThrowError("Unrecognized LoStage id: " << device);
    break;
  }

  // Now send/store the message

  if(send)
    postMessage(mid, data);
  else
    msgs.push_back(nodeMessage(mid, data));
  
  return msgs;
}

/** .......................................................................
 * Toggle auto relock
 */
std::vector<carma::canbus::Message>
BiasTunedGunn::enableAutoRelock(bool enable, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  uByteToData(data, enable);
  
  COUT("Commanding autorelock = " << enable);

  if(send)
    postMessage(HOSTCMD_TOGGLE_AUTO_RELOCK, data);
  else
    msgs.push_back(nodeMessage(HOSTCMD_TOGGLE_AUTO_RELOCK, data));
  
  return msgs;
}

/** .......................................................................
 * Home actuators
 */
std::vector<carma::canbus::Message>
BiasTunedGunn::homeDevice(unsigned int mask, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;

  COUT("Homing: " << mask);
  
  switch (mask) {
  case LoStage::LO_ALL:
    uByteToData(data, 0);
    break;
  case LoStage::LO_TUNER:
    uByteToData(data, 1);
    break;
  case LoStage::LO_BACKSHORT:
    uByteToData(data, 2);
    break;
  case LoStage::LO_ATTEN:
    uByteToData(data, 3);
    break;
  default:
    ThrowError("Unrecognized LoStage id: " << mask);
    break;
  }
  
  if(send)
    postMessage(HOSTCMD_HOME_ACTUATORS, data);
  else
    msgs.push_back(nodeMessage(HOSTCMD_HOME_ACTUATORS, data));

  return msgs;
}

//-----------------------------------------------------------------------
// Engineering commands
//-----------------------------------------------------------------------

/**.......................................................................
 * Set the operating frequency.
 */
std::vector<carma::canbus::Message>
BiasTunedGunn::setFrequency(unsigned short frequency, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;

  COUT("Sending frequency: " << frequency);

  // Stuff the data byte into the data packet

  uShortToData(data, frequency);

  // Now send/store the message

  if(send)
    postMessage(ENGCMD_SET_GUNN_FREQ, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_SET_GUNN_FREQ, data));
  
  return msgs;
}

/** .......................................................................
 * Set the loop gain resistance.
 */
std::vector<carma::canbus::Message>
BiasTunedGunn::downloadId(unsigned short gunnId, unsigned char calMonth, unsigned char calDay,
			  unsigned char calYear,
			  unsigned short calVoltage, unsigned char nRow, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;

  // Stuff the data byte into the data packet

  COUT("Commanding id: " << gunnId << " " << calMonth << " " << calDay << " " 
       << calYear << " " << calVoltage << " " << nRow);

  uShortToData(data, gunnId);
  uByteToData(data,  calMonth);
  uByteToData(data,  calDay);
  uByteToData(data,  calYear);
  uShortToData(data, calVoltage);
  uByteToData(data,  nRow);

  // Now send/store the message

  if(send)
    postMessage(ENGCMD_DL_GUNN_ID, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_DL_GUNN_ID, data));
  
  return msgs;
}	

/** .......................................................................
 * Download the tuning table to the 1-wire device
 */
std::vector<carma::canbus::Message>
BiasTunedGunn::downloadTuningTableToOneWire(bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;

  // Now send/store the message

  COUT("Downloading tuning table");

  if(send)
    postMessage(ENGCMD_DL_TO_OWIRE, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_DL_TO_OWIRE, data));
  
  return msgs;
}

/**....................................................................... 
 * Set the loop gain resistance
 */
std::vector<carma::canbus::Message>
BiasTunedGunn::controlZaber(unsigned short deviceNo,
			    unsigned char zaberCmdCode,
			    vector<unsigned char>& zaberData, 
			    bool send)
{
  if(deviceNo > ZABER_MAX_DEV) 
    ThrowError("Device number (" << deviceNo << ") is too large (max = "
	       << ZABER_MAX_DEV << ")");
  
  if(zaberData.size() > 4)
    ThrowError("Too many bytes in the command data vector");
  
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  uByteToData(data, (unsigned char)deviceNo);  
  uByteToData(data, zaberCmdCode);  
  
  for(unsigned idata=0; idata < zaberData.size(); idata++)
    uByteToData(data, zaberData[idata]);  
  
  if(send)
    postMessage(ENGCMD_CONTROL_ZABER, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_CONTROL_ZABER, data));
  
  return msgs;
}

/** .......................................................................
 * Zaber CAN monitor request
 */
std::vector<carma::canbus::Message>
BiasTunedGunn::requestZaberPackets(bool request, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  if(send)
    postMessage(ENGCMD_ZABER_REQUEST, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_ZABER_REQUEST, data));
  
  return msgs;
}	

//-----------------------------------------------------------------------
// Monitor packet processing.
//-----------------------------------------------------------------------

/**.......................................................................
 * Process monitor packet 1
 */
void BiasTunedGunn::processBlankingFrameMonitor1(vector<byteType>& data, bool isSim)
{
  monitor_.findMonitorPoint("phaseLockState")->writeReg(isSim, dataToUbyte(data));

  hwLockStatus_ = dataToUbyte(data);

  monitor_.findMonitorPoint("hwLockStatus")->writeReg(isSim, hwLockStatus_);

  monitor_.findMonitorPoint("refLockStatus")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("sweepStatus")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("gunnStatus")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("dataValid")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("autoRelock")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("relockCount")->writeReg(isSim, dataToUbyte(data));

  // Since this monitor packet includes the lock status, check the
  // lock status for errors now

  checkHwLockStatus();
}

/**.......................................................................
 * Process monitor packet 2
 */
void BiasTunedGunn::processBlankingFrameMonitor2(vector<byteType>& data, bool isSim)
{
  monitor_.findMonitorPoint("gunnId")->writeReg(isSim, dataToUshort(data));
  monitor_.findMonitorPoint("gunnVoltage")->writeReg(isSim, dataToUshort(data));
  monitor_.findMonitorPoint("multiplier")->writeReg(isSim, dataToUshort(data));
  monitor_.findMonitorPoint("freqRangeCheck")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("ifMonState")->writeReg(isSim, dataToUbyte(data));
}

/**.......................................................................
 * Process monitor packet 3
 */
void BiasTunedGunn::processBlankingFrameMonitor3(vector<byteType>& data, bool isSim)
{
  monitor_.findMonitorPoint("calTableState")->writeReg(isSim, dataToUbyte(data));

  unsigned char month = dataToUbyte(data);
  unsigned char day   = dataToUbyte(data);
  unsigned char year  = dataToUbyte(data);

  monitor_.findMonitorPoint("calMonth")->writeReg(isSim, month);;
  monitor_.findMonitorPoint("calDay")->writeReg(isSim, day);
  monitor_.findMonitorPoint("calYear")->writeReg(isSim, year);

  std::string calDate = monthDayYearToString(month, day, year);
  monitor_.findMonitorPoint("calDate")->writeReg(isSim, (unsigned char*)calDate.c_str());

  // New as of Version D

  monitor_.findMonitorPoint("numZabers")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("allZabers")->writeReg(isSim, dataToUbyte(data));
}

/**.......................................................................
 * Process monitor packet 4
 */
void BiasTunedGunn::processBlankingFrameMonitor4(vector<byteType>& data, bool isSim)
{
  monitor_.findMonitorPoint("gunnFrequency")->writeReg(isSim, dataToUshort(data));
  monitor_.findMonitorPoint("loopGain")->writeReg(isSim,      dataToUshort(data));
  monitor_.findMonitorPoint("tunerPosition")->writeReg(isSim, (unsigned int)dataToUlong(data));
}

/**.......................................................................
 * Process monitor packet 5
 */
void BiasTunedGunn::processBlankingFrameMonitor5(vector<byteType>& data, bool isSim)
{
  monitor_.findMonitorPoint("backShortPosition")->writeReg(isSim,  (unsigned int)dataToUlong(data));
  monitor_.findMonitorPoint("attenuatorPosition")->writeReg(isSim, (unsigned int)dataToUlong(data));
}

/**.......................................................................
 * Process monitor packet 6
 */
void BiasTunedGunn::processBlankingFrameMonitor6(vector<byteType>& data, 
						 bool isSim)
{
  monitor_.findMonitorPoint("ifLevel")->writeReg(isSim,           dataToShort(data));
  monitor_.findMonitorPoint("errorVoltage")->writeReg(isSim,      dataToShort(data));
  monitor_.findMonitorPoint("gunnCurrent")->writeReg(isSim,       dataToUshort(data));
  monitor_.findMonitorPoint("noiseMeterVoltage")->writeReg(isSim, dataToShort(data));
}

/**.......................................................................
 * Process monitor packet 7
 */
void BiasTunedGunn::processBlankingFrameMonitor7(vector<byteType>& data, 
						 bool isSim)
{
  monitor_.findMonitorPoint("boardTemperature")->writeReg(isSim,     dataToShort(data));
  monitor_.findMonitorPoint("pos24VAnalogVoltage")->writeReg(isSim,  dataToShort(data));
  monitor_.findMonitorPoint("pos5VDigitalVoltage")->writeReg(isSim,  dataToShort(data));
  monitor_.findMonitorPoint("pos15VAnalogVoltage")->writeReg(isSim,  dataToShort(data));
}

/**.......................................................................
 * Process monitor packet 8
 */
void BiasTunedGunn::processBlankingFrameMonitor8(vector<byteType>& data, 
						 bool isSim)
{
  monitor_.findMonitorPoint("pos12VAnalogVoltage")->writeReg(isSim,  dataToShort(data));
  monitor_.findMonitorPoint("pos5VAnalogVoltage")->writeReg(isSim,   dataToShort(data));
  monitor_.findMonitorPoint("neg12VAnalogVoltage")->writeReg(isSim,  dataToShort(data));
  monitor_.findMonitorPoint("pos6VAnalogVoltage")->writeReg(isSim,   dataToShort(data));
}

/**.......................................................................
 * Process monitor packet 9
 */
void BiasTunedGunn::processBlankingFrameMonitor9(vector<byteType>& data, 
						 bool isSim)
{
  // New as of Version D

  monitor_.findMonitorPoint("crowbarState")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("crowbarCount")->writeReg(isSim, (unsigned int)dataToUlong(data));
}

/**.......................................................................
 * Return a map of half-second monitor packets.
 */ 
std::map<carma::canbus::msgType, std::string> 
BiasTunedGunn::getHalfSecMonitors() const 
{
  map<msgType, string> tmp; 

  tmp[MONITOR_PACKET_1] = "BiasTunedGunn::MONITOR_PACKET_1";
  tmp[MONITOR_PACKET_2] = "BiasTunedGunn::MONITOR_PACKET_2";
  tmp[MONITOR_PACKET_3] = "BiasTunedGunn::MONITOR_PACKET_3";
  tmp[MONITOR_PACKET_4] = "BiasTunedGunn::MONITOR_PACKET_4";
  tmp[MONITOR_PACKET_5] = "BiasTunedGunn::MONITOR_PACKET_5";
  tmp[MONITOR_PACKET_6] = "BiasTunedGunn::MONITOR_PACKET_6";
  tmp[MONITOR_PACKET_7] = "BiasTunedGunn::MONITOR_PACKET_7";
  tmp[MONITOR_PACKET_8] = "BiasTunedGunn::MONITOR_PACKET_8";
  tmp[MONITOR_PACKET_9] = "BiasTunedGunn::MONITOR_PACKET_9";

  return tmp; 
}


/**.......................................................................
 * Check the hardware lock status against the currently selected
 * receiver id, and determine whether or not it is an error
 */
void BiasTunedGunn::checkHwLockStatus()
{
  bool err = false;

  switch (rxId_) {
  case sza::util::Rx::RX90GHZ:
    err = (hwLockStatus_ != 1);
    break;
  default:
    break;
  }

  monitor_.findMonitorPoint("lockStatusError")->writeReg(false, err);
}

void BiasTunedGunn::setRxId(sza::util::Rx::Id rxId)
{
  CanDevice::setRxId(rxId);

  // Force writing of the lock status (to false) if the current
  // received doesn't involve us.  This is to circumvent the problem
  // where the btg module is offline, so that no blanking frame
  // packets are received and therefore checkHwLockStatus() is never
  // called, which can result in an erroneous error being reported if
  // we are tuned to 30 GHz

  if(rxId == sza::util::Rx::RX30GHZ)
    checkHwLockStatus();
}
