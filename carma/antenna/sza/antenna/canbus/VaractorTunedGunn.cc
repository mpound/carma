
#include <sstream>
#include <vector>

#include "carma/szautil/CanModule.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/VaractorFlags.h"

#include "carma/canbus/Utilities.h"

#include "carma/antenna/sza/antenna/canbus/VaractorTunedGunn.h"

#include "carma/antenna/sza/antenna/control/SzaShare.h"

using namespace std;
using namespace sza::antenna::canbus;
using namespace sza::util;

using namespace carma::canbus;

#define CARMA_MODULES

/**.......................................................................
 * Constructor.
 */
VaractorTunedGunn::VaractorTunedGunn(sza::antenna::control::SzaShare* share, 
				     string boardName,
				     carma::canbus::nodeType node, 
				     carma::canbus::CanOutput& io) : 
  Oscillator(share, boardName, sza::util::CanModule::varactorApiNo_, node, io) 
{
  monitor_.addMonitorPoint("lockStatus");
  monitor_.addMonitorPoint("refStatus");
  monitor_.addMonitorPoint("sweepStatus");
  monitor_.addMonitorPoint("gunnStatus");
  monitor_.addMonitorPoint("boardTemperature");
  monitor_.addMonitorPoint("ifMonStatus");
  monitor_.addMonitorPoint("dataValid");
  monitor_.addMonitorPoint("noiseMeterVoltage");
  monitor_.addMonitorPoint("validityMask");
  monitor_.addMonitorPoint("ifLevel");
  monitor_.addMonitorPoint("errorVoltage");
  monitor_.addMonitorPoint("biasCurrent");
  monitor_.addMonitorPoint("maxChnl");
  monitor_.addMonitorPoint("loopGainResistance");
  monitor_.addMonitorPoint("statusRegister");
  monitor_.addMonitorPoint("statusRegisterMask");
  monitor_.addMonitorPoint("powSupPos24V");
  monitor_.addMonitorPoint("powSupPosDig5V");
  monitor_.addMonitorPoint("powSupPosDig15V");
  monitor_.addMonitorPoint("powSupPos12V");
  monitor_.addMonitorPoint("powSupPos6V");
  monitor_.addMonitorPoint("powSupNeg15V");
  monitor_.addMonitorPoint("powSupPos5V");
  monitor_.addMonitorPoint("powSupPos9V");

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
VaractorTunedGunn::getSpecificControls() const
{
  map<msgType, string> tmp; 
  tmp[HOSTCMD_GUNN]                  = "VaractorTunedGunn::HOSTCMD_GUNN";
  tmp[HOSTCMD_SWEEP]                 = "VaractorTunedGunn::HOSTCMD_SWEEP";
  tmp[HOSTCMD_MONITOR]               = "VaractorTunedGunn::HOSTCMD_MONITOR";
  tmp[HOSTCMD_LOOP_GAIN_RESISTANCE]  = "VaractorTunedGunn::HOSTCMD_LOOP_GAIN_RESISTANCE";
  return tmp; 
}

/**.......................................................................
 * Return an inheritor-specific prefix to use when
 * constructing monitor points
 */
std::string VaractorTunedGunn::controlPrefix()
{
  return std::string("VaractorTunedGunn");
}

//-----------------------------------------------------------------------
// Engineering commands.
//-----------------------------------------------------------------------

/**....................................................................... 
 * Turn the Gunn on/off
 */
std::vector<carma::canbus::Message>
VaractorTunedGunn::turnGunnOn(bool on, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  uByteToData(data, on);
  
  if(send)
    postMessage(HOSTCMD_GUNN, data);
  else
    msgs.push_back(nodeMessage(HOSTCMD_GUNN, data));
  
  return msgs;
}

/**....................................................................... 
 * Turn the sweep on/off
 */
std::vector<carma::canbus::Message>
VaractorTunedGunn::turnSweepOn(bool on, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  uByteToData(data, on);
  
  if(send)
    postMessage(HOSTCMD_SWEEP, data);
  else
    msgs.push_back(nodeMessage(HOSTCMD_SWEEP, data));
  
  return msgs;
}

/**....................................................................... 
 * Turn the monitor on/off
 */
std::vector<carma::canbus::Message>
VaractorTunedGunn::turnMonitorOn(bool on, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  uByteToData(data, on);
  
  if(send)
    postMessage(HOSTCMD_MONITOR, data);
  else
    msgs.push_back(nodeMessage(HOSTCMD_MONITOR, data));
  
  return msgs;
}

/**....................................................................... 
 * Set the loop gain resistance
 */
std::vector<carma::canbus::Message>
VaractorTunedGunn::setLoopGainResistance(unsigned short voltage, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  uShortToData(data, voltage);  
  
  if(send)
    postMessage(HOSTCMD_LOOP_GAIN_RESISTANCE, data);
  else
    msgs.push_back(nodeMessage(HOSTCMD_LOOP_GAIN_RESISTANCE, data));
  
  return msgs;
}

//-----------------------------------------------------------------------
// Methods for processing monitor packets
//-----------------------------------------------------------------------

/**.......................................................................
 * Process monitor packet 1
 */
void VaractorTunedGunn::processBlankingFrameMonitor1(vector<byteType>& data, bool isSim)
{
#ifdef CARMA_MODULES
  hwLockStatus_  = dataToUbyte(data);
  unsigned char refStatus   = dataToUbyte(data);
  unsigned char sweepStatus = dataToUbyte(data);
  unsigned char gunnStatus  = dataToUbyte(data);

  monitor_.findMonitorPoint("lockStatus")->writeReg(isSim,  hwLockStatus_);
  monitor_.findMonitorPoint("statusRegister")->writeReg(isSim, 
							statusToBit(hwLockStatus_ ? LOCKED: 0x0));

  monitor_.findMonitorPoint("refStatus")->writeReg(isSim,   refStatus);
  monitor_.findMonitorPoint("sweepStatus")->writeReg(isSim, sweepStatus);
  monitor_.findMonitorPoint("gunnStatus")->writeReg(isSim,  gunnStatus);
  monitor_.findMonitorPoint("loopGainResistance")->writeReg(isSim, dataToUshort(data));
  monitor_.findMonitorPoint("boardTemperature")->writeReg(isSim, dataToShort(data));

  // And write a bitmask version of the status registers

  monitor_.findMonitorPoint("statusRegisterMask")->writeReg(isSim, statusToBit(hwLockStatus_, refStatus, sweepStatus, gunnStatus));

  // Since this monitor packet includes the lock status, check the
  // lock status for errors now
  
  checkHwLockStatus();

#else
  unsigned char status = dataToUbyte(data);

  monitor_.findMonitorPoint("statusRegister")->writeReg(isSim, status);
  monitor_.findMonitorPoint("loopGainResistance")->writeReg(isSim, dataToUshort(data));
  monitor_.findMonitorPoint("boardTemperature")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("noiseMeterVoltage")->writeReg(isSim, dataToShort(data));

  // And write a bitmask version of the status

  monitor_.findMonitorPoint("statusRegisterMask")->writeReg(isSim, statusToBit(status));
#endif
}

/**.......................................................................
 * Process monitor packet 2
 */
void VaractorTunedGunn::processBlankingFrameMonitor2(vector<byteType>& data, bool isSim)
{
#ifdef CARMA_MODULES
  unsigned char ifmonStatus = dataToUbyte(data);
  monitor_.findMonitorPoint("ifMonStatus")->writeReg(isSim, ifmonStatus);

  unsigned char dataValid = dataToUbyte(data);
  monitor_.findMonitorPoint("dataValid")->writeReg(isSim, dataValid);

  monitor_.findMonitorPoint("noiseMeterVoltage")->writeReg(isSim, dataToShort(data));

  // And write a bitmask version of the status

  monitor_.findMonitorPoint("validityMask")->writeReg(isSim, validityToBit(ifmonStatus, dataValid));

#else
  monitor_.findMonitorPoint("ifLevel")->writeReg(isSim, dataToShort(data));

  short voltage = dataToShort(data);

  monitor_.findMonitorPoint("errorVoltage")->writeReg(isSim, voltage);
  monitor_.findMonitorPoint("biasCurrent")->writeReg(isSim, dataToShort(data));

  CoordRange range((unsigned int)0);
  monitor_.findMonitorPoint("maxChnl")->writeReg(isSim, dataToShort(data), &range);
#endif
}

/**.......................................................................
 * Process monitor packet 3
 */
void VaractorTunedGunn::processBlankingFrameMonitor3(vector<byteType>& data, bool isSim)
{
#ifdef CARMA_MODULES
  monitor_.findMonitorPoint("ifLevel")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("errorVoltage")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("biasCurrent")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("powSupPos24V")->writeReg(isSim, dataToShort(data));
#else
  CoordRange range;
  for(unsigned int iChn = 1; iChn <= 4; iChn++) {
    range.setIndex(iChn);
    monitor_.findMonitorPoint("maxChnl")->writeReg(isSim, dataToShort(data), &range);
  }
#endif
}

/**.......................................................................
 * Process monitor packet 4
 */
void VaractorTunedGunn::processBlankingFrameMonitor4(vector<byteType>& data, bool isSim)
{
#ifdef CARMA_MODULES
  monitor_.findMonitorPoint("powSupPosDig5V")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("powSupPosDig15V")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("powSupPos12V")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("powSupPos6V")->writeReg(isSim, dataToShort(data));
#else
  CoordRange range;
  for(unsigned int iChn = 5; iChn <= 7; iChn++) {
    range.setIndex(iChn);
    monitor_.findMonitorPoint("maxChnl")->writeReg(isSim, dataToShort(data), &range);
  }
#endif
}

/**.......................................................................
 * Process monitor packet 5
 */
void VaractorTunedGunn::processBlankingFrameMonitor5(vector<byteType>& data, bool isSim)
{
  monitor_.findMonitorPoint("powSupNeg15V")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("powSupPos5V")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("powSupPos9V")->writeReg(isSim, dataToShort(data));
}

/**.......................................................................
 * Return a map of half-second monitor packets.
 */ 
std::map<carma::canbus::msgType, std::string> 
VaractorTunedGunn::getHalfSecMonitors() const 
{
  map<msgType, string> tmp; 

#ifdef CARMA_MODULES
  tmp[MONITOR_PACKET_1] = "VaractorTunedGunn::MONITOR_PACKET_1";
  tmp[MONITOR_PACKET_2] = "VaractorTunedGunn::MONITOR_PACKET_2";
  tmp[MONITOR_PACKET_3] = "VaractorTunedGunn::MONITOR_PACKET_3";
  tmp[MONITOR_PACKET_4] = "VaractorTunedGunn::MONITOR_PACKET_4";
  tmp[MONITOR_PACKET_5] = "VaractorTunedGunn::MONITOR_PACKET_5";
#else
  tmp[VAR_MONITOR_PACKET_1] = "VaractorTunedGunn::MONITOR_PACKET_1";
  tmp[VAR_MONITOR_PACKET_2] = "VaractorTunedGunn::MONITOR_PACKET_2";
  tmp[VAR_MONITOR_PACKET_3] = "VaractorTunedGunn::MONITOR_PACKET_3";
  tmp[VAR_MONITOR_PACKET_4] = "VaractorTunedGunn::MONITOR_PACKET_4";
#endif

  return tmp; 
}

/**.......................................................................
 * Convert from an integral lock state to an orthogonal bit so that
 * states can be unioned
 */
unsigned char VaractorTunedGunn::statusToBit(unsigned char status)
{
  unsigned char statusMask=0x0;

  if(status & LOCKED)
    statusMask |= VaractorFlags::LOCKED;
  else
    statusMask |= VaractorFlags::UNLOCKED;

  if(status & RF_GOOD)
    statusMask |= VaractorFlags::RF_GOOD;
  else
    statusMask |= VaractorFlags::RF_BAD;

  if(status & SWEEP_ON)
    statusMask |= VaractorFlags::SWEEP_ON;
  else
    statusMask |= VaractorFlags::SWEEP_OFF;

  if(status & GUNN_ON)
    statusMask |= VaractorFlags::GUNN_ON;
  else
    statusMask |= VaractorFlags::GUNN_OFF;

  return statusMask;
}

/**.......................................................................
 * Convert from an integral lock state to an orthogonal bit so that
 * states can be unioned
 */
unsigned char VaractorTunedGunn::statusToBit(unsigned char lockStatus, unsigned char refStatus, unsigned char sweepStatus, unsigned char gunnStatus)
{
  unsigned char statusMask=0x0;
  
  if(lockStatus == 1)
    statusMask |= VaractorFlags::LOCKED;
  else
    statusMask |= VaractorFlags::UNLOCKED;

  if(refStatus == 1)
    statusMask |= VaractorFlags::RF_GOOD;
  else
    statusMask |= VaractorFlags::RF_BAD;

  if(sweepStatus == 1)
    statusMask |= VaractorFlags::SWEEP_ON;
  else
    statusMask |= VaractorFlags::SWEEP_OFF;

  if(gunnStatus == 1)
    statusMask |= VaractorFlags::GUNN_ON;
  else
    statusMask |= VaractorFlags::GUNN_OFF;

  return statusMask;
}

/**.......................................................................
 * Convert from an integral lock state to an orthogonal bit so that
 * states can be unioned
 */
unsigned char VaractorTunedGunn::validityToBit(unsigned char ifmonStatus, unsigned char dataValid)
{
  unsigned char statusMask=0x0;
  
  if(ifmonStatus == 1)
    statusMask |= VaractorFlags::IFMON_GOOD;
  else
    statusMask |= VaractorFlags::IFMON_BAD;

  if(dataValid == 1)
    statusMask |= VaractorFlags::DATA_VALID;
  else
    statusMask |= VaractorFlags::DATA_INVALID;

  return statusMask;
}

/**.......................................................................
 * Check the hardware lock status against the currently selected
 * receiver id, and determine whether or not it is an error
 */
void VaractorTunedGunn::checkHwLockStatus()
{
  bool err = false;

  switch (rxId_) {
  case sza::util::Rx::RX30GHZ:
    err = (hwLockStatus_ != 1);
    break;
  default:
    break;
  }

  monitor_.findMonitorPoint("lockStatusError")->writeReg(false, err);
}

void VaractorTunedGunn::setRxId(sza::util::Rx::Id rxId)
{
  CanDevice::setRxId(rxId);

  // Force writing of the lock status (to false) if the current
  // received doesn't involve us.  This is to circumvent the problem
  // where the btg module is offline, so that no blanking frame
  // packets are received and therefore checkHwLockStatus() is never
  // called, which can result in an erroneous error being reported if
  // we are tuned to 90 GHz

  if(rxId == sza::util::Rx::RX90GHZ)
    checkHwLockStatus();
}
