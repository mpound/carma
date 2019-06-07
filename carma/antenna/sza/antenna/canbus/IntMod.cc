#include "carma/canbus/Utilities.h"

#include "carma/szautil/CanModule.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/LogStream.h"

#include "carma/antenna/sza/antenna/canbus/IntMod.h"

#include "carma/antenna/sza/antenna/control/SzaShare.h"

using namespace std;
using namespace carma::canbus;
using namespace sza::antenna::canbus;
using namespace sza::util;

#define CARMA_MODULES

/**.......................................................................
 * Constructor.
 */
IntMod::IntMod(sza::antenna::control::SzaShare* share, 
	       std::string boardName,
	       carma::canbus::nodeType node, 
	       carma::canbus::CanOutput& io) :
  CanDevice(share, boardName, sza::util::CanModule::intmodApiNo_, node, io) 
{
  monitor_.addMonitorPoint("photoLev10MHz");
  monitor_.addMonitorPoint("photoLev50MHz");
  monitor_.addMonitorPoint("photoLevLOTerm");
  monitor_.addMonitorPoint("loRFInLev");
  monitor_.addMonitorPoint("loRFOutLev");
  monitor_.addMonitorPoint("loTempLev");
  monitor_.addMonitorPoint("pamAtten");
  monitor_.addMonitorPoint("modTemp");
  monitor_.addMonitorPoint("powSupNeg28V");
  monitor_.addMonitorPoint("powSupPosDig5V");
  monitor_.addMonitorPoint("powSupPos15V");
  monitor_.addMonitorPoint("powSupPos24V");
  monitor_.addMonitorPoint("powSupNeg9V");
  monitor_.addMonitorPoint("powSupNeg5V");
  monitor_.addMonitorPoint("powSupNeg15V");
  monitor_.addMonitorPoint("powSupPos5V");
  monitor_.addMonitorPoint("powSupPos9V");
  monitor_.addMonitorPoint("statusRegister");
  monitor_.addMonitorPoint("lockStatus10MHz");
  monitor_.addMonitorPoint("photoStatus10MHz");
  monitor_.addMonitorPoint("photoStatus50MHz");
  monitor_.addMonitorPoint("loTermPowerState");
  monitor_.addMonitorPoint("sn10MHzModule");
  monitor_.addMonitorPoint("sn50MHzModule");
  monitor_.addMonitorPoint("snLoTermModule");
}

/**.......................................................................
 * Destructor.
 */
IntMod::~IntMod() {}

/**.......................................................................
 * Set the PAM attenuation
 */
std::vector<carma::canbus::Message>
IntMod::setPAMAttenuation(unsigned char atten, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;

  uByteToData(data, atten);

  if(send)
    postMessage(ENGCMD_SET_PAM_ATTEN, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_SET_PAM_ATTEN, data));

  return msgs;
}

/**.......................................................................
 * Set the output power
 */
std::vector<carma::canbus::Message>
IntMod::setPAMOutputPower(short power, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  sShortToData(data, power);

  if(send)
    postMessage(ENGCMD_SET_PAM_POWER, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_SET_PAM_POWER, data));

  return msgs;
}
	
/**.......................................................................
 * Set the output power to a preset level
 */
std::vector<carma::canbus::Message>
IntMod::presetPAMOutputPower(bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  if(send)
    postMessage(ENGCMD_PRESET_PAM_POWER, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_PRESET_PAM_POWER, data));

  return msgs;
}

//-----------------------------------------------------------------------
// Blanking monitor packet processing
//-----------------------------------------------------------------------
 
/**.......................................................................
 * Return a map of half-second monitor points.
 */ 
std::map<carma::canbus::msgType, std::string> 
IntMod::getHalfSecMonitors() const
{
  map<msgType, string> tmp; 

  tmp[MONITOR_PACKET_1] = "IntMod::MONITOR_PACKET_1";
  tmp[MONITOR_PACKET_2] = "IntMod::MONITOR_PACKET_2";
  tmp[MONITOR_PACKET_3] = "IntMod::MONITOR_PACKET_3";
  tmp[MONITOR_PACKET_4] = "IntMod::MONITOR_PACKET_4";
  tmp[MONITOR_PACKET_5] = "IntMod::MONITOR_PACKET_5";

  return tmp;
}

/**.......................................................................
 * Process monitor packet 1.
 */
void IntMod::processBlankingFrameMonitor1(std::vector<byteType>& data, bool isSim)
{
#ifdef CARMA_MODULES
  monitor_.findMonitorPoint("photoLev10MHz")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("photoLev50MHz")->writeReg(isSim, dataToShort(data));
#else
  // The API 184 document says that this packet has 10MHz first, then
  // 50MHz, but in the SZA version, these are actually swapped

  monitor_.findMonitorPoint("photoLev50MHz")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("photoLev10MHz")->writeReg(isSim, dataToShort(data));
#endif

  monitor_.findMonitorPoint("photoLevLOTerm")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("loRFInLev")->writeReg(isSim, dataToShort(data));
}

/**.......................................................................
 * Process monitor packet 2.
 */
void IntMod::processBlankingFrameMonitor2(std::vector<byteType>& data, bool isSim)
{
  monitor_.findMonitorPoint("loRFOutLev")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("loTempLev")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("pamAtten")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("modTemp")->writeReg(isSim, dataToShort(data));
}

/**.......................................................................
 * Process monitor packet 3.
 */
void IntMod::processBlankingFrameMonitor3(std::vector<byteType>& data, bool isSim)
{
#ifdef CARMA_MODULES
  monitor_.findMonitorPoint("powSupPos24V")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("powSupPosDig5V")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("powSupPos15V")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("powSupNeg9V")->writeReg(isSim, dataToShort(data));
#else
  monitor_.findMonitorPoint("powSupNeg28V")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("powSupPosDig5V")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("powSupPos15V")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("powSupNeg9V")->writeReg(isSim, dataToShort(data));
#endif
}

/**.......................................................................
 * Process monitor packet 5
 */
void IntMod::processBlankingFrameMonitor4(std::vector<byteType>& data, bool isSim)
{
  monitor_.findMonitorPoint("powSupNeg5V")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("powSupNeg15V")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("powSupPos5V")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("powSupPos9V")->writeReg(isSim, dataToShort(data));
}

/**.......................................................................
 * Process monitor packet 5
 */
void IntMod::processBlankingFrameMonitor5(std::vector<byteType>& data, bool isSim)
{
#ifdef CARMA_MODULES
  monitor_.findMonitorPoint("lockStatus10MHz")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("photoStatus10MHz")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("photoStatus50MHz")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("loTermPowerState")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("sn10MHzModule")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("sn50MHzModule")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("snLoTermModule")->writeReg(isSim, dataToUbyte(data));
#else
  CoordRange range;

  for(unsigned iEl=0; iEl < 3; iEl++) {
    range.setIndex(iEl);
    monitor_.findMonitorPoint("statusRegister")->writeReg(isSim, dataToUbyte(data), &range);
  }
#endif
}

