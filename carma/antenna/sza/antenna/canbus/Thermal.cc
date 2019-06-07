#include "carma/canbus/Utilities.h"

#include "carma/szautil/CanModule.h"
#include "carma/szautil/Debug.h"

#include "carma/antenna/sza/antenna/canbus/Thermal.h"

#include "carma/antenna/sza/antenna/control/SzaShare.h"

using namespace std;
using namespace carma::canbus;
using namespace sza::antenna::canbus;

/**.......................................................................
 * Constructor.
 */
Thermal::Thermal(sza::antenna::control::SzaShare* share, 
                 std::string boardName,
                 carma::canbus::nodeType node, 
                 carma::canbus::CanOutput& io) :
  CanDevice(share, boardName, sza::util::CanModule::thermalApiNo_, node, io) 
{
  // Get pointers to the registers managed by this object.
  
  monitor_.addMonitorPoint("moduleTemperature");
  monitor_.addMonitorPoint("rboxTopTemperature");
  monitor_.addMonitorPoint("eboxTemperature");
  monitor_.addMonitorPoint("powSup24V");
  
  monitor_.addMonitorPoint("rboxLoopState");
  monitor_.addMonitorPoint("rboxPwmFraction");
  monitor_.addMonitorPoint("rboxTemperatureError");
  monitor_.addMonitorPoint("rboxIntTemperatureError");
  
  monitor_.addMonitorPoint("rboxLoopGain");
  monitor_.addMonitorPoint("rboxIntGainConstant");
  monitor_.addMonitorPoint("rboxLoopRateConstant");
  monitor_.addMonitorPoint("rboxLoopBandwidth");
  
  monitor_.addMonitorPoint("eboxLoopState");
  monitor_.addMonitorPoint("eboxVoltage");
  monitor_.addMonitorPoint("eboxTemperatureError");
  monitor_.addMonitorPoint("eboxIntTemperatureError");
  
  monitor_.addMonitorPoint("eboxLoopGain");
  monitor_.addMonitorPoint("eboxIntGainConstant");
  monitor_.addMonitorPoint("eboxLoopRateConstant");
  monitor_.addMonitorPoint("eboxLoopBandwidth");
  
  monitor_.addMonitorPoint("rboxBottomTemperature");
  monitor_.addMonitorPoint("rboxSetTemperature");
  monitor_.addMonitorPoint("eboxSetTemperature");
  monitor_.addMonitorPoint("circPropConst");

  monitor_.addMonitorPoint("powSupPos12V");
  monitor_.addMonitorPoint("powSupNeg12V");
  monitor_.addMonitorPoint("powSupPos5V");

  monitor_.addMonitorPoint("powSupVoltage");
  monitor_.addMonitorPoint("powSupCurrent");
  monitor_.addMonitorPoint("powSupError");

  monitor_.addMonitorPoint("voltageOffset");
}

/**.......................................................................
 * Destructor.
 */
Thermal::~Thermal() {}

//-----------------------------------------------------------------------
// Blanking monitor packet processing
//-----------------------------------------------------------------------

/**.......................................................................
 * Return a map of half-second monitor points.
 */ 
std::map<carma::canbus::msgType, std::string> 
Thermal::getHalfSecMonitors() const
{
  map<msgType, string> tmp; 
  
  tmp[MONITOR_PACKET_1] = "Thermal::MONITOR_PACKET_1";
  tmp[MONITOR_PACKET_2] = "Thermal::MONITOR_PACKET_2";
  tmp[MONITOR_PACKET_3] = "Thermal::MONITOR_PACKET_3";
  tmp[MONITOR_PACKET_4] = "Thermal::MONITOR_PACKET_4";
  tmp[MONITOR_PACKET_5] = "Thermal::MONITOR_PACKET_5";
  tmp[MONITOR_PACKET_6] = "Thermal::MONITOR_PACKET_6";
  tmp[MONITOR_PACKET_7] = "Thermal::MONITOR_PACKET_7";
  tmp[MONITOR_PACKET_8] = "Thermal::MONITOR_PACKET_8";
  
  return tmp;
}

/**.......................................................................
 * Process monitor packet 1.
 */
void Thermal::processBlankingFrameMonitor1(std::vector<byteType>& data, bool isSim)
{
  monitor_.findMonitorPoint("moduleTemperature")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("rboxTopTemperature")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("eboxTemperature")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("powSup24V")->writeReg(isSim, dataToShort(data));
}

/**.......................................................................
 * Process monitor packet 2.
 */
void Thermal::processBlankingFrameMonitor2(std::vector<byteType>& data, bool isSim)
{
  monitor_.findMonitorPoint("rboxLoopState")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("rboxPwmFraction")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("rboxTemperatureError")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("rboxIntTemperatureError")->writeReg(isSim, dataToShort(data));
}

/**.......................................................................
 * Process monitor packet 3.
 */
void Thermal::processBlankingFrameMonitor3(std::vector<byteType>& data, bool isSim)
{
  monitor_.findMonitorPoint("rboxLoopGain")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("rboxIntGainConstant")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("rboxLoopBandwidth")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("rboxLoopRateConstant")->writeReg(isSim, dataToShort(data));
}

/**.......................................................................
 * Process monitor packet 4.
 */
void Thermal::processBlankingFrameMonitor4(std::vector<byteType>& data, bool isSim) 
{
  monitor_.findMonitorPoint("eboxLoopState")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("eboxVoltage")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("eboxTemperatureError")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("eboxIntTemperatureError")->writeReg(isSim, dataToShort(data));
}

/**.......................................................................
 * Process monitor packet 5.
 */
void Thermal::processBlankingFrameMonitor5(std::vector<byteType>& data, bool isSim)
{
  monitor_.findMonitorPoint("eboxLoopGain")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("eboxIntGainConstant")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("eboxLoopBandwidth")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("eboxLoopRateConstant")->writeReg(isSim, dataToShort(data));
}

/**.......................................................................
 * Process monitor packet 6.
 */
void Thermal::processBlankingFrameMonitor6(std::vector<byteType>& data, bool isSim) 
{
  monitor_.findMonitorPoint("rboxBottomTemperature")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("rboxSetTemperature")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("eboxSetTemperature")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("circPropConst")->writeReg(isSim, dataToShort(data));
}

/**.......................................................................
 * Process monitor packet 7.
 */
void Thermal::processBlankingFrameMonitor7(std::vector<byteType>& data, bool isSim) 
{
  monitor_.findMonitorPoint("powSupPos12V")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("powSupNeg12V")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("powSupPos5V")->writeReg(isSim, dataToShort(data));
}

/**.......................................................................
 * Process monitor packet 8.
 */
void Thermal::processBlankingFrameMonitor8(std::vector<byteType>& data, bool isSim) 
{
  monitor_.findMonitorPoint("powSupVoltage")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("powSupCurrent")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("powSupError")->writeReg(isSim, dataToShort(data));

  monitor_.findMonitorPoint("voltageOffset")->writeReg(isSim, dataToShort(data));
}

/**.......................................................................
 * Set the rbox temperature. (C)
 */
std::vector<carma::canbus::Message>
Thermal::setTemperature(sza::util::Thermal::Target target, 
			float celsius, bool send)
{
  engineeringCommand command;
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  floatToData(data, celsius);
  
  if(target & sza::util::Thermal::EBOX) {
    if(send)
      postMessage(ENGCMD_SET_EBOX_TEMP, data);
    else
      msgs.push_back(nodeMessage(ENGCMD_SET_EBOX_TEMP, data));
  }
  
  if(target & sza::util::Thermal::RBOX) {
    if(send)
      postMessage(ENGCMD_SET_RBOX_TEMP, data);
    else
      msgs.push_back(nodeMessage(ENGCMD_SET_RBOX_TEMP, data));
  }
  
  return msgs;
}

/**.......................................................................
 * Set the mode
 */
std::vector<carma::canbus::Message>
Thermal::setMode(sza::util::Thermal::Target target, 
		 sza::util::Thermal::BoxMode mode, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  uByteToData(data, mode);
  
  if(target & sza::util::Thermal::EBOX) {
    if(send)
      postMessage(ENGCMD_SET_EBOX_MODE, data);
    else
      msgs.push_back(nodeMessage(ENGCMD_SET_EBOX_MODE, data));
  }
  
  if(target & sza::util::Thermal::RBOX) {
    if(send)
      postMessage(ENGCMD_SET_RBOX_MODE, data);
    else
      msgs.push_back(nodeMessage(ENGCMD_SET_RBOX_MODE, data));
  }
  
  return msgs;
}

/**.......................................................................
 * Set the loop gain
 */
std::vector<carma::canbus::Message>
Thermal::setLoopGain(sza::util::Thermal::Target target, 
		     float gain, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  floatToData(data, gain);
  
  if(target & sza::util::Thermal::EBOX) {
    if(send)
      postMessage(ENGCMD_SET_EBOX_LOOP_GAIN, data);
    else
      msgs.push_back(nodeMessage(ENGCMD_SET_EBOX_LOOP_GAIN, data));
  }
  
  if(target & sza::util::Thermal::RBOX) {
    if(send)
      postMessage(ENGCMD_SET_RBOX_LOOP_GAIN, data);
    else
      msgs.push_back(nodeMessage(ENGCMD_SET_RBOX_LOOP_GAIN, data));
  }
  
  return msgs;
}

/**.......................................................................
 * Set the integration constant
 */
std::vector<carma::canbus::Message>
Thermal::setIntegrationConstant(sza::util::Thermal::Target target, 
				float constant, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  floatToData(data, constant);
  
  if(target & sza::util::Thermal::EBOX) {
    if(send)
      postMessage(ENGCMD_SET_EBOX_INTEG_CONST, data);
    else
      msgs.push_back(nodeMessage(ENGCMD_SET_EBOX_INTEG_CONST, data));
  }
  
  if(target & sza::util::Thermal::RBOX) {
    if(send)
      postMessage(ENGCMD_SET_RBOX_INTEG_CONST, data);
    else
      msgs.push_back(nodeMessage(ENGCMD_SET_RBOX_INTEG_CONST, data));
  }
  
  return msgs;
}

/**.......................................................................
 * Set the rbox loop bw
 */
std::vector<carma::canbus::Message>
Thermal::setLoopBandWidth(sza::util::Thermal::Target target, 
			  float bw, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  floatToData(data, bw);
  
  if(target & sza::util::Thermal::EBOX) {
    if(send)
      postMessage(ENGCMD_SET_EBOX_LOOP_BW, data);
    else
      msgs.push_back(nodeMessage(ENGCMD_SET_EBOX_LOOP_BW, data));
  }
  
  if(target & sza::util::Thermal::RBOX) {
    if(send)
      postMessage(ENGCMD_SET_RBOX_LOOP_BW, data);
    else
      msgs.push_back(nodeMessage(ENGCMD_SET_RBOX_LOOP_BW, data));
  }
  
  return msgs;
}

/**.......................................................................
 * Set the rate constant
 */
std::vector<carma::canbus::Message>
Thermal::setRateConstant(sza::util::Thermal::Target target, 
			 float rate, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  floatToData(data, rate);
  
  if(target & sza::util::Thermal::EBOX) {
    if(send)
      postMessage(ENGCMD_SET_EBOX_RATE_CONST, data);
    else
      msgs.push_back(nodeMessage(ENGCMD_SET_EBOX_RATE_CONST, data));
  }
  
  if(target & sza::util::Thermal::RBOX) {
    if(send)
      postMessage(ENGCMD_SET_RBOX_RATE_CONST, data);
    else
      msgs.push_back(nodeMessage(ENGCMD_SET_RBOX_RATE_CONST, data));
  }
  
  return msgs;
}

/**.......................................................................
 * Set the proportionality constant for the circulation fans
 */
std::vector<carma::canbus::Message>
Thermal::setCirculationFanPropConst(float rate, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  floatToData(data, rate);
  
  if(send)
    postMessage(ENGCMD_SET_CIRC_PROP_CONST, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_SET_CIRC_PROP_CONST, data));
  
  return msgs;
}

/**.......................................................................
 * Set the proportionality constant for the circulation fans
 */
std::vector<carma::canbus::Message>
Thermal::setVoltageOffset(float voltageOffset, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  floatToData(data, voltageOffset);
  
  if(send)
    postMessage(ENGCMD_SET_VOLTAGE_OFFSET, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_SET_VOLTAGE_OFFSET, data));
  
  return msgs;
}

/**.......................................................................
 * Set the ebox equilibrium state
 */
std::vector<carma::canbus::Message>
Thermal::setEboxEqState(bool eqStateOn, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  floatToData(data, (float)eqStateOn);
  
  if(send)
    postMessage(ENGCMD_SET_EBOX_EQ_STATE, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_SET_EBOX_EQ_STATE, data));
  
  return msgs;
}

/**.......................................................................
 * Set the ebox integral error
 */
std::vector<carma::canbus::Message>
Thermal::setEboxIntError(float intError, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  floatToData(data, intError);
  
  if(send)
    postMessage(ENGCMD_SET_EBOX_INT_ERROR, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_SET_EBOX_INT_ERROR, data));
  
  return msgs;
}

