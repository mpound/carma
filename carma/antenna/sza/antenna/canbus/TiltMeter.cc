#include "carma/canbus/Utilities.h"

#include "carma/antenna/sza/antenna/canbus/TiltMeter.h"

#include "carma/antenna/sza/antenna/control/SzaShare.h"

#include "carma/szautil/CanModule.h"

#include <string>

using namespace std;
using namespace sza::antenna::canbus;
using namespace sza::util;
using namespace carma::canbus;

/**.......................................................................
 * Constructor.
 */
TiltMeter::TiltMeter(sza::antenna::control::SzaShare* share, 
		     std::string boardName,
		     carma::canbus::nodeType node, 
		     carma::canbus::CanOutput& io) :
  CanDevice(share, boardName, sza::util::CanModule::tiltMeterApiNo_, node, io) 
{
  afZero_.setDegrees(0.0);
  lrZero_.setDegrees(0.0);

  monitor_.addMonitorPoint("lrTilt");
  monitor_.addMonitorPoint("afTilt");
  monitor_.addMonitorPoint("boardTemperature");
  
  monitor_.addMonitorPoint("tiltTemp");
  monitor_.addMonitorPoint("structTemp");
  monitor_.addMonitorPoint("heaterVoltage");
  monitor_.addMonitorPoint("heaterCurrent");
  
  monitor_.addMonitorPoint("loopState");
  monitor_.addMonitorPoint("pwrFract");
  monitor_.addMonitorPoint("tempDiff");
  monitor_.addMonitorPoint("integDiff");
  
  monitor_.addMonitorPoint("loopGain");
  monitor_.addMonitorPoint("loopIntegration");
  monitor_.addMonitorPoint("loopDiffGain");
  monitor_.addMonitorPoint("loopBw");
  
  monitor_.addMonitorPoint("pos24VPsVoltage");
  monitor_.addMonitorPoint("pos12VTiltPsVoltage");
  monitor_.addMonitorPoint("neg15VTiltPsVoltage");
  monitor_.addMonitorPoint("pos5VTiltPsVoltage");
  
  monitor_.addMonitorPoint("pos12VThermalPsVoltage");
  monitor_.addMonitorPoint("neg12VThermalPsVoltage");
  monitor_.addMonitorPoint("pos5VThermalPsVoltage");
  monitor_.addMonitorPoint("teePeeTemp");
}


/**.......................................................................
 * Destructor.
 */
TiltMeter::~TiltMeter() {}

/**
 * Process monitor packet 1.
 */
void TiltMeter::processBlankingFrameMonitor1(std::vector<carma::canbus::byteType>& data, bool isSim)
{
  short af, lr;
  lr = dataToShort(data);
  af = dataToShort(data);

  lr -= lrZero_.tiltmeterUnits();
  af -= afZero_.tiltmeterUnits();

  monitor_.findMonitorPoint("lrTilt")->writeReg(isSim, lr);
  monitor_.findMonitorPoint("afTilt")->writeReg(isSim, af);
  monitor_.findMonitorPoint("boardTemperature")->writeReg(isSim, dataToShort(data));
}

/**
 * Process monitor packet 2.
 */
void TiltMeter::processBlankingFrameMonitor2(std::vector<carma::canbus::byteType>& data, bool isSim)
{
  monitor_.findMonitorPoint("tiltTemp")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("structTemp")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("heaterVoltage")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("heaterCurrent")->writeReg(isSim, dataToShort(data));
}

/**
 * Process monitor packet 3.
 */
void TiltMeter::processBlankingFrameMonitor3(std::vector<carma::canbus::byteType>& data, bool isSim)
{
  monitor_.findMonitorPoint("loopState")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("pwrFract")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("tempDiff")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("integDiff")->writeReg(isSim, dataToShort(data));
}

/**
 * Process monitor packet 4.
 */
void TiltMeter::processBlankingFrameMonitor4(std::vector<carma::canbus::byteType>& data, bool isSim)
{
  monitor_.findMonitorPoint("loopGain")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("loopIntegration")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("loopDiffGain")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("loopBw")->writeReg(isSim, dataToShort(data));
}

/**
 * Process monitor packet 5.
 */
void TiltMeter::processBlankingFrameMonitor5(std::vector<carma::canbus::byteType>& data, bool isSim)
{
  monitor_.findMonitorPoint("pos24VPsVoltage")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("pos12VTiltPsVoltage")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("neg15VTiltPsVoltage")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("pos5VTiltPsVoltage")->writeReg(isSim, dataToShort(data));
}

/**
 * Process monitor packet 6.
 */
void TiltMeter::processBlankingFrameMonitor6(std::vector<carma::canbus::byteType>& data, bool isSim)
{
  monitor_.findMonitorPoint("pos12VThermalPsVoltage")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("neg12VThermalPsVoltage")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("pos5VThermalPsVoltage")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("teePeeTemp")->writeReg(isSim, dataToShort(data));
}

/**.......................................................................
 * Return a map of half-second monitor packets.
 */ 
std::map<carma::canbus::msgType, std::string> 
TiltMeter::getHalfSecMonitors() const 
{
  map<msgType, string> tmp; 
  
  tmp[MONITOR_PACKET_1] = "TiltMeter::MONITOR_PACKET_1";
  tmp[MONITOR_PACKET_2] = "TiltMeter::MONITOR_PACKET_2";
  tmp[MONITOR_PACKET_3] = "TiltMeter::MONITOR_PACKET_3";
  tmp[MONITOR_PACKET_4] = "TiltMeter::MONITOR_PACKET_4";
  tmp[MONITOR_PACKET_5] = "TiltMeter::MONITOR_PACKET_5";
  tmp[MONITOR_PACKET_6] = "TiltMeter::MONITOR_PACKET_6";
  
  return tmp; 
}

/**.......................................................................
 * Set the desired operating temperature
 */
std::vector<carma::canbus::Message>
TiltMeter::setTemperature(sza::util::Temperature temp, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  floatToData(data, temp.C());
  
  if(send)
    postMessage(HOSTCMD_SET_TEMP, data);
  else
    msgs.push_back(nodeMessage(HOSTCMD_SET_TEMP, data));

  return msgs;
}

/**.......................................................................
 * Turn the thermal control loop on/off/ or put into manual mode
 */
std::vector<carma::canbus::Message>
TiltMeter::regulateTemperature(sza::array::TiltmeterMode opmode, float pwrFract, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  uByteToData(data, opmode);
  floatToData(data, pwrFract);
  
  if(send)
    postMessage(HOSTCMD_REGULATE_TEMP, data);
  else
    msgs.push_back(nodeMessage(HOSTCMD_REGULATE_TEMP, data));

  return msgs;
}

/**.......................................................................
 * Set the loop gain
 */
std::vector<carma::canbus::Message>
TiltMeter::setLoopGain(float gain, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  floatToData(data, gain);
  
  if(send)
    postMessage(HOSTCMD_SET_LOOP_GAIN, data);
  else
    msgs.push_back(nodeMessage(HOSTCMD_SET_LOOP_GAIN, data));

  return msgs;
}

/** .......................................................................
 * Set the loop integration constant
 */
std::vector<carma::canbus::Message>
TiltMeter::setLoopIntegrationConstant(float constant, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  floatToData(data, constant);
  
  if(send)
    postMessage(HOSTCMD_SET_LOOP_INT_CONST, data);
  else
    msgs.push_back(nodeMessage(HOSTCMD_SET_LOOP_INT_CONST, data));

  return msgs;
}

/** .......................................................................
 * Set the loop rate constant
 */
std::vector<carma::canbus::Message>
TiltMeter::setLoopRateConstant(float constant, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  floatToData(data, constant);
  
  if(send)
    postMessage(HOSTCMD_SET_LOOP_RATE_CONST, data);
  else
    msgs.push_back(nodeMessage(HOSTCMD_SET_LOOP_RATE_CONST, data));

  return msgs;
}

/** .......................................................................
 * Set the loop banadwidth
 */
std::vector<carma::canbus::Message>
TiltMeter::setLoopBandwidth(float bw, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  floatToData(data, bw);
  
  if(send)
    postMessage(HOSTCMD_SET_LOOP_BW, data);
  else
    msgs.push_back(nodeMessage(HOSTCMD_SET_LOOP_BW, data));

  return msgs;
}

/** .......................................................................
 * Write the loop parameters to EEPROM
 */
std::vector<carma::canbus::Message>
TiltMeter::writeToEeprom(bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  if(send)
    postMessage(HOSTCMD_WRITE_EEPROM, data);
  else
    msgs.push_back(nodeMessage(HOSTCMD_WRITE_EEPROM, data));

  return msgs;
}


void TiltMeter::setZeros(sza::util::Angle& afZero, sza::util::Angle& lrZero)
{
  afZero_ = afZero;
  lrZero_ = lrZero;
}
