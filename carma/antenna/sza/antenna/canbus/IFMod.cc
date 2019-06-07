#include "carma/canbus/Utilities.h"

#include "carma/antenna/sza/antenna/canbus/IFMod.h"

#include "carma/antenna/sza/antenna/control/AntennaRx.h"

#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/Temperature.h"

using namespace std;
using namespace carma::canbus;
using namespace sza::antenna::canbus;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
IFMod::IFMod(sza::antenna::control::AntennaRx* parent,
	     sza::antenna::control::SzaShare* share, 
	     std::string boardName,
	     carma::canbus::nodeType node, 
	     carma::canbus::CanOutput& io) :
  CanDevice(share, boardName, sza::util::CanModule::ifmodApiNo_, node, io) 
{
  
  parent_  = parent;
  
  monitor_.addMonitorPoint("ifTotalPower");
  monitor_.addMonitorPoint("pamTemperature");
  monitor_.addMonitorPoint("totalAtten");
  monitor_.addMonitorPoint("pamStatus");
  monitor_.addMonitorPoint("ifSwitchState");
  monitor_.addMonitorPoint("laserStatus");
  monitor_.addMonitorPoint("laserPower");
  monitor_.addMonitorPoint("laserRegError");
  monitor_.addMonitorPoint("inputAtten");
  monitor_.addMonitorPoint("outputAtten");
  monitor_.addMonitorPoint("laserId");
  monitor_.addMonitorPoint("errorCount");
  
  lastInputAtten_ = 0.0;
  lastOutputAtten_ = 0.0;
}

/**.......................................................................
 * Destructor.
 */
IFMod::~IFMod() {}

//-----------------------------------------------------------------------
// Blanking monitor packet processing
//-----------------------------------------------------------------------

/**.......................................................................
 * Return a map of half-second monitor points.
 */ 
std::map<carma::canbus::msgType, std::string>
IFMod::getHalfSecMonitors() const
{
  map<msgType, string> tmp; 
  
  tmp[MONITOR_PACKET_1] = "IFMod::MONITOR_PACKET_1";
  tmp[MONITOR_PACKET_2] = "IFMod::MONITOR_PACKET_2";
  tmp[MONITOR_PACKET_3] = "IFMod::MONITOR_PACKET_3";
  tmp[MONITOR_PACKET_4] = "IFMod::MONITOR_PACKET_4";
  tmp[MONITOR_PACKET_5] = "IFMod::MONITOR_PACKET_5";
  
  return tmp;
}

/**.......................................................................
 * Process monitor packet 1.
 */
void IFMod::processBlankingFrameMonitor1(std::vector<byteType>& data, bool isSim)
{
  monitor_.findMonitorPoint("ifTotalPower")->writeReg(isSim, dataToFloat(data));

  Temperature temp;
  temp.setC(dataToFloat(data));
  float tempInK = temp.K();

  monitor_.findMonitorPoint("pamTemperature")->writeReg(isSim, tempInK);
}

/**.......................................................................
 * Process monitor packet 2.
 */
void IFMod::processBlankingFrameMonitor2(std::vector<byteType>& data, bool isSim)
{
  monitor_.findMonitorPoint("totalAtten")->writeReg(isSim, dataToFloat(data));    // Db
  monitor_.findMonitorPoint("pamStatus")->writeReg(isSim, dataToUbyte(data));     // Status register

  unsigned char state = dataToUbyte(data);

  monitor_.findMonitorPoint("ifSwitchState")->writeReg(isSim, state);             // IF switch state
  monitor_.findMonitorPoint("laserStatus")->writeReg(isSim, dataToUbyte(data));   // Laser status

#ifdef CARMA_MODULES
  monitor_.findMonitorPoint("errorCount")->writeReg(isSim, dataToUbyte(data));    // Error count
#endif
}

/**.......................................................................
 * Process monitor packet 3.
 */
void IFMod::processBlankingFrameMonitor3(std::vector<byteType>& data, bool isSim)
{
  monitor_.findMonitorPoint("laserPower")->writeReg(isSim, dataToFloat(data));    // Db
  monitor_.findMonitorPoint("laserRegError")->writeReg(isSim, dataToFloat(data)); // V
}

/**.......................................................................
 * Process monitor packet 4.
 */
void IFMod::processBlankingFrameMonitor4(std::vector<byteType>& data, bool isSim)
{
  monitor_.findMonitorPoint("inputAtten")->writeReg(isSim, dataToFloat(data));  // dB
  monitor_.findMonitorPoint("outputAtten")->writeReg(isSim, dataToFloat(data)); // dB
}


/**.......................................................................
 * Process monitor packet 5.
 */
void IFMod::processBlankingFrameMonitor5(std::vector<byteType>& data, bool isSim)
{
  CoordRange range;
  for(unsigned iByte=0; iByte < 7; iByte++) {
    range.setIndex(iByte);
    monitor_.findMonitorPoint("laserId")->writeReg(isSim, dataToUbyte(data), &range);
  }
}

/**.......................................................................
 * Select a band number
 */
std::vector<carma::canbus::Message>
IFMod::selectBand(sza::util::Rx::Id rxId, bool send)
{
  LogStream errStr;
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;

  // Convert the rx id to an IF switch position
  
  uByteToData(data, Rx::rxToIFSwitchPos(rxId));
  
  if(send)
    postMessage(ENGCMD_SELECT_BAND, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_SELECT_BAND, data));

  return msgs;
}

/**.......................................................................
 * Set the IF attenutation.
 *
 * @param atten 0-63 dB, in steps of 0.5 dB
 */
std::vector<carma::canbus::Message>
IFMod::setAtten(Attenuation atten, bool send)
{
  return setAtten((float)atten.dB(), send);
}

std::vector<carma::canbus::Message>
IFMod::setAtten(float atten, bool send)
{
  LogStream errStr;
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  floatToData(data, atten);
  
  if(send)
    postMessage(ENGCMD_SET_IF_TOTAL_ATTEN, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_SET_IF_TOTAL_ATTEN, data));

  return msgs;
}

/**.......................................................................
 * Set the IF attenutation.
 *
 * @param atten 0-63 dB, in steps of 0.5 dB
 */
std::vector<carma::canbus::Message>
IFMod::setAtten(float input, float output, bool send)
{
  LogStream errStr;
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  floatToData(data, input);
  floatToData(data, output);
  
  if(send)
    postMessage(ENGCMD_SET_IF_INOUT_ATTEN, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_SET_IF_INOUT_ATTEN, data));

  return msgs;
}

/**.......................................................................
 * Set IF level
 *
 * @param level Total power, in mW
 */
std::vector<carma::canbus::Message>
IFMod::setLevel(float level, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  floatToData(data, level);
  
  if(send)
    postMessage(ENGCMD_SET_IF_LEVEL, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_SET_IF_LEVEL, data));

  return msgs;
}

#ifdef CARMA_MODULES
/**.......................................................................
 * Set input atten level
 */
std::vector<carma::canbus::Message>
IFMod::setInputAtten(float inputAtten, bool send) 
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  floatToData(data, inputAtten);
  
  if(send)
    postMessage(ENGCMD_SET_IF_INPUT_ATTEN, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_SET_IF_INPUT_ATTEN, data));

  return msgs;
}
#else
/**.......................................................................
 * Set input atten level
 */
std::vector<carma::canbus::Message>
IFMod::setInputAtten(float inputAtten, bool send) 
{
  lastInputAtten_ = inputAtten;
  
  // Since no output atten was specified, use the last commanded atten
  
  return setAtten(inputAtten, lastOutputAtten_, send);
}
#endif

#ifdef CARMA_MODULES
/**.......................................................................
 * Set output atten level
 */
std::vector<carma::canbus::Message>
IFMod::setOutputAtten(float inputAtten, bool send) 
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  floatToData(data, inputAtten);
  
  if(send)
    postMessage(ENGCMD_SET_IF_OUTPUT_ATTEN, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_SET_IF_OUTPUT_ATTEN, data));

  return msgs;
}
#else
/**.......................................................................
 * Set output atten levels 
 */
std::vector<carma::canbus::Message>
IFMod::setOutputAtten(float outputAtten, bool send) 
{
  // Since no input atten was specified, use the last commanded atten
  
  lastOutputAtten_ = outputAtten;
  
  return setAtten(lastInputAtten_, outputAtten, send);
}
#endif

//-----------------------------------------------------------------------
// Status message processing
//-----------------------------------------------------------------------

/**.......................................................................
 * Return a map of half-second monitor points.
 */ 
std::map<carma::canbus::msgType, std::string>
IFMod::getStatusMessages() const
{
  map<msgType, string> tmp; 
  
  tmp[STATUS_MSG_1] = "IFMod::STATUS_MSG_1";
  tmp[STATUS_MSG_2] = "IFMod::STATUS_MSG_2";
  
  return tmp;
}

/**.......................................................................
 * Process status message 1.
 */
void IFMod::processStatusMessage1(bool isSim) 
{
  registerDone();
}

/**.......................................................................
 * Process status message 2.
 */
void IFMod::processStatusMessage2(bool isSim) 
{
  registerDone();
}

/**.......................................................................
 * Register the receipt of a control-program command that needs to be
 * acknowledged when it has taken effect.
 */
void IFMod::registerRequest(unsigned seq)
{
  if(seq > 0)
    lastReq_ = seq;
}

/**.......................................................................
 * Register completion of a sequence-number marked transaction
 */
void IFMod::registerDone()
{
  if(lastReq_ > lastAck_) {
    parent_->sendIFModDoneMsg(lastReq_);
  }
  
  lastAck_ = lastReq_;
}
