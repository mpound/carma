#include <sstream>
#include <vector>

#include "carma/canbus/Utilities.h"

#include "carma/szautil/CanModule.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

#include "carma/antenna/sza/antenna/canbus/Receiver.h"

#include "carma/antenna/sza/antenna/control/SzaShare.h"

#define CARMA_MODULES

using namespace std;
using namespace carma::canbus;
using namespace sza::antenna::canbus;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
Receiver::Receiver(sza::antenna::control::SzaShare* share, 
		   string boardName,
		   carma::canbus::nodeType node,
		   carma::canbus::CanOutput& io) : 
  CanDevice(share, boardName, sza::util::CanModule::receiverApiNo_, node, io) 
{
  // Get pointers to the registers managed by this object.
  
  monitor_.addMonitorPoint("boardTemperature");
  monitor_.addMonitorPoint("neg15VAnalogVoltage");
  monitor_.addMonitorPoint("pos5VAnalogVoltage");
  monitor_.addMonitorPoint("neg5VDigitalVoltage");
  monitor_.addMonitorPoint("pos15VAnalogVoltage");
  monitor_.addMonitorPoint("pos5VDigitalVoltage");
  monitor_.addMonitorPoint("pos24VAnalogVoltage");
  monitor_.addMonitorPoint("pos28VDigitalVoltage");
  monitor_.addMonitorPoint("bias90GHz");
  monitor_.addMonitorPoint("drainCurrent30GHz");
  monitor_.addMonitorPoint("gateVoltage30GHz");
  monitor_.addMonitorPoint("gateCurrent30GHz");
  monitor_.addMonitorPoint("ifAmpVoltage30GHz");
  monitor_.addMonitorPoint("ifAmpCurrent30GHz");
  monitor_.addMonitorPoint("mixerCurrent30GHz");
  monitor_.addMonitorPoint("ledCurrent30GHz");
  monitor_.addMonitorPoint("gateCurrent90GHz");
  monitor_.addMonitorPoint("drainCurrent90GHz");
  monitor_.addMonitorPoint("ifAmpDrainCurrent90GHz");
  monitor_.addMonitorPoint("ifAmpGateCurrent90GHz");
  monitor_.addMonitorPoint("tempSensor");
  monitor_.addMonitorPoint("tempRadShield");
  monitor_.addMonitorPoint("tempStage2ColdHead");
  monitor_.addMonitorPoint("temp90GHzIsolator");
  monitor_.addMonitorPoint("temp4");
  monitor_.addMonitorPoint("drainSetVoltage30GHz");
  monitor_.addMonitorPoint("drainSetVoltage90GHz");
}

/**.......................................................................
 * Destructor
 */
Receiver::~Receiver() {};

//-----------------------------------------------------------------------
// Commands from the host.
//-----------------------------------------------------------------------

/**.......................................................................
 * Start/stop fast sampling.
 */
std::vector<carma::canbus::Message>
Receiver::toggleFastSampling(unsigned channel, bool start, bool send) 
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  msgType mid;
  
  uShortToData(data, channel);
  
  switch (channel) {
  case 1:
    if(start)
      mid = HOSTCMD_START_CHAN1;
    else
      mid = HOSTCMD_STOP_CHAN1;
    break;
  case 2:
    if(start)
      mid = HOSTCMD_START_CHAN2;
    else
      mid = HOSTCMD_STOP_CHAN2;
    break;
  default:
    {
      LogStream logStr;
      logStr.initMessage(true);
      logStr << "Unknown message type: " << mid;
      throw Error(logStr);
    }
    break;
  }

  if(send)
    postMessage(mid, data);      
  else
    msgs.push_back(nodeMessage(mid, data));
  
  return msgs;
}

//-----------------------------------------------------------------------
// Engineering commands.
//-----------------------------------------------------------------------

/**.......................................................................
 * Set an amplifier input voltage
 */
std::vector<carma::canbus::Message>
Receiver::setAmpBias(Amp amp, signed short value, bool send)
{
  COUT("Inside setAmpBias with amp = " << amp << " value = " << value);

  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  uByteToData(data, amp);
  sShortToData(data, value);
  
  if(send)
    postMessage(ENGCMD_SET_AMP_BIAS, data);
  else {
    msgs.push_back(nodeMessage(ENGCMD_SET_AMP_BIAS, data));
  }
  
  return msgs;
}

/**.......................................................................
 * Set an amplifier input voltage
 */
std::vector<carma::canbus::Message>
Receiver::setAmpBias(sza::util::Rx::Id rxType, 
		     sza::util::Amp::Type ampType, 
		     sza::util::Amp::Stage stage, 
		     sza::util::Amp::Bias biasType,
		     signed short bias, bool send)
{
  // If commanding 30 GHz biases
  
  if(rxType & sza::util::Rx::RX30GHZ) {
    
    // If commanding RF biases
    
    if(ampType & sza::util::Amp::RF) {
      
      if(stage & sza::util::Amp::FIRST) {
	
	if(biasType & sza::util::Amp::DRAIN_VOLTAGE) 
	  std::cout << Amp30GHzRFStage1Vg << std::endl;
	
	if(biasType & sza::util::Amp::DRAIN_CURRENT) 
	  std::cout << Amp30GHzRFStage1Id << std::endl;
      }
      
      if(stage & sza::util::Amp::SECOND) {
	
	if(biasType & sza::util::Amp::DRAIN_VOLTAGE) 
	  std::cout << Amp30GHzRFStage2Vg << std::endl;
	
	
	if(biasType & sza::util::Amp::DRAIN_CURRENT) 
	  std::cout << Amp30GHzRFStage2Id << std::endl;
	
      }
      
      if(stage & sza::util::Amp::THIRD) {
	
	if(biasType & sza::util::Amp::DRAIN_VOLTAGE) 
	  std::cout << Amp30GHzRFStage3Vg << std::endl;
	
	if(biasType & sza::util::Amp::DRAIN_CURRENT) 
	  std::cout << Amp30GHzRFStage3Id << std::endl;
	
      }
      
      if(stage & sza::util::Amp::FOURTH) {
	if(biasType & sza::util::Amp::DRAIN_VOLTAGE) 
	  std::cout << Amp30GHzRFStage4Vg << std::endl;
	
	if(biasType & sza::util::Amp::DRAIN_CURRENT) 
	  std::cout << Amp30GHzRFStage4Id << std::endl;
      }
    }
    
    // If commanding IF biases
    
    if(ampType & sza::util::Amp::IF) 
      std::cout << Amp30GHzIF1 << std::endl;
  }
  
}

//-----------------------------------------------------------------------
// Monitor packet processing.
//-----------------------------------------------------------------------

/**.......................................................................
 * Process monitor packet 1.
 */
void Receiver::processBlankingFrameMonitor1(vector<byteType>& data, bool isSim)
{
  // Extract the monitor points from the data vector.
  
  monitor_.findMonitorPoint("boardTemperature")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("neg15VAnalogVoltage")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("pos5VAnalogVoltage")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("neg5VDigitalVoltage")->writeReg(isSim, dataToShort(data));
}

/**.......................................................................
 * Process monitor packet 2.
 */
void Receiver::processBlankingFrameMonitor2(vector<byteType>& data, bool isSim)
{
#ifdef CARMA_MODULES
  monitor_.findMonitorPoint("pos15VAnalogVoltage")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("pos5VDigitalVoltage")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("pos24VAnalogVoltage")->writeReg(isSim, dataToShort(data));
#else
  // Extract the monitor points from the data vector.
  
  monitor_.findMonitorPoint("pos15VAnalogVoltage")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("pos5VDigitalVoltage")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("pos28VDigitalVoltage")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("bias90GHz")->writeReg(isSim, dataToShort(data));
#endif
}

/**.......................................................................
 * Process monitor packet 3.
 */
void Receiver::processBlankingFrameMonitor3(vector<byteType>& data, bool isSim)
{
  // Extract the monitor points from the data vector.
  
  CoordRange range((unsigned int)0);
  monitor_.findMonitorPoint("drainCurrent30GHz")->writeReg(isSim, dataToShort(data), &range);
  monitor_.findMonitorPoint("gateVoltage30GHz")->writeReg(isSim, dataToShort(data), &range);
  monitor_.findMonitorPoint("gateCurrent30GHz")->writeReg(isSim, dataToShort(data), &range);
  
  range.setIndex(1);
  monitor_.findMonitorPoint("drainCurrent30GHz")->writeReg(isSim, dataToShort(data), &range);
}

/**.......................................................................
 * Process monitor packet 4.
 */
void Receiver::processBlankingFrameMonitor4(vector<byteType>& data, bool isSim)
{
  // Extract the monitor points from the data vector.
  
  CoordRange range(1);
  monitor_.findMonitorPoint("gateVoltage30GHz")->writeReg(isSim, dataToShort(data), &range);
  monitor_.findMonitorPoint("gateCurrent30GHz")->writeReg(isSim, dataToShort(data), &range);
  
  range.setIndex(2);
  monitor_.findMonitorPoint("drainCurrent30GHz")->writeReg(isSim, dataToShort(data), &range);
  monitor_.findMonitorPoint("gateVoltage30GHz")->writeReg(isSim, dataToShort(data), &range);
}

/**.......................................................................
 * Process monitor packet 5.
 */
void Receiver::processBlankingFrameMonitor5(vector<byteType>& data, bool isSim)
{
  // Extract the monitor points from the data vector.
  
  CoordRange range(2);
  monitor_.findMonitorPoint("gateCurrent30GHz")->writeReg(isSim, dataToShort(data), &range);
  
  range.setIndex(3);
  monitor_.findMonitorPoint("drainCurrent30GHz")->writeReg(isSim, dataToShort(data), &range);
  monitor_.findMonitorPoint("gateVoltage30GHz")->writeReg(isSim, dataToShort(data), &range);
  monitor_.findMonitorPoint("gateCurrent30GHz")->writeReg(isSim, dataToShort(data), &range);
}

/**.......................................................................
 * Process monitor packet 6.
 */
void Receiver::processBlankingFrameMonitor6(vector<byteType>& data, bool isSim)
{
  // Extract the monitor points from the data vector.
  
  monitor_.findMonitorPoint("ifAmpVoltage30GHz")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("ifAmpCurrent30GHz")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("mixerCurrent30GHz")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("ledCurrent30GHz")->writeReg(isSim, dataToShort(data));
}

/**.......................................................................
 * Process monitor packet 7.
 */
void Receiver::processBlankingFrameMonitor7(vector<byteType>& data, bool isSim)
{
  // Extract the monitor points from the data vector.
  
  CoordRange range;
  for(unsigned iEl=0; iEl <= 3; iEl++) {
    range.setIndex(iEl);
    monitor_.findMonitorPoint("gateCurrent90GHz")->writeReg(isSim, dataToShort(data), &range);
  }
}

/**.......................................................................
 * Process monitor packet 8.
 */
void Receiver::processBlankingFrameMonitor8(vector<byteType>& data, bool isSim)
{
  // Extract the monitor points from the data vector.
  
  CoordRange range;
  
  for(unsigned iEl=0; iEl <= 1; iEl++) {
    range.setIndex(iEl);
    monitor_.findMonitorPoint("drainCurrent90GHz")->writeReg(isSim, dataToShort(data), &range);
  }
  
  monitor_.findMonitorPoint("ifAmpDrainCurrent90GHz")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("ifAmpGateCurrent90GHz")->writeReg(isSim, dataToShort(data));
}

/**.......................................................................
 * Process monitor packet 9.
 */
void Receiver::processBlankingFrameMonitor9(vector<byteType>& data, bool isSim)
{
  short temp1 = dataToShort(data);
  short temp2 = dataToShort(data);
  short temp3 = dataToShort(data);
  short temp4 = dataToShort(data);

#ifdef CARMA_MODULES
  monitor_.findMonitorPoint("tempRadShield")->writeReg(isSim, temp1);
  monitor_.findMonitorPoint("tempStage2ColdHead")->writeReg(isSim, temp2);
  monitor_.findMonitorPoint("temp90GHzIsolator")->writeReg(isSim, temp3);
  monitor_.findMonitorPoint("temp4")->writeReg(isSim, temp4);
#endif

  // Always pack the data into the tempSensor register array, which is
  // used for the dewar temps
  
  CoordRange range;

  range.setIndex(0);
  monitor_.findMonitorPoint("tempSensor")->writeReg(isSim, temp1, &range);

  range.setIndex(1);
  monitor_.findMonitorPoint("tempSensor")->writeReg(isSim, temp2, &range);

  range.setIndex(2);
  monitor_.findMonitorPoint("tempSensor")->writeReg(isSim, temp3, &range);
}

/**.......................................................................
 * Process monitor packet 10.
 */
void Receiver::processBlankingFrameMonitor10(vector<byteType>& data, bool isSim)
{
  // Extract the monitor points from the data vector.

  CoordRange range;
  for(unsigned iEl=0; iEl <= 3; iEl++) {
    range.setIndex(iEl);
    monitor_.findMonitorPoint("drainSetVoltage30GHz")->writeReg(isSim, dataToShort(data), &range);
  }
}

/**.......................................................................
 * Process monitor packet 11.
 */
void Receiver::processBlankingFrameMonitor11(vector<byteType>& data, bool isSim)
{
  // Extract the monitor points from the data vector.
  
  CoordRange range;
  for(unsigned iEl=0; iEl <= 1; iEl++) {
    range.setIndex(iEl);
    monitor_.findMonitorPoint("drainSetVoltage90GHz")->writeReg(isSim, dataToShort(data), &range);
  }
}

/**.......................................................................
 * Return a map of half-second monitor packets.
 */ 
std::map<carma::canbus::msgType, std::string> 
Receiver::getHalfSecMonitors() const 
{
  map<msgType, string> tmp; 
  
  tmp[MONITOR_PACKET_1] = "Receiver::MONITOR_PACKET_1";
  tmp[MONITOR_PACKET_2] = "Receiver::MONITOR_PACKET_2";
  tmp[MONITOR_PACKET_3] = "Receiver::MONITOR_PACKET_3";
  tmp[MONITOR_PACKET_4] = "Receiver::MONITOR_PACKET_4";
  tmp[MONITOR_PACKET_5] = "Receiver::MONITOR_PACKET_5";
  tmp[MONITOR_PACKET_6] = "Receiver::MONITOR_PACKET_6";
  tmp[MONITOR_PACKET_7] = "Receiver::MONITOR_PACKET_7";
  tmp[MONITOR_PACKET_8] = "Receiver::MONITOR_PACKET_8";
  tmp[MONITOR_PACKET_9] = "Receiver::MONITOR_PACKET_9";
#ifdef CARMA_MODULES
  tmp[MONITOR_PACKET_10] = "Receiver::MONITOR_PACKET_10";
  tmp[MONITOR_PACKET_11] = "Receiver::MONITOR_PACKET_11";
#endif
  return tmp; 
}
