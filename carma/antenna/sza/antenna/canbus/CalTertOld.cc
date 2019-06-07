#include "carma/canbus/Utilities.h"

#include "carma/szautil/CalTertFlags.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

#include "carma/antenna/sza/antenna/canbus/CalTertOld.h"
#include "carma/antenna/sza/antenna/control/AntennaRx.h"

using namespace std;
using namespace carma::canbus;
using namespace sza::antenna::canbus;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
CalTertOld::CalTertOld(sza::antenna::control::AntennaRx* parent,
		 sza::antenna::control::SzaShare* share, 
		 std::string boardName,
		 carma::canbus::nodeType node, 
		 carma::canbus::CanOutput& io) :
  CalTert(parent, share, boardName, node, io)
{
  lastReq_ = 0;
  lastAck_ = 0;
  
  rxId_ = sza::util::Rx::RXUNKNOWN;
  posnCode_ = 3;

  // Get pointers to the registers managed by this object.
  
  monitor_.addMonitorPoint("tertState");
  monitor_.addMonitorPoint("tertStateMask");
  monitor_.addMonitorPoint("moveMirOk");
  monitor_.addMonitorPoint("posnCode");
  monitor_.addMonitorPoint("encPos");
  monitor_.addMonitorPoint("calibState");
  monitor_.addMonitorPoint("calibPosReq");
  monitor_.addMonitorPoint("calibMoveOk");
  monitor_.addMonitorPoint("calibTemp");
  monitor_.addMonitorPoint("powSup5V");
  monitor_.addMonitorPoint("inPowSup24V");
  monitor_.addMonitorPoint("outPowSup24V");
  monitor_.addMonitorPoint("modTemp");
  monitor_.addMonitorPoint("mirStable");
  monitor_.addMonitorPoint("calibStable");
  monitor_.addMonitorPoint("calibTempStable");
  monitor_.addMonitorPoint("calFault");
  monitor_.addMonitorPoint("stepFault");
  monitor_.addMonitorPoint("encFault");        

  monitor_.addMonitorPoint("tertPosError");
}

/**.......................................................................
 * Destructor.
 */
CalTertOld::~CalTertOld() {};

/**.......................................................................
 * Interface with a 1-wire device
 */
std::vector<carma::canbus::Message>
CalTertOld::oneWireInterface(CalTertTypes::OwDevice device, 
			  CalTertTypes::OwCommand command, bool send)
{
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  uByteToData(data, device);
  uByteToData(data, command);
  
  if(send)
    postMessage(ENGCMD_1WIRE_INTERFACE, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_1WIRE_INTERFACE, data));
  
  return msgs;
}

/**.......................................................................
 * Request a calibrator position
 */
std::vector<carma::canbus::Message>
CalTertOld::positionCalibrator(sza::util::CalPos::Pos position, bool send)
{
  COUT("Inside positionCalibrator: send = " << send);
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  switch(position) {
  case CalPos::SKY:
    uByteToData(data, 0);
    break;
  case CalPos::AMBIENT:
    uByteToData(data, 1);
    break;
  case CalPos::HOTLOAD:
    uByteToData(data, 2);
    break;
  default:
    ThrowError("Unrecognized calibrator position");
    break;
  }
  
  if(send)
    postMessage(ENGCMD_POS_CAL, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_POS_CAL, data));
  
  return msgs;
}

/**.......................................................................
 * Home the tertiary
 */
std::vector<carma::canbus::Message>
CalTertOld::homeTertiary(bool send)
{
  COUT("Inside homeTertiary: send = " << send);
  vector<carma::canbus::Message> msgs;
  
  if(send)
    postMessage(ENGCMD_HOME_TERT);
  else
    msgs.push_back(nodeMessage(ENGCMD_HOME_TERT));

  return msgs;
}

/**.......................................................................
 * Move the tertiary
 */
std::vector<carma::canbus::Message>
CalTertOld::positionTertiary(unsigned short position, bool send)
{
  COUT("Inside positionTertiary (1): send = " << send);
  vector<carma::canbus::Message> msgs;
  
  vector<byteType> data;
  
  uByteToData(data, 3);
  uShortToData(data, position);
  
  if(send)
    postMessage(ENGCMD_POS_TERT, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_POS_TERT, data));
  return msgs;
}

/**.......................................................................
 * Move the tertiary
 */
std::vector<carma::canbus::Message>
CalTertOld::positionTertiary(sza::util::Rx::Id id, bool send)
{
  COUT("Inside positionTertiary (2): send = " << send);
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  uByteToData(data, rxIdToMirPos(id));

  if(send)
    postMessage(ENGCMD_POS_TERT, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_POS_TERT, data));

  return msgs;
}

/**.......................................................................
 * Enable the tertiary
 */
std::vector<carma::canbus::Message>
CalTertOld::enableTertiary(bool enable, bool send)
{
  COUT("Inside enableTertiary: send = " << send);
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  uByteToData(data, enable ? 1 : 0);
  
  if(send)
    postMessage(ENGCMD_ENABLE_TERT, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_ENABLE_TERT, data));

  COUT("Inside enableTertiary: leaving");

  return msgs;
}

/**.......................................................................
 * Reset the stepper driver
 */
std::vector<carma::canbus::Message>
CalTertOld::resetStepper(bool send)
{
  COUT("Inside resetStepper: send = " << send);
  vector<carma::canbus::Message> msgs;
  
  if(send)
    postMessage(ENGCMD_RESET_STEPPER);
  else
    msgs.push_back(nodeMessage(ENGCMD_RESET_STEPPER));

  return msgs;
}

/**.......................................................................
 * Write the current encoder position to one of the positions
 * indexed as 30GHz, 90GHz or 230GHz
 */
std::vector<carma::canbus::Message>
CalTertOld::indexCurrentEncoderPosition(sza::util::Rx::Id id, bool send)
{
  COUT("Inside indexCurrentEncoderPosition: send = " << send);
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  uByteToData(data, rxIdToMirPos(id));
  if(send)
    postMessage(ENGCMD_IND_CURR_ENC_POS, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_IND_CURR_ENC_POS, data));

  return msgs;
}

/**.......................................................................
 * Write the current encoder position to one of the positions
 * indexed as 30GHz, 90GHz or 230GHz
 */
std::vector<carma::canbus::Message>
CalTertOld::setEncoderPositionIndex(sza::util::Rx::Id id,
				 unsigned short position, bool send)
{
  COUT("Inside setEncoderPositionIndex: send = " << send);
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;

  // If no position was given, go to the stored index mark
  
  if(position==0) 
    position = getEncoderPosition(id);

  uByteToData(data, rxIdToMirPos(id));
  uShortToData(data, position);
  
  if(send)
    postMessage(ENGCMD_SET_ENC_POS, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_SET_ENC_POS, data));

  return msgs;
}

/**.......................................................................
 * Write the current encoder position to one of the positions
 * indexed as 30GHz, 90GHz or 230GHz
 */
std::vector<carma::canbus::Message>
CalTertOld::setDefaultEncoderPositionIndex(sza::util::Rx::Id id, bool send)
{
  COUT("Inside setDefaultEncoderPositionIndex: send = " << send);

  vector<carma::canbus::Message> msgs;
  vector<byteType> data;

  // If no position was given, go to the stored index mark
  
  unsigned short position = getEncoderPosition(id);

  uByteToData(data, rxIdToMirPos(id));
  uShortToData(data, position);
  
  if(send)
    postMessage(ENGCMD_SET_ENC_POS, data);
  else
    msgs.push_back(nodeMessage(ENGCMD_SET_ENC_POS, data));

  COUT("Inside setDefaultEncoderPositionIndex: done: msgId = " << msgs[0].getId());

  return msgs;
}

void CalTertOld::storeEncoderPositionIndex(sza::util::Rx::Id id, 
				       unsigned short index)
{
  switch (id) {
  case sza::util::Rx::RX30GHZ:
    rx30GHzIndex_ = index;
    break;
  case sza::util::Rx::RX90GHZ:
    rx90GHzIndex_ = index;
    break;
  default:
    break;
  }
}

unsigned short CalTertOld::getEncoderPosition(sza::util::Rx::Id id)
{
  switch (id) {
  case sza::util::Rx::RX30GHZ:
    return rx30GHzIndex_;
    break;
  case sza::util::Rx::RX90GHZ:
    return rx90GHzIndex_;
    break;
  default:
    return 0;
    break;
  }
}

//-----------------------------------------------------------------------
// Blanking monitor packet processing
//-----------------------------------------------------------------------

/**.......................................................................
 * Return a map of half-second monitor points.
 */ 
std::map<carma::canbus::msgType, std::string> 
CalTertOld::getHalfSecMonitors() const
{
  map<msgType, string> tmp; 
  
  tmp[MONITOR_PACKET_1] = "CalTertOld::MONITOR_PACKET_1";
  tmp[MONITOR_PACKET_2] = "CalTertOld::MONITOR_PACKET_2";
  tmp[MONITOR_PACKET_3] = "CalTertOld::MONITOR_PACKET_3";
  tmp[MONITOR_PACKET_4] = "CalTertOld::MONITOR_PACKET_4";
  
  return tmp;
}

/**.......................................................................
 * Process monitor packet 1.
 */
void CalTertOld::processBlankingFrameMonitor1(std::vector<byteType>& data, 
					   bool isSim)
{
  unsigned char tertState = dataToUbyte(data);

  monitor_.findMonitorPoint("tertState")->writeReg(isSim, tertState); 
  monitor_.findMonitorPoint("moveMirOk")->writeReg(isSim, dataToUbyte(data));

  posnCode_ = dataToUbyte(data);
  monitor_.findMonitorPoint("posnCode")->writeReg(isSim, posnCode_);

  monitor_.findMonitorPoint("encPos")->writeReg(isSim, dataToUshort(data));
  monitor_.findMonitorPoint("calibState")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("calibPosReq")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("calibMoveOk")->writeReg(isSim, dataToUbyte(data));

  // And write an extended bitmask version of tertState

  monitor_.findMonitorPoint("tertStateMask")->writeReg(isSim, tertStateToBit(tertState));

  // Since this monitor packet includes the position code, check the
  // caltert position for errors now

  checkTertPos();
}

/**.......................................................................
 * Process monitor packet 2.
 */
void CalTertOld::processBlankingFrameMonitor2(std::vector<byteType>& data, 
					   bool isSim)
{
  monitor_.findMonitorPoint("inPowSup24V")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("outPowSup24V")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("powSup5V")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("calibTemp")->writeReg(isSim, dataToShort(data));
}

/**.......................................................................
 * Process monitor packet 3.
 */
void CalTertOld::processBlankingFrameMonitor3(std::vector<byteType>& data, 
					   bool isSim)
{
  monitor_.findMonitorPoint("modTemp")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("mirStable")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("calibStable")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("calibTempStable")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("calFault")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("stepFault")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("encFault")->writeReg(isSim, dataToUbyte(data));
}

/**.......................................................................
 * Process monitor packet 4.
 */
void CalTertOld::processBlankingFrameMonitor4(std::vector<byteType>& data, 
					   bool isSim) {}

//-----------------------------------------------------------------------
// Status message processing
//-----------------------------------------------------------------------

/**.......................................................................
 * Return a map of half-second monitor points.
 */ 
std::map<carma::canbus::msgType, std::string> 
CalTertOld::getStatusMessages() const
{
  map<msgType, string> tmp; 
  
  tmp[STATUS_MSG_1] = "CalTertOld::STATUS_MSG_1";
  tmp[STATUS_MSG_2] = "CalTertOld::STATUS_MSG_2";
  
  return tmp;
}

/**.......................................................................
 * Process status message 1.
 */
void CalTertOld::processStatusMessage1(bool isSim) 
{
  registerDone();
}

/**.......................................................................
 * Process status message 2.
 */
void CalTertOld::processStatusMessage2(bool isSim) 
{
  registerDone();
}

/**.......................................................................
 * Register the receipt of a control-program command that needs to be
 * acknowledged when it has taken effect.
 */
void CalTertOld::registerRequest(unsigned seq)
{
  if(seq > 0)
    lastReq_ = seq;
}

/**.......................................................................
 * Register completion of a sequence-number marked transaction
 */
void CalTertOld::registerDone()
{
  if(lastReq_ > lastAck_) {
    parent_->sendCalTertDoneMsg(lastReq_);
  }
  
  lastAck_ = lastReq_;
}


/**.......................................................................
 * Convert from Rx Id to Mirror Position
 */
CalTertOld::MirPos CalTertOld::rxIdToMirPos(sza::util::Rx::Id rxId)
{
  LogStream errStr;
  
  switch(rxId) {
  case Rx::RX30GHZ:
    return RX30GHZ;
    break;
  case Rx::RX90GHZ:
    return RX90GHZ;
    break;
  case Rx::RX230GHZ:
    return RX230GHZ;
    break;
  default:
    ThrowError("Unrecognized receiver position");
    break;
  }
}

/**.......................................................................
 * Register a handler to be called when the tertiary reports
 * an in-position message
 */
void CalTertOld::registerTertiaryInPositionHandler(CAN_STATUS_MSG_HANDLER(*handler), 
						void* arg1, unsigned arg2)
{
  installHandler(STATUS_MSG_2, handler, arg1, arg2);
}

/**.......................................................................
 * Convert from integer state to bit state
 */
unsigned char CalTertOld::tertStateToBit(unsigned char tertState)
{
  switch(tertState) {
  case IN_POSITION:
    return CalTertFlags::IN_POSITION;
    break;
  case MOVING:
    return CalTertFlags::MOVING;
    break;
  case HOMING:
    return CalTertFlags::HOMING;
    break;
  case STOPPED:
    return CalTertFlags::STOPPED;
    break;
  case POS_SOFT_LIMIT:
    return CalTertFlags::POS_SOFT_LIMIT;
    break;
  case NEG_SOFT_LIMIT:
    return CalTertFlags::NEG_SOFT_LIMIT;
    break;
  case HARD_LIMIT:
    return CalTertFlags::HARD_LIMIT;
    break;
  case ERROR:
    return CalTertFlags::ERROR;
    break;
  default:
    ThrowError("Unrecognized tertiary state");
    break;
  }
}

/**.......................................................................
 * Maintain information about the receiver currently selected
 */
void CalTertOld::checkTertPos()
{
  bool err = false;
  
  switch (rxId_) {
  case sza::util::Rx::RXUNKNOWN:
    break;
  case sza::util::Rx::RX30GHZ:
    err = (posnCode_ != 0);
    break;
  case sza::util::Rx::RX90GHZ:
    err = (posnCode_ != 1);
    break;
  case sza::util::Rx::RX230GHZ:
    err = (posnCode_ != 2);
    break;
  default:
    err = true;
    break;
  }

  monitor_.findMonitorPoint("tertPosError")->writeReg(false, err);
}
