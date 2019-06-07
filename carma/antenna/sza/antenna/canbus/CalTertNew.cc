#include "carma/canbus/Utilities.h"

#include "carma/szautil/CalTertNewFlags.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

#include "carma/antenna/sza/antenna/canbus/CalTertNew.h"
#include "carma/antenna/sza/antenna/control/AntennaRx.h"

using namespace std;
using namespace carma::canbus;
using namespace sza::antenna::canbus;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
CalTertNew::CalTertNew(sza::antenna::control::AntennaRx* parent,
		 sza::antenna::control::SzaShare* share, 
		 std::string boardName,
		 carma::canbus::nodeType node, 
		 carma::canbus::CanOutput& io) :
  CalTert(parent, share, boardName, node, io)
{
  parent_  = parent;
  
  lastReqCalStateSeq_ = 0;
  lastAckCalStateSeq_ = 0;

  lastReqTertStateSeq_ = 0;
  lastAckTertStateSeq_ = 0;

  lastReqTertPosSeq_ = 0;
  lastAckTertPosSeq_ = 0;

  rxId_ = sza::util::Rx::RXUNKNOWN;
  tertState_ = TS_IDLE;

  // Get pointers to the registers managed by this object.
  
  monitor_.addMonitorPoint("tertState");
  monitor_.addMonitorPoint("moveMirOk");
  monitor_.addMonitorPoint("mirStable");
  monitor_.addMonitorPoint("ccwDirLim");
  monitor_.addMonitorPoint("cwDirLim");
  monitor_.addMonitorPoint("ccwUltLim");
  monitor_.addMonitorPoint("cwUltLim");
  monitor_.addMonitorPoint("stepFault");
  monitor_.addMonitorPoint("stepDisabled");
  monitor_.addMonitorPoint("dataValid");
  monitor_.addMonitorPoint("tertStateMask");
  monitor_.addMonitorPoint("tertPos");
  monitor_.addMonitorPoint("encIndex");
  monitor_.addMonitorPoint("encFault");
  monitor_.addMonitorPoint("calibState");
  monitor_.addMonitorPoint("calibTemp");
  monitor_.addMonitorPoint("calId");
  monitor_.addMonitorPoint("calOut");
  monitor_.addMonitorPoint("calIn");
  monitor_.addMonitorPoint("calOutLim");
  monitor_.addMonitorPoint("calInLim");
  monitor_.addMonitorPoint("calDriveFault");
  monitor_.addMonitorPoint("calDisable");
  monitor_.addMonitorPoint("calDone");
  monitor_.addMonitorPoint("calFault");
  monitor_.addMonitorPoint("modTemp");
  monitor_.addMonitorPoint("inPowSup24V");
  monitor_.addMonitorPoint("outPowSup24V");
  monitor_.addMonitorPoint("powSup5V");
  monitor_.addMonitorPoint("tertPosErr");
  monitor_.addMonitorPoint("fpgaMaj");
  monitor_.addMonitorPoint("fpgaMin");
  monitor_.addMonitorPoint("fpgaVer");
  monitor_.addMonitorPoint("ctrlReg");
  monitor_.addMonitorPoint("calHardLimFault");
  monitor_.addMonitorPoint("calStepFault");
  monitor_.addMonitorPoint("calDisableFault");

  monitor_.addMonitorPoint("tertPosError");
}

/**.......................................................................
 * Destructor.
 */
CalTertNew::~CalTertNew() {};

/**.......................................................................
 * Request a calibrator position
 */
std::vector<carma::canbus::Message>
CalTertNew::positionCalibrator(sza::util::CalPos::Pos position, unsigned seq, bool send)
{
  COUT("Inside positionCalibrator: send = " << send);
  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
   
 CalState expectedCalState;

  switch(position) {
  case CalPos::SKY:
    uByteToData(data, 0);
    expectedCalState = CS_SKY;
    break;
  case CalPos::AMBIENT:
    uByteToData(data, 1);
    expectedCalState = CS_AMBIENT;
    break;
  default:
    ThrowError("Unrecognized calibrator position");
    break;
  }
  
  if(send){

    calStateGuard_.lock();
    expectedCalState_   = expectedCalState;
    lastReqCalStateSeq_ = seq;
    calStateGuard_.unlock();

    postMessage(ENGCMD_POS_CAL, data);
  } else {
    msgs.push_back(nodeMessage(ENGCMD_POS_CAL, data));
  }
  
  return msgs;
}

/**.......................................................................
 * Select a tertiary position
 */
std::vector<carma::canbus::Message>
CalTertNew::positionTertiary(sza::util::Rx::Id id, unsigned seq, bool send)
{
  COUT("Inside positionTertiary: send = " << send << " id = " << id);

  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  COUT("Packing mirror position: " << rxIdToMirPos(id));

  uByteToData(data, rxIdToMirPos(id));

  TertState expectedTertState = rxIdToTertState(id);

  if(send) {

    tertStateGuard_.lock();
    expectedTertState_   = expectedTertState;
    lastReqTertStateSeq_ = seq;
    tertStateGuard_.unlock();

    postMessage(ENGCMD_SELECT_RX, data);

    // For bookkeeping purposes, set our internal flag to the requested rx

    setRxId(id);

  } else {
    msgs.push_back(nodeMessage(ENGCMD_SELECT_RX, data));
  }

  return msgs;
}

/**.......................................................................
 * Move the tertiary
 */
std::vector<carma::canbus::Message>
CalTertNew::positionTertiary(sza::util::Angle position, unsigned seq, bool send)
{
  COUT("Inside positionTertiary: send = " << send << " position = " << position.caltertUnits());

  vector<carma::canbus::Message> msgs;
  vector<byteType> data;
  
  sShortToData(data, position.caltertUnits());
  
  if(send) {

    tertPosGuard_.lock();
    expectedTertPos_   = position.caltertUnits();
    lastReqTertPosSeq_ = seq;
    tertPosGuard_.unlock();

    postMessage(ENGCMD_POS_TERT, data);

    // For bookkeeping purposes, set our internal flag to unknown.

    setRxId(sza::util::Rx::RXUNKNOWN);

  } else {
    msgs.push_back(nodeMessage(ENGCMD_POS_TERT, data));
  }

  return msgs;
}

/**.......................................................................
 * Home the tertiary
 */
std::vector<carma::canbus::Message>
CalTertNew::homeTertiary(bool send)
{
  COUT("Inside CAN home tertiary command send = " << send);

  vector<carma::canbus::Message> msgs;
  
  if(send)
    postMessage(ENGCMD_HOME_TERT);
  else
    msgs.push_back(nodeMessage(ENGCMD_HOME_TERT));

  return msgs;
}

//-----------------------------------------------------------------------
// Blanking monitor packet processing
//-----------------------------------------------------------------------

/**.......................................................................
 * Return a map of half-second monitor points.
 */ 
std::map<carma::canbus::msgType, std::string> 
CalTertNew::getHalfSecMonitors() const
{
  map<msgType, string> tmp; 
  
  tmp[MONITOR_PACKET_1] = "CalTertNew::MONITOR_PACKET_1";
  tmp[MONITOR_PACKET_2] = "CalTertNew::MONITOR_PACKET_2";
  tmp[MONITOR_PACKET_3] = "CalTertNew::MONITOR_PACKET_3";
  tmp[MONITOR_PACKET_4] = "CalTertNew::MONITOR_PACKET_4";
  tmp[MONITOR_PACKET_5] = "CalTertNew::MONITOR_PACKET_5";
  
  return tmp;
}

/**.......................................................................
 * Process monitor packet 1.
 */
void CalTertNew::processBlankingFrameMonitor1(std::vector<byteType>& data, 
					   bool isSim)
{
  tertState_ = dataToUbyte(data);

  monitor_.findMonitorPoint("tertState")->writeReg(isSim, tertState_); 
  monitor_.findMonitorPoint("ccwDirLim")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("cwDirLim")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("ccwUltLim")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("cwUltLim")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("stepFault")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("stepDisabled")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("dataValid")->writeReg(isSim, dataToUbyte(data));

  checkTertPos();
}

/**.......................................................................
 * Process monitor packet 2.
 */
void CalTertNew::processBlankingFrameMonitor2(std::vector<byteType>& data, 
					      bool isSim)
{
  short tertPos = dataToShort(data);
  monitor_.findMonitorPoint("tertPos")->writeReg(isSim, tertPos);
  monitor_.findMonitorPoint("encIndex")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("encFault")->writeReg(isSim, dataToUbyte(data));

  unsigned char calibState = dataToUbyte(data);
  monitor_.findMonitorPoint("calibState")->writeReg(isSim, calibState);
  monitor_.findMonitorPoint("calibTemp")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("calId")->writeReg(isSim, dataToUbyte(data));

  // If the current tertiary position matches the last requested one,
  // send a done message to the parent

  tertPosGuard_.lock();
  if(tertPos == expectedTertPos_) {
    registerDone(lastReqTertPosSeq_, lastAckTertPosSeq_);
  }
  tertPosGuard_.unlock();

  calStateGuard_.lock();
  if(calibState == expectedCalState_) {
    registerDone(lastReqCalStateSeq_, lastAckCalStateSeq_);
  }
  calStateGuard_.unlock();
}

/**.......................................................................
 * Process monitor packet 3.
 */
void CalTertNew::processBlankingFrameMonitor3(std::vector<byteType>& data, 
					      bool isSim)
{
  monitor_.findMonitorPoint("calOut")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("calIn")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("calOutLim")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("calInLim")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("calDriveFault")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("calDisable")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("calDone")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("calFault")->writeReg(isSim, dataToUbyte(data));
}

/**.......................................................................
 * Process monitor packet 4.
 */
void CalTertNew::processBlankingFrameMonitor4(std::vector<byteType>& data, 
					      bool isSim)
{
  monitor_.findMonitorPoint("modTemp")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("inPowSup24V")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("outPowSup24V")->writeReg(isSim, dataToShort(data));
  monitor_.findMonitorPoint("powSup5V")->writeReg(isSim, dataToShort(data));
}

/**.......................................................................
 * Process monitor packet 5.
 */
void CalTertNew::processBlankingFrameMonitor5(std::vector<byteType>& data, 
					      bool isSim)
{
  monitor_.findMonitorPoint("tertPosErr")->writeReg(isSim, dataToShort(data));

  unsigned char fpgaMaj = dataToUbyte(data);
  unsigned char fpgaMin = dataToUbyte(data);
  float fpgaVer = (int)fpgaMaj + ((float)((int)fpgaMin))/10;

  monitor_.findMonitorPoint("fpgaMaj")->writeReg(isSim, fpgaMaj);
  monitor_.findMonitorPoint("fpgaMin")->writeReg(isSim, fpgaMin);
  monitor_.findMonitorPoint("fpgaVer")->writeReg(isSim, fpgaVer);

  monitor_.findMonitorPoint("ctrlReg")->writeReg(isSim, dataToUbyte(data));

  monitor_.findMonitorPoint("calHardLimFault")->writeReg(isSim, dataToUbyte(data));
  monitor_.findMonitorPoint("calStepFault")->writeReg(isSim,    dataToUbyte(data));
  monitor_.findMonitorPoint("calDisableFault")->writeReg(isSim, dataToUbyte(data));
}

/**.......................................................................
 * Convert from Rx Id to Mirror Position
 */
CalTertNew::MirPos CalTertNew::rxIdToMirPos(sza::util::Rx::Id rxId)
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
 * Convert from Rx Id to Mirror Position
 */
CalTertNew::TertState CalTertNew::rxIdToTertState(sza::util::Rx::Id rxId)
{
  switch(rxId) {
  case Rx::RX1CM:
    return TS_RX1CM_SELECTED;
    break;
  case Rx::RX3MM:
    return TS_RX3MM_SELECTED;
    break;
  case Rx::RX1MM:
    return TS_RX1MM_SELECTED;
    break;
  default:
    ThrowError("Unrecognized receiver position");
    break;
  }
}

/**.......................................................................
 * Maintain information about the receiver currently selected
 */
void CalTertNew::checkTertPos()
{
  bool err = false;
  
  switch (rxId_) {
  case sza::util::Rx::RXUNKNOWN:
    break;
  case sza::util::Rx::RX30GHZ:
    err = ((int)tertState_ != 5);
    break;
  case sza::util::Rx::RX90GHZ:
    err = ((int)tertState_ != 6);
    break;
  case sza::util::Rx::RX230GHZ:
    err = ((int)tertState_ != 7);
    break;
  default:
    err = true;
    break;
  }

  monitor_.findMonitorPoint("tertPosError")->writeReg(false, err);

  // If the current tertiary state matches the last requested one,
  // send a done message to the parent

  tertStateGuard_.lock();
  if(tertState_ == expectedTertState_) {
    registerDone(lastReqTertStateSeq_, lastAckTertStateSeq_);
  }
  tertStateGuard_.unlock();
}

void CalTertNew::registerDone(unsigned& req, unsigned& ack)
{
  if(req > ack) {
    parent_->sendCalTertDoneMsg(req);
  }
  
  ack = req;
}
