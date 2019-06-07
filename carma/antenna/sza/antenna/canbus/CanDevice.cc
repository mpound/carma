#include "carma/canbus/Message.h"
#include "carma/canbus/Utilities.h"

#include "carma/szautil/CanModule.h"
#include "carma/szautil/Date.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/TimeVal.h"

#include "carma/antenna/sza/antenna/canbus/CanDevice.h"

#include "carma/antenna/sza/antenna/control/SzaShare.h"

#include "carma/szaarrayutils/regmap.h"

using namespace std;

using namespace carma::canbus;

using namespace sza::antenna::canbus;
using namespace sza::util;

#define CARMA_MODULES

/**.......................................................................
 * Constructor.  
 *
 * @param share Pointer to the shared memory object used for
 * monitoring
 *
 * @param boardName The name of the board in the shared
 * memory object which this device will control.
 *
 * @param api An identifier for the API version of
 * inheritors of this class.
 *
 * @param node The CAN node id
 *
 * @param io Reference to CanOutput class - should be
 * obtained from downcasting the canbus::Master reference in
 * inherited canbus::Master class.
 */
CanDevice::CanDevice(sza::antenna::control::SzaShare* share,
		     string boardName,
		     carma::canbus::apiType api,
		     carma::canbus::nodeType node, 
		     carma::canbus::CanOutput& io) : 
  Board(share, boardName), Device(api, node, io), monitor_(share, boardName)
{
  // Initialize monitor points common to all CAN devices

  initRegs();

  // And initialize the rx id to unknown

  setRxId(sza::util::Rx::RXUNKNOWN);

  apiNo_ = api;
}

/**.......................................................................
 * Init regs common to all can devices
 */
void CanDevice::initRegs()
{
  // My own monitor points

  monitor_.addMonitorPoint("received");

  CanMonitorPoint* api = monitor_.addMonitorPoint("apiNo");
  api->writeReg(true, (unsigned short)api_);

  monitor_.addMonitorPoint("swVersion");
  monitor_.addMonitorPoint("swVersionStr");
  monitor_.addMonitorPoint("dongleId");

  // From system monitor packet 1

  monitor_.addMonitorPoint("serialNo");
  monitor_.addMonitorPoint("moduleType");
  monitor_.addMonitorPoint("initRequest");
  monitor_.addMonitorPoint("rxErrors");
  monitor_.addMonitorPoint("txErrors");
  monitor_.addMonitorPoint("memoryErrors");
  monitor_.addMonitorPoint("systemErrors");

  // From system monitor packet 2

  monitor_.addMonitorPoint("schOverflowCnt");
  monitor_.addMonitorPoint("tSchOverflowCnt");
  monitor_.addMonitorPoint("swVerMaj");
  monitor_.addMonitorPoint("swVerMin");
  monitor_.addMonitorPoint("swVerTst");
  monitor_.addMonitorPoint("testMode");

  // From system monitor packet 3

  monitor_.addMonitorPoint("commErrCnt");
  monitor_.addMonitorPoint("timeErrCnt");
  monitor_.addMonitorPoint("swErrCnt");
  monitor_.addMonitorPoint("hwErrCnt");

  // From system monitor packet 4

  monitor_.addMonitorPoint("timeJitter");
  monitor_.addMonitorPoint("sinceLastTs");
  monitor_.addMonitorPoint("tsDelta");
  monitor_.addMonitorPoint("apiVer");
  monitor_.addMonitorPoint("timeOffset");
  monitor_.addMonitorPoint("timeStampInt");
  monitor_.addMonitorPoint("timeStampDelta");

  // From system monitor packet 5

  monitor_.addMonitorPoint("uptime");
  monitor_.addMonitorPoint("bootLoader");
  monitor_.addMonitorPoint("buildTime");
  monitor_.addMonitorPoint("buildDate");
}

/**.......................................................................
 * Destructor.
 */
CanDevice::~CanDevice() {}

/**.......................................................................
 * Return a CAN message ID constructed from the api, node and command
 * ID.
 */
idType CanDevice::createDummyId()
{
  return carma::canbus::createId(false, 255, node_, 0x3FF);
}

/**.......................................................................
 * Return a CAN message ID constructed from the api, node and command
 * ID.
 */
idType CanDevice::createId(bool host, msgType mid)
{
  return carma::canbus::createId(host, api_, node_, mid);
}

/**.......................................................................
 * Return a CAN message ID constructed from the api, node and command
 * ID.
 */
idType CanDevice::createEngId(bool host, msgType mid)
{
  return carma::canbus::createEngId(host, api_, node_, mid);
}

/**.......................................................................
 * Return a CAN bus ID suitable for passing to Message constructor
 */
busIdType CanDevice::getBusId()
{
  return (node_ == 0 ? ALL_BUSSES : carma::canbus::Device::getBusId());
}

/**.......................................................................
 * Return a map of device controls.
 */
std::map<carma::canbus::msgType, std::string> 
CanDevice::getControls() 
{
  map<msgType, string> commonControls   = getCommonControls();
  map<msgType, string> specificControls = getSpecificControls();
  map<msgType, string> controls;
  
  // Call the base class method to get controls common to all devices
  
  for(std::map<msgType, string>::iterator imap=commonControls.begin(); 
      imap != commonControls.end(); imap++) {
    controls[imap->first] = imap->second;
  }
  
  // Call the inheritor-specific method to get controls for this device
  
  for(std::map<msgType, string>::iterator imap=specificControls.begin(); 
      imap != specificControls.end(); imap++) {
    controls[imap->first] = imap->second;
  }
  
  return controls;
}

/**.......................................................................
 * Return a map of controls common to all devices.
 */
std::map<carma::canbus::msgType, std::string> 
CanDevice::getCommonControls()
{
  map<msgType, string> tmp;
  std::string prefix = controlPrefix();
  
  tmp[HOSTCMD_RESET]       = prefix+"HOSTCMD_TEST_MODE";
  tmp[HOSTCMD_SET_TIME]    = prefix+"HOSTCMD_SET_TIME";
  tmp[HOSTCMD_STOP_CHAN1]  = prefix+"HOSTCMD_STOP_CHAN1";
  tmp[HOSTCMD_STOP_CHAN2]  = prefix+"HOSTCMD_STOP_CHAN2";
  tmp[HOSTCMD_START_CHAN1] = prefix+"HOSTCMD_START_CHAN1";
  tmp[HOSTCMD_START_CHAN2] = prefix+"HOSTCMD_START_CHAN2";
  
  return tmp;
}

/**.......................................................................
 * Return a map of device controls.
 */
std::map<carma::canbus::msgType, std::string> 
CanDevice::getSpecificControls() const 
{
  map<msgType, string> emptyMap;
  return emptyMap;
}

/**.......................................................................
 * Process a CAN message. 
 */
void CanDevice::processMsg(msgType messageId, std::vector<byteType>& data, 
			   bool isSim) 
{
  // Process each type of message.
  
  try {
  if(isBlankingFrameMonitor(messageId))
    processBlankingFrameMonitor(messageId, data, isSim);
  
  else if(isSlowMonitor(messageId))
    processSlowMonitor(messageId, data, isSim);
  
  else if(isEngineeringMonitor(messageId))
    processEngineeringMonitor(messageId, data, isSim);
  
  else if(isStatusMessage(messageId))
    processStatusMessage(messageId, isSim);
  
  else {

    //    COUT("Got an unknown message (" << messageId << ")" << " for api: " << apiNo_);

    //    ThrowError("Unknown message type: " << messageId);
  }
  } catch(Exception& err) {
    cout << err.what() << endl;
    throw err;
  }
  
  // If this device is flagged as OFFLINE, and this is a real monitor
  // packet, tell the CanMaster it is back online
  
  if((getState() != ONLINE) && !isSim) 
    setState(ONLINE);
}

/**.......................................................................
 * Simulate a CAN message. 
 */
carma::canbus::Message CanDevice::simulateMsg(msgType messageId)
{
  // Simulate each type of message.
  
  if(isBlankingFrameMonitor(messageId))
    return simulateBlankingFrameMonitor(messageId);
}

/**.......................................................................
 * Simulate a message to this device.
 */
carma::canbus::Message CanDevice::simulateBlankingFrameMonitor(msgType mid) 
{
  DBPRINT(false, Debug::DEBUG16, "Inside: mid = " << idOfBlankingFrameMonitorMsgType(mid));
  
  carma::canbus::Message msg;
  
  switch(idOfBlankingFrameMonitorMsgType(mid)) {
  case 1:
    msg = simulateBlankingFrameMonitor1();
    break;
  case 2:
    msg = simulateBlankingFrameMonitor2();
    break;
  case 3:
    msg = simulateBlankingFrameMonitor3();
    break;
  case 4:
    msg = simulateBlankingFrameMonitor4();
    break;
  case 5:
    msg = simulateBlankingFrameMonitor5();
    break;
  case 6:
    msg = simulateBlankingFrameMonitor6();
    break;
  case 7:
    msg = simulateBlankingFrameMonitor7();
    break;
  case 8:
    msg = simulateBlankingFrameMonitor8();
    break;
  case 9:
    msg = simulateBlankingFrameMonitor9();
    break;
  case 10:
    msg = simulateBlankingFrameMonitor10();
    break;
  case 11:
    msg = simulateBlankingFrameMonitor11();
    break;
  default:
    msg = simulateGenericBlankingFrameMonitor(mid);
    break;
  }
  return msg;
}

/**.......................................................................
 * Stub out monitor packet sims
 */
carma::canbus::Message CanDevice::simulateBlankingFrameMonitor1() 
{
  return simulateGenericBlankingFrameMonitor(msgTypeOfBlankingFrameMonitorId(1));
}

carma::canbus::Message CanDevice::simulateBlankingFrameMonitor2() 
{
  return simulateGenericBlankingFrameMonitor(msgTypeOfBlankingFrameMonitorId(2));
}

carma::canbus::Message CanDevice::simulateBlankingFrameMonitor3() 
{
  return simulateGenericBlankingFrameMonitor(msgTypeOfBlankingFrameMonitorId(3));
}

carma::canbus::Message CanDevice::simulateBlankingFrameMonitor4() 
{
  return simulateGenericBlankingFrameMonitor(msgTypeOfBlankingFrameMonitorId(4));
}

carma::canbus::Message CanDevice::simulateBlankingFrameMonitor5() 
{
  return simulateGenericBlankingFrameMonitor(msgTypeOfBlankingFrameMonitorId(5));
}

carma::canbus::Message CanDevice::simulateBlankingFrameMonitor6() 
{
  return simulateGenericBlankingFrameMonitor(msgTypeOfBlankingFrameMonitorId(6));
}

carma::canbus::Message CanDevice::simulateBlankingFrameMonitor7() 
{
  return simulateGenericBlankingFrameMonitor(msgTypeOfBlankingFrameMonitorId(7));
}

carma::canbus::Message CanDevice::simulateBlankingFrameMonitor8() 
{
  return simulateGenericBlankingFrameMonitor(msgTypeOfBlankingFrameMonitorId(8));
}

carma::canbus::Message CanDevice::simulateBlankingFrameMonitor9() 
{
  return simulateGenericBlankingFrameMonitor(msgTypeOfBlankingFrameMonitorId(9));
}

carma::canbus::Message CanDevice::simulateBlankingFrameMonitor10() 
{
  return simulateGenericBlankingFrameMonitor(msgTypeOfBlankingFrameMonitorId(10));
}

carma::canbus::Message CanDevice::simulateBlankingFrameMonitor11() 
{
  return simulateGenericBlankingFrameMonitor(msgTypeOfBlankingFrameMonitorId(11));
}


/**.......................................................................
 * Simulate a generic monitor packet
 */
carma::canbus::Message CanDevice::simulateGenericBlankingFrameMonitor(carma::canbus::msgType mid) 
{
  carma::canbus::Message msg;
  vector<byteType> data;
  
  uShortToData(data, 0);
  uShortToData(data, 0);
  uShortToData(data, 0);
  uShortToData(data, 0);
  
  return hostMessage(mid, data);
};


//-----------------------------------------------------------------------
// Blanking frame methods
//-----------------------------------------------------------------------

/**
 * Return a map of devices half second monitor points.
 */ 
std::map<carma::canbus::msgType, std::string> 
CanDevice::getHalfSecMonitors() const
{
  map<msgType, string> tmp; 
  return tmp; 
}

/**.......................................................................
 * Return true if this is a blanking monitor packet.
 */
bool CanDevice::isBlankingFrameMonitor(msgType mid)
{
  std::map<carma::canbus::msgType, std::string> monitors = getHalfSecMonitors();
  std::map<carma::canbus::msgType, std::string>::iterator slot;
  
  slot = monitors.find(mid);
  
  if(slot != monitors.end())
    return true;
  
  return false;
}

/**.......................................................................
 * Return a standard id corresponding to this blanking frame id
 */
unsigned CanDevice::idOfBlankingFrameMonitorMsgType(msgType mid)
{
  std::map<carma::canbus::msgType, std::string> monitors = getHalfSecMonitors();
  std::map<carma::canbus::msgType, std::string>::iterator slot;
  
  // Throw an erorr if this is not a recognized blanking frame id
  
  if(!isBlankingFrameMonitor(mid)) {
    LogStream errStr;
    errStr.initMessage(true);
    errStr.setf(std::ios::showbase | std::ios::hex);
    errStr << "Unrecognized blanking frame message id: " << mid << endl;
    errStr.unsetf(std::ios::showbase | std::ios::hex);
    throw Error(errStr);
  }
  
  // Return an integer index corresponding to the msg type
  
  unsigned islot=1;
  for(slot=monitors.begin(); slot != monitors.end(); slot++, islot++)
    if(slot->first == mid) 
      return islot;
}

/**.......................................................................
 * Return a standard id corresponding to this blanking frame id
 */
carma::canbus::msgType CanDevice::msgTypeOfBlankingFrameMonitorId(unsigned id)
{
  std::map<carma::canbus::msgType, std::string> monitors = getHalfSecMonitors();
  std::map<carma::canbus::msgType, std::string>::iterator slot;
  
  
  if(id < 1 || id > monitors.size()+1) {
    LogStream errStr;
    errStr.initMessage(true);
    errStr << "Unrecognized blanking frame id requested: " << id << endl;
    throw Error(errStr);
  }
  
  unsigned islot=1;
  for(slot=monitors.begin(); slot != monitors.end(); slot++, islot++)
    if(islot == id)
      return slot->first;
}

/**.......................................................................
 * Process a blanking frame monitor packet.
 */
void CanDevice::processBlankingFrameMonitor(msgType mid, 
					    vector<byteType>& data, bool isSim) 
{
  switch(idOfBlankingFrameMonitorMsgType(mid)) {
  case 1:
    processBlankingFrameMonitor1(data, isSim);
    registerReceived(1, isSim);
    break;
  case 2:
    processBlankingFrameMonitor2(data, isSim);
    registerReceived(2, isSim);
    break;
  case 3:
    processBlankingFrameMonitor3(data, isSim);
    registerReceived(3, isSim);
    break;
  case 4:
    processBlankingFrameMonitor4(data, isSim);
    registerReceived(4, isSim);
    break;
  case 5:
    processBlankingFrameMonitor5(data, isSim);
    registerReceived(5, isSim);
    break;
  case 6:
    processBlankingFrameMonitor6(data, isSim);
    registerReceived(6, isSim);
    break;
  case 7:
    processBlankingFrameMonitor7(data, isSim);
    registerReceived(7, isSim);
    break;
  case 8:
    processBlankingFrameMonitor8(data, isSim);
    registerReceived(8, isSim);
    break;
  case 9:
    processBlankingFrameMonitor9(data, isSim);
    registerReceived(9, isSim);
    break;
  case 10:
    processBlankingFrameMonitor10(data, isSim);
    registerReceived(10, isSim);
    break;
  case 11:
    processBlankingFrameMonitor11(data, isSim);
    registerReceived(11, isSim);
    break;
  default:
    {
      LogStream errStr;
      errStr.initMessage(true);
      errStr.setf(std::ios::showbase | std::ios::hex);
      errStr << "Unknown blanking frame message id: " << mid << "";
      errStr.unsetf(std::ios::showbase | std::ios::hex);
      errStr << mid << ")" << endl;
      throw Error(errStr);
    }
    break;
  }
};

/**.......................................................................
 * Register receipt of a blanking frame monitor packet.
 */
void CanDevice::registerReceived(unsigned id, bool isSim)
{
  unsigned short received;
  CanMonitorPoint* monitorPoint = monitor_.findMonitorPoint("received");
  RegMapBlock* receivedReg = monitorPoint->block();
  
  share_->readReg(receivedReg, &received);
  
  if(isSim)
    received &= ~(1U << id);
  else
    received |=  (1U << id);

  monitorPoint->writeReg(isSim, received);
}

//-----------------------------------------------------------------------
// Slow monitor methods.
//-----------------------------------------------------------------------


/**.......................................................................
 * Return a map of devices slow monitor points (same for all CAN
 * devices).
 */
std::map<carma::canbus::msgType, std::string> 
CanDevice::getSlowMonitors() const 
{
  map<msgType, string> tmp; 
  
  tmp[SLOW_MONITOR_1] = "CanDevice::SLOW_MONITOR_1";
  tmp[SLOW_MONITOR_2] = "CanDevice::SLOW_MONITOR_2";
  tmp[SLOW_MONITOR_3] = "CanDevice::SLOW_MONITOR_3";
  tmp[SLOW_MONITOR_4] = "CanDevice::SLOW_MONITOR_4";
  tmp[SLOW_MONITOR_5] = "CanDevice::SLOW_MONITOR_5";
  
  return tmp; 
}

/**.......................................................................
 * Return true if this is a slow monitor packet.
 */
bool CanDevice::isSlowMonitor(msgType mid)
{
  std::map<carma::canbus::msgType, std::string> monitors = getSlowMonitors();
  std::map<carma::canbus::msgType, std::string>::iterator slot;
  
  slot = monitors.find(mid);
  
  if(slot != monitors.end())
    return true;
  
  return false;
}

/**.......................................................................
 * Process a slow monitor packet.
 */
void CanDevice::processSlowMonitor(msgType mid, vector<byteType>& data, bool isSim)
{
  switch(mid) {
  case SLOW_MONITOR_1:
    processSlowMonitor1(data, isSim);
    break;
  case SLOW_MONITOR_2:
    processSlowMonitor2(data, isSim);
    break;
  case SLOW_MONITOR_3:
    processSlowMonitor3(data, isSim);
    break;
  case SLOW_MONITOR_4:
    processSlowMonitor4(data, isSim);
    break;
  case SLOW_MONITOR_5:
    processSlowMonitor5(data, isSim);
    break;
  default:
    {
      LogStream errStr;
      errStr.initMessage(true);
      errStr << "Unknown message type: " 
	     << mid << endl;
      throw Error(errStr);
    }
  }
}

/**.......................................................................
 * Process the first slow monitor packet.
 */
void CanDevice::processSlowMonitor1(vector<byteType>& data, bool isSim)
{
  monitor_.findMonitorPoint("serialNo")->writeReg(isSim, dataToUshort(data));
  
  moduleType_     = dataToUbyte(data);
  initRequest_    = dataToUbyte(data);
  rxErrors_       = dataToUbyte(data);
  txErrors_       = dataToUbyte(data);
  memoryErrors_   = dataToUbyte(data);
  systemErrors_   = dataToUbyte(data);

  monitor_.findMonitorPoint("moduleType")->writeReg(isSim,   (unsigned char)moduleType_);
  monitor_.findMonitorPoint("initRequest")->writeReg(isSim,  (unsigned char)initRequest_);
  monitor_.findMonitorPoint("rxErrors")->writeReg(isSim,     (unsigned char)rxErrors_);
  monitor_.findMonitorPoint("txErrors")->writeReg(isSim,     (unsigned char)txErrors_);
  monitor_.findMonitorPoint("memoryErrors")->writeReg(isSim, (unsigned char)memoryErrors_);
  monitor_.findMonitorPoint("systemErrors")->writeReg(isSim, (unsigned char)systemErrors_);
}

/**.......................................................................
 * Process the first slow monitor packet.
 */
void CanDevice::processSlowMonitor2(vector<byteType>& data, bool isSim)
{
#ifdef CARMA_MODULES

  // Write the scheduler overflow count

  schedulerOverflowCount_      = dataToUshort(data);

  monitor_.findMonitorPoint("schOverflowCnt")->
    writeReg(isSim, schedulerOverflowCount_);

  // Write the timed scheduler overflow count

  timedSchedulerOverflowCount_ = dataToUshort(data);

  monitor_.findMonitorPoint("tSchOverflowCnt")->
    writeReg(isSim, timedSchedulerOverflowCount_);

  // Write the software version

  softwareMajorVersion_ = dataToUbyte(data);
  monitor_.findMonitorPoint("swVerMaj")->writeReg(isSim, (unsigned char)softwareMajorVersion_);

  softwareMinorVersion_ = dataToUbyte(data);
  monitor_.findMonitorPoint("swVerMin")->writeReg(isSim, (unsigned char)softwareMinorVersion_);

  softwareTestVersion_  = dataToUbyte(data);
  monitor_.findMonitorPoint("swVerTst")->writeReg(isSim, (unsigned char)softwareTestVersion_);

  // Format the information into a complete version string

  char str[12];
  for(unsigned i=0; i < 12; i++) {
    str[i] = '\0';
  }

  sprintf(str, "%d.%d.%d", softwareMajorVersion_, softwareMinorVersion_, softwareTestVersion_);
  monitor_.findMonitorPoint("swVersionStr")->writeReg(isSim, (unsigned char*)str);

  unsigned char testMode = dataToUbyte(data);
  monitor_.findMonitorPoint("testMode")->writeReg(isSim, (unsigned char)testMode);

#else
  schedulerOverflowCount_      = dataToUshort(data);

  monitor_.findMonitorPoint("schOverflowCnt")->
    writeReg(isSim, schedulerOverflowCount_);

  timedSchedulerOverflowCount_ = dataToUshort(data);

  monitor_.findMonitorPoint("tSchOverflowCnt")->
    writeReg(isSim, timedSchedulerOverflowCount_);

  CoordRange range(0, 0);
  softwareMajorVersion_ = dataToUbyte(data);
  monitor_.findMonitorPoint("swVersion")->
    writeReg(isSim, softwareMajorVersion_, &range);
  
  range.setStartIndex(0, 1);
  range.setStopIndex(0, 1);
  softwareMinorVersion_ = dataToUbyte(data);
  monitor_.findMonitorPoint("swVersion")->
    writeReg(isSim, softwareMinorVersion_, &range);
  
  range.setStartIndex(0, 2);
  range.setStopIndex(0, 2);
  softwareTestVersion_ = dataToUbyte(data);
  monitor_.findMonitorPoint("swVersion")->
    writeReg(isSim, softwareTestVersion_, &range);
  
  char str[12];
  sprintf(str, "%d.%d.%d\0", softwareMajorVersion_, softwareMinorVersion_, softwareTestVersion_);
  monitor_.findMonitorPoint("swVersionStr")->writeReg(isSim, (unsigned char*)str);

#endif
}

/**.......................................................................
 * Process the third slow monitor packet.
 */
void CanDevice::processSlowMonitor3(vector<byteType>& data, bool isSim)
{
  communicationErrorCount_ = dataToUshort(data);
  timeErrorCount_          = dataToUshort(data);
  softwareErrorCount_      = dataToUshort(data);
  hardwareErrorCount_      = dataToUshort(data);

  monitor_.findMonitorPoint("commErrCnt")->writeReg(isSim, communicationErrorCount_);
  monitor_.findMonitorPoint("timeErrCnt")->writeReg(isSim, timeErrorCount_);
  monitor_.findMonitorPoint("swErrCnt")->writeReg(isSim, softwareErrorCount_);
  monitor_.findMonitorPoint("hwErrCnt")->writeReg(isSim, hardwareErrorCount_);
}

/**.......................................................................
 * Process the fourth slow monitor packet.
 */
void CanDevice::processSlowMonitor4(vector<byteType>& data, bool isSim)
{
#ifdef CARMA_MODULES
  short jitter               = dataToShort(data);
  monitor_.findMonitorPoint("timeJitter")->writeReg(isSim, jitter);

  unsigned short sinceLastTs = dataToUshort(data);
  monitor_.findMonitorPoint("sinceLastTs")->writeReg(isSim, sinceLastTs);

  short tsDelta              = dataToShort(data);
  monitor_.findMonitorPoint("tsDelta")->writeReg(isSim, tsDelta);

  unsigned char apiVer       = dataToUbyte(data);
  monitor_.findMonitorPoint("apiVer")->writeReg(isSim, apiVer);
#else
  timeOffset_        = dataToShort(data);
  timeStampInterval_ = dataToShort(data);
  timeStampDelta_    = dataToShort(data);

  monitor_.findMonitorPoint("timeOffset")->writeReg(isSim, timeOffset_);
  monitor_.findMonitorPoint("timeStampInt")->writeReg(isSim, timeStampInterval_);
  monitor_.findMonitorPoint("timeStampDelta")->writeReg(isSim, timeStampDelta_);
#endif
}

/**.......................................................................
 * Process the fifth slow monitor packet.
 */
void CanDevice::processSlowMonitor5(vector<byteType>& data, bool isSim)
{
  unsigned int uptime = dataToUlong(data);
  monitor_.findMonitorPoint("uptime")->writeReg(isSim, uptime);

  unsigned char bootLoader = dataToUbyte(data);
  monitor_.findMonitorPoint("bootLoader")->writeReg(isSim, bootLoader);

  // Convert from build time in minutes since 2005 to sensible date + time

  unsigned int buildTimeInMinutes;
  unsigned char* cptr = (unsigned char*)&buildTimeInMinutes;

  *(cptr+3) = 0x0;
  *(cptr+2) = dataToUbyte(data);
  *(cptr+1) = dataToUbyte(data);
  *(cptr)   = dataToUbyte(data);

  // Base MJD is 01 Jan 2005

  double mjd0 = sza::util::Date::calToMjd(1, 1, 2005);

  // Add on days since base MJD

  double mjd = mjd0 + ((double)buildTimeInMinutes)/(24*60);

  // And convert to time + date strings

  std::string buildDate = sza::util::Date::mjdToBuildDate(mjd);
  std::string buildTime = sza::util::Date::mjdToBuildTime(mjd);

  // Finally write to the registers

  monitor_.findMonitorPoint("buildDate")->writeReg(isSim, (unsigned char*)buildDate.c_str());
  monitor_.findMonitorPoint("buildTime")->writeReg(isSim, (unsigned char*)buildTime.c_str());
}

//-----------------------------------------------------------------------
// Engineering monitor methods.
//-----------------------------------------------------------------------

/**.......................................................................
 * Return a map of devices engineering monitor points (same for all CAN
 * devices).
 */
std::map<carma::canbus::msgType, std::string> 
CanDevice::getEngineeringMonitors() const 
{
  map<msgType, string> tmp; 
  
  tmp[ENG_MONITOR_1] = "CanDevice::ENG_MONITOR_1";
  tmp[ENG_MONITOR_2] = "CanDevice::ENG_MONITOR_2";
  tmp[ENG_MONITOR_3] = "CanDevice::ENG_MONITOR_3";
  tmp[ENG_MONITOR_4] = "CanDevice::ENG_MONITOR_4";
  
  return tmp; 
}

/**.......................................................................
 * Return true if this is a engineering monitor packet.
 */
bool CanDevice::isEngineeringMonitor(msgType mid)
{
  std::map<carma::canbus::msgType, std::string> monitors = getEngineeringMonitors();
  std::map<carma::canbus::msgType, std::string>::iterator slot;
  
  slot = monitors.find(mid);
  
  if(slot != monitors.end())
    return true;
  
  return false;
}

/**.......................................................................
 * Process an engineering monitor packet.
 */
void CanDevice::processEngineeringMonitor(carma::canbus::msgType mid, 
					  vector<byteType>& data,
					  bool isSim)
{
  switch(mid) {
  case ENG_MONITOR_1:
    processEngineeringMonitor1(data, isSim);
    break;
  case ENG_MONITOR_2:
    processEngineeringMonitor2(data, isSim);
    break;
  case ENG_MONITOR_3:
    processEngineeringMonitor3(data, isSim);
    break;
  case ENG_MONITOR_4:
    processEngineeringMonitor4(data, isSim);
    break;
  default:
    {
      LogStream errStr;
      errStr.initMessage(true);
      errStr << "Unknown message type: " 
	     << mid << endl;
      throw Error(errStr);
    }
    break;
  }
}

/**.......................................................................
 * Process the first error record.
 */
void CanDevice::processEngineeringMonitor1(vector<byteType>& data, bool isSim)
{
  errLog_  = dataToUbyte(data);
  
  skipByte(data, 1);
  
  mjd_     = dataToUshort(data);
  mjdTime_ = dataToUlong(data);
}

/**.......................................................................
 * Process the second error record.
 */
void CanDevice::processEngineeringMonitor2(vector<byteType>& data, bool isSim)
{
  errCode_ = dataToUshort(data);
  errCnt_  = dataToUshort(data);
  errData_ = dataToUlong(data);
}

/**.......................................................................
 * Process a response to an ID request.
 */
void CanDevice::processEngineeringMonitor3(vector<byteType>& data, bool isSim)
{
  moduleType_ = dataToUshort(data);
  serialNo_   = dataToUshort(data);
  
  //  monitor_.findMonitorPoint("apiNo")->writeReg(isSim, dataToUshort(data));
  //  monitor_.findMonitorPoint("dongleId")->writeReg(isSim, dataToUshort(data));
}

/**.......................................................................
 * Process a character string received from the CAN node.
 */
void CanDevice::processEngineeringMonitor4(vector<byteType>& data, bool isSim)
{
  for(unsigned short ichar=0; ichar < CAN_MAX_CHAR; ichar++)
    asciiString_[ichar] = dataToUbyte(data);
}

//-----------------------------------------------------------------------
// Status message methods
//-----------------------------------------------------------------------

/**.......................................................................
 * Return a map of devices status messages
 */
std::map<carma::canbus::msgType, std::string> 
CanDevice::getStatusMessages() const 
{
  map<msgType, string> tmp; 
  return tmp; 
}

/**.......................................................................
 * Return true if this is a slow monitor packet.
 */
bool CanDevice::isStatusMessage(msgType mid)
{
  std::map<carma::canbus::msgType, std::string> messages = getStatusMessages();
  std::map<carma::canbus::msgType, std::string>::iterator slot;
  
  slot = messages.find(mid);
  
  if(slot != messages.end())
    return true;
  
  return false;
}

/**.......................................................................
 * Return a standard id corresponding to this status frame id
 */
unsigned CanDevice::idOfStatusMessageMsgType(msgType mid)
{
  std::map<carma::canbus::msgType, std::string> messages = getStatusMessages();
  std::map<carma::canbus::msgType, std::string>::iterator slot;
  
  if(!isStatusMessage(mid)) {
    LogStream errStr;
    errStr.initMessage(true);
    errStr << "Unrecognized status message id: " << mid << endl;
    throw Error(errStr);
  }
  
  unsigned islot=1;
  for(slot=messages.begin(); slot != messages.end(); slot++, islot++)
    if(slot->first == mid) 
      return islot;
}

/**.......................................................................
 * Process a status message
 */
void CanDevice::processStatusMessage(msgType mid, bool isSim) 
{
  DBPRINT(true, DEBUG_CAN, "Inside");
  
  switch(idOfStatusMessageMsgType(mid)) {
  case 1:
    processStatusMessage1(isSim);
    break;
  case 2:
    processStatusMessage2(isSim);
    break;
  case 3:
    processStatusMessage3(isSim);
    break;
  default:
    processGenericStatusMessage(isSim);
    break;
  }
  
  // Call any handler which may have been installed for this message
  
  bool found;
  do {
    
    found = false;
    for(std::vector<HandlerInfo>::iterator handler=handlers_.begin();
	handler != handlers_.end(); handler++) {
      
      // If the message id matches, call this handler, then remove it
      // from the vector
      
      if(handler->mid_ == mid) {
	handler->handler_(handler->arg1_, handler->arg2_);
	handlers_.erase(handler);
	found = true;
	break;
      }
    }    
  } while(found);
  
};

/**
 * Stub out methods for handling status messages
 */
void CanDevice::processStatusMessage1(bool isSim)
{
  return processGenericStatusMessage(isSim);
}

void CanDevice::processStatusMessage2(bool isSim)
{
  return processGenericStatusMessage(isSim);
}

void CanDevice::processStatusMessage3(bool isSim)
{
  return processGenericStatusMessage(isSim);
}

void CanDevice::processGenericStatusMessage(bool isSim) {};

//-----------------------------------------------------------------------
// Common Engineering commands.
//-----------------------------------------------------------------------

/**.......................................................................
 * Retrieve a numbered error log entry.
 */
void CanDevice::retrieveErrorLogEntry(unsigned short recordNo)
{
  if(recordNo > CAN_MAX_ERROR_LOG_ENTRY)
    throw Error("CanDevice::retrieveErrorLogEntry: "
		" Requested record number is too large.\n");
  
  vector<byteType> data;
  
  uShortToData(data, recordNo);
  
  postMessage(ENGCMD_ERROR_LOG, data);
}

/**.......................................................................
 * Clear the error log.
 */
void CanDevice::clearErrorLog()
{
  vector<byteType> data;
  
  data.push_back(0xE1);
  data.push_back(0xC3);
  data.push_back(0xA5);
  data.push_back(0x96);
  data.push_back(0x69);
  data.push_back(0x5A);
  data.push_back(0x3C);
  data.push_back(0x1E);
  
  postMessage(ENGCMD_ERROR_LOG, data);
}

/**.......................................................................
 * Request the module ID.
 */
void CanDevice::requestId()
{
  postMessage(ENGCMD_ID_REQUEST);
}

/**.......................................................................
 * Start downloading a new program.
 */
void CanDevice::startDownLoad()
{
  vector<byteType> data;
  
  data.push_back(0x1E);
  data.push_back(0xAA);
  data.push_back(0x69);
  data.push_back(0xC3);
  data.push_back(0x3C);
  data.push_back(0x96);
  data.push_back(0x55);
  data.push_back(0xE1);
  
  postMessage(ENGCMD_START_DOWNLOAD, data);
}

/**.......................................................................
 * Enable/Disable monitor packets.
 */
void CanDevice::enableMonitorPackets(bool timeStampEnable, 
				     bool blankingFrameEnable,
				     bool slowMonitorEnable)
{
  vector<byteType> data;
  
  DBPRINT(true, Debug::DEBUG11, "Inside enableMonitorPackets");
  
  data.push_back(0xE1);
  data.push_back(0x1E);
  data.push_back(0xA5);
  data.push_back(0x5A);
  data.push_back(0xC3);
  
  uByteToData(data, timeStampEnable);
  uByteToData(data, blankingFrameEnable);
  uByteToData(data, slowMonitorEnable);
  
  postMessage(ENGCMD_MONITOR_ENABLE, data);
  
  DBPRINT(true, Debug::DEBUG11, "Leaving enableMonitorPackets");
}

/**.......................................................................
 * Send a timestamp
 */
void CanDevice::sendTimeStamp()
{
  sza::util::TimeVal time;
  
  time.setToCurrentTime();
  
  unsigned short days = (unsigned short)time.getMjdDays();
  
  vector<byteType> data;
  
  uShortToData(data, days);
  
  postMessage(HOSTCMD_SET_TIME, data);
}

/**.......................................................................
 * Send a string of up to 8 characters to the CAN node.
 */
void CanDevice::sendAsciiString(string sendStr)
{
  vector<byteType> data;
  
  if(sendStr.size() > CAN_MAX_CHAR)
    throw Error("CanDevice::sendAsciiString: "
		" String is too long.\n");
  
  for(unsigned short ichar=0; ichar < sendStr.size(); ichar++)
    uByteToData(data, (sendStr.c_str())[ichar]);
  
  postMessage(ENGCMD_ASCII_STRING, data);
}

//------------------------------------------------------------
// Utility methods.
//------------------------------------------------------------

void CanDevice::skipByte(vector<byteType>& data, unsigned short nByte)
{
  if(nByte > data.size())
    throw Error("CanDevice::skipByte: "
		"Data vector is shorter than the requested number "
		"of bytes to skip.\n");
  
  data.erase(data.begin(), data.begin() + nByte);
}

/**.......................................................................
 * Post a message intended for this device.
 */
void CanDevice::postMessage(msgType mid, vector<byteType>& data)
{
  io_.postMessage(nodeMessage(mid, data));
}

/**.......................................................................
 * Post a message with no payload.
 */
void CanDevice::postMessage(msgType mid)
{
  io_.postMessage(nodeMessage(mid));
}

/**.......................................................................
 * Post a dummy message
 */
void CanDevice::postDummyMessage()
{
  io_.postMessage(dummyMessage());
}

/**.......................................................................
 * Post a fully constructed message
 */
void CanDevice::postMessage(carma::canbus::Message msg)
{
  io_.postMessage(msg);
}

/**.......................................................................
 * Post an engineering message intended for this device.
 */
void CanDevice::postEngMessage(msgType mid, vector<byteType>& data)
{
  io_.postMessage(nodeEngMessage(mid, data));
}

/**.......................................................................
 * Return an engineering Message container intended for a node.
 */
carma::canbus::Message 
CanDevice::nodeEngMessage(msgType mid, vector<byteType>& data)
{
  carma::canbus::Message msg(createEngId(false, mid), data, getBusId());
  
  DBPRINT(true, DEBUG_CAN, "Message Id is: " << msg.getId() << endl
	  << "Message Bus Id is: " << msg.getBusId() << endl
	  << "IO Bus Id is: " << getBusId());
  
  return msg;
}

/**.......................................................................
 * Return a Message container intended for a node.
 */
carma::canbus::Message 
CanDevice::nodeMessage(msgType mid, vector<byteType>& data)
{
  carma::canbus::Message msg(createId(false, mid), data, getBusId());
  
  DBPRINT(true, DEBUG_CAN, "Message Id is: " << msg.getId() << endl
	  << "Message Bus Id is: " << msg.getBusId() << endl
	  << "IO Bus Id is: " << getBusId());
  
  return msg;
}

/**.......................................................................
 * Return a Message container intended for a node.
 */
carma::canbus::Message 
CanDevice::dummyMessage()
{
  vector<byteType> data;
  
  carma::canbus::Message msg(createDummyId(), data, getBusId());
  
  return msg;
}

/**.......................................................................
 * Return a Message container intended for a node.
 */
carma::canbus::Message 
CanDevice::nodeMessage(msgType mid)
{
  vector<byteType> data;
  
  carma::canbus::Message msg(createId(false, mid), data, getBusId());
  
  return msg;
}

/**.......................................................................
 * Return a Message container intended for a host
 */
carma::canbus::Message 
CanDevice::hostMessage(msgType mid, vector<byteType>& data)
{
  carma::canbus::Message msg(createId(true, mid), data, getBusId());
  
  return msg;
}

/**.......................................................................
 * Return a Message container intended for a host.
 */
carma::canbus::Message 
CanDevice::hostMessage(msgType mid)
{
  vector<byteType> data;
  
  carma::canbus::Message msg(createId(true, mid), data, getBusId());
  
  return msg;
}

/**.......................................................................
 * Return an inheritor-specific prefix to use when
 * constructing monitor points
 */
std::string CanDevice::controlPrefix()
{
  return std::string("CanDevice");
}

/**.......................................................................
 * Install a handler for a given message id
 */
void CanDevice::installHandler(carma::canbus::msgType mid,
			       CAN_STATUS_MSG_HANDLER(*handler),
			       void* arg1,
			       unsigned arg2)
{
  HandlerInfo info(mid, handler, arg1, arg2);
  handlers_.push_back(info);
}

CanMonitorPoint* CanDevice::findMonitorPoint(char* name)
{
  return monitor_.findMonitorPoint(name);
}

void CanDevice::setRxId(sza::util::Rx::Id rxId)
{
  rxId_ = rxId;
}

std::string CanDevice::monthDayYearToString(unsigned int month, unsigned int day, unsigned year)
{
  std::ostringstream os;
  os << month << "/" << day << "/" << std::setw(2) << std::setfill('0') << year;
  return os.str();
}

std::vector<carma::canbus::Message>
CanDevice::sendDummyMsg(bool send)
{
  vector<carma::canbus::Message> msgs;

  if(send)
    postDummyMessage();
  else
    msgs.push_back(dummyMessage());

  return msgs;
}
