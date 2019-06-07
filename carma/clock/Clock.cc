/** @file
 * $Id: Clock.cc,v 1.35 2011/08/25 20:54:37 abeard Exp $
 *
 * Clock implementation
 *
 * @author $Author: abeard $
 * @version $Revision: 1.35 $
 * $Date: 2011/08/25 20:54:37 $
 *
 */

// Clock includes
#include "carma/clock/Clock.h"

// Carma includes
#include "carma/canbus/Utilities.h"
#include "carma/util/Trace.h"
#include "carma/util/Program.h"

// other includes
#include <sstream>
#include <unistd.h>

#include <log4cpp/Category.hh>

using namespace std;
using namespace carma::clock;

namespace {
  // some test variables for emulation

  int gPpsMode = 0;
  int gRbMode = 0;
  int gGpsSource = 0;
  long g10MHzSource = 0;
  int gDelay = 123;
  int gRegister = 0;
  int gWalshSync = 0;
  const double gMicrosPerMilli = 1000.0; // Exactly represented

  map<carma::canbus::msgType, string> gControls;
  map<carma::canbus::msgType, string> gHalfSecMonitors;

  // blanking frame half-second monitor packet ids
  const carma::canbus::msgType BLANKING_FRAME_PACKET_1 = 0x0E0;
  const carma::canbus::msgType BLANKING_FRAME_PACKET_2 = 0x0E1;
  const carma::canbus::msgType BLANKING_FRAME_PACKET_3 = 0x0E2;
  const carma::canbus::msgType BLANKING_FRAME_PACKET_4 = 0x0E3;
  const carma::canbus::msgType BLANKING_FRAME_PACKET_5 = 0x0E4;
  const carma::canbus::msgType BLANKING_FRAME_PACKET_6 = 0x0E5;
  const carma::canbus::msgType BLANKING_FRAME_PACKET_7 = 0x0E6;
  const carma::canbus::msgType BLANKING_FRAME_PACKET_8 = 0x0E7;
  const carma::canbus::msgType BLANKING_FRAME_PACKET_9 = 0x0E8;

  // control command ids
  const carma::canbus::msgType SET_PPS_MODE     = 0x080;
  const carma::canbus::msgType SET_RB_MODE      = 0x081;
  const carma::canbus::msgType SET_PRIMARY_GPS  = 0x082;
  const carma::canbus::msgType SET_10MHZ_SOURCE = 0x083;
  const carma::canbus::msgType SET_HB_DELAY     = 0x084;
  const carma::canbus::msgType RESYNC_10MHZ_GPS = 0x085;
  const carma::canbus::msgType RESET_RB         = 0x086;

  // CANbus API information
  const carma::canbus::apiType apiId_ = 240;
  const char apiVersion_ = 'F';

  typedef carma::monitor::MasterClockSubsystem MCS;

}


// constructor
Clock::Clock(carma::canbus::nodeType node,
	     carma::canbus::CanOutput &io,
	     carma::monitor::MasterClockSubsystem *masterClockSubsystem) :
  carma::canbus::devices::XacDevice(apiId_, node, io) {

  // initialize global map for controls
  gControls.insert( ::make_pair(SET_PPS_MODE,     "Clock::SET_PPS_MODE") );
  gControls.insert( ::make_pair(SET_RB_MODE,      "Clock::SET_RB_MODE") );
  gControls.insert( ::make_pair(SET_PRIMARY_GPS,  "Clock::SET_PRIMARY_GPS") );
  gControls.insert( ::make_pair(SET_10MHZ_SOURCE, "Clock::SET_10MHZ_SOURCE") );
  gControls.insert( ::make_pair(SET_HB_DELAY,     "Clock::SET_HB_DELAY") );
  gControls.insert( ::make_pair(RESYNC_10MHZ_GPS, "Clock::RESYNC_10MHZ_GPS") );
  gControls.insert( ::make_pair(RESET_RB,         "Clock::RESET_RB") );

  // initialize global map for half-sec monitor points
  gHalfSecMonitors.insert( ::make_pair(BLANKING_FRAME_PACKET_1, "Clock::BLANKING_FRAME_PACKET_1") );
  gHalfSecMonitors.insert( ::make_pair(BLANKING_FRAME_PACKET_2, "Clock::BLANKING_FRAME_PACKET_2") );
  gHalfSecMonitors.insert( ::make_pair(BLANKING_FRAME_PACKET_3, "Clock::BLANKING_FRAME_PACKET_3") );
  gHalfSecMonitors.insert( ::make_pair(BLANKING_FRAME_PACKET_4, "Clock::BLANKING_FRAME_PACKET_4") );
  gHalfSecMonitors.insert( ::make_pair(BLANKING_FRAME_PACKET_5, "Clock::BLANKING_FRAME_PACKET_5") );
  gHalfSecMonitors.insert( ::make_pair(BLANKING_FRAME_PACKET_6, "Clock::BLANKING_FRAME_PACKET_6") );
  gHalfSecMonitors.insert( ::make_pair(BLANKING_FRAME_PACKET_7, "Clock::BLANKING_FRAME_PACKET_7") );
  gHalfSecMonitors.insert( ::make_pair(BLANKING_FRAME_PACKET_8, "Clock::BLANKING_FRAME_PACKET_8") );
  gHalfSecMonitors.insert( ::make_pair(BLANKING_FRAME_PACKET_9, "Clock::BLANKING_FRAME_PACKET_9") );

  if (node != 0) {
    mon_ = &(masterClockSubsystem->clock());
  } else {
    mon_ = 0;
  }
}

// destructor
Clock::~Clock() {}

carma::canbus::apiType
Clock::getApiId() { return apiId_; }

// get map of device controls
map<carma::canbus::msgType, string>
Clock::getControls() const {
  return gControls;
}

// get map of half-second monitor points
map<carma::canbus::msgType, string>
Clock::getHalfSecMonitors() const {
  return gHalfSecMonitors;
}

// get map of system monitor points
map<carma::canbus::msgType, string>
Clock::getSlowMonitors() const {
  return carma::canbus::devices::XacDevice::getSlowMonitors();
}

// simulate CAN message
carma::canbus::Message
Clock::simulateMsg(carma::canbus::msgType messageId) {
  carma::canbus::Message msg;
  switch(messageId) {

    /*** halfSecMonitors ***/
  case BLANKING_FRAME_PACKET_1:
    msg = simulateBlankingFramePacket1();
    break;
  case BLANKING_FRAME_PACKET_2:
    msg = simulateBlankingFramePacket2();
    break;
  case BLANKING_FRAME_PACKET_3:
    msg = simulateBlankingFramePacket3();
    break;
  case BLANKING_FRAME_PACKET_4:
    msg = simulateBlankingFramePacket4();
    break;
  case BLANKING_FRAME_PACKET_5:
    msg = simulateBlankingFramePacket5();
    break;
  case BLANKING_FRAME_PACKET_6:
    msg = simulateBlankingFramePacket6();
    break;
  case BLANKING_FRAME_PACKET_7:
    msg = simulateBlankingFramePacket7();
    break;
  case BLANKING_FRAME_PACKET_8:
    msg = simulateBlankingFramePacket8();
    break;
  case BLANKING_FRAME_PACKET_9:
    msg = simulateBlankingFramePacket9();
    break;
  case carma::canbus::devices::XacDevice::SYSTEM_MONITOR_PACKET_1:
    msg = carma::canbus::devices::XacDevice::simSystemMonitorPacket1();
    break;
  case carma::canbus::devices::XacDevice::SYSTEM_MONITOR_PACKET_2:
    msg = carma::canbus::devices::XacDevice::simSystemMonitorPacket2();
    break;
  case carma::canbus::devices::XacDevice::SYSTEM_MONITOR_PACKET_3:
    msg = carma::canbus::devices::XacDevice::simSystemMonitorPacket3();
    break;
  case carma::canbus::devices::XacDevice::SYSTEM_MONITOR_PACKET_4:
    msg = carma::canbus::devices::XacDevice::simSystemMonitorPacket4();
    break;
  case carma::canbus::devices::XacDevice::SYSTEM_MONITOR_PACKET_5:
    msg = carma::canbus::devices::XacDevice::simSystemMonitorPacket5();
    break;
  default:
    CARMA_CPTRACE(carma::util::Trace::TRACE1,
	    "Clock::simulateMsg() - Unknown message type: " << messageId);
  }
  return msg;
}

carma::canbus::Message Clock::simulateBlankingFramePacket1() {
  carma::canbus::Message msg;
  vector<carma::canbus::byteType> data;
  static long ntp1pps = 21000;
  static long tenMHz1pps = 6;

  msg.setId(carma::canbus::createId(true, getApi(), getNode(),
				    BLANKING_FRAME_PACKET_1));
  msg.setBusId(getBusId());
  carma::canbus::sLongToData(data, ntp1pps++ ); // NTP - 1pps
  carma::canbus::sLongToData(data, (tenMHz1pps++) % 11); // external 10MHz - 1pps
  msg.setData(data);
  return msg;
}

carma::canbus::Message Clock::simulateBlankingFramePacket2() {
  carma::canbus::Message msg;
  vector<carma::canbus::byteType> data;
  static long gps1pps = 3;

  msg.setId(carma::canbus::createId(true, getApi(), getNode(),
				    BLANKING_FRAME_PACKET_2));
  msg.setBusId(getBusId());
  carma::canbus::sLongToData(data, (gps1pps+=279) % 1000); // primary GPS - 1pps
  carma::canbus::sLongToData(data, 350);                 // secondary GPS - 1pps
  msg.setData(data);
  return msg;
}

carma::canbus::Message Clock::simulateBlankingFramePacket3() {
  carma::canbus::Message msg;
  vector<carma::canbus::byteType> data;

  msg.setId(carma::canbus::createId(true, getApi(), getNode(),
				    BLANKING_FRAME_PACKET_3));
  msg.setBusId(getBusId());
  carma::canbus::uByteToData(data, gGpsSource);   // primary GPS source
  carma::canbus::uByteToData(data, g10MHzSource);   // 10 MHz source
  carma::canbus::uByteToData(data, gPpsMode);   // 1 pps mode
  msg.setData(data);
  return msg;
}

carma::canbus::Message Clock::simulateBlankingFramePacket4() {
  carma::canbus::Message msg;
  vector<carma::canbus::byteType> data;
  static unsigned short hbDuration = 10;

  msg.setId(carma::canbus::createId(true, getApi(), getNode(),
				    BLANKING_FRAME_PACKET_4));
  msg.setBusId(getBusId());
  carma::canbus::uShortToData(data, (hbDuration+=3) % 5);   // duration of last heartbeat
  carma::canbus::uByteToData(data, 1);    // status of heartbeat generator
  carma::canbus::uByteToData(data, gWalshSync);    // walsh state timing synchronization
  msg.setData(data);
  return msg;
}

carma::canbus::Message Clock::simulateBlankingFramePacket5() {
  carma::canbus::Message msg;
  vector<carma::canbus::byteType> data;

  static short canTemp = 20;
  static unsigned short delayA = 1;
  static unsigned short delayB = 4;

  if ( gRegister == REGISTER1 ) {
    delayA = gDelay;
  } else if ( gRegister == REGISTER2 ) {
    delayB = gDelay;
  }

  msg.setId(carma::canbus::createId(true, getApi(), getNode(),
				    BLANKING_FRAME_PACKET_5));
  msg.setBusId(getBusId());
  carma::canbus::sShortToData(data, (canTemp+=3) % 500); // temp of CAN module
  carma::canbus::uShortToData(data, delayA);  // 1pps A Delay
  carma::canbus::uShortToData(data, delayB); // 1pps B Delay
  msg.setData(data);
  return msg;
}

carma::canbus::Message Clock::simulateBlankingFramePacket6() {
  carma::canbus::Message msg;
  vector<carma::canbus::byteType> data;

  static unsigned short rbTemp = 60;

  msg.setId(carma::canbus::createId(true, getApi(), getNode(),
				    BLANKING_FRAME_PACKET_6));
  msg.setBusId(getBusId());
  carma::canbus::uByteToData(data, 0);   // Rb lock state
  carma::canbus::uByteToData(data, gRbMode);   // Rb Mode
  carma::canbus::uByteToData(data, 1);   // Rb status
  carma::canbus::uByteToData(data, 2);   // Rb error code
  carma::canbus::sShortToData(data, (rbTemp+=3) % 300); // temp of OXCO
  carma::canbus::sShortToData(data, (rbTemp+=4) % 300); // temp of Physics
  msg.setData(data);
  return msg;
}

carma::canbus::Message Clock::simulateBlankingFramePacket7() {
  carma::canbus::Message msg;
  vector<carma::canbus::byteType> data;

  msg.setId(carma::canbus::createId(true, getApi(), getNode(),
				    BLANKING_FRAME_PACKET_7));
  msg.setBusId(getBusId());
  carma::canbus::sShortToData(data, 24000);  // 24V power supply voltage
  carma::canbus::sShortToData(data, 3300);   // 3.3V power supply voltage
  carma::canbus::sShortToData(data, 5000);   // +5V analog power supply voltage
  carma::canbus::sShortToData(data, -5000);  // -5V analog power supply voltage
  msg.setData(data);
  return msg;
}

carma::canbus::Message Clock::simulateBlankingFramePacket8() {
  carma::canbus::Message msg;
  vector<carma::canbus::byteType> data;

  msg.setId(carma::canbus::createId(true, getApi(), getNode(),
				    BLANKING_FRAME_PACKET_8));
  msg.setBusId(getBusId());
  carma::canbus::sShortToData(data, 5000);   // 5V digital power supply A voltage
  carma::canbus::sShortToData(data, 5000);   // 5V digital power supply B voltage
  carma::canbus::uLongToData( data, 5000);
  msg.setData(data);
  return msg;
}

carma::canbus::Message Clock::simulateBlankingFramePacket9() {
  carma::canbus::Message msg;
  vector<carma::canbus::byteType> data;

  msg.setId(carma::canbus::createId(true, getApi(), getNode(),
				    BLANKING_FRAME_PACKET_9));
  msg.setBusId(getBusId());
  carma::canbus::uByteToData(data, 0); // power flags
  carma::canbus::uByteToData(data, 1); // RF flags
  carma::canbus::uByteToData(data, 2); // temp flags
  carma::canbus::uByteToData(data, 3); // freq lock flags
  carma::canbus::uByteToData(data, 4); // freq to gps flags
  carma::canbus::uByteToData(data, 5); // system flags
  carma::canbus::uByteToData(data, 1); // major version 1
  carma::canbus::uByteToData(data, 1); // minor version 1
  msg.setData(data);
  return msg;
}

// process CAN message
void Clock::processMsg(carma::canbus::msgType messageId,
		       vector<carma::canbus::byteType>& data,
		       bool sim) {

  // if state is ONLINE, check if packet is late
  if (getState() == carma::canbus::ONLINE) {
    if (isPacketLate(200.0)) {
      incrementLatePacketCount();
    }
  }

  switch(messageId) {

    /*** halfSecMonitors ***/
  case BLANKING_FRAME_PACKET_1:
    processBlankingFramePacket1(data);
    break;
  case BLANKING_FRAME_PACKET_2:
    processBlankingFramePacket2(data);
    break;
  case BLANKING_FRAME_PACKET_3:
    processBlankingFramePacket3(data);
    break;
  case BLANKING_FRAME_PACKET_4:
    processBlankingFramePacket4(data);
    break;
  case BLANKING_FRAME_PACKET_5:
    processBlankingFramePacket5(data);
    break;
  case BLANKING_FRAME_PACKET_6:
    processBlankingFramePacket6(data);
    break;
  case BLANKING_FRAME_PACKET_7:
    processBlankingFramePacket7(data);
    break;
  case BLANKING_FRAME_PACKET_8:
    processBlankingFramePacket8(data);
    break;
  case BLANKING_FRAME_PACKET_9:
    processBlankingFramePacket9(data);
    break;
  case carma::canbus::devices::XacDevice::SYSTEM_MONITOR_PACKET_1:
    carma::canbus::devices::XacDevice::processSystemMonitorPacket1(data,
									mon_->xac());
    break;
  case carma::canbus::devices::XacDevice::SYSTEM_MONITOR_PACKET_2:
    carma::canbus::devices::XacDevice::processSystemMonitorPacket2(data,
									mon_->xac());
    break;
  case carma::canbus::devices::XacDevice::SYSTEM_MONITOR_PACKET_3:
    carma::canbus::devices::XacDevice::processSystemMonitorPacket3(data,
									mon_->xac());
    break;
  case carma::canbus::devices::XacDevice::SYSTEM_MONITOR_PACKET_4:
    carma::canbus::devices::XacDevice::processSystemMonitorPacket4(data,
									mon_->xac());
    break;
  case carma::canbus::devices::XacDevice::SYSTEM_MONITOR_PACKET_5:
    carma::canbus::devices::XacDevice::processSystemMonitorPacket5(data,
									mon_->xac());
    break;
  default:
    CARMA_CPTRACE(carma::util::Trace::TRACE1,
	    "Clock::processMsg() - Unknown message type.");
  }
}

/*** monitor packet processing routines ***/
void Clock::processBlankingFramePacket1(vector<carma::canbus::byteType> &data) {

  const int ntpOffsetMs = carma::canbus::dataToLong(data);
  const int extRefPpsOffset = carma::canbus::dataToLong(data);

  // put data into monitor stream
  mon_->ntpPpsOffset().setValue(ntpOffsetMs);
  // Convert to microseconds and store
  mon_->extRefPpsOffset().setValue( extRefPpsOffset / 10.0 );
}

void Clock::processBlankingFramePacket2(vector<carma::canbus::byteType> &data) {

  // separate data from CANbus message
  const int gpsOffsetA = carma::canbus::dataToLong(data);
  const int gpsOffsetB = carma::canbus::dataToLong(data);

  const double lsbsPerMicro = 10.0;

  // put data into monitor stream
  mon_->primaryGpsPpsOffset().setValue( gpsOffsetA / lsbsPerMicro );
  mon_->secondaryGpsPpsOffset().setValue( gpsOffsetB / lsbsPerMicro);
}

void Clock::processBlankingFramePacket3(vector<carma::canbus::byteType> &data) {

  const unsigned char gpsSource = carma::canbus::dataToUbyte(data);
  const unsigned char tenMHzSource = carma::canbus::dataToUbyte(data);
  const unsigned char ppsSource = carma::canbus::dataToUbyte(data);

  // put data onto monitor stream
  mon_->primaryGps().setValue(
    static_cast<MCS::PrimaryGpsMonitorPointEnum::PRIMARYGPS>(gpsSource) );
  mon_->tenMHzOutputSource().setValue(
    static_cast<MCS::TenMHzOutputSourceMonitorPointEnum::TENMHZOUTPUTSOURCE>(
        tenMHzSource ) );
  mon_->onePpsSource().setValue(
    static_cast<MCS::OnePpsSourceMonitorPointEnum::ONEPPSSOURCE>(ppsSource) );
}

void Clock::processBlankingFramePacket4(vector<carma::canbus::byteType> &data) {

  const unsigned short lastWalshPeriod = carma::canbus::dataToUshort(data);
  const unsigned char hbStatus = carma::canbus::dataToUbyte(data);
  const unsigned char counted10MhzStatus = carma::canbus::dataToUbyte(data);

  // put data onto monitor stream
  mon_->lastWalshPeriod().setValue( lastWalshPeriod / 10.0 );
  mon_->heartbeatStatus().setValue(
    static_cast<MCS::HeartbeatStatusMonitorPointEnum::HEARTBEATSTATUS>(hbStatus));
  mon_->countedTenMHzStatus().setValue(
    static_cast<MCS::CountedTenMHzStatusMonitorPointEnum::COUNTEDTENMHZSTATUS>(
        counted10MhzStatus ) );
}

void Clock::processBlankingFramePacket5(vector<carma::canbus::byteType> &data) {

  const short moduleTemp = carma::canbus::dataToShort(data);
  const unsigned short delayGpsAPs = carma::canbus::dataToUshort(data);
  const unsigned short delayGpsBPs = carma::canbus::dataToUshort(data);
  const float lsbsPerNano = 1000.0; // Exactly representable

  // place values into monitor stream
  mon_->moduleTemp().setValue(moduleTemp*0.01); // send temp in deg C
  mon_->delayGpsA().setValue( delayGpsAPs / lsbsPerNano );
  mon_->delayGpsB().setValue( delayGpsBPs / lsbsPerNano );
}

void Clock::processBlankingFramePacket6(vector<carma::canbus::byteType> &data) {

  const unsigned char rbLockStatus  = carma::canbus::dataToUbyte(data);
  const unsigned char rbMode = carma::canbus::dataToUbyte(data);
  const unsigned char rbStatus = carma::canbus::dataToUbyte(data);
  const unsigned char errorCode = carma::canbus::dataToUbyte(data);
  const short rbCellTemp    = carma::canbus::dataToShort(data);
  const short rbCaseTemp = carma::canbus::dataToShort(data);

  mon_->rbLockStatus().setValue(
    static_cast<MCS::RbLockStatusMonitorPointEnum::RBLOCKSTATUS>(rbLockStatus) );
  mon_->rbOperationalMode().setValue(
    static_cast<MCS::RbOperationalModeMonitorPointEnum::RBOPERATIONALMODE>(
        rbMode) );
  mon_->rbOscillatorStatus().setValue(
    static_cast<MCS::RbOscillatorStatusMonitorPointEnum::RBOSCILLATORSTATUS>(
        rbStatus) );
  mon_->prs10ErrorCode().setValue(
    static_cast<MCS::Prs10ErrorCodeMonitorPointEnum::PRS10ERRORCODE>(errorCode) );
  mon_->rbOxcoTemp().setValue( rbCellTemp / 100.0 ); // send in deg C
  mon_->rbPhysicsTemp().setValue( rbCaseTemp / 100.0 ); // send in deg C
}

void Clock::processBlankingFramePacket7(vector<carma::canbus::byteType> &data) {

  const short ps24V   = carma::canbus::dataToShort(data);
  const short ps3_3V  = carma::canbus::dataToShort(data);
  const short psPos5V = carma::canbus::dataToShort(data);
  const short psNeg5V = carma::canbus::dataToShort(data);

  // place values into monitor stream
  mon_->ps24V().setValue(ps24V/1000.0);
  mon_->ps3_3V().setValue(ps3_3V/1000.0);
  mon_->psPosAnalog5V().setValue(psPos5V/1000.0);
  mon_->psNegAnalog5V().setValue(psNeg5V/1000.0);
}

void Clock::processBlankingFramePacket8(vector<carma::canbus::byteType> &data) {

  const short psA5V = carma::canbus::dataToShort(data);
  const short psB5V = carma::canbus::dataToShort(data);
  const unsigned int ext10Mhz = carma::canbus::dataToUlong(data);

  mon_->psADigital5V().setValue(psA5V/1000.0);
  mon_->psBDigital5V().setValue(psB5V/1000.0);
  mon_->external10MhzCount().setValue(static_cast<double>( ext10Mhz ));
}

void Clock::processBlankingFramePacket9(vector<carma::canbus::byteType> &data) {

  const unsigned char power   = carma::canbus::dataToUbyte(data);
  const unsigned char rf      = carma::canbus::dataToUbyte(data);
  const unsigned char temp    = carma::canbus::dataToUbyte(data);
  const unsigned char freq    = carma::canbus::dataToUbyte(data);
  const unsigned char gpsFreq = carma::canbus::dataToUbyte(data);
  const unsigned char system  = carma::canbus::dataToUbyte(data);
  const unsigned char majVer  = carma::canbus::dataToUbyte(data);
  const unsigned char minVer  = carma::canbus::dataToUbyte(data);

  ostringstream fpgaVersion;
  fpgaVersion << static_cast< short >( majVer ) << "."
        << static_cast< short >( minVer );

  mon_->powerFlag().setValue( static_cast<short>(power) );
  mon_->rfFlag().setValue( static_cast<short>(rf) );
  mon_->tempFlag().setValue( static_cast<short>(temp) );
  mon_->freqFlag().setValue( static_cast<short>(freq) );
  mon_->gpsFreqFlag().setValue( static_cast<short>(gpsFreq) );
  mon_->systemFlag().setValue( static_cast<short>(system) );
  mon_->fpgaVersion().setValue( fpgaVersion.str( ) );
}

/*** control routines ***/
void Clock::setPpsMode(carma::clock::ppsModeType ppsMode)
{
  // set up logger
  log4cpp::Category &logger = carma::util::Program::getLogger();

  carma::canbus::Message message;
  vector<carma::canbus::byteType> data;

  message.setId(carma::canbus::createId(false, getApi(), getNode(),
					SET_PPS_MODE));
  message.setBusId(getBusId());

  carma::canbus::uByteToData( data, static_cast<char>(ppsMode) );
  message.setData(data);
  io_.postMessage(message);

  ostringstream os;
  os << "Clock::setPpsMode(carma::clock::ppsModeType): PPS mode set to " << ppsMode;
  logger.info(os.str());

  gPpsMode = ppsMode; // set emulate variable for debugging
}

void Clock::setRbMode(carma::clock::rbModeType rbMode)
{
  // set up logger
  log4cpp::Category &logger = carma::util::Program::getLogger();

  carma::canbus::Message message;
  vector<carma::canbus::byteType> data;

  message.setId(carma::canbus::createId(false, getApi(), getNode(),
					SET_RB_MODE));
  message.setBusId(getBusId());

  carma::canbus::uByteToData( data, static_cast<char>(rbMode) );
  message.setData(data);
  io_.postMessage(message);

  ostringstream os;
  os << "Clock::setRbMode(carma::clock::rbModeType): Rb oscillation mode set to " << rbMode;
  logger.info(os.str());

  gRbMode = rbMode; // set emulate variable for debugging
}

void Clock::resync10MHzGps(carma::clock::walshSyncType walshSync)
{
  // set up logger
  log4cpp::Category &logger = carma::util::Program::getLogger();

  CARMA_CPTRACE(carma::util::Trace::TRACE5, "10MHz resync'ed with GPS");

  carma::canbus::Message message;
  vector<carma::canbus::byteType> data;

  message.setId(carma::canbus::createId(false, getApi(), getNode(),
                                        RESYNC_10MHZ_GPS));
  message.setBusId(getBusId());

  carma::canbus::uByteToData( data, static_cast<char>(walshSync) );
  message.setData(data);
  io_.postMessage(message);
  ostringstream os;
  os << "Clock::resync10MHzGps(): 10MHz resynced with GPS";
  logger.info(os.str());

  gWalshSync = walshSync;
}

void Clock::setGpsSource(carma::clock::gpsSourceType gpsSource)
{
  // set up logger
  log4cpp::Category &logger = carma::util::Program::getLogger();

  carma::canbus::Message message;
  vector<carma::canbus::byteType> data;

  message.setId(carma::canbus::createId(false, getApi(), getNode(),
					SET_PRIMARY_GPS));
  message.setBusId(getBusId());

  carma::canbus::uByteToData( data, static_cast<char>(gpsSource) );
  message.setData(data);
  io_.postMessage(message);

  ostringstream os;
  os << "Clock::setGpsSource(carma::clock::gpsSourceType): Primary GPS set to "
     << gpsSource;
  logger.info(os.str());

  gGpsSource = gpsSource; // set emulation variable for debugging
}

void Clock::set10MHzSource(carma::clock::tenMHzSourceType tenMHzSource)
{
  // set up logger
  log4cpp::Category &logger = carma::util::Program::getLogger();

  carma::canbus::Message message;
  vector<carma::canbus::byteType> data;

  message.setId(carma::canbus::createId(false, getApi(), getNode(),
					SET_10MHZ_SOURCE));
  message.setBusId(getBusId());

  carma::canbus::uByteToData( data, static_cast<char>(tenMHzSource) );
  message.setData(data);
  io_.postMessage(message);

  ostringstream os;
  os << "Clock::set10MHzsource(carma::clock::tenMHzSourceType): "
     << "10 MHz source set to " << tenMHzSource;
  logger.info(os.str());

  g10MHzSource = tenMHzSource; // set emulation variable for debugging
}

void Clock::setHbDelay(CORBA::UShort delay, carma::clock::delayRegister reg)
{
  // set up logger
  log4cpp::Category &logger = carma::util::Program::getLogger();

  carma::canbus::Message message;
  vector<carma::canbus::byteType> data;

  message.setId( carma::canbus::createId(false, getApi(),
					 getNode(), SET_HB_DELAY) );
  message.setBusId( getBusId() );

  carma::canbus::uShortToData(data, delay);
  carma::canbus::uByteToData(data, reg);

  message.setData(data);
  io_.postMessage(message);

  ostringstream os;
  os << "Clock::setHbDelay(): delay set to " << delay
     << " on register " << reg;
  logger.info(os.str());

  gDelay = delay;  // set emulation variable for debugging
  gRegister = reg; // set emulation variable for debugging
}

void Clock::resetRb()
{
  // set up logger
  log4cpp::Category &logger = carma::util::Program::getLogger();

  carma::canbus::Message message;
  vector<carma::canbus::byteType> data;

  message.setId( carma::canbus::createId(false, getApi(), getNode(), RESET_RB) );
  message.setBusId( getBusId() );

  // API 240, Version I gives a very specific byte sequence
  carma::canbus::uByteToData(data, 0x1E);
  carma::canbus::uByteToData(data, 0xE1);
  carma::canbus::uByteToData(data, 0x5A);
  carma::canbus::uByteToData(data, 0xA5);
  carma::canbus::uByteToData(data, 0x3C);
  carma::canbus::uByteToData(data, 0xC3);
  carma::canbus::uByteToData(data, 0x69);
  carma::canbus::uByteToData(data, 0x96);
  message.setData(data);
  io_.postMessage(message);

  ostringstream os;
  os << "Clock::resetRb() called";
  logger.info(os.str());
}

void Clock::initialize(carma::clock::ppsModeType ppsMode,
		       carma::clock::rbModeType rbMode,
		       carma::clock::gpsSourceType gpsSource,
		       carma::clock::tenMHzSourceType tenMHzSource,
		       carma::clock::walshSyncType walshSync)
{
  // set up logger
  log4cpp::Category &logger = carma::util::Program::getLogger();

  ostringstream os;
  os << "Clock::initialize(): Initializing Clock";
  logger.info(os.str());

  setPpsMode(ppsMode);
  sleep(1);
  setRbMode(rbMode);
  sleep(1);
  setGpsSource(gpsSource);
  sleep(1);
  set10MHzSource(tenMHzSource);
  sleep(1);
  resync10MHzGps(walshSync);
}

void Clock::updateFrameData( )
{
  mon_->state().setValue(
    static_cast< carma::monitor::StateMonitorPointEnum::STATE>( getState( ) ) );
}
