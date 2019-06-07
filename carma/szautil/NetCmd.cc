#include "carma/szautil/NetCmd.h"

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
NetCmd::NetCmd() {}

/**.......................................................................
 * Constructor.
 */
NetCmd::NetCmd(sza::array::RtcNetCmd rtc, sza::array::NetCmdId opcode) 
{
  rtc_    = rtc;
  opcode_ = opcode;
}

/**.......................................................................
 * Destructor.
 */
NetCmd::~NetCmd() {}

void NetCmd::packAtmosCmd(Temperature& airTemp, double humidity, 
			  double pressure, AntNum::Id antennas)
{
  opcode_ = sza::array::NET_ATMOS_CMD;

  rtc_.cmd.atmos.temperature = airTemp.K();
  rtc_.cmd.atmos.humidity    = humidity;
  rtc_.cmd.atmos.pressure    = pressure;

  // Send this to all antennas

  rtc_.antennas = (NetMask)antennas;
}

void NetCmd::packAtmosCmd(Temperature& airTemp, Percent& humidity, 
			  Pressure& pressure, AntNum::Id antennas)
{
  opcode_ = sza::array::NET_ATMOS_CMD;

  rtc_.cmd.atmos.temperature = airTemp.K();
  rtc_.cmd.atmos.humidity    = humidity.percentMax1();
  rtc_.cmd.atmos.pressure    = pressure.milliBar();

  // Send this to all antennas

  rtc_.antennas = (NetMask)antennas;
}
