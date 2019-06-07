#include "carma/szaarrayutils/szaregs.h"
#include "carma/szaarrayutils/miscregs.h"

#include "carma/szautil/AntNum.h"
#include "carma/szautil/CorrelatorBand.h"
#include "carma/szautil/Directives.h"
#include "carma/szautil/LoMonitorFlags.h"
#include "carma/szautil/LobeRotatorFlags.h"
#include "carma/szautil/Oscillator.h"
#include "carma/szautil/TimeVal.h"

#include <map>

using namespace sza::util;
using namespace sza::array;

//-----------------------------------------------------------------------
// CAN Modules
//-----------------------------------------------------------------------

/**.......................................................................
 * Describe the register map of the SZA Rx module.
 */
static RegBlockTemp szaRx[] = {

  // Include registers common to all can modules

  CAN_COMMON_REGS

  // Registers specific to the receiver module

  RegBlockTemp("", "boardTemperature",	      REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "neg15VAnalogVoltage",     REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "pos5VAnalogVoltage",      REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "neg5VDigitalVoltage",     REG_SHORT|REG_PREAVG,  0, 1),
  
  RegBlockTemp("", "pos15VAnalogVoltage",     REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "pos5VDigitalVoltage",     REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "pos28VDigitalVoltage",    REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "bias90GHz",	              REG_SHORT|REG_PREAVG,  0, 1),
  
  RegBlockTemp("", "drainCurrent30GHz",	      REG_SHORT|REG_PREAVG,  0, 4),
  RegBlockTemp("", "gateVoltage30GHz",	      REG_SHORT|REG_PREAVG,  0, 4),
  RegBlockTemp("", "gateCurrent30GHz",	      REG_SHORT|REG_PREAVG,  0, 4),
  
  RegBlockTemp("", "ifAmpVoltage30GHz",	      REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "ifAmpCurrent30GHz",	      REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "mixerCurrent30GHz",	      REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "ledCurrent30GHz",	      REG_SHORT|REG_PREAVG,  0, 1),
  
  RegBlockTemp("", "gateCurrent90GHz",	      REG_SHORT|REG_PREAVG,  0, 4),
  RegBlockTemp("", "drainCurrent90GHz",	      REG_SHORT|REG_PREAVG,  0, 2),
  RegBlockTemp("", "ifAmpDrainCurrent90GHz",  REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "ifAmpGateCurrent90GHz",   REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "tempSensor",              REG_SHORT|REG_PREAVG,  0, 3, 0, 0, "0.01 Kelvin", "1000", "2300"),

  RegBlockTemp("", "pos24VAnalogVoltage",     REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "tempRadShield",           REG_SHORT|REG_PREAVG,  0, 1, 0, 0, "0.01 Kelvin"),
  RegBlockTemp("", "tempStage2ColdHead",      REG_SHORT|REG_PREAVG,  0, 1, 0, 0, "0.01 Kelvin", "1000", "3000"),
  RegBlockTemp("", "temp90GHzIsolator",       REG_SHORT|REG_PREAVG,  0, 1, 0, 0, "0.01 Kelvin", "1000", "3000"),
  RegBlockTemp("", "temp4",                   REG_SHORT|REG_PREAVG,  0, 1, 0, 0, "0.01 Kelvin"),
  RegBlockTemp("", "drainSetVoltage30GHz",    REG_SHORT|REG_PREAVG,  0, 4),
  RegBlockTemp("", "drainSetVoltage90GHz",    REG_SHORT|REG_PREAVG,  0, 2),
};

RegBlockTemp* getSzaRx()
{
  return szaRx;
}

unsigned getSizeOfSzaRx()
{
  return ARRAY_DIM(szaRx);
}

/**.......................................................................
 * Describe the register map of the thermal control module
 */
static RegBlockTemp szaThermal[] = {   

  // Include registers common to all can modules

  CAN_COMMON_REGS

  // Registers specific to the thermal module
  
  RegBlockTemp("", "moduleTemperature",       REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "rboxTopTemperature",      REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "eboxTemperature",         REG_SHORT|REG_PREAVG,  0, 1, 0, 0, "0.01 C", "0", "4700"),
  RegBlockTemp("", "powSup24V",               REG_SHORT|REG_PREAVG,  0, 1),
  
  RegBlockTemp("", "rboxLoopState",           REG_UCHAR|REG_UNION,   0, 1),
  RegBlockTemp("", "rboxPwmFraction",         REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "rboxTemperatureError",    REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "rboxIntTemperatureError", REG_SHORT|REG_PREAVG,  0, 1),
  
  RegBlockTemp("", "rboxLoopGain",            REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "rboxIntGainConstant",     REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "rboxLoopRateConstant",    REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "rboxLoopBandwidth",       REG_SHORT|REG_PREAVG,  0, 1),
  
  RegBlockTemp("", "eboxLoopState",           REG_UCHAR|REG_UNION,   0, 1),
  RegBlockTemp("", "eboxVoltage",             REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "eboxTemperatureError",    REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "eboxIntTemperatureError", REG_SHORT|REG_PREAVG,  0, 1),
  
  RegBlockTemp("", "eboxLoopGain",            REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "eboxIntGainConstant",     REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "eboxLoopRateConstant",    REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "eboxLoopBandwidth",       REG_SHORT|REG_PREAVG,  0, 1),
  
  RegBlockTemp("", "rboxBottomTemperature",   REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "rboxSetTemperature",      REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "eboxSetTemperature",      REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "circPropConst",           REG_SHORT|REG_PREAVG,  0, 1),
  
  RegBlockTemp("", "powSupPos12V",            REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "powSupNeg12V",            REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "powSupPos5V",             REG_SHORT|REG_PREAVG,  0, 1),
  
  RegBlockTemp("", "powSupVoltage",           REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "powSupCurrent",           REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "powSupError",             REG_SHORT|REG_PREAVG,  0, 1),
  
  RegBlockTemp("", "voltageOffset",           REG_SHORT|REG_PREAVG,  0, 1),
};

RegBlockTemp* getSzaThermal()
{
  return szaThermal;
}

unsigned getSizeOfSzaThermal()
{
  return ARRAY_DIM(szaThermal);
}

/**.......................................................................
 * Describe the register map of the tiltmeter
 */
static RegBlockTemp szaTiltMeter[] = {

  // Include registers common to all can modules

  CAN_COMMON_REGS

  // Registers specific to the tiltmeter

  RegBlockTemp("Tilt of the telescope perpendicular to the elevation axis"
	       "(tilt to the right is positive).  Also known as the X axis."
	       "THE TILT ZERO IS SUBTRACTED", 
	       "lrTilt",                      REG_SHORT|REG_PREAVG,  0, 1),

  RegBlockTemp("Tilt of the telescope perpendicular to the elevation direction"
	       "(forward tilt is positive). Also know as the Y axis."
	       "THE TILT ZERO IS SUBTRACTED", 
	       "afTilt",                      REG_SHORT|REG_PREAVG,  0, 1),

  RegBlockTemp("", "boardTemperature",        REG_SHORT|REG_PREAVG,  0, 1),

  RegBlockTemp("", "tiltTemp",                REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "structTemp",              REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "heaterVoltage",           REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "heaterCurrent",           REG_SHORT|REG_PREAVG,  0, 1),

  RegBlockTemp("", "loopState",               REG_UCHAR|REG_PREAVG,  0, 1),
  RegBlockTemp("", "pwrFract",                REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "tempDiff",                REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "integDiff",               REG_SHORT|REG_PREAVG,  0, 1),
  
  RegBlockTemp("", "loopGain",                REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "loopIntegration",         REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "loopDiffGain",            REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "loopBw",                  REG_SHORT|REG_PREAVG,  0, 1),
  
  RegBlockTemp("", "pos24VPsVoltage",         REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "pos12VTiltPsVoltage",     REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "neg15VTiltPsVoltage",     REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "pos5VTiltPsVoltage",      REG_SHORT|REG_PREAVG,  0, 1),
  
  RegBlockTemp("", "pos12VThermalPsVoltage",  REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "neg12VThermalPsVoltage",  REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "pos5VThermalPsVoltage",   REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "teePeeTemp",              REG_SHORT|REG_PREAVG,  0, 1),
};

RegBlockTemp* getSzaTiltMeter()
{
  return szaTiltMeter;
}

unsigned getSizeOfSzaTiltMeter()
{
  return ARRAY_DIM(szaTiltMeter);
}
