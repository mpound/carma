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
 * Describe the register map of the SZA Varactor-tuned Gunn
 * oscillator module
 */
static RegBlockTemp szaVarGunn[] = {

  // Include registers common to all can modules

  CAN_COMMON_REGS

  RegBlockTemp("Lock status:<br/><ul>"
	       "<li>0 -- unlocked</li>"		       
	       "<li>1 -- locked</li></ul>",
	       "lockStatus",            REG_UCHAR|REG_UNION,   0, 1, 0, 0, "", "0", "0"),

  RegBlockTemp("Ref status:<br/><ul>"
	       "<li>0 -- reference low</li>"		       
	       "<li>1 -- reference OK</li></ul>",
	       "refStatus",             REG_UCHAR,             0, 1, 0, 0, "", "0", "0"),

  RegBlockTemp("Sweep status:<br/><ul>"
	       "<li>0 -- off</li>"		       
	       "<li>1 -- on</li></ul>",   
	       "sweepStatus",           REG_UCHAR,             0, 1, 0, 0, "", "0", "0"),

  RegBlockTemp("Gunn status:<br/><ul>"
	       "<li>0 -- off</li>"		       
	       "<li>1 -- on</li></ul>",   
	       "gunnStatus",            REG_UCHAR,             0, 1, 0, 0, "", "0", "0"),

  RegBlockTemp("Flags if the IF monitor port is turned off:<br/><ul>"
	       "<li>0 -- possible data corruption</li>"		       
	       "<li>1 -- system OK</li></ul>",
	       "ifMonStatus",           REG_UCHAR,             0, 1, 0, 0, "", "0", "0"),

  RegBlockTemp("True (1) if the module state was good during the last half-second period:<ul>"
	       "<li>0 -- possible data corruption</li>"		       
	       "<li>1 -- system OK</li></ul>",       
	       "dataValid",             REG_UCHAR,             0, 1, 0, 0, "", "0", "0"),

  RegBlockTemp("Validity bitmask:<br/><ul>"  
	       "<li>Bit 0 high -- ifMonStatus bad</li>"		       
	       "<li>Bit 1 high -- ifMonStatus good</li>"		       
	       "<li>Bit 2 high -- data invalid</li>"
	       "<li>Bit 3 high -- data valid</li></ul>",
	       "validityMask",          REG_UCHAR|REG_UNION,   0, 1),

  RegBlockTemp("", "powSupPos24V",    REG_SHORT|REG_PREAVG, 0, 1),
  RegBlockTemp("", "powSupPosDig5V",  REG_SHORT|REG_PREAVG, 0, 1),
  RegBlockTemp("", "powSupPosDig15V", REG_SHORT|REG_PREAVG, 0, 1),
  RegBlockTemp("", "powSupPos12V",    REG_SHORT|REG_PREAVG, 0, 1),
  RegBlockTemp("", "powSupPos6V",     REG_SHORT|REG_PREAVG, 0, 1),

  RegBlockTemp("", "powSupNeg15V",    REG_SHORT|REG_PREAVG, 0, 1),
  RegBlockTemp("", "powSupPos5V",     REG_SHORT|REG_PREAVG, 0, 1),
  RegBlockTemp("", "powSupPos9V",     REG_SHORT|REG_PREAVG, 0, 1),

  // Registers specific to the varactor-tuned gunn

  RegBlockTemp("Status register bitmask returned by the module:<br/><ul>"
	       "<li>Bit 0 low|high -- unlocked|locked</li>"		       
	       "<li>Bit 1 low|high -- RF bad|good</li>"	       
	       "<li>Bit 2 low|high -- Sweep off|on</li>"		       
	       "<li>Bit 3 low|high -- Gunn off|on</li></ul>",
	       "statusRegister" ,         REG_UCHAR|REG_UNION,   0, 1, 0, 0),

  RegBlockTemp("", "loopGainResistance",      REG_USHORT|REG_PREAVG, 0, 1),
  RegBlockTemp("", "boardTemperature",        REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "noiseMeterVoltage",       REG_SHORT|REG_PREAVG,  0, 1),
  
  RegBlockTemp("", "ifLevel",                 REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "errorVoltage",            REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "biasCurrent",             REG_SHORT|REG_PREAVG, 0, 1),
  RegBlockTemp("", "maxChnl",                 REG_SHORT|REG_PREAVG,  0, sza::util::Oscillator::MAX_CHNL),

  RegBlockTemp("Status register bitmask:<br/><ul>"  
		"<li>Bit 0 high -- unlocked</li>"		       
		"<li>Bit 1 high -- locked</li>"	       
		"<li>Bit 2 high -- RF bad</li>"		       
		"<li>Bit 3 high -- RF good</li>"             
		"<li>Bit 4 high -- Sweep off</li>"		       
		"<li>Bit 5 high -- Sweep on</li>"	       
		"<li>Bit 6 high -- Gunn off</li>"		       
                "<li>Bit 7 high -- Gunn on</li></ul>",
	           "statusRegisterMask" ,     REG_UCHAR|REG_UNION,   0, 1),

  // For monitoring only                                                        

  RegBlockTemp("(For monitoring only) True if the lock status represents an err\
or "
               "for the requested receiver.  False if not.",
               "lockStatusError",             REG_BOOL|REG_EXC,        0, 1, 0, 0, "", "", "true"),
};

RegBlockTemp* getSzaVarGunn()
{
  return szaVarGunn;
}

unsigned getSizeOfSzaVarGunn()
{
  return ARRAY_DIM(szaVarGunn);
}

/**.......................................................................
 * Describe the register map of the SZA Yig module.
 */
static RegBlockTemp szaYig[] = {

  // Include registers common to all can modules

  CAN_COMMON_REGS

  // Registers specific to the yig

  RegBlockTemp("The current Yig lock state:<br/><ul>"
	       "<li>0 -- Unlocked</li>"		       
	       "<li>1 -- Searching</li>"	       
	       "<li>2 -- Refining</li>"		       
	       "<li>3 -- Locked</li></ul>",              
	       "lockState",                   REG_UCHAR|REG_UNION,      0, 1, 0, 0, "", "0", "0"),

  RegBlockTemp("", "dataValid",               REG_UCHAR|REG_UNION,      0, 1),

  RegBlockTemp("YIG frequency (MHz)", "frequency",               REG_USHORT|REG_PREAVG,    0, 1),

  RegBlockTemp("Actually not a resistance at all, but the loop gain, in units of 0.01%", 
	       "loopGainResistance",          REG_USHORT|REG_PREAVG,    0, 1),

  RegBlockTemp("Damping resistance, in Ohms", 
	       "dampingResistance",           REG_USHORT|REG_PREAVG,    0, 1),
  
  RegBlockTemp("mV", "ifLevel",               REG_SHORT|REG_PREAVG,     0, 1),

  RegBlockTemp("mV", "errorVoltage",          REG_SHORT|REG_PREAVG,     0, 1),

  RegBlockTemp("Yig current, mA", "current",  REG_USHORT|REG_PREAVG,    0, 1),

  RegBlockTemp("Noise meter voltage, mV", 
	       "noiseMeterVoltage",           REG_SHORT|REG_PREAVG,     0, 1),
  
  RegBlockTemp("Temperature, 0.01 C", 
	       "boardTemperature",            REG_SHORT|REG_PREAVG,     0, 1),

  RegBlockTemp("YIG monitor voltages:<br/><ul>"	 
	       "<li>maxChnl[0] -- +24V power supply (mV)</li>"
	       "<li>maxChnl[1] --  +5V digital (mV)</li>"		       	
	       "<li>maxChnl[2] --  +5V analog power supply (mV)</li>"
	       "<li>maxChnl[3] --  +9V analog (mV)</li>"
	       "<li>maxChnl[4] -- +15V analog power supply (mV)</li>"
	       "<li>maxChnl[5] --  -5V analog power supply (mV)</li>"
	       "<li>maxChnl[6] -- -15V analog power supply (mV)</li></ul>",
	       "maxChnl",                     REG_SHORT|REG_PREAVG,     0, sza::util::Oscillator::MAX_CHNL),
  
  RegBlockTemp("", "id",                      REG_UCHAR,                0, 1),
  RegBlockTemp("", "calDate",                 REG_UCHAR,                0, 3),
  RegBlockTemp("", "sweepStatus",             REG_UCHAR|REG_UNION,      0, 1),
  RegBlockTemp("", "refStatus",               REG_UCHAR|REG_UNION,      0, 1),
  RegBlockTemp("", "autoRelock",              REG_UCHAR|REG_UNION,      0, 1),
  RegBlockTemp("", "relockCount",             REG_UCHAR|REG_SUM,        0, 1),

  // And extended bitmask version of lockState

  RegBlockTemp("A mask of Yig lock states:<br/><ul>"
	       "<li>Bit 0 high -- Unlocked</li>"
	       "<li>Bit 1 high -- Searching</li>"
	       "<li>Bit 2 high -- Refining</li>"
	       "<li>Bit 3 high -- Locked</li></ul>",
	       "lockStateMask",               REG_UCHAR|REG_UNION,      0, 1),

  RegBlockTemp("The current Yig lock bit:<br/><ul>"
	       "<li>0 -- Unlocked</li>"
	       "<li>1 -- Locked</li></ul>",
	       "lockBit",                     REG_UCHAR|REG_UNION,      0, 1, 0, 0, "", "0", "0"),


  // And extended bitmask version of lockBit

  RegBlockTemp("A mask of Yig lock bits:<br/><ul>"
	       "<li>Bit 0 high -- Unlocked</li>"
	       "<li>Bit 1 high -- Locked</li></ul>",
	       "lockBitMask",                 REG_UCHAR|REG_UNION,      0, 1),
};

RegBlockTemp* getSzaYig()
{
  return szaYig;
}

unsigned getSizeOfSzaYig()
{
  return ARRAY_DIM(szaYig);
}
