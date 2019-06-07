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
// CAN Modules -- file 1
//-----------------------------------------------------------------------

/**.......................................................................
 * Describe the register map of the SZA Bias-tuned Gunn oscillator
 * module
 */
static RegBlockTemp szaBiasGunn[] = {

  // Include registers common to all can modules

  CAN_COMMON_REGS

  // Registers specific to the bias-tuned gunn

  RegBlockTemp("Current state of the phase lock:<br/><ul>"
	       "<li>0 -- unlocked</li>" 
	       "<li>1 -- waiting for tuner</li>" 
	       "<li>2 -- waiting for backshort</li>" 
	       "<li>3 -- waiting for attenuator</li>" 
	       "<li>4 -- searching</li>" 
	       "<li>5 -- reducing error voltage</li>" 
	       "<li>6 -- adjusting loop gain</li>" 
	       "<li>7 -- locked</li></ul>", 
	       "phaseLockState",              REG_UCHAR|REG_PREAVG,     0, 1),

  RegBlockTemp("Hardware lock indication from the PLL module:<br/><ul>"
	       "<li>0 -- unlocked</li>" 
	       "<li>1 -- locked</li></ul>",
	       "hwLockStatus",                REG_UCHAR|REG_PREAVG,     0, 1),

  RegBlockTemp("Status of the 50 MHz reference to the PLL module:<br/><ul>"
	       "<li>0 -- unlocked</li>" 
	       "<li>1 -- locked</li></ul>",
	       "refLockStatus",               REG_UCHAR|REG_PREAVG,     0, 1),

  RegBlockTemp("Status of the PLL sweep:<br/><ul>"
	       "<li>0 -- off</li>" 
	       "<li>1 -- on</li></ul>",
	       "sweepStatus",                 REG_UCHAR|REG_PREAVG,     0, 1),

  RegBlockTemp("Gunn on/off status:<br/><ul>"
	       "<li>0 -- Off</li>" 
	       "<li>1 -- On</li></ul>",
	       "gunnStatus",                  REG_UCHAR|REG_PREAVG,     0, 1),

  RegBlockTemp("Whether module state could have corrupted visibility data in te last 0.5s:<br/><ul>"
	       "<li>0 -- Data not valid</li>"
	       "<li>1 -- Consistent with valid data</li></ul>",
	       "dataValid",                   REG_UCHAR|REG_PREAVG,     0, 1),

  RegBlockTemp("Status of the auto-relock:<br/><ul>"
	       "<li>0 -- Off</li>" 
	       "<li>1 -- On</li></ul>",
	       "autoRelock",                  REG_UCHAR|REG_PREAVG,     0, 1),

  RegBlockTemp("Number of times the Gunn has trie to relock.  If a new frequency command is sent"
	       " the relock count is reset to zero.  Range: 0-9",
	       "relockCount",                 REG_UCHAR|REG_PREAVG,     0, 1),

  RegBlockTemp("Unique identifier for bias-tuned Gunn", 
	       "gunnId",                      REG_USHORT|REG_PREAVG,    0, 1),

  RegBlockTemp("Gunn operating voltage set point (units of 0.01 V)", 
	       "gunnVoltage",                 REG_USHORT|REG_PREAVG,    0, 1),

  RegBlockTemp("Multiplier from Gunn frequency to LO frequency", 
	       "multiplier",                  REG_USHORT|REG_PREAVG,    0, 1),

  RegBlockTemp("Applies to the last frequency command:<br/><ul>"
	       "<li>0 -- In range</li>" 
	       "<li>1 -- Frequency too low</li>" 
	       "<li>2 -- Frequency too high</li>" 
	       "<li>3 -- Frequency not set</li></ul>",
	       "freqRangeCheck",              REG_UCHAR|REG_PREAVG,     0, 1),

  RegBlockTemp("Status of the IF monitor port:<br/><ul>"
	       "<li>0 -- Off</li>" 
	       "<li>1 -- On</li></ul>",
	       "ifMonState",                  REG_UCHAR|REG_PREAVG,     0, 1),

  RegBlockTemp("State of the calibration table:<br/><ul>"
	       "<li>0 -- Valid</li>" 
	       "<li>1 -- Invalid</li></ul>",
	       "calTableState",               REG_UCHAR|REG_PREAVG,     0, 1),

  RegBlockTemp("Month Gunn calibration table was made.  Range: 1-12", 
	       "calMonth",                    REG_UCHAR|REG_PREAVG,     0, 1),

  RegBlockTemp("Day Gunn calibration table was made.  Range: 1-31", 
	       "calDay",                      REG_UCHAR|REG_PREAVG,     0, 1),

  RegBlockTemp("Two-digit year Gunn calibration table was made.", 
	       "calYear",                     REG_UCHAR|REG_PREAVG,     0, 1),

  RegBlockTemp("", "calDate",                 REG_UCHAR|REG_STRING,     0, 12),

  RegBlockTemp("Count of Zaber actuators detected.", 
	       "numZabers",                   REG_UCHAR|REG_PREAVG,      0, 1),

  RegBlockTemp("Reports whether or not all the Zabers expected for this module have been detected.", 
	       "allZabers",                   REG_UCHAR|REG_PREAVG,      0, 1),

  RegBlockTemp("Gunn operating frequency determined from the tuning actuator setting "
	       "(units of 0.01 GHz", 
	       "gunnFrequency",               REG_USHORT|REG_PREAVG,    0, 1),

  RegBlockTemp("PLL loop gain (units of 0.01 %)", 
	       "loopGain",                    REG_USHORT|REG_PREAVG,    0, 1),

  RegBlockTemp("Current Gunn tuner position (units of micro-steps)", 
	       "tunerPosition",               REG_UINT|REG_PREAVG,      0, 1),

  RegBlockTemp("Current Gunn backshort position (units of micro-steps)", 
	       "backShortPosition",           REG_UINT|REG_PREAVG,      0, 1),

  RegBlockTemp("Current Gunn attenuator position (units of micro-steps)", 
	       "attenuatorPosition",          REG_UINT|REG_PREAVG,      0, 1),

  RegBlockTemp("IF level voltage (units of mV)", 
	       "ifLevel",                     REG_SHORT|REG_PREAVG,     0, 1),

  RegBlockTemp("Placeholder for this module, which doesn't have the same monitoring structure "
	       "as the other oscillator modules", 
	       "maxChnl",                     REG_SHORT|REG_PREAVG,     0, sza::util::Oscillator::MAX_CHNL),

  RegBlockTemp("Error voltage (units of mV)", 
	       "errorVoltage",                REG_SHORT|REG_PREAVG,     0, 1),

  RegBlockTemp("Gunn current (units of mA)", 
	       "gunnCurrent",                 REG_USHORT|REG_PREAVG,    0, 1),

  RegBlockTemp("Noise meter voltage (units of mV)", 
	       "noiseMeterVoltage",           REG_SHORT|REG_PREAVG,     0, 1),

  RegBlockTemp("Module temp (units of 0.01 C)", 
	       "boardTemperature",            REG_SHORT|REG_PREAVG,     0, 1),

  RegBlockTemp("24 V PSU voltage (units mV)", 
	       "pos24VAnalogVoltage",         REG_SHORT|REG_PREAVG,     0, 1),

  RegBlockTemp("5 V digital PSU voltage (units mV)", 
	       "pos5VDigitalVoltage",         REG_SHORT|REG_PREAVG,     0, 1),

  RegBlockTemp("15 V analog PSU voltage (units mV)", 
	       "pos15VAnalogVoltage",         REG_SHORT|REG_PREAVG,     0, 1),

  RegBlockTemp("12 V analog PSU voltage (units mV)", 
	       "pos12VAnalogVoltage",         REG_SHORT|REG_PREAVG,     0, 1),

  RegBlockTemp("5 V analog PSU voltage (units mV)", 
	       "pos5VAnalogVoltage",          REG_SHORT|REG_PREAVG,     0, 1),

  RegBlockTemp("-12 V analog PSU voltage (units mV)", 
	       "neg12VAnalogVoltage",         REG_SHORT|REG_PREAVG,     0, 1),

  RegBlockTemp("6 V analog PSU voltage (units mV)", 
	       "pos6VAnalogVoltage",          REG_SHORT|REG_PREAVG,     0, 1),

  RegBlockTemp("State of Gunn crowbar protection:<br/><ul>"
	       "<li>0 -- Normal.  Gunn operating properly</li>"
	       "<li>1 -- Crowbarred</li>"
	       "<li>2 -- Resetting</li>"
	       "<li>3 -- Disabled. Maximum number of resets reached</li></ul>",
	       "crowbarState",                REG_UCHAR|REG_UNION,      0, 1),

  RegBlockTemp("Number of times the crowbar circuit has been reset", 
	       "crowbarCount",                REG_UINT|REG_PREAVG,      0, 1),

  // For monitoring only

  RegBlockTemp("(For monitoring only) True if the lock status represents an error "
	       "for the requested receiver.  False if not.", 
	       "lockStatusError",             REG_BOOL|REG_EXC,         0, 1, 0, 0, "", "", "true"),
};

RegBlockTemp* getSzaBiasGunn()
{
  return szaBiasGunn;
}

unsigned getSizeOfSzaBiasGunn()
{
  return ARRAY_DIM(szaBiasGunn);
}

/**.......................................................................
 * Describe the register map of the SZA CalTert CAN module
 */
static RegBlockTemp szaCalTert[] = {

  CAN_COMMON_REGS

  // Registers specific to the caltert

  RegBlockTemp("The current tertiary state returned by the module:<br/><ul>"
	       "<li>0 -- Idle</li>"
	       "<li>1 -- Homing</li>"
	       "<li>2 -- Home</li>"
	       "<li>3 -- Home error</li>"
	       "<li>4 -- Moving</li>"
	       "<li>5 -- 1-cm Rx selected</li>"
	       "<li>6 -- 3-mm Rx selected</li>"
	       "<li>7 -- 1-mm Rx selected</li>"
	       "<li>8 -- Manual position</li>"
	       "<li>9 -- Stuck</li></ul>",
	       "tertState",                   REG_UCHAR|REG_UNION,      0, 1, 0, 0, "", "0", "8"),

  // Register defs used to be:
  //
  //  RegBlockTemp("The current tertiary state returned by the module:<br/><ul>"
  //	       "<li>0 -- In position</li>"
  //	       "<li>1 -- Moving</li>"
  //	       "<li>2 -- Homing</li>"
  //	       "<li>3 -- Stopped</li>"
  //	       "<li>4 -- Positive soft limit</li>"
  //	       "<li>5 -- Negative soft limit</li>"
  //	       "<li>6 -- Hard limit</li>"
  //	       "<li>7 -- Error</li></ul>",
  //	       "tertState",                   REG_UCHAR|REG_UNION,      0, 1),

  RegBlockTemp("Whether or not the Tertiary is allowed to move.", 
	       "moveMirOk",                   REG_UCHAR|REG_UNION,      0, 1),

  RegBlockTemp("Commanded position of the secondary mirror:<br/><ul>"
	       "<li>1 -- 30 GHz</li>"
	       "<li>2 -- 90 GHz</li>"
	       "<li>3 -- 230 GHz</li>"
	       "<li>4 -- Specified encoder position</li></ul>",
	       "posnCode",	              REG_UCHAR|REG_UNION,      0, 1),

  RegBlockTemp("Current 16-bit encoder position", 
	       "encPos",                      REG_USHORT|REG_PREAVG,    0, 1),

  RegBlockTemp("State of calibration mechanism:<ul>"
	       "<li>0 -- Idle</li>"
	       "<li>1 -- Moving</li>"
	       "<li>2 -- Sky</li>"
	       "<li>3 -- Ambient load</li>"
	       "<li>4 -- Wait for tertiary</li>"
	       "<li>5 -- Stuck</li></ul>",
	       "calibState",                  REG_UCHAR|REG_UNION,      0, 1, 0, 0, "", "0", "4"),

  // Register defs used to be
  // 
  //  RegBlockTemp("State of calibration mechanism:<ul>"
  //           "<li>0 -- In requested position</li>"
  //	       "<li>1 -- Moving to requested position</li>"
  //	       "<li>2 -- At hard limit</li>"
  //	       "<li>3 -- Error condition</li></ul>",
  //	       "calibState",                  REG_UCHAR|REG_UNION,      0, 1),


  RegBlockTemp("", "calibPosReq",             REG_UCHAR|REG_UNION,      0, 1),
  RegBlockTemp("", "calibMoveOk",             REG_UCHAR|REG_UNION,      0, 1),
  
  RegBlockTemp("", "inPowSup24V",             REG_SHORT|REG_PREAVG,     0, 1),
  RegBlockTemp("", "outPowSup24V",            REG_SHORT|REG_PREAVG,     0, 1),
  RegBlockTemp("", "powSup5V",                REG_SHORT|REG_PREAVG,     0, 1),

  RegBlockTemp("Ambient calibration load temperature. Units: 0.01 degrees C", 
	       "calibTemp",                   REG_SHORT|REG_PREAVG,     0, 1),
  
  RegBlockTemp("", "modTemp",                 REG_SHORT|REG_PREAVG,     0, 1),
  RegBlockTemp("", "mirStable",               REG_UCHAR|REG_UNION,      0, 1),
  RegBlockTemp("", "calibStable",             REG_UCHAR|REG_UNION,      0, 1),
  RegBlockTemp("", "calibTempStable",         REG_UCHAR|REG_UNION,      0, 1),

  RegBlockTemp("Calibration motion fault detected:<br/><ul>"
	       "<li>0 -- Ok</li>"
	       "<li>1 -- Fault detected</li></ul>",
	       "calFault",                   REG_UCHAR,                 0, 1, 0, 0, "", "", "1"),

  RegBlockTemp("Stepper motor fault state:<br/><ul>"
	       "<li>0 -- No stepper fault</li>"
	       "<li>1 -- Stepper fault detected</li>",
	       "stepFault",                  REG_UCHAR|REG_UNION,       0, 1, 0, 0, "", "", "1"),

  RegBlockTemp("Encoder fault flag:<br/><ul>"
	       "<li>0 -- No fault</li>"
	       "<li>1 -- At index mark</li>", 
	       "encFault",                   REG_UCHAR|REG_UNION,       0, 1),

  // An extended bitmask version of the tertState register

  RegBlockTemp("The current tertiary state returned by the module:<br/><ul>"
	       "<li>Bit 0 high -- Idle</li>"
	       "<li>Bit 1 high -- Homing</li>"
	       "<li>Bit 2 high -- Home</li>"
	       "<li>Bit 3 high -- Home error</li>"
	       "<li>Bit 4 high -- Moving</li>"
	       "<li>Bit 5 high -- 1-cm Rx selected</li>"
	       "<li>Bit 6 high -- 3-mm Rx selected</li>"
	       "<li>Bit 7 high -- 1-mm Rx selected</li>"
	       "<li>Bit 8 high -- Manual position</li>"
	       "<li>Bit 9 high -- Stuck</li>",
	       "tertStateMask",              REG_UCHAR|REG_UNION,       0, 1),

  // Used to be:
  //
  //  RegBlockTemp("A mask of tertiary states:<br/><ul>"
  //	       "<li>Bit 0 high -- In position</li>"
  //	       "<li>Bit 1 high -- Moving</li>"
  //	       "<li>Bit 2 high -- Homing</li>"
  //	       "<li>Bit 3 high -- Stopped</li>"
  //	       "<li>Bit 4 high -- Positive soft limit</li>"
  //	       "<li>Bit 5 high -- Negative soft limit</li>"
  //	       "<li>Bit 6 high -- Hard limit</li>"
  //	       "<li>Bit 7 high -- Error</li></ul>",
  //	           "tertStateMask",           REG_UCHAR|REG_UNION,      0, 1),

  // For monitoring only

  RegBlockTemp("(For monitoring only) True if the current position of the "
	       "tertiary constitutes an error for the requested receiver.  "
	       "False if not.", 
	       "tertPosError",                REG_BOOL|REG_EXC,         0, 1, 0, 0, "", "", "true"),

  // New register added as of March 2011

  RegBlockTemp("Counter-clockwise directional limit:<br/><ul>"
	       "<li>0: Not at limit</li>"
	       "<li>1: Limit reached</li></ul>",
	       "ccwDirLim",                   REG_UCHAR,      0, 1),

  RegBlockTemp("Clockwise directional limit:<br/><ul>"
	       "<li>0: Not at limit</li>"
	       "<li>1: Limit reached</li></ul>",
	       "cwDirLim",                    REG_UCHAR,      0, 1),

  RegBlockTemp("Counter-clockwise ultimate limit:<br/><ul>"
	       "<li>0: Not at limit</li>"
	       "<li>1: Limit reached</li></ul>",
	       "ccwUltLim",                   REG_UCHAR,      0, 1),

  RegBlockTemp("Clockwise ultimate limit:<br/><ul>"
	       "<li>0: Not at limit</li>"
	       "<li>1: Limit reached</li></ul>",
	       "cwUltLim",                    REG_UCHAR,      0, 1),

  RegBlockTemp("Stepper enable state (via the switch on the CAN module):<br/><ul>"
	       "<li>0 -- Stepper is enabled</li>"
	       "<li>1 -- Stepper is disabled</li></ul>",
	       "stepDisabled",                REG_UCHAR,      0, 1),

  RegBlockTemp("This will show valid when the tertiary mirror has been in a valid"
	       "position, and the calibration load is completely in or out for the"
	       "complete half second blanking frame time.",
	       "dataValid",                   REG_UCHAR,      0, 1),

  RegBlockTemp("Encoder angle reading for the tertiary mirror.  Units: 0.01 degrees",
	       "tertPos",                     REG_SHORT,      0, 1),

  RegBlockTemp("Encoder index mark:<br/><ul>"
	       "<li>0 -- Not at index</li>"
	       "<li>1 -- At index mark</li></ul>", 
	       "encIndex",                    REG_UCHAR,      0, 1),

  RegBlockTemp("Calibrator and tertiary mirror assembly identifier",
	       "calId",                       REG_UCHAR,      0, 1),

  RegBlockTemp("Calibrator load out of beam (in sky position):<br/><ul>"
	       "<li>0 -- Not in sky position</li>"
	       "<li>1 -- In sky position</li></ul>", 
	       "calOut",                      REG_UCHAR,      0, 1),

  RegBlockTemp("Calibrator load fully in beam (ambient position):<br/><ul>"
	       "<li>0 -- Ambient load not in</li>"
	       "<li>1 -- Ambient load in</li></ul>", 
	       "calIn",                       REG_UCHAR,      0, 1),

  RegBlockTemp("Calibrator load at out position hard limit:<br/><ul>"
	       "<li>0 -- Ok</li>"
	       "<li>1 -- At limit</li></ul>", 
	       "calOutLim",                   REG_UCHAR,      0, 1),

  RegBlockTemp("Calibrator load at in position hard limit:<br/><ul>"
	       "<li>0 -- Ok</li>"
	       "<li>1 -- At limit</li></ul>", 
	       "calInLim",                    REG_UCHAR,      0, 1),

  RegBlockTemp("Calibrator load driver chip over temperature:<br/><ul>"
	       "<li>0 -- Ok</li>"
	       "<li>1 -- Driver fault detected</li></ul>", 
	       "calDriveFault",               REG_UCHAR,      0, 1),

  RegBlockTemp("Status of calibration load manual switch:<br/><ul>"
	       "<li>0 -- Normal operation</li>"
	       "<li>1 -- Disabled</li></ul>", 
	       "calDisable",                  REG_UCHAR,      0, 1),

  RegBlockTemp("Calibration load motion completed and motor turned off:<br/><ul>"
	       "<li>0 -- Motor running</li>"
	       "<li>1 -- Motor stopped</li></ul>",
	       "calDone",                     REG_UCHAR,      0, 1),

  RegBlockTemp("Position error of the tertiary mirror relative to requested position. Units: 0.01 degrees", 
	       "tertPosErr",                  REG_SHORT|REG_PREAVG,     0, 1),

  RegBlockTemp("FPGA Major version number", 
	       "fpgaMaj",                     REG_UCHAR,      0, 1),

  RegBlockTemp("FPGA Minor version number", 
	       "fpgaMin",                     REG_UCHAR,      0, 1),

  RegBlockTemp("FPGA version number", 
	       "fpgaVer",                     REG_FLOAT,      0, 1),

  RegBlockTemp("Control byte for the tertiary mirror stepper driver:<br/><ul>"
	       "<li>0: Driver enable</li>"
	       "<li>1: Step enable</li>"
	       "<li>2: Direction</li>"
	       "<li>3: Home</li>"
	       "<li>4: Reset fault</li>"
	       "<li>5: Set acquired LED</li>"
	       "<li>6: Select stepper frequency</li></ul>",
	       "ctrlReg",                     REG_UCHAR,      0, 1),

  RegBlockTemp("Last calibrator fault detected was hard limit:<br/><ul>"
	       "<li>0: False</li>"
	       "<li>1: True</li></ul>", 
	       "calHardLimFault",             REG_UCHAR,      0, 1),

  RegBlockTemp("Last calibrator fault detected was stepper drive:<br/><ul>"
	       "<li>0: False</li>"
	       "<li>1: True</li></ul>", 
	       "calStepFault",                REG_UCHAR,      0, 1),

  RegBlockTemp("Last calibrator fault detected was disable:<br/><ul>"
	       "<li>0: False</li>"
	       "<li>1: True</li></ul>", 
	       "calDisableFault",             REG_UCHAR,      0, 1),
};

RegBlockTemp* getSzaCalTert()
{
  return szaCalTert;
}

unsigned getSizeOfSzaCalTert()
{
  return ARRAY_DIM(szaCalTert);
}

/**.......................................................................
 * Describe the register map of the SZA Antenna IF CAN module
 */
static RegBlockTemp szaIFMod[] = {

  // Registers common to all CAN modules

  CAN_COMMON_REGS

  // IFMod specific registers

  RegBlockTemp("", "ifTotalPower",            REG_FLOAT|REG_PREAVG,  0, 1), 
  RegBlockTemp("", "pamTemperature",          REG_FLOAT|REG_PREAVG,  0, 1),
  
  RegBlockTemp("", "totalAtten",              REG_FLOAT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "pamStatus",               REG_UCHAR|REG_UNION,   0, 1),
  RegBlockTemp("", "ifSwitchState",           REG_UCHAR|REG_UNION,   0, 1),
  RegBlockTemp("", "laserStatus",             REG_UCHAR|REG_UNION,   0, 1),
  
  RegBlockTemp("", "laserPower",              REG_FLOAT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "laserRegError",           REG_FLOAT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "inputAtten",              REG_FLOAT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "outputAtten",             REG_FLOAT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "laserId",                 REG_UCHAR,             0, 8),

  RegBlockTemp("", "errorCount",              REG_UCHAR,             0, 1)
};

RegBlockTemp* getSzaIFMod()
{
  return szaIFMod;
}

unsigned getSizeOfSzaIFMod()
{
  return ARRAY_DIM(szaIFMod);
}

/**.......................................................................
 * Describe the register map of the Berkeley Interface module
 */
static RegBlockTemp szaIntMod[] = {

  // Include registers common to all can modules

  CAN_COMMON_REGS

  // Registers specific to the interface module

  RegBlockTemp("", "photoLev10MHz",   REG_SHORT|REG_PREAVG,  0, 1), 
  RegBlockTemp("", "photoLev50MHz",   REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "photoLevLOTerm",  REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "loRFInLev",       REG_SHORT|REG_PREAVG,  0, 1),
  
  RegBlockTemp("", "loRFOutLev",      REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "loTempLev",       REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "pamAtten",        REG_UCHAR|REG_PREAVG,  0, 1),
  RegBlockTemp("", "modTemp",         REG_SHORT|REG_PREAVG,  0, 1),
  
  RegBlockTemp("", "powSupPos24V",    REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "powSupNeg28V",    REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "powSupPosDig5V",  REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "powSupPos15V",    REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "powSupNeg9V",     REG_SHORT|REG_PREAVG,  0, 1),
  
  RegBlockTemp("", "powSupNeg5V",     REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "powSupNeg15V",    REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "powSupPos5V",     REG_SHORT|REG_PREAVG,  0, 1),
  RegBlockTemp("", "powSupPos9V",     REG_SHORT|REG_PREAVG,  0, 1),
  
  RegBlockTemp("", "statusRegister",  REG_UCHAR|REG_UNION,   0, 3),

  RegBlockTemp("", "lockStatus10MHz",  REG_UCHAR,   0, 1, 0, 0, "", "0", ""),
  RegBlockTemp("", "photoStatus10MHz", REG_UCHAR,   0, 1, 0, 0, "", "0", ""),
  RegBlockTemp("", "photoStatus50MHz", REG_UCHAR,   0, 1, 0, 0, "", "0", ""),
  RegBlockTemp("", "loTermPowerState", REG_UCHAR,   0, 1, 0, 0, "", "0", ""),
  RegBlockTemp("", "sn10MHzModule",    REG_UCHAR,   0, 1),
  RegBlockTemp("", "sn50MHzModule",    REG_UCHAR,   0, 1),
  RegBlockTemp("", "snLoTermModule",   REG_UCHAR,   0, 1),
};

RegBlockTemp* getSzaIntMod()
{
  return szaIntMod;
}

unsigned getSizeOfSzaIntMod()
{
  return ARRAY_DIM(szaIntMod);
}
