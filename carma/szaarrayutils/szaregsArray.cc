#include "carma/szaarrayutils/szaregs.h"
#include "carma/szaarrayutils/arraymap.h"
#include "carma/szaarrayutils/arraytemplate.h"
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

/**.......................................................................
 * The following virtual board encapsulates the state of the delay
 * engine for each antenna
 */
static RegBlockTemp szaDelay[] = {
  
  
  RegBlockTemp("True if we are using the adjustable delay",
	       "useAdjustableDelay",   REG_BOOL,              0, AntNum::NANT),
  
  RegBlockTemp("True if we are using the fixed delay",
	       "useFixedDelay",        REG_BOOL,              0, AntNum::NANT),
  
  RegBlockTemp("True if we are using the geometric delay",
	       "useGeometricDelay",    REG_BOOL,              0, AntNum::NANT),
  
  RegBlockTemp("True if we are using the tropospheric delay",
	       "useTroposphericDelay", REG_BOOL,              0, AntNum::NANT),
  
  RegBlockTemp("True if we are using the non-intersection term",
	       "useNiaDelay",          REG_BOOL,              0, AntNum::NANT),
  
  RegBlockTemp("The N displacement, in meters, of the elevation axis, at AZ=0",
	       "nia",                  REG_DOUBLE,            0, AntNum::NANT),
  
  RegBlockTemp("True if a frequency has been set for this antenna",
	       "hasFrequency",         REG_BOOL,              0, AntNum::NANT),
  
  RegBlockTemp("The LO frequency for this antenna (Hz)",
	       "LOFrequency",          REG_DOUBLE,            0, AntNum::NANT),
  
  RegBlockTemp("The sky frequency for this antenna (Hz)",
	       "skyFrequency",         REG_DOUBLE,            0, AntNum::NANT),
  
  RegBlockTemp("The value of a fixed delay (ns)",
	       "fixedDelay",           REG_DOUBLE,            0, AntNum::NANT),
  
  RegBlockTemp("The value of an adjustable delay (ns)",
	       "adjustableDelay",      REG_DOUBLE,            0, AntNum::NANT),
  
  RegBlockTemp("The fiducial location of the site {longitude (rad), latitude"
	       "(rad), altitude (m)}",
	       "siteFiducial",         REG_DOUBLE,            0, 3),
  
  RegBlockTemp("Antenna offsets from the nominal site {Up(m), East(m), North(m)}",
	       "location",             REG_DOUBLE,            0, AntNum::NANT, 3),

  RegBlockTemp("The phase reference location, relative to the nominal site {Up(m), East(m), North(m)}", 
	       "referenceLocation",    REG_DOUBLE,            0, 3),

  RegBlockTemp("True if the sign of the delay is flipped when sent to the correlator", 
	       "flipCorrDelay",        REG_BOOL),

  RegBlockTemp("True if the sign of the delay is flipped when sent to the lobe rotator", 
	       "flipLrDelay",          REG_BOOL),

  RegBlockTemp("True if delays are being sent to the correlator", 
	       "corrFringeTracking",   REG_BOOL),

  RegBlockTemp("Ture if delays are being sent to the lobe rotator", 
	       "lrFringeTracking",     REG_BOOL),

  RegBlockTemp("A NULL-terminated string indicating the antenna currently used "
	       "as a phase reference", 
	       "referenceAntenna",     REG_UCHAR,             0, SRC_LEN)
};

/**.......................................................................
 * The following virtual board encapsulates the downconverter
 */
static RegBlockTemp szaTrailer[] = {
  
  RegBlockTemp("", "received",           REG_UCHAR|REG_UNION),

  RegBlockTemp("The ambient temperature (under trailer) (C)",
	       "ambient",                REG_DOUBLE|REG_PREAVG),

  RegBlockTemp("The air supply temperature (into trailer) (C)",
	       "supply",                 REG_DOUBLE|REG_PREAVG),

  RegBlockTemp("The air return temperature (from trailer) (C)",
	       "return",                 REG_DOUBLE|REG_PREAVG),

  RegBlockTemp("Thermostat setting (C)",
	       "setTemperature",         REG_DOUBLE|REG_PREAVG),

  RegBlockTemp("Actual temperature (C)",
	       "actualTemperature",      REG_DOUBLE|REG_PREAVG),

  RegBlockTemp("Compressor 1 (C)",
	       "compressor1",            REG_DOUBLE|REG_PREAVG),

  RegBlockTemp("Compressor 2 (C)",
	       "compressor2",            REG_DOUBLE|REG_PREAVG),

  RegBlockTemp("Alarm level (C)",
	       "alarmLevel",             REG_DOUBLE|REG_PREAVG),

  RegBlockTemp("Power down setting (C)",
	       "powerDownSetting",       REG_DOUBLE|REG_PREAVG),

  RegBlockTemp("Power down? (C)",
	       "powerDown",              REG_DOUBLE|REG_PREAVG),

  RegBlockTemp("Phase per foot (at 2 GHz)",
	       "phasePerFoot",           REG_DOUBLE|REG_PREAVG),

  RegBlockTemp("Inside temperature, as measured by the wall thermometer"
	       " inside the trailer (C)",
	       "insideTemperature",      REG_DOUBLE|REG_PREAVG),
};

/**.......................................................................
 * The following virtual board encapsulates the downconverter
 */
static RegBlockTemp szaDcon[] = {
  
  RegBlockTemp("", "received",       REG_USHORT|REG_UNION,     0, 1), 

  // Psys powers
  
  RegBlockTemp("Total powers [antennas][bands], in mW", 
	       "psys",                REG_FLOAT|REG_PREAVG, 0, AntNum::NANT, CorrelatorBand::NBAND),

  // Psys powers
  
  RegBlockTemp("Total powers [antennas][bands][5], in mW", 
	       "psysSamp0",            REG_SHORT|REG_EXC, 0, AntNum::NANT, CorrelatorBand::NBAND),
  RegBlockTemp("Total powers [antennas][bands][5], in mW", 
	       "psysSamp1",            REG_SHORT|REG_EXC, 0, AntNum::NANT, CorrelatorBand::NBAND),
  RegBlockTemp("Total powers [antennas][bands][5], in mW", 
	       "psysSamp2",            REG_SHORT|REG_EXC, 0, AntNum::NANT, CorrelatorBand::NBAND),
  RegBlockTemp("Total powers [antennas][bands][5], in mW", 
	       "psysSamp3",            REG_SHORT|REG_EXC, 0, AntNum::NANT, CorrelatorBand::NBAND),
  RegBlockTemp("Total powers [antennas][bands][5], in mW", 
	       "psysSamp4",            REG_SHORT|REG_EXC, 0, AntNum::NANT, CorrelatorBand::NBAND),

  // Psys attenuation
  
  RegBlockTemp("Psys attenuation, in 0.01 dB", 
	       "psysAttenuation",     REG_SHORT, 0, AntNum::NANT, CorrelatorBand::NBAND),

  // Temperature
  
  RegBlockTemp("Module temperatures, in 0.01 C", 
	       "temperature",         REG_SHORT|REG_PREAVG, 0, AntNum::NANT, CorrelatorBand::NBAND),

  // IF output power
  
  RegBlockTemp("IF output powers, in mW", 
	       "ifOutputPower",       REG_FLOAT|REG_PREAVG, 0, AntNum::NANT, CorrelatorBand::NBAND),

  // IF output attenuation
  
  RegBlockTemp("IF output attenuation, in 0.01 dB", 
	       "ifOutputAttenuation", REG_SHORT, 0, AntNum::NANT, CorrelatorBand::NBAND),

  // Center frequency of each band

  RegBlockTemp("Center frequency of each band (currently just returns the band number)", 
	           "frequency",       REG_DOUBLE, 0, CorrelatorBand::NBAND),
};

 /**.......................................................................
 * The following virtual board encapsulates the LO Monitor
 */
static RegBlockTemp szaLobeRotator[] = {

  // Noise source received 

  RegBlockTemp("True if messages for this module were received",
	       "received",                    REG_UCHAR|REG_UNION),
  
  // LO Monitor data received 

  RegBlockTemp("A bitmask of states corresponding to:<br/><ul>"
	       "<li>Bit 0 high -- Data from the Lobe rotator were not received</li>"
	       "<li>Bit 1 high -- Data from the Lobe rotator were received</li></ul>",
	       "receivedMask",                REG_UCHAR|REG_UNION, 0, sza::util::AntNum::NANT),

  RegBlockTemp("A bitmask of states corresponding to:<br/><ul>"
	       "<li>Bit 0 high -- Data from the Lobe rotator were not received</li>"
	       "<li>Bit 1 high -- Data from the Lobe rotator were received</li></ul>",
	       "colStateMask",                REG_UCHAR|REG_UNION, 0, sza::util::AntNum::NANT),

  RegBlockTemp("A bitmask of states corresponding to:<br/><ul>"
	       "<li>Bit 0 high -- Data from the Lobe rotator were not received</li>"
	       "<li>Bit 1 high -- Data from the Lobe rotator were received</li></ul>",
	       "phaseStateMask",              REG_UCHAR|REG_UNION, 0, sza::util::AntNum::NANT),

  RegBlockTemp("A bitmask of states corresponding to:<br/><ul>"
	       "<li>Bit 0 high -- Data from the Lobe rotator were not received</li>"
	       "<li>Bit 1 high -- Data from the Lobe rotator were received</li></ul>",
	       "walshStateMask",              REG_UCHAR|REG_UNION, 0, sza::util::AntNum::NANT),

};

/**.......................................................................
 * The following virtual board encapsulates the LO Monitor
 */
static RegBlockTemp szaLoMonitor[] = {

  // Noise source received 

  RegBlockTemp("True if messages for this module were received",
	       "received",                    REG_UCHAR|REG_UNION),
  
  // LO Monitor data received 

  RegBlockTemp("A bitmask of states corresponding to:<br/><ul>"
	       "<li>Bit 0 high -- Data from the LO Monitor were not received</li>"
	       "<li>Bit 1 high -- Data from the LO Monitor were received</li></ul>",
	       "receivedMask",                REG_UCHAR|REG_UNION, 0, sza::util::LoMonitorFlags::nLo_),

  // Phase lock frequency

  RegBlockTemp("The phase lock frequencye (Hz)",
	       "phaseLockFrequency",          REG_DOUBLE|REG_PREAVG, 0, sza::util::LoMonitorFlags::nLo_),

  // Phase lock voltage

  RegBlockTemp("The phase lock voltage (mV)",
	       "phaseLockVoltage",            REG_SHORT|REG_PREAVG, 0, sza::util::LoMonitorFlags::nLo_),

  // LO Monitor status

  RegBlockTemp("", "phaseLockStatus",         REG_UCHAR|REG_UNION, 0, sza::util::LoMonitorFlags::nLo_),

  // LO Monitor status

  RegBlockTemp("", "phaseLockStatusMask",     REG_UCHAR|REG_UNION, 0, sza::util::LoMonitorFlags::nLo_),

  // LO Monitor power

  RegBlockTemp("", "phaseLockPower",          REG_FLOAT|REG_PREAVG, 0, sza::util::LoMonitorFlags::nLo_),

  // LO Monitor status

  RegBlockTemp("", "phaseLockMeasStatus",     REG_UCHAR|REG_UNION, 0, sza::util::LoMonitorFlags::nLo_),

  // LO Monitor status

  RegBlockTemp("", "phaseLockMeasStatusMask", REG_UCHAR|REG_UNION, 0, sza::util::LoMonitorFlags::nLo_),

  // Phase lock voltage2 (?)

  RegBlockTemp("The phase lock voltage2 (mV)",
	       "phaseLockVoltage2",           REG_SHORT|REG_PREAVG, 0, sza::util::LoMonitorFlags::nLo_),
};

/**.......................................................................
 * The following virtual board encapsulates the noise source
 */
static RegBlockTemp szaNoise[] = {

  // Noise source received 

  RegBlockTemp("True if messages for this module were received",
	       "received",    REG_UCHAR|REG_UNION),
  
  // Noise source received 

  RegBlockTemp("A bitmask of states corresponding to:<br/><ul>"
	       "<li>Bit 0 high -- Data from the noise source were not received</li>"
	       "<li>Bit 1 high -- Data from the noise source were received</li></ul>",
	       "receivedMask",    REG_UCHAR|REG_UNION),

  // Noise source status mask

  RegBlockTemp("A bitmask of states corresponding to:<br/><ul>"
	       "<li>Bit 0 high -- Noise source off</li>"
	       "<li>Bit 1 high -- Noise source on</li></ul>",
	       "noiseStatusMask",    REG_UCHAR|REG_UNION),
  
  // Tone source status mask
  
  RegBlockTemp("A bitmask of states corresponding to:<br/><ul>"
	       "<li>Bit 0 high -- Tone source off</li>"
	       "<li>Bit 1 high -- Tone source on</li></ul>",
	       "toneStatusMask",     REG_UCHAR|REG_UNION)
};

/**.......................................................................
 * The following virtual board encapsulates the quadrature modulators
 */
static RegBlockTemp szaQuadMod[] = {
  
  // Quad mod received 

  RegBlockTemp("True if messages for this module were received",
	       "received",    REG_UCHAR|REG_UNION),

  // Received status Mask
  
  RegBlockTemp("A bitmask of states for each antenna, corresponding to:<br/><ul>"
	       "<li>Bit 0 high -- Data from this quad mod were not received</li>"
	       "<li>Bit 1 high -- Data from this quad mod were received</li></ul>",
	       "receivedMask", REG_UCHAR|REG_UNION, 0, AntNum::NANT),

  // Quad mod enable status mask
  
  RegBlockTemp("A bitmask of states for each antenna, corresponding to:<br/><ul>"
	       "<li>Bit 0 high -- Quad mod disabled</li>"
	       "<li>Bit 1 high -- Quad mod enabled</li></ul>",
	       "enabledMask",  REG_UCHAR|REG_UNION, 0, AntNum::NANT),
};

/**.......................................................................
 * The following virtual board encapsulates the quadrature modulators
 */
static RegBlockTemp szaCorrInfo[] = {
  
  RegBlockTemp("A bitmask of states for each antenna, corresponding to:<br/><ul>"
	       "<li>Bit 0 high -- The source was: unknown or mixed for reporting bands/li>"
	       "<li>Bit 1 high -- The source was: noise for all reporting bands/li>"
	       "<li>Bit 2 high -- The source was: RF for all reporting bands</li></ul>",
	       "sourceMask",    REG_UCHAR|REG_UNION),
};

/**.......................................................................
 * Create a virtual board of weather-data software registers.
 */
static RegBlockTemp szaWeather[] = {
  
  RegBlockTemp("A bitmask of states corresponding to:<br/><ul>"
	       "<li>Bit 0 high -- Data from the Lobe rotator were not received</li>"
	       "<li>Bit 1 high -- Data from the Lobe rotator were received</li></ul>",
	       "received",         REG_UCHAR|REG_UNION, 0, 1),

  RegBlockTemp("The date and time reported by the weather station in MJD "
	       "days and ms ",
	       "utc",              REG_UTC),
  
  RegBlockTemp("The air temperature around the weather station (C) ",
	       "airTemperature",   REG_DOUBLE|REG_PREAVG),
  
  RegBlockTemp("The weather-station battery voltage (mV) ",
	       "battery",          REG_DOUBLE|REG_PREAVG),
  
  RegBlockTemp("The relative humidity (0-1)",
	       "relativeHumidity", REG_DOUBLE|REG_PREAVG),
  
  RegBlockTemp("The wind speed measured by the weather station (m/s) ",
	       "windSpeed",        REG_DOUBLE|REG_PREAVG),
  
  RegBlockTemp("The azimuth from which the wind is blowing (degrees)",
	       "windDirection",    REG_DOUBLE|REG_PREAVG),
  
  RegBlockTemp("The atmospheric pressure (millibars) ",
	       "pressure",         REG_DOUBLE|REG_PREAVG)
};

/**.......................................................................
 * Create a virtual board of synthesizer registers
 */
static RegBlockTemp szaSynth[] = {
  
  RegBlockTemp("A bitmask of states corresponding to:<br/><ul>"
	       "<li>Bit 0 high -- Data from the synthesizer were not received</li>"
	       "<li>Bit 1 high -- Data from the synthesizer were received</li></ul>",
	       "receivedMask",     REG_UCHAR|REG_UNION, 0, 1),

  RegBlockTemp("The frequency, in MHz",
	       "frequency",   REG_DOUBLE|REG_PREAVG),
  
  RegBlockTemp("The power, in dBm",
	       "power",       REG_DOUBLE|REG_PREAVG),
  
  RegBlockTemp("A bitmask of states, corresponding to:<br/><ul>"
	       "<li>Bit 0 high -- RF Output disabled</li>"
	       "<li>Bit 1 high -- RF Output enabled</li></ul>",
	       "rfOutputEnabledMask",  REG_UCHAR|REG_UNION),
  
  RegBlockTemp("A bitmask of states, corresponding to:<br/><ul>"
	       "<li>Bit 0 high -- RF Output modulation disabled</li>"
	       "<li>Bit 1 high -- RF Output modulation enabled</li></ul>",
	       "outputModEnabledMask",  REG_UCHAR|REG_UNION),
  };

/**.......................................................................
 * Describe the register map of the controllable power strips
 */
static RegBlockTemp szaPowerStrip[] = {

  RegBlockTemp("The state of each of the four outlets on the programmable "
	       "power strips (0=off, 1=on, 2=unknown). "
	       "First dimension indexes antenna number, second indexes the outlet number",
	       "outlet",                 REG_UCHAR,   0, AntNum::NANT+1, 4),

  RegBlockTemp("The state of the power strip circuit breaker (0=off, 1=on, 2=unknown)", 
	       "circuitBreaker",         REG_UCHAR,   0, AntNum::NANT+1)
};

/**.......................................................................
 * Describe the register map of the CARMA array configuration
 */
static RegBlockTemp szaCarmaConf[] = {

  RegBlockTemp("The array configuration.  One of:<br/><ul>"
	       "<li>A -- CARMA A array</li>"
	       "<li>B -- CARMA B array</li>"
	       "<li>C -- CARMA C array</li>"
	       "<li>D -- CARMA D array</li>"
	       "<li>E -- CARMA E array</li>"
	       "<li>U -- Unknown (non-standard) array</li>",
	       "config",                 REG_UCHAR,    0, 1),

  RegBlockTemp("The number of pads currently specified. "
	       "(Note that this can be less than 15)",
	       "nPad",                   REG_UINT,     0, 1),

  RegBlockTemp("The pad number corresponding to this location",
	       "pad",                    REG_UINT,     0, 15),

  RegBlockTemp("The antenna number of the antenna on this pad",
	       "antId",                  REG_UINT,     0, 15),

  RegBlockTemp("The type of antenna on this pad",
	       "antType",                REG_UINT,     0, 15),

  RegBlockTemp("The location (ENU, in meters) of this pad",
	       "location",               REG_DOUBLE,   0, 15, 3),
};

/**.......................................................................
 * Describe the register map of the SZA array configuration
 */
static RegBlockTemp szaSzaConf[] = {

  RegBlockTemp("The array configuration.  One of:<br/><ul>"
	       "<li>I -- SZA I array</li>"
	       "<li>L -- SZA L array</li>"
	       "<li>H -- SZA H array</li>"
	       "<li>U -- Unknown (non-standard) array</li>",
	       "config",                 REG_UCHAR,  0, 1),

  RegBlockTemp("The pad number corresponding to this location",
	       "pad",                    REG_UINT,   0, 8),

  RegBlockTemp("The antenna number of the antenna on this pad",
	       "antId",                  REG_UINT,   0, 8),

  RegBlockTemp("The location (ENU, in meters) of this pad",
	       "location",               REG_DOUBLE, 0, 8, 3),
};

//-----------------------------------------------------------------------
// Template for array-specific regs
//-----------------------------------------------------------------------

/**.......................................................................
 * Collect the array-specific boards into an array and give them names.
 */
static RegBoardTemp sza_array_boards[] = {
  {"powerStrip",  szaPowerStrip,  ARRAY_DIM(szaPowerStrip),  {0x0},
   "The state of the programmable BayTech power strips"}, 
  {"corrInfo",    szaCorrInfo,    ARRAY_DIM(szaCorrInfo),    {0x0},
   "Registers pertaining to the correlator"},
  {"delay",       szaDelay,       ARRAY_DIM(szaDelay),       {0x0},
   "Registers pertaining to delay calculations"},
  {"dcon",        szaDcon,        ARRAY_DIM(szaDcon),        {0x0},
   "Registers pertaining to the downconverter"},
  {"noise",       szaNoise,       ARRAY_DIM(szaNoise),       {0x0},
   "Registers pertaining to the noise source"},
  {"loMonitor",   szaLoMonitor,   ARRAY_DIM(szaLoMonitor),   {0x0},
   "Registers pertaining to the LO monitors"},
  {"lobeRotator", szaLobeRotator, ARRAY_DIM(szaLobeRotator), {0x0},
   "Registers pertaining to the lobe rotator"},
  {"trailer",     szaTrailer,     ARRAY_DIM(szaTrailer),     {0x0},
   "Registers pertaining to the correlator trailer"},
  {"quadMod",     szaQuadMod,     ARRAY_DIM(szaQuadMod),     {0x0},
   "Registers pertaining to the quadrature modulators"},
  {"weather",     szaWeather,     ARRAY_DIM(szaWeather),     {0x0},
   "Registers of the CS weather station"},
  {"wx200",       szaWeather,     ARRAY_DIM(szaWeather),     {0x0},
   "Registers of the wx200 weather station"},
  {"synth",       szaSynth,       ARRAY_DIM(szaSynth),       {0x0},
   "Registers of the HP synthesizer"},
  {"carma",       szaCarmaConf,   ARRAY_DIM(szaCarmaConf),   {0x0},
   "Registers specifying the CARMA array configuration."},
  {"sza",         szaSzaConf,     ARRAY_DIM(szaSzaConf),     {0x0},
   "Registers specifying the SZA array configuration."},
};

/**.......................................................................
 * Create a template for the array.
 */
static RegTemplate sza_array_template = {
  sza_array_boards,   ARRAY_DIM(sza_array_boards)
};

// And a public accessor for it

RegTemplate* getSzaArrayTemplate()
{
  return &sza_array_template;
}

/**.......................................................................
 * Create the SZA array register map.
 *
 * Output:
 *  return    SzaRegMap *   The SZA register container object.
 */
SzaRegMap *new_SzaArrRegMap(void)
{
  return new RegMap(&sza_array_template);
}

/**.......................................................................
 * Delete a register map that was previously returned by new_SzaRegMap().
 *
 * Input:
 *  regs    SzaRegMap *  The register map to be deleted.
 * Output:
 *  return  SzaRegMap *  The deleted register map (always NULL).
 */
SzaRegMap *del_SzaArrRegMap(SzaRegMap *regs)
{
  delete regs;
  return NULL;
}

/**.......................................................................
 * Pack the current register map for transmission over a network.
 *
 * Input:
 *  net   NetBuf *  The network buffer in which to pack the register
 *                  map. It is left to the caller to call
 *                  net_start_put() and net_end_put().
 * Output:
 *  return   int    0 - OK.
 *                  1 - Error.
 */
int net_put_SzaArrRegMap(NetBuf *net)
{
  // Pack the register map via its template.
  
  return net_put_RegTemplate(&sza_array_template, net);
}

/**.......................................................................
 * Return the number of bytes needed by net_put_SzaArrRegMap() to pack the
 * current register map into a network buffer.
 *
 * Output:
 *  return  long   The number of bytes required.
 */
long net_SzaArrRegMap_size(void)
{
  return net_RegTemplate_size(&sza_array_template);
}

