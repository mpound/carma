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
 * Define the register map of the PMAC motion controller dual port ram.
 */
static RegBlockTemp szaPmac[] = {
  
  RegBlockTemp("A flag by which we'll tell the pmac that we are writing a new "
	       "commanded position.  The persistent value will be 0.  When we "
	       "have written a new commanded position, we will set this to 1. "
	       "On a state change of this flag, the pmac will read the new "
	       "commanded position, then set this flag back to 0.",

	       "new_position",    REG_UINT|REG_DPRAM|REG_RW,           0x0, 1),
  
  RegBlockTemp(" A new commanded position from the host.",
	       "new_mode",        REG_UINT|REG_DPRAM|REG_W,            0x4, 1),
  
  RegBlockTemp("", "new_az",      REG_INT|REG_DPRAM|REG_W,             0x8, 1),
  
  RegBlockTemp("", "new_el",      REG_INT|REG_W,                       0xC, 1),
  
  RegBlockTemp("", "new_az_rate", REG_INT|REG_W,                       0x10, 1),
  
  RegBlockTemp("", "new_el_rate", REG_INT|REG_W,                       0x14, 1),
  
  RegBlockTemp("A flag by which we will tell the pmac that we want to read"
	       "monitor data back.  Persistent value will be 0.  We will set "
	       "this to 1 when we want to read, then delay some unspecified "
	       "amount of time to allow the pmac time to finish writing the"
	       "monitor data. The pmac will be responsible for setting this"
	       "back to 0 when it is done writing.",

	       "host_read",       REG_UINT|REG_DPRAM|REG_RW,           0x18, 1),
  
  //------------------------------------------------------------
  // Monitor data.
  //------------------------------------------------------------
  
  RegBlockTemp("A flag used by the pmac to let the host know if it was done"
	       "writing to dpram. This will not be used in the current system.",
	       "pmac_write",         REG_UINT|REG_DPRAM|REG_R|REG_EXC, 0x1C, 1),
  
  RegBlockTemp("A flag to tell if the pmac is tracking",
	       "position_fault",     REG_UINT|REG_DPRAM|REG_R,   0x20, 1),
  
  RegBlockTemp("", "mtr_stat",           REG_UINT|REG_DPRAM|REG_R,   0x24, 1),
  RegBlockTemp("", "mtr_pos",            REG_INT|REG_DPRAM|REG_R,    0x28, 2),
  RegBlockTemp("", "mtr_com_i",          REG_INT|REG_DPRAM|REG_R,    0x30, 2),
  RegBlockTemp("", "mtr_com_i_phase_az", REG_INT|REG_DPRAM|REG_R,    0x38, 3),
  RegBlockTemp("", "mtr_com_i_phase_el", REG_INT|REG_DPRAM|REG_R,    0x44, 3),
  RegBlockTemp("", "mtr_i_mon_phase_az", REG_INT|REG_DPRAM|REG_R,    0x50, 2),
  RegBlockTemp("", "mtr_i_mon_phase_el", REG_INT|REG_DPRAM|REG_R,    0x58, 2),
  RegBlockTemp("", "enc_conv_tab",       REG_INT|REG_DPRAM|REG_R,    0x60, 4),
  RegBlockTemp("", "res_inc_cnt_err",    REG_UINT|REG_DPRAM|REG_R,   0x70, 1),
  RegBlockTemp("", "count",              REG_INT|REG_DPRAM|REG_R,    0x74, 1),
  RegBlockTemp("", "res_abs",            REG_INT|REG_DPRAM|REG_R,    0x78, 2),
  RegBlockTemp("", "res_inc_raw",        REG_INT|REG_DPRAM|REG_R,    0x80, 2),
  RegBlockTemp("", "axis_stat",          REG_UINT|REG_DPRAM|REG_R,   0x88, 1),
  RegBlockTemp("", "az_pos",             REG_INT|REG_DPRAM|REG_R,    0x8C, 1),
  RegBlockTemp("", "el_pos",             REG_INT|REG_DPRAM|REG_R,    0x90, 1),
  RegBlockTemp("", "az_err",             REG_INT|REG_DPRAM|REG_R,    0x94, 1),
  RegBlockTemp("", "el_err",             REG_INT|REG_DPRAM|REG_R,    0x98, 1),
  RegBlockTemp("", "az_rms_err",         REG_INT|REG_DPRAM|REG_R,    0x9C, 1),
  RegBlockTemp("", "el_rms_err",         REG_INT|REG_DPRAM|REG_R,    0xA0, 1),
  RegBlockTemp("", "x_tilt",             REG_INT|REG_DPRAM|REG_R,    0xA4, 1),
  RegBlockTemp("", "y_tilt",             REG_INT|REG_DPRAM|REG_R,    0xA8, 1),
  RegBlockTemp("", "t_tilt",             REG_INT|REG_DPRAM|REG_R,    0xAC, 1),
  RegBlockTemp("", "tb_offset",          REG_INT|REG_DPRAM|REG_R,    0xB0, 1),
  RegBlockTemp("", "tb_raw_count",       REG_INT|REG_DPRAM|REG_R,    0xB4, 1),
  RegBlockTemp("", "drive_status",       REG_UINT|REG_DPRAM|REG_R,   0xB8, 1),
  RegBlockTemp("", "pvt_move_time",      REG_INT|REG_DPRAM|REG_R,    0xBC, 1),
  RegBlockTemp("", "track_time",         REG_INT|REG_DPRAM|REG_R,    0xC0, 1),
  RegBlockTemp("", "mtr_mon_i",          REG_INT|REG_DPRAM|REG_R,    0xC4, 2),
  RegBlockTemp("", "amp_err_code",       REG_INT|REG_DPRAM|REG_R,    0xCC, 2),
  RegBlockTemp("", "amp_temp",           REG_INT|REG_DPRAM|REG_R,    0xD4, 2, 0, 0, "0.01 C", "0", "5500"),
  
  // These are hangovers from DASI that can't be removed until the
  // tracker code is modified
  
  RegBlockTemp("", "new_dk",         REG_INT|REG_DPRAM|REG_W|REG_EXC, 0xD0, 1),
  RegBlockTemp("", "new_dk_rate",    REG_INT|REG_DPRAM|REG_W|REG_EXC, 0xD0, 1),
  RegBlockTemp("", "dk_pos",         REG_INT|REG_DPRAM|REG_W|REG_EXC, 0xD0, 1),

  RegBlockTemp("A bitmask of states corresponding to drive_status:<br/><ul>"
	       "<li>Bit 0 high -- Stopped</li>"
	       "<li>Bit 1 high -- Running</li>"
	       "<li>Bit 2 high -- Program ok</li>"
	       "<li>Bit 3 high -- Program error</li>"
	       "<li>Bit 4 high -- Source not acquired</li>"
	       "<li>Bit 5 high -- Source acquired</li>"
	       "<li>Bit 6 high -- Stable</li>"
	       "<li>Bit 7 high -- Unstable</li></ul>",
	           "statusMask",     REG_UCHAR|REG_UNION)
};

/**.......................................................................
 * Create a virtual board of Tracker software registers.
 */
static RegBlockTemp szaTracker[] = {
  
  RegBlockTemp("A bitwise union of sza::util::Pointing::Parameter "
	       "enumerators. Each represents an unreceived pointing model "
	       "parameter (see sza::util::Pointing for definitions). ",
	       "lacking",     REG_DEFAULT,        0, 1),
  
  RegBlockTemp("The MJD date and time",
	       "utc",         REG_UTC,            0, 1, 0, 0, "absTime"),
  
  RegBlockTemp("The local apparent sidereal time (milliseconds) ",
	       "lst",         REG_DEFAULT,        0, 1, 0, 0, "ms"),
  
  RegBlockTemp("The value of UT1-UTC (milliseconds) ",
	       "ut1utc",      REG_INT, 0, 1, 0, 0, "ms"),
  
  RegBlockTemp("The value of the equation of the equinoxes (milliseconds) ",
	       "eqneqx",      REG_INT, 0, 1, 0, 0, "ms"),
  
  RegBlockTemp("The tracking mode (A PmacMode enumerator from rtcnetcoms.h) ",
	       "mode",        REG_DEFAULT,        0, 1),
  
  RegBlockTemp("The deck-axis tracking mode (A DeckMode enumerator from "
	       "rtcnetcoms.h) ",
	       "deck_mode",   REG_DEFAULT,        0, 1),
  
  RegBlockTemp("The A and B refraction terms (micro-arcseconds) and the "
	       "resulting offset in elevation (milli-arcseconds)",
	       "refraction",  REG_INT, 0, 3),
  
  RegBlockTemp("The encoder zero points {azimuth,elevation,deck} "
	       "(milli-arcseconds) ",
	       "encoder_off", REG_INT, 0, 3, 0, 0, "mas"),
  
  RegBlockTemp("The number of encoder counts per turn "
	       "{azimuth,elevation,deck} ",
	       "encoder_mul", REG_INT, 0, 3),
  
  RegBlockTemp("The azimuth software limits {min,max} in topocentric mount "
	       "angles (mas) ",
	       "az_limits",   REG_INT, 0, 2, 0, 0, "mas"),
  
  RegBlockTemp("The elevation software limits {min,max} in topocentric mount "
	       "angles (mas) ",
	       "el_limits",   REG_INT, 0, 2, 0, 0, "mas"),
  
  RegBlockTemp("The deck software limits {min,max} in topocentric mount angles "
	       "(mas) ",
	       "dk_limits",   REG_INT, 0, 2, 0, 0, "mas"),
  
  RegBlockTemp("Axis tilts {hour-angle, latitude, elevation} "
	       "(milli-arcseconds) ",
	       "tilts",       REG_INT, 0, 3, 0, 0, "mas"),
  
  RegBlockTemp("The gravitational flexure of the telescope. "
	       "First index is the coefficient of the sin(el) term "
	       "(milli-arcseconds per sin(el), "
	       "second index is the coefficient of the cos(el) term "
	       "(mas per cos(el))",
	       "flexure",     REG_INT, 0, 2),
  
  RegBlockTemp("The collimation model being used (true if radio, false if "
	       "optical) ",
	       "axis",        REG_DEFAULT,        0, 1),
  
  RegBlockTemp("The collimation tilt {magnitude, direction} (milli-arcseconds)",
	       "collimation", REG_INT, 0, 2, 0, 0, "mas"),
  
  RegBlockTemp("The actual location of the site {longitude (mas), "
	       "latitude (mas), altitude (mm)}",
	       "siteActual",  REG_INT, 0, 3),
  
  RegBlockTemp("The fiducial location of the site {longitude (mas), latitude "
	       "(mas), altitude (mm)}",
	       "siteFiducial",REG_INT, 0, 3),
  
  RegBlockTemp("Offset from the nominal site {Up(mm), East(mm), North(mm)}",
	       "location",    REG_INT, 0, 3, 0, 0, "mm"),
  
  RegBlockTemp("The '\\0' terminated source name ",
	       "source",      REG_UCHAR|REG_STRING,  0, 12),
  
  RegBlockTemp("The geocentric apparent {RA (mas), Dec (mas), "
	       "Distance (micro-AU)} ",
	       "equat_geoc",  REG_INT, 0, 3),
  
  RegBlockTemp("User-supplied equatorial offsets {RA,Dec} (milli-arcsec) ",
	       "equat_off",   REG_INT, 0, 2, 0, 0, "mas"),
  
  RegBlockTemp("The geocentric apparent {Az,El,Pa} (milli-arcseconds) ",
	       "horiz_geoc",  REG_INT, 0, 3, 0, 0, "mas"),
  
  RegBlockTemp("The topocentric apparent {Az,El,Pa} (milli-arcseconds) ",
	       "horiz_topo",  REG_INT, 0, 3, 0, 0, "mas"),
  
  RegBlockTemp("The mount {Az,El,Pa} (milli-arcseconds) ",
	       "horiz_mount", REG_INT, 0, 3, 0, 0, "mas"),
  
  RegBlockTemp("User-supplied offsets in {Azimuth, Elevation} "
	       "(milli-arcsec) ",
	       "horiz_off",   REG_INT, 0, 3, 0, 0, "mas"),
  
  RegBlockTemp("Sky-based offsets {x,y} (milli-arcsec), with y directed towards "
	       "the zenith along the great circle that joins the zenith and the "
	       "(un-offset) pointing center, and x directed along the "
	       "perpendicular great circle that goes through the (un-offset) "
	       "pointing center and is perpendicular to y",
	       "sky_xy_off",  REG_INT, 0, 2, 0, 0, "mas"),
  
  RegBlockTemp("The demanded encoder positions {Azimuth, Elevation} "
	       "(counts) ",
	       "counts",      REG_INT, 0, 3),
  
  RegBlockTemp("The demanded move rates {Azimuth, Elevation} "
	       "(milli-counts/second) ",
	       "rates",       REG_INT, 0, 3, 0, 0, "mas/s"),
  
  RegBlockTemp("The actual position of the telescope on the last 1-second tick {Azimuth, Elevation} (mas)",
	       "actual",      REG_INT, 0, 3, 0, 0, "mas"),
  
  RegBlockTemp("The expected position of the telescope on the last 1-second "
	       "tick ",
	       "expected",    REG_INT, 0, 3, 0, 0, "mas"),
  
  RegBlockTemp("The difference between the actual and expected positions "
	       "of the telescope ",
	       "errors",      REG_INT, 0, 3, 0, 0, "mas"),
  
  RegBlockTemp("The tracking status, represented by one of the TrackingStatus "
	       "enumerators defined in sza::util::TrackingStatus.h:<br/><ul>"
	       "<li>0 -- Lacking</li>"
	       "<li>1 -- Time error</li>"
	       "<li>2 -- Updating</li>"
	       "<li>3 -- Halted</li>"
	       "<li>4 -- Slewing</li>"
	       "<li>5 -- Tracking</li>"
	       "<li>6 -- Too low</li>"
	       "<li>7 -- Too high</li></ul>",
	       "state",       REG_UCHAR|REG_UNION,        0, 1),

  RegBlockTemp("A bit mask of the tracking status:<br/><ul>"
	       "<li>Bit 0 high -- Lacking</li>"
	       "<li>Bit 1 high -- Time error</li>"
	       "<li>Bit 2 high -- Updating</li>"
	       "<li>Bit 3 high -- Halted</li>"
	       "<li>Bit 4 high -- Slewing</li>"
	       "<li>Bit 5 high -- Tracking</li>"
	       "<li>Bit 6 high -- Too low</li>"
	       "<li>Bit 7 high -- Too high</li></ul>",
	       "stateMask",   REG_UCHAR|REG_UNION,        0, 1),

  RegBlockTemp("The following register takes either the value 0 or 1. If 1, "
	       "this means that the telescope was known to be off source "
	       "during the archived frame.",
	       "off_source",  REG_UINT|REG_UNION,  0, 1),
};

//-----------------------------------------------------------------------
// Collect the sza per-antenna boards into an array and give them names.
//-----------------------------------------------------------------------

static RegBoardTemp sza_antenna_boards[] = {

  {"bias",      getSzaBiasGunn(),  getSizeOfSzaBiasGunn(),  {0x0},
   "Registers of the bias-tuned Gunn oscillator"},
  {"caltert",   getSzaCalTert(),   getSizeOfSzaCalTert(),   {0x0},
   "Registers of the calibration/tertiary module"},    
  {"ifmod",     getSzaIFMod(),     getSizeOfSzaIFMod(),     {0x0},
   "Registers of the IF module"},      
  {"intmod",    getSzaIntMod(),    getSizeOfSzaIntMod(),    {0x0},
   "Registers of the interface module"},

  {"pmac",      szaPmac,           ARRAY_DIM(szaPmac),      {0x0, 0x0},
   "Registers of the PMAC motion controller"},      

  {"rx",        getSzaRx(),        getSizeOfSzaRx(),        {0x0},
   "Receiver registers"},
  {"thermal",   getSzaThermal(),   getSizeOfSzaThermal(),   {0x0},
   "Registers of the thermal control module"},
  {"tiltmeter", getSzaTiltMeter(), getSizeOfSzaTiltMeter(), {0x0},
   "Registers of the tiltmeter control module"},

  {"tracker",   szaTracker,        ARRAY_DIM(szaTracker),   {0x0},     
   "Tracking registers"},

  {"varactor",  getSzaVarGunn(),   getSizeOfSzaVarGunn(),   {0x0},
   "Registers of the varactor-tuned Gunn oscillator"},
  {"yig",       getSzaYig(),       getSizeOfSzaYig(),       {0x0},
   "Registers of the YIG oscillator"},
};

/**.......................................................................
 * Create a template for a single antenna
 */
static RegTemplate sza_antenna_template = {
  sza_antenna_boards,   ARRAY_DIM(sza_antenna_boards)
};

// And a public accessor for it

RegTemplate* getSzaAntennaTemplate()
{
  return &sza_antenna_template;
}

/**.......................................................................
 * Create the SZA antenna register map.
 *
 * Output:
 *  return    SzaRegMap *   The SZA register container object.
 */
SzaRegMap *new_SzaAntRegMap(void)
{
  RegMap* regmap = new RegMap(&sza_antenna_template);
  return regmap;
}

/**.......................................................................
 * Delete a register map that was previously returned by new_SzaRegMap().
 *
 * Input:
 *  regs    SzaRegMap *  The register map to be deleted.
 * Output:
 *  return  SzaRegMap *  The deleted register map (always NULL).
 */
SzaRegMap *del_SzaAntRegMap(SzaRegMap *regs)
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
int net_put_SzaAntRegMap(NetBuf *net)
{
  // Pack the register map via its template.
  
  return net_put_RegTemplate(&sza_antenna_template, net);
}

/**.......................................................................
 * Return the number of bytes needed by net_put_SzaAntRegMap() to pack the
 * current register map into a network buffer.
 *
 * Output:
 *  return  long   The number of bytes required.
 */
long net_SzaAntRegMap_size(void)
{
  return net_RegTemplate_size(&sza_antenna_template);
}

