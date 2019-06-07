#include "carma/antenna/sza/antenna/corba/CarmaDataMapper.h"
#include "carma/szautil/ArrayDataFrameManager.h"

#include "carma/szautil/String.h"

#include <fstream>

using namespace std;
using namespace sza::util;
using namespace sza::antenna::corba;

const unsigned CarmaDataMapper::nSlAnt_    = 15;
const unsigned CarmaDataMapper::nSlBase_   = (nSlAnt_ * (nSlAnt_-1))/2;
const unsigned CarmaDataMapper::nBandMax_  = 16;
const unsigned CarmaDataMapper::nChan_     = 15;
const unsigned CarmaDataMapper::nWbInput_  = 8;
const unsigned CarmaDataMapper::nSlInput_  = 15;
const unsigned CarmaDataMapper::nAntTotal_ = 23;

// AntennaCommon registers

RegBlockTemp CarmaDataMapper::carmaCommonDrive_[] = {
  RegBlockTemp("AntennaCommon.Drive.sourcename",     "sourcename",     REG_UCHAR,  0, 30),
  RegBlockTemp("AntennaCommon.Drive.rightAscension", "rightAscension", REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.declination",    "declination",    REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.errorSky",       "errorSky",       REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.state",          "state",          REG_UINT|REG_UNION, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.safeState",      "safeState",      REG_UINT|REG_UNION, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.safeAzLow",      "safeAzLow",      REG_FLOAT, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.safeAzHigh",     "safeAzHigh",     REG_FLOAT, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.safeElLow",      "safeElLow",      REG_FLOAT, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.safeElHigh",     "safeElHigh",     REG_FLOAT, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.mode",           "mode",           REG_UINT|REG_UNION, 0, 1),
};

RegBlockTemp CarmaDataMapper::carmaCommonTrack_[] = {
  RegBlockTemp("AntennaCommon.Drive.Track.requestedAzimuth",   "requestedAzimuth",   REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Track.actualAzimuth",      "actualAzimuth",      REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Track.errorAzimuth",       "errorAzimuth",       REG_FLOAT,  0, 1),
  RegBlockTemp("AntennaCommon.Drive.Track.errorAzimuthSky",    "errorAzimuthSky",    REG_FLOAT,  0, 1),
  RegBlockTemp("AntennaCommon.Drive.Track.azimuthRate",        "azimuthRate",        REG_FLOAT,  0, 1),
  RegBlockTemp("AntennaCommon.Drive.Track.requestedElevation", "requestedElevation", REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Track.actualElevation",    "actualElevation",    REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Track.errorElevation",     "errorElevation",     REG_FLOAT,  0, 1),
  RegBlockTemp("AntennaCommon.Drive.Track.elevationRate",      "elevationRate",      REG_FLOAT,  0, 1),
  RegBlockTemp("AntennaCommon.Drive.Track.wrapLogic",          "wrapLogic",          REG_UINT|REG_UNION, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Track.emergencyOff",       "emergencyOff",       REG_UINT|REG_UNION, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Track.manualSwitch",       "manualSwitch",       REG_UINT|REG_UNION, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Track.trackTolerance",     "trackTolerance",     REG_FLOAT,  0, 1),
};

RegBlockTemp CarmaDataMapper::carmaCommonPoint_[] = {
  RegBlockTemp("AntennaCommon.Drive.Point.mountOffsetAz",   "mountOffsetAz",   REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Point.mountOffsetEl",   "mountOffsetEl",   REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Point.offsetAz",        "offsetAz",        REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Point.offsetEl",        "offsetEl",        REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Point.refraction",      "refraction",      REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Point.refractionModel", "refractionModel", REG_UINT|REG_UNION, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Point.magnitude",       "magnitude",       REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Point.direction",       "direction",       REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Point.coefChange",      "coefChange",      REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Point.selectedApert",   "selectedApert",   REG_UINT|REG_UNION, 0, 1),
};

//------------------------------------------------------------
// Limit monitor points
//------------------------------------------------------------

RegBlockTemp CarmaDataMapper::carmaCommonLimit_[] = {
  RegBlockTemp("AntennaCommon.Drive.Limit.azSwLimit",        "azSwLimit",        REG_UINT|REG_UNION, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Limit.elSwLimit",        "elSwLimit",        REG_UINT|REG_UNION, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Limit.azHwLimit",        "azHwLimit",        REG_UINT|REG_UNION, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Limit.elHwLimit",        "elHwLimit",        REG_UINT|REG_UNION, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Limit.azLowSwLimitVal",  "azLowSwLimitVal",  REG_FLOAT, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Limit.azHighSwLimitVal", "azHighSwLimitVal", REG_FLOAT, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Limit.azLowHwLimitVal",  "azLowHwLimitVal",  REG_FLOAT, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Limit.azHighHwLimitVal", "azHighHwLimitVal", REG_FLOAT, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Limit.elLowSwLimitVal",  "elLowSwLimitVal",  REG_FLOAT, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Limit.elHighSwLimitVal", "elHighSwLimitVal", REG_FLOAT, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Limit.elLowHwLimitVal",  "elLowHwLimitVal",  REG_FLOAT, 0, 1),
  RegBlockTemp("AntennaCommon.Drive.Limit.elHighHwLimitVal", "elHighHwLimitVal", REG_FLOAT, 0, 1),
};

RegBlockTemp CarmaDataMapper::carmaCommonLo_[] = {
  RegBlockTemp("AntennaCommon.LO.oscFreq",    "oscFreq",    REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.LO.yigFreq",    "yigFreq",    REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.LO.yigIFLevel", "yigIFLevel", REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.LO.yigError",   "yigError",   REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.LO.yigState",   "yigState",   REG_UINT|REG_UNION, 0, 1),
  RegBlockTemp("AntennaCommon.LO.yigSweep",   "yigSweep",   REG_UINT|REG_UNION, 0, 1),
  RegBlockTemp("AntennaCommon.LO.loFreq",     "loFreq",     REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.LO.loSweep",    "loSweep",    REG_UINT|REG_UNION, 0, 1),
  RegBlockTemp("AntennaCommon.LO.loState",    "loState",    REG_UINT|REG_UNION, 0, 1),
};

RegBlockTemp CarmaDataMapper::carmaCommonRx_[] = {
  RegBlockTemp("AntennaCommon.receivers.currentRx",       "currentRx",       REG_UINT|REG_UNION, 0, 1),
  RegBlockTemp("AntennaCommon.receivers.rxState",         "rxState",         REG_UINT|REG_UNION, 0, 1),
  RegBlockTemp("AntennaCommon.receivers.rxTsysState",     "rxTsysState",     REG_UINT|REG_UNION, 0, 1),
  RegBlockTemp("AntennaCommon.receivers.rxTsys",          "rxTsys",          REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.receivers.rxOffsetAz",      "rxOffsetAz",      REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.receivers.rxOffsetEl",      "rxOffsetEl",      REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.receivers.compressorState", "compressorState", REG_UINT|REG_UNION, 0, 1),
};

RegBlockTemp CarmaDataMapper::carmaCommonLocation_[] = {
  RegBlockTemp("AntennaCommon.location.latitude",  "latitude",  REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.location.longitude", "longitude", REG_DOUBLE, 0, 1),
  RegBlockTemp("AntennaCommon.location.altitude",  "altitude",  REG_DOUBLE, 0, 1),
};

//-----------------------------------------------------------------------
// SZA-specific registers
//-----------------------------------------------------------------------

RegBlockTemp CarmaDataMapper::szaTracker_[] = {
  RegBlockTemp("Tracker.errors",      "errors",      REG_INT,   0, 3),
  RegBlockTemp("Tracker.equat_geoc",  "equat_geoc",  REG_INT,   0, 3),
  RegBlockTemp("Tracker.lst",         "lst",         REG_INT,   0, 1),
  RegBlockTemp("Tracker.actual",      "actual",      REG_INT,   0, 3),
  RegBlockTemp("Tracker.lacking",     "lacking",     REG_INT,   0, 1),
  RegBlockTemp("Tracker.source",      "source",      REG_UCHAR, 0, 30),
  RegBlockTemp("Tracker.sky_xy_off",  "sky_xy_off",  REG_INT,   0, 2),
  RegBlockTemp("Tracker.stateMask",   "stateMask",   REG_SHORT, 0, 1),
  RegBlockTemp("Tracker.location",    "location",    REG_INT,   0, 3),
  RegBlockTemp("Tracker.horiz_topo",  "horiz_topo",  REG_INT,   0, 3),
  RegBlockTemp("Tracker.horiz_mount", "horiz_mount", REG_INT,   0, 3),
  RegBlockTemp("Tracker.flexure",     "flexure",     REG_INT,   0, 2),
  RegBlockTemp("Tracker.tilts",       "tilts",       REG_INT,   0, 3),
  RegBlockTemp("Tracker.collimation", "collimation", REG_INT,   0, 2),
  RegBlockTemp("Tracker.encoder_off", "encoder_off", REG_INT,   0, 3),
};

RegBlockTemp CarmaDataMapper::szaCaltert_[] = {
  RegBlockTemp("Caltert.tertStateMask",   "tertStateMask",    REG_SHORT,   0, 1),
  RegBlockTemp("Caltert.calibTemp",       "calibTemp",        REG_INT,     0, 1),
  RegBlockTemp("Caltert.posnCode",        "posnCode",         REG_SHORT,   0, 1),
  RegBlockTemp("Caltert.encPos",          "encPos",           REG_INT,     0, 1),
};

RegBlockTemp CarmaDataMapper::szaThermal_[] = {
  RegBlockTemp("Thermal.eboxTemperature",    "eboxTemperature",     REG_SHORT,   0, 1),
  RegBlockTemp("Thermal.eboxSetTemperature", "eboxSetTemperature",  REG_SHORT,   0, 1),
};

RegBlockTemp CarmaDataMapper::szaIfmod_[] = {
  RegBlockTemp("Ifmod.ifTotalPower",   "ifTotalPower",   REG_FLOAT,   0, 1),
  RegBlockTemp("Ifmod.inputAtten",     "inputAtten",     REG_FLOAT,   0, 1),
  RegBlockTemp("Ifmod.outputAtten",    "outputAtten",    REG_FLOAT,   0, 1),
};

RegBlockTemp CarmaDataMapper::szaYig_[] = {
  RegBlockTemp("Yig.lockStateMask",   "lockStateMask",   REG_SHORT,   0, 1),
};

RegBlockTemp CarmaDataMapper::szaVaractor_[] = {
  RegBlockTemp("Varactor.statusRegisterMask",   "statusRegisterMask",   REG_SHORT,   0, 1),
};

RegBlockTemp CarmaDataMapper::szaRx_[] = {
  RegBlockTemp("Rx.tempSensor",             "tempSensor",             REG_SHORT,   0, 3),
  RegBlockTemp("Rx.drainCurrent30GHz",      "drainCurrent30GHz",      REG_SHORT,   0, 4),
  RegBlockTemp("Rx.gateCurrent90GHz",      "gateCurrent90GHz",        REG_SHORT,   0, 4),
  RegBlockTemp("Rx.drainCurrent90GHz",      "drainCurrent90GHz",      REG_SHORT,   0, 2),
  RegBlockTemp("Rx.ifAmpDrainCurrent90GHz", "ifAmpDrainCurrent90GHz", REG_SHORT,   0, 1),
};

RegBlockTemp CarmaDataMapper::szaPmac_[] = {
  RegBlockTemp("Pmac.statusMask",           "statusMask",           REG_SHORT,   0, 1),
  RegBlockTemp("Pmac.mtr_com_i",            "mtr_com_i",            REG_INT,     0, 2),
  RegBlockTemp("Pmac.mtr_stat",             "mtr_stat",             REG_INT,     0, 1),
  RegBlockTemp("Pmac.drive_status",         "drive_status",         REG_INT,     0, 1),
};

/**.......................................................................
 * Define the antenna IF register map.  This is common to all
 * antennas, but not part of AntennaCommon in the CARMA monitor system
 */
RegBlockTemp CarmaDataMapper::carmaAntennaIf_[] = {
  RegBlockTemp("*.AntennaIfContainer.AntennaIF.attenSet", "totalAttenuation",  REG_FLOAT,  0, 1),
  RegBlockTemp("*.AntennaIfContainer.AntennaIF.attenSet", "inputAttenuation",  REG_FLOAT,  0, 1),
  RegBlockTemp("*.AntennaIfContainer.AntennaIF.attenSet", "outputAttenuation", REG_FLOAT,  0, 1),
};

RegBlockTemp CarmaDataMapper::carmaWeather_[] = {
  RegBlockTemp("Weather.ambientTemperature (C)",     "ambientTemperature",     REG_FLOAT,  0, 1),
  RegBlockTemp("Weather.dewpointTemperature (C)",    "dewpointTemperature",    REG_FLOAT,  0, 1),
  RegBlockTemp("Weather.pressure (mBar)",            "pressure",               REG_FLOAT,  0, 1),
  RegBlockTemp("Weather.humidity (0-100)",           "humidity",               REG_FLOAT,  0, 1),
  RegBlockTemp("Weather.windDirection (degrees)",    "windDirection",          REG_FLOAT,  0, 1),
  RegBlockTemp("Weather.windSpeed (mph)",            "windSpeed",              REG_FLOAT,  0, 1),
  RegBlockTemp("Weather.peakWindSpeed (mph)",        "peakWindSpeed",          REG_FLOAT,  0, 1),
  RegBlockTemp("Weather.averageWindSpeed (mph)",     "averageWindSpeed",       REG_FLOAT,  0, 1),
  RegBlockTemp("Weather.averageWindDirection (mph)", "averageWindDirection",   REG_FLOAT,  0, 1),
  RegBlockTemp("Weather.precipWater (mm)",           "precipWater",            REG_FLOAT,  0, 1),
};

RegBlockTemp CarmaDataMapper::carmaDelays_[] = {
  RegBlockTemp("DelayEngine.DelayData*.adjustableDelay",   "adjustableDelay",   REG_FLOAT,  0, nAntTotal_),
  RegBlockTemp("DelayEngine.DelayData*.antennaDelay",      "antennaDelay",      REG_FLOAT,  0, nAntTotal_),
  RegBlockTemp("DelayEngine.DelayData*.axisDelay",         "axisDelay",         REG_FLOAT,  0, nAntTotal_),
  RegBlockTemp("DelayEngine.DelayData*.geometricDelay",    "geometricDelay",    REG_FLOAT,  0, nAntTotal_),
  RegBlockTemp("DelayEngine.DelayData*.heightDelay",       "heightDelay",       REG_FLOAT,  0, nAntTotal_),
  RegBlockTemp("DelayEngine.DelayData*.ionosphericDelay",  "ionosphericDelay",  REG_FLOAT,  0, nAntTotal_),
  RegBlockTemp("DelayEngine.DelayData*.padDelay",          "padDelay",          REG_FLOAT,  0, nAntTotal_),
  RegBlockTemp("DelayEngine.DelayData*.thermalDelay",      "thermalDelay",      REG_FLOAT,  0, nAntTotal_),
  RegBlockTemp("DelayEngine.DelayData*.totalDelay",        "totalDelay",        REG_FLOAT,  0, nAntTotal_),
  RegBlockTemp("DelayEngine.DelayData*.troposphericDelay", "troposphericDelay", REG_FLOAT,  0, nAntTotal_),
};

// Now the template for each type of antenna

RegBoardTemp CarmaDataMapper::szaAntennaBoards_[] = {
  {"drive", carmaCommonDrive_, ARRAY_DIM(carmaCommonDrive_), {0x0},
   "AntennaCommon.drive monitor points"},
  {"point", carmaCommonPoint_, ARRAY_DIM(carmaCommonPoint_), {0x0},
   "AntennaCommon.Drive.point monitor points"},
  {"track", carmaCommonTrack_, ARRAY_DIM(carmaCommonTrack_), {0x0},
   "AntennaCommon.Drive.track monitor points"},
  {"limit", carmaCommonLimit_, ARRAY_DIM(carmaCommonLimit_), {0x0},
   "AntennaCommon.Drive.limit monitor points"},
  {"lo",    carmaCommonLo_,    ARRAY_DIM(carmaCommonLo_),    {0x0},
   "AntennaCommon.lO monitor points"},
  {"crx",   carmaCommonRx_,    ARRAY_DIM(carmaCommonRx_),    {0x0},
   "AntennaCommon.receivers monitor points"},
  {"location", carmaCommonLocation_, ARRAY_DIM(carmaCommonLocation_), {0x0},
   "AntennaCommon.location monitor points"},
  {"pam",   carmaAntennaIf_, ARRAY_DIM(carmaAntennaIf_), {0x0},
   "Antenna IF module monitor points"},

  {"tracker",  szaTracker_,  ARRAY_DIM(szaTracker_),  {0x0},
   "Tracker monitor points"},
  {"caltert",  szaCaltert_,  ARRAY_DIM(szaCaltert_),  {0x0},
   "Caltert monitor points"},
  {"thermal",  szaThermal_,  ARRAY_DIM(szaThermal_),  {0x0},
   "Thermal monitor points"},
  {"ifmod",    szaIfmod_,    ARRAY_DIM(szaIfmod_),    {0x0},
   "Ifmod monitor points"},
  {"yig",      szaYig_,      ARRAY_DIM(szaYig_),      {0x0},
   "Yig monitor points"},
  {"varactor", szaVaractor_, ARRAY_DIM(szaVaractor_), {0x0},
   "Varactor monitor points"},
  {"rx",       szaRx_,       ARRAY_DIM(szaRx_),       {0x0},
   "Rx monitor points"},
  {"pmac",     szaPmac_,     ARRAY_DIM(szaPmac_),     {0x0},
   "Pmac monitor points"},
};

RegBoardTemp CarmaDataMapper::bimaAntennaBoards_[] = {
  {"drive", carmaCommonDrive_, ARRAY_DIM(carmaCommonDrive_), {0x0},
   "AntennaCommon.drive monitor points"},
  {"point", carmaCommonPoint_, ARRAY_DIM(carmaCommonPoint_), {0x0},
   "AntennaCommon.Drive.point monitor points"},
  {"track", carmaCommonTrack_, ARRAY_DIM(carmaCommonTrack_), {0x0},
   "AntennaCommon.Drive.track monitor points"},
  {"limit", carmaCommonLimit_, ARRAY_DIM(carmaCommonLimit_), {0x0},
   "AntennaCommon.Drive.limit monitor points"},
  {"lo",    carmaCommonLo_,    ARRAY_DIM(carmaCommonLo_),    {0x0},
   "AntennaCommon.lO monitor points"},
  {"crx",    carmaCommonRx_,    ARRAY_DIM(carmaCommonRx_),    {0x0},
   "AntennaCommon.receivers monitor points"},
  {"location", carmaCommonLocation_, ARRAY_DIM(carmaCommonLocation_), {0x0},
   "AntennaCommon.location monitor points"},
  {"pam",    carmaAntennaIf_, ARRAY_DIM(carmaAntennaIf_), {0x0},
   "Antenna IF module monitor points"},
};


RegBoardTemp CarmaDataMapper::ovroAntennaBoards_[] = {
  {"drive", carmaCommonDrive_, ARRAY_DIM(carmaCommonDrive_), {0x0},
   "AntennaCommon.drive monitor points"},
  {"point", carmaCommonPoint_, ARRAY_DIM(carmaCommonPoint_), {0x0},
   "AntennaCommon.Drive.point monitor points"},
  {"track", carmaCommonTrack_, ARRAY_DIM(carmaCommonTrack_), {0x0},
   "AntennaCommon.Drive.track monitor points"},
  {"limit", carmaCommonLimit_, ARRAY_DIM(carmaCommonLimit_), {0x0},
   "AntennaCommon.Drive.limit monitor points"},
  {"lo",    carmaCommonLo_,    ARRAY_DIM(carmaCommonLo_),    {0x0},
   "AntennaCommon.lO monitor points"},
  {"crx",    carmaCommonRx_,    ARRAY_DIM(carmaCommonRx_),    {0x0},
   "AntennaCommon.receivers monitor points"},
  {"location", carmaCommonLocation_, ARRAY_DIM(carmaCommonLocation_), {0x0},
   "AntennaCommon.location monitor points"},
  {"pam",   carmaAntennaIf_, ARRAY_DIM(carmaAntennaIf_), {0x0},
   "Antenna IF module monitor points"},
};

/**.......................................................................
 * Create a template for a single antenna
 */
RegTemplate CarmaDataMapper::szaAntennaTemplate_ = {
  szaAntennaBoards_,   ARRAY_DIM(szaAntennaBoards_)
};

/**.......................................................................
 * Create a template for a single antenna
 */
RegTemplate CarmaDataMapper::bimaAntennaTemplate_ = {
  bimaAntennaBoards_,  ARRAY_DIM(bimaAntennaBoards_)
};

/**.......................................................................
 * Create a template for a single antenna
 */
RegTemplate CarmaDataMapper::ovroAntennaTemplate_ = {
  ovroAntennaBoards_,  ARRAY_DIM(ovroAntennaBoards_)
};

// Array template

RegBlockTemp CarmaDataMapper::carmaInfo_[] = {
  RegBlockTemp("The array state",               "state",        REG_UCHAR,  0, 1),
};

RegBoardTemp CarmaDataMapper::carmaArrayBoards_[] = {
  {"delay",   carmaDelays_,  ARRAY_DIM(carmaDelays_),  {0x0}, 
   "Weather information"},
  {"info",    carmaInfo_,    ARRAY_DIM(carmaInfo_),    {0x0}, 
   "Info registers about the array"},
  {"weather", carmaWeather_, ARRAY_DIM(carmaWeather_), {0x0}, 
   "Weather information"},
};

/**.......................................................................
 * Create a template for a single antenna
 */
RegTemplate CarmaDataMapper::carmaArrayTemplate_ = {
  carmaArrayBoards_,   ARRAY_DIM(carmaArrayBoards_)
};

RegBlockTemp CarmaDataMapper::carmaSubarrayInfo_[] = {

  RegBlockTemp("The reference latitude of this subarray",
	       "latitude",  REG_FLOAT, 0, 1),

  RegBlockTemp("The reference longitude of this subarray",
	       "longitude", REG_FLOAT, 0, 1),

  RegBlockTemp("The reference altitude of this subarray",
	       "altitude",  REG_FLOAT, 0, 1),

  RegBlockTemp("The commanded rest frequency (GHz)",
	       "restFreq", REG_DOUBLE, 0, 1),

  RegBlockTemp("The commanded IF frequency (GHz)",
	       "ifFreq", REG_DOUBLE, 0, 1),

  RegBlockTemp("The current source",
	       "source", REG_UCHAR, 0, 30),

  RegBlockTemp("The index of the current object",
	       "currentObsObject", REG_INT, 0, 1),

  RegBlockTemp("The default purpose string",
	       "defaultPurpose", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName1", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName2", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName3", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName4", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName5", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName6", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName7", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName8", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName9", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName10", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName11", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName12", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName13", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName14", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName15", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName16", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName17", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName18", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName19", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName20", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName21", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName22", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName23", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName24", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName25", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName26", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName27", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName28", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName29", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName30", REG_UCHAR, 0, 30),

  RegBlockTemp("The name of an object",
	       "objectName31", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose1", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose2", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose3", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose4", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose5", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose6", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose7", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose8", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose9", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose10", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose11", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose12", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose13", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose14", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose15", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose16", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose17", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose18", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose19", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose20", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose21", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose22", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose23", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose24", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose25", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose26", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose27", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose28", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose29", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose30", REG_UCHAR, 0, 30),

  RegBlockTemp("The purpose of an object",
	       "objectPurpose31", REG_UCHAR, 0, 30),

};

RegBoardTemp CarmaDataMapper::carmaSubarrayBoards_[] = {
  {"info",   carmaSubarrayInfo_,  ARRAY_DIM(carmaSubarrayInfo_),  {0x0}, 
   "Subarray information"},
};

/**.......................................................................
 * Create a template for the correlator
 */
RegTemplate CarmaDataMapper::carmaSubarrayTemplate_ = {
  carmaSubarrayBoards_,   ARRAY_DIM(carmaSubarrayBoards_)
};

/**.......................................................................
 * Define the register map of a single correlator band.
 */
RegBlockTemp CarmaDataMapper::carmaCorrBand_[] = {
  
  // True when data for this band have been received from the correlator
  
  RegBlockTemp("A bitmask of states corresponding to:<br/><ul>"
	       "<li>Bit 0 high -- Data were not received for this band</li>"
	       "<li>Bit 1 high -- Data were received for this band</li></ul>",
	       "received", REG_UCHAR|REG_UNION),

  RegBlockTemp("A bitmask of states corresponding to:<br/><ul>"
	       "<li>Bit 0 high -- Data were not received for this baseline</li>"
	       "<li>Bit 1 high -- Data were received for this baseline</li></ul>",
	       "baselineReceived", REG_UCHAR|REG_UNION, 0, nSlBase_),

  RegBlockTemp("The input source to the correlator",
	       "source", REG_UCHAR, 0, 12),

  RegBlockTemp("Number of samples in the USB integration",
	       "nUsb",   REG_UINT, 0, 1),

  RegBlockTemp("Total integration time, in milliseconds", 
	       "tIntUsb",  REG_FLOAT, 0, 1),

  RegBlockTemp("A complex floating-point register, consisting of (re, im) pairs "
	       "for the upper sideband (USB) cross-correlation.  The first dimension of this register "
	       "indexes the baseline number (i-j), in the order: 0-1, 0-2 ... 0-7, 1-2, 1-3, etc. "
	       "The second dimension indexes the number of frequency channels, from highest to lowest, "
	       "into which this 500 MHz band is divided, corresponding to the values "
	       "stored in the \"frequency\" register of this board",
	       "usb",      REG_PREAVG|REG_COMPLEX|REG_FLOAT, 0, nSlBase_, 
	       nChan_),

  RegBlockTemp("Validity flags for channels of the USB (valid = 1)",
	       "usbValid",   REG_INT, 0, nSlBase_, nChan_),
  
  RegBlockTemp("(re, im) pairs for the USB cross-correlation, averaged over frequency "
	       "channels. First dimension indexes the baseline number",
	       "usbAvg",   REG_PREAVG|REG_COMPLEX|REG_FLOAT, 0, nSlBase_),
  
  RegBlockTemp("The variance of the frequency-averaged USB cross-correlations. "
	       "The first dimension indexes the baseline number",
  	       "usbVar",   REG_PREAVG|REG_COMPLEX|REG_FLOAT, 0, nSlBase_),
  
  RegBlockTemp("Number of samples in the LSB integration",
	       "nLsb",   REG_UINT, 0, 1),

  RegBlockTemp("Total integration time, in milliseconds", 
	       "tIntLsb",  REG_FLOAT, 0, 1),

  RegBlockTemp("A complex floating-point register, consisting of (re, im) pairs "
	       "for the lower sideband (LSB) cross-correlation.  (see documentation for \"usb\")",
	       "lsb",      REG_PREAVG|REG_COMPLEX|REG_FLOAT, 0, nSlBase_, 
	       nChan_),
    
  RegBlockTemp("Validity flags for channels of the LSB (valid = 1)",
	       "lsbValid",   REG_INT, 0, nSlBase_, nChan_),

  RegBlockTemp("(re, im) pairs for the LSB cross-correlation, averaged over frequency "
	       "channels. First dimension indexes the baseline number",
	       "lsbAvg",   REG_PREAVG|REG_COMPLEX|REG_FLOAT, 0, nSlBase_),
  
  RegBlockTemp("The variance of the frequency-averaged LSB cross-correlations. "
	       "The first dimension indexes the baseline number",
	       "lsbVar",   REG_PREAVG|REG_COMPLEX|REG_FLOAT, 0, nSlBase_),

  RegBlockTemp("Number of samples in the autocorrelation",
	       "nAuto",    REG_UINT, 0, 1),

  RegBlockTemp("Total integration time, in milliseconds", 
	       "tIntAuto",  REG_FLOAT, 0, 1),
  
  RegBlockTemp("Autocorrelation.  First dimension indexes antenna number, second indexes frequency channel",
	       "auto",     REG_PREAVG|REG_FLOAT,             0, nSlAnt_, 
	       nChan_),

  RegBlockTemp("Validity flags for channels of the auto correlation (valid = 1)",
	       "autoValid",   REG_INT, 0, nSlAnt_, nChan_),
  
  RegBlockTemp("Autocorrelation, averaged over frequency, one per antnena",
	       "autoAvg",  REG_PREAVG|REG_FLOAT,             0, nSlAnt_),
  
  RegBlockTemp("Variance of the frequency-averaged autocorrelation, one per antenna",
	       "autoVar",  REG_PREAVG|REG_FLOAT,             0, nSlAnt_),
  
  RegBlockTemp("The center frequency of this 500 MHz band (GHz)",
	       "centerFrequency", REG_PREAVG|REG_FLOAT,      0, 1),
  
  RegBlockTemp("The frequency of each channel (GHz)",
	       "frequency",REG_PREAVG|REG_FLOAT,      0, 
	       nChan_),
};

// Info about the correlator data

RegBlockTemp CarmaDataMapper::carmaCorrInfo_[] = {

  RegBlockTemp("The time associated with this frame of correlator data",
	       "utc", REG_UTC, 0, 1),

  RegBlockTemp("The desired integration time of the spectral-line subarray",
	       "slDesiredIntegTime", REG_FLOAT, 0, 1),

  RegBlockTemp("The desired integration time of the spectral-line subarray",
	       "wbDesiredIntegTime", REG_FLOAT, 0, 1),
};


RegBoardTemp CarmaDataMapper::carmaCorrBoards_[] = {

  {"info",   carmaCorrInfo_, ARRAY_DIM(carmaCorrInfo_), {0x0},
   "Information about the integrations"},

  {"band1",  carmaCorrBand_, ARRAY_DIM(carmaCorrBand_), {0x0},
   "Correlator band  1"},

  {"band2",  carmaCorrBand_, ARRAY_DIM(carmaCorrBand_), {0x0},
   "Correlator band  2"},

  {"band3",  carmaCorrBand_, ARRAY_DIM(carmaCorrBand_), {0x0},
   "Correlator band  3"},

  {"band4",  carmaCorrBand_, ARRAY_DIM(carmaCorrBand_), {0x0},
   "Correlator band  4"},

  {"band5",  carmaCorrBand_, ARRAY_DIM(carmaCorrBand_), {0x0},
   "Correlator band  5"},

  {"band6",  carmaCorrBand_, ARRAY_DIM(carmaCorrBand_), {0x0},
   "Correlator band  6"},

  {"band7",  carmaCorrBand_, ARRAY_DIM(carmaCorrBand_), {0x0},
   "Correlator band  7"},

  {"band8",  carmaCorrBand_, ARRAY_DIM(carmaCorrBand_), {0x0},
   "Correlator band  8"},

  {"band9",  carmaCorrBand_, ARRAY_DIM(carmaCorrBand_), {0x0},
   "Correlator band  9"},

  {"band10", carmaCorrBand_, ARRAY_DIM(carmaCorrBand_), {0x0},
   "Correlator band 10"},

  {"band11", carmaCorrBand_, ARRAY_DIM(carmaCorrBand_), {0x0},
   "Correlator band 11"},

  {"band12", carmaCorrBand_, ARRAY_DIM(carmaCorrBand_), {0x0},
   "Correlator band 12"},

  {"band13", carmaCorrBand_, ARRAY_DIM(carmaCorrBand_), {0x0},
   "Correlator band 13"},

  {"band14", carmaCorrBand_, ARRAY_DIM(carmaCorrBand_), {0x0},
   "Correlator band 14"},

  {"band15", carmaCorrBand_, ARRAY_DIM(carmaCorrBand_), {0x0},
   "Correlator band 15"},

  {"band16",  carmaCorrBand_, ARRAY_DIM(carmaCorrBand_), {0x0},
   "Correlator band 16"},
};

/**.......................................................................
 * Create a template for the correlator
 */
RegTemplate CarmaDataMapper::carmaCorrTemplate_ = {
  carmaCorrBoards_,   ARRAY_DIM(carmaCorrBoards_)
};

/**.......................................................................
 * Define the info register map of a single antenna
 */
RegBlockTemp CarmaDataMapper::carmaAntennaInfo_[] = {
  
  // True when data for this band have been received from the correlator
  
  RegBlockTemp("Control.Antenna*.name",                     "name",                  REG_UCHAR,  0, 30),
  RegBlockTemp("Control.Antenna*.carmaAntennaNnumber",      "carmaAntennaNumber",    REG_UCHAR,  0, 1),
  RegBlockTemp("Control.Antenna*.subarrayName",             "subarrayName",          REG_UCHAR,  0, 30),
  RegBlockTemp("Control.Antenna*.subarrayNumber",           "subarrayNumber",        REG_UCHAR,  0, 1),
  RegBlockTemp("Control.Antenna*.correlatorDesignation",    "correlatorDesignation", REG_UCHAR,  0, 30),
  RegBlockTemp("Control.Antenna*.newCorrelatorDesignation", "newCorrelatorDesignation", REG_UCHAR,  0, 30),
  RegBlockTemp("Control.Antenna*.correlatorInputNumber",    "correlatorInputNumber", REG_UINT,   0, 1),
  RegBlockTemp("Control.Antenna*.TotalENU.east",            "east",                  REG_FLOAT,  0, 1),
  RegBlockTemp("Control.Antenna*.TotalENU.north",           "north",                 REG_FLOAT,  0, 1),
  RegBlockTemp("Control.Antenna*.TotalENU.up",              "up",                    REG_FLOAT,  0, 1),
};

RegBoardTemp CarmaDataMapper::carmaControlBoards_[] = {
  {"antenna1" , carmaAntennaInfo_, ARRAY_DIM(carmaAntennaInfo_), {0x0},   "Information about antenna  1"},
  {"antenna2" , carmaAntennaInfo_, ARRAY_DIM(carmaAntennaInfo_), {0x0},   "Information about antenna  2"},
  {"antenna3" , carmaAntennaInfo_, ARRAY_DIM(carmaAntennaInfo_), {0x0},   "Information about antenna  3"},
  {"antenna4" , carmaAntennaInfo_, ARRAY_DIM(carmaAntennaInfo_), {0x0},   "Information about antenna  4"},
  {"antenna5" , carmaAntennaInfo_, ARRAY_DIM(carmaAntennaInfo_), {0x0},   "Information about antenna  5"},
  {"antenna6" , carmaAntennaInfo_, ARRAY_DIM(carmaAntennaInfo_), {0x0},   "Information about antenna  6"},
  {"antenna7" , carmaAntennaInfo_, ARRAY_DIM(carmaAntennaInfo_), {0x0},   "Information about antenna  7"},
  {"antenna8" , carmaAntennaInfo_, ARRAY_DIM(carmaAntennaInfo_), {0x0},   "Information about antenna  8"},
  {"antenna9" , carmaAntennaInfo_, ARRAY_DIM(carmaAntennaInfo_), {0x0},   "Information about antenna  9"},
  {"antenna10", carmaAntennaInfo_, ARRAY_DIM(carmaAntennaInfo_), {0x0},   "Information about antenna 10"},
  {"antenna11", carmaAntennaInfo_, ARRAY_DIM(carmaAntennaInfo_), {0x0},   "Information about antenna 11"},
  {"antenna12", carmaAntennaInfo_, ARRAY_DIM(carmaAntennaInfo_), {0x0},   "Information about antenna 12"},
  {"antenna13", carmaAntennaInfo_, ARRAY_DIM(carmaAntennaInfo_), {0x0},   "Information about antenna 13"},
  {"antenna14", carmaAntennaInfo_, ARRAY_DIM(carmaAntennaInfo_), {0x0},   "Information about antenna 14"},
  {"antenna15", carmaAntennaInfo_, ARRAY_DIM(carmaAntennaInfo_), {0x0},   "Information about antenna 15"},
  {"antenna16", carmaAntennaInfo_, ARRAY_DIM(carmaAntennaInfo_), {0x0},   "Information about antenna 16"},
  {"antenna17", carmaAntennaInfo_, ARRAY_DIM(carmaAntennaInfo_), {0x0},   "Information about antenna 17"},
  {"antenna18", carmaAntennaInfo_, ARRAY_DIM(carmaAntennaInfo_), {0x0},   "Information about antenna 18"},
  {"antenna19", carmaAntennaInfo_, ARRAY_DIM(carmaAntennaInfo_), {0x0},   "Information about antenna 19"},
  {"antenna20", carmaAntennaInfo_, ARRAY_DIM(carmaAntennaInfo_), {0x0},   "Information about antenna 20"},
  {"antenna21", carmaAntennaInfo_, ARRAY_DIM(carmaAntennaInfo_), {0x0},   "Information about antenna 21"},
  {"antenna22", carmaAntennaInfo_, ARRAY_DIM(carmaAntennaInfo_), {0x0},   "Information about antenna 22"},
  {"antenna23", carmaAntennaInfo_, ARRAY_DIM(carmaAntennaInfo_), {0x0},   "Information about antenna 23"},
};

/**.......................................................................
 * Create a template for the 'control' registers
 */
RegTemplate CarmaDataMapper::carmaControlTemplate_ = {
  carmaControlBoards_,   ARRAY_DIM(carmaControlBoards_)
};

//=======================================================================
// Spectral line dwnconverter regs
//=======================================================================

RegBlockTemp CarmaDataMapper::carmaSldcBand_[] = {
  
  // True when data for this band have been received from the correlator
  
  RegBlockTemp("Sldc.Band*.ifOutPower[0-14]", "ifOutPower",  REG_FLOAT,  0, nSlInput_),
  RegBlockTemp("Sldc.Band*.psys[0-14]",       "psys",        REG_FLOAT,  0, nSlInput_),
};

RegBoardTemp CarmaDataMapper::carmaSldcBoards_[] = {
  {"band1" , carmaSldcBand_, ARRAY_DIM(carmaSldcBand_), {0x0},   "Information about band 1"},
  {"band2" , carmaSldcBand_, ARRAY_DIM(carmaSldcBand_), {0x0},   "Information about band 2"},
  {"band3" , carmaSldcBand_, ARRAY_DIM(carmaSldcBand_), {0x0},   "Information about band 3"},
  {"band4" , carmaSldcBand_, ARRAY_DIM(carmaSldcBand_), {0x0},   "Information about band 4"},
  {"band5" , carmaSldcBand_, ARRAY_DIM(carmaSldcBand_), {0x0},   "Information about band 5"},
  {"band6" , carmaSldcBand_, ARRAY_DIM(carmaSldcBand_), {0x0},   "Information about band 6"},
  {"band7" , carmaSldcBand_, ARRAY_DIM(carmaSldcBand_), {0x0},   "Information about band 7"},
  {"band8" , carmaSldcBand_, ARRAY_DIM(carmaSldcBand_), {0x0},   "Information about band 8"},
};

/**.......................................................................
 * Create a template for the spectral line downconverter
 */
RegTemplate CarmaDataMapper::carmaSldcTemplate_ = {
  carmaSldcBoards_,   ARRAY_DIM(carmaSldcBoards_)
};

//=======================================================================
// Wideband dcon registers
//=======================================================================

RegBlockTemp CarmaDataMapper::carmaWbdcBand_[] = {
  
  // True when data for this band have been received from the correlator
  
  RegBlockTemp("Wbdc.Band*.ifOutPower[0-7]", "ifOutPower",  REG_FLOAT,  0, nWbInput_),
  RegBlockTemp("Wbdc.Band*.psys[0-7]",       "psys",        REG_FLOAT,  0, nWbInput_),
};

RegBoardTemp CarmaDataMapper::carmaWbdcBoards_[] = {
  {"band1"  , carmaWbdcBand_, ARRAY_DIM(carmaWbdcBand_), {0x0},   "Information about band 1"},
  {"band2"  , carmaWbdcBand_, ARRAY_DIM(carmaWbdcBand_), {0x0},   "Information about band 2"},
  {"band3"  , carmaWbdcBand_, ARRAY_DIM(carmaWbdcBand_), {0x0},   "Information about band 3"},
  {"band4"  , carmaWbdcBand_, ARRAY_DIM(carmaWbdcBand_), {0x0},   "Information about band 4"},
  {"band5"  , carmaWbdcBand_, ARRAY_DIM(carmaWbdcBand_), {0x0},   "Information about band 5"},
  {"band6"  , carmaWbdcBand_, ARRAY_DIM(carmaWbdcBand_), {0x0},   "Information about band 6"},
  {"band7"  , carmaWbdcBand_, ARRAY_DIM(carmaWbdcBand_), {0x0},   "Information about band 7"},
  {"band8"  , carmaWbdcBand_, ARRAY_DIM(carmaWbdcBand_), {0x0},   "Information about band 8"},
  {"band9"  , carmaWbdcBand_, ARRAY_DIM(carmaWbdcBand_), {0x0},   "Information about band 9"},
  {"band10" , carmaWbdcBand_, ARRAY_DIM(carmaWbdcBand_), {0x0},   "Information about band 10"},
  {"band11" , carmaWbdcBand_, ARRAY_DIM(carmaWbdcBand_), {0x0},   "Information about band 11"},
  {"band12" , carmaWbdcBand_, ARRAY_DIM(carmaWbdcBand_), {0x0},   "Information about band 12"},
  {"band13" , carmaWbdcBand_, ARRAY_DIM(carmaWbdcBand_), {0x0},   "Information about band 13"},
  {"band14" , carmaWbdcBand_, ARRAY_DIM(carmaWbdcBand_), {0x0},   "Information about band 14"},
  {"band15" , carmaWbdcBand_, ARRAY_DIM(carmaWbdcBand_), {0x0},   "Information about band 15"},
  {"band16" , carmaWbdcBand_, ARRAY_DIM(carmaWbdcBand_), {0x0},   "Information about band 16"},
};

/**.......................................................................
 * Create a template for the wideband downconverter
 */
RegTemplate CarmaDataMapper::carmaWbdcTemplate_ = {
  carmaWbdcBoards_,   ARRAY_DIM(carmaWbdcBoards_)
};

/**.......................................................................
 * Collect all sza templates into an array and give them names.
 */
RegTemp CarmaDataMapper::carmaRegTemplates_[] = {
  {"array",     &carmaArrayTemplate_,    "Boards pertaining to the array"},

  {"bima1",     &bimaAntennaTemplate_,   "Boards of antenna BIMA1"},
  {"bima2",     &bimaAntennaTemplate_,   "Boards of antenna BIMA2"},
  {"bima3",     &bimaAntennaTemplate_,   "Boards of antenna BIMA3"},
  {"bima4",     &bimaAntennaTemplate_,   "Boards of antenna BIMA4"},
  {"bima5",     &bimaAntennaTemplate_,   "Boards of antenna BIMA5"},
  {"bima6",     &bimaAntennaTemplate_,   "Boards of antenna BIMA6"},
  {"bima7",     &bimaAntennaTemplate_,   "Boards of antenna BIMA7"},
  {"bima8",     &bimaAntennaTemplate_,   "Boards of antenna BIMA8"},
  {"bima9",     &bimaAntennaTemplate_,   "Boards of antenna BIMA9"},

  {"ovro1",     &ovroAntennaTemplate_,   "Boards of antenna OVRO1"},
  {"ovro2",     &ovroAntennaTemplate_,   "Boards of antenna OVRO2"},
  {"ovro3",     &ovroAntennaTemplate_,   "Boards of antenna OVRO3"},
  {"ovro4",     &ovroAntennaTemplate_,   "Boards of antenna OVRO4"},
  {"ovro5",     &ovroAntennaTemplate_,   "Boards of antenna OVRO5"},
  {"ovro6",     &ovroAntennaTemplate_,   "Boards of antenna OVRO6"},

  {"sza1",      &szaAntennaTemplate_,    "Boards of antenna SZA1"},
  {"sza2",      &szaAntennaTemplate_,    "Boards of antenna SZA2"},
  {"sza3",      &szaAntennaTemplate_,    "Boards of antenna SZA3"},
  {"sza4",      &szaAntennaTemplate_,    "Boards of antenna SZA4"},
  {"sza5",      &szaAntennaTemplate_,    "Boards of antenna SZA5"},
  {"sza6",      &szaAntennaTemplate_,    "Boards of antenna SZA6"},
  {"sza7",      &szaAntennaTemplate_,    "Boards of antenna SZA7"},
  {"sza8",      &szaAntennaTemplate_,    "Boards of antenna SZA8"},

  {"corr",      &carmaCorrTemplate_,     "Boards of the correlator"},

  {"subarray1", &carmaSubarrayTemplate_, "Boards relating to subarray1"},
  {"subarray2", &carmaSubarrayTemplate_, "Boards relating to subarray2"},

  {"control",   &carmaControlTemplate_,  "Boards relating to the whole system"},

  {"sldc",      &carmaSldcTemplate_,     "Boards relating to the spectral line downconverter"},
  {"wbdc",      &carmaWbdcTemplate_,     "Boards relating to the spectral line downconverter"},
};	       

/**.......................................................................
 * Create a template for the whole array
 */
ArrayTemplate CarmaDataMapper::carmaTemplate_ = {
  carmaRegTemplates_,   ARRAY_DIM(carmaRegTemplates_)
};

/**.......................................................................
 * Create the SZA array map.
 *
 * Output:
 *  return    SzaArrayMap *   The SZA array container object.
 */
ArrayMap* CarmaDataMapper::newCarmaArrayMap(void)
{
  return new ArrayMap(&carmaTemplate_);
}

/**.......................................................................
 * Constructor.
 */
CarmaDataMapper::CarmaDataMapper() 
{
  constructCarmaToSzaMap();
  tagIdToCarmaMapInitialized_ = false;
}

/**.......................................................................
 * Destructor.
 */
CarmaDataMapper::~CarmaDataMapper() {}

ArrayTemplate* CarmaDataMapper::getCarmaTemplate()
{
  return &carmaTemplate_;
}

void CarmaDataMapper::printArrayMap()
{
  ArrayMap* arrayMap = newCarmaArrayMap();
  
  for(unsigned iRegMap=0; iRegMap < arrayMap->nregmap; iRegMap++) {
    ArrRegMap* arrregmap = arrayMap->regmaps[iRegMap];
    RegMap* regmap = arrayMap->regmaps[iRegMap]->regmap;
    for(unsigned iBoard=0; iBoard < regmap->nboard_; iBoard++) {
      RegMapBoard* board = regmap->boards_[iBoard];
      for(unsigned iBlock=0; iBlock < board->nblock; iBlock++) {
	RegMapBlock* block = board->blocks[iBlock];
	COUT(arrregmap->name << "." << board->name << "." << block->name_);
      }
    }
  }

  delete arrayMap;
}

void CarmaDataMapper::constructCarmaToTagIdMap(std::string file)
{
  ifstream inFile(file.c_str());

  if (!inFile) {
    ThrowError("Can't open input file " << file);
  }

  String str;
  std::string s;
  while(getline(inFile, s)) {
    if(isalpha(s[0])) {
      str = s;
      String fullName  = str.findNextInstanceOf(" ", false, "=", true, false);
      fullName.strip(" ");
      string name      = fullName.findNextInstanceOf(" ", false, " ", false, false).str();
      unsigned tag     = str.findNextInstanceOf("=", true, "\n", false).toInt();
      tagIdToCarmaMap_[tag]  = name;
      carmaToTagIdMap_[name] = tag;
    }
  }

  inFile.close();

  tagIdToCarmaMapInitialized_ = true;
}

void CarmaDataMapper::constructCarmaToSzaMap()
{
  // Fully qualified registers

  carmaToSzaMap_["WbPipeline.IntegratorStageContainer.IntegratorStage.desiredIntegTime"] = "corr.info.slDesiredIntegTime";
  carmaToSzaMap_["SlPipeline.IntegratorStageContainer.IntegratorStage.desiredIntegTime"] = "corr.info.wbDesiredIntegTime";

  carmaToSzaMap_["Control.Subarray1.ArrayReference.latitude"]   = "subarray1.info.latitude";
  carmaToSzaMap_["Control.Subarray1.ArrayReference.longitude"]  = "subarray1.info.longitude";
  carmaToSzaMap_["Control.Subarray1.ArrayReference.altitude"]   = "subarray1.info.altitude";
  carmaToSzaMap_["Control.Subarray1.Commands.Freq.restFreq"]    = "subarray1.info.restFreq";
  carmaToSzaMap_["Control.Subarray1.Commands.Freq.IFfreq"]      = "subarray1.info.ifFreq";

  carmaToSzaMap_["Control.Subarray2.ArrayReference.latitude"]   = "subarray2.info.latitude";
  carmaToSzaMap_["Control.Subarray2.ArrayReference.longitude"]  = "subarray2.info.longitude";
  carmaToSzaMap_["Control.Subarray2.ArrayReference.altitude"]   = "subarray2.info.altitude";
  carmaToSzaMap_["Control.Subarray2.Commands.Freq.restFreq"]    = "subarray2.info.restFreq";
  carmaToSzaMap_["Control.Subarray2.Commands.Freq.IFfreq"]      = "subarray2.info.ifFreq";

  carmaToSzaMap_["Control.Subarray1.Obsblock.currentObsObject"] = "subarray1.info.currentObsObject";
  carmaToSzaMap_["Control.Subarray2.Obsblock.currentObsObject"] = "subarray2.info.currentObsObject";
  carmaToSzaMap_["Control.Subarray1.Obsblock.defaultPurpose"]   = "subarray1.info.defaultPurpose";
  carmaToSzaMap_["Control.Subarray2.Obsblock.defaultPurpose"]   = "subarray2.info.defaultPurpose";

  carmaToSzaMap_["Control.Subarray1.source"]   = "subarray1.info.source";
  carmaToSzaMap_["Control.Subarray2.source"]   = "subarray2.info.source";

  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject1.name"]  = "subarray1.info.objectName1";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject2.name"]  = "subarray1.info.objectName2";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject3.name"]  = "subarray1.info.objectName3";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject4.name"]  = "subarray1.info.objectName4";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject5.name"]  = "subarray1.info.objectName5";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject6.name"]  = "subarray1.info.objectName6";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject7.name"]  = "subarray1.info.objectName7";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject8.name"]  = "subarray1.info.objectName8";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject9.name"]  = "subarray1.info.objectName9";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject10.name"] = "subarray1.info.objectName10";

  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject11.name"] = "subarray1.info.objectName11";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject12.name"] = "subarray1.info.objectName12";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject13.name"] = "subarray1.info.objectName13";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject14.name"] = "subarray1.info.objectName14";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject15.name"] = "subarray1.info.objectName15";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject16.name"] = "subarray1.info.objectName16";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject17.name"] = "subarray1.info.objectName17";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject18.name"] = "subarray1.info.objectName18";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject19.name"] = "subarray1.info.objectName19";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject20.name"] = "subarray1.info.objectName20";

  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject21.name"] = "subarray1.info.objectName21";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject22.name"] = "subarray1.info.objectName22";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject23.name"] = "subarray1.info.objectName23";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject24.name"] = "subarray1.info.objectName24";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject25.name"] = "subarray1.info.objectName25";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject26.name"] = "subarray1.info.objectName26";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject27.name"] = "subarray1.info.objectName27";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject28.name"] = "subarray1.info.objectName28";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject29.name"] = "subarray1.info.objectName29";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject30.name"] = "subarray1.info.objectName30";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject31.name"] = "subarray1.info.objectName31";

  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject1.name"]  = "subarray2.info.objectName1";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject2.name"]  = "subarray2.info.objectName2";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject3.name"]  = "subarray2.info.objectName3";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject4.name"]  = "subarray2.info.objectName4";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject5.name"]  = "subarray2.info.objectName5";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject6.name"]  = "subarray2.info.objectName6";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject7.name"]  = "subarray2.info.objectName7";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject8.name"]  = "subarray2.info.objectName8";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject9.name"]  = "subarray2.info.objectName9";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject10.name"] = "subarray2.info.objectName10";

  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject11.name"] = "subarray2.info.objectName11";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject12.name"] = "subarray2.info.objectName12";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject13.name"] = "subarray2.info.objectName13";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject14.name"] = "subarray2.info.objectName14";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject15.name"] = "subarray2.info.objectName15";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject16.name"] = "subarray2.info.objectName16";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject17.name"] = "subarray2.info.objectName17";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject18.name"] = "subarray2.info.objectName18";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject19.name"] = "subarray2.info.objectName19";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject20.name"] = "subarray2.info.objectName20";

  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject21.name"] = "subarray2.info.objectName21";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject22.name"] = "subarray2.info.objectName22";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject23.name"] = "subarray2.info.objectName23";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject24.name"] = "subarray2.info.objectName24";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject25.name"] = "subarray2.info.objectName25";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject26.name"] = "subarray2.info.objectName26";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject27.name"] = "subarray2.info.objectName27";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject28.name"] = "subarray2.info.objectName28";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject29.name"] = "subarray2.info.objectName29";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject30.name"] = "subarray2.info.objectName30";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject31.name"] = "subarray2.info.objectName31";

  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject1.purpose"]   = "subarray1.info.objectPurpose1";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject2.purpose"]   = "subarray1.info.objectPurpose2";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject3.purpose"]   = "subarray1.info.objectPurpose3";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject4.purpose"]   = "subarray1.info.objectPurpose4";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject5.purpose"]   = "subarray1.info.objectPurpose5";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject6.purpose"]   = "subarray1.info.objectPurpose6";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject7.purpose"]   = "subarray1.info.objectPurpose7";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject8.purpose"]   = "subarray1.info.objectPurpose8";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject9.purpose"]   = "subarray1.info.objectPurpose9";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject10.purpose"]  = "subarray1.info.objectPurpose10";

  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject11.purpose"]  = "subarray1.info.objectPurpose11";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject12.purpose"]  = "subarray1.info.objectPurpose12";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject13.purpose"]  = "subarray1.info.objectPurpose13";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject14.purpose"]  = "subarray1.info.objectPurpose14";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject15.purpose"]  = "subarray1.info.objectPurpose15";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject16.purpose"]  = "subarray1.info.objectPurpose16";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject17.purpose"]  = "subarray1.info.objectPurpose17";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject18.purpose"]  = "subarray1.info.objectPurpose18";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject19.purpose"]  = "subarray1.info.objectPurpose19";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject20.purpose"]  = "subarray1.info.objectPurpose20";

  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject21.purpose"]  = "subarray1.info.objectPurpose21";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject22.purpose"]  = "subarray1.info.objectPurpose22";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject23.purpose"]  = "subarray1.info.objectPurpose23";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject24.purpose"]  = "subarray1.info.objectPurpose24";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject25.purpose"]  = "subarray1.info.objectPurpose25";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject26.purpose"]  = "subarray1.info.objectPurpose26";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject27.purpose"]  = "subarray1.info.objectPurpose27";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject28.purpose"]  = "subarray1.info.objectPurpose28";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject29.purpose"]  = "subarray1.info.objectPurpose29";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject30.purpose"]  = "subarray1.info.objectPurpose30";
  carmaToSzaMap_["Control.Subarray1.Obsblock.ObsObject31.purpose"]  = "subarray1.info.objectPurpose31";

  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject1.purpose"]   = "subarray2.info.objectPurpose1";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject2.purpose"]   = "subarray2.info.objectPurpose2";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject3.purpose"]   = "subarray2.info.objectPurpose3";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject4.purpose"]   = "subarray2.info.objectPurpose4";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject5.purpose"]   = "subarray2.info.objectPurpose5";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject6.purpose"]   = "subarray2.info.objectPurpose6";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject7.purpose"]   = "subarray2.info.objectPurpose7";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject8.purpose"]   = "subarray2.info.objectPurpose8";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject9.purpose"]   = "subarray2.info.objectPurpose9";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject10.purpose"]  = "subarray2.info.objectPurpose10";

  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject11.purpose"]  = "subarray2.info.objectPurpose11";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject12.purpose"]  = "subarray2.info.objectPurpose12";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject13.purpose"]  = "subarray2.info.objectPurpose13";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject14.purpose"]  = "subarray2.info.objectPurpose14";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject15.purpose"]  = "subarray2.info.objectPurpose15";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject16.purpose"]  = "subarray2.info.objectPurpose16";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject17.purpose"]  = "subarray2.info.objectPurpose17";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject18.purpose"]  = "subarray2.info.objectPurpose18";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject19.purpose"]  = "subarray2.info.objectPurpose19";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject20.purpose"]  = "subarray2.info.objectPurpose20";

  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject21.purpose"]  = "subarray2.info.objectPurpose21";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject22.purpose"]  = "subarray2.info.objectPurpose22";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject23.purpose"]  = "subarray2.info.objectPurpose23";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject24.purpose"]  = "subarray2.info.objectPurpose24";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject25.purpose"]  = "subarray2.info.objectPurpose25";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject26.purpose"]  = "subarray2.info.objectPurpose26";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject27.purpose"]  = "subarray2.info.objectPurpose27";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject28.purpose"]  = "subarray2.info.objectPurpose28";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject29.purpose"]  = "subarray2.info.objectPurpose29";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject30.purpose"]  = "subarray2.info.objectPurpose30";
  carmaToSzaMap_["Control.Subarray2.Obsblock.ObsObject31.purpose"]  = "subarray2.info.objectPurpose31";

  carmaToSzaMap_["Control.Subarray2.ArrayReference.latitude"]  = "subarray2.info.latitude";
  carmaToSzaMap_["Control.Subarray2.ArrayReference.longitude"] = "subarray2.info.longitude";
  carmaToSzaMap_["Control.Subarray2.ArrayReference.altitude"]  = "subarray2.info.altitude";

  carmaToSzaMap_["Weather.ambientTemperature"]                             = "array.weather.ambientTemperature";
  carmaToSzaMap_["Weather.dewpointTemperature"]                            = "array.weather.dewpointTemperature";
  carmaToSzaMap_["Weather.pressure"]                                       = "array.weather.pressure";
  carmaToSzaMap_["Weather.humidity"]                                       = "array.weather.humidity";
  carmaToSzaMap_["Weather.windDirection"]                                  = "array.weather.windDirection";
  carmaToSzaMap_["Weather.windSpeed"]                                      = "array.weather.windSpeed";
  carmaToSzaMap_["Weather.peakWindSpeed"]                                  = "array.weather.peakWindSpeed";
  carmaToSzaMap_["Weather.averageWindSpeed"]                               = "array.weather.averageWindSpeed";
  carmaToSzaMap_["Weather.averageWindDirection"]                           = "array.weather.averageWindDirection";
  carmaToSzaMap_["Weather.precipWater"]                                    = "array.weather.precipWater";

  // Partially-qualified registers

  carmaToSzaMap_["AntennaCommon.Drive.sourcename"]                 = "drive.sourcename";
  carmaToSzaMap_["AntennaCommon.Drive.rightAscension"]             = "drive.rightAscension";
  carmaToSzaMap_["AntennaCommon.Drive.declination"]                = "drive.declination";
  carmaToSzaMap_["AntennaCommon.Drive.errorSky"]                   = "drive.errorSky";
  carmaToSzaMap_["AntennaCommon.Drive.state"]                      = "drive.state";
  carmaToSzaMap_["AntennaCommon.Drive.safeState"]                  = "drive.safeState";
  carmaToSzaMap_["AntennaCommon.Drive.safeAzLow"]                  = "drive.safeAzLow";
  carmaToSzaMap_["AntennaCommon.Drive.safeAzHigh"]                 = "drive.safeAzHigh";
  carmaToSzaMap_["AntennaCommon.Drive.safeElLow"]                  = "drive.safeElLow";
  carmaToSzaMap_["AntennaCommon.Drive.safeElHigh"]                 = "drive.safeElHigh";
  carmaToSzaMap_["AntennaCommon.Drive.mode"]                       = "drive.mode";
  carmaToSzaMap_["AntennaCommon.Drive.Track.requestedAzimuth"]     = "track.requestedAzimuth";
  carmaToSzaMap_["AntennaCommon.Drive.Track.actualAzimuth"]        = "track.actualAzimuth";
  carmaToSzaMap_["AntennaCommon.Drive.Track.errorAzimuth"]         = "track.errorAzimuth";
  carmaToSzaMap_["AntennaCommon.Drive.Track.errorAzimuthSky"]      = "track.errorAzimuthSky";
  carmaToSzaMap_["AntennaCommon.Drive.Track.azimuthRate"]          = "track.azimuthRate";
  carmaToSzaMap_["AntennaCommon.Drive.Track.requestedElevation"]   = "track.requestedElevation";
  carmaToSzaMap_["AntennaCommon.Drive.Track.actualElevation"]      = "track.actualElevation";
  carmaToSzaMap_["AntennaCommon.Drive.Track.errorElevation"]       = "track.errorElevation";
  carmaToSzaMap_["AntennaCommon.Drive.Track.elevationRate"]        = "track.elevationRate";
  carmaToSzaMap_["AntennaCommon.Drive.Track.wrapLogic"]            = "track.wrapLogic";
  carmaToSzaMap_["AntennaCommon.Drive.Track.emergencyOff"]         = "track.emergencyOff";
  carmaToSzaMap_["AntennaCommon.Drive.Track.manualSwitch"]         = "track.manualSwitch";
  carmaToSzaMap_["AntennaCommon.Drive.Track.trackTolerance"]       = "track.trackTolerance";
  carmaToSzaMap_["AntennaCommon.Drive.Point.mountOffsetAz"]        = "point.mountOffsetAz";
  carmaToSzaMap_["AntennaCommon.Drive.Point.mountOffsetEl"]        = "point.mountOffsetEl";
  carmaToSzaMap_["AntennaCommon.Drive.Point.offsetAz"]             = "point.offsetAz";
  carmaToSzaMap_["AntennaCommon.Drive.Point.offsetEl"]             = "point.offsetEl";
  carmaToSzaMap_["AntennaCommon.Drive.Point.refraction"]           = "point.refraction";
  carmaToSzaMap_["AntennaCommon.Drive.Point.refractionModel"]      = "point.refractionModel";
  carmaToSzaMap_["AntennaCommon.Drive.Point.magnitude"]            = "point.magnitude";
  carmaToSzaMap_["AntennaCommon.Drive.Point.Constants.direction"]  = "point.direction";
  carmaToSzaMap_["AntennaCommon.Drive.Point.Constants.coefChange"] = "point.coefChange";
  carmaToSzaMap_["AntennaCommon.Drive.Point.selectedApert"]        = "point.selectedApert";
  carmaToSzaMap_["AntennaCommon.Drive.Limit.azSwLimit"]            = "limit.azSwLimit";
  carmaToSzaMap_["AntennaCommon.Drive.Limit.elSwLimit"]            = "limit.elSwLimit";
  carmaToSzaMap_["AntennaCommon.Drive.Limit.azHwLimit"]            = "limit.azHwLimit";
  carmaToSzaMap_["AntennaCommon.Drive.Limit.elHwLimit"]            = "limit.elHwLimit";
  carmaToSzaMap_["AntennaCommon.Drive.Limit.azLowSwLimitVal"]      = "limit.azLowSwLimitVal";
  carmaToSzaMap_["AntennaCommon.Drive.Limit.azHighSwLimitVal"]     = "limit.azHighSwLimitVal";
  carmaToSzaMap_["AntennaCommon.Drive.Limit.azLowHwLimitVal"]      = "limit.azLowHwLimitVal";
  carmaToSzaMap_["AntennaCommon.Drive.Limit.azHighHwLimitVal"]     = "limit.azHighHwLimitVal";
  carmaToSzaMap_["AntennaCommon.Drive.Limit.elLowSwLimitVal"]      = "limit.elLowSwLimitVal";
  carmaToSzaMap_["AntennaCommon.Drive.Limit.elHighSwLimitVal"]     = "limit.elHighSwLimitVal";
  carmaToSzaMap_["AntennaCommon.Drive.Limit.elLowHwLimitVal"]      = "limit.elLowHwLimitVal";
  carmaToSzaMap_["AntennaCommon.Drive.Limit.elHighHwLimitVal"]     = "limit.elHighHwLimitVal";
  carmaToSzaMap_["AntennaCommon.LO.oscFreq"]                       = "lo.oscFreq";
  carmaToSzaMap_["AntennaCommon.LO.yigFreq"]                       = "lo.yigFreq";
  carmaToSzaMap_["AntennaCommon.LO.yigIFLevel"]                    = "lo.yigIFLevel";
  carmaToSzaMap_["AntennaCommon.LO.yigError"]                      = "lo.yigError";
  carmaToSzaMap_["AntennaCommon.LO.yigState"]                      = "lo.yigState";
  carmaToSzaMap_["AntennaCommon.LO.yigSweep"]                      = "lo.yigSweep";
  carmaToSzaMap_["AntennaCommon.LO.loFreq"]                        = "lo.loFreq";
  carmaToSzaMap_["AntennaCommon.LO.loSweep"]                       = "lo.loSweep";
  carmaToSzaMap_["AntennaCommon.LO.loState"]                       = "lo.loState";
  carmaToSzaMap_["AntennaCommon.Location.latitude"]                = "location.latitude";
  carmaToSzaMap_["AntennaCommon.Location.longitude"]               = "location.longitude";
  carmaToSzaMap_["AntennaCommon.Location.altitude"]                = "location.altitude";

  carmaToSzaMap_["Tracker.errors[1-3]"]                            = "tracker.errors[0-2]";
  carmaToSzaMap_["Tracker.equat_geoc[1-3]"]                        = "tracker.equat_geoc[0-2]";
  carmaToSzaMap_["Tracker.lst"]                                    = "tracker.lst";
  carmaToSzaMap_["Tracker.actual[1-3]"]                            = "tracker.actual[0-2]";
  carmaToSzaMap_["Tracker.lacking"]                                = "tracker.lacking";
  carmaToSzaMap_["Tracker.source"]                                 = "tracker.source";
  carmaToSzaMap_["Tracker.sky_xy_off[1-2]"]                        = "tracker.sky_xy_off[0-1]";
  carmaToSzaMap_["Tracker.stateMask"]                              = "tracker.stateMask";
  carmaToSzaMap_["Tracker.location[1-3]"]                          = "tracker.location[0-2]";
  carmaToSzaMap_["Tracker.horiz_topo[1-3]"]                        = "tracker.horiz_topo[0-2]";
  carmaToSzaMap_["Tracker.horiz_mount[1-3]"]                       = "tracker.horiz_mount[0-2]";
  carmaToSzaMap_["Tracker.flexure[1-2]"]                           = "tracker.flexure[0-1]";
  carmaToSzaMap_["Tracker.tilts[1-3]"]                             = "tracker.tilts[0-2]";
  carmaToSzaMap_["Tracker.collimation[1-2]"]                       = "tracker.collimation[0-1]";
  carmaToSzaMap_["Tracker.encoder_off[0-2]"]                       = "tracker.encoder_off[0-2]";

  carmaToSzaMap_["Caltert.tertStateMask"]                          = "caltert.tertStateMask";
  carmaToSzaMap_["Caltert.calibTemp"]                              = "caltert.calibTemp";
  carmaToSzaMap_["Caltert.posnCode"]                               = "caltert.posnCode";
  carmaToSzaMap_["Caltert.encPos"]                                 = "caltert.encPos";

  carmaToSzaMap_["Thermal.eboxTemperature"]                        = "thermal.eboxTemperature";
  carmaToSzaMap_["Thermal.eboxSetTemperature"]                     = "thermal.eboxSetTemperature";

  carmaToSzaMap_["Ifmod.ifTotalPower"]                             = "ifmod.ifTotalPower";
  carmaToSzaMap_["Ifmod.inputAtten"]                               = "ifmod.inputAtten";
  carmaToSzaMap_["Ifmod.outputAtten"]                              = "ifmod.outputAtten";
  carmaToSzaMap_["Yig.lockStateMask"]                              = "yig.lockStateMask";
  carmaToSzaMap_["Varactor.statusRegisterMask"]                    = "varactor.statusRegisterMask";

  carmaToSzaMap_["Rx.tempSensor[1-3]"]                             = "rx.tempSensor[0-2]";
  carmaToSzaMap_["Rx.drainCurrent30GHz[1-4]"]                      = "rx.drainCurrent30GHz[0-3]";
  carmaToSzaMap_["Rx.drainCurrent90GHz[1-4]"]                      = "rx.drainCurrent90GHz[0-3]";
  carmaToSzaMap_["Rx.ifAmpDrainCurrent90GHz"]                      = "rx.ifAmpDrainCurrent90GHz";

  carmaToSzaMap_["Pmac.statusMask"]                                = "pmac.statusMask";
  carmaToSzaMap_["Pmac.mtr_com_i[1-2]"]                                 = "pmac.mtr_com_i[0-1]";
  carmaToSzaMap_["Pmac.mtr_stat"]                                  = "pmac.mtr_stat";
  carmaToSzaMap_["Pmac.drive_status"]                              = "pmac.drive_status";

  carmaToSzaMap_["AntennaIfContainer1.AntennaIF.attenSet"]         = "pam.attenSet";
  carmaToSzaMap_["AntennaIfContainer1.AntennaIF.setInputAtten"]    = "pam.setInputAtten";
  carmaToSzaMap_["AntennaIfContainer1.AntennaIF.setOutputAtten"]   = "pam.setOutputAtten";

  carmaToSzaMap_["AntennaIfContainer.AntennaIF.attenSet"]          = "pam.attenSet";
  carmaToSzaMap_["AntennaIfContainer.AntennaIF.setInputAtten"]     = "pam.setInputAtten";
  carmaToSzaMap_["AntennaIfContainer.AntennaIF.setOutputAtten"]    = "pam.setOutputAtten";

  // Unqualified registers

  carmaToSzaMap_["name"]                                  = "name";
  carmaToSzaMap_["carmaAntennaNumber"]                    = "carmaAntennaNumber";
  carmaToSzaMap_["subarrayName"]                          = "subarrayName";
  carmaToSzaMap_["subarrayNumber"]                        = "subarrayNumber";
  carmaToSzaMap_["correlatorDesignation"]                 = "correlatorDesignation";
  carmaToSzaMap_["newCorrelatorDesignation"]              = "newCorrelatorDesignation";
  carmaToSzaMap_["correlatorInputNumber"]                 = "correlatorInputNumber";
  carmaToSzaMap_["TotalENU.east"]                         = "east";
  carmaToSzaMap_["TotalENU.north"]                        = "north";
  carmaToSzaMap_["TotalENU.up"]                           = "up";

  // Indexed regs:

  carmaToSzaMap_["Sldc.Band[1-8].Input[1-15].ifOutPower"] = "sldc.band[1-8].ifOutPower[0-14]";
  carmaToSzaMap_["Wbdc.Band[1-16].Input[1-8].ifOutPower"] = "wbdc.band[1-16].ifOutPower[0-7]";

  carmaToSzaMap_["DelayEngine.DelayData[1-23].adjustableDelay"]   = "array.delay.adjustableDelay[0-22]";
  carmaToSzaMap_["DelayEngine.DelayData[1-23].antennaDelay"]      = "array.delay.antennaDelay[0-22]";
  carmaToSzaMap_["DelayEngine.DelayData[1-23].axisDelay"]         = "array.delay.axisDelay[0-22]";
  carmaToSzaMap_["DelayEngine.DelayData[1-23].geometricDelay"]    = "array.delay.geometricDelay[0-22]";
  carmaToSzaMap_["DelayEngine.DelayData[1-23].heightDelay"]       = "array.delay.heightDelay[0-22]";
  carmaToSzaMap_["DelayEngine.DelayData[1-23].ionosphericDelay"]  = "array.delay.ionosphericDelay[0-22]";
  carmaToSzaMap_["DelayEngine.DelayData[1-23].padDelay"]          = "array.delay.padDelay[0-22]";
  carmaToSzaMap_["DelayEngine.DelayData[1-23].thermalDelay"]      = "array.delay.thermalDelay[0-22]";
  carmaToSzaMap_["DelayEngine.DelayData[1-23].totalDelay"]        = "array.delay.totalDelay[0-22]";
  carmaToSzaMap_["DelayEngine.DelayData[1-23].troposphericDelay"] = "array.delay.troposphericDelay[0-22]";
};

std::map<unsigned, SzaRegister> CarmaDataMapper::getTagIdToSzaRegisterMap(sza::util::ArrayDataFrameManager* fm)
{
  if(!tagIdToCarmaMapInitialized_)
    ThrowError("Tag id map not initialized.  Use: constructCarmaToTagIdMap()");

  std::map<unsigned, SzaRegister> retMap;

  COUT("Abotu to add temp regs");
  addTemplatizedRegs(fm, retMap);
  COUT("Abotu to add pq regs");
  addPartiallyQualifiedRegs(fm, retMap);
  COUT("Abotu to add fq regs");
  addFullyQualifiedRegs(fm, retMap);
  COUT("Abotu to add ind regs");
  addIndexedRegs(fm, retMap);
  COUT("Done");

  return retMap;
}

void CarmaDataMapper::getRanges(std::string str, bool& oneRange, 
				String& pref1, unsigned& start1, unsigned& stop1, 
				String& pref2, unsigned& start2, unsigned& stop2, 
				String& remainder)
{
  String name(str);
  
  // We know we have at least one range
  
  oneRange = true;
  
  pref1 = name.findNextInstanceOf(" ", false, "[", true, true);
  String rng1  = name.findNextInstanceOf(" ", false,  "]", true, true);

  start1 = rng1.findNextInstanceOf(" ", false, "-", true, true).toInt();
  stop1  = rng1.findNextInstanceOf(" ", false, " ", false, true).toInt();

  // See if we have another
  
  remainder = name.findNextInstanceOf(" ", false, " ", false, false);
  
  if(remainder.contains("[")) {
    oneRange = false;
    pref2 = remainder.findNextInstanceOf(" ", false, "[", true, true);
    String rng2  = remainder.findNextInstanceOf(" ", false,  "]", true, true);
    start2 = rng2.findNextInstanceOf(" ", false, "-", true, true).toInt();
    stop2  = rng2.findNextInstanceOf(" ", false, " ", false, true).toInt();

    remainder = remainder.findNextInstanceOf(" ", false, " ", false, false);
  }
  
}

void CarmaDataMapper::addIndexedRegs(sza::util::ArrayDataFrameManager* fm,
				     std::map<unsigned, SzaRegister>& retMap)
{
  // Iterate over monitor points we know about for this register map

  std::map<std::string, std::string>::iterator carmaToSzaIter;
  for(carmaToSzaIter=carmaToSzaMap_.begin(); carmaToSzaIter!=carmaToSzaMap_.end(); carmaToSzaIter++) {

    String szaBrdAndRegName(carmaToSzaIter->second);
    std::string regName;
    std::string brdName;
    std::string blkName;
    
    // Only iterate over map entries where an explicit board was given
    
    if(szaBrdAndRegName.contains(".")) {
      String str1 = szaBrdAndRegName.findNextInstanceOf(" ", false, ".", true, false);
      String str2 = szaBrdAndRegName.findNextInstanceOf(".", true,  ".", true, false);
      String str3 = szaBrdAndRegName.findNextInstanceOf(".", true,  " ", false);
      
      if(str2.size() == 0) {
	continue;

      } else if(!szaBrdAndRegName.contains("[")) {
	continue;
      } else {
	regName = str1.str();
	brdName = str2.str();
	blkName = str3.str();
      }

      bool oneRangeCarma, oneRangeSza;

      String pref1Carma, pref2Carma, remainderCarma;
      String pref1Sza, pref2Sza, remainderSza;

      unsigned start1Carma, stop1Carma, start2Carma, stop2Carma;
      unsigned start1Sza, stop1Sza, start2Sza, stop2Sza;

      getRanges(carmaToSzaIter->first,  oneRangeCarma, pref1Carma, start1Carma, stop1Carma, pref2Carma, start2Carma, stop2Carma, remainderCarma);
      getRanges(carmaToSzaIter->second, oneRangeSza,   pref1Sza,   start1Sza,   stop1Sza,   pref2Sza,   start2Sza,   stop2Sza,   remainderSza);
	
      std::ostringstream carmaName;
      std::ostringstream szaName;
      int szaIndex = 0;

      if(oneRangeCarma) {
	for(unsigned ind1=0; ind1 < stop1Carma-start1Carma+1; ind1++) {
	  carmaName.str("");
	  szaName.str("");
	    
	  carmaName << pref1Carma << (start1Carma + ind1) << remainderCarma;
	  szaName   << pref1Sza; 
	  szaIndex = start1Sza + ind1;

	  //	    COUT(carmaName.str() << " = " << szaName.str());

	  addBlock(carmaName.str(), szaName.str(), szaIndex, fm, retMap);
	}
      } else {
	for(unsigned ind1=0; ind1 < stop1Carma-start1Carma+1; ind1++) {
	  for(unsigned ind2=0; ind2 < stop2Carma-start2Carma+1; ind2++) {
	    carmaName.str("");
	    szaName.str("");
	    
	    carmaName << pref1Carma << (start1Carma + ind1) << pref2Carma << (start2Carma + ind2) << remainderCarma;
	    szaName   << pref1Sza   << (start1Sza + ind1)   << pref2Sza;
	    szaIndex = start2Sza + ind2;

	    //	      COUT(carmaName.str() << " = " << szaName.str());

	    addBlock(carmaName.str(), szaName.str(), szaIndex, fm, retMap);

	  }
	}
      }
    }
  }
}

void CarmaDataMapper::addBlock(std::string carmaName, std::string szaName, unsigned index,
			       sza::util::ArrayDataFrameManager* fm,
			       std::map<unsigned, SzaRegister>& retMap)
{
  try {

    String szaStr(szaName);

    std::string regName = szaStr.findNextInstanceOf(" ", false, ".", true, false).str();
    std::string brdName = szaStr.findNextInstanceOf(".", true,  ".", true, false).str();
    std::string blkName = szaStr.findNextInstanceOf(".", true,  " ", false).str();

    RegMapBlock* block = fm->findReg(regName, brdName, blkName);
		
    // If the block exists in our register map, look up the tag id
    // that corresponds to it
    
    if(block != 0) {
	    
      if(carmaToTagIdMap_.find(carmaName) != carmaToTagIdMap_.end()) {
	unsigned tagId = carmaToTagIdMap_[carmaName];
	
	SzaRegister szaReg;
	
	szaReg.arrRegMap_ = fm->getArrReg(regName);
	szaReg.block_     = block;
	szaReg.index_     = index;

	retMap[tagId]     = szaReg;
      }
    }
  } catch(...) {
  }
}

void CarmaDataMapper::addFullyQualifiedRegs(sza::util::ArrayDataFrameManager* fm,
					    std::map<unsigned, SzaRegister>& retMap)
{
  // Iterate over monitor points we know about for this register map

  std::map<std::string, std::string>::iterator carmaToSzaIter;
  for(carmaToSzaIter=carmaToSzaMap_.begin(); carmaToSzaIter!=carmaToSzaMap_.end(); carmaToSzaIter++) {

    String szaBrdAndRegName(carmaToSzaIter->second);
    std::string regName;
    std::string brdName;
    std::string blkName;
    
    // Only iterate over map entries where an explicit board was given
    
    if(szaBrdAndRegName.contains(".")) {
      String str1 = szaBrdAndRegName.findNextInstanceOf(" ", false, ".", true, false);
      String str2 = szaBrdAndRegName.findNextInstanceOf(".", true,  ".", true, false);
      String str3 = szaBrdAndRegName.findNextInstanceOf(".", true,  " ", false);
      
      // Don't process partially qualified, or indexed regs here.

      if(str2.size() == 0 || szaBrdAndRegName.contains("[")) {
	continue;
      } else {
	regName = str1.str();
	brdName = str2.str();
	blkName = str3.str();
      }

      try {
	  
	RegMapBlock* block = fm->findReg(regName, brdName, blkName);
	  
	// If the block exists in our register map, look up the tag id
	// that corresponds to it
	  
	if(block != 0) {
	    
	  std::string carmaName = carmaToSzaIter->first;
	    
	  if(carmaToTagIdMap_.find(carmaName) != carmaToTagIdMap_.end()) {
	    unsigned tagId = carmaToTagIdMap_[carmaName];
	      
	    SzaRegister szaReg;
	      
	    szaReg.arrRegMap_ = fm->getArrReg(regName);
	    szaReg.block_     = block;
	    retMap[tagId]     = szaReg;
	  }
	}
      } catch(...) {
      }

    }
  }
}

void CarmaDataMapper::addPartiallyQualifiedRegs(sza::util::ArrayDataFrameManager* fm,
						std::map<unsigned, SzaRegister>& retMap)
{
  ArrayMap* arrayMap = fm->arrayMap();
  ostringstream os;

  // These registers are specified by board and register only.  Thus
  // we iterate over register maps to find all matches

  for(unsigned iRegMap=0; iRegMap < arrayMap->nregmap; iRegMap++) {

    ArrRegMap* arrregmap = arrayMap->regmaps[iRegMap];
    std::string regName = String::firstToUpper(arrregmap->name);

    // Iterate over monitor points we know about for this register map

    std::map<std::string, std::string>::iterator carmaToSzaIter;
    for(carmaToSzaIter=carmaToSzaMap_.begin(); carmaToSzaIter!=carmaToSzaMap_.end(); carmaToSzaIter++) {
      String szaBrdAndRegName(carmaToSzaIter->second);

      std::string brdName;
      std::string blkName;

      // Only iterate over map entries where an explicit board was given
      
      if(szaBrdAndRegName.contains(".")) {
	String str1 = szaBrdAndRegName.findNextInstanceOf(" ", false, ".", true, false);
	String str2 = szaBrdAndRegName.findNextInstanceOf(".", true,  ".", true, false);
	String str3 = szaBrdAndRegName.findNextInstanceOf(".", true,  " ", false);

	if(str2.size() > 0)
	  continue;
	else {
	  brdName = str1.str();
	  blkName = str3.str();
	}

	// See if ranges were specified

	if(szaBrdAndRegName.contains("[")) {

	  bool oneRangeCarma, oneRangeSza;
	  
	  String pref1Carma, pref2Carma, remainderCarma;
	  String pref1Sza, pref2Sza, remainderSza;
	  
	  unsigned start1Carma, stop1Carma, start2Carma, stop2Carma;
	  unsigned start1Sza, stop1Sza, start2Sza, stop2Sza;
	  
	  getRanges(carmaToSzaIter->first,  oneRangeCarma, pref1Carma, start1Carma, stop1Carma, pref2Carma, start2Carma, stop2Carma, remainderCarma);
	  getRanges(carmaToSzaIter->second, oneRangeSza,   pref1Sza,   start1Sza,   stop1Sza,   pref2Sza,   start2Sza,   stop2Sza,   remainderSza);

	  std::ostringstream carmaName;
	  std::ostringstream szaName;
	  int szaIndex = 0;
	  
	  if(oneRangeCarma) {
	    for(unsigned ind1=0; ind1 < stop1Carma-start1Carma+1; ind1++) {
	      carmaName.str("");
	      szaName.str("");
	      
	      carmaName << regName << "." << pref1Carma << (start1Carma + ind1) << remainderCarma;
	      szaName   << arrregmap->name << "." << pref1Sza; 
	      szaIndex = start1Sza + ind1;
	      
	      //	    COUT(carmaName.str() << " = " << szaName.str());
	      
	      addBlock(carmaName.str(), szaName.str(), szaIndex, fm, retMap);
	    }
	  } else {
	    for(unsigned ind1=0; ind1 < stop1Carma-start1Carma+1; ind1++) {
	      for(unsigned ind2=0; ind2 < stop2Carma-start2Carma+1; ind2++) {
		carmaName.str("");
		szaName.str("");
		
		carmaName << regName         << "." << pref1Carma << (start1Carma + ind1) << pref2Carma << (start2Carma + ind2) << remainderCarma;
		szaName   << arrregmap->name << "." << pref1Sza   << (start1Sza + ind1)   << pref2Sza;
		szaIndex = start2Sza + ind2;
		
		//	      COUT(carmaName.str() << " = " << szaName.str());
		
		addBlock(carmaName.str(), szaName.str(), szaIndex, fm, retMap);
	      }
	    }
	    
	  }
	} else {
	  
	  try {
	    RegMapBlock* block = fm->findReg(arrregmap->name, brdName, blkName);
	    
	    // If the block exists in our register map, look up the tag id
	    // that corresponds to it
	  
	    if(block != 0) {
	    
	      // If only a block name was specified, construct the
	      // hierarchical CARMA name from the SZA name
	    
	      os.str("");
	      os << regName << "." << carmaToSzaIter->first;
	    
	      std::string carmaName = os.str();
	    
	      if(carmaToTagIdMap_.find(carmaName) != carmaToTagIdMap_.end()) {
		unsigned tagId = carmaToTagIdMap_[carmaName];
	      
		SzaRegister szaReg;
	      
		szaReg.arrRegMap_ = arrregmap;
		szaReg.block_     = block;
		retMap[tagId] = szaReg;
	      }
	    }
	  } catch(...) {
	  }
	}

      }
    }
  }
}

void CarmaDataMapper::addTemplatizedRegs(sza::util::ArrayDataFrameManager* fm,
					 std::map<unsigned, SzaRegister>& retMap)
{
  ArrayMap* arrayMap = fm->arrayMap();
  ostringstream os;

  for(unsigned iRegMap=0; iRegMap < arrayMap->nregmap; iRegMap++) {

    ArrRegMap* arrregmap = arrayMap->regmaps[iRegMap];
    std::string regName = String::firstToUpper(arrregmap->name);

    // Iterate over monitor points we know about for this register map

    std::map<std::string, std::string>::iterator carmaToSzaIter;
    for(carmaToSzaIter=carmaToSzaMap_.begin(); carmaToSzaIter!=carmaToSzaMap_.end(); carmaToSzaIter++) {
      String szaBrdAndRegName(carmaToSzaIter->second);
      std::string brdName;
      std::string blkName;

      // Only iterate over map entries where no explicit board was given

      if(!szaBrdAndRegName.contains(".")) {

	blkName = szaBrdAndRegName.str();

	for(unsigned iBrd=0; iBrd < arrregmap->regmap->boards_.size(); iBrd++) {
	  brdName = arrregmap->regmap->boards_[iBrd]->name;

	  try {
	    RegMapBlock* block = fm->findReg(arrregmap->name, brdName, blkName);
	    
	    // If the block exists in our register map, look up the tag id
	    // that corresponds to it
	    
	    if(block != 0) {
	      
	      // If only a block name was specified, construct the
	      // hierarchical CARMA name from the SZA name
	      
	      os.str("");
	      os << regName << "." << String::firstToUpper(block->brd_->name) << "." << carmaToSzaIter->first;
	      
	      std::string carmaName = os.str();
	      
	      if(carmaToTagIdMap_.find(carmaName) != carmaToTagIdMap_.end()) {
		unsigned tagId = carmaToTagIdMap_[carmaName];
		
		SzaRegister szaReg;
		
		szaReg.arrRegMap_ = arrregmap;
		szaReg.block_     = block;
		retMap[tagId] = szaReg;
	      }
	    }
	  } catch(...) {
	  }
	}
      }
    }
  }
}

std::ostream& sza::antenna::corba::operator<<(std::ostream& os, SzaRegister& reg)
{
  os << reg.arrRegMap_->name << "." << reg.block_->brd_->name << "." << reg.block_->name_;
  return os;
}
