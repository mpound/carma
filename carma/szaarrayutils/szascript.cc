#include <iostream>

#include <math.h>
#include <string.h>
#include <stdlib.h>

#include "carma/szautil/Angle.h"
#include "carma/szautil/AntNum.h"
#include "carma/szautil/Axis.h"
#include "carma/szautil/CalPos.h"
#include "carma/szautil/CarmaConfig.h"
#include "carma/szautil/DDSChannel.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/HtmlDoc.h"
#include "carma/szautil/IFAtten.h"
#include "carma/szautil/CorrelatorBand.h"
#include "carma/szautil/OffsetMsg.h"
#include "carma/szautil/LoOsc.h"
#include "carma/szautil/RegDescription.h"
#include "carma/szautil/RegParser.h"
#include "carma/szautil/Rx.h"
#include "carma/szautil/TimeVal.h"
#include "carma/szautil/Tracking.h"

#include "carma/antenna/sza/antenna/control/Axis.h"

#include "szacontrol.h"
#include "szascript.h"
#include "szaregs.h"
#include "scheduler.h"
#include "szaconst.h"
#include "navigator.h"
#include "szatypes.h"
#include "arcfile.h"
#include "pathname.h"
#include "grabber.h"
#include "archiver.h"

using namespace sza::array;
using namespace std;

/*
 * Define constructor and destructor functions for schedule-specific
 * project data.
 */
static SC_NEW_FN(new_sch_data);
static SC_CLR_FN(clr_sch_data);
static SC_DEL_FN(del_sch_data);

/*
 * All commands that send messages to the real-time controller, should
 * call rtc_offline() before attempting to send the command.  This
 * function reports an error and returns 1 if the command should be
 * aborted. Some commands will of course get queued before we know
 * that the connection has been lost. These will be silently discarded
 * by the communications thread.
 */
static int rtc_offline(Script *sc, char *cmd);

/* Time and date inquiry functions */

static FUNC_FN(sc_date_fn);
static FUNC_FN(sc_mjd_fn);
static FUNC_FN(sc_time_fn);
static FUNC_FN(sc_today_fn);
static FUNC_FN(sc_tomorrow_fn);
static FUNC_FN(sc_after_fn);
static FUNC_FN(sc_between_fn);
static FUNC_FN(sc_elapsed_fn);
static LOOP_FN(sc_elapsed_loop_fn);
static CMD_FN(sc_update_year_cmd);
static double sc_time_of_day(Script *sc, char *caller, TimeScale type);

/* Real-time register manipulation and acquisition control commands */

static CMD_FN(sc_setreg_cmd);
static FUNC_FN(sc_getreg_fn);
static FUNC_FN(sc_regVal_fn);
static FUNC_FN(sc_intToString_fn);
static FUNC_FN(sc_printToString_fn);
static CMD_FN(sc_unflag_cmd);
static CMD_FN(sc_setdio_cmd);

/* Archive control commands */

static CMD_FN(sc_logdir_cmd);
static CMD_FN(sc_grabdir_cmd);
static CMD_FN(sc_open_cmd);
static CMD_FN(sc_flush_cmd);
static CMD_FN(sc_close_cmd);
static CMD_FN(sc_archive_cmd);
static FUNC_FN(sc_archiving_interval_fn);
static FUNC_FN(sc_archive_filtering_fn);
static CMD_FN(sc_mark_cmd);
static CMD_FN(sc_newFrame_cmd);

/* Commands for shutting down or restarting control system components */

static CMD_FN(sc_load_reboot_script_cmd);

/* Schedule control commands */

static CMD_FN(sc_schedule_cmd);
static CMD_FN(sc_abort_schedule_cmd);
static CMD_FN(sc_remove_schedule_cmd);
static CMD_FN(sc_advance_schedule_cmd);
static CMD_FN(sc_retard_schedule_cmd);
static CMD_FN(sc_suspend_schedule_cmd);
static CMD_FN(sc_resume_schedule_cmd);
static CMD_FN(sc_check_schedule_cmd);

/* Event management commands and functions */

static CMD_FN(sc_add_signals_cmd);
static CMD_FN(sc_signal_cmd);
static FUNC_FN(sc_iteration_fn);
static LOOP_FN(sc_iteration_loop_fn);
static FUNC_FN(sc_acquired_fn);

/* Receiver control commands */

static CMD_FN(sc_selectRx_cmd);
static CMD_FN(sc_setBias_cmd);
static CMD_FN(sc_setDefaultBias_cmd);
static CMD_FN(sc_biasRx_cmd);

// LO commands

static CMD_FN(sc_lo_cmd);
static CMD_FN(sc_setYigFrequency_cmd);
static CMD_FN(sc_setDefaultYigFrequency_cmd);
static CMD_FN(sc_setYigVoltage_cmd);
static CMD_FN(sc_setLoopGainResistance_cmd);
static CMD_FN(sc_setDampingGainResistance_cmd);
static CMD_FN(sc_downloadYigId_cmd);
static CMD_FN(sc_downloadYigTuningTableEntry_cmd);
static CMD_FN(sc_downloadYigTuningTableToOneWire_cmd);
static CMD_FN(sc_enableYigAutoRelock_cmd);
static CMD_FN(sc_setDACCalCoefficient_cmd);

// Bias-tuned Gunn specific commands

static CMD_FN(sc_setGunnVoltage_cmd);
static CMD_FN(sc_setGunnLOFrequency_cmd);
static CMD_FN(sc_setGunnDevice_cmd);
static CMD_FN(sc_jogGunnDevice_cmd);
static CMD_FN(sc_enableGunnAutoRelock_cmd);
static CMD_FN(sc_homeGunnDevice_cmd);

static CMD_FN(sc_setGunnFrequency_cmd);
static CMD_FN(sc_downloadGunnId_cmd);
static CMD_FN(sc_downloadGunnTuningTableEntry_cmd);
static CMD_FN(sc_downloadGunnTuningTableToOneWire_cmd);

/* Site specification commands */

static CMD_FN(sc_site_cmd);

/* Source-catalog commands and functions */

static CMD_FN(sc_track_cmd);
static CMD_FN(sc_slew_cmd);
static CMD_FN(sc_stow_cmd);
static CMD_FN(sc_service_cmd);
static CMD_FN(sc_halt_cmd);
static CMD_FN(sc_show_cmd);
static int output_almanac_time(OutputStream *output, double query_utc,
			       double event_utc);
static FUNC_FN(sc_elevation_fn);
static FUNC_FN(sc_azimuth_fn);
static CMD_FN(sc_catalog_cmd);
static CMD_FN(sc_ut1utc_cmd);
static CMD_FN(sc_horizon_cmd);

/* Scan catalog commands and functions */

static CMD_FN(sc_scan_cmd);
static CMD_FN(sc_scan_catalog_cmd);
static CMD_FN(sc_show_scan_cmd);

// Transaction catalog 

static CMD_FN(sc_loadTransaction_cmd);
static CMD_FN(sc_logTransaction_cmd);
static CMD_FN(sc_transactionEmailAddress_cmd);

/* Pointing model commands */

static CMD_FN(sc_collimate_cmd);
static CMD_FN(sc_encoder_cals_cmd);
static CMD_FN(sc_encoder_zeros_cmd);
static CMD_FN(sc_encoder_limits_cmd);
static CMD_FN(sc_tilts_cmd);
static CMD_FN(sc_flexure_cmd);
static CMD_FN(sc_model_cmd);
static CMD_FN(sc_offset_cmd);
static CMD_FN(sc_radec_offset_cmd);
static CMD_FN(sc_tv_offset_cmd);
static CMD_FN(sc_tv_angle_cmd);
static CMD_FN(sc_sky_offset_cmd);
static CMD_FN(sc_deck_mode_cmd);
static CMD_FN(sc_slew_rate_cmd);

//------------------------------------------------------------
// Old DASI Crap
//------------------------------------------------------------

/* Thermometry acquisition commands */

static CMD_FN(sc_thermo_cmd);
static CMD_FN(sc_thermo_readout_interval_cmd);

/* Optical Camera/Stepper command */

static CMD_FN(sc_optcam_cmd);

/* Frame grabber diagnostic command */

static CMD_FN(sc_configureFrameGrabber_cmd);
static CMD_FN(sc_setOpticalCameraFov_cmd);
static CMD_FN(sc_setOpticalCameraAspect_cmd);
static CMD_FN(sc_setOpticalCameraCollimation_cmd);

/* Command to toggle flat fielding of frame grabber images */

static CMD_FN(sc_flatfield_cmd);

/*
 * Command to return the frame grabber peak offsets and staticstics about the
 * peak pixel.
 */
static FUNC_FN(sc_peak_fn);
static FUNC_FN(sc_imstat_fn);

/*
 * Command to control the auto queue mechanism
 */
static CMD_FN(sc_auto_queue_cmd);

/*
 * Commands to control the pager
 */
static CMD_FN(sc_pager_cmd);
static CMD_FN(sc_pagerEmailAddress_cmd);
static CMD_FN(sc_addPagerRegister_cmd);
static CMD_FN(sc_remPagerRegister_cmd);

//------------------------------------------------------------
// Downconverter control commands
//------------------------------------------------------------

static CMD_FN(sc_psys_cmd);
static CMD_FN(sc_psysAtten_cmd);
static CMD_FN(sc_ifout_cmd);
static CMD_FN(sc_ifoutAtten_cmd);
static CMD_FN(sc_rfamp_cmd);
static CMD_FN(sc_ifalc_cmd);

//------------------------------------------------------------
// Noise Source control commands
//------------------------------------------------------------

static CMD_FN(sc_noisePower_cmd);
static CMD_FN(sc_noiseAtten_cmd);
static CMD_FN(sc_toneAtten_cmd);
static CMD_FN(sc_noiseDiode_cmd);
static CMD_FN(sc_noise_cmd);
static CMD_FN(sc_tone_cmd);

//------------------------------------------------------------
// Quad Mod control commands
//------------------------------------------------------------

static CMD_FN(sc_quadPower_cmd);
static CMD_FN(sc_quadAtten_cmd);
static CMD_FN(sc_quadWalshCol_cmd);
static CMD_FN(sc_quad_cmd);
static CMD_FN(sc_quadWalshTable_cmd);
static CMD_FN(sc_quadPhase_cmd);

//-----------------------------------------------------------------------
// CAN module commands
//-----------------------------------------------------------------------

static CMD_FN(sc_reset_cmd);
static CMD_FN(sc_fast_sampling_cmd);

//-----------------------------------------------------------------------
// Lobe Rotator commands
//-----------------------------------------------------------------------

static CMD_FN(sc_DDS_cmd);
static CMD_FN(sc_setAntennaDDS_cmd);
static CMD_FN(sc_setDDSPhase_cmd);
static CMD_FN(sc_setDDSFreq_cmd);
static CMD_FN(sc_enableDDSWalshing_cmd);
static CMD_FN(sc_setDDSWalshColumn_cmd);
static CMD_FN(sc_setOutputRegs_cmd);
static CMD_FN(sc_setInputPhase_cmd);
static CMD_FN(sc_setInputFreq_cmd);
static CMD_FN(sc_setInputDelay_cmd);
static CMD_FN(sc_setAntennaPhase_cmd);
static CMD_FN(sc_setAntennaFreq_cmd);
static CMD_FN(sc_setAntennaLocation_cmd);
static CMD_FN(sc_enableLrFrequencyOffset_cmd);

//-----------------------------------------------------------------------
// Delay configuration
//-----------------------------------------------------------------------

static CMD_FN(sc_setDelayReference_cmd);
static CMD_FN(sc_setDelay_cmd);
static CMD_FN(sc_setDefaultDelay_cmd);
static CMD_FN(sc_setRefAnt_cmd);
static CMD_FN(sc_useDelay_cmd);
static CMD_FN(sc_fringeTracking_cmd);
static CMD_FN(sc_setNia_cmd);

//-----------------------------------------------------------------------
// Thermal commands
//-----------------------------------------------------------------------

static CMD_FN(sc_setTemperature_cmd);
static CMD_FN(sc_setMode_cmd);
static CMD_FN(sc_setLoopGain_cmd);
static CMD_FN(sc_setIntegConst_cmd);
static CMD_FN(sc_setLoopBw_cmd);
static CMD_FN(sc_setRateConst_cmd);
static CMD_FN(sc_setCircFanPropConst_cmd);
static CMD_FN(sc_setVoltageOffset_cmd);
static CMD_FN(sc_setEboxEqState_cmd);
static CMD_FN(sc_setEboxIntError_cmd);

//-----------------------------------------------------------------------
// Tiltmeter commands
//-----------------------------------------------------------------------

static CMD_FN(sc_setTiltmeterTemperature_cmd);
static CMD_FN(sc_regulateTiltmeterTemperature_cmd);
static CMD_FN(sc_setTiltmeterLoopGain_cmd);
static CMD_FN(sc_setTiltmeterIntegConst_cmd);
static CMD_FN(sc_setTiltmeterRateConst_cmd);
static CMD_FN(sc_setTiltmeterLoopBw_cmd);
static CMD_FN(sc_writeTiltmeterParamsToEeprom_cmd);

//-----------------------------------------------------------------------
// CalTert commands
//-----------------------------------------------------------------------

static CMD_FN(sc_caltertOneWire_cmd);
static CMD_FN(sc_enableTertiary_cmd);
static CMD_FN(sc_homeTertiary_cmd);
static CMD_FN(sc_indexTertiary_cmd);
static CMD_FN(sc_setEncoderPosition_cmd);
static CMD_FN(sc_storeEncoderPosition_cmd);
static CMD_FN(sc_positionCalibrator_cmd);
static CMD_FN(sc_positionTertiary_cmd);
static CMD_FN(sc_resetStepper_cmd);

//-----------------------------------------------------------------------
// antenna IF module commands
//-----------------------------------------------------------------------

static CMD_FN(sc_selectIF_cmd);
static CMD_FN(sc_setIFAtten_cmd);
static CMD_FN(sc_setDefaultIFAtten_cmd);
static CMD_FN(sc_setSkyIFAtten_cmd);
static CMD_FN(sc_setLoadIFAtten_cmd);
static CMD_FN(sc_setIFLevel_cmd);

//-----------------------------------------------------------------------
// IntMod commands
//-----------------------------------------------------------------------

static CMD_FN(sc_setLOTermAtten_cmd);
static CMD_FN(sc_setDefaultLOTermAtten_cmd);

//-----------------------------------------------------------------------
// Synthesizer commands
//-----------------------------------------------------------------------

static CMD_FN(sc_setSynthFrequency_cmd);
static CMD_FN(sc_setSynthPower_cmd);
static CMD_FN(sc_enableSynthOutput_cmd);

//-----------------------------------------------------------------------
// CARMA/SZA array configuration commands
//-----------------------------------------------------------------------

static CMD_FN(sc_setArrayConfiguration_cmd); // Specify a default CARMA/SZA configuration
static CMD_FN(sc_addArrayAntenna_cmd);       // Add a CARMA/SZA antenna to a pad
static CMD_FN(sc_remArrayAntenna_cmd);       // Remove a CARMA/SZA antenna

//-----------------------------------------------------------------------
// General configuration commands
//-----------------------------------------------------------------------

static CMD_FN(sc_setDefaultAntennas_cmd);
static CMD_FN(sc_turnPower_cmd);
static CMD_FN(sc_configureCmdTimeout_cmd);
static CMD_FN(sc_configurePagerAutoEnable_cmd);

/* Host environment commands and functions */

static CMD_FN(sc_cd_cmd);
static CMD_FN(sc_pwd_cmd);

static Variable* add_hostname_variable(Script *sc, char *name);

static CMD_FN(sc_flipDelay_cmd);
static CMD_FN(sc_flipDelayRate_cmd);

static CMD_FN(sc_atmosphere_cmd);

static CMD_FN(sc_autoDoc_cmd);

/*.......................................................................
 * Create a new SZA scripting environment.
 *
 * Input:
 *  cp    ControlProg *  The host control program.
 *  batch         int    True to create an environment for schedulable
 *                       scripts. False to create an environment for
 *                       single-line interactive commands.
 *  signals HashTable *  The symbol table of signals maintained by
 *                       the scheduler.
 * Output:
 *  return     Script *  The new object, or NULL on error.
 */
Script *new_SzaScript(ControlProg *cp, int batch, HashTable *signals)
{
  Script *sc;         /* The object to be returned */
/*
 * Create an empty script environment.
 */
  sc = new_Script(cp, new_sch_data, clr_sch_data, del_sch_data, signals);
  if(!sc)
    return NULL;
/*
 * Add time & date datatypes along with time & date inquiry functions.
 */
  if(!add_IntervalDataType(sc, "Interval") ||
     !add_TimeDataType(sc, "Time") ||
     !add_DateDataType(sc, "Date") ||
     !add_TimeScaleDataType(sc,"TimeScale") ||
     !add_BuiltinFunction(sc,  "Date date()", sc_date_fn) ||
     !add_BuiltinFunction(sc,  "Double mjd()", sc_mjd_fn) ||
     !add_BuiltinFunction(sc,  "Time time(TimeScale scale)", sc_time_fn) ||
     !add_BuiltinFunction(sc,  "Date today()", sc_today_fn) ||
     !add_BuiltinFunction(sc,  "Date tomorrow()", sc_tomorrow_fn) ||
     !add_BuiltinFunction(sc,  "Boolean after(Time time, TimeScale scale)",
			  sc_after_fn) ||
     !add_BuiltinFunction(sc,
		  "Boolean between(Time start, Time end, TimeScale scale)",
		  sc_between_fn) ||
     !add_LoopStateFunction(sc, "Interval elapsed()",
			    sc_elapsed_fn, sc_elapsed_loop_fn, "Date") ||
     !add_BuiltinCommand(sc, "update_year()", sc_update_year_cmd))
    return del_SzaScript(sc);
/*
 * Add real-time register manipulation and acquisition datatypes and commands.
 */
  if(!add_RegisterDataType(sc, "Register", cp_ArrayMap(cp)) ||
     !add_UintDataType(sc, "RegValue", 0, sc_iterate_uint, "Integer") ||
     !add_BoardDataType(sc, "Board", cp_ArrayMap(cp)) ||
     !add_DioBoardDataType(sc, "DioBoard", cp_ArrayMap(cp)) ||
     !add_BitMaskDataType(sc, "BitMask") ||
     !add_BitMaskOperDataType(sc, "BitMaskOper") ||
     !add_BuiltinCommand(sc, "setreg(Register reg, RegValue val)",
			 sc_setreg_cmd) ||
     !add_BuiltinFunction(sc, "RegValue getreg(Register reg)",
			 sc_getreg_fn) ||
     !add_StringDataType(sc, "RegSpec", 0, 0) ||
     !add_BuiltinFunction(sc, "Double regVal(RegSpec reg)",
			 sc_regVal_fn) ||
     !add_BuiltinCommand(sc, "unflag(Board board)", sc_unflag_cmd) ||
     !add_BuiltinFunction(sc, "String intToString(Integer int)",
			 sc_intToString_fn) ||
     !add_BuiltinFunction(sc, "String printToString(Wildcard var)",
			 sc_printToString_fn) ||
     !add_BuiltinCommand(sc,
			 "setdio(DioBoard board, BitMaskOper op, BitMask mask)",
			 sc_setdio_cmd))
    return del_SzaScript(sc);
/*
 * Add archive control datatypes and commands.
 */
  if(!add_WdirDataType(sc, "Wdir") ||
     !add_IntTimeDataType(sc, "IntTime") ||
     !add_UintDataType(sc, "Count", 0, sc_iterate_uint, "Integer") ||
     !add_FeaturesDataType(sc, "Features") ||
     !add_ArcFileDataType(sc, "ArcFile") ||
     !add_FeatureChangeDataType(sc, "FeatureChange") ||

     !add_BuiltinCommand(sc, "logdir(Wdir dir)", sc_logdir_cmd, "Tell the control program where to put subsequent log files") ||
     !add_BuiltinCommand(sc, "grabdir(Wdir dir)", sc_grabdir_cmd, "Tell the control program where to put subsequent frame grabber images") ||
     !add_BuiltinCommand(sc, "open(ArcFile file, [Wdir dir])", sc_open_cmd, "Start recording log|arc|grabber files") ||
     !add_BuiltinCommand(sc, "flush(ArcFile file)", sc_flush_cmd, "Flush buffered data to the current archive or log file") ||
     !add_BuiltinCommand(sc, "close(ArcFile file)", sc_close_cmd, "Close current archive or log file") ||
     !add_BuiltinCommand(sc, "archive([Count combine, Wdir dir, Boolean filter, Count file_size])",
			 sc_archive_cmd, 
			 "Configure the data archiver") ||
     !add_BuiltinFunction(sc, "Count archiving_interval()",
			  sc_archiving_interval_fn) ||
     !add_BuiltinFunction(sc, "Boolean archive_filtering()",
			  sc_archive_filtering_fn) ||
     !add_BuiltinCommand(sc, "mark(FeatureChange what, Features features)",
			 sc_mark_cmd) ||
     !add_BuiltinCommand(sc, "newFrame()", sc_newFrame_cmd))
    return del_SzaScript(sc);
/*
 * Add schedule control commands.
 */
  if(!add_UintDataType(sc, "QueueEntry", 0, sc_iterate_uint, "Integer") ||
     !add_ScriptDataType(sc, "Script") ||
     !add_BuiltinCommand(sc, "load_reboot_script(Script script)",
			 sc_load_reboot_script_cmd) ||
     !add_BuiltinCommand(sc, "schedule(Script script)", sc_schedule_cmd) ||
     !add_BuiltinCommand(sc, "abort_schedule()", sc_abort_schedule_cmd) ||
     !add_BuiltinCommand(sc, "remove_schedule(QueueEntry n)",
			 sc_remove_schedule_cmd) ||
     !add_BuiltinCommand(sc, "advance_schedule(QueueEntry n, Count dn)",
			 sc_advance_schedule_cmd) ||
     !add_BuiltinCommand(sc, "retard_schedule(QueueEntry n, Count dn)",
			 sc_retard_schedule_cmd) ||
     !add_BuiltinCommand(sc, "suspend_schedule()", sc_suspend_schedule_cmd) ||
     !add_BuiltinCommand(sc, "resume_schedule()", sc_resume_schedule_cmd) ||
     !add_BuiltinCommand(sc, "check_schedule(Script script)",
			 sc_check_schedule_cmd))
    return del_SzaScript(sc);

  // Add Antenna datatype

  if(!add_AntennasDataType(sc,      "Antennas"))
    return del_SzaScript(sc);

  // Event management functions.

  if(!add_KeywordDataType(sc, "Keyword", 1, 0) ||
     !add_AcquireTargetsDataType(sc, "AcquireTargets") ||
     !add_BuiltinCommand(sc, "add_signals(listof Keyword keys)",
			 sc_add_signals_cmd, "Define the list of signal names.") ||
     !add_BuiltinCommand(sc, "signal/op=send|clear|init(Signal signal)",
			 sc_signal_cmd) ||
     !add_LoopStateFunction(sc, "Count iteration()",
			    sc_iteration_fn, sc_iteration_loop_fn, "Count") ||
     !add_BuiltinFunction(sc, "Boolean acquired(AcquireTargets targets, "
			  "[Antennas ant])",
			  sc_acquired_fn))
    return del_SzaScript(sc);
  
  // Add receiver-control datatypes.

  if(!add_RxBandDataType(sc,        "RxBand") ||
     !add_RxStageDataType(sc,       "RxStage") ||
     !add_SwitchStateDataType(sc,   "SwitchState") ||
     !add_WalshStagesDataType(sc,   "WalshStages") ||
     !add_WalshFunctionDataType(sc, "WalshFunction") ||
     !add_WalshStepDataType(sc,     "WalshStep") ||
     !add_ReceiversDataType(sc,     "Receivers") ||
     !add_HeaterVoltageDataType(sc, "HeaterVoltage") ||
     !add_HeatersDataType(sc,       "Heaters") ||
     !add_PhaseStepDataType(sc,     "PhaseStep") ||
     !add_PhaseShiftDataType(sc,    "PhaseShift") ||
     !add_PolarStateDataType(sc,    "PolarState") ||
     !add_LoOscDataType(sc,         "LoOsc") ||
     !add_LoStagesDataType(sc,      "LoStages") ||
     !add_LoFrequencyDataType(sc,   "LoFrequency") ||
     !add_QuadPhaseDataType(sc,     "QuadPhase"))
    return del_SzaScript(sc);
  
  // Add receiver-control commands and functions.

  if(!add_BuiltinCommand(sc, "selectRx(RxBand band, [Antennas ant])", sc_selectRx_cmd, 
			 "Set up for observations with the specified receiver") ||
     !add_BuiltinCommand(sc, "setBias(RxStage amp, Integer bias, [Antennas ant])", sc_setBias_cmd, 
			 "Set a bias on a stage of a receiver") ||
     !add_BuiltinCommand(sc, "setDefaultBias(RxStage amp, Integer bias, [Antennas ant])", sc_setDefaultBias_cmd,
			 "Set the default bias to be used when the 'selectRx' or 'biasRx' command is invoked") ||
     !add_BuiltinCommand(sc, "biasRx(RxBand band, [Antennas ant])", sc_biasRx_cmd,
			 "Set all biases for a given receiver") ||
     !add_BuiltinCommand(sc, "configureLO(LoOsc oscillator, LoStages stages, SwitchState state, [Antennas ant])", sc_lo_cmd,
			 "Configure a stage of the LO chain") ||
     !add_BuiltinCommand(sc, "setYigFrequency(LoFrequency frequency, [Antennas ant])", sc_setYigFrequency_cmd,
			 "Set up the YIG frequency.  Initiates a lock sequence on the YIG.") ||
     !add_BuiltinCommand(sc, "setDefaultYigFrequency(LoFrequency frequency, [Antennas ant, RxBand rx])", sc_setDefaultYigFrequency_cmd,
			 "Set the default YIG frequency to be used when the 'selectRx' command is invoked.") ||
     !add_BuiltinCommand(sc, "downloadYigId(Integer id, Integer month, Integer day, Integer year, [Antennas ant])", sc_downloadYigId_cmd,
			 "Download identification information to a YIG module.") ||
     !add_BuiltinCommand(sc, "downloadYigTuningTableEntry(Integer voltage, Integer frequency, [Antennas ant])",
			 sc_downloadYigTuningTableEntry_cmd,
			 "Download a single tuning table entry (voltage vs. frequency) to a YIG module") ||
     !add_BuiltinCommand(sc, "downloadYigTuningTableToOneWire([Antennas ant])", 
			 sc_downloadYigTuningTableToOneWire_cmd,
			 "Tell the YIG module to write its tuning table to the one-wire device") ||
     !add_BuiltinCommand(sc, "setYigVoltage(Integer voltage, [Antennas ant])", 
			 sc_setYigVoltage_cmd) ||
     !add_BuiltinCommand(sc, "enableYigAutoRelock(Boolean enable, [Antennas ant])", 
			 sc_enableYigAutoRelock_cmd) ||
     !add_BuiltinCommand(sc, "setDACCalCoefficient(Double coefficient, [Antennas ant])", sc_setDACCalCoefficient_cmd) ||
     !add_BuiltinCommand(sc, "setEboxEqState(SwitchState state, [Antennas ant])", sc_setEboxEqState_cmd) ||
     !add_BuiltinCommand(sc, "setEboxIntError(Double intError, [Antennas ant])", sc_setEboxIntError_cmd) ||
     !add_BuiltinCommand(sc, "setLoopGainResistance(LoOsc oscillator, Integer loopGain, [Antennas ant])", sc_setLoopGainResistance_cmd) ||
     !add_BuiltinCommand(sc, "setDampingGainResistance(Integer loopGain, [Antennas ant])", sc_setDampingGainResistance_cmd) ||
     !add_BuiltinCommand(sc, "fast_sampling(Integer channel, SwitchState state, [Antennas ant])", sc_fast_sampling_cmd)) 
    return del_SzaScript(sc);

  // Add Bias-tuned Gunn commands and functions.

  if(!add_GunnDeviceDataType(sc, "GunnDevice") ||
     !add_BuiltinCommand(sc, "setGunnVoltage(Integer voltage, [Antennas ant])", sc_setGunnVoltage_cmd) ||
     !add_BuiltinCommand(sc, "setGunnLOFrequency(LoFrequency frequency, [Antennas ant])", sc_setGunnLOFrequency_cmd, 
                         "Set the Gunn frequency without initiating the tuning optimization loop") ||
     !add_BuiltinCommand(sc, "setGunnDevice(GunnDevice device, Integer position, [Antennas ant])", sc_setGunnDevice_cmd) ||
     !add_BuiltinCommand(sc, "jogGunnDevice(GunnDevice device, Integer step, [Antennas ant])", sc_jogGunnDevice_cmd) ||
     !add_BuiltinCommand(sc, "homeGunnDevice(GunnDevice device, [Antennas ant])", sc_homeGunnDevice_cmd) ||
     !add_BuiltinCommand(sc, "enableGunnAutoRelock(Boolean enable, [Antennas ant])", sc_enableGunnAutoRelock_cmd) ||
     !add_BuiltinCommand(sc, "setGunnFrequency(LoFrequency frequency, [Antennas ant])", sc_setGunnFrequency_cmd,
			 "Set the Gunn frequency and initiate the tuning optimization loop") ||
     !add_BuiltinCommand(sc, "downloadGunnId(Integer id, Integer month, Integer day, Integer year, Integer voltage, "
			 "Integer npt, [Antennas ant])", sc_downloadGunnId_cmd) ||
     !add_BuiltinCommand(sc, "downloadGunnTuningTableEntry(LoFrequency freq, Integer tunerPos, Integer backshortPos, Integer attenPos, [Antennas ant])", 
			 sc_downloadGunnTuningTableEntry_cmd) ||
     !add_BuiltinCommand(sc, "downloadGunnTuningTableToOneWire([Antennas ant])", sc_downloadGunnTuningTableToOneWire_cmd))
    return del_SzaScript(sc);

  // Add site-specification datatypes and commands.

  if(!add_LatitudeDataType(sc,  "Latitude") ||
     !add_LongitudeDataType(sc, "Longitude") ||
     !add_AltitudeDataType(sc,  "Altitude") ||
     !add_BuiltinCommand(sc,
	     "site(Longitude longitude, Latitude latitude, Altitude altitude)",
	     sc_site_cmd, "Set the fiducial location of the SZA") ||
     !add_BuiltinCommand(sc, 
			 "setAntennaLocation(Double up, Double east, "
			 "Double north, [Antennas ant])", 
			 sc_setAntennaLocation_cmd) || 
    !add_BuiltinCommand(sc, 
			"setDelayReference(Double up, Double east, Double north)", sc_setDelayReference_cmd,
			"Set a reference position (in meters) for delay calculations"))
    return del_SzaScript(sc);
  
  // Add the commands, functions and datatypes used for interaction
  // with the navigator-thread.

  if(!add_AzimuthDataType(sc, "Azimuth") ||
     !add_DeckAngleDataType(sc, "DeckAngle") ||
     !add_ElevationDataType(sc, "Elevation") ||
     !add_SourceDataType(sc, "Source") ||
     !add_ScanDataType(sc, "Scan") ||
     !add_TrackingDataType(sc, "Tracking") ||
     !add_BuiltinCommand(sc, "track(Source source, [Tracking type, Antennas ant])", 
			 sc_track_cmd) ||
     !add_BuiltinCommand(sc, "slew([Azimuth az, Elevation el, DeckAngle dk, "
			 "Antennas ant])", sc_slew_cmd) ||
     !add_BuiltinCommand(sc, "stow([Antennas ant])", sc_stow_cmd) ||
     !add_BuiltinCommand(sc, "service([Antennas ant])", sc_service_cmd) ||
     !add_BuiltinCommand(sc, "halt([Antennas ant])", sc_halt_cmd) ||
     !add_BuiltinCommand(sc, "show(Source source, "
			 "[Tracking type, Antennas ant, Date utc, "
			 "Elevation horizon])", sc_show_cmd) ||
     !add_BuiltinFunction(sc, "Elevation elevation(Source source)",
			  sc_elevation_fn) ||
     !add_BuiltinFunction(sc, "Azimuth azimuth(Source source)",
			  sc_azimuth_fn) ||
     !add_BuiltinCommand(sc, "catalog(InputFile filename)", sc_catalog_cmd, "Load sources from a source catalog file") ||
     !add_BuiltinCommand(sc, "ut1utc(InputFile filename)", sc_ut1utc_cmd, "Load an ephemeris of UT1-UTC") ||
     !add_BuiltinCommand(sc, "horizon(Elevation angle)", sc_horizon_cmd, "Specify the horizon that is to be assumed by the <a href=commands/show.html>show</a> command") ||
     !add_BuiltinCommand(sc, "scan(Scan scan)", sc_scan_cmd) ||
     !add_BuiltinCommand(sc, "scan_catalog(InputFile filename)", 
			 sc_scan_catalog_cmd) ||
     !add_BuiltinCommand(sc, "show_scan(Scan scan)", sc_show_scan_cmd))
    
    return del_SzaScript(sc);

  // Transaction catalog

  if(!add_TransDevDataType(sc, "TransDev") ||
     !add_TransLocationDataType(sc, "TransLocation") ||
     !add_TransSerialDataType(sc, "TransSerial") ||
     !add_TransSerialDataType(sc, "TransWho") ||
     !add_TransSerialDataType(sc, "TransComment") ||
     !add_EmailActionDataType(sc, "EmailAction") ||
     !add_StringDataType(sc, "Email", 0, 0) ||
     !add_BuiltinCommand(sc, "loadTransaction(InputFile filename, "
			 "[Boolean clear])", sc_loadTransaction_cmd) ||
     !add_BuiltinCommand(sc, "logTransaction(TransDev device, "
			 "TransSerial serial, TransLocation location, "
			 "Date date, TransWho who, [TransComment comment])",
			 sc_logTransaction_cmd),
     !add_BuiltinCommand(sc, "transactionEmailAddress(EmailAction action, [Email email])", 
			 sc_transactionEmailAddress_cmd))

    return del_SzaScript(sc);
  

  // Add pointing-model datatypes and commands.

  if(!add_PointingOffsetDataType(sc, "PointingOffset") ||
     !add_ModelDataType(sc, "Model") ||
     !add_IntDataType(sc, "EncoderCount", 0, sc_iterate_int, "Integer", 1) ||
     !add_FlexureDataType(sc, "Flexure") ||
     !add_TiltDataType(sc, "Tilt") ||
     !add_SlewRateDataType(sc, "SlewRate") ||
     !add_DeckModeDataType(sc, "DeckMode") ||
     !add_BuiltinCommand(sc, "collimate/add(Model model, PointingOffset x, "
			 "PointingOffset y, [Antennas ant])", 
			 sc_collimate_cmd) ||
     !add_BuiltinCommand(sc, "encoder_cals(EncoderCount az_turn, "
			 "EncoderCount el_turn, EncoderCount dk_turn, "
			 "[Antennas ant])", 
			 sc_encoder_cals_cmd) ||
     !add_BuiltinCommand(sc, "encoder_zeros(PointingOffset az, "
			 "PointingOffset el, [Antennas ant])", 
			 sc_encoder_zeros_cmd) ||
     !add_BuiltinCommand(sc, "encoder_limits(EncoderCount az_min,"
			 "EncoderCount az_max, EncoderCount el_min,"
			 "EncoderCount el_max, [Antennas ant])", 
			 sc_encoder_limits_cmd) ||
     !add_BuiltinCommand(sc, "tilts(Tilt ha, Tilt lat, Tilt el,"
			 "[Antennas ant])", sc_tilts_cmd) ||
     !add_BuiltinCommand(sc, "flexure(Model model, Flexure sinEl, Flexure cosEl, [Antennas ant])", 
			 sc_flexure_cmd) ||
     !add_BuiltinCommand(sc, "model(Model model, [Antennas ant])", 
			 sc_model_cmd) ||
     !add_BuiltinCommand(sc, "offset/add([PointingOffset az, PointingOffset el,"
			 "PointingOffset dk, Antennas ant])",
			 sc_offset_cmd) ||
     !add_BuiltinCommand(sc, "radec_offset/add([PointingOffset ra, "
			 "PointingOffset dec, Antennas ant])",
			 sc_radec_offset_cmd) ||
     !add_BuiltinCommand(sc,
			 "tv_offset(PointingOffset right, PointingOffset up,"
			 "[Antennas ant])",
			 sc_tv_offset_cmd) ||
     !add_BuiltinCommand(sc, "tv_angle(PointingOffset angle, [Antennas ant])",
			 sc_tv_angle_cmd) ||
     !add_BuiltinCommand(sc,
			 "sky_offset/add([PointingOffset x, PointingOffset y,"
			 "Antennas ant])",
			 sc_sky_offset_cmd) ||
     !add_BuiltinCommand(sc, "deck_mode(DeckMode mode)", sc_deck_mode_cmd) ||
     !add_BuiltinCommand(sc, "slew_rate([SlewRate az, SlewRate el, "
			 "SlewRate dk, Antennas ant])", sc_slew_rate_cmd))
    return del_SzaScript(sc);
  
  // Add Downconverter commands
  
  if(!add_DcPowerDataType(sc, "DcPower") ||
     !add_AttenuationDataType(sc, "Attenuation") ||
     !add_BandsDataType(sc, "Bands") ||
     !add_BuiltinCommand(sc,"psys(DcPower power, [Antennas ant, Bands band])", 
			 sc_psys_cmd, "Set the downconverter psys power level") ||
     !add_BuiltinCommand(sc,"psysAtten(Double atten, [Antennas ant, Bands band])",
			 sc_psysAtten_cmd, "Set the downconverter psys attenuation level") ||
     !add_BuiltinCommand(sc,"ifout(DcPower power, [Antennas ant, Bands band])", 	 
			 sc_ifout_cmd, "Set the IF output power for the downconverter") ||
     !add_BuiltinCommand(sc,"ifoutAtten(Double atten, [Antennas ant, Bands band])",
			 sc_ifoutAtten_cmd,
			 "Set the IF output power for the downconverter") ||
     !add_BuiltinCommand(sc,"rfamp(SwitchState state, [Antennas ant, Bands band])", 
			 sc_rfamp_cmd,
			 "Turn the RF input amplifier to the downconverter on or off") ||
     !add_BuiltinCommand(sc,"ifalc(SwitchState state, [Antennas ant, Bands band])", 
			 sc_ifalc_cmd,
			 "Enable auto level control of the downconvenrter IF output power"))
    return del_SzaScript(sc);

  // Add Noise Source commands

  if(!add_BuiltinCommand(sc,"noisePower(DcPower power)", 
			 sc_noisePower_cmd, "Set the level of the noise source") ||
     !add_BuiltinCommand(sc,"noiseAtten(Attenuation atten)",
			 sc_noiseAtten_cmd, "Set the noise source attenuation") ||
     !add_BuiltinCommand(sc,"toneAtten(Attenuation atten)", 	 
			 sc_toneAtten_cmd, "Set the tone attenuation") ||
     !add_BuiltinCommand(sc,"noiseDiode(SwitchState state)",
			 sc_noiseDiode_cmd, "Turn the noise source on/off without reconfiguring the delay system") ||
     !add_BuiltinCommand(sc,"noise(SwitchState state)",
			 sc_noise_cmd, "Turn the noise source on/off, and reconfigure the delay system accordingly") ||
     !add_BuiltinCommand(sc,"tone(SwitchState state)",
			 sc_tone_cmd, "Turn the tone source on/off"))
    return del_SzaScript(sc);

  // Add Quad Mod commands

  if(!add_BuiltinCommand(sc,"quadPower(DcPower power, [Antennas ant])", 
			 sc_quadPower_cmd, "Set the output power") ||
     !add_BuiltinCommand(sc,"quadAtten(Attenuation atten, [Antennas ant])", 
			 sc_quadAtten_cmd, "Set the output attenuation") ||
     !add_BuiltinCommand(sc,"quadWalshCol(WalshFunction walsh, [Antennas ant])",			  
			 sc_quadWalshCol_cmd, "Set the walsh column for an antenna") ||
     !add_BuiltinCommand(sc,"quad(SwitchState state, [Antennas ant])",
			 sc_quad_cmd, "Enable/disable the quadrature modulator") ||
     !add_BuiltinCommand(sc,"quadWalshTable([Antennas ant])", 
			 sc_quadWalshTable_cmd) ||
     !add_BuiltinCommand(sc,"quadPhase(QuadPhase phase, [Antennas ant])",
			 sc_quadPhase_cmd,  "Set the phase state of the quad mod"))
    return del_SzaScript(sc);

  // Add CAN Module commands

  if(!add_CanModulesDataType(sc, "CanModules") ||
     !add_BuiltinCommand(sc,
			 "reset(CanModules module, [Antennas ant, Bands band, Boolean hard])", 
			 sc_reset_cmd, "Issue a CAN reset command to one or more modules"))
    return del_SzaScript(sc);

     
     // Thermometry acquisition command.

     if(!add_DsCommandDataType(sc, "DsCommand") ||
     !add_StringDataType(sc, "RomId", 0, 0) ||
     !add_BuiltinCommand(sc, "thermo(DsCommand cmd, [RomId address, Integer index])", sc_thermo_cmd) ||
     !add_BuiltinCommand(sc, "thermo_readout_interval(Interval dt)",
			 sc_thermo_readout_interval_cmd))
    return del_SzaScript(sc);
     
     // Add Optical Camera datatypes and command.

     if(!add_OptCamTargetDataType(sc, "OptCamTarget") ||
      !add_OptCamActionDataType(sc, "OptCamAction") ||
      !add_OptCamCountDataType(sc, "OptCamCount") ||
      !add_BuiltinCommand(sc, "optcam(OptCamTarget target, OptCamCount count, [Antennas ant])",
			  sc_optcam_cmd))
    return del_SzaScript(sc);
     
     // Add frame grabber datatypes and command.

     if(!add_FgRegDataType(sc, "FgReg") ||
	!add_PeakDataType(sc, "Peak") ||
	!add_ImstatDataType(sc, "Imstat") ||
	!add_BuiltinCommand(sc, "flatfield(SwitchState state)", sc_flatfield_cmd) ||
	!add_BuiltinCommand(sc, "configureFrameGrabber([Integer channel, Integer combine, Boolean flatfield])", 
			    sc_configureFrameGrabber_cmd) ||

	!add_BuiltinCommand(sc, "setOpticalCameraFov([Double fov])", 
			    sc_setOpticalCameraFov_cmd) ||
	!add_BuiltinCommand(sc, "setOpticalCameraAspect([Double aspect])", 
			    sc_setOpticalCameraAspect_cmd) ||
	!add_BuiltinCommand(sc, "setOpticalCameraCollimation([PointingOffset collimation])", 
			    sc_setOpticalCameraCollimation_cmd) ||

	!add_BuiltinFunction(sc, "PointingOffset peak(Peak offset)", 
			     sc_peak_fn) ||
	!add_BuiltinFunction(sc, "Double imstat(Imstat stat)", sc_imstat_fn))
       return del_SzaScript(sc);
     
     // Add command for auto_queueing
     
     if(!add_BuiltinCommand(sc, "auto_queue([Wdir dir, SwitchState state, Interval dt])", sc_auto_queue_cmd))
       return del_SzaScript(sc);
     
     // Host environment commands and functions.

     if(!add_DirDataType(sc, "Dir") ||
      !add_StringDataType(sc, "Hostname", 0, 0) ||
      !add_BuiltinCommand(sc, "cd(Dir path)", sc_cd_cmd) ||
      !add_BuiltinCommand(sc, "pwd()", sc_pwd_cmd) ||
      !add_hostname_variable(sc, "hostname"))
     return del_SzaScript(sc);
     
     // Add command for paging

     if(!add_PagerStateDataType(sc, "PagerState") ||
	!add_StringDataType(sc, "Ip", 0, 0) ||
	!add_StringDataType(sc, "RegName", 0, 0) ||
	!add_PagerDevDataType(sc, "PagerDev") ||
	!add_BuiltinCommand(sc, "pager(PagerState state, [Ip ip, PagerDev dev, RegName register, Ip host])", 
			    sc_pager_cmd, "Control the pager"),
	!add_BuiltinCommand(sc, "addPagerRegister(RegName register, Double min, Double max, [Integer nFrame, Boolean delta, Boolean outOfRange, String comment])", 
			    sc_addPagerRegister_cmd, "Add a register condition on which to activate the pager"),
	!add_BuiltinCommand(sc, "remPagerRegister(RegName register)", 
			    sc_remPagerRegister_cmd, "Remove a register condition on which to activate the pager"),
	!add_BuiltinCommand(sc, "pagerEmailAddress(EmailAction action, [Email email])", 
			    sc_pagerEmailAddress_cmd, 
			    "Configure the list of email addresses to be "
			    "notified when the pager is activated"))
       return del_SzaScript(sc);

     // Add commands for handling delays

     if(!add_DelayTypeDataType(sc, "DelayType") ||
	!add_DelayTargetDataType(sc, "DelayTarget") ||
	!add_DDSStateDataType(sc, "DDSState") ||
	!add_DDSChannelDataType(sc, "DDSChannel") ||

	!add_BuiltinCommand(sc, "DDS(DDSState state)", 
			    sc_DDS_cmd, 
			    "Enable/disable output from the DDS chips") ||
	!add_BuiltinCommand(sc, "setAntennaDDS(Antennas ant, DDSChannel channel)", 
			    sc_setAntennaDDS_cmd, 
			    "Establish the mapping between antennas and DDS channels") || 
	!add_BuiltinCommand(sc, "setDDSPhase(DDSChannel channel, Integer phase)", 
			    sc_setDDSPhase_cmd, 
			    "Set the phase (in degrees) on a DDS channel") || 
	!add_BuiltinCommand(sc, "setDDSFreq(DDSChannel channel,Double frequency)", 
			    sc_setDDSFreq_cmd) || 
	!add_BuiltinCommand(sc, "enableDDSWalshing(Boolean on, DDSChannel channel)", 
			    sc_enableDDSWalshing_cmd) || 
	!add_BuiltinCommand(sc, "loadWalshColumn(DDSChannel channel, Integer column)", 
			    sc_setDDSWalshColumn_cmd) || 
	!add_BuiltinCommand(sc, "setOutputRegs(DDSChannel channel, BitMask freg, BitMask preg)", 
			    sc_setOutputRegs_cmd) || 
	!add_BuiltinCommand(sc, "setInputPhase(Integer input, Integer phase)", 
			    sc_setInputPhase_cmd) || 
	!add_BuiltinCommand(sc, "setInputFreq(Integer input, Double frequency)", 
			    sc_setInputFreq_cmd) || 
	!add_BuiltinCommand(sc, "setInputDelay(Integer input, Double delay,"
			    "[Double mjd, Boolean disc])", 
			    sc_setInputDelay_cmd) || 
	!add_BuiltinCommand(sc, "setAntennaPhase(Integer phase, [Antennas ant])", 
			    sc_setAntennaPhase_cmd) || 
	!add_BuiltinCommand(sc, "setAntennaFreq(Double frequency, [Antennas ant])", 
			    sc_setAntennaFreq_cmd) || 
	!add_BuiltinCommand(sc, "setDelay(DelayType delayType, Double delay,"
			    "[Antennas ant])", 
			    sc_setDelay_cmd, "Set the delay for an antenna") ||
	!add_BuiltinCommand(sc, "setNia(Double nia, [Antennas ant])", 
			    sc_setNia_cmd, "Set the el/az axis misalignment term for an antenna") ||
	!add_BuiltinCommand(sc, "setDefaultDelay(DelayType delayType, Double delay, RxBand band, "
			    "[Antennas ant])", 
			    sc_setDefaultDelay_cmd, "Set the default delay (to be used in selectRx) for an antenna") ||
	!add_BuiltinCommand(sc, "setRefAnt(Antennas ant)", 
			    sc_setRefAnt_cmd,
			    "Select an antenna to be the reference position for delay calculations") ||
	!add_BuiltinCommand(sc, "useDelay(DelayType delayType, Boolean use, "
			    "[Antennas ant])", 
			    sc_useDelay_cmd,
			    "Select whether or not a given delay is to be included in delay calculations") ||
	!add_BuiltinCommand(sc, "fringeTracking(SwitchState state, DelayTarget target)", 
			    sc_fringeTracking_cmd,
			    "Toggle sending delays to the correlator and|or lobe rotator") ||
	!add_BuiltinCommand(sc,"enableLrFrequencyOffset(Boolean enable)",
			    sc_enableLrFrequencyOffset_cmd, "Enable the LR frequency offset"))
       return del_SzaScript(sc);
     
     // Add commands for the thermal control module
     
     if(!add_ThermalTargetDataType(sc, "ThermalTarget") ||
	!add_ThermalModeDataType(sc, "ThermalMode") ||
	!add_BuiltinCommand(sc, "setTemperature(ThermalTarget target, Double temperature, [Antennas ant])", 
			    sc_setTemperature_cmd,
			    "A thermal control module command") ||
	!add_BuiltinCommand(sc, "setMode(ThermalTarget target, ThermalMode mode, [Antennas ant])",          
			    sc_setMode_cmd,
			    "A thermal control module command") ||
	!add_BuiltinCommand(sc, "setLoopGain(ThermalTarget target, Double gain, [Antennas ant])",           
			    sc_setLoopGain_cmd,
			    "A thermal control module command") ||
	!add_BuiltinCommand(sc, "setIntegConst(ThermalTarget target, Double const, [Antennas ant])",        
			    sc_setIntegConst_cmd,
			    "A thermal control module command") ||
	!add_BuiltinCommand(sc, "setLoopBw(ThermalTarget target, Double bw, [Antennas ant])",               
			    sc_setLoopBw_cmd,
			    "A thermal control module command") ||
	!add_BuiltinCommand(sc, "setRateConst(ThermalTarget target, Double rate, [Antennas ant])",          
			    sc_setRateConst_cmd,
			    "A thermal control module command") ||
	!add_BuiltinCommand(sc, "setCircFanPropConst(Double const, [Antennas ant])",                        
			    sc_setCircFanPropConst_cmd,
			    "A thermal control module command") ||
	!add_BuiltinCommand(sc, "setVoltageOffset(Double voltageOffset, [Antennas ant])",                        
			    sc_setVoltageOffset_cmd,
			    "A thermal control module command"))
       return del_SzaScript(sc);
	
     // Add commands for the tiltmeter control module

     if(!add_BuiltinCommand(sc, "setTiltmeterTemperature(Double temperature, [Antennas ant])", 
			    sc_setTiltmeterTemperature_cmd,
			    "Set the desired tiltmeter temperature") ||
	!add_BuiltinCommand(sc, "regulateTiltmeterTemperature(ThermalMode mode, [Antennas ant])",          
			    sc_regulateTiltmeterTemperature_cmd,
			    "Configure the tiltmeter thermal control loop") ||
	!add_BuiltinCommand(sc, "setTiltmeterLoopGain(Double gain, [Antennas ant])",           
			    sc_setTiltmeterLoopGain_cmd,
			    "Set the new value for the tiltmeter loop gain. <br>"
			    "This parameter is updated immediately but an additional command "
			    "is required to store it into EEPROM for use upon reset") ||
	!add_BuiltinCommand(sc, "setTiltmeterIntegConst(Double const, [Antennas ant])",        
			    sc_setTiltmeterIntegConst_cmd,
			    "Set the new value for the tiltmeter loop integration constant. <br>"
			    "This parameter is updated immediately but an additional command "
			    "is required to store it into EEPROM for use upon reset") ||
	!add_BuiltinCommand(sc, "setTiltmeterLoopBw(Double bw, [Antennas ant])",               
			    sc_setTiltmeterLoopBw_cmd,
			    "Set the new value for the tiltmeter loop bandwidth. <br>"
			    "This parameter is updated immediately but an additional command "
			    "is required to store it into EEPROM for use upon reset") ||
	!add_BuiltinCommand(sc, "setTiltmeterRateConst(Double rate, [Antennas ant])",          
			    sc_setTiltmeterRateConst_cmd,
			    "Set the new value for the tiltmeter error derivative gain. <br>"
			    "This parameter is updated immediately but an additional command "
			    "is required to store it into EEPROM for use upon reset") ||
	!add_BuiltinCommand(sc, "writeTiltmeterParamsToEeprom([Antennas ant])",                        
			    sc_writeTiltmeterParamsToEeprom_cmd,
			    "Save the tiltmeter thermal control parameters to EEPROM for re-use upon reset"))
       return del_SzaScript(sc);
	

     // Add general configuration commands
     
     if(!add_BuiltinCommand(sc, "setDefaultAntennas(Antennas ant)",
			    sc_setDefaultAntennas_cmd))
       return del_SzaScript(sc);

     // Add general hardware commands
     
     if(!add_OutletDataType(sc,      "Outlet"))
       return del_SzaScript(sc);

     if(!add_BuiltinCommand(sc, "turnPower(SwitchState state, [Outlet outlet, Antennas ant])",
			    sc_turnPower_cmd, "Turn power on/off to an antenna power strip"))
       return del_SzaScript(sc);

     // Add CalTert command
     
     if(!add_CalPosDataType(sc, "CalPos") ||
	!add_TertPosDataType(sc, "TertPos") ||
	!add_CalTertOWDeviceDataType(sc, "CalTertOWDevice") ||
	!add_CalTertOWCommandDataType(sc, "CalTertOWCommand") ||
	!add_BuiltinCommand(sc, "positionCalibrator(CalPos position, [Antennas ant])",
			       sc_positionCalibrator_cmd, "Position the calibrator") ||
	!add_BuiltinCommand(sc, "homeTertiary([Antennas ant])",
			       sc_homeTertiary_cmd, "Home the tertiary") ||
	!add_BuiltinCommand(sc, "positionTertiary(TertPos position, [Antennas ant])",
			       sc_positionTertiary_cmd, "Position the tertiary") ||
	!add_BuiltinCommand(sc, "enableTertiary(Boolean enable, [Antennas ant])",
			       sc_enableTertiary_cmd, "Enable the tertiary") ||
	!add_BuiltinCommand(sc, "resetStepper([Antennas ant])",
			       sc_resetStepper_cmd, "Reset the tertiary stepper motor") ||
	!add_BuiltinCommand(sc, "indexTertiary(TertPos position, [Antennas ant])",
			    sc_indexTertiary_cmd, "Index the current tertiary position") ||
	!add_BuiltinCommand(sc, "setEncoderPosition(RxBand band, TertPos positions, [Antennas ant])",
			    sc_setEncoderPosition_cmd, "Set the tertiary encoder position for a given receiver") ||
	!add_BuiltinCommand(sc, "storeEncoderPosition(RxBand band, TertPos positions, [Antennas ant])",
			    sc_storeEncoderPosition_cmd) ||
	!add_BuiltinCommand(sc, "caltertOneWire(CalTertOWDevice device, CalTertOWCommand command, Antennas ant)",
			    sc_caltertOneWire_cmd, "Interface with a caltert one-wire device"))
       return del_SzaScript(sc);

     // Add Antenna IF modules commands
     
     if(!add_IFLevelDataType(sc, "IFLevel") ||
	!add_IFAttenuationDataType(sc, "IFAttenuation") ||

	!add_BuiltinCommand(sc, "selectIF(RxBand band, [Antennas ant])",
			    sc_selectIF_cmd, 
			    "Set the position of the IF switch") ||

	!add_BuiltinCommand(sc, "setIFAtten([IFAttenuation total, IFAttenuation input, "
			    "IFAttenuation output, Antennas ant])",
			    sc_setIFAtten_cmd, 
			    "Set the IF input/output/total attenuation") ||

	!add_BuiltinCommand(sc, "setSkyIFAtten([Antennas ant])",
			       sc_setSkyIFAtten_cmd, 
			    "Set the IF attenuators to default values against the SKY"
			    " for the currently selected receiver (last selected using the selectRx command)") ||

	!add_BuiltinCommand(sc, "setLoadIFAtten([Antennas ant])",
			    sc_setLoadIFAtten_cmd,
			    "Set the IF attenuators to default values against the LOAD"
			    " for the currently selected receiver (last selected using the selectRx command)") ||

	!add_BuiltinCommand(sc, "setDefaultIFAtten([IFAttenuation total, IFAttenuation input, "
			    "IFAttenuation output, RxBand band, Antennas ant, CalPos pos])",
			    sc_setDefaultIFAtten_cmd) ||
	
	!add_BuiltinCommand(sc, "setIFLevel(IFLevel level, [Antennas ant])",
			    sc_setIFLevel_cmd))

       return del_SzaScript(sc);
     
     // Add Antenna IF modules commands

     if(!add_BuiltinCommand(sc, "setLOTermAtten(Integer atten, [Antennas ant])",
			    sc_setLOTermAtten_cmd) ||
	!add_BuiltinCommand(sc, "setDefaultLOTermAtten(Integer atten, [Antennas ant])",
			    sc_setDefaultLOTermAtten_cmd))
       return del_SzaScript(sc);


     if(!add_BuiltinCommand(sc, "flipDelay(Boolean flip, DelayTarget target)", 
			    sc_flipDelay_cmd, "Flip the sign of the delay to the lobe rotator or correlator") ||
	!add_BuiltinCommand(sc, "flipDelayRate(Boolean flip)", 
			    sc_flipDelayRate_cmd))
       return del_SzaScript(sc);

     if(!add_BuiltinCommand(sc, "atmosphere(Double temperature, "
			    "Double humidity, Double pressure, "
			    "[Antennas ant])", sc_atmosphere_cmd))
       return del_SzaScript(sc);     

     //------------------------------------------------------------
     // Add autodocumentation command
     //------------------------------------------------------------

     if(!add_BuiltinCommand(sc, "autoDoc([Wdir dir])",
			    sc_autoDoc_cmd,
			    "Automatically generate documentation from the control code"))
       return del_SzaScript(sc);
     
     //------------------------------------------------------------
     // Add the deadman configuration command
     //------------------------------------------------------------

     if(!add_BuiltinCommand(sc, "configureCmdTimeout([SwitchState state, "
			    "Interval interval])", 
			    sc_configureCmdTimeout_cmd,
			    "Configure the control system to activate the pager when no commands have been sent"))
       return del_SzaScript(sc);     

     if(!add_BuiltinCommand(sc, "configurePagerAutoEnable([SwitchState state, "
			    "Interval interval])", 
			    sc_configurePagerAutoEnable_cmd,
			    "Configure the control system to re-activate the pager after an interval of time has elapsed"))
       return del_SzaScript(sc);     

     //------------------------------------------------------------
     // Add synthesizer configuration commands
     //------------------------------------------------------------

     if(!add_BuiltinCommand(sc, "setSynthFrequency(Double frequency, String units)",
			    sc_setSynthFrequency_cmd,
			    "Set the synthesizer frequency") ||
	!add_BuiltinCommand(sc, "setSynthPower(Double power)",
			    sc_setSynthPower_cmd,
			    "Set the synthesizer power (in dBm)") ||
	!add_BuiltinCommand(sc, "enableSynthOutput(Boolean enable)",
			    sc_enableSynthOutput_cmd,
			    "Enable the synthesizer RF output"))
       return del_SzaScript(sc);

     //------------------------------------------------------------
     // Add array configuration commands
     //------------------------------------------------------------

     if(!add_ArrayNameDataType(sc, "ArrayName") ||
	!add_ArrayConfigDataType(sc, "ArrayConfig") ||
	!add_AntennaTypeDataType(sc, "AntennaType") ||
     	!add_BuiltinCommand(sc, "setArrayConfiguration(ArrayName array, ArrayConfig config)",
			    sc_setArrayConfiguration_cmd,
			    "Set an array configuration") ||
	!add_BuiltinCommand(sc, "addArrayAntenna(ArrayName array, Integer pad, "
			    "AntennaType type, [Integer ant])",
			    sc_addArrayAntenna_cmd,
			    "Add an antenna to an array configuration") ||
	!add_BuiltinCommand(sc, "remArrayAntenna(ArrayName array, Integer pad, "
			    "[Integer ant])",
			    sc_remArrayAntenna_cmd,
			    "Remove an antenna from an array configuration"))
       return del_SzaScript(sc);

     return sc;
}

/*.......................................................................
 * Delete a SZA Script object.
 *
 * Input:
 *  sc      Script *  The object to be deleted.
 * Output:
 *  return  Script *  The deleted object (always NULL).
 */
Script *del_SzaScript(Script *sc)
{
  return del_Script(sc);
}

/*.......................................................................
 * Report an error if a command destined for the real-time controller
 * is sent when no controller is connected.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  cmd           char *  The name of the command that was about
 *                        to be executed.
 * Output:
 *  return         int    0 - The controller is connected.
 *                        1 - The controller is not connected.
 */
static int rtc_offline(Script *sc, char *cmd)
{
  if(!cp_rtc_online((ControlProg* )sc->project)) {
    lprintf(stderr,
	    "The \"%s\" command was dropped. The controller is offline.\n",
	    cmd);
  };
  return 0;
}

/*-----------------------------------------------------------------------*
 * Time and date inquiry functions                                       *
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Implement a function that returns the current date and time (UTC).
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  args   VariableList *  The list of command-line arguments:
 *                          (none)
 * Input/Output:
 *  result     Variable *  The return Date value.
 *  state      Variable *  Unused.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
static FUNC_FN(sc_date_fn)
{
  double utc;            /* The current UTC as a Modified Julian Date */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, NULL))
    return 1;
/*
 * Get the current UTC as a modified Julian date.
 */
  utc = current_mjd_utc();
  if(utc < 0.0)
    return 1;
/*
 * Return the date and time.
 */
  DOUBLE_VARIABLE(result)->d = utc;
  return 0;
}

/*.......................................................................
 * Implement a function that returns the current MJD
 */
static FUNC_FN(sc_mjd_fn)
{
  double utc;            /* The current UTC as a Modified Julian Date */
  
  // Get the command-line arguments.

  if(get_Arguments(args, NULL))
    return 1;
  
  // Get the current UTC as a modified Julian date.

  utc = current_mjd_utc();
  if(utc < 0.0)
    return 1;
  
  // Return the date and time.

  DOUBLE_VARIABLE(result)->d = utc;

  return 0;
}

/*.......................................................................
 * Implement a function that returns the current time of day for a given
 * timescale.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  args   VariableList *  The list of command-line arguments:
 *                          scale - One of the following timescales:
 *                                   utc - Universal Coordinated time.
 *                                   lst - Local Sidereal Time.
 * Input/Output:
 *  result     Variable *  The return value.
 *  state      Variable *  Unused.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
static FUNC_FN(sc_time_fn)
{
  Variable *vscale;       /* The target timescale */
  double hours;           /* The current time of day (hours) */

  // Get the command-line arguments.

  if(get_Arguments(args, &vscale, NULL))
    return 1;
  
  // Get the current time of day in the specified time-scale.

  hours = sc_time_of_day(sc, "$time()", (TimeScale)CHOICE_VARIABLE(vscale)->choice);

  if(hours < 0)
    return 1;
  
  // Get the time of day in hours and record it for return.

  DOUBLE_VARIABLE(result)->d = hours;

  return 0;
}

/*.......................................................................
 * Implement a function that returns the current date (UTC).
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  args   VariableList *  The list of command-line arguments:
 *                          (none)
 * Input/Output:
 *  result     Variable *  The return value.
 *  state      Variable *  Unused.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
static FUNC_FN(sc_today_fn)
{
  double utc;            /* The current UTC as a Modified Julian Date */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, NULL))
    return 1;
/*
 * Get the current UTC as a modified Julian date.
 */
  utc = current_mjd_utc();
  if(utc < 0.0)
    return 1;
/*
 * Round the returned date to the start of the current day.
 */
  DOUBLE_VARIABLE(result)->d = floor(utc);
  return 0;
}

/*.......................................................................
 * Implement a function that returns tomorrow's date (UTC).
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  args   VariableList *  The list of command-line arguments:
 *                          (none)
 * Input/Output:
 *  result     Variable *  The return value.
 *  state      Variable *  Unused.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
static FUNC_FN(sc_tomorrow_fn)
{
  double utc;            /* The current UTC as a Modified Julian Date */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, NULL))
    return 1;
/*
 * Get the current UTC as a modified Julian date.
 */
  utc = current_mjd_utc();
  if(utc < 0.0)
    return 1;
/*
 * Round the returned date up to the start of the next day.
 */
  DOUBLE_VARIABLE(result)->d = ceil(utc);
  return 0;
}

/*.......................................................................
 * Implement a function that returns true if the current time of day, in
 * a given timescale, is later than a given value. Note that since time
 * wraps around on itself every 24 hours, we define later to mean within
 * the 12 hours following the specified time of day.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  args   VariableList *  The list of command-line arguments:
 *                          time  -  The time of day to compare against.
 *                          scale -  The time-scale to use, from:
 *                                    utc  - Universal Coordinated Time.
 *                                    lst  - Local Sidereal Time.
 * Input/Output:
 *  result     Variable *  The return value.
 *  state      Variable *  Unused.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
static FUNC_FN(sc_after_fn)
{
  Variable *vtime;               /* The time-of-day argument */
  Variable *vscale;              /* The time-scale argument */
  double hours;                  /* The current time of day (hours) */
  double dt;                     /* The difference between the times */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vtime, &vscale, NULL))
    return 1;
/*
 * Get the current time of day in the specified time-scale.
 */
  hours = sc_time_of_day(sc, "$after()", (TimeScale)CHOICE_VARIABLE(vscale)->choice);
  if(hours < 0)
    return 1;
/*
 * Get the time difference modulo 24 hours.
 */

  dt = hours - DOUBLE_VARIABLE(vtime)->d;
  if(dt < 0)
    dt += 24.0;
/*
 * See if the time is within the 12 hours following the specified time.
 */
  BOOL_VARIABLE(result)->boolvar = dt < 12.0;
  return 0;
}

/*.......................................................................
 * Implement a function that returns true if the current time of day,
 * on a given timescale, is between two given times of day. If the second
 * time is at a numerically earlier time of day than the first, it is
 * interpretted as belonging to the following day.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  args   VariableList *  The list of command-line arguments:
 *                          start -  The start time of the window.
 *                          end   -  The end time of the window.
 *                          scale -  The timescale of the start and end
 *                                   times, chosen from:
 *                                     utc  - Universal Coordinated Time.
 *                                     lst  - Local Sidereal Time.
 * Input/Output:
 *  result     Variable *  The return value.
 *  state      Variable *  Unused.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
static FUNC_FN(sc_between_fn)
{
  Variable *va, *vb;   /* The arguments that hold the start and end times */
  Variable *vscale;    /* The argument that contains the timescale */
  double hours;        /* The current time of day (hours) */
  double ta, tb;       /* The two time-of-days to compare to hours */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &va, &vb, &vscale, NULL))
    return 1;
/*
 * Get the current time of day in the specified time-scale.
 */
  hours = sc_time_of_day(sc, "$between()", (TimeScale)CHOICE_VARIABLE(vscale)->choice);
  if(hours < 0)
    return 1;
/*
 * Get the limiting times of the window.
 */
  ta = DOUBLE_VARIABLE(va)->d;
  tb = DOUBLE_VARIABLE(vb)->d;
/*
 * See if the current time is within the specified window.
 *
 * If the second time is numerically later in the day, interpret
 * the times as being in the same day. Otherwise interpret the second
 * as being from the following day.
 */
  if(tb > ta) {
    BOOL_VARIABLE(result)->boolvar = hours > ta && hours < tb;
  } else {
    BOOL_VARIABLE(result)->boolvar = hours > ta || hours < tb;
  };
  return 0;
}

/*.......................................................................
 * Implement a loop-aware function that returns the amount of time that
 * its containing loop has been executing.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  args   VariableList *  The list of command-line arguments:
 *                          (none)
 * Input/Output:
 *  result     Variable *  The return value.
 *  state      Variable *  The loop-state object that contains the
 *                         start time of the loop as a MJD utc.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
static FUNC_FN(sc_elapsed_fn)
{
  double utc;                 /* The current utc expressed as a MJD */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, NULL))
    return 1;
/*
 * Get the current UTC as a modified Julian date.
 */
  utc = current_mjd_utc();
  if(utc < 0.0)
    return 1;
/*
 * Return the time that has elapsed so far (in seconds).
 */
  DOUBLE_VARIABLE(result)->d = (utc - DOUBLE_VARIABLE(state)->d) * 86400.0;
  return 0;
}

/*.......................................................................
 * This is the loop-state iterator function of the elapsed function.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  state      Variable *  The loop-state variable to be initialized.
 *                         No iteration of its value is needed.
 *  oper       LoopOper    The type of operation to perform on *state.
 */
static LOOP_FN(sc_elapsed_loop_fn)
{
/*
 * On entry to the enclosing loop, initialize the state variable
 * with the current UTC, expressed as a MJD.
 */
  switch(oper) {
  case LOOP_ENTER:
    DOUBLE_VARIABLE(state)->d = current_mjd_utc();
    break;
  case LOOP_INCR:
  case LOOP_EXIT:
    break;
  };
}

/*.......................................................................
 * Implement a loop-aware function that returns the iteration count of
 * its enclosing loop or until statement.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  args   VariableList *  The list of command-line arguments:
 *                          (none)
 * Input/Output:
 *  result     Variable *  The return value.
 *  state      Variable *  The loop-state object that contains the
 *                         start time of the loop as a MJD utc.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
static FUNC_FN(sc_iteration_fn)
{
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, NULL))
    return 1;
/*
 * Return the time that has elapsed so far.
 */
  UINT_VARIABLE(result)->uint = UINT_VARIABLE(state)->uint;
  return 0;
}

/*.......................................................................
 * This is the loop-state iterator function of the iteration() function.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  state      Variable *  The loop-state variable to be initialized.
 *                         This countains the iteration count to be
 *                         incremented.
 *  oper       LoopOper    The type of operation to perform on *state.
 */
static LOOP_FN(sc_iteration_loop_fn)
{
  switch(oper) {
  case LOOP_ENTER:   /* Initialize the iteration count on loop entry */
    UINT_VARIABLE(state)->uint = 0;
    break;
  case LOOP_INCR:    /* Increment the iteration count */
    UINT_VARIABLE(state)->uint++;
  case LOOP_EXIT:
    break;
  };
}

/*.......................................................................
 * Implement the command that sends the year to the control system.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments: (none).
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_update_year_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  RtcNetCmd rtc;                 /* The network object to be sent to the */
                                 /*  real-time controller task */
  Date date;                     /* The current date (utc) */

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "update_year"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, NULL))
    return 1;
  
  // Deduce the current utc date.

  if(current_date(&date))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.year.year = date.year;
  
  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_YEAR_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * This is a utility function to return the current time of day in a given
 * format.
 *
 * Input:
 *  sc       Script *   The host scripting environment.
 *  caller     char *   An identification prefix for error messages.
 *  type  TimeScale     The time-system to return.
 */
static double sc_time_of_day(Script *sc, char *caller, TimeScale type)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Date utc;                      /* The current date and time (utc) */
  
  // Get the current date and time (utc).

  if(current_date(&utc))
    return -1.0;
  
  // Get the corresponding time of day on the specified time-scale.

  switch(type) {

  case TIME_UTC:
    return date_to_time_of_day(&utc);
    break;

  case TIME_LST:
    {
      Scheduler *sch = (Scheduler* )cp_ThreadData(cp, CP_SCHEDULER);
      return date_to_lst(&utc, sch_Site(sch), 0.0, 0.0) * rtoh;
    };
    break;
  };

  lprintf(stderr, "%s: Unknown time system.\n", caller);

  return -1.0;
}

/*-----------------------------------------------------------------------*
 * Real-time register manipulation and acquisition control commands      *
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Implement the command that sets a specified SZA register to a specified
 * value.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         reg  -  The register specification.
 *                         val  -  The register value.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setreg_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The context of the
						   host control
						   program */
  Variable *vreg;   /* The register specification argument */
  Variable *vval;   /* The register value argument */
  RegisterVariable *regvar;  /* The derived type of vreg */
  RtcNetCmd rtc;    /* The network command object to be sent to the */
                    /*  real-time controller task */
/*
 * Do we have a controller to send the command to?
 */
  if(rtc_offline(sc, "setreg"))
    return 1;
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vreg, &vval, NULL))
    return 1;
  regvar = REGISTER_VARIABLE(vreg);
/*
 * Compose a network object to be sent to the real-time controller task.
 */
  rtc.cmd.setreg.value = UINT_VARIABLE(vval)->uint;
  rtc.cmd.setreg.board = regvar->board;
  rtc.cmd.setreg.block = regvar->block;
  rtc.cmd.setreg.index = regvar->index;
  rtc.cmd.setreg.nreg = regvar->nreg;
  rtc.cmd.setreg.seq = sch_next_setreg_seq(sc, cp_Scheduler(cp));
/*
 * Queue the object to be sent to the controller.
 */
  return queue_rtc_command(cp, &rtc, NET_SETREG_CMD);
}

/*.......................................................................
 * Implement the command that returns the value of a specified SZA register
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         reg  -  The register specification.
 *                         val  -  The register value.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static FUNC_FN(sc_getreg_fn)
{
  ControlProg *cp = (ControlProg* )sc->project;  /* The context of the
						    host control
						    program */
  Variable *vreg;   /* The register specification argument */
  RegisterVariable *regvar;  /* The derived type of vreg */
  unsigned long val; /* The value of the register */

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "getreg"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vreg, NULL))
    return 1;

  regvar = REGISTER_VARIABLE(vreg);

  // Get the value of the requested register from the last frame
  // buffer.

  if(get_reg_info(cp_Archiver(cp), regvar->regmap, regvar->board, 
		  regvar->block, regvar->index, &val))
    return 1;

  UINT_VARIABLE(result)->uint = val;

  return 0;
}

/*.......................................................................
 * Implement the command that tells the scanner to unflag a given
 * board of registers after a bus error has resulted in it being marked
 * as unreachable.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        board - The board to unflag.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_unflag_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  RtcNetCmd rtc;                 /* The network object to be sent to the */
                                 /*  real-time controller task */
  Variable *vboard;              /* The board specification argument */
/*
 * Do we have a controller to send the command to?
 */
  if(rtc_offline(sc, "unflag"))
    return 1;
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vboard, NULL))
    return 1;
/*
 * Compose a network object to be sent to the real-time controller task.
 */
  rtc.cmd.unflag.board = UINT_VARIABLE(vboard)->uint;
/*
 * Queue the object to be sent to the controller.
 */
  return queue_rtc_command(cp, &rtc, NET_UNFLAG_CMD);
}

/*.......................................................................
 * Implement the command that sets the output register of a given
 * digital I/O card.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         board -  The name of a digital I/O board.
 *                         oper  -  The operation to use to combine the
 *                                  specified value with the existing value
 *                                  of the output register.
 *                         val   -  The value to be combined.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setdio_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project;  /* The context of the
						    host control
						    program */
  Variable *vbrd;   /* The digital I/O board argument */
  Variable *voper;  /* The operator argument */
  Variable *vval;   /* The value argument */
  RtcNetCmd rtc;    /* The network command object to be sent to the */
                    /*  real-time controller task */
/*
 * Do we have a controller to send the command to?
 */
  if(rtc_offline(sc, "setdio"))
    return 1;
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vbrd, &voper, &vval, NULL))
    return 1;
/*
 * Compose a network object to be sent to the real-time controller task.
 */
  rtc.cmd.setdio.value = UINT_VARIABLE(vval)->uint;
  rtc.cmd.setdio.board = UINT_VARIABLE(vbrd)->uint;
  rtc.cmd.setdio.oper = CHOICE_VARIABLE(voper)->choice;
/*
 * Queue the object to be sent to the controller.
 */
  return queue_rtc_command(cp, &rtc, NET_SETDIO_CMD);
}

/*-----------------------------------------------------------------------*
 * Archive control commands                                              *
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Tell the logger where to place subsequent log files.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         dir  -  The new log-file directory.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_logdir_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project;  /* The context of the
						    host control
						    program */
  Variable *vdir;     /* The directory-name argument */
  LoggerMessage msg;  /* The message to be sent to the logger thread */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vdir, NULL))
    return 1;
/*
 * Send the request to the logger thread.
 */
  if(pack_logger_chdir(&msg, STRING_VARIABLE(vdir)->string) ||
     send_LoggerMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR)
    return 1;
  return 0;
}

/*.......................................................................
 * Tell the grabber where to place subsequent fits files.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         dir  -  The new log-file directory.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_grabdir_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project;  /* The context of the
						    host control
						    program */
  Variable *vdir;     /* The directory-name argument */
  GrabberMessage msg;  /* The message to be sent to the grabber thread */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vdir, NULL))
    return 1;
/*
 * Send the request to the grabber thread.
 */
  if(pack_grabber_chdir(&msg, STRING_VARIABLE(vdir)->string) ||
     send_GrabberMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR)
    return 1;
  return 0;
}

/*.......................................................................
 * Implement the command that tells the archiver or logger or grabber to 
 * open a new file.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         file -  The type of file to open.
 *                       Optional arguments.
 *                         dir  -  The name of the directory in which
 *                                 to open the file. If not specified, the
 *                                 directory from previous opens will be
 *                                 used, or if never specified the directory
 *                                 in which szacontrol was started will be
 *                                 used.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_open_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project;  /* The context of the
						    host control
						    program */
  Variable *vfile;                /* The type of file to open */
  Variable *vdir;                 /* The optional directory */
  char *dir;                      /* The archiving directory */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vfile, &vdir, NULL))
    return 1;
/*
 * Get the target directory.
 */
  dir = OPTION_HAS_VALUE(vdir) ? STRING_VARIABLE(vdir)->string : (char* )"";
/*
 * Send an open message to the specified thread.
 */
  switch(CHOICE_VARIABLE(vfile)->choice) {
  case ARC_LOG_FILE:
    {
      LoggerMessage msg;
      if(pack_logger_open(&msg, dir) ||
	 send_LoggerMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR)
	return 1;
    };
    break;
  case ARC_DAT_FILE:
    {
      ArchiverMessage msg;
      if(pack_archiver_open(&msg, dir) ||
	 send_ArchiverMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR)
	return 1;
    };
    break;
  case ARC_GRAB_FILE:
    {
      GrabberMessage msg;
      if(pack_grabber_open(&msg, dir) ||
	 send_GrabberMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR)
	return 1;
    };
    break;
  default:
    lprintf(stderr, "sc_open_cmd: Unknown type of archive file.\n");
    return 1;
  };
  return 0;
}

/*.......................................................................
 * Implement the command that tells the archiver or logger to flush the
 * stdio buffers of its file to the disk.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         file  -  The type of file.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_flush_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project;  /* The context of the
						    host control
						    program */
  Variable *vfile;                /* The type of file */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vfile, NULL))
    return 1;
/*
 * Send a close message to the specified thread.
 */
  switch(CHOICE_VARIABLE(vfile)->choice) {
  case ARC_LOG_FILE:
    {
      LoggerMessage msg;
      if(pack_logger_flush(&msg) ||
	 send_LoggerMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR)
	return 1;
    };
    break;
  case ARC_DAT_FILE:
    {
      ArchiverMessage msg;
      if(pack_archiver_flush(&msg) ||
	 send_ArchiverMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR)
	return 1;
    };
    break;
  case ARC_GRAB_FILE:
    {
      GrabberMessage msg;
      if(pack_grabber_flush(&msg) ||
	 send_GrabberMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR)
	return 1;
    };
    break;
  default:
    lprintf(stderr, "sc_flush_cmd: Unknown type of archive file.\n");
    return 1;
  };
  return 0;
}

/*.......................................................................
 * Implement the command that tells the archiver or logger to close its
 * current file.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         file  -  The type of file.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_close_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project;  /* The context of the
						    host control
						    program */
  Variable *vfile;                /* The type of file */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vfile, NULL))
    return 1;
/*
 * Send a close message to the specified thread.
 */
  switch(CHOICE_VARIABLE(vfile)->choice) {
  case ARC_LOG_FILE:
    {
      LoggerMessage msg;
      if(pack_logger_close(&msg) ||
	 send_LoggerMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR)
	return 1;
    };
    break;
  case ARC_DAT_FILE:
    {
      ArchiverMessage msg;
      if(pack_archiver_close(&msg) ||
	 send_ArchiverMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR)
	return 1;
    };
    break;
  case ARC_GRAB_FILE:
    {
      GrabberMessage msg;
      if(pack_grabber_close(&msg) ||
	 send_GrabberMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR)
	return 1;
    };
    break;
  default:
    lprintf(stderr, "sc_close_cmd: Unknown type of archive file.\n");
    return 1;
  };
  return 0;
}

/*.......................................................................
 * Implement the command that tells the archiver how many frames to
 * integrate per archived frame.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of optional command-line arguments:
 *                         combine   - The number of input frames per archived
 *                                     frame.
 *                         dir       - The default directory for archive files.
 *                         filter    - True to only include feature-marked
 *                                     frames in the archive.
 *                         file_size - The number of frames to archive before
 *                                     opening a new archive file.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_archive_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project;  /* The context of the
						    host control
						    program */
  Variable *vcombine;  /* The number of frames to integrate */
  Variable *vdir;      /* The archiving directory */
  Variable *vfilter;   /* The boolean filtering flag */
  Variable *vfile_size;/* The frames_per_file argument */
  ArchiverMessage msg; /* The message to be sent to the archiver thread */
/*
 * Get the cache which holds the last "archive combine" value commanded
 * by the user.
 */
  SchedCache *cache = sch_sched_cache(cp_Scheduler(cp));
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vcombine, &vdir, &vfilter, &vfile_size, NULL))
    return 1;
/*
 * Send the requested configuration messages to the archiver.
 */
  if(OPTION_HAS_VALUE(vcombine)) {
    cache->archive.combine = UINT_VARIABLE(vcombine)->uint;
    if(pack_archiver_sampling(&msg, UINT_VARIABLE(vcombine)->uint,
			      sch_next_frame_seq(sc, cp_Scheduler(cp))) ||
       send_ArchiverMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR)
      return 1;
  };
  if(OPTION_HAS_VALUE(vdir) &&
     (pack_archiver_chdir(&msg, STRING_VARIABLE(vdir)->string) ||
      send_ArchiverMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR))
    return 1;
  if(OPTION_HAS_VALUE(vfilter)) {
    cache->archive.filter = BOOL_VARIABLE(vfilter)->boolvar;
    if(pack_archiver_filter(&msg, BOOL_VARIABLE(vfilter)->boolvar) ||
       send_ArchiverMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR)
      return 1;
  };
  if(OPTION_HAS_VALUE(vfile_size) &&
     (pack_archiver_file_size(&msg, UINT_VARIABLE(vfile_size)->uint) ||
      send_ArchiverMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR))
    return 1;
  return 0;
}

/*.......................................................................
 * Implement a function that returns the last commanded archiving interval.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        (none)
 * Input/Output:
 *  result     Variable *  The return value.
 *  state      Variable *  Unused.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static FUNC_FN(sc_archiving_interval_fn)
{
/*
 * Get the cache which holds the last "archive combine" value commanded
 * by the user.
 */
  SchedCache *cache = (SchedCache* )sch_sched_cache(cp_Scheduler((ControlProg* )sc->project));
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, NULL))
    return 1;
/*
 * Record the result for return.
 */
  UINT_VARIABLE(result)->uint = cache->archive.combine;
  return 0;
}

/*.......................................................................
 * Implement a function that returns disposition of the last commanded
 * request for archiving filtering.
 * 
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        (none)
 * Input/Output:
 *  result     Variable *  The return value.
 *  state      Variable *  Unused.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static FUNC_FN(sc_archive_filtering_fn)
{
/*
 * Get the cache which holds the last "archive filter" value commanded
 * by the user.
 */
  SchedCache *cache = (SchedCache* )sch_sched_cache(cp_Scheduler((ControlProg* )sc->project));
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, NULL))
    return 1;
/*
 * Record the result for return.
 */
  BOOL_VARIABLE(result)->boolvar = cache->archive.filter;
  return 0;
}


/*-----------------------------------------------------------------------*
 * Commands for shutting down or restarting control system components    *
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Implement the command that loads a script to be executed whenever the
 * real-time controller is started.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         file  -  The name of the schedule file.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_load_reboot_script_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project;   /* The
						     control-program
						     resource
						     container */
  Scheduler *sch = (Scheduler* )cp_ThreadData(cp, CP_SCHEDULER);
  Variable *vsc;                   /* The script argument */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vsc, NULL))
    return 1;
/*
 * Compile the contents of the schedule and queue it for execution.
 */
  if(sch_change_init_script(sch, SCRIPT_VARIABLE(vsc)->sc))
    return 1;
  return 0;
}

/*-----------------------------------------------------------------------*
 * Schedule control commands                                             *
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Implement the command that queues a schedule for execution.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         sc  -  The scheduling script to queue.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_schedule_cmd)
{
  Variable *vsc;                   /* The schedule argument */
/*
 * Get the resource object of the parent thread.
 */
  Scheduler *sch = (Scheduler* )cp_ThreadData((ControlProg* )sc->project, CP_SCHEDULER);
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vsc, NULL))
    return 1;

/*
 * Queue the schedule.
 */
  if(sch_queue_schedule(sch, SCRIPT_VARIABLE(vsc)->sc))
    return 1;
  return 0;
}

/*.......................................................................
 * Implement the command that kills the currently running schedule.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments (none).
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_abort_schedule_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project;   /* The
						     control-program
						     resource
						     container */
  Scheduler *sch = (Scheduler* )cp_ThreadData(cp, CP_SCHEDULER);
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, NULL))
    return 1;
/*
 * Pass on the request to the scheduler.
 */
  if(sch_abort_schedule(sch))
    return 1;
  return 0;
}

/*.......................................................................
 * Implement the command that temporarily suspends execution the
 * currently running schedule.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments (none).
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_suspend_schedule_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project;   /* The
						     control-program
						     resource
						     container */
  Scheduler *sch = (Scheduler* )cp_ThreadData(cp, CP_SCHEDULER);
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, NULL))
    return 1;
/*
 * Pass on the request to the scheduler.
 */
  if(sch_suspend_schedule(sch))
    return 1;
  return 0;
}

/*.......................................................................
 * Implement the command that temporarily resumes execution of the
 * currently running schedule, after it having been suspended.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments (none).
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_resume_schedule_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project;   /* The
						     control-program
						     resource
						     container */
  Scheduler *sch = (Scheduler* )cp_ThreadData(cp, CP_SCHEDULER);
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, NULL))
    return 1;
/*
 * Pass on the request to the scheduler.
 */
  if(sch_resume_schedule(sch))
    return 1;
  return 0;
}

/*.......................................................................
 * Implement the command that remove a given schedule from the schedule
 * queue.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments.
 *                         number  -  The schedule queue index of the
 *                                    schedule to be removed.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_remove_schedule_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project;   /* The
						     control-program
						     resource
						     container */
  Scheduler *sch = (Scheduler* )cp_ThreadData(cp, CP_SCHEDULER);
  Variable *num;                   /* The queue index of the target schedule */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &num, NULL))
    return 1;
/*
 * Pass on the request to the scheduler.
 */
  if(sch_remove_schedule(sch, UINT_VARIABLE(num)->uint))
    return 1;
  return 0;
}

/*.......................................................................
 * Implement the command that advances a given schedule within the schedule
 * queue.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments.
 *                         n  -  The schedule queue index of the
 *                               schedule to be moved.
 *                         dn -  The number of entries to advance the
 *                               schedule by.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_advance_schedule_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project;   /* The
						     control-program
						     resource
						     container */
  Scheduler *sch = (Scheduler* )cp_ThreadData(cp, CP_SCHEDULER);
  Variable *n;               /* The queue index of the target schedule */
  Variable *dn;              /* The number of entries to move the schedule */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &n, &dn, NULL))
    return 1;
/*
 * Pass on the request to the scheduler.
 */
  if(sch_move_schedule(sch, UINT_VARIABLE(n)->uint, -UINT_VARIABLE(dn)->uint))
    return 1;
  return 0;
}

/*.......................................................................
 * Implement the command that retards a given schedule within the schedule
 * queue.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments.
 *                         n  -  The schedule queue index of the
 *                               schedule to be moved.
 *                         dn -  The number of entries to retard the
 *                               schedule by.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_retard_schedule_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project;   /* The
						     control-program
						     resource
						     container */
  Scheduler *sch = (Scheduler* )cp_ThreadData(cp, CP_SCHEDULER);
  Variable *n;               /* The queue index of the target schedule */
  Variable *dn;              /* The number of entries to move the schedule */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &n, &dn, NULL))
    return 1;
/*
 * Pass on the request to the scheduler.
 */
  if(sch_move_schedule(sch, UINT_VARIABLE(n)->uint, UINT_VARIABLE(dn)->uint))
    return 1;
  return 0;
}

/*.......................................................................
 * Implement the command that checks wether a schedule is valid.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         sc  -  The scheduling script to check.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_check_schedule_cmd)
{
  Variable *vsc;                   /* The schedule argument */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vsc, NULL))
    return 1;
/*
 * Since the schedule was checked when it was parsed as a Script variable,
 * we know that it is ok, if we got this far.
 */
  lprintf(stdout, "Schedule %s is ok.\n",
	  SCRIPT_VARIABLE(vsc)->sc->script.name);
  return 0;
}

/*.......................................................................
 * Implement the command that controls the scheduler autoqueue feature.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                       Optional arguments:
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_auto_queue_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vstate;              /* State of the queueing -- on or off */
  Variable *vdir;                /* Directory in which to look for files */
  Variable *vinterval;           /* Polling interval */
  SchedulerMessage msg;          /* The message to be sent to the scheduler
				    thread. */
                                 /*  real-time controller task */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vdir, &vstate, &vinterval, NULL))
    return 1;
/*
 * Compose the scheduler message.
 */
  if(OPTION_HAS_VALUE(vdir)) {
    if(pack_scheduler_auto_dir(&msg, STRING_VARIABLE(vdir)->string) ||
       send_SchedulerMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR)
      return 1;
  }
  if(OPTION_HAS_VALUE(vinterval)) {
    /*
     * Convert to milliseconds
     */
    double ms = floor(DOUBLE_VARIABLE(vinterval)->d + 0.5)*1000; 
    if(pack_scheduler_auto_poll(&msg, (long int)ms) ||
       send_SchedulerMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR)
      return 1;
  }
  if(OPTION_HAS_VALUE(vstate)) {
    if(pack_scheduler_auto_state(&msg, 
				 CHOICE_VARIABLE(vstate)->choice==SWITCH_ON) ||
       send_SchedulerMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR)
      return 1;
  }
  return 0;
}

/*.......................................................................
 * Implement the command that configures the pager email list
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                       Optional arguments:
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_pagerEmailAddress_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vaction;
  Variable *vemail;
  char* email=0;
  OutputStream *output = sc->output; /* The stream wrapper around stdout */
                                 /*  real-time controller task */
  TermMessage msg;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vaction, &vemail, NULL))
    return 1;
  
  email = OPTION_HAS_VALUE(vemail) ? STRING_VARIABLE(vemail)->string : NULL;

  // See what was requested

  try {
    switch((EmailAction)CHOICE_VARIABLE(vaction)->choice) {
    case EMAIL_ADD:
      if(pack_pager_email(&msg, true, email) ||
	 send_TermMessage(cp, &msg, PIPE_WAIT)==PIPE_ERROR)
	return 1;
      break;
    case EMAIL_CLEAR:
      if(pack_pager_email(&msg, false, email) ||
	 send_TermMessage(cp, &msg, PIPE_WAIT)==PIPE_ERROR)
	return 1;
      break;
    case EMAIL_LIST:
      std::vector<std::string>* emailList = getPagerEmailList(cp);

      for(unsigned i=0; i < emailList->size(); i++) {
	std::cout << emailList->at(i) << std::endl;
	if(output_printf(output, "%s\n", emailList->at(i).c_str())<0)
	  return 1;
      }
      std::cout << "Done" << std::endl;
      break;
    }
  } catch(...) {
    return 1;
  }

  return 0;
}

/*.......................................................................
 * Implement the command that controls the pager
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                       Optional arguments:
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_pager_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vstate=0; // State of the queueing -- on or off
  Variable *vip=0;    // The optional ip address
  Variable *vdev=0;   // The device we are addressing
  Variable *vreg=0;   // The register which activated the pager
  Variable *vhost=0;  // The optional host name/address

  PagerDev dev;
  char *ip=NULL;
  char *reg=NULL;
  char *host=NULL;

  Scheduler *sch = (Scheduler* )cp_ThreadData(cp, CP_SCHEDULER);

  TermMessage msg;

  // Get the command-line arguments.

  if(get_Arguments(args, &vstate, &vip, &vdev, &vreg, &vhost, NULL))
    return 1;
  
  // Get the target directory.

  ip   = OPTION_HAS_VALUE(vip) ? STRING_VARIABLE(vip)->string : NULL;
  
  // Get the target directory.

  dev  = (PagerDev)(OPTION_HAS_VALUE(vdev) ? CHOICE_VARIABLE(vdev)->choice : PAGER_NONE);

  // Get the optional host name

  host = OPTION_HAS_VALUE(vhost) ? STRING_VARIABLE(vhost)->string : NULL;

  // Compose the scheduler message

  switch (CHOICE_VARIABLE(vstate)->choice) {
    
    // Tell the terminal thread to change the IP address of the pager

  case PAGER_IP:

    if(ip) {
      
      // Now the user must specify a device as well

      if(dev==PAGER_NONE) {
	lprintf(stderr, "pager_cmd: No device specified.\n");
	return 1;
      }
      
      // Pack the message for transmission to the terminal thread

      if(pack_pager_ip(&msg, dev, ip) || 
	 send_TermMessage(cp, &msg, PIPE_WAIT)==PIPE_ERROR)
	return 1;
    } else {
      lprintf(stderr, "pager_cmd: No IP address specified.\n");
      return 1;
    }

    break;

  case PAGER_LIST:

    //    return sendListPagerMsg(cp);

    break;


  case PAGER_CLEAR:

    return sendClearPagerMsg(cp);

    break;

  case PAGER_ENABLE:
    
    // And enable paging requests from clients

    if(sch_send_paging_state(sch, 1, NULL))
      return 1;

    // And send the messsage to disable the pager to the control
    // thread

    if(sendEnablePagerMsg(cp, true))
      return 1;

    break;
    
    // If we wish to activate the pager, send the disallow message to
    // the clients, and forward the appropriate command to the rtc

  case PAGER_ON:

    // We don't allow pager activation by anybody but szanet for now

    if(host != 0 && (strcmp(host, "szanet.ovro.caltech.edu")==0 ||
		     strcmp(host, "192.100.16.160")==0)) {

      // Disallow further paging commands

      if(sch_send_paging_state(sch, 0, NULL))
	return 1;
      
      // And activate the pager

      reg = OPTION_HAS_VALUE(vreg) ? STRING_VARIABLE(vreg)->string : NULL;

      // Use this wrapper to send pages

      if(send_reg_page_msg(cp, reg))
	return 1;

    } else {

      lprintf(stderr, "Request to activate pager by %s ignored.\n",
	      (host ? host : "an unrecognized host"));

    }

    break;
    
    // De-activate the pager

  case PAGER_OFF:
    
    // Send the message to deactivate the pager

    if(pack_term_reg_page(&msg, 0, 0, false) || send_TermMessage(cp, &msg, PIPE_WAIT)==PIPE_ERROR)
      return 1;

    break;

  case PAGER_DISABLE:
    
    // Disallow further paging commands from remote monitor clients

    if(sch_send_paging_state(sch, 0, NULL))
      return 1;

    // And send the messsage to disable the pager to the control
    // thread

    if(sendEnablePagerMsg(cp, false))
      return 1;

    break;
  }
  return 0;
}

/*.......................................................................
 * Implement the command that sets a register on which to page
 *
 * Input:
 *
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                       Optional arguments:
 * Output:
 *
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_addPagerRegister_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object

  Variable *vReg=0;        // The register which should activate the
			   // pager

  Variable *vMin=0;        // The min

  Variable *vMax=0;        // The max

  Variable *vNframe=0;     // The number of frames before this
			   // condition should activate the pager

  Variable *vDelta=0;      // True if this condition should be applied
			   // to the delta, instead of the value.

  Variable *vOutOfRange=0; // True if the pager should be activated
			   // when the value falls out of the
			   // specified range.  False if
  Variable* vComment=0;

  // Get the command-line arguments.

  if(get_Arguments(args, &vReg, &vMin, &vMax, &vNframe, &vDelta, &vOutOfRange, &vComment, NULL))
    return 1;
  
  // Get the register specification

  char*  reg      = STRING_VARIABLE(vReg)->string;
  double min      = DOUBLE_VARIABLE(vMin)->d;
  double max      = DOUBLE_VARIABLE(vMax)->d;

  bool delta      = OPTION_HAS_VALUE(vDelta)      ? BOOL_VARIABLE(vDelta)->boolvar      : false;
  unsigned nFrame = OPTION_HAS_VALUE(vNframe)     ? UINT_VARIABLE(vNframe)->uint        : 2;
  bool outOfRange = OPTION_HAS_VALUE(vOutOfRange) ? BOOL_VARIABLE(vOutOfRange)->boolvar : true;
  char* comment   = OPTION_HAS_VALUE(vComment) ? STRING_VARIABLE(vComment)->string : NULL;

  return sendAddPagerRegisterMsg(cp, reg, min, max, delta, nFrame, outOfRange, comment);
}

/*.......................................................................
 * Implement the command that sets a register on which to page
 *
 * Input:
 *
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                       Optional arguments:
 * Output:
 *
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_remPagerRegister_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object

  Variable *vReg=0;        // The register which should activate the
			   // pager

  // Get the command-line arguments.

  if(get_Arguments(args, &vReg, NULL))
    return 1;
  
  // Get the register specification

  char*  reg      = STRING_VARIABLE(vReg)->string;

  return sendRemPagerRegisterMsg(cp, reg);
}

/*-----------------------------------------------------------------------*
 * Event management commands and functions                               *
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Add to the list of keywords that are accepted as signal names.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of optional command-line arguments:
 *                        keys  -  A list of keywords.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_add_signals_cmd)
{
  Variable *vkeys;          /* The list of keywords */
  ListNode *node;           /* A node in the list of keyword variables */
/*
 * Get the resource object of the parent thread.
 */
  Scheduler *sch = (Scheduler* )cp_ThreadData((ControlProg *)sc->project, 
					      CP_SCHEDULER);
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vkeys, NULL))
    return 1;
/*
 * Traverse the list of keywords to be added.
 */
  for(node=LIST_VARIABLE(vkeys)->list->head; node; node=node->next) {
    char *name = STRING_VARIABLE(node->data)->string;
    if(!sch_add_signal(sch, name))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Send/discard or initialize a given signal.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of optional command-line arguments:
 *                        op      -  One of the options send|clear|init.
 *                        signal  -  The name of the signal.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_signal_cmd)
{
  Variable *vop;        /* The operation to perform */
  Variable *vsig;       /* The signal name */
  char *op;             /* The value of *vop */
  Symbol *sig;          /* The value of *vsig */
/*
 * Get the resource object of the parent thread.
 */
  Scheduler *sch = (Scheduler* )cp_ThreadData((ControlProg *)sc->project, 
					      CP_SCHEDULER);
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vop, &vsig, NULL))
    return 1;
/*
 * Extract their values.
 */
  op = STRING_VARIABLE(vop)->string;
  sig = SIGNAL_VARIABLE(vsig)->sym;
/*
 * Perform the specified operation.
 */
  if(strcmp(op, "send") == 0) {
    sch_signal_schedule(sch, sig);
  } else if(strcmp(op, "clear") == 0) {
    clear_script_signal(sig);
  } else if(strcmp(op, "init") == 0) {
    reset_script_signal(sig);
  } else {
    lprintf(stderr, "signal: Unhandled qualifier (%s).\n", op);
    return 1;
  };
  return 0;
}

/*.......................................................................
 * Implement a function that returns true when a given set of operations
 * have completed.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        AcquireTargets - The set of targets to
 *                                         check.
 * Input/Output:
 *  result     Variable *  The return value.
 *  state      Variable *  Unused.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static FUNC_FN(sc_acquired_fn)
{
  ControlProg *cp = (ControlProg* )sc->project;  /* The
						    control-program
						    resource
						    container */
  Scheduler *sch = (Scheduler* )cp_Scheduler(cp); /* The resource
						     object of this
						     thread */
  Variable *vtarget;   /* The set of targets */
  Variable *vant;      /* The set of antenans */
  int completed = 1;   /* True if the specified operations have completed */
  unsigned set;        /* The set of operations to check. */
  unsigned antennas;

  // Get the command-line arguments.

  if(get_Arguments(args, &vtarget, &vant, NULL))
    return 1;

  set = SET_VARIABLE(vtarget)->set;

  antennas = OPTION_HAS_VALUE(vant) ? 
    SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Accumulate the combined completion status of all the specified
  // operations.

  if(set & ACQ_CALTERT)
    completed = completed && (antennas & ~sch_caltert_done(sch))==0x0;
  if(set & ACQ_IFMOD)
    completed = completed && (antennas & ~sch_IFMod_done(sch))==0x0;
  if(set & ACQ_SOURCE) 
    completed = completed && sch_pmac_done(sch);
  if(set & ACQ_TV_OFFSET)
    completed = completed && (antennas & ~sch_tv_offset_done(sch))==0x0;
  if(set & ACQ_CAN)
    completed = completed && (antennas & ~sch_can_done(sch))==0x0;

  // The following don't depend on antennas

  if(set & ACQ_MARK)
    completed = completed && sch_mark_done(sch);
  if(set & ACQ_GRAB)
    completed = completed && sch_grab_done(sch);
  if(set & ACQ_SETREG)
    completed = completed && sch_setreg_done(sch);
  if(set & ACQ_FRAME)
    completed = completed && sch_frame_done(sch);
  if(set & ACQ_NOISE)
    completed = completed && sch_noise_done(sch);
  
  // Record the result for return.

  BOOL_VARIABLE(result)->boolvar = completed;

  return 0;
}

/*-----------------------------------------------------------------------*
 * Receiver control commands                                             *
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Implement the command that sets the walsh modulation and/or demodulation
 * functions.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        WalshStages stages - The stages to change, from
 *                                             modulation and/or demodulation.
 *                        Receivers rx       - The set of target receivers.
 *                        WalshFunction fn   - The desired walsh function.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_selectRx_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vant; /* The antennas to apply this command to */
  Variable *vrx;  /* The receiver to select to change */
  RtcNetCmd rtc;  /* The network object to be sent to the */
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "selectRx"))
    return 1;

  // Get the command-line arguments.

  if(get_Arguments(args, &vrx, &vant, NULL))
    return 1;

  // Set the requested band

  rtc.cmd.selectRx.band = SET_VARIABLE(vrx)->set;

  // Parse optional antenna argument here.  If no argument was given,
  // default to all antennas.
  
  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();
  
  rtc.cmd.selectRx.seq = sch_next_can_seq(sc, cp_Scheduler(cp));

  // Queue the command

  if(queue_rtc_command(cp, &rtc, NET_SELECT_RX_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that sets an amplifier bias
 */
static CMD_FN(sc_setBias_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vamp;  // The amplifier bias to change 
  Variable *vbias; // The value to set 
  Variable *vant;  // The antennas to apply this command to
  RtcNetCmd rtc;   // The network object to be sent to the   

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setBias"))
    return 1;

  // Get the command-line arguments.

  if(get_Arguments(args, &vamp, &vbias, &vant, NULL))
    return 1;

  // Set the requested band

  rtc.cmd.setBias.amp       = SET_VARIABLE(vamp)->set;
  rtc.cmd.setBias.bias      = INT_VARIABLE(vbias)->i;
  rtc.cmd.setBias.biasType  = sza::array::AMP;
  rtc.cmd.setBias.isDefault = false;

  // Parse optional antenna argument here.  If no argument was given,
  // default to all antennas.
  
  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();
  
  // Queue the command

  if(queue_rtc_command(cp, &rtc, NET_SET_BIAS_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that sets a default bias
 */
static CMD_FN(sc_setDefaultBias_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vamp;  // The amplifier bias to change 
  Variable *vbias; // The value to set 
  Variable *vant;  // The antennas to apply this command to
  RtcNetCmd rtc;   // The network object to be sent to the   

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setDefaultBias"))
    return 1;

  // Get the command-line arguments.

  if(get_Arguments(args, &vamp, &vbias, &vant, NULL))
    return 1;

  // Set the requested band

  rtc.cmd.setBias.amp       = SET_VARIABLE(vamp)->set;
  rtc.cmd.setBias.bias      = INT_VARIABLE(vbias)->i;
  rtc.cmd.setBias.biasType  = sza::array::RX;
  rtc.cmd.setBias.isDefault = true;

  // Parse optional antenna argument here.  If no argument was given,
  // default to all antennas.
  
  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();
  
  // Queue the command

  if(queue_rtc_command(cp, &rtc, NET_SET_BIAS_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that sets a bias
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        WalshStages stages - The stages to change, from
 *                                             modulation and/or demodulation.
 *                        Receivers rx       - The set of target receivers.
 *                        WalshFunction fn   - The desired walsh function.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_biasRx_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vrx;  // The amplifier bias to change 
  Variable *vant;  // The antennas to apply this command to
  RtcNetCmd rtc;   // The network object to be sent to the   

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "biasRx"))
    return 1;

  // Get the command-line arguments.

  if(get_Arguments(args, &vrx, &vant, NULL))
    return 1;

  // Set the requested band

  rtc.cmd.setBias.rxId      = CHOICE_VARIABLE(vrx)->choice;
  rtc.cmd.setBias.biasType  = sza::array::RX;
  rtc.cmd.setBias.seq       = sch_next_can_seq(sc, cp_Scheduler(cp));
  rtc.cmd.setBias.isDefault = false;

  // Parse optional antenna argument here.  If no argument was given,
  // default to all antennas.
  
  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();
  
  // Queue the command

  if(queue_rtc_command(cp, &rtc, NET_SET_BIAS_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that enables or disables fast sampling on the
 * Rx card
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Integer channel   -  The channel on which to 
 *                                             dis/enable fast sampling
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_fast_sampling_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vchan;               // The channel
  Variable *vstate;              // The switch state
  Variable *vant;                // The set of target antennas 
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "fast_sampling"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vchan, &vstate, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.fast_sampling.channel = UINT_VARIABLE(vchan)->uint;
  rtc.cmd.fast_sampling.start  = CHOICE_VARIABLE(vstate)->choice == SWITCH_ON;

  // Parse optional antenna argument here.  If no argument was given,
  // default to all antennas.
  
  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_FAST_SAMPLING_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that enables or disables local-oscillator stages.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Receivers rx      -  The set of target receivers.
 *                        Stages stages     -  The set of target stages.
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_lo_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vosc;               // The set of target LO bands
  Variable *vstage;              // The set of target stages 
  Variable *vstate;              // The desired switch state 
  Variable *vant;                // The set of target antennas 
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "configureLO"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vosc, &vstage, &vstate, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.lo.msgId     = LO_TOGGLE;
  rtc.cmd.lo.on        = (CHOICE_VARIABLE(vstate)->choice == SWITCH_ON);
  rtc.cmd.lo.oscs      = SET_VARIABLE(vosc)->set;
  rtc.cmd.lo.stages    = SET_VARIABLE(vstage)->set;

  // Parse optional antenna argument here.  If no argument was given,
  // use the default set

  rtc.antennas  = OPTION_HAS_VALUE(vant) ? 
    SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();
  
  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_LO_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that sets the Yig operating voltage
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Receivers rx      -  The set of target receivers.
 *                        Stages stages     -  The set of target stages.
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setYigVoltage_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vvolt;               // The desired frequency
  Variable *vant;                // The set of target antennas 
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setYigVoltage"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vvolt, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.lo.msgId    = LO_VOLTAGE;
  rtc.cmd.lo.oscs     = sza::util::LoOsc::YIG;
  rtc.cmd.lo.voltage  = INT_VARIABLE(vvolt)->i;

  // Parse optional antenna argument here.  If no argument was given,
  // use the default antenna set

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_LO_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that uploads the Yig ID
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Receivers rx      -  The set of target receivers.
 *                        Stages stages     -  The set of target stages.
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_downloadYigId_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable* vid;               // The desired frequency
  Variable* vmonth;            // The set of target antennas 
  Variable* vday;              // The set of target antennas 
  Variable* vyear;             // The set of target antennas 
  Variable* vant;              // The set of target antennas 
  RtcNetCmd rtc;               // The network object to be sent to
			       // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "downloadYigId"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vid, &vmonth, &vday, &vyear, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.lo.msgId = LO_ID;
  rtc.cmd.lo.oscs  = sza::util::LoOsc::YIG;
  rtc.cmd.lo.id    = INT_VARIABLE(vid)->i;
  rtc.cmd.lo.month = INT_VARIABLE(vmonth)->i;
  rtc.cmd.lo.day   = INT_VARIABLE(vday)->i;
  rtc.cmd.lo.year  = INT_VARIABLE(vyear)->i;

  // Parse optional antenna argument here.  If no argument was given,
  // use the default antenna set

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_LO_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that uploads the Gunn ID
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Receivers rx      -  The set of target receivers.
 *                        Stages stages     -  The set of target stages.
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_downloadGunnId_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable* vid;               // The desired frequency
  Variable* vmonth;            // The set of target antennas 
  Variable* vday;              // The set of target antennas 
  Variable* vyear;             // The set of target antennas 
  Variable* vvolt;
  Variable* vnpt;
  Variable* vant;              // The set of target antennas 
  RtcNetCmd rtc;               // The network object to be sent to
			       // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "downloadGunnId"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vid, &vmonth, &vday, &vyear, &vvolt, &vnpt, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.lo.msgId   = LO_ID;
  rtc.cmd.lo.oscs    = sza::util::LoOsc::GUNN;
  rtc.cmd.lo.id      = INT_VARIABLE(vid)->i;
  rtc.cmd.lo.month   = INT_VARIABLE(vmonth)->i;
  rtc.cmd.lo.day     = INT_VARIABLE(vday)->i;
  rtc.cmd.lo.year    = INT_VARIABLE(vyear)->i;
  rtc.cmd.lo.voltage = INT_VARIABLE(vvolt)->i;
  rtc.cmd.lo.npt     = INT_VARIABLE(vnpt)->i;

  // Parse optional antenna argument here.  If no argument was given,
  // use the default antenna set

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_LO_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that downloads a Yig tuning table entry
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Receivers rx      -  The set of target receivers.
 *                        Stages stages     -  The set of target stages.
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_downloadYigTuningTableEntry_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vvolt;             // The desired frequency
  Variable *vfreq;            // The set of target antennas 
  Variable* vant;
  RtcNetCmd rtc;               // The network object to be sent to
			       // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "downloadYigTuningTableEntry"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vvolt, &vfreq, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.lo.msgId     = LO_TUNINGTABLE;
  rtc.cmd.lo.oscs      = sza::util::LoOsc::YIG;
  rtc.cmd.lo.voltage   = INT_VARIABLE(vvolt)->i;
  rtc.cmd.lo.frequency = INT_VARIABLE(vfreq)->i;

  // Parse optional antenna argument here.  If no argument was given,
  // use the default antenna set

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_LO_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that downloads a Gunn tuning table entry
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Receivers rx      -  The set of target receivers.
 *                        Stages stages     -  The set of target stages.
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_downloadGunnTuningTableEntry_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vvolt;             // The desired frequency
  Variable *vfreq;            // The set of target antennas 
  Variable* vant;
  RtcNetCmd rtc;               // The network object to be sent to
			       // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "downloadGunnTuningTableEntry"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vvolt, &vfreq, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.lo.msgId     = LO_TUNINGTABLE;
  rtc.cmd.lo.oscs      = sza::util::LoOsc::GUNN;
  rtc.cmd.lo.voltage   = INT_VARIABLE(vvolt)->i;
  rtc.cmd.lo.frequency = INT_VARIABLE(vfreq)->i;

  // Parse optional antenna argument here.  If no argument was given,
  // use the default antenna set

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_LO_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that downloads a Yig tuning table to the
 * one-wire device.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Receivers rx      -  The set of target receivers.
 *                        Stages stages     -  The set of target stages.
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_downloadYigTuningTableToOneWire_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable* vant;
  RtcNetCmd rtc;               // The network object to be sent to
			       // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "downloadYigTuningTableToOneWire"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.lo.msgId     = LO_ONEWIRE;
  rtc.cmd.lo.oscs      = sza::util::LoOsc::YIG;

  // Parse optional antenna argument here.  If no argument was given,
  // use the default antenna set

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_LO_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that downloads a Gunn tuning table to the
 * one-wire device.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Receivers rx      -  The set of target receivers.
 *                        Stages stages     -  The set of target stages.
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_downloadGunnTuningTableToOneWire_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable* vant;
  RtcNetCmd rtc;               // The network object to be sent to
			       // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "downloadGunnTuningTableToOneWire"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.lo.msgId     = LO_ONEWIRE;
  rtc.cmd.lo.oscs      = sza::util::LoOsc::GUNN;

  // Parse optional antenna argument here.  If no argument was given,
  // use the default antenna set

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_LO_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that enables/disables the Yig autolock function
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Receivers rx      -  The set of target receivers.
 *                        Stages stages     -  The set of target stages.
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_enableYigAutoRelock_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *venable; // The desired frequency
  Variable* vant;
  RtcNetCmd rtc;   // The network object to be sent to
		   // the real-time controller task
                    
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "enableYigAutoRelock"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &venable, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.lo.msgId     = LO_AUTOLOCK;
  rtc.cmd.lo.oscs      = sza::util::LoOsc::YIG;
  rtc.cmd.lo.on        = BOOL_VARIABLE(venable)->boolvar;

  // Parse optional antenna argument here.  If no argument was given,
  // use the default antenna set

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_LO_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that enables/disables the Gunn autolock function
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Receivers rx      -  The set of target receivers.
 *                        Stages stages     -  The set of target stages.
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_enableGunnAutoRelock_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *venable; // The desired frequency
  Variable* vant;
  RtcNetCmd rtc;   // The network object to be sent to
		   // the real-time controller task
                    
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "enableGunnAutoRelock"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &venable, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.lo.msgId     = LO_AUTOLOCK;
  rtc.cmd.lo.oscs      = sza::util::LoOsc::GUNN;
  rtc.cmd.lo.on        = BOOL_VARIABLE(venable)->boolvar;

  // Parse optional antenna argument here.  If no argument was given,
  // use the default antenna set

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_LO_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that sets the DAC cal coefficient
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Receivers rx      -  The set of target receivers.
 *                        Stages stages     -  The set of target stages.
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setDACCalCoefficient_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vcoeff; // The calibation coefficient
  Variable* vant;
  RtcNetCmd rtc;   // The network object to be sent to
		   // the real-time controller task
                    
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setDACCalCoefficient"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vcoeff, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.lo.msgId = LO_SETDACCOEFF;
  rtc.cmd.lo.coeff = DOUBLE_VARIABLE(vcoeff)->d;

  // Parse optional antenna argument here.  If no argument was given,
  // use the default antenna set

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_LO_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that enables or disables local-oscillator stages.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Receivers rx      -  The set of target receivers.
 *                        Stages stages     -  The set of target stages.
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setYigFrequency_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vfreq;               // The desired frequency
  Variable *vant;                // The set of target antennas 
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setYigFrequency"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vfreq, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.lo.msgId     = LO_LO_FREQ;
  rtc.cmd.lo.oscs      = sza::util::LoOsc::YIG;
  rtc.cmd.lo.frequency = INT_VARIABLE(vfreq)->i;

  // Parse optional antenna argument here.  If no argument was given,
  // default to all antennas.

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_LO_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that enables or disables local-oscillator stages.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Receivers rx      -  The set of target receivers.
 *                        Stages stages     -  The set of target stages.
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setDefaultYigFrequency_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vfreq;               // The desired frequency
  Variable *vant;                // The set of target antennas 
  Variable *vrx;                 // The set of target receivers
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setDefaultYigFrequency"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vfreq, &vant, &vrx, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.lo.msgId     = LO_DEFAULT_FREQ;
  rtc.cmd.lo.oscs      = sza::util::LoOsc::YIG;
  rtc.cmd.lo.frequency = INT_VARIABLE(vfreq)->i;
  rtc.cmd.lo.rxId      = OPTION_HAS_VALUE(vrx) ? SET_VARIABLE(vrx)->set : sza::util::Rx::RXALL;

  // Parse optional antenna argument here.  If no argument was given,
  // default to all antennas.

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_LO_CMD))
    return 1;

  return 0;
}

/**.......................................................................
 * Implement the command that enables or disables local-oscillator stages.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Receivers rx      -  The set of target receivers.
 *                        Stages stages     -  The set of target stages.
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setLoopGainResistance_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vosc;               // The set of target LO bands
  Variable *vgain;              // The desired switch state 
  Variable *vant;               // The set of target antennas 
  RtcNetCmd rtc;                // The network object to be sent to
                                // the real-time controller task
 
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setLoopGainResistance"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vosc, &vgain, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.lo.msgId     = LO_LOOPGAIN;
  rtc.cmd.lo.oscs      = SET_VARIABLE(vosc)->set;
  rtc.cmd.lo.loopGain  = INT_VARIABLE(vgain)->i;

  // Parse optional antenna argument here.  If no argument was given,
  // default to all antennas.

  rtc.antennas  = OPTION_HAS_VALUE(vant) ? 
    SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();
  
  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_LO_CMD))
    return 1;

  return 0;
}

/**.......................................................................
 * Implement the command that enables or disables local-oscillator stages.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Receivers rx      -  The set of target receivers.
 *                        Stages stages     -  The set of target stages.
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setDampingGainResistance_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vgain;              // The desired switch state 
  Variable *vant;               // The set of target antennas 
  RtcNetCmd rtc;                // The network object to be sent to
                                // the real-time controller task
 
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setDampingGainResistance"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vgain, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.lo.msgId     = LO_DAMPGAIN;
  rtc.cmd.lo.dampGain  = INT_VARIABLE(vgain)->i;

  // Parse optional antenna argument here.  If no argument was given,
  // default to all antennas.

  rtc.antennas  = OPTION_HAS_VALUE(vant) ? 
    SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();
  
  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_LO_CMD))
    return 1;

  return 0;
}


/*-----------------------------------------------------------------------*
 * Site specification commands                                           *
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Implement the command that records the location of the SZA.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Longitude longitude -  The SZA longitude (-180..180).
 *                        Latitude latitude   -  The SZA latitude (-180..180).
 *                        Altitude altitude   -  The SZA altitude (meters).
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_site_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Scheduler *sch = (Scheduler* )cp_ThreadData(cp, CP_SCHEDULER);
  Variable *vlon;                /* The longitude of the SZA */
  Variable *vlat;                /* The latitude of the SZA */
  Variable *valt;                /* The altitude of the SZA */
  NavigatorMessage msg;          /* A message to send to the navigator thread */
  Site *site;                    /* The local site description object */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vlon, &vlat, &valt, NULL))
    return 1;
/*
 * Get the scheduler's SZA site description object.
 */
  site = sch_Site(sch);
/*
 * Set the new site parameters.
 */
  if(set_Site(site, DOUBLE_VARIABLE(vlon)->d * dtor,
	      DOUBLE_VARIABLE(vlat)->d * dtor, DOUBLE_VARIABLE(valt)->d))
    return 1;
/*
 * Send the new site parameters to the navigator thread.
 */
  if(pack_navigator_site(&msg, site->longitude, site->latitude,
			 site->altitude) ||
     send_NavigatorMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR)
    return 1;
  return 0;
}

/*-----------------------------------------------------------------------*
 * Source-catalog commands and functions                                 *
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Implement the command that starts the telescope tracking a new
 * source.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        source  -  The source to be tracked.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_track_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vsrc;  // The source to be tracked 
  Variable *vtype; // The type of tracking
  Variable *vant;  // The set of antennas to command
  sza::util::Tracking::Type type;

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "track"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vsrc, &vtype, &vant, NULL))
    return 1;

  // If no type was specified, default to
  // phase and pointing tracking

  type = OPTION_HAS_VALUE(vtype) ?
    (sza::util::Tracking::Type)CHOICE_VARIABLE(vtype)->choice : 
    sza::util::Tracking::TRACK_BOTH;

  // Tell the navigator thread to send a track command to the control
  // system, update its source catalog to show this as the current
  // source, and arrange to send position updates for it when
  // necessary.
  
  // If no antennas were specified, default to all antennas

  unsigned antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();
			
  return nav_track_source(cp_Navigator(cp),
			  SOURCE_VARIABLE(vsrc)->name, 
			  type,
			  antennas,
			  sch_next_pmac_seq(sc, cp_Scheduler(cp), antennas));
}

/*.......................................................................
 * Implement the command that slews the telescope to a given location.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Azimuth az     -  The target azimuth.
 *                        Elevation el   -  The target elevation.
 *                        DeckAngle dk   -  The target deck-rotator angle.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_slew_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vaz;  /* The optional target azimuth */
  Variable *vel;  /* The optional target elevation */
  Variable *vdk;  /* The optional target deck-rotator angle */
  Variable *vant; // The set of antennas to command

  // The set of axes to be slewed 

  unsigned mask = sza::util::Axis::NONE;

  double az=0.0, el=0.0, dk=0.0; /* The target location of the slew (radians) */
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "slew"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vaz, &vel, &vdk, &vant, NULL))
    return 1;
  
  // Determine which axes are to be slewed, and extract their target
  // positions.

  if(OPTION_HAS_VALUE(vaz)) {
    mask |= sza::util::Axis::AZ;
    az = DOUBLE_VARIABLE(vaz)->d * dtor;
  };
  if(OPTION_HAS_VALUE(vel)) {
    mask |= sza::util::Axis::EL;
    el = DOUBLE_VARIABLE(vel)->d * dtor;
  };
  if(OPTION_HAS_VALUE(vdk)) {
    mask |= sza::util::Axis::PA;
    dk = DOUBLE_VARIABLE(vdk)->d * dtor;
  };
/*
 * Tell the navigator thread to slew the telescope to the
 * specified coordinates, and update the catalog to make this
 * the current source.
 */
  unsigned antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  COUT("Sending slew command");

  return nav_slew_telescope(cp_Navigator(cp), mask, az, el, dk,
			    antennas,
			    sch_next_pmac_seq(sc, cp_Scheduler(cp), antennas));
}

/*.......................................................................
 * Implement the command that stows the telescope.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Azimuth az     -  The target azimuth.
 *                        Elevation el   -  The target elevation.
 *                        DeckAngle dk   -  The target deck-rotator angle.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_stow_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vant; // The set of antennas to command

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "stow"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vant, NULL))
    return 1;
  
  // Tell the navigator thread to send a track command to the control
  // system, update its source catalog to show this as the current
  // source, and arrange to send position updates for it when
  // necessary.
  
  // If no antennas were specified, default to
  // all antennas
  unsigned antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  return nav_track_source(cp_Navigator(cp),
			  "stow",
			  sza::util::Tracking::TRACK_BOTH,
			  antennas,
			  sch_next_pmac_seq(sc, cp_Scheduler(cp), antennas));
}

/*.......................................................................
 * Implement the command that sends the telescope to service position
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Azimuth az     -  The target azimuth.
 *                        Elevation el   -  The target elevation.
 *                        DeckAngle dk   -  The target deck-rotator angle.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_service_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vant; // The set of antennas to command

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "service"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vant, NULL))
    return 1;
  
  // Tell the navigator thread to send a track command to the control
  // system, update its source catalog to show this as the current
  // source, and arrange to send position updates for it when
  // necessary.
  
  // If no antennas were specified, default to
  // all antennas

  unsigned antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  return nav_track_source(cp_Navigator(cp),
			  "service",
			  sza::util::Tracking::TRACK_BOTH,
			  antennas,
			  sch_next_pmac_seq(sc, cp_Scheduler(cp), antennas));
}

/*.......................................................................
 * Implement the command that halts the telescope.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments: (none).
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_halt_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vant; // The set of antennas to command


  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "halt"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vant, NULL))
    return 1;
  
  // Tell the navigator thread to halt the telescope and update its
  // catalog to make the current source a fixed source at the position
  // at which we stopped.

  unsigned antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  return nav_halt_telescope(cp_Navigator(cp),
			    antennas,
			    sch_next_pmac_seq(sc, cp_Scheduler(cp), antennas));
}

/*.......................................................................
 * Display the contemporary statistics of a given source.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         source  -  The name of the source.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_show_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project;     /* The
						       control-program
						       resource
						       object */
  OutputStream *output = sc->output; /* The stream wrapper around stdout */
  Variable *vsrc;                    /* The source-name argument */
  Variable *vtype;                   /* The optional antenna specifier */
  Variable *vant;                    /* The optional antenna specifier */
  Variable *vutc;                    /* The optional date specifier */
  Variable *vhorizon;                /* The optional horizon specifier */
  char *name;                        /* The value of vsrc */
  SourceInfo info;                   /* The requested information */
  SourceId id;                       /* The identification of the source */
  double utc;                        /* The time-stamp of the information */
  double horizon;                    /* The horizon to use when computing */
                                     /*  rise and set times. */
  unsigned antennas;
  sza::util::Tracking::Type type;
  std::vector<std::pair<SourceId, sza::util::AntNum::Id> > sourceList;

  // Get the opaque resource object of the navigator thread.

  Navigator *nav = cp_Navigator(cp);
  
  // Get the resource object of the scheduling task that is running
  // this script.

  Scheduler *sch = cp_Scheduler(cp);
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vsrc, &vtype, &vant, &vutc, &vhorizon, NULL))
    return 1;

  // See if a type was specified.  This will be ignored unless the
  // source is "current"

  type = OPTION_HAS_VALUE(vtype) ? 
    (sza::util::Tracking::Type)CHOICE_VARIABLE(vtype)->choice : 
    sza::util::Tracking::TRACK_POINT;

  // See if antennas were specified.  These will be ignored unless the
  // source is "current"

  antennas = OPTION_HAS_VALUE(vant) ? 
    SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Get the name of the source.  If the name is "current," this is an
  // alias for the current source of the specified antennas.

  name = SOURCE_VARIABLE(vsrc)->name;
  
  // Lookup the true name of the source.

  try {
    sourceList = navLookupSourceExtended(nav, name, type, antennas, 1);
  } catch(...) {
    lprintf(stderr, "show: Failed to lookup source: %s\n", name);
    return 1;
  };
  
  // Get the time for which the information should be computed.

  utc = OPTION_HAS_VALUE(vutc) ? DOUBLE_VARIABLE(vutc)->d : -1.0;
  
  // Get the horizon to use for computing rise and set times.

  horizon = OPTION_HAS_VALUE(vhorizon) ?
    DOUBLE_VARIABLE(vhorizon)->d * dtor : sch_get_horizon(sch);
  
  // Now loop over all returned sources, displaying the requested
  // information for each one.

  for(std::vector<std::pair<SourceId, sza::util::AntNum::Id> >::iterator 
	isrc = sourceList.begin(); isrc != sourceList.end(); isrc++) 
  {
    sza::util::AntNum antSet(isrc->second);

    id = isrc->first;

    // Request all information about the specified source for the
    // current time.
    
    if(nav_source_info(nav, id.number, utc, horizon, 
		       SIO_ALL, &info))
      return 1;
    
    // Display the requested information.
    
    // Only print the antennas if the current source was requested.

    if(navIsCurrent(name))
      if(output_printf(output, "\n%cAntennas: %s:\n", 
		       (isrc == sourceList.begin()) ? '\0' : '\n',
		       antSet.printAntennaSet().c_str())<0)
	return 1;
    
    if(output_printf(output, "Source: %s  (", id.name)<0 ||
       output_utc(output, "", 0, 0, info.utc) ||
       write_OutputStream(output, " UTC)\n") ||
       write_OutputStream(output, " AZ: ") ||
       (info.axis_mask & sza::util::Axis::AZ ?
	output_sexagesimal(output, "", 13, 0, 3, info.coord.az * rtod) :
	output_string(output, 0, "", 13, -1, -1, "(unspecified)")) ||
       write_OutputStream(output, "  EL: ") ||
       (info.axis_mask & sza::util::Axis::EL ?
	output_sexagesimal(output, "", 13, 0, 3, info.coord.el * rtod) :
	output_string(output, 0, "", 13, -1, -1, "(unspecified)")) ||
       write_OutputStream(output, "  PA: ") ||
       (info.axis_mask & sza::util::Axis::PA ?
	output_sexagesimal(output, "", 13, 0, 3, info.coord.pa * rtod) :
	output_string(output, 0, "", 13, -1, -1, "(unspecified)")) ||
       write_OutputStream(output, "\n"))
      return 1;
    
    // Display its equatorial apparent geocentric coordinates.
    
    if(info.axis_mask & sza::util::Axis::AZ && 
       info.axis_mask & sza::util::Axis::EL) {
      if(write_OutputStream(output, " RA: ") ||
	 output_sexagesimal(output, "", 13, 0, 3, info.ra * rtoh) ||
	 write_OutputStream(output, " DEC: ") ||
	 output_sexagesimal(output, "", 13, 0, 3, info.dec * rtod) ||
	 write_OutputStream(output, " (apparent)\n"))
	return 1;
      
      if(write_OutputStream(output, " RA: ") ||
	 output_sexagesimal(output, "", 13, 0, 3, info.raMean * rtoh) ||
	 write_OutputStream(output, " DEC: ") ||
	 output_sexagesimal(output, "", 13, 0, 3, info.decMean * rtod) ||
	 write_OutputStream(output, " (J2000)\n"))
	return 1;

      // Is the source above the local horizon?
      
      if(output_printf(output, " Currently %s the %.4g degree horizon (%s).\n",
		       info.coord.el > horizon ? "above":"below", horizon * rtod,
		       info.rates.el > 0.0 ? "rising":"setting") < 0)
	return 1;
      
      // Display the almanac of the source.
      
      switch(info.sched) {
      case SRC_NEVER_RISES:
	if(write_OutputStream(output, " Never rises.\n"))
	  return 1;
	break;
      case SRC_NEVER_SETS:
	if(write_OutputStream(output, " Never sets.\n"))
	  return 1;
	break;
      case SRC_ALTERNATES:
	if(info.rise < info.set) {
	  if(write_OutputStream(output, " Next rises in ") ||
	     output_almanac_time(output, info.utc, info.rise) ||
	     write_OutputStream(output, ", and sets in ") ||
	     output_almanac_time(output, info.utc, info.set) ||
	     write_OutputStream(output, ".\n"))
	    return 1;
	} else {
	  if(write_OutputStream(output, " Next sets in ") ||
	     output_almanac_time(output, info.utc, info.set) ||
	     write_OutputStream(output, ", and rises in ") ||
	     output_almanac_time(output, info.utc, info.rise) ||
	     write_OutputStream(output, ".\n"))
	    return 1;
	};
	break;
      case SRC_IRREGULAR:
      default:
	if(write_OutputStream(output, " Rise and set times indeterminate.\n"))
	  return 1;
	break;
      };
    };
  }

  return 0;
}

/*.......................................................................
 * A private function of sc_show_cmd() used to display the length of
 * time that remains before a given almanac event.
 *
 * Input:
 *  output   OutputStream *  The stream to write to.
 *  query_utc      double    The date and time of the query (UTC MJD).
 *  event_utc      double    The date and time of the event (UTC MJD).
 *  event            char *  The event description to place before the time of
 *                           the event.
 * Output:
 *  return            int    0 - OK.
 *                           1 - Error.
 */
static int output_almanac_time(OutputStream *output, double query_utc,
			       double event_utc)
{
  char *units;   /* The units of time to be used */
/*
 * Work out the time remaining before the event.
 */
  double dt = event_utc - query_utc;
/*
 * Convert it to fractional hours, minutes or seconds, where apropriate.
 */
  if(dt >= 1.0) {
    units = "days";
  } else if((dt *= 24.0) >= 1.0) {
    units = "hours";
  } else if((dt *= 60.0) >= 1.0) {
    units = "minutes";
  } else {
    dt *= 60.0;
    units = "seconds";
  };
  return output_printf(output, "%.1f %s", dt, units) < 0;
}

/*.......................................................................
 * Implement a function that returns the elevation of the current source.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  args   VariableList *  The list of command-line arguments:
 *                          source -  The source to investigate.
 * Input/Output:
 *  result     Variable *  The return value.
 *  state      Variable *  Unused.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
static FUNC_FN(sc_elevation_fn)
{
  SourceInfo info;   /* The horizon coordinates of the source */
  Variable *vsrc;    /* The source variable */
/*
 * Get the opaque resource object of the navigator thread.
 */
  Navigator *nav = (Navigator* )cp_Navigator((ControlProg* )sc->project);
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vsrc, NULL))
    return 1;
/*
 * Ask the navigator thread for the current horizon coordinates of the source.
 */
  if(nav_source_info(nav, SOURCE_VARIABLE(vsrc)->name, -1.0, 0.0, SIO_HORIZ,
		     &info))
    return 1;
/*
 * Return the elevation in degrees.
 */
  DOUBLE_VARIABLE(result)->d = info.coord.el * rtod;
  return 0;
}

/*.......................................................................
 * Implement a function that returns the azimuth of the current source.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  args   VariableList *  The list of command-line arguments:
 *                          source -  The source to investigate.
 * Input/Output:
 *  result     Variable *  The return value.
 *  state      Variable *  Unused.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
static FUNC_FN(sc_azimuth_fn)
{
  SourceInfo info;   /* The horizon coordinates of the source */
  Variable *vsrc;    /* The source variable */
/*
 * Get the opaque resource object of the navigator thread.
 */
  Navigator *nav = (Navigator* )cp_Navigator((ControlProg* )sc->project);
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vsrc, NULL))
    return 1;
/*
 * Ask the navigator thread for the current horizon coordinates of the source.
 */
  if(nav_source_info(nav, SOURCE_VARIABLE(vsrc)->name, -1.0, 0.0, SIO_HORIZ,
		     &info))
    return 1;
/*
 * Return the azimuth in degrees.
 */
  DOUBLE_VARIABLE(result)->d = info.coord.az * rtod;
  return 0;
}

/*.......................................................................
 * Tell the navigator thread to read a given source catalog.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         file  -  The name of the source-catalog file.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_catalog_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project;   /* The
						     control-program
						     resource
						     container */
  NavigatorMessage msg;          /* A message to send to the navigator thread */
  Variable *vfile;     /* The file-name argument */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vfile, NULL))
    return 1;
/*
 * Send the source-catalog file-name to the navigator thread.
 */
  if(pack_navigator_catalog(&msg, STRING_VARIABLE(vfile)->string) ||
     send_NavigatorMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR)
    return 1;
  return 0;
}

/*-----------------------------------------------------------------------*
 * Transactions
 *-----------------------------------------------------------------------*/
/*.......................................................................
 * Tell the logger thread to read a transaction catalog
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         file  -  The name of the source-catalog file.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_loadTransaction_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource container
  LoggerMessage msg;          /* A message to send to the navigator thread */
  Variable* vfile;            /* The file-name argument */
  Variable* vclear;
  bool clear=true;

  // Get the command-line arguments.

  if(get_Arguments(args, &vfile, &vclear, NULL))
    return 1;

  // If the clear argument was specified, set it now

  if(OPTION_HAS_VALUE(vclear))
    clear = BOOL_VARIABLE(vclear)->boolvar;

  // Send the transaction catalog file-name to the logger thread

  if(pack_logger_transaction_catalog(&msg, STRING_VARIABLE(vfile)->string,
				     clear) ||
     send_LoggerMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR)
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that configures the transaction email list
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                       Optional arguments:
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_transactionEmailAddress_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vaction;
  Variable *vemail;
  char* email=0;
  OutputStream *output = sc->output; /* The stream wrapper around stdout */
                                 /*  real-time controller task */
  LoggerMessage msg;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vaction, &vemail, NULL))
    return 1;
  
  email = OPTION_HAS_VALUE(vemail) ? STRING_VARIABLE(vemail)->string : NULL;

  // See what was requested

  try {
    switch((EmailAction)CHOICE_VARIABLE(vaction)->choice) {
    case EMAIL_ADD:
      if(pack_logger_transaction_email(&msg, true, email) ||
	 send_LoggerMessage(cp, &msg, PIPE_WAIT)==PIPE_ERROR)
	return 1;
      break;
    case EMAIL_CLEAR:
      if(pack_logger_transaction_email(&msg, false, email) ||
	 send_LoggerMessage(cp, &msg, PIPE_WAIT)==PIPE_ERROR)
	return 1;
      break;
    case EMAIL_LIST:
      std::vector<std::string>* emailList = getTransactionEmailList(cp);

      for(unsigned i=0; i < emailList->size(); i++) {
	std::cout << emailList->at(i) << std::endl;
	if(output_printf(output, "%s\n", emailList->at(i).c_str())<0)
	  return 1;
      }
      std::cout << "Done" << std::endl;
      break;
    }
  } catch(...) {
    return 1;
  }

  return 0;
}

/*.......................................................................
 * Tell the logger thread to read a transaction catalog
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         file  -  The name of the source-catalog file.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_logTransaction_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource container
  LoggerMessage msg;          // A message to send to the navigator thread 
  Variable* vdevice;          // The device argument 
  Variable* vlocation;        // The location argument 
  Variable* vserial;          // The serial argument 
  Variable* vdate;            // The date
  Variable* vwho;             // A string identifying the culprit
  Variable* vcomment;         // A comment

  // Get the command-line arguments.

  if(get_Arguments(args, &vdevice, &vserial, &vlocation, &vdate, &vwho,
		   &vcomment, NULL))
    return 1;

  // Send the transaction to the logger thread
  
  if(pack_logger_log_transaction(&msg, 
				 TRANSDEV_VARIABLE(vdevice)->name,
				 STRING_VARIABLE(vserial)->string,
				 STRING_VARIABLE(vlocation)->string,
				 DOUBLE_VARIABLE(vdate)->d,
				 STRING_VARIABLE(vwho)->string,
				 OPTION_HAS_VALUE(vcomment) ?
				 STRING_VARIABLE(vcomment)->string : (char*)"") 
     || send_LoggerMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR)
    return 1;

  return 0;
}

/*-----------------------------------------------------------------------*
 * Scan-catalog commands and functions                                 *
 *-----------------------------------------------------------------------*/
/*.......................................................................
 * Tell the navigator thread to read a given scan catalog.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         file  -  The name of the source-catalog file.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_scan_catalog_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource container
  NavigatorMessage msg;          /* A message to send to the navigator thread */
  Variable *vfile;               /* The file-name argument */

  /*
   * Get the command-line arguments.
   */
  if(get_Arguments(args, &vfile, NULL))
    return 1;

  /*
   * Send the source-catalog file-name to the navigator thread.
   */
  if(pack_navigator_scan_catalog(&msg, STRING_VARIABLE(vfile)->string) ||
     send_NavigatorMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR)
    return 1;

  return 0;
}

/*.......................................................................
 * Tell the navigator thread to read a given ephemeris of UT1-UTC.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         file  -  The name of the ephemeris file.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_ut1utc_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource
						   container */
  NavigatorMessage msg;          /* A message to send to the navigator thread */
  Variable *vfile;               /* The file-name argument */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vfile, NULL))
    return 1;
/*
 * Send the ephemeris file-name to the navigator thread.
 */
  if(pack_navigator_ut1utc(&msg, STRING_VARIABLE(vfile)->string) ||
     send_NavigatorMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR)
    return 1;
  return 0;
}

/*.......................................................................
 * Tell the scheduler the default horizon to use when getting source
 * information from the navigator.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         angle  -  The elevation of the horizon.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_horizon_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource
						   container */
  Variable *vangle;              /* The horizon angle argument */
/*
 * Get the parent thread of this script.
 */
  Scheduler *sch = cp_Scheduler(cp);
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vangle, NULL))
    return 1;
/*
 * Record the new horizon.
 */
  return sch_set_horizon(sch, DOUBLE_VARIABLE(vangle)->d * dtor);
}

//-----------------------------------------------------------------------
// Scan commands
//-----------------------------------------------------------------------

/*.......................................................................
 * Implement the command that starts the telescope tracking a new
 * source.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        source  -  The source to be tracked.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_scan_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vscan;                /* The source to be tracked */

  /*
   * Do we have a controller to send the command to?
   */
  if(rtc_offline(sc, "scan"))
    return 1;

  /*
   * Get the command-line arguments.
   */
  if(get_Arguments(args, &vscan, NULL))
    return 1;

  /*
   * Tell the navigator thread to send a track command to the control
   * system, update its source catalog to show this as the current
   * source, and arrange to send position updates for it when
   * necessary.
   */
  unsigned antennas = cp_AntSet(cp)->getId();

  return nav_start_scan(cp_Navigator(cp),
			SCAN_VARIABLE(vscan)->name,
			sch_next_pmac_seq(sc, cp_Scheduler(cp), antennas));
}

/*.......................................................................
 * Display the statistics of a given scan.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         source  -  The name of the source.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_show_scan_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vscan;                   /* The source-name argument */
  char *name;                        /* The value of vsrc */
  ScanId id;                         /* The identification of the source */

  // Get the opaque resource object of the navigator thread.

  Navigator *nav = cp_Navigator(cp);
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vscan, NULL))
    return 1;

  name = SCAN_VARIABLE(vscan)->name;

  // Lookup the true name of the scan

  if(nav_print_scan_info(nav, name, 1, &id)) {
    lprintf(stderr, "show: Failed to lookup scan: %s\n", name);
    return 1;
  };
  return 0;
}

/*-----------------------------------------------------------------------*
 * Pointing model commands                                               *
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Implement the command that sets the collimation terms of the pointing
 * model.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Model model    -  The type of model (optical|radio).
 *                        Tilt size      -  The size of the collimation tilt.
 *                        DeckAngle dir  -  The deck angle at which the
 *                                          tilt is directed radially
 *                                          outwards.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_collimate_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable* vadd;
  Variable *vmodel;              /* The collimation axis */
  Variable *vaz;                /* The size of the azimuth offset */
  Variable *vel;                /* The size of the elevation offset */ 
  Variable* vant;
  RtcNetCmd rtc;                 /* The network object to be sent to the */
                                 /*  real-time controller task */
/*
 * Do we have a controller to send the command to?
 */
  if(rtc_offline(sc, "collimate"))
    return 1;
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vadd, &vmodel, &vaz, &vel, &vant, NULL))
    return 1;
/*
 * Compose the real-time controller network command.
 */
  rtc.antennas = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Only generate a new sequence number if the caller was a schedule

  rtc.cmd.collimate.seq     = sch_next_pmac_seq(sc, cp_Scheduler(cp), rtc.antennas);

  rtc.cmd.collimate.mode    = CHOICE_VARIABLE(vmodel)->choice;
  rtc.cmd.collimate.addMode = BOOL_VARIABLE(vadd)->boolvar ? 
    sza::util::OffsetMsg::ADD : sza::util::OffsetMsg::SET;

  // mill-arcsec 

  rtc.cmd.collimate.x = (long int)(DOUBLE_VARIABLE(vaz)->d * dtomas);
  rtc.cmd.collimate.y = (long int)(DOUBLE_VARIABLE(vel)->d * dtomas);
 
  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_COLLIMATE_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that sets the encoder scales and directions.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        az_turn - The # of azimuth encoder counts per turn.
 *                        el_turn - The # of elevation encoder counts per turn.
 *                        dk_turn - The # of deck encoder counts per turn.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_encoder_cals_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vaz_t;               /* Azimuth encoder counts per turn */
  Variable *vel_t;               /* Elevation encoder counts per turn */
  Variable *vdk_t;               /* Deck encoder counts per turn */
  Variable *vant;                /* Deck encoder counts per turn */
  RtcNetCmd rtc;                 /* The network object to be sent to the */
                                 /*  real-time controller task */
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "encoder_cals"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vaz_t, &vel_t, &vdk_t, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Only generate a new sequence number if the caller was a schedule

  rtc.cmd.encoder_cals.seq  = sch_next_pmac_seq(sc, cp_Scheduler(cp), rtc.antennas);

  rtc.cmd.encoder_cals.az   = INT_VARIABLE(vaz_t)->i;
  rtc.cmd.encoder_cals.el   = INT_VARIABLE(vel_t)->i;
  rtc.cmd.encoder_cals.dk   = INT_VARIABLE(vdk_t)->i;
  
  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_ENCODER_CALS_CMD))
    return 1;
  return 0;
}

/*.......................................................................
 * Implement the command that sets the encoder zero points.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        az - The encoder angle at zero azimuth.
 *                        el - The encoder angle at zero elevation.
 *                        dk - The encoder angle at the deck reference
 *                             position.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_encoder_zeros_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vaz,*vel;   /* Azimuth,elevation and deck zero points */
  Variable *vant;
  RtcNetCmd rtc;        /* The network object to be sent to the
			   real-time controller task */
/*
 * Do we have a controller to send the command to?
 */
  if(rtc_offline(sc, "encoder_zeros"))
    return 1;
  /*
   * Get the command-line arguments.
   */
  if(get_Arguments(args, &vaz, &vel, &vant, NULL))
    return 1;

  // Compose the real-time controller network command.

  rtc.antennas = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  
  // Compose the real-time controller network command.

  // Only generate a new sequence number if the caller was a schedule

  rtc.cmd.encoder_zeros.seq = sch_next_pmac_seq(sc, cp_Scheduler(cp), rtc.antennas);

  // Convert from degrees to radians

  rtc.cmd.encoder_zeros.az = (DOUBLE_VARIABLE(vaz)->d * dtor); 
  rtc.cmd.encoder_zeros.el = (DOUBLE_VARIABLE(vel)->d * dtor);
  
  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_ENCODER_ZEROS_CMD))
    return 1;
  return 0;
}

/*.......................................................................
 * Implement the command that tells the control system the encoder
 * limit terms of the pointing model.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        az_min - The minimum azimuth encoder count.
 *                        az_max - The maximum azimuth encoder count.
 *                        el_min - The minimum elevation encoder count.
 *                        el_max - The maximum elevation encoder count.
 *                        dk_min - The minimum deck encoder count.
 *                        dk_max - The maximum deck encoder count.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_encoder_limits_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vaz_min;             /* The upper azimuth encoder limit */
  Variable *vaz_max;             /* The lower azimuth encoder limit */
  Variable *vel_min;             /* The upper elevation encoder limit */
  Variable *vel_max;             /* The lower elevation encoder limit */
  Variable *vant;                /* The set of antennas */
  RtcNetCmd rtc;                 /* The network object to be sent to the */
                                 /*  real-time controller task */
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "encoder_limits"))
    return 1;

  // Get the command-line arguments.

  if(get_Arguments(args, &vaz_min, &vaz_max, &vel_min, &vel_max, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Only generate a new sequence number if the caller was a schedule

  rtc.cmd.encoder_limits.seq    = sch_next_pmac_seq(sc, cp_Scheduler(cp), rtc.antennas);

  rtc.cmd.encoder_limits.az_min = INT_VARIABLE(vaz_min)->i;
  rtc.cmd.encoder_limits.az_max = INT_VARIABLE(vaz_max)->i;
  rtc.cmd.encoder_limits.el_min = INT_VARIABLE(vel_min)->i;
  rtc.cmd.encoder_limits.el_max = INT_VARIABLE(vel_max)->i;

  // These are not used in SZA, and I have removed the arguments which
  // set them.  Leaving the message container to include space for a
  // third axis for now, however.

  rtc.cmd.encoder_limits.pa_min = 0;
  rtc.cmd.encoder_limits.pa_max = 0;
  
  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_ENCODER_LIMITS_CMD))
    return 1;
  return 0;
}

/*.......................................................................
 * Implement the command that tells the control system the axis-tilt
 * terms of the pointing model.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        ha      - The azimuth tilt in the direction of
 *                                  increasing hour angle.
 *                        lat     - The azimuth tilt in the direction of
 *                                  increasing latitude.
 *                        el      - The tilt of the elevation axis clockwise
 *                                  around the direction of the azimuth vector.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_tilts_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vha;                 /* The azimuth tilt parallel to hour angle */
  Variable *vlat;                /* The azimuth tilt parallel to latitude */
  Variable *vel;                 /* The elevation tilt */
  Variable* vant;
  RtcNetCmd rtc;                 /* The network object to be sent to the */
                                 /*  real-time controller task */
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "tilts"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vha, &vlat, &vel, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.antennas = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Only generate a new sequence number if the caller was a schedule
  
  rtc.cmd.tilts.seq = sch_next_pmac_seq(sc, cp_Scheduler(cp), rtc.antennas);

  // milli-arcsec 

  rtc.cmd.tilts.ha = (long int)(DOUBLE_VARIABLE(vha)->d * dtomas);        
  rtc.cmd.tilts.lat = (long int)(DOUBLE_VARIABLE(vlat)->d * dtomas);      
  rtc.cmd.tilts.el = (long int)(DOUBLE_VARIABLE(vel)->d * dtomas);        
  
  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_TILTS_CMD))
    return 1;
  return 0;
}

/*.......................................................................
 * Implement the command that tells the control system the gravitational
 * flexure term of the pointing model.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        flexure - The gravitational flexure of the mount
 *                                  per cosine of elevation.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_flexure_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vsflex;               /* The gravitational flexure of the mount */
  Variable *vcflex;               /* The gravitational flexure of the mount */
  Variable *vmodel;               /* The gravitational flexure of the mount */
  Variable* vant;
  RtcNetCmd rtc;                 /* The network object to be sent to the */
                                 /*  real-time controller task */
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "flexure"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vmodel, &vsflex, &vcflex, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.antennas = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Only generate a new sequence number if the caller was a schedule
  
  rtc.cmd.flexure.seq = sch_next_pmac_seq(sc, cp_Scheduler(cp), rtc.antennas);

  rtc.cmd.flexure.mode = CHOICE_VARIABLE(vmodel)->choice;

  // Radians per sine and cosine elevation

  rtc.cmd.flexure.sFlexure = (long)(DOUBLE_VARIABLE(vsflex)->d * dtomas);
  rtc.cmd.flexure.cFlexure = (long)(DOUBLE_VARIABLE(vcflex)->d * dtomas);
  
  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_FLEXURE_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that selects between the optical and radio
 * pointing models.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Model model  -  The model type (optical or radio).
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_model_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vmodel;              /* The model type */
  Variable* vant;
  RtcNetCmd rtc;                 /* The network object to be sent to the */
                                 /*  real-time controller task */
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "model"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vmodel, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.antennas = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Only generate a new sequence number if the caller was a schedule

  rtc.cmd.model.seq = sch_next_pmac_seq(sc, cp_Scheduler(cp), rtc.antennas);
  
  rtc.cmd.model.mode = CHOICE_VARIABLE(vmodel)->choice;
  
  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_MODEL_CMD))
    return 1;
  return 0;
}

/*.......................................................................
 * Implement the command that adds to pointing offsets on one or more
 * pointing axes.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        mode     -  Whether to add or replace any existing
 *                                    offsets.
 *                       Optional arguments:
 *                        az,el,dk -  Horizon pointing offsets.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_offset_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vadd;                /* The add-offset modifier argument */
  Variable *vaz,*vel,*vdk;       /* The optional horizon offsets */
  Variable* vant;
  RtcNetCmd rtc;                 /* The network object to be sent to the */
                                 /*  real-time controller task */
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "offset"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vadd, &vaz, &vel, &vdk, &vant, NULL))
    return 1;
  
  // Compose a network command for sending horizon offsets.

  rtc.antennas = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Only generate a new sequence number if the caller was a schedule

  rtc.cmd.mount_offset.seq = sch_next_pmac_seq(sc, cp_Scheduler(cp), rtc.antennas);

  rtc.cmd.mount_offset.axes = sza::util::OffsetMsg::NONE;
  rtc.cmd.mount_offset.mode = BOOL_VARIABLE(vadd)->boolvar ? 
    sza::util::OffsetMsg::ADD : sza::util::OffsetMsg::SET;

  if(OPTION_HAS_VALUE(vaz)) {
    rtc.cmd.mount_offset.axes |= sza::util::OffsetMsg::AZ;
    rtc.cmd.mount_offset.az = (long int)(DOUBLE_VARIABLE(vaz)->d * dtomas);
  } else {
    rtc.cmd.mount_offset.az = 0;
  };
  if(OPTION_HAS_VALUE(vel)) {
    rtc.cmd.mount_offset.axes |= sza::util::OffsetMsg::EL;
    rtc.cmd.mount_offset.el = (long int)(DOUBLE_VARIABLE(vel)->d * dtomas);
  } else {
    rtc.cmd.mount_offset.el = 0;
  };
  if(OPTION_HAS_VALUE(vdk)) {
    rtc.cmd.mount_offset.axes |= sza::util::OffsetMsg::PA;;
    rtc.cmd.mount_offset.dk = (long int)(DOUBLE_VARIABLE(vdk)->d * dtomas);
  } else {
    rtc.cmd.mount_offset.dk = 0;
  };
  
  // Send the message to the tracker.

  return queue_rtc_command(cp, &rtc, NET_MOUNT_OFFSET_CMD);
}

/*.......................................................................
 * Implement the command that adds to the equatorial pointing offsets.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        mode     -  Whether to add or replace any existing
 *                                    offsets.
 *                       Optional arguments:
 *                        ra,dec   -  Right Ascension and Declination offsets.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_radec_offset_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vadd;                /* The add-offset modifier argument */
  Variable *vra,*vdec;           /* The optional equatorial offsets */
  Variable* vant;
  RtcNetCmd rtc;                 /* The network object to be sent to the */
                                 /*  real-time controller task */
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "radec_offset"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vadd, &vra, &vdec, &vant, NULL))
    return 1;
  
  // Compose a network command for sending equatorial offsets.

  rtc.antennas = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Only generate a new sequence number if the caller was a schedule

  rtc.cmd.equat_offset.seq = sch_next_pmac_seq(sc, cp_Scheduler(cp), rtc.antennas);

  rtc.cmd.equat_offset.axes = sza::util::OffsetMsg::NONE;
  rtc.cmd.equat_offset.mode = BOOL_VARIABLE(vadd)->boolvar ? 
    sza::util::OffsetMsg::ADD : sza::util::OffsetMsg::SET;

  if(OPTION_HAS_VALUE(vra)) {
    rtc.cmd.equat_offset.axes |= sza::util::OffsetMsg::RA;
    rtc.cmd.equat_offset.ra = (long int)(DOUBLE_VARIABLE(vra)->d * dtomas);
  } else {
    rtc.cmd.equat_offset.ra = 0;
  };
  if(OPTION_HAS_VALUE(vdec)) {
    rtc.cmd.equat_offset.axes |= sza::util::OffsetMsg::DEC;
    rtc.cmd.equat_offset.dec = (long int)(DOUBLE_VARIABLE(vdec)->d * dtomas);
  } else {
    rtc.cmd.equat_offset.dec = 0;
  };
  
  // Send the message to the tracker.

  return queue_rtc_command(cp, &rtc, NET_EQUAT_OFFSET_CMD);
}

/*.......................................................................
 * Implement the command that arranges for the az and el tracking offsets
 * to be adjusted such that the image on the optical-pointing TV display
 * moves a given amount horizontally and vertically.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        right - The amount to move the image rightwards.
 *                        up    - The amount to move the image upwards.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_tv_offset_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vright;              /* The amount to move the image rightwards */
  Variable *vup;                 /* The amount to move the image upwards */
  Variable* vant;
  RtcNetCmd rtc;                 /* The network object to be sent to the */
                                 /*  real-time controller task */
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "tv_offset"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vright, &vup, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.antennas = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Only generate a new sequence number if the caller was a schedule

  rtc.cmd.tv_offset.seq = sch_next_pmac_seq(sc, cp_Scheduler(cp), rtc.antennas);

  rtc.cmd.tv_offset.right = (long int)(DOUBLE_VARIABLE(vright)->d * dtomas);
  rtc.cmd.tv_offset.up    = (long int)(DOUBLE_VARIABLE(vup)->d * dtomas);
  
  // Send the message to the tracker.

  return queue_rtc_command(cp, &rtc, NET_TV_OFFSET_CMD);
}

/*.......................................................................
 * Implement the command that configures the orientation angle of the
 * tv display.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        angle - The deck angle at which the TV display
 *                                shows the sky in its normal orientation.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_tv_angle_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vangle;              /* The orientation angle of the camera */
  Variable* vant;
  RtcNetCmd rtc;                 /* The network object to be sent to the */
                                 /*  real-time controller task */
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "tv_angle"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vangle, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.antennas = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  rtc.cmd.tv_angle.angle = (long int)(DOUBLE_VARIABLE(vangle)->d * dtomas);
  
  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_TV_ANGLE_CMD))
    return 1;
  return 0;
}

/*.......................................................................
 * The sky_offset command asks the telescope to track a point displaced
 * from the normal pointing center by a given fixed angle on the sky,
 * irrespective of the elevation or declination (ie. no cos(el) dependence).
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        mode     -  Whether to add or replace any existing
 *                                    offsets.
 *                       Optional arguments:
 *                        x,y      -  The offsets on the sky, with x directed
 *                                    towards the zenith along the great circle
 *                                    that connects the zenith to the pole, and
 *                                    y along the perpendicular great circle
 *                                    that goes through the zenith.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_sky_offset_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vadd;                /* The add-offset modifier argument */
  Variable *vx,*vy;              /* The orthoganol offsets to request */
  Variable* vant;
  RtcNetCmd rtc;                 /* The network object to be sent to the */
                                 /*  real-time controller task */
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "sky_offset"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vadd, &vx, &vy, &vant, NULL))
    return 1;
  
  // Compose a network command for sending horizon offsets.

  rtc.antennas = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Only generate a new sequence number if the caller was a schedule

  rtc.cmd.sky_offset.seq = sch_next_pmac_seq(sc, cp_Scheduler(cp), rtc.antennas);

  rtc.cmd.sky_offset.axes = sza::util::OffsetMsg::NONE;
  rtc.cmd.sky_offset.mode = BOOL_VARIABLE(vadd)->boolvar ? 
    sza::util::OffsetMsg::ADD : sza::util::OffsetMsg::SET;

  if(OPTION_HAS_VALUE(vx)) {
    rtc.cmd.sky_offset.axes |= sza::util::OffsetMsg::X;
    rtc.cmd.sky_offset.x = (long int)(DOUBLE_VARIABLE(vx)->d * dtomas);
  } else {
    rtc.cmd.sky_offset.x = 0;
  };
  if(OPTION_HAS_VALUE(vy)) {
    rtc.cmd.sky_offset.axes |= sza::util::OffsetMsg::Y;
    rtc.cmd.sky_offset.y = (long int)(DOUBLE_VARIABLE(vy)->d * dtomas);
  } else {
    rtc.cmd.sky_offset.y = 0;
  };
  
  // Send the message to the tracker.

  return queue_rtc_command(cp, &rtc, NET_SKY_OFFSET_CMD);
}

/*.......................................................................
 * Implement the command that tells the tracker how to position the
 * deck axis while tracking a source.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        DeckMode mode  -  The deck-axis tracking mode.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_deck_mode_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vmode;               /* The tracking mode */
  RtcNetCmd rtc;                 /* The network object to be sent to the */
                                 /*  real-time controller task */
/*
 * Do we have a controller to send the command to?
 */
  if(rtc_offline(sc, "deck_mode"))
    return 1;
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vmode, NULL))
    return 1;
/*
 * Compose the real-time controller network command.
 */
  // Only generate a new sequence number if the caller was a schedule

  unsigned antennas = cp_AntSet(cp)->getId();
  rtc.cmd.deck_mode.seq = sch_next_pmac_seq(sc, cp_Scheduler(cp), antennas);

  rtc.cmd.deck_mode.mode = CHOICE_VARIABLE(vmode)->choice;
/*
 * Send the command to the real-time controller.
 */
  if(queue_rtc_command(cp, &rtc, NET_DECK_MODE_CMD))
    return 1;
  return 0;
}

/*.......................................................................
 * Implement the command that reduces the slew rate of one or more
 * telescope axes.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        SlewRate az   -  The azimuth slew rate (%).
 *                        SlewRate el   -  The elevation slew rate (%).
 *                        SlewRate dk   -  The deck slew rate (%).
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_slew_rate_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable* vant;
  RtcNetCmd rtc;                 /* The network object to be sent to the */
                                 /*  real-time controller task */
  Variable *vaz;                 /* The optional azimuth slew rate */
  Variable *vel;                 /* The optional elevation slew rate */
  Variable *vdk;                 /* The optional deck slew rate */
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "slew_rate"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vaz, &vel, &vdk, &vant, NULL))
    return 1;
  
  // Compose the network command to be sent to the control system.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? 
    SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  rtc.cmd.slew_rate.seq = sch_next_pmac_seq(sc, cp_Scheduler(cp), rtc.antennas);
  rtc.cmd.slew_rate.mask = sza::antenna::control::Axis::NONE;
  
  // Determine which axes are to be slewed, and extract their target
  // positions.

  if(OPTION_HAS_VALUE(vaz)) {
    rtc.cmd.slew_rate.mask |= sza::antenna::control::Axis::AZ;
    rtc.cmd.slew_rate.az = UINT_VARIABLE(vaz)->uint;
  } else {
    rtc.cmd.slew_rate.az = 0;
  };

  if(OPTION_HAS_VALUE(vel)) {
    rtc.cmd.slew_rate.mask |= sza::antenna::control::Axis::EL;
    rtc.cmd.slew_rate.el = UINT_VARIABLE(vel)->uint;
  } else {
    rtc.cmd.slew_rate.el = 0;
  };

  if(OPTION_HAS_VALUE(vdk)) {
    rtc.cmd.slew_rate.mask |= sza::antenna::control::Axis::PA;
    rtc.cmd.slew_rate.dk = UINT_VARIABLE(vdk)->uint;
  } else {
    rtc.cmd.slew_rate.dk = 0;
  };
  
  // Send the command to the real-time controller.
  
  if(queue_rtc_command(cp, &rtc, NET_SLEW_RATE_CMD))
    return 1;
  return 0;
}

/*.......................................................................
 * Implement the command that controls what feature markers are to be
 * recorded with subsequent archive frames.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        FeatureChange what  - What to do with the
 *                                              specified set of features.
 *                        Features features   - The chosen set of features.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_mark_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vmode;               /* What to do with the feature set */
  Variable *vfeatures;           /* The set of features */
  RtcNetCmd rtc;                 /* The network object to be sent to the */
                                 /*  real-time controller task. */
/*
 * Do we have a controller to send the command to?
 */
  if(rtc_offline(sc, "mark"))
    return 1;
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vmode, &vfeatures, NULL))
    return 1;
/*
 * Compose the network command that is to be sent to the real-time
 * controller.
 */
  rtc.cmd.feature.seq = sch_next_mark_seq(sc, cp_Scheduler(cp));
  rtc.cmd.feature.mode = CHOICE_VARIABLE(vmode)->choice;
  rtc.cmd.feature.mask = SET_VARIABLE(vfeatures)->set;
/*
 * Send the command to the real-time controller.
 */
  if(queue_rtc_command(cp, &rtc, NET_FEATURE_CMD))
    return 1;
  return 0;
}

/*.......................................................................
 * Implement the command that forces the current frame to be written,
 * and new frame to be started
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        FeatureChange what  - What to do with the
 *                                              specified set of features.
 *                        Features features   - The chosen set of features.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_newFrame_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */

  ArchiverMessage msg;

  if(pack_archiver_newFrame(&msg, sch_next_frame_seq(sc, cp_Scheduler(cp))) ||
     send_ArchiverMessage(cp, &msg, PIPE_WAIT) == PIPE_ERROR)
    return 1;

  return 0;
}

/*-----------------------------------------------------------------------*
 * Host environment commands and functions                               *
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Change the current working directory of the process.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of optional command-line arguments:
 *                        Dir path  -  The pathname of the new working
 *                                     directory.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_cd_cmd)
{
  Variable *vpath;               /* The path of the new directory */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vpath, NULL))
    return 1;
/*
 * Attempt to change the directory.
 */
  if(set_working_directory(STRING_VARIABLE(vpath)->string))
    return 1;
  return 0;
}

/*.......................................................................
 * Display the current working directory.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of optional command-line arguments:
 *                        (none)
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_pwd_cmd)
{
/*
 * Get the path name.
 */
  char *path = get_working_directory();
  if(!path)
    return 1;
/*
 * Display the pathname to the user.
 */
  lprintf(stdout, "%s\n", path);
/*
 * Discard the redundant copy of the path name.
 */
  free(path);
  return 0;
}

/*.......................................................................
 * Add a variable containing the name of the current host to the current
 * scripting environment.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  name         char *  The name to give the variable.
 * Output:
 *  return   Variable *  The new variable, or NULL on error.
 */
static Variable *add_hostname_variable(Script *sc, char *name)
{
  Variable *var;   /* The new variable */
  char *host;      /* The name of the host computer */
/*
 * Lookup the hostname datatype.
 */
  DataType *dt = find_DataType(sc, NULL, "Hostname");
  if(!dt)
    return NULL;
/*
 * Get the name of the current host.
 */
  host = get_name_of_host();
  if(!host)
    return NULL;
/*
 * Create the variable.
 */
  var = add_BuiltinVariable(sc, "Hostname hostname");
  if(!var)
    return NULL;
/*
 * Allocate a copy of this string from the string segment of the program.
 */
  STRING_VARIABLE(var)->string = new_ScriptString(sc, host);
  free(host);
  if(!STRING_VARIABLE(var)->string)
    return NULL;
/*
 * Mark the variable as having a constant value.
 */
  var->flags = VAR_IS_CONST;
  return var;
}

/*.......................................................................
 * This is the constructor for a schedule-specific data object.
 */
static SC_NEW_FN(new_sch_data)
{
/*
 * Attempt to allocate the data object.
 */
  ScheduleData *data = (ScheduleData* )malloc(sizeof(ScheduleData));
  if(!data) {
    lprintf(stderr, "Insufficient memory to allocate schedule data.\n");
    return NULL;
  };
/*
 * Before attempting any operation that might fail, initialize the container
 * at least up to the point at which it can be safely passed to del_sch_data().
 */
  data->ref_count = 1;
  data->schedules = new_ScriptList(sc);
  if(!data->schedules) {
    lprintf(stderr,
	    "Insufficient memory to allocate list of script schedules.\n");
    return del_sch_data(sc, data);
  };
  return data;
}

/*.......................................................................
 * Garbage collect any non-script objects allocated during compilation
 * of a schedule.
 */
static SC_CLR_FN(clr_sch_data)
{
/*
 * Get the resource object of the parent thread.
 */
  Scheduler *sch = (Scheduler* )cp_ThreadData((ControlProg *)sc->project, 
					      CP_SCHEDULER);
/*
 * Get the script-specific data.
 */
  ScheduleData *sd = (ScheduleData* )data;
/*
 * Discard any schedules that aren't currently queued to be run.
 */
  while(sd->schedules->head) {
    Script *sched = (Script* )del_ListNode(sd->schedules, sd->schedules->head, 
					   NULL);
    sch_discard_schedule(sch, sched);
  };
  return 0;
}

/*.......................................................................
 * This is the destructor for a schedule-specific data object.
 */
static SC_DEL_FN(del_sch_data)
{
  if(data) {
    ScheduleData *sd = (ScheduleData* )data;
/*
 * Delete objects that were allocated while compiling the script.
 */
    clr_sch_data(sc, data);
    sd->schedules = NULL;  /* The list will be deleted by del_Script() */
    free(data);
  };
  return NULL;
}

/*-----------------------------------------------------------------------*
 * Thermometry commands.
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Implement the command that specifies the time interval at which
 * the temperatures of the DS1820 digital thermometers are read.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                          dt  -  The time interval for power
 *                                 measurements (seconds).
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_thermo_readout_interval_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vdt;                 /* The interval between power measurements */
  RtcNetCmd rtc;                 /* The network object to be sent to the */
                                 /*  real-time controller task */
/*
 * Do we have a controller to send the command to?
 */
  if(rtc_offline(sc, "thermo_readout_interval"))
    return 1;
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vdt, NULL))
    return 1;
/*
 * Compose the real-time controller network command.
 *
 * Note that the time interval arguments record intervals in floating
 * point hours, whereas the network command requires integer seconds.
 */
  if(DOUBLE_VARIABLE(vdt)->d > 0)
    rtc.cmd.ds_dt.seconds = (long int)floor(DOUBLE_VARIABLE(vdt)->d + 0.5);
  else
    rtc.cmd.ds_dt.seconds = 0;
/*
 * Send the command to the real-time controller.
 */
  if(queue_rtc_command(cp, &rtc, NET_DS_DT_CMD))
    return 1;
  return 0;
}
/*.......................................................................
 * Implement the command that sends commands to the digital 
 * thermomemeters.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         cmd  -  The type of command to be sent.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_thermo_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project;/* The control-program
						  resource object */
  Variable *vcommand=NULL;      /* The type of command to be sent */
  Variable *vaddress=NULL;      /* The type of command to be sent */
  char *address=NULL;
  Variable *vindex=NULL;        /* The type of command to be sent */
  RtcNetCmd rtc;                /* The network object to be sent to the 
				   real-time controller task */
  /*
   * Do we have a controller to send the command to?
   */
  if(rtc_offline(sc, "thermo"))
    return 1;
  /*
   * Get the command-line arguments.
   */
  if(get_Arguments(args, &vcommand, &vaddress, &vindex, NULL))
    return 1;
  /*
   * Send the appropriate command.
   */
  rtc.cmd.thermometer.command = CHOICE_VARIABLE(vcommand)->choice;

  address = OPTION_HAS_VALUE(vaddress) ? STRING_VARIABLE(vaddress)->string : 
    NULL;

  rtc.cmd.thermometer.index = OPTION_HAS_VALUE(vindex) ? 
    UINT_VARIABLE(vindex)->uint : (unsigned int)(-1);

  /*
   * See if this is one of the commands which requires further arguments
   */
  switch (rtc.cmd.thermometer.command) {
  case DS_FLAG_CMD:
  case DS_UNFLAG_CMD:
  case DS_ADDRESS_CMD:
  case DS_ROMID_CMD:
  case DS_READ_CMD:
    {
      int doall=0;
      int len;

      if(address==NULL) {
	lprintf(stderr,"thermo_cmd: No ROM id specified\n");
	return 1;
      }
      len = strlen(address);
      if(strcmp(address,"all")==0) {
	doall = 1;
	if(rtc.cmd.thermometer.command != DS_FLAG_CMD && 
	   rtc.cmd.thermometer.command != DS_UNFLAG_CMD) {
	  lprintf(stderr,"thermo_cmd: option \"all\" not supported for this command\n");
	  return 1;
	}
      }
      if(!doall && len != 16) {
	lprintf(stderr,"thermo_cmd: ROM id must have 16 characters\n");
	return 1;
      }
      else {
	int irom;
	for(irom=0;irom < 16;irom++) 
	  rtc.cmd.thermometer.address[irom] = address[irom];
      }
    }
    break;
  case DS_RESET_CMD:
  case DS_SEARCH_CMD:
  case DS_READALL_CMD:
  case DS_DISPLAY_CMD:
  case DS_CHECK_CMD:
  case DS_NOCHECK_CMD:
    break;
  }
  /*
   * Send the command to the real-time controller.
   */
  if(queue_rtc_command(cp, &rtc, NET_THERMO_CMD))
    return 1;

  return 0;
}
/*.......................................................................
 * Implement the command that controls the optical camera stepper motor,
 * camera and camera controls.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                       Target        - The target of the command
 *                       OptCamCount   - The step count, or any of a number
 *                                       of keywords.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_optcam_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *count;               /* The desired step count */
  Variable *target;              /* The desired step count */
  Variable *vant;                 // The desired antennas
  NetCmdId type;                 /* The type of network command being sent */
  RtcNetCmd rtc;                 /* The network object to be sent to the 
				    real-time controller task */
/*
 * Do we have a controller to send the command to?
 */
  if(rtc_offline(sc, "optcam"))
    return 1;
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &target, &count, &vant, NULL))
    return 1;
  /*
   * Compose the appropriate real-time controller network command.
   */
  rtc.cmd.camera.target = CHOICE_VARIABLE(target)->choice;
  type = NET_CAMERA_CMD;
      
  switch(CHOICE_VARIABLE(target)->choice) {
    /*
     * If this was a command to cycle power to the camera, send a control
     * command.
     */
  case OPTCAM_FRAME:
    switch((OptCamAction)INT_VARIABLE(count)->i) {
      /*
       * If this is a frame grabber command, send it through the
       * scheduler thread, so it can keep track of when a new frame
       * has been received.
       */
    case OPTCAM_GRAB:
      {
      	Scheduler *sch = (Scheduler* )cp_ThreadData((ControlProg *)sc->project, 
						   CP_SCHEDULER);
	return sch_grab_send(sch);
      }
      break;
    case OPTCAM_CENTER:
      {
	double xoff, yoff;
	/*
	 * Get the offsets from the grabber thread. (in mas)
	 */
	grabber_offset_info(cp, xoff, yoff);

	/*
	 * Check that we have a controller to send the command to.
	 */
	if(rtc_offline(sc, "tv_offset"))
	  return 1;
	/*
	 * Compose the real-time controller network command.
	 */
	rtc.antennas = 
	  OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

	rtc.cmd.tv_offset.seq = sch_next_pmac_seq(sc, cp_Scheduler(cp), rtc.antennas);
	rtc.cmd.tv_offset.right = (long int)(xoff * sza::util::Angle::masPerDegree_);
	rtc.cmd.tv_offset.up    = (long int)(-yoff * sza::util::Angle::masPerDegree_);

	type = NET_TV_OFFSET_CMD;
      }
      break;
    default:
      lprintf(stderr, "sc_camera_cmd: Invalid camera function.\n");
      return 1;
    }
    break;
  case OPTCAM_CONTROLLER:
    switch((OptCamAction)INT_VARIABLE(count)->i) {
    case OPTCAM_ID:
    case OPTCAM_RBC:
      rtc.cmd.camera.action = (OptCamAction)INT_VARIABLE(count)->i;
      break;
    default:
      lprintf(stderr, "sc_camera_cmd: Invalid camera controller function.\n");
      return 1;
    }
    break;
  case OPTCAM_CAMERA:
    switch((OptCamAction)INT_VARIABLE(count)->i) {
    case OPTCAM_ON:
      rtc.cmd.optcam_cntl.on = 1;
      rtc.cmd.optcam_cntl.target = OPTCAM_CAMERA;
      type = NET_OPTCAM_CNTL_CMD;
      break;
    case OPTCAM_OFF:
      rtc.cmd.optcam_cntl.on = 0;
      rtc.cmd.optcam_cntl.target = OPTCAM_CAMERA;
      type = NET_OPTCAM_CNTL_CMD;
      break;
    case OPTCAM_PRESET:
      rtc.cmd.camera.action = (OptCamAction)INT_VARIABLE(count)->i;
      break;
    default:
      lprintf(stderr, "sc_camera_cmd: Invalid camera function.\n");
      return 1;
    }
    break;
  case OPTCAM_IRIS:
    switch((OptCamAction)INT_VARIABLE(count)->i) {
    case OPTCAM_OPEN:
    case OPTCAM_CLOSE:
    case OPTCAM_STOP:
    case OPTCAM_PRESET:
      rtc.cmd.camera.action = (OptCamAction)INT_VARIABLE(count)->i;
      break;
    default:
      lprintf(stderr, "sc_camera_cmd: Invalid iris function.\n");
      return 1;
    }
    break;
  case OPTCAM_SHUT:
    switch((OptCamAction)INT_VARIABLE(count)->i) {
    case OPTCAM_ON:
    case OPTCAM_OFF:
      rtc.cmd.camera.action = (OptCamAction)INT_VARIABLE(count)->i;
      break;
    default:
      switch(INT_VARIABLE(count)->i) {
      case 100:
	rtc.cmd.camera.action = OPTCAM_100;
	break;
      case 250:
	rtc.cmd.camera.action = OPTCAM_250;
	break;
      case 500:
	rtc.cmd.camera.action = OPTCAM_500;
	break;
      case 1000:
	rtc.cmd.camera.action = OPTCAM_1000;
	break;
      case 2000:
	rtc.cmd.camera.action = OPTCAM_2000;
	break;
      case 4000:
	rtc.cmd.camera.action = OPTCAM_4000;
	break;
      case OPTCAM_10000:
	rtc.cmd.camera.action = OPTCAM_10000;
	break;
      case OPTCAM_INC:
      case OPTCAM_DEC:
	rtc.cmd.camera.action = (OptCamAction)INT_VARIABLE(count)->i;
	break;
      default: 
	lprintf(stderr, "sc_camera_cmd: Invalid shutter function.\n");
	return 1;
      }
    }
    break;
  case OPTCAM_SENS_AUTO:
    switch((OptCamAction)INT_VARIABLE(count)->i) {
    case OPTCAM_ON:
    case OPTCAM_OFF:
    case OPTCAM_INC:
    case OPTCAM_DEC:
      rtc.cmd.camera.action = (OptCamAction)INT_VARIABLE(count)->i;
      break;
    default:
      lprintf(stderr, "sc_camera_cmd: Invalid sens_auto function.\n");
      return 1;
    }
    break;
  case OPTCAM_SENS_MANU:
    switch((OptCamAction)INT_VARIABLE(count)->i) {
    case OPTCAM_ON:
    case OPTCAM_OFF:
    case OPTCAM_INC:
    case OPTCAM_DEC:
      rtc.cmd.camera.action = (OptCamAction)INT_VARIABLE(count)->i;
      break;
      default:
	lprintf(stderr, "sc_camera_cmd: Invalid sens_manual function.\n");
	return 1;
    }
    break;
  case OPTCAM_AGC:
    switch((OptCamAction)INT_VARIABLE(count)->i) {
    case OPTCAM_ON:
    case OPTCAM_OFF:
    case OPTCAM_LOW:
    case OPTCAM_MID:
    case OPTCAM_HIGH:
      rtc.cmd.camera.action = (OptCamAction)INT_VARIABLE(count)->i;
      break;
    default:
      lprintf(stderr, "sc_camera_cmd: Invalid agc function.\n");
      return 1;
    }
    break;
  case OPTCAM_ALC:
    switch((OptCamAction)INT_VARIABLE(count)->i) {
    case OPTCAM_ON:
      rtc.cmd.camera.action = (OptCamAction)INT_VARIABLE(count)->i;
      break;
    default:
      lprintf(stderr, "sc_camera_cmd: Invalid alc function.\n");
      return 1;
    }
    break;
  case OPTCAM_MANU_IRIS:
    switch((OptCamAction)INT_VARIABLE(count)->i) {
    case OPTCAM_ON:
      rtc.cmd.camera.action = (OptCamAction)INT_VARIABLE(count)->i;
      break;
    default:
      lprintf(stderr, "sc_camera_cmd: Invalid manu_iris function.\n");
      return 1;
    }
    break;
  case OPTCAM_SUPERD:
    switch((OptCamAction)INT_VARIABLE(count)->i) {
    case OPTCAM_ON:
    case OPTCAM_OFF:
      rtc.cmd.camera.action = (OptCamAction)INT_VARIABLE(count)->i;
      break;
    default:
      lprintf(stderr, "sc_camera_cmd: Invalid super-d function.\n");
      return 1;
    }
    break;
  case OPTCAM_STEPPER:
    switch((OptCamAction)INT_VARIABLE(count)->i) {
    case OPTCAM_ON:
      rtc.cmd.optcam_cntl.on = 1;
      break;
    case OPTCAM_OFF:
      rtc.cmd.optcam_cntl.on = 0;
      break;
    default:
      lprintf(stderr, "sc_camera_cmd: Invalid camera function.\n");
      return 1;
    }
    rtc.cmd.optcam_cntl.target = OPTCAM_STEPPER;
    type = NET_OPTCAM_CNTL_CMD;
    break;
  case OPTCAM_FOCUS:
    switch((OptCamAction)INT_VARIABLE(count)->i) {
      /* Create a fake control command to stop the stepper motor. */
    case OPTCAM_STOP:
      rtc.cmd.optcam_cntl.on = 0;
      rtc.cmd.optcam_cntl.target = OPTCAM_FOCUS;
      type = NET_OPTCAM_CNTL_CMD;
      break;
    default:
	      rtc.cmd.stepper.count = INT_VARIABLE(count)->i;
	      type = NET_STEPPER_CMD;
      break;
    }
  }

  /*
   * Send the command to the real-time controller.
   */
  if(queue_rtc_command(cp, &rtc, type))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that sets the frame grabber input channel
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         register -  The name of the register to write to.
 *                         val      -  The value to write.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_configureFrameGrabber_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project;  /* The context of the
						    host control
						    program */
  Variable *vchan;      /* The channel */
  Variable *vcombine;   /* The number of images to combine */
  Variable *vflatfield; /* Flatfield the image? */
  RtcNetCmd rtc;    /* The network command object to be sent to the */
                    /*  real-time controller task */
  
  // Do we have a controller to send the command to?
  
  if(rtc_offline(sc, "configureFrameGrabber"))
    return 1;
  
  // Get the command-line arguments.
  
  if(get_Arguments(args, &vchan, &vcombine, &vflatfield, NULL))
    return 1;
  
  // Compose a network object to be sent to the real-time controller
  // task.
  
  rtc.cmd.configureFrameGrabber.mask    = FG_NONE;
  
  if(OPTION_HAS_VALUE(vchan)) {
    rtc.cmd.configureFrameGrabber.channel = UINT_VARIABLE(vchan)->uint;
    rtc.cmd.configureFrameGrabber.mask   |= FG_CHAN;
  }
  
  if(OPTION_HAS_VALUE(vcombine)) {
    rtc.cmd.configureFrameGrabber.nCombine = UINT_VARIABLE(vcombine)->uint;
    rtc.cmd.configureFrameGrabber.mask    |= FG_COMBINE;
  }

  if(OPTION_HAS_VALUE(vflatfield)) {
    rtc.cmd.configureFrameGrabber.flatfield = BOOL_VARIABLE(vflatfield)->boolvar;
    rtc.cmd.configureFrameGrabber.mask     |= FG_FLATFIELD;
  }

  // Queue the object to be sent to the controller.

  return queue_rtc_command(cp, &rtc, NET_CONFIGURE_FG_CMD);
}

/*.......................................................................
 * Implement the command that sets the optical camera fov
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         register -  The name of the register to write to.
 *                         val      -  The value to write.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setOpticalCameraFov_cmd)
{
  Variable *vfov;       // The FOV, in arcminutes

  // Get the command-line arguments.
  
  if(get_Arguments(args, &vfov, NULL))
    return 1;
  
  if(OPTION_HAS_VALUE(vfov)) 
    setOpticalCameraFov(sza::util::Angle(sza::util::Angle::ArcMinutes(), 
					 DOUBLE_VARIABLE(vfov)->d));
  else
    setOpticalCameraFov();

  return 0;
}

/*.......................................................................
 * Implement the command that sets the optical camera aspect
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         register -  The name of the register to write to.
 *                         val      -  The value to write.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setOpticalCameraAspect_cmd)
{
  Variable *vaspect;       /* The ASPECT */

  // Get the command-line arguments.
  
  if(get_Arguments(args, &vaspect, NULL))
    return 1;
  
  if(OPTION_HAS_VALUE(vaspect)) 
    setOpticalCameraAspect(DOUBLE_VARIABLE(vaspect)->d);
  else
    setOpticalCameraAspect();

  return 0;
}

/*.......................................................................
 * Implement the command that sets the optical camera collimation
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         register -  The name of the register to write to.
 *                         val      -  The value to write.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setOpticalCameraCollimation_cmd)
{
  Variable *vcollimation;       // The COLLIMATION, in degrees

  // Get the command-line arguments.
  
  if(get_Arguments(args, &vcollimation, NULL))
    return 1;
  
  if(OPTION_HAS_VALUE(vcollimation)) 
    setOpticalCameraCollimation(sza::util::Angle(sza::util::Angle::Degrees(),
						 DOUBLE_VARIABLE(vcollimation)->d));
  else
    setOpticalCameraCollimation();

  return 0;
}

/*.......................................................................
 * Implement the command that enables/disables flat fielding of frame
 * grabber images.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         on      -  true|false (1|0): do we flatfield?
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.  */
static CMD_FN(sc_flatfield_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project;  /* The context of the
						    host control
						    program */
  Variable *vstate; /* The switch argument */
  RtcNetCmd rtc;    /* The network command object to be sent to the */
                    /*  real-time controller task */
/*
 * Do we have a controller to send the command to?
 */
  if(rtc_offline(sc, "flatfield"))
    return 1;
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vstate, NULL))
    return 1;
/*
 * Compose a network object to be sent to the real-time controller task.
 */
  rtc.cmd.flatfield.on = CHOICE_VARIABLE(vstate)->choice==SWITCH_ON;
/*
 * Queue the object to be sent to the controller.
 */
  return queue_rtc_command(cp, &rtc, NET_FLATFIELD_CMD);
}

/*.......................................................................
 * Implement a function that returns the peak offsets of the current frame
 * grabber image.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  args   VariableList *  The list of command-line arguments:
 *                          source -  The source to investigate.
 * Input/Output:
 *  result     Variable *  The return value.
 *  state      Variable *  Unused.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
static FUNC_FN(sc_peak_fn)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vpeak;    /* The source variable */
  double xoff, yoff;  /* The offset of the peak of the fg image */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vpeak, NULL))
    return 1;
  /*
   * Get the offsets from the grabber thread. (in mas)
   */
  grabber_offset_info(cp, xoff, yoff);

/*
 * Return the requested offset in degrees (to be consistent with the 
 * PointingOffset data type.
 */
  switch (CHOICE_VARIABLE(vpeak)->choice) {
  case PEAK_X:
    DOUBLE_VARIABLE(result)->d = xoff;
    break;
  case PEAK_Y:
    DOUBLE_VARIABLE(result)->d = yoff;
    break;
  case PEAK_XABS:
    DOUBLE_VARIABLE(result)->d = fabs(xoff);
    break;
  case PEAK_YABS:
    DOUBLE_VARIABLE(result)->d = fabs(yoff);
    break;
  default:
    break;
  }
  return 0;
}
/*.......................................................................
 * Implement a function that returns the snr of the peak of the current frame
 * grabber image.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  args   VariableList *  The list of command-line arguments:
 *                          source -  The source to investigate.
 * Input/Output:
 *  result     Variable *  The return value.
 *  state      Variable *  Unused.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
static FUNC_FN(sc_imstat_fn)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vpeak;    /* The source variable */
  double snr;         /* The SNR of the peak of the fg image */
  double peak;         /* The peak pixel value */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vpeak, NULL))
    return 1;
  /*
   * Get the snr from the grabber thread. 
   */
  if(grabber_peak_info(cp, &peak, &snr))
    return 1;
/*
 * Return the requested statistic as a double.
 */
  switch (CHOICE_VARIABLE(vpeak)->choice) {
  case IMSTAT_SNR:
    DOUBLE_VARIABLE(result)->d = snr;
    break;
  case IMSTAT_PEAK:
    DOUBLE_VARIABLE(result)->d = peak;
    break;
  default:
    break;
  }
  return 0;
}

//-----------------------------------------------------------------------
// Downconverter commands.
//-----------------------------------------------------------------------

/*.......................................................................
 * Implement the command that sets the Psys power level.
 */
static CMD_FN(sc_psys_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vpower;              // Power level
  Variable* vant;                // Antennas
  Variable *vband;               // Bands
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "psys"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vpower, &vant, &vband, NULL))
    return 1;
  
  // Compose a network command for sending the total power 

  // Default to all antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // Default to all bands if none was given.

  if(OPTION_HAS_VALUE(vband))
    rtc.cmd.psys.bands = SET_VARIABLE(vband)->set;
  else
    rtc.cmd.psys.bands = sza::util::CorrelatorBand::BANDALL;
    
  // Set the power. A value > 10000 means set to a preset level.

  if(DOUBLE_VARIABLE(vpower)->d > 10000) {
    rtc.cmd.psys.power = -1;
    rtc.cmd.psys.preset = true;
  } else {
    rtc.cmd.psys.power = DOUBLE_VARIABLE(vpower)->d;
    rtc.cmd.psys.preset = false;
  }

  return queue_rtc_command(cp, &rtc, NET_PSYS_CMD);
}

/*.......................................................................
 * Implement the command that sets the Psys atten level.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        atten -  The requested attenuation
 *
 *                       Optional arguments:
 *
 *                        antennas -  The requested antennas
 *                        bands    -  The requested bands
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_psysAtten_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vatten;              // Attenuation
  Variable* vant;                // Antennas
  Variable* vband;               // Bands
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "psysAtten"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vatten, &vant, &vband, NULL))
    return 1;
  
  // Compose a network command for sending the total atten 

  rtc.cmd.psys_atten.atten = DOUBLE_VARIABLE(vatten)->d;

  // Default to all antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // Default to all bands if none was given.

  if(OPTION_HAS_VALUE(vband))
    rtc.cmd.psys_atten.bands = SET_VARIABLE(vband)->set;
  else
    rtc.cmd.psys_atten.bands = sza::util::CorrelatorBand::BANDALL;
    
  return queue_rtc_command(cp, &rtc, NET_PSYS_ATTEN_CMD);
}

/*.......................................................................
 * Implement the command that sets the Ifout power level.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        power -  The requested power
 *
 *                       Optional arguments:
 *
 *                        antennas -  The requested antennas
 *                        bands    -  The requested bands
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_ifout_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vpower;              // Power level
  Variable* vant;                // Antennas
  Variable* vband;               // Bands
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "ifout"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vpower, &vant, &vband, NULL))
    return 1;
  
  // Compose a network command for sending the total power 

  // Default to all antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // Default to all bands if none was given.

  if(OPTION_HAS_VALUE(vband))
    rtc.cmd.ifout.bands = SET_VARIABLE(vband)->set;
  else
    rtc.cmd.ifout.bands = sza::util::CorrelatorBand::BANDALL;
    
  // Set the power. A negative value means set to a preset level.

  if(DOUBLE_VARIABLE(vpower)->d < 0) {
    rtc.cmd.ifout.power = -1;
    rtc.cmd.ifout.preset = true;
  } else {
    rtc.cmd.ifout.power = DOUBLE_VARIABLE(vpower)->d;
    rtc.cmd.ifout.preset = false;
  }

  return queue_rtc_command(cp, &rtc, NET_IFOUT_CMD);
}

/*.......................................................................
 * Implement the command that sets the Ifout atten level.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        atten -  The requested attenuation
 *
 *                       Optional arguments:
 *
 *                        antennas -  The requested antennas
 *                        bands    -  The requested bands
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_ifoutAtten_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vatten;              // Attenuation
  Variable *vant;                // Antennas
  Variable *vband;               // Bands
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "ifoutAtten"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vatten, &vant, &vband, NULL))
    return 1;
  
  // Compose a network command for sending the total atten 

  rtc.cmd.ifout_atten.atten = DOUBLE_VARIABLE(vatten)->d;

  // Default to all antennas if none was given.


  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // Default to all bands if none was given.

  if(OPTION_HAS_VALUE(vband))
    rtc.cmd.ifout_atten.bands = SET_VARIABLE(vband)->set;
  else
    rtc.cmd.ifout_atten.bands = sza::util::CorrelatorBand::BANDALL;
    
  return queue_rtc_command(cp, &rtc, NET_IFOUT_ATTEN_CMD);
}

/*.......................................................................
 * Implement the command that enables the RF amplifier
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 * 
 *                        state - on|off
 *
 *                       Optional arguments:
 *
 *                        antennas -  The requested antennas
 *                        bands    -  The requested bands
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_rfamp_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vstate;              // state: on|off
  Variable *vant;                // Antennas
  Variable *vband;               // Bands
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "rfamp"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vstate, &vant, &vband, NULL))
    return 1;
  
  // Compose a network command for sending the message

  rtc.cmd.rf_amp.enable = (CHOICE_VARIABLE(vstate)->choice == SWITCH_ON);

  // Default to all antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // Default to all bands if none was given.

  if(OPTION_HAS_VALUE(vband))
    rtc.cmd.rf_amp.bands = SET_VARIABLE(vband)->set;
  else
    rtc.cmd.rf_amp.bands = sza::util::CorrelatorBand::BANDALL;
    
  return queue_rtc_command(cp, &rtc, NET_RF_AMP_CMD);
}

/*.......................................................................
 * Implement the command that enables the IF auto level control
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        state - on|off
 *
 *                       Optional arguments:
 *
 *                        antennas -  The requested antennas
 *                        bands    -  The requested bands
 * 
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_ifalc_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vstate;              // state: on|off
  Variable *vant;                // Antennas
  Variable *vband;               // Bands
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "ifalc"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vstate, &vant, &vband, NULL))
    return 1;
  
  // Compose a network command for sending the message

  rtc.cmd.if_alc.enable = (CHOICE_VARIABLE(vstate)->choice == SWITCH_ON);

  // Default to all antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // Default to all bands if none was given.

  if(OPTION_HAS_VALUE(vband))
    rtc.cmd.if_alc.bands = SET_VARIABLE(vband)->set;
  else
    rtc.cmd.if_alc.bands = sza::util::CorrelatorBand::BANDALL;
    
  return queue_rtc_command(cp, &rtc, NET_IF_ALC_CMD);
}

//-----------------------------------------------------------------------
// Noise Source commands
//-----------------------------------------------------------------------

/*.......................................................................
 * Implement the command that sets the noise source power level.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        power -  The requested power
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_noisePower_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vpower;              // Power level
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "noisePower"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vpower, NULL))
    return 1;
  
  // Compose a network command for sending the total power 

  // Set the power. A negative value means set to a preset level.

  if(DOUBLE_VARIABLE(vpower)->d < 0) {
    rtc.cmd.noise_power.power = -1;
    rtc.cmd.noise_power.preset = true;
  } else {
    rtc.cmd.noise_power.power = DOUBLE_VARIABLE(vpower)->d;
    rtc.cmd.noise_power.preset = false;
  }

  return queue_rtc_command(cp, &rtc, NET_NOISE_POWER_CMD);
}

/*.......................................................................
 * Implement the command that sets the noise source atten level.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        atten -  The requested attenuation
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_noiseAtten_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vatten;              // Attenuation
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "noiseAtten"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vatten, NULL))
    return 1;
  
  // Compose a network command for sending the total atten 

  rtc.cmd.noise_atten.atten = UINT_VARIABLE(vatten)->uint;

  return queue_rtc_command(cp, &rtc, NET_NOISE_ATTEN_CMD);
}

/*.......................................................................
 * Implement the command that sets the tone source atten level.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        atten -  The requested attenuation
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_toneAtten_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vatten;              // Attenuation
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "toneAtten"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vatten, NULL))
    return 1;
  
  // Compose a network command for sending the total atten 

  rtc.cmd.tone_atten.atten = UINT_VARIABLE(vatten)->uint;

  return queue_rtc_command(cp, &rtc, NET_TONE_ATTEN_CMD);
}

/**.......................................................................
 * Implement the command that turns just the noise diode on/off
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        state - on|off
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_noiseDiode_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vstate;              // state: on|off
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "noise"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vstate, NULL))
    return 1;
  
  // Compose a network command for sending the message

  rtc.cmd.noise.enable = (CHOICE_VARIABLE(vstate)->choice == SWITCH_ON);
  rtc.cmd.noise.mask   = NOISE_SIMPLE;

  return queue_rtc_command(cp, &rtc, NET_NOISE_CMD);
}

/**.......................................................................
 * Implement the command that configures the control system for
 * noise/rf
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        state - on|off
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_noise_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vstate;              // state: on|off
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "noise"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vstate, NULL))
    return 1;
  
  // Compose a network command for sending the message

  rtc.cmd.noise.enable = (CHOICE_VARIABLE(vstate)->choice == SWITCH_ON);
  rtc.cmd.noise.mask   = NOISE_ALL;
  rtc.cmd.noise.seq    = sch_next_noise_seq(sc, cp_Scheduler(cp));

  return queue_rtc_command(cp, &rtc, NET_NOISE_CMD);
}

/*.......................................................................
 * Implement the command that turns the tone on/off
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        state - on|off
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_tone_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vstate;              // state: on|off
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "tone"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vstate, NULL))
    return 1;
  
  // Compose a network command for sending the message

  rtc.cmd.tone.enable = (CHOICE_VARIABLE(vstate)->choice == SWITCH_ON);

  return queue_rtc_command(cp, &rtc, NET_TONE_CMD);
}

//-----------------------------------------------------------------------
// Quad Mod commands
//-----------------------------------------------------------------------

/*.......................................................................
 * Implement the command that sets the quad source power level.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        power -  The requested power
 *
 *                       Optional arguments:
 *
 *                        antennas -  The requested antennas

 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_quadPower_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vant;                // Antennas
  Variable* vpower;              // Power level
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "quadPower"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vpower, &vant, NULL))
    return 1;
  
  // Compose a network command for sending the total power 

  // Default to all antennas if none was given.

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Set the power. A negative value means set to a preset level.

  if(DOUBLE_VARIABLE(vpower)->d < 0) {
    rtc.cmd.quad_power.power = -1;
    rtc.cmd.quad_power.preset = true;
  } else {
    rtc.cmd.quad_power.power = DOUBLE_VARIABLE(vpower)->d;
    rtc.cmd.quad_power.preset = false;
  }

  return queue_rtc_command(cp, &rtc, NET_QUAD_POWER_CMD);
}

/**.......................................................................
 * Implement the command that sets the quad source atten level.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        atten -  The requested attenuation
 *
 *                       Optional arguments:
 *
 *                        antennas -  The requested antennas
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_quadAtten_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vant;                // Antennas
  Variable* vatten;              // Attenuation
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "quadAtten"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vatten, &vant, NULL))
    return 1;
  
  // Default to all antennas if none was given.

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Compose a network command for sending the total atten 

  rtc.cmd.quad_atten.atten = UINT_VARIABLE(vatten)->uint;

  return queue_rtc_command(cp, &rtc, NET_QUAD_ATTEN_CMD);
}

/**.......................................................................
 * Implement the command that sets the quad mod's walshstate column
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        atten -  The requested attenuation
 *
 *                       Optional arguments:
 *
 *                        antennas -  The requested antennas
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_quadWalshCol_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vant;                // Antennas
  Variable* vcolumn;             // The column index
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "quadWalshCol"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vcolumn, &vant, NULL))
    return 1;
  
  // Default to all antennas if none was given.

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Compose a network command for sending the Walsh column 

  rtc.cmd.quad_walshcol.column = UINT_VARIABLE(vcolumn)->uint;

  return queue_rtc_command(cp, &rtc, NET_QUAD_WALSH_COLUMN_CMD);
}

/**.......................................................................
 * Implement the command that turns the quad on/off
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        state - on|off
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_quad_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vant;                // Antennas
  Variable *vstate;              // state: on|off
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "quad"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vstate, &vant, NULL))
    return 1;
  
  // Default to all antennas if none was given.

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Compose a network command for sending the message

  rtc.cmd.quad.enable = (CHOICE_VARIABLE(vstate)->choice == SWITCH_ON);

  return queue_rtc_command(cp, &rtc, NET_QUAD_CMD);
}

/**.......................................................................
 * Implement the command that installs the Walsh table
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        state - on|off
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_quadWalshTable_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vant;                // Antennas
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "quadWalshTable"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vant, NULL))
    return 1;
  
  // Default to all antennas if none was given.

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  return queue_rtc_command(cp, &rtc, NET_QUAD_WALSH_TABLE_CMD);
}

/**.......................................................................
 * Implement the command that installs the Walsh table
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        state - on|off
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_quadPhase_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vant;                // Antennas
  Variable *vphase;              // phase
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "quadPhase"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vphase, &vant, NULL))
    return 1;
  
  rtc.cmd.quad_phase.phase = UINT_VARIABLE(vphase)->uint;

  // Default to all antennas if none was given.

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  return queue_rtc_command(cp, &rtc, NET_QUAD_PHASE_CMD);
}

//-----------------------------------------------------------------------
// CAN Module commands
//-----------------------------------------------------------------------

/*.......................................................................
 * Implement the command that enables the IF auto level control
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        state - on|off
 *
 *                       Optional arguments:
 *
 *                        antennas -  The requested antennas
 *                        bands    -  The requested bands
 * 
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_reset_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vmodule;             // Modules
  Variable *vant;                // Antennas
  Variable *vband;               // Bands
  Variable *vhard;               // Hardware (true) or software
				 // (false) reset?
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "reset"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vmodule, &vant, &vband, &vhard, NULL))
    return 1;
  
  rtc.cmd.reset.modules = SET_VARIABLE(vmodule)->set;

  // Default to all antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // Default to all bands if none was given.

  if(OPTION_HAS_VALUE(vband))
    rtc.cmd.reset.bands = SET_VARIABLE(vband)->set;
  else
    rtc.cmd.reset.bands = sza::util::CorrelatorBand::BANDALL;

  // Default to software reset if none was specified

  if(OPTION_HAS_VALUE(vhard))
    rtc.cmd.reset.hard = BOOL_VARIABLE(vhard)->boolvar;
  else
    rtc.cmd.reset.hard = false;
    
  return queue_rtc_command(cp, &rtc, NET_RESET_CMD);
}

/**.......................................................................
 * Implement the command to set the reference antenna
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        state - on|off
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setRefAnt_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vant;   // Antennas
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setRefAnt"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vant, NULL))
    return 1;
  
  // Set the antenna

  rtc.antennas = SET_VARIABLE(vant)->set;

  // Check if this is a valid single antenna

  if(!(sza::util::AntNum::isValidSingleAnt((sza::util::AntNum::Id)rtc.antennas) ||
       (sza::util::AntNum::Id)rtc.antennas==sza::util::AntNum::ANTNONE))
    return 1;

  return queue_rtc_command(cp, &rtc, NET_REF_ANT_CMD);
}

/**.......................................................................
 * Implement the command to set a delay
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        state - on|off
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setDelay_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vtype;  // Type of delay
  Variable *vdelay; // The delay
  Variable *vant;   // Antennas
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setDelay"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vtype, &vdelay, &vant, NULL))
    return 1;
  
  // Which delay are we configuring?

  rtc.cmd.setDelay.delayType = CHOICE_VARIABLE(vtype)->choice;

  switch((DelayType) rtc.cmd.setDelay.delayType) {
  case FIXED:
  case ADJUSTABLE:
  case CORR_OFFSET:
    break;
  default:
    lprintf(stderr, "Only fixed, adjustable and offset delays can be set.\n");
    return 1;
    break;
  }

  // Get the value of the delay, in fractional naonseconds

  rtc.cmd.setDelay.delay = DOUBLE_VARIABLE(vdelay)->d;

  // Default to all antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  return queue_rtc_command(cp, &rtc, NET_SET_DELAY_CMD);
}

/**.......................................................................
 * Implement the command to set an axis misalignment (NIA)
 */
static CMD_FN(sc_setNia_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vlength; // The misalignment, in meters
  Variable *vant;    // Antennas
  RtcNetCmd rtc;     // The network object to be sent to the real-time
		     // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setNia"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vlength, &vant, NULL))
    return 1;
  
  // Which delay are we configuring?

  rtc.cmd.setDelay.delayType = NIA;

  // Get the value of the nia term, in meters

  rtc.cmd.setDelay.delay = DOUBLE_VARIABLE(vlength)->d;

  // Default to all antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  return queue_rtc_command(cp, &rtc, NET_SET_DELAY_CMD);
}

/**.......................................................................
 * Implement the command to set a default delay
 */
static CMD_FN(sc_setDefaultDelay_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vtype;  // Type of delay
  Variable *vdelay; // The delay
  Variable *vrx;    // The band
  Variable *vant;   // Antennas
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setDefaultDelay"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vtype, &vdelay, &vrx, &vant, NULL))
    return 1;
  
  // Which delay are we configuring?

  rtc.cmd.setDelay.delayType = CHOICE_VARIABLE(vtype)->choice;
  rtc.cmd.setDelay.rxId      = CHOICE_VARIABLE(vrx)->choice;

  switch((DelayType) rtc.cmd.setDelay.delayType) {
  case FIXED:
  case ADJUSTABLE:
    break;
  default:
    lprintf(stderr, "Only fixed and adjustable delays can be set.\n");
    return 1;
    break;
  }

  // Get the value of the delay, in fractional naonseconds

  rtc.cmd.setDelay.delay = DOUBLE_VARIABLE(vdelay)->d;

  // Default to all antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  return queue_rtc_command(cp, &rtc, NET_SET_DEFAULT_DELAY_CMD);
}

/**.......................................................................
 * Implement the command to use a delay
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        state - on|off
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_useDelay_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vtype;  // Type of delay
  Variable *vuse;   // true if we are using this delay
  Variable *vant;   // Antennas
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "useDelay"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vtype, &vuse, &vant, NULL))
    return 1;
  
  // Which delay are we configuring?

  rtc.cmd.useDelay.delayType = CHOICE_VARIABLE(vtype)->choice;

  // Whether or not the delay is to be used

  rtc.cmd.useDelay.use = BOOL_VARIABLE(vuse)->boolvar;

  // Default to all antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  return queue_rtc_command(cp, &rtc, NET_USE_DELAY_CMD);
}

/**.......................................................................
 * Implement the command to turn fringe tracking on/off
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        state - on|off
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_fringeTracking_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vstate; // On or off?
  Variable *vdelay; // On or off?
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "fringeTracking"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vstate, &vdelay, NULL))
    return 1;
  
  // Which delay are we configuring?

  rtc.cmd.fringeTracking.on     = CHOICE_VARIABLE(vstate)->choice == SWITCH_ON;
  rtc.cmd.fringeTracking.target = CHOICE_VARIABLE(vdelay)->choice;

  return queue_rtc_command(cp, &rtc, NET_FRINGE_TRACKING_CMD);
}

/**.......................................................................
 * Command to control the state of the DDS output
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        state - on|off
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_DDS_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vstate; // The desired state
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "DDS"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vstate, NULL))
    return 1;
  
  // Which delay are we configuring?

  rtc.cmd.DDS.state = CHOICE_VARIABLE(vstate)->choice;

  return queue_rtc_command(cp, &rtc, NET_DDS_CMD);
}

/**.......................................................................
 * Implement the command to set the mapping between antennas and Lobe
 * rotator DDS channels
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        state - on|off
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setAntennaDDS_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vant;   // Antennas
  Variable *vchannel; // The delay
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setAntennaDDS"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vant, &vchannel, NULL))
    return 1;
  
  // Which DDS are we configuring?

  rtc.cmd.setAntennaDDS.ddsId = SET_VARIABLE(vchannel)->set;

  // Make sure this corresponds to a valid single channel

  sza::util::DDSChannel 
    dds((sza::util::DDSChannel::Id)rtc.cmd.setAntennaDDS.ddsId);

  if(!dds.isValidSingleChannel()) {
    lprintf(stderr, "%s is not a valid single DDS channel", 
	    dds.printChannels().c_str());
    return 1;
  }

  // Which antenna are we configuring?

  rtc.antennas = SET_VARIABLE(vant)->set;

  // Make sure this correspond to a valid single antenna.

  sza::util::AntNum antNum((sza::util::AntNum::Id)rtc.antennas);

  if(!antNum.isValidSingleAnt()) {
    lprintf(stderr, "%s is not a valid single antenna", 
	    antNum.printAntennaSet().c_str());
    return 1;
  }

  return queue_rtc_command(cp, &rtc, NET_SET_ANTENNA_DDS_CMD);
}

/**.......................................................................
 * Implement the command that enables the LR frequency offset
 */
static CMD_FN(sc_enableLrFrequencyOffset_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vstate;              // state: on|off
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "enableLrFrequencyOffset"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vstate, NULL))
    return 1;
  
  // Compose a network command for sending the message

  rtc.cmd.enableLrFrequencyOffset.enable = BOOL_VARIABLE(vstate)->boolvar;

  return queue_rtc_command(cp, &rtc, NET_ENABLE_FREQUENCY_OFFSET_CMD);
}

/**.......................................................................
 * Implement the command to set the phase on a lobe rotator DDS
 * channel
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setDDSPhase_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vchannel; // The DDS channel
  Variable *vphase;   // The phase
  RtcNetCmd rtc;      // The network object to be sent to the real-time
		      // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setDDSPhase"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vchannel, &vphase, NULL))
    return 1;
  
  // Which channel are we configuring?

  rtc.cmd.setLRPhase.input = SET_VARIABLE(vchannel)->set;

  // Set the phase

  rtc.cmd.setLRPhase.phase = UINT_VARIABLE(vphase)->uint;

  // Set the type

  rtc.cmd.setLRPhase.type = LR_DDS;

  return queue_rtc_command(cp, &rtc, NET_SET_LR_PHASE_CMD);
}

/**.......................................................................
 * Implement the command to set the freq on a lobe rotator DDS
 * channel
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setDDSFreq_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vchannel; // The DDS channel
  Variable *vfreq;   // The freq
  RtcNetCmd rtc;      // The network object to be sent to the real-time
		      // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setDDSFreq"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vchannel, &vfreq, NULL))
    return 1;
  
  // Which channel are we configuring?

  rtc.cmd.setLRFreq.input = SET_VARIABLE(vchannel)->set;

  // Set the frequency

  rtc.cmd.setLRFreq.freq = DOUBLE_VARIABLE(vfreq)->d;

  // Set the type

  rtc.cmd.setLRFreq.type = LR_DDS;

  return queue_rtc_command(cp, &rtc, NET_SET_LR_FREQ_CMD);
}

/**.......................................................................
 * Implement the command to enable walshing on a lobe rotator DDS
 * channel
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setOutputRegs_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vchannel; // The DDS channel
  Variable *vfreg;    // The first register
  Variable *vpreg;    // The second register

  RtcNetCmd rtc;      // The network object to be sent to the real-time
		      // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setOutputRegs"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vchannel, &vfreg, &vpreg, NULL))
    return 1;
  
  // Which channel are we configuring?

  rtc.cmd.setOutputRegs.input = UINT_VARIABLE(vchannel)->uint;
  rtc.cmd.setOutputRegs.freg = UINT_VARIABLE(vfreg)->uint;
  rtc.cmd.setOutputRegs.preg = UINT_VARIABLE(vpreg)->uint;

  return queue_rtc_command(cp, &rtc, NET_SET_OUTPUT_REGS_CMD);
}

/**.......................................................................
 * Implement the command to enable walshing on a lobe rotator DDS
 * channel
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_enableDDSWalshing_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vchannel; // The DDS channel
  Variable *venable;  // to walsh or not to walsh?
  RtcNetCmd rtc;      // The network object to be sent to the real-time
		      // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "enableDDSWalshing"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &venable, &vchannel, NULL))
    return 1;
  
  // Which channel are we configuring?

  rtc.cmd.enableDDSWalshing.input = SET_VARIABLE(vchannel)->set;

  // Set the enable state

  rtc.cmd.enableDDSWalshing.enable = BOOL_VARIABLE(venable)->boolvar;

  return queue_rtc_command(cp, &rtc, NET_ENABLE_DDS_WALSHING_CMD);
}

/**.......................................................................
 * Implement the command to enable walshing on a lobe rotator DDS
 * channel
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setDDSWalshColumn_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vchannel; // The DDS channel
  Variable *vcolumn;  // to walsh or not to walsh?
  RtcNetCmd rtc;      // The network object to be sent to the real-time
		      // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setDDSWalshColumn"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vchannel, &vcolumn, NULL))
    return 1;
  
  // Which channel are we configuring?

  rtc.cmd.setDDSWalshColumn.input  = SET_VARIABLE(vchannel)->set;
  rtc.cmd.setDDSWalshColumn.column = UINT_VARIABLE(vcolumn)->uint;

  return queue_rtc_command(cp, &rtc, NET_SET_DDS_WALSH_COLUMN_CMD);
}

/**.......................................................................
 * Implement the command to set the phase on a lobe rotator Input
 * channel
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setInputPhase_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vchannel; // The Input channel
  Variable *vphase;   // The phase
  RtcNetCmd rtc;      // The network object to be sent to the real-time
		      // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setInputPhase"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vchannel, &vphase, NULL))
    return 1;
  
  // Which channel are we configuring?

  rtc.cmd.setLRPhase.input = UINT_VARIABLE(vchannel)->uint;

  // Set the phase

  rtc.cmd.setLRPhase.phase = UINT_VARIABLE(vphase)->uint;

  // Set the type

  rtc.cmd.setLRPhase.type = LR_INPUT;

  return queue_rtc_command(cp, &rtc, NET_SET_LR_PHASE_CMD);
}

/**.......................................................................
 * Implement the command to set the freq on a lobe rotator DDS
 * channel
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setInputFreq_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vchannel; // The Input channel
  Variable *vfreq;   // The freq
  RtcNetCmd rtc;      // The network object to be sent to the real-time
		      // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setInputFreq"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vchannel, &vfreq, NULL))
    return 1;
  
  // Which channel are we configuring?

  rtc.cmd.setLRFreq.input = UINT_VARIABLE(vchannel)->uint;

  // Set the frequency

  rtc.cmd.setLRFreq.freq = DOUBLE_VARIABLE(vfreq)->d;

  // Set the type

  rtc.cmd.setLRFreq.type = LR_INPUT;

  return queue_rtc_command(cp, &rtc, NET_SET_LR_FREQ_CMD);
}

/**.......................................................................
 * Implement the command to set the delay on a lobe rotator input
 * channel
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setInputDelay_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vchannel; // The Input channel
  Variable *vdelay;   // The delay
  Variable *vmjd;     // The mjd
  Variable *vdisc;    // The discontinuity flag
  RtcNetCmd rtc;      // The network object to be sent to the real-time
		      // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setInputDelay"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vchannel, &vdelay, &vmjd, &vdisc, NULL))
    return 1;
  
  // Which channel are we configuring?

  rtc.cmd.setLRDelay.input = UINT_VARIABLE(vchannel)->uint;

  // Set the delay

  rtc.cmd.setLRDelay.delay = DOUBLE_VARIABLE(vdelay)->d;

  // Set the MJD

  if(OPTION_HAS_VALUE(vmjd))
    rtc.cmd.setLRDelay.mjd = DOUBLE_VARIABLE(vmjd)->d;
  else {
    sza::util::TimeVal timeVal;
    timeVal.setToCurrentTime();
    rtc.cmd.setLRDelay.mjd = timeVal.getMjd();
  }

  // Set the discontinuity flag

  rtc.cmd.setLRDelay.disc =  OPTION_HAS_VALUE(vdisc) ? BOOL_VARIABLE(vdisc)->boolvar : false;

  // Queue the command

  return queue_rtc_command(cp, &rtc, NET_SET_LR_DELAY_CMD);
}

/**.......................................................................
 * Implement the command to set the phase on a lobe rotator Antenna
 * channel
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setAntennaPhase_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vphase;   // The phase
  Variable *vant;     // The Antenna
  RtcNetCmd rtc;      // The network object to be sent to the real-time
		      // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setAntennaPhase"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vphase, &vant, NULL))
    return 1;
  
  // Set the phase

  rtc.cmd.setAntennaPhase.phase = UINT_VARIABLE(vphase)->uint;

  // Use antenna defaults if none was specified

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  return queue_rtc_command(cp, &rtc, NET_SET_ANTENNA_PHASE_CMD);
}

/**.......................................................................
 * Implement the command to set the freq on a lobe rotator Antenna
 * channel
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setAntennaFreq_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vfreq;   // The freq
  Variable* vant;    // The antennas
  RtcNetCmd rtc;      // The network object to be sent to the real-time
		      // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setAntennaFreq"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vfreq, &vant, NULL))
    return 1;
  
  // Set the frequency

  rtc.cmd.setAntennaFreq.freq = DOUBLE_VARIABLE(vfreq)->d;

  // Use antenna defaults if none was specified

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  return queue_rtc_command(cp, &rtc, NET_SET_ANTENNA_FREQ_CMD);
}

/**.......................................................................
 * Implement the command to set an antenna location
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        state - on|off
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setAntennaLocation_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *veast;  // East
  Variable *vnorth; // North
  Variable *vup;    // Up
  Variable *vant;   // Antennas
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setAntennaLocation"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vup, &veast, &vnorth, &vant, NULL))
    return 1;
  
  // Which delay are we configuring?

  rtc.cmd.location.up    = DOUBLE_VARIABLE(vup)->d;
  rtc.cmd.location.east  = DOUBLE_VARIABLE(veast)->d;
  rtc.cmd.location.north = DOUBLE_VARIABLE(vnorth)->d;

  // Default to all antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  return queue_rtc_command(cp, &rtc, NET_LOCATION_CMD);
}

/**.......................................................................
 * Implement the command to set a delay reference location
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        state - on|off
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setDelayReference_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *veast;  // East
  Variable *vnorth; // North
  Variable *vup;    // Up
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setDelayReference"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vup, &veast, &vnorth, NULL))
    return 1;
  
  // Set the delay reference position

  rtc.cmd.delayref.up    = DOUBLE_VARIABLE(vup)->d;
  rtc.cmd.delayref.east  = DOUBLE_VARIABLE(veast)->d;
  rtc.cmd.delayref.north = DOUBLE_VARIABLE(vnorth)->d;

  return queue_rtc_command(cp, &rtc, NET_DELAYREF_CMD);
}

/**.......................................................................
 * Implement the command to set the default antenna set.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        state - on|off
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_turnPower_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vbreaker; // Which breaker
  Variable *vstate;   // on/off?
  Variable *vant;     // Antennas
  
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "turnPower"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vstate, &vbreaker, &vant, NULL))
    return 1;
  
  // Turn the power on/off?

  rtc.cmd.power.power = CHOICE_VARIABLE(vstate)->choice == SWITCH_ON;

  // Record the breaker to cycle.  Default to outlet 0 (all outlets)
  // if none was specified

  if(OPTION_HAS_VALUE(vbreaker))
    rtc.cmd.power.breaker = CHOICE_VARIABLE(vbreaker)->choice;
  else {
    lprintf(stderr, "You must specify an outlet.\n");
    return 1;
  }

  // Cycle power for which antennas?

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  return queue_rtc_command(cp, &rtc, NET_POWER_CMD);
}

/**.......................................................................
 * Implement the command to set the default antenna set.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 *                        state - on|off
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setDefaultAntennas_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  // Get the resource object of the parent thread.

  Scheduler *sch = (Scheduler* )cp_ThreadData(cp, CP_SCHEDULER);
  Variable *vant;   // Antennas
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vant, NULL))
    return 1;
  
  // Install these antennas as the default

  cp_AntSet(cp)->set((sza::util::AntNum::Id)SET_VARIABLE(vant)->set);

  // And queue a message to all connected clients that the selection
  // has changed

  sch_send_antenna_selection(sch, NULL);

  return 0;
}

//-----------------------------------------------------------------------
// CalTert commands
//-----------------------------------------------------------------------

/**.......................................................................
 * Implement the command to position the tertiary
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_positionTertiary_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vpos;   // A position
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "positionTertiary"))
    return 1;

  // Get the command-line arguments.

  if(get_Arguments(args, &vpos, &vant, NULL))
    return 1;
  
  // Install in the caltert message container

  rtc.cmd.caltert.msgId = CALTERT_POSITION_TERT;

  // Parse the position

  switch((TertPos)INT_VARIABLE(vpos)->i) {
  case TERTPOS_RX30GHZ:
    rtc.cmd.caltert.rxId = sza::util::Rx::RX30GHZ;
    break;
  case TERTPOS_RX90GHZ:
    rtc.cmd.caltert.rxId = sza::util::Rx::RX90GHZ;
    break;
  case TERTPOS_RX230GHZ:
    rtc.cmd.caltert.rxId = sza::util::Rx::RX230GHZ;
    break;
  default:
    rtc.cmd.caltert.rxId = sza::util::Rx::RXNONE;
    rtc.cmd.caltert.tertPosition = (short)INT_VARIABLE(vpos)->i;
    break;
  }

  // Default to all antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // Get the sequence number associated with this command

  rtc.cmd.caltert.seq = sch_next_caltert_seq(sc, cp_Scheduler(cp));

  return queue_rtc_command(cp, &rtc, NET_CALTERT_CMD);
}

/**.......................................................................
 * Implement the command to home the tertiary
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_homeTertiary_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vant=0;   // Antennas
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "homeTertiary"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vant, NULL))
    return 1;

  rtc.cmd.caltert.msgId = CALTERT_HOME_TERT;

  // Default to all antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // Get the sequence number associated with this command

  rtc.cmd.caltert.seq = sch_next_caltert_seq(sc, cp_Scheduler(cp));

  return queue_rtc_command(cp, &rtc, NET_CALTERT_CMD);
}

/**.......................................................................
 * Implement the command to position the calibrator
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_positionCalibrator_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vpos; // The calibrator position
  Variable* vant; // Antennas
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "positionCalibrator"))
    return 1;

  // Get the command-line arguments.

  if(get_Arguments(args, &vpos, &vant, NULL))
    return 1;
  
  // Install in the caltert message container

  rtc.cmd.caltert.msgId = CALTERT_POSITION_CAL;
  rtc.cmd.caltert.calPosition = 
    (sza::util::CalPos::Pos)CHOICE_VARIABLE(vpos)->choice;

  // Default to all antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // Get the sequence number associated with this command

  rtc.cmd.caltert.seq = sch_next_caltert_seq(sc, cp_Scheduler(cp));

  return queue_rtc_command(cp, &rtc, NET_CALTERT_CMD);
}

/**.......................................................................
 * Implement the command to enable the tertiary
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_enableTertiary_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *venable;
  Variable* vant; 
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "enableTertiary"))
    return 1;

  // Get the command-line arguments.

  if(get_Arguments(args, &venable, &vant, NULL))
    return 1;
  
  // Install in the terttert message container

  rtc.cmd.caltert.msgId = CALTERT_ENABLE_TERT;
  rtc.cmd.caltert.enable = BOOL_VARIABLE(venable)->boolvar;

  // Default to all antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  return queue_rtc_command(cp, &rtc, NET_CALTERT_CMD);
}

/**.......................................................................
 * Implement the command to reset the tertiary
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_resetStepper_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vant=0; // Antennas
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "resetStepper"))
    return 1;
  
  rtc.cmd.caltert.msgId = CALTERT_RESET_STEPPER;

  // Default to all antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  return queue_rtc_command(cp, &rtc, NET_CALTERT_CMD);
}

/**.......................................................................
 * Implement the command to index the current tertiary encoder position
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_indexTertiary_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vpos;
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "indexTertiary"))
    return 1;

  // Get the command-line arguments.

  if(get_Arguments(args, &vpos, &vant, NULL))
    return 1;
  
  // Install in the caltert message container

  rtc.cmd.caltert.msgId = CALTERT_INDEX_TERT;

  // Parse the position.  This must be a valid rx specifier

  switch((TertPos)INT_VARIABLE(vpos)->i) {
  case TERTPOS_RX30GHZ:
    rtc.cmd.caltert.rxId = sza::util::Rx::RX30GHZ;
    break;
  case TERTPOS_RX90GHZ:
    rtc.cmd.caltert.rxId = sza::util::Rx::RX90GHZ;
    break;
  case TERTPOS_RX230GHZ:
    rtc.cmd.caltert.rxId = sza::util::Rx::RX230GHZ;
    break;
  default:
    lprintf(stderr, "Position must correspond to a known receiver.\n");
    return 1;
    break;
  }

  // Default to all antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  return queue_rtc_command(cp, &rtc, NET_CALTERT_CMD);
}

/**.......................................................................
 * Implement the command to set the index position
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setEncoderPosition_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vrx;
  Variable* vpos;
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setEncoderPosition"))
    return 1;

  // Get the command-line arguments.

  if(get_Arguments(args, &vrx, &vpos, &vant, NULL))
    return 1;
  
  // Install in the caltert message container

  rtc.cmd.caltert.msgId        = CALTERT_SET_ENCODER;
  rtc.cmd.caltert.rxId         = CHOICE_VARIABLE(vrx)->choice;
  rtc.cmd.caltert.tertPosition = (short)INT_VARIABLE(vpos)->i;

  // Default to all antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  return queue_rtc_command(cp, &rtc, NET_CALTERT_CMD);
}

/**.......................................................................
 * Implement the command to store the index position
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_storeEncoderPosition_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vrx;
  Variable* vpos;
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "storeEncoderPosition"))
    return 1;

  // Get the command-line arguments.

  if(get_Arguments(args, &vrx, &vpos, &vant, NULL))
    return 1;
  
  // Install in the caltert message container

  rtc.cmd.caltert.msgId        = CALTERT_STORE_ENCODER;
  rtc.cmd.caltert.rxId         = CHOICE_VARIABLE(vrx)->choice;
  rtc.cmd.caltert.tertPosition = (short)INT_VARIABLE(vpos)->i;

  // Default to all antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  return queue_rtc_command(cp, &rtc, NET_CALTERT_CMD);
}

/**.......................................................................
 * Implement the command to index the current tertiary encoder position
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_caltertOneWire_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vdevice;
  Variable* vcommand;
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "caltertOneWire"))
    return 1;

  // Get the command-line arguments.

  if(get_Arguments(args, &vdevice, &vcommand, &vant, NULL))
    return 1;
  
  // Install in the caltert message container

  rtc.cmd.caltert.msgId = CALTERT_ONE_WIRE;

  // Get the device.

  rtc.cmd.caltert.owDevice = CHOICE_VARIABLE(vdevice)->choice;

  // Get the command

  rtc.cmd.caltert.owCommand = CHOICE_VARIABLE(vcommand)->choice;

  // Don't default antennas -- this is a potentially dangerous
  // operation, as it may overwrite tertiary positions stored in the
  // one-wires.  The user should make certain s/he is commanding the
  // right antenna.

  rtc.antennas = SET_VARIABLE(vant)->set;

  return queue_rtc_command(cp, &rtc, NET_CALTERT_CMD);
}

/**.......................................................................
 * Implement the command to set the position of the IF switch
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_selectIF_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vband;
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "selectIF"))
    return 1;

  // Get the command-line arguments.

  if(get_Arguments(args, &vband, &vant, NULL))
    return 1;
  
  // Install in the IFMod message container

  rtc.cmd.IFMod.msgId = IFMOD_POSITION_SWITCH;

  // Parse the position.  This must be a valid rx specifier

  rtc.cmd.IFMod.band = (sza::util::Rx::Id)CHOICE_VARIABLE(vband)->choice;

  // Default on antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // Get the sequence number associated with this command

  rtc.cmd.IFMod.seq = sch_next_IFMod_seq(sc, cp_Scheduler(cp));

  return queue_rtc_command(cp, &rtc, NET_IFMOD_CMD);
}

/**.......................................................................
 * Implement the command to set the antenna IF level
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setIFLevel_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vlevel;
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setIFLevel"))
    return 1;

  // Get the command-line arguments.

  if(get_Arguments(args, &vlevel, &vant, NULL))
    return 1;
  
  // Install in the antenna IF message container

  rtc.cmd.IFMod.msgId = IFMOD_SET_LEVEL;

  // Parse the level

  rtc.cmd.IFMod.level = DOUBLE_VARIABLE(vlevel)->d;

  // Default on antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  return queue_rtc_command(cp, &rtc, NET_IFMOD_CMD);
}

/**.......................................................................
 * Implement the command to set the antenna IF attenuation
 */
static CMD_FN(sc_setIFAtten_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vtotal;
  Variable* vinput;
  Variable* voutput;
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task
  using sza::util::IFAtten;
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setIFAtten"))
    return 1;

  // Get the command-line arguments.

  if(get_Arguments(args, &vtotal, &vinput, &voutput, &vant, NULL))
    return 1;
  
  // Install in the antenna IF message container

  rtc.cmd.IFMod.msgId = IFMOD_SET_ATTEN;

  // Set the attenuation

  rtc.cmd.IFMod.attenSet = sza::util::IFAtten::ATTEN_NONE;

  if(OPTION_HAS_VALUE(vinput)) {
    rtc.cmd.IFMod.input = DOUBLE_VARIABLE(vinput)->d;
    rtc.cmd.IFMod.attenSet |= sza::util::IFAtten::ATTEN_INPUT;
  }

  if(OPTION_HAS_VALUE(voutput)) {
    rtc.cmd.IFMod.output = DOUBLE_VARIABLE(voutput)->d;
    rtc.cmd.IFMod.attenSet |= sza::util::IFAtten::ATTEN_OUTPUT;
  }

  if(OPTION_HAS_VALUE(vtotal)) {

    // Sanity check arguments

    if(rtc.cmd.IFMod.attenSet &= sza::util::IFAtten::ATTEN_INPUT) {
      if(rtc.cmd.IFMod.attenSet &= sza::util::IFAtten::ATTEN_OUTPUT) {
	lprintf(stderr, "sc_setIFAtten_cmd: You may specify any two of total/input/output"
		" attenuations, but not all three\n");
	return 1;

      } else {
	double total = DOUBLE_VARIABLE(vtotal)->d;
	rtc.cmd.IFMod.output = total - rtc.cmd.IFMod.input;
	rtc.cmd.IFMod.attenSet = 
	  (sza::util::IFAtten::ATTEN_OUTPUT|sza::util::IFAtten::ATTEN_INPUT);
      }
    }

    else if(rtc.cmd.IFMod.attenSet &= sza::util::IFAtten::ATTEN_INPUT) {
      if(rtc.cmd.IFMod.attenSet &= sza::util::IFAtten::ATTEN_INPUT) {
	lprintf(stderr, "sc_setIFAtten_cmd: You may specify any two of total/input/output"
		" attenuations, but not all three\n");
	return 1;

      } else {
	double total = DOUBLE_VARIABLE(vtotal)->d;
	rtc.cmd.IFMod.input = total - rtc.cmd.IFMod.output;
	rtc.cmd.IFMod.attenSet = 
	  (sza::util::IFAtten::ATTEN_OUTPUT|sza::util::IFAtten::ATTEN_INPUT);
      }
    }
    else {
      rtc.cmd.IFMod.total = DOUBLE_VARIABLE(vtotal)->d;
      rtc.cmd.IFMod.attenSet |= sza::util::IFAtten::ATTEN_TOTAL;
    }
  }

  // Default on antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // And queue the command

  cout << "Sending command with total = " 
       << rtc.cmd.IFMod.total << endl;

  return queue_rtc_command(cp, &rtc, NET_IFMOD_CMD);
}

/**.......................................................................
 * Implement the command to set the antenna IF attenuation to default
 * sky values
 */
static CMD_FN(sc_setSkyIFAtten_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task
  using sza::util::IFAtten;
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setSkyIFAtten"))
    return 1;

  // Get the command-line arguments.

  if(get_Arguments(args, &vant, NULL))
    return 1;
  
  // Install in the antenna IF message container

  rtc.cmd.IFMod.msgId = IFMOD_SET_TO_DEFAULT;

  // Set the requested position;

  rtc.cmd.IFMod.pos = sza::util::CalPos::SKY;

  // Default on antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // And queue the command

  return queue_rtc_command(cp, &rtc, NET_IFMOD_CMD);
}

/**.......................................................................
 * Implement the command to set the antenna IF attenuation to default
 * load values
 */
static CMD_FN(sc_setLoadIFAtten_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task
  using sza::util::IFAtten;
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setLoadIFAtten"))
    return 1;

  // Get the command-line arguments.

  if(get_Arguments(args, &vant, NULL))
    return 1;
  
  // Install in the antenna IF message container

  rtc.cmd.IFMod.msgId = IFMOD_SET_TO_DEFAULT;

  // Set the requested position;

  rtc.cmd.IFMod.pos = sza::util::CalPos::HOTLOAD;

  // Default on antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // And queue the command

  return queue_rtc_command(cp, &rtc, NET_IFMOD_CMD);
}

/**.......................................................................
 * Implement the command to set the default antenna IF attenuation
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setDefaultIFAtten_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vtotal;
  Variable* vinput;
  Variable* voutput;
  Variable* vrx;
  Variable* vant;
  Variable* vpos;
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task
  using sza::util::IFAtten;
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setDefaultIFAtten"))
    return 1;

  // Get the command-line arguments.

  if(get_Arguments(args, &vtotal, &vinput, &voutput, &vrx, &vant, &vpos, NULL))
    return 1;
  
  // Install in the antenna IF message container

  rtc.cmd.IFMod.msgId = IFMOD_SET_DEFAULT_ATTEN;

  // Set the attenuation

  rtc.cmd.IFMod.attenSet = sza::util::IFAtten::ATTEN_NONE;

  if(OPTION_HAS_VALUE(vinput)) {
    rtc.cmd.IFMod.input = DOUBLE_VARIABLE(vinput)->d;
    rtc.cmd.IFMod.attenSet |= sza::util::IFAtten::ATTEN_INPUT;
  }

  if(OPTION_HAS_VALUE(voutput)) {
    rtc.cmd.IFMod.output = DOUBLE_VARIABLE(voutput)->d;
    rtc.cmd.IFMod.attenSet |= sza::util::IFAtten::ATTEN_OUTPUT;
  }

  if(OPTION_HAS_VALUE(vtotal)) {

    // Sanity check arguments

    if(rtc.cmd.IFMod.attenSet &= sza::util::IFAtten::ATTEN_INPUT) {
      if(rtc.cmd.IFMod.attenSet &= sza::util::IFAtten::ATTEN_OUTPUT) {
	lprintf(stderr, "sc_setIFAtten_cmd: You may specify any two of total/input/output"
		" attenuations, but not all three\n");
	return 1;

      } else {
	double total = DOUBLE_VARIABLE(vtotal)->d;
	rtc.cmd.IFMod.output = total - rtc.cmd.IFMod.input;
	rtc.cmd.IFMod.attenSet = 
	  (sza::util::IFAtten::ATTEN_OUTPUT|sza::util::IFAtten::ATTEN_INPUT);
      }
    }

    else if(rtc.cmd.IFMod.attenSet &= sza::util::IFAtten::ATTEN_INPUT) {
      if(rtc.cmd.IFMod.attenSet &= sza::util::IFAtten::ATTEN_INPUT) {
	lprintf(stderr, "sc_setIFAtten_cmd: You may specify any two of total/input/output"
		" attenuations, but not all three\n");
	return 1;

      } else {
	double total = DOUBLE_VARIABLE(vtotal)->d;
	rtc.cmd.IFMod.input = total - rtc.cmd.IFMod.output;
	rtc.cmd.IFMod.attenSet = 
	  (sza::util::IFAtten::ATTEN_OUTPUT|sza::util::IFAtten::ATTEN_INPUT);
      }
    }
    else {
      rtc.cmd.IFMod.total = DOUBLE_VARIABLE(vtotal)->d;
      rtc.cmd.IFMod.attenSet |= sza::util::IFAtten::ATTEN_TOTAL;
    }
  }

  // Default to all receivers if none was given

  rtc.cmd.IFMod.band = OPTION_HAS_VALUE(vrx) ? SET_VARIABLE(vrx)->set :
    sza::util::Rx::RXALL;

  // Default on antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // Default to all calibrator positions if none was given

  rtc.cmd.IFMod.pos = OPTION_HAS_VALUE(vpos) ? SET_VARIABLE(vpos)->set :
    sza::util::CalPos::ALL;

  // And queue the command

  return queue_rtc_command(cp, &rtc, NET_IFMOD_CMD);
}

/**.......................................................................
 * Implement the command to set the antenna IF attenuation
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setDefaultLOTermAtten_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vatten;
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setDefaultLOTermAtten"))
    return 1;

  // Get the command-line arguments.

  if(get_Arguments(args, &vatten, &vant, NULL))
    return 1;
  
  // Set the attenuation

  rtc.cmd.intmod.atten = INT_VARIABLE(vatten)->i;
  rtc.cmd.intmod.msgId = INTMOD_SET_DEFAULT_ATTEN;

  // Default on antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // And queue the command

  return queue_rtc_command(cp, &rtc, NET_INTMOD_CMD);
}

/**.......................................................................
 * Implement the command to set the antenna IF attenuation
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setLOTermAtten_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vatten;
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time
		    // controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setLOTermAtten"))
    return 1;

  // Get the command-line arguments.

  if(get_Arguments(args, &vatten, &vant, NULL))
    return 1;
  
  // Set the attenuation

  rtc.cmd.intmod.atten = INT_VARIABLE(vatten)->i;
  rtc.cmd.intmod.msgId = INTMOD_SET_ATTEN;

  // Default on antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // And queue the command

  return queue_rtc_command(cp, &rtc, NET_INTMOD_CMD);
}

/**.......................................................................
 * Implement the command to set the antenna IF attenuation
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_flipDelay_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vflip;
  Variable* vdelay;
  RtcNetCmd rtc;    // The network object to be sent to the real-time

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "flipDelay"))
    return 1;

  // Get the command-line arguments.

  if(get_Arguments(args, &vflip, &vdelay, NULL))
    return 1;
  
  // Set the attenuation

  rtc.cmd.flipDelay.target       = FLIP_DELAY;
  rtc.cmd.flipDelay.delay        = BOOL_VARIABLE(vflip)->boolvar;
  rtc.cmd.flipDelay.delayTarget  = CHOICE_VARIABLE(vdelay)->choice;

  cout << "Flip command: target = " <<   rtc.cmd.flipDelay.delayTarget  << endl;

  // And queue the command
  
  return queue_rtc_command(cp, &rtc, NET_FLIP_DELAY_CMD);
}

/**.......................................................................
 * Implement the command to set the antenna IF attenuation
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_flipDelayRate_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vflip;
  RtcNetCmd rtc;    // The network object to be sent to the real-time

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "flipDelay"))
    return 1;
  
  // Get the command-line arguments.
  
  if(get_Arguments(args, &vflip, NULL))
    return 1;
  
  // Set the attenuation
  
  rtc.cmd.flipDelay.target = FLIP_RATE;
  rtc.cmd.flipDelay.rate = BOOL_VARIABLE(vflip)->boolvar;

  // And queue the command

  return queue_rtc_command(cp, &rtc, NET_FLIP_DELAY_CMD);
}

//-----------------------------------------------------------------------
// Commands for the thermal modules
//-----------------------------------------------------------------------

/**.......................................................................
 * Implement the command to set a temperature on the thermal modules
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setTemperature_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vtarget;
  Variable* vtemp;
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setTemperature"))
    return 1;
  
  // Get the command-line arguments.
  
  if(get_Arguments(args, &vtarget, &vtemp, &vant, NULL))
    return 1;
  
  rtc.cmd.thermal.msgId  = THERMAL_SET_TEMP;
  rtc.cmd.thermal.target = SET_VARIABLE(vtarget)->set;
  rtc.cmd.thermal.value  = DOUBLE_VARIABLE(vtemp)->d;

  // Default on antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // And queue the command

  return queue_rtc_command(cp, &rtc, NET_THERMAL_CMD);

}

/**.......................................................................
 * Implement the command to set a mode on the thermal modules
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setMode_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vtarget;
  Variable* vmode;
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setMode"))
    return 1;
  
  // Get the command-line arguments.
  
  if(get_Arguments(args, &vtarget, &vmode, &vant, NULL))
    return 1;
  
  rtc.cmd.thermal.msgId  = THERMAL_SET_MODE;
  rtc.cmd.thermal.target = SET_VARIABLE(vtarget)->set;
  rtc.cmd.thermal.mode   = CHOICE_VARIABLE(vmode)->choice;

  // Default on antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // And queue the command

  return queue_rtc_command(cp, &rtc, NET_THERMAL_CMD);

}

/**.......................................................................
 * Implement the command to set a gain on the thermal modules
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setLoopGain_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vtarget;
  Variable* vgain;
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setLoopGain"))
    return 1;
  
  // Get the command-line arguments.
  
  if(get_Arguments(args, &vtarget, &vgain, &vant, NULL))
    return 1;
  
  rtc.cmd.thermal.msgId  = THERMAL_SET_LOOP_GAIN;
  rtc.cmd.thermal.target = SET_VARIABLE(vtarget)->set;
  rtc.cmd.thermal.value  = DOUBLE_VARIABLE(vgain)->d;

  // Default on antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // And queue the command

  return queue_rtc_command(cp, &rtc, NET_THERMAL_CMD);
}

/**.......................................................................
 * Implement the command to set a gain on the thermal modules
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setIntegConst_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vtarget;
  Variable* vconst;
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setIntegConst"))
    return 1;
  
  // Get the command-line arguments.
  
  if(get_Arguments(args, &vtarget, &vconst, &vant, NULL))
    return 1;
  
  rtc.cmd.thermal.msgId  = THERMAL_SET_INTEG_CONST;
  rtc.cmd.thermal.target = SET_VARIABLE(vtarget)->set;
  rtc.cmd.thermal.value  = DOUBLE_VARIABLE(vconst)->d;

  // Default on antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // And queue the command

  return queue_rtc_command(cp, &rtc, NET_THERMAL_CMD);
}

/**.......................................................................
 * Implement the command to set an integration constant on the thermal modules
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setLoopBw_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vtarget;
  Variable* vbw;
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setLoopBw"))
    return 1;
  
  // Get the command-line arguments.
  
  if(get_Arguments(args, &vtarget, &vbw, &vant, NULL))
    return 1;
  
  rtc.cmd.thermal.msgId  = THERMAL_SET_LOOP_BW;
  rtc.cmd.thermal.target = SET_VARIABLE(vtarget)->set;
  rtc.cmd.thermal.value  = DOUBLE_VARIABLE(vbw)->d;

  // Default on antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // And queue the command

  return queue_rtc_command(cp, &rtc, NET_THERMAL_CMD);
}

/**.......................................................................
 * Implement the command to set a rate constant on the thermal modules
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setRateConst_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vtarget;
  Variable* vrate;
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setRateConst"))
    return 1;
  
  // Get the command-line arguments.
  
  if(get_Arguments(args, &vtarget, &vrate, &vant, NULL))
    return 1;
  
  rtc.cmd.thermal.msgId  = THERMAL_SET_RATE_CONST;
  rtc.cmd.thermal.target = SET_VARIABLE(vtarget)->set;
  rtc.cmd.thermal.value  = DOUBLE_VARIABLE(vrate)->d;

  // Default on antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // And queue the command

  return queue_rtc_command(cp, &rtc, NET_THERMAL_CMD);
}

/**.......................................................................
 * Implement the commit the last values sent to the thermal modules
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setCircFanPropConst_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vant;
  Variable* vconst;
  RtcNetCmd rtc;    // The network object to be sent to the real-time

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setCircFanPropConst"))
    return 1;
  
  // Get the command-line arguments.
  
  if(get_Arguments(args, &vconst, &vant, NULL))
    return 1;
  
  rtc.cmd.thermal.msgId = THERMAL_SET_PROP_CONST;
  rtc.cmd.thermal.value = DOUBLE_VARIABLE(vconst)->d;

  // Default on antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // And queue the command

  return queue_rtc_command(cp, &rtc, NET_THERMAL_CMD);
}

/**.......................................................................
 * Implement the command to set a voltage offset
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setVoltageOffset_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vant;
  Variable* vvolt;
  RtcNetCmd rtc;    // The network object to be sent to the real-time

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setVoltageOffset"))
    return 1;
  
  // Get the command-line arguments.
  
  if(get_Arguments(args, &vvolt, &vant, NULL))
    return 1;
  
  rtc.cmd.thermal.msgId = THERMAL_SET_VOLTAGE_OFFSET;
  rtc.cmd.thermal.value = DOUBLE_VARIABLE(vvolt)->d;

  // Default on antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // And queue the command

  return queue_rtc_command(cp, &rtc, NET_THERMAL_CMD);
}

/*.......................................................................
 * Implement the command that sets the Ebox equilibrium mode
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Receivers rx      -  The set of target receivers.
 *                        Stages stages     -  The set of target stages.
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setEboxEqState_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vstate; // The state
  Variable* vant;
  RtcNetCmd rtc;   // The network object to be sent to
		   // the real-time controller task
                    
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setEboxEqState"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vstate, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.thermal.msgId = THERMAL_SET_EBOX_EQ_STATE;
  rtc.cmd.thermal.state = CHOICE_VARIABLE(vstate)->choice == SWITCH_ON;

  // Parse optional antenna argument here.  If no argument was given,
  // use the default antenna set

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_THERMAL_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that sets the Ebox equilibrium mode
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Receivers rx      -  The set of target receivers.
 *                        Stages stages     -  The set of target stages.
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setEboxIntError_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable* verror; // The integral error term
  Variable* vant;
  RtcNetCmd rtc;   // The network object to be sent to
		   // the real-time controller task
                    
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setEboxIntError"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &verror, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.thermal.msgId = THERMAL_SET_EBOX_INT_ERROR;
  rtc.cmd.thermal.value = DOUBLE_VARIABLE(verror)->d;

  // Parse optional antenna argument here.  If no argument was given,
  // use the default antenna set

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_THERMAL_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that sets the Ebox equilibrium mode
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Receivers rx      -  The set of target receivers.
 *                        Stages stages     -  The set of target stages.
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_atmosphere_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable* vtemp;     // The air temperature
  Variable* vhumidity; // The humidity
  Variable* vpressure; // The pressure
  Variable* vant;
  RtcNetCmd rtc;   // The network object to be sent to
		   // the real-time controller task
                    
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "atmosphere"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vtemp, &vhumidity, &vpressure, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.atmos.temperature = DOUBLE_VARIABLE(vtemp)->d;
  rtc.cmd.atmos.humidity    = DOUBLE_VARIABLE(vhumidity)->d;
  rtc.cmd.atmos.pressure    = DOUBLE_VARIABLE(vpressure)->d;

  // Parse optional antenna argument here.  If no argument was given,
  // use the default antenna set

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_ATMOS_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that configures the dead-man timeout
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Receivers rx      -  The set of target receivers.
 *                        Stages stages     -  The set of target stages.
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_configureCmdTimeout_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable* vstate;    // Turn the timeout on/off
  Variable* vint; // The timeout interval

  // Get the command-line arguments.

  if(get_Arguments(args, &vstate, &vint, NULL))
    return 1;
  
  // If an interval was specified, set the timeout, in seconds

  if(OPTION_HAS_VALUE(vint))
    configureCmdTimeout(cp,(unsigned int)floor(DOUBLE_VARIABLE(vint)->d + 0.5));

  // If a request to de/activate the timeout was received, do it now

  if(OPTION_HAS_VALUE(vstate)) {
    if(CHOICE_VARIABLE(vstate)->choice == SWITCH_ON) {
      configureCmdTimeout(cp, true);
    } else {
      configureCmdTimeout(cp, false);
    }
  }

  return 0;
}

/*.......................................................................
 * Implement the command that configures the dead-pager timeout
 */
static CMD_FN(sc_configurePagerAutoEnable_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable* vstate; // Turn the timeout on/off
  Variable* vint;   // The timeout interval

  // Get the command-line arguments.

  if(get_Arguments(args, &vstate, &vint, NULL))
    return 1;
  
  TermMessage msg;

  // If an interval was specified, set the timeout, in seconds

  if(OPTION_HAS_VALUE(vint)) {
    if(packPagerAutoEnable(&msg, (unsigned int)floor(DOUBLE_VARIABLE(vint)->d + 0.5)) || 
       send_TermMessage(cp, &msg, PIPE_WAIT)==PIPE_ERROR) {
      return 1;
    }
  }

  // If a request to de/activate the timeout was received, do it now

  if(OPTION_HAS_VALUE(vstate)) {
    if(packPagerAutoEnable(&msg, CHOICE_VARIABLE(vstate)->choice == SWITCH_ON) ||
       send_TermMessage(cp, &msg, PIPE_WAIT)==PIPE_ERROR) {
      return 1;
    }
  }

  return 0;
}

/*.......................................................................
 * Implement the command that enables or disables local-oscillator stages.
 */
static CMD_FN(sc_homeGunnDevice_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *vdevices;            // The device to control
  Variable *vant;                // The set of target antennas 
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "homeGunnDevice"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vdevices, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.lo.msgId    = LO_HOME_GUNN_DEVICE;
  rtc.cmd.lo.oscs     = sza::util::LoOsc::GUNN;
  rtc.cmd.lo.stages   = SET_VARIABLE(vdevices)->set;

  // Parse optional antenna argument here.  If no argument was given,
  // default to all antennas.

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_LO_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that enables or disables local-oscillator stages.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Receivers rx      -  The set of target receivers.
 *                        Stages stages     -  The set of target stages.
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setGunnDevice_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vdevices;             // The device to control
  Variable *vposition;           // The position to set   
  Variable *vant;                // The set of target antennas 
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setGunnDevice"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vdevices, &vposition, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.lo.msgId    = LO_POS_GUNN_DEVICE;
  rtc.cmd.lo.oscs     = sza::util::LoOsc::GUNN;
  rtc.cmd.lo.stages   = SET_VARIABLE(vdevices)->set;
  rtc.cmd.lo.position = INT_VARIABLE(vposition)->i;

  // Parse optional antenna argument here.  If no argument was given,
  // default to all antennas.

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_LO_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that enables or disables local-oscillator stages.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Receivers rx      -  The set of target receivers.
 *                        Stages stages     -  The set of target stages.
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_jogGunnDevice_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vdevices;            // The device to control
  Variable *vposition;           // The position to set   
  Variable *vant;                // The set of target antennas 
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "jogGunnDevice"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vdevices, &vposition, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.lo.msgId    = LO_JOG_GUNN_DEVICE;
  rtc.cmd.lo.oscs     = sza::util::LoOsc::GUNN;
  rtc.cmd.lo.stages   = SET_VARIABLE(vdevices)->set;
  rtc.cmd.lo.position = INT_VARIABLE(vposition)->i;

  // Parse optional antenna argument here.  If no argument was given,
  // default to all antennas.

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_LO_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that enables or disables local-oscillator stages.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Receivers rx      -  The set of target receivers.
 *                        Stages stages     -  The set of target stages.
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setGunnFrequency_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vfreq;               // The desired frequency
  Variable *vant;                // The set of target antennas 
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setGunnFrequency"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vfreq, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.lo.msgId     = LO_FREQ;
  rtc.cmd.lo.oscs      = sza::util::LoOsc::GUNN;
  rtc.cmd.lo.frequency = INT_VARIABLE(vfreq)->i;

  // Parse optional antenna argument here.  If no argument was given,
  // default to all antennas.

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_LO_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that enables or disables local-oscillator stages.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Receivers rx      -  The set of target receivers.
 *                        Stages stages     -  The set of target stages.
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setGunnLOFrequency_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vfreq;               // The desired frequency
  Variable *vant;                // The set of target antennas 
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setGunnLOFrequency"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vfreq, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.lo.msgId     = LO_LO_FREQ;
  rtc.cmd.lo.oscs      = sza::util::LoOsc::GUNN;
  rtc.cmd.lo.frequency = INT_VARIABLE(vfreq)->i;

  // Parse optional antenna argument here.  If no argument was given,
  // default to all antennas.

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_LO_CMD))
    return 1;

  return 0;
}

/*.......................................................................
 * Implement the command that sets the Gunn operating voltage
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        Receivers rx      -  The set of target receivers.
 *                        Stages stages     -  The set of target stages.
 *                        SwitchState state -  The desired switch state.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_setGunnVoltage_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *vvolt;               // The desired frequency
  Variable *vant;                // The set of target antennas 
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setGunnVoltage"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vvolt, &vant, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.lo.msgId    = LO_VOLTAGE;
  rtc.cmd.lo.oscs     = sza::util::LoOsc::GUNN;
  rtc.cmd.lo.voltage  = INT_VARIABLE(vvolt)->i;

  // Parse optional antenna argument here.  If no argument was given,
  // use the default antenna set

  rtc.antennas  = 
    OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set : cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_LO_CMD))
    return 1;

  return 0;
}

/**.......................................................................                                    
 * Generate auto documentation                                                                                
 */
static CMD_FN(sc_autoDoc_cmd)
{
  Variable *vdir;      /* The archiving directory */

  // Get the command-line arguments.                                                                          

  if(get_Arguments(args, &vdir, NULL))
    return 1;

  // Send the requested configuration messages to the archiver.                                               

  sza::util::HtmlDoc::generateAutoDocumentation(sc, OPTION_HAS_VALUE(vdir) ? STRING_VARIABLE(vdir)->string : ".");

  return 0;
}

//-----------------------------------------------------------------------
// Commands for the TILTMETER modules
//-----------------------------------------------------------------------

/**.......................................................................
 * Implement the command to set a temperature on the tiltmeter modules
 */
static CMD_FN(sc_setTiltmeterTemperature_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vtemp;
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setTiltmeterTemperature"))
    return 1;
  
  // Get the command-line arguments.
  
  if(get_Arguments(args, &vtemp, &vant, NULL))
    return 1;
  
  rtc.cmd.tiltmeter.msgId  = TILTMETER_SET_TEMP;
  rtc.cmd.tiltmeter.value  = DOUBLE_VARIABLE(vtemp)->d;

  // Default on antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // And queue the command

  return queue_rtc_command(cp, &rtc, NET_TILTMETER_CMD);
}

/**.......................................................................
 * Implement the command to configure the thermal control loop of the
 * tiltmeter modules
 */
static CMD_FN(sc_regulateTiltmeterTemperature_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vtemp;
  Variable* vmode;
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "regulateTiltmeterTemperature"))
    return 1;
  
  // Get the command-line arguments.
  
  if(get_Arguments(args, &vmode, &vtemp, &vant, NULL))
    return 1;
  
  rtc.cmd.tiltmeter.msgId  = TILTMETER_SET_TEMP;
  rtc.cmd.tiltmeter.mode   = CHOICE_VARIABLE(vmode)->choice;
  rtc.cmd.tiltmeter.value  = DOUBLE_VARIABLE(vtemp)->d;

  // Default on antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // And queue the command

  return queue_rtc_command(cp, &rtc, NET_TILTMETER_CMD);
}

/**.......................................................................
 * Implement the command to set the loop gain on the tiltmeter modules
 */
static CMD_FN(sc_setTiltmeterLoopGain_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vval;
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setTiltmeterLoopGain"))
    return 1;
  
  // Get the command-line arguments.
  
  if(get_Arguments(args, &vval, &vant, NULL))
    return 1;
  
  rtc.cmd.tiltmeter.msgId  = TILTMETER_SET_LOOP_GAIN;
  rtc.cmd.tiltmeter.value  = DOUBLE_VARIABLE(vval)->d;

  // Default on antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // And queue the command

  return queue_rtc_command(cp, &rtc, NET_TILTMETER_CMD);
}

/**.......................................................................
 * Implement the command to set the tiltmeter integration constant
 */
static CMD_FN(sc_setTiltmeterIntegConst_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vval;
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setTiltmeterIntegConst"))
    return 1;
  
  // Get the command-line arguments.
  
  if(get_Arguments(args, &vval, &vant, NULL))
    return 1;
  
  rtc.cmd.tiltmeter.msgId  = TILTMETER_SET_INTEG_CONST;
  rtc.cmd.tiltmeter.value  = DOUBLE_VARIABLE(vval)->d;

  // Default on antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // And queue the command

  return queue_rtc_command(cp, &rtc, NET_TILTMETER_CMD);
}

/**.......................................................................
 * Implement the command to set the tiltmeter integration constant
 */
static CMD_FN(sc_setTiltmeterRateConst_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vval;
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setTiltmeterRateConst"))
    return 1;
  
  // Get the command-line arguments.
  
  if(get_Arguments(args, &vval, &vant, NULL))
    return 1;
  
  rtc.cmd.tiltmeter.msgId  = TILTMETER_SET_RATE_CONST;
  rtc.cmd.tiltmeter.value  = DOUBLE_VARIABLE(vval)->d;

  // Default on antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // And queue the command

  return queue_rtc_command(cp, &rtc, NET_TILTMETER_CMD);
}

/**.......................................................................
 * Implement the command to set the tiltmeter loop bandwidth
 */
static CMD_FN(sc_setTiltmeterLoopBw_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vval;
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setTiltmeterLoopBw"))
    return 1;
  
  // Get the command-line arguments.
  
  if(get_Arguments(args, &vval, &vant, NULL))
    return 1;
  
  rtc.cmd.tiltmeter.msgId  = TILTMETER_SET_LOOP_BW;
  rtc.cmd.tiltmeter.value  = DOUBLE_VARIABLE(vval)->d;

  // Default on antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // And queue the command

  return queue_rtc_command(cp, &rtc, NET_TILTMETER_CMD);
}

/**.......................................................................
 * Implement the command to set the tiltmeter loop bandwidth
 */
static CMD_FN(sc_writeTiltmeterParamsToEeprom_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable* vant;
  RtcNetCmd rtc;    // The network object to be sent to the real-time

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "writeTiltmeterParamsToEeprom"))
    return 1;
  
  // Get the command-line arguments.
  
  if(get_Arguments(args, &vant, NULL))
    return 1;
  
  rtc.cmd.tiltmeter.msgId  = TILTMETER_WRITE_TO_EEPROM;

  // Default on antennas if none was given.

  rtc.antennas = OPTION_HAS_VALUE(vant) ? SET_VARIABLE(vant)->set :
    cp_AntSet(cp)->getId();

  // And queue the command

  return queue_rtc_command(cp, &rtc, NET_TILTMETER_CMD);
}

/*.......................................................................
 * Implement the command that returns the value of a specified SZA register
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         reg  -  The register specification.
 *                         val  -  The register value.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static FUNC_FN(sc_regVal_fn)
{
  ControlProg *cp = (ControlProg* )sc->project;  /* The context of the
						    host control
						    program */
  Variable *vreg;      /* The register specification argument */

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "regVal"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vreg, NULL))
    return 1;

  try {
    std::string regSpec(STRING_VARIABLE(vreg)->string);

    static sza::util::RegParser parser(false);
    sza::util::RegDescription regDesc = parser.inputReg(regSpec);
    
    // Get the value of the requested register from the last frame
    // buffer.
    
    DOUBLE_VARIABLE(result)->d = getRegVal(cp, regDesc);

  } catch(sza::util::Exception& err) {
    lprintf(stderr, "%s\n", err.what());
    return 1;
  } catch(...) {
    lprintf(stderr, "Caught an unknown exception\n");
    return 1;
  }

  return 0;
}

/*.......................................................................
 * Implement the command that converts from an integer to a string
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         reg  -  The register specification.
 *                         val  -  The register value.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static FUNC_FN(sc_intToString_fn)
{
  Variable *vint;      /* The register specification argument */

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "intToString"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vint, NULL))
    return 1;

  try {

    std::ostringstream os;
    os << UINT_VARIABLE(vint)->uint;

    // Allocate a copy of the string from the string-segment of the
    // program.

    char* s = new_ScriptString(sc, (char*)os.str().c_str());

    if(!s)
      return 1;

    STRING_VARIABLE(result)->string = s;

  } catch(sza::util::Exception& err) {
    lprintf(stderr, "%s\n", err.what());
    return 1;
  } catch(...) {
    lprintf(stderr, "Caught an unknown exception\n");
    return 1;
  }

  return 0;
}

/*.......................................................................
 * Implement the command that converts from any type to a string
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                         reg  -  The register specification.
 *                         val  -  The register value.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static FUNC_FN(sc_printToString_fn)
{
  Variable *vint;      /* The register specification argument */

  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "printToString"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vint, NULL))
    return 1;

  // Allocate a copy of the string from the string-segment of the
  // program.

  char* s = new_ScriptString(sc, "This is a test of printToString");
  
  if(!s)
    return 1;
  
  STRING_VARIABLE(result)->string = s;
  
  return 0;
}

/**.......................................................................
 * Implement the command that sets the synthesizer frequency
 */
static CMD_FN(sc_setSynthFrequency_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable* vfreq=0;               // The desired frequency
  Variable* vunits=0;              // The set of target antennas 
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setSynthFrequency"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vfreq, &vunits, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.synth.msgId = SYNTH_FREQ;

  double val = DOUBLE_VARIABLE(vfreq)->d;
  char* units = STRING_VARIABLE(vunits)->string;

  sza::util::Frequency freq;
  freq.setVal(val, units);

  rtc.cmd.synth.val  = freq.MHz();
  
  // Parse optional antenna argument here.  If no argument was given,
  // default to all antennas.

  rtc.antennas  = cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_SYNTH_CMD))
    return 1;

  return 0;
}

/**.......................................................................
 * Implement the command that sets the synthesizer RF output power
 */
static CMD_FN(sc_setSynthPower_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable* vpow=0;              // The desired power
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "setSynthPower"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &vpow, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.synth.msgId = SYNTH_POW;
  rtc.cmd.synth.val   = DOUBLE_VARIABLE(vpow)->d;
  
  // Parse optional antenna argument here.  If no argument was given,
  // default to all antennas.

  rtc.antennas  = cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_SYNTH_CMD))
    return 1;

  return 0;
}

/**.......................................................................
 * Implement the command that enables the synthesizer RF output
 */
static CMD_FN(sc_enableSynthOutput_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable* ven=0;              // The desired power
  RtcNetCmd rtc;                 // The network object to be sent to
				 // the real-time controller task
  
  // Do we have a controller to send the command to?

  if(rtc_offline(sc, "enableSynthOutput"))
    return 1;
  
  // Get the command-line arguments.

  if(get_Arguments(args, &ven, NULL))
    return 1;
  
  // Compose the real-time controller network command.

  rtc.cmd.synth.msgId  = SYNTH_RFOUTPUT;
  rtc.cmd.synth.enable = BOOL_VARIABLE(ven)->boolvar;
  
  // Parse optional antenna argument here.  If no argument was given,
  // default to all antennas.

  rtc.antennas  = cp_AntSet(cp)->getId();

  // Send the command to the real-time controller.

  if(queue_rtc_command(cp, &rtc, NET_SYNTH_CMD))
    return 1;

  return 0;
}

/**.......................................................................
 * Set an array configuration
 */
static CMD_FN(sc_setArrayConfiguration_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable* varray=0;
  Variable* vconfig=0;

  sza::util::CarmaConfig cc;

  // Get the command-line arguments.
  
  if(get_Arguments(args, &varray, &vconfig, NULL))
    return 1;

  return  sendArrayConfigMsg(cp, 
			     CHOICE_VARIABLE(varray)->choice, 
			     CHOICE_VARIABLE(vconfig)->choice);
}

/**.......................................................................
 * Add an antenna to an array configuration
 */
static CMD_FN(sc_addArrayAntenna_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable* vArray=0;
  Variable* vPad=0;
  Variable* vAntType=0;
  Variable* vAnt=0;

  // Get the command-line arguments.
  
  if(get_Arguments(args, &vArray, &vPad, &vAntType, &vAnt, NULL))
    return 1;

  return sendAddArrayAntennaMsg(cp, 
				CHOICE_VARIABLE(vArray)->choice,
				UINT_VARIABLE(vPad)->uint,
				CHOICE_VARIABLE(vAntType)->choice,

				OPTION_HAS_VALUE(vAnt) ? 
				(int)UINT_VARIABLE(vAnt)->uint : -1);
}

/**.......................................................................
 * Remove an antenna from an array configuration
 */
static CMD_FN(sc_remArrayAntenna_cmd)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable* vArray=0;
  Variable* vPad=0;
  Variable* vAnt=0;

  // Get the command-line arguments.
  
  if(get_Arguments(args, &vArray, &vPad, &vAnt, NULL))
    return 1;

  return sendRemArrayAntennaMsg(cp,
				CHOICE_VARIABLE(vArray)->choice,
				UINT_VARIABLE(vPad)->uint);
}
