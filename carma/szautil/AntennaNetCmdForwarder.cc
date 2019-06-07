#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/AntennaNetCmdForwarder.h"

using namespace std;

using namespace sza::array;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
AntennaNetCmdForwarder::AntennaNetCmdForwarder() 
{
}

/**.......................................................................
 * Destructor.
 */
AntennaNetCmdForwarder::~AntennaNetCmdForwarder() {}

/**.......................................................................
 * Forward a network command intended for an antenna
 */
void AntennaNetCmdForwarder::forwardNetCmd(sza::util::NetCmd* netCmd)
{
  RtcNetCmd* rtc  = &netCmd->rtc_;
  NetCmdId opcode = netCmd->opcode_;
  LogStream errStr;

  DBPRINT(true, Debug::DEBUG2, "forwarding a net command");

  // Interpret the command.
  
  switch(opcode) {
  case NET_INTERVAL_CMD:
  case NET_INHIBIT_CMD:
  case NET_STROBE_CMD:
  case NET_FEATURE_CMD: 
  case NET_WALSH_CMD: 
    forwardScannerNetCmd(netCmd);
    break;
  case NET_UNFLAG_CMD:
    forwardBoardNetCmd(netCmd);
    break;
  case NET_SETREG_CMD:
  case NET_SETDIO_CMD:
    forwardProbeNetCmd(netCmd);
    break;
  case NET_PAGER_CMD:
    forwardChannelizerNetCmd(netCmd);
    break;
    
    // A special case -- this is a message for both the scanner and
    // rx tasks.  The rx task needs to know how to associate
    // polarization states with encoder positions, and the scanner
    // task needs to recognize them.  I handle this by forwarding
    // the message to the scanner task, which in turn forwards it to
    // the rx task.  That way the valid states are guaranteed to be
    // defined before they can be used
    
  case NET_RX_POLAR_CMD:
    forwardScannerNetCmd(netCmd);
    break;
  case NET_POLWALSH_CMD:
    forwardRxNetCmd(netCmd);
    break;
  case NET_PHASE_MOTOR_CMD:
  case NET_PHASE_SHIFT_CMD:
    forwardControlNetCmd(netCmd);
    break;
    // CAN module reset command
  case NET_RESET_CMD:
  case NET_RX_HEATER_CMD:
  case NET_RX_COLDHEAD_CMD:
  case NET_RX_TEMP_CMD:
    forwardRxNetCmd(netCmd);
    break;
  case NET_IFMOD_CMD:
  case NET_CALTERT_CMD:
  case NET_INTMOD_CMD:
  case NET_THERMAL_CMD:
  case NET_SET_BIAS_CMD:
  case NET_SELECT_RX_CMD:
    forwardRxNetCmd(netCmd);
    break;
  case NET_LO_CMD:
    DBPRINT(true, Debug::DEBUG9, "Got an LO_CMD");
  case NET_RX_QUAD_CMD:
  case NET_FAST_SAMPLING_CMD:
    forwardRxNetCmd(netCmd);
    break;
  case NET_INIT_CMD:
    forwardControlNetCmd(netCmd);
    break;
  case NET_SLEW_CMD:              // Any of the following may contain a phase     
  case NET_TRACK_CMD: 		  // tracking command, so we			     
    forwardTrackerNetCmd(netCmd); // forward to both task methods.                
    break;
  case NET_SITE_CMD:
  case NET_LOCATION_CMD:
  case NET_REBOOT_PMAC_CMD:
  case NET_UT1UTC_CMD:
  case NET_EQNEQX_CMD:
  case NET_HALT_CMD: 
  case NET_MOUNT_OFFSET_CMD:
  case NET_EQUAT_OFFSET_CMD:
  case NET_TV_OFFSET_CMD:
  case NET_TV_ANGLE_CMD:
  case NET_SKY_OFFSET_CMD:
  case NET_SLEW_RATE_CMD:
  case NET_TILTS_CMD:
  case NET_FLEXURE_CMD:
  case NET_COLLIMATE_CMD:
  case NET_ENCODER_LIMITS_CMD:
  case NET_ENCODER_CALS_CMD:
  case NET_ENCODER_ZEROS_CMD:
  case NET_MODEL_CMD:
  case NET_YEAR_CMD:
  case NET_DECK_MODE_CMD:
  case NET_ATMOS_CMD:
    forwardTrackerNetCmd(netCmd);
    break;
  case NET_GPIB_SEND_CMD:
  case NET_GPIB_READ_CMD:
    forwardGpibNetCmd(netCmd);
    break;
  case NET_POWER_CMD:
    forwardStripControlNetCmd(netCmd);
    break;
  case NET_POWER_DT_CMD:
  case NET_POWER_METER_CMD:
  case NET_NOISE_CAL_CMD:
    forwardNoiseCalNetCmd(netCmd);
    break;
  case NET_CHZR_POWER_CMD:
  case NET_CHZR_ZERO_CMD:
  case NET_CHZR_ATTN_CMD:
  case NET_CHZR_SWITCH_CMD:
  case NET_CHZR_ENABLE_CMD:
  case NET_TPCAL_CMD:
    forwardChannelizerNetCmd(netCmd);
    break;
  case NET_RXSIM_CMD:
    forwardRxSimulatorNetCmd(netCmd);
    break;
  case NET_THERMO_CMD: 
  case NET_DS_DT_CMD:
    forwardThermoNetCmd(netCmd);
    break;
    
    // Forward optical camera and stepper motor commands to the
    // optical camera task.
    
  case NET_OPTCAM_CNTL_CMD: // Control power to the camera/stepper 
  case NET_STEPPER_CMD:    // Step the stepper motor 
  case NET_CAMERA_CMD:     // Control the camera via the camera control box 
  case NET_FG_CMD:         // Write to a frame grabber register 
  case NET_FLATFIELD_CMD:  // Toggle flat fielding of frame grabber images 
    forwardOpticalCameraNetCmd(netCmd);
    break;
    
  default:
    errStr.initMessage(true);
    errStr <<  "Unknown command: " << opcode 
	   << "received from control program.";
    break;
  }
}

/**.......................................................................,
 * Forward a control command 
 */
void AntennaNetCmdForwarder::forwardControlNetCmd(sza::util::NetCmd* netCmd) {}

/**.......................................................................
 * Optical Camera commands
 */
void AntennaNetCmdForwarder::forwardOpticalCameraNetCmd(sza::util::NetCmd* netCmd) {}

/**.......................................................................
 * Thermometry commands
 */
void AntennaNetCmdForwarder::forwardThermoNetCmd(sza::util::NetCmd* netCmd) {}

/**.......................................................................
 * Channelizer commands
 */
void AntennaNetCmdForwarder::forwardChannelizerNetCmd(sza::util::NetCmd* netCmd) {}

/**.......................................................................
 * RxSim commands
 */
void AntennaNetCmdForwarder::forwardRxSimulatorNetCmd(sza::util::NetCmd* netCmd) {}

/**.......................................................................
 * GPIB commands
 */
void AntennaNetCmdForwarder::forwardGpibNetCmd(sza::util::NetCmd* netCmd) {}

/**.......................................................................
 * Noise cal commands
 */
void AntennaNetCmdForwarder::forwardNoiseCalNetCmd(sza::util::NetCmd* netCmd) {}

/**.......................................................................
 * Forward a command for the weather station
 */
void AntennaNetCmdForwarder::forwardWeatherNetCmd(sza::util::NetCmd* netCmd) {};

/**.......................................................................
 * Forward a tracker net command
 */
void AntennaNetCmdForwarder::forwardTrackerNetCmd(sza::util::NetCmd* netCmd) {}

/**.......................................................................
 * Receiver commands
 */
void AntennaNetCmdForwarder::forwardRxNetCmd(sza::util::NetCmd* netCmd) {}

/**.......................................................................
 * Scanner task commands
 */
void AntennaNetCmdForwarder::forwardScannerNetCmd(sza::util::NetCmd* netCmd) {}

/**.......................................................................
 * Probe commands
 */
void AntennaNetCmdForwarder::forwardProbeNetCmd(sza::util::NetCmd* netCmd){}

/**.......................................................................
 * Board flagging commands
 */
void AntennaNetCmdForwarder::forwardBoardNetCmd(sza::util::NetCmd* netCmd){}

/**.......................................................................
 * StripControl commands
 */
void AntennaNetCmdForwarder::forwardStripControlNetCmd(sza::util::NetCmd* netCmd) {};
