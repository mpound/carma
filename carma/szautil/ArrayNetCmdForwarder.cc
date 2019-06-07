#include "carma/szautil/Debug.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/ArrayNetCmdForwarder.h"

#include <iostream>

using namespace sza::util;
using namespace sza::array;
using namespace std;

/**.......................................................................
 * Constructor.
 */
ArrayNetCmdForwarder::ArrayNetCmdForwarder() 
{
  antennaForwarder_ = 0;
  controlForwarder_ = 0;
  dcForwarder_      = 0;
  delayForwarder_   = 0;
  grabberForwarder_ = 0;
  scannerForwarder_ = 0;
  stripForwarder_   = 0;
  synthForwarder_   = 0;
}

/**.......................................................................
 * Destructor.
 */
ArrayNetCmdForwarder::~ArrayNetCmdForwarder() 
{
  if(antennaForwarder_ != 0)
    delete antennaForwarder_;

  if(controlForwarder_ != 0)
    delete controlForwarder_;

  if(dcForwarder_ != 0)
    delete dcForwarder_;

  if(delayForwarder_ != 0)
    delete delayForwarder_;

  if(grabberForwarder_ != 0)
    delete grabberForwarder_;

  if(scannerForwarder_ != 0)
    delete scannerForwarder_;

  if(stripForwarder_ != 0)
    delete stripForwarder_;

  if(synthForwarder_ != 0)
    delete synthForwarder_;
}

/**.......................................................................
 * Forward a network command read from the network stream buffer.
 */
void ArrayNetCmdForwarder::forwardNetCmd(sza::util::NetCmd* netCmd)
{
  RtcNetCmd* rtc  = &netCmd->rtc_;
  NetCmdId opcode = netCmd->opcode_;
  LogStream errStr;

  DBPRINT(true, Debug::DEBUG5, "Got a command");

  // Interpret the command.
  
  switch(opcode) {

    //------------------------------------------------------------
    // Commands for the scanner task
    //------------------------------------------------------------

  case NET_FEATURE_CMD:
  DBPRINT(true, Debug::DEBUG3, "Got a feature command");
    forwardScannerNetCmd(netCmd);
    break;

    //------------------------------------------------------------
    // Commands for the control task
    //------------------------------------------------------------

  case NET_INIT_CMD:
    forwardControlNetCmd(netCmd);
    break;

    //------------------------------------------------------------
    // Commands for the antenna power strips
    //------------------------------------------------------------

  case NET_POWER_CMD:
    forwardStripNetCmd(netCmd);
    break;

    //------------------------------------------------------------
    // Commands for the synthesizer
    //------------------------------------------------------------

  case NET_SYNTH_CMD:
    forwardSynthNetCmd(netCmd);
    break;

    //------------------------------------------------------------
    // Commands for the antennas
    //------------------------------------------------------------

  case NET_LO_CMD:
    DBPRINT(true, Debug::DEBUG9, "Got an LO command");
  case NET_IFMOD_CMD:
  case NET_CALTERT_CMD:
  case NET_INTMOD_CMD:
  case NET_THERMAL_CMD:
  case NET_SET_BIAS_CMD:
  case NET_INTERVAL_CMD:
  case NET_INHIBIT_CMD:
  case NET_STROBE_CMD:
  case NET_WALSH_CMD: 
  case NET_UNFLAG_CMD:
  case NET_SETREG_CMD:
  case NET_SETDIO_CMD:
  case NET_PAGER_CMD:
  case NET_RX_POLAR_CMD:
  case NET_POLWALSH_CMD:
  case NET_PHASE_MOTOR_CMD:
  case NET_PHASE_SHIFT_CMD:
  case NET_RX_HEATER_CMD:
  case NET_RX_COLDHEAD_CMD:
  case NET_RX_TEMP_CMD:
  case NET_RX_QUAD_CMD:
  case NET_FAST_SAMPLING_CMD:
  case NET_REBOOT_PMAC_CMD:

    // Tracking commands

  case NET_HALT_CMD: 
  case NET_MOUNT_OFFSET_CMD:
  case NET_EQUAT_OFFSET_CMD:
    forwardAntennaNetCmd(netCmd);
    break;
  case NET_TV_OFFSET_CMD:
    DBPRINT(true, Debug::DEBUG3, "Got a tv offset cmd");
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

  case NET_GPIB_SEND_CMD:
  case NET_GPIB_READ_CMD:
  case NET_POWER_DT_CMD:
  case NET_POWER_METER_CMD:
  case NET_NOISE_CAL_CMD:
  case NET_CHZR_POWER_CMD:
  case NET_CHZR_ZERO_CMD:
  case NET_CHZR_ATTN_CMD:
  case NET_CHZR_SWITCH_CMD:
  case NET_CHZR_ENABLE_CMD:
  case NET_TPCAL_CMD:
  case NET_RXSIM_CMD:
  case NET_THERMO_CMD: 
  case NET_DS_DT_CMD:

    // Forward optical camera and stepper motor commands to the
    // optical camera task.
    
  case NET_OPTCAM_CNTL_CMD: // Control power to the camera/stepper 
  case NET_STEPPER_CMD:    // Step the stepper motor 
  case NET_CAMERA_CMD:     // Control the camera via the camera control box 
  case NET_FG_CMD:         // Write to a frame grabber register 
  case NET_FLATFIELD_CMD:  // Toggle flat fielding of frame grabber images 
    forwardAntennaNetCmd(netCmd);
    break;
    
    //------------------------------------------------------------ 
    // The following commands are forwarded both to the antennas and
    // the delay engine
    //------------------------------------------------------------

  case NET_SELECT_RX_CMD:         // The delay engine also has to know
				  // the observing frequency
  case NET_SLEW_CMD:              // Any of the following may contain a phase     
  case NET_TRACK_CMD: 		  // tracking command, so we			     
  case NET_SITE_CMD:
  case NET_LOCATION_CMD:
  case NET_UT1UTC_CMD:            // UT1-UTC ephemeris
  case NET_EQNEQX_CMD:            // equation of the equinox ephemeris
  case NET_ATMOS_CMD:
    forwardAntennaNetCmd(netCmd); // Note deliberate fall-through
				  // here. We want the above commands
				  // to be forwarded to both tasks.

  case NET_DELAYREF_CMD:          // Commands here only get forwarded
				  // to the delay engine
  case NET_FLIP_DELAY_CMD:        // Command to flip a delay sign
    forwardDelayNetCmd(netCmd);
    break;
  case NET_CONFIGURE_FG_CMD: // Configure the frame grabber 
  case NET_GRABBER_CMD:      // Grab the next frame from the frame grabber 
    forwardGrabberNetCmd(netCmd);
    break;

    //------------------------------------------------------------
    // Commands for the downconverter
    //------------------------------------------------------------

    // Downconverter commands
    
  case NET_PSYS_CMD:
  case NET_PSYS_ATTEN_CMD:
  case NET_IFOUT_CMD:
  case NET_IFOUT_ATTEN_CMD:
  case NET_RF_AMP_CMD:
  case NET_IF_ALC_CMD:
    
    // Noise source commands
    
  case NET_NOISE_POWER_CMD:
  case NET_NOISE_ATTEN_CMD:
  case NET_TONE_ATTEN_CMD:
  case NET_TONE_CMD:
    
    // Quad Mod commands
    
  case NET_QUAD_POWER_CMD:
  case NET_QUAD_ATTEN_CMD:
  case NET_QUAD_WALSH_COLUMN_CMD:
  case NET_QUAD_CMD:
  case NET_QUAD_WALSH_TABLE_CMD:
  case NET_QUAD_PHASE_CMD:
    forwardDcNetCmd(netCmd);
    break;
  case NET_RESET_CMD:
    forwardDcNetCmd(netCmd);
    forwardAntennaNetCmd(netCmd);
    break;

    // Noise command must be forwarded to both the delay control
    // thread and the downconverter

  case NET_NOISE_CMD:

    // The command should always be forwarded to the downconverter
    // control task

    forwardDcNetCmd(netCmd);

    // If we are configuring all devices, forward to the delay control and scanner 
    // tasks as well

    if(rtc->cmd.noise.mask & NOISE_ALL) {
      forwardDelayNetCmd(netCmd);
      forwardScannerNetCmd(netCmd);
    }

    break;    

    //-----------------------------------------------------------------------
    // Delay commands
    //-----------------------------------------------------------------------

  case NET_SET_ANTENNA_DDS_CMD:
    forwardScannerNetCmd(netCmd);
    forwardDelayNetCmd(netCmd);
    break; 
  case NET_SET_DEFAULT_DELAY_CMD:
    forwardDelayNetCmd(netCmd);
    break;
  case NET_DDS_CMD:
  case NET_REF_ANT_CMD:
  case NET_SET_ANTENNA_COORDS_CMD:
  case NET_SET_ANTENNA_FREQ_CMD:
  case NET_SET_ANTENNA_PARAMS_CMD:
  case NET_SET_ANTENNA_PHASE_CMD:
  case NET_SET_LR_PHASE_CMD:
  case NET_SET_LR_FREQ_CMD:
  case NET_ENABLE_DDS_WALSHING_CMD:
  case NET_SET_DDS_WALSH_COLUMN_CMD:
  case NET_SET_OUTPUT_REGS_CMD:
  case NET_SET_LR_DELAY_CMD:
  case NET_SET_DELAY_CMD:
  case NET_SET_WEATHER_PARAMS_CMD:
  case NET_USE_DELAY_CMD:
  case NET_FRINGE_TRACKING_CMD:
  case NET_ENABLE_FREQUENCY_OFFSET_CMD:
    forwardDelayNetCmd(netCmd);
    break;
  default:
    errStr.initMessage(true);
    errStr <<  "Unknown command: " << opcode 
	   << "received from control program.";
    errStr.report();
    break;
  }
}

/**.......................................................................
 * Forward a command to the antenna subsystem
 */
void ArrayNetCmdForwarder::forwardAntennaNetCmd(sza::util::NetCmd* netCmd)
{
  COUT("Forwarding antenna command");
  if(antennaForwarder_ != 0)
    antennaForwarder_->forwardNetCmd(netCmd);
}

/**.......................................................................
 * Forward a control command
 */
void ArrayNetCmdForwarder::forwardControlNetCmd(sza::util::NetCmd* netCmd)
{
  if(controlForwarder_ != 0)
    controlForwarder_->forwardNetCmd(netCmd);
}

/**.......................................................................
 * Forward a command to the delay subsystem
 */
void ArrayNetCmdForwarder::forwardDelayNetCmd(sza::util::NetCmd* netCmd)
{
  if(delayForwarder_ != 0)
    delayForwarder_->forwardNetCmd(netCmd);
}

/**.......................................................................
 * Forward a command to the dc subsystem
 */
void ArrayNetCmdForwarder::forwardDcNetCmd(sza::util::NetCmd* netCmd)
{
  if(dcForwarder_ != 0)
    dcForwarder_->forwardNetCmd(netCmd);
}

/**.......................................................................
 * Forward a command to the grabber subsystem
 */
void ArrayNetCmdForwarder::forwardGrabberNetCmd(sza::util::NetCmd* netCmd)
{
  if(grabberForwarder_ != 0)
    grabberForwarder_->forwardNetCmd(netCmd);
}

/**.......................................................................
 * Forward a scanner command
 */
void ArrayNetCmdForwarder::forwardScannerNetCmd(sza::util::NetCmd* netCmd)
{
  if(scannerForwarder_ != 0)
    scannerForwarder_->forwardNetCmd(netCmd);
}

/**.......................................................................
 * Forward a strip command
 */
void ArrayNetCmdForwarder::forwardStripNetCmd(sza::util::NetCmd* netCmd)
{
  if(stripForwarder_ != 0)
    stripForwarder_->forwardNetCmd(netCmd);
}

/**.......................................................................
 * Forward a synth command
 */
void ArrayNetCmdForwarder::forwardSynthNetCmd(sza::util::NetCmd* netCmd)
{
  if(synthForwarder_ != 0)
    synthForwarder_->forwardNetCmd(netCmd);
}

