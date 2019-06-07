#ifndef rtcnetcoms_h
#define rtcnetcoms_h

/*
 * This file defines the network interfaces between the host control-program
 * and the real-time controller task, and between control-clients and
 * the control program.
 */
#include "carma/szaarrayutils/netbuf.h"
#include "carma/szaarrayutils/netobj.h"
#include "carma/szaarrayutils/szaregs.h"
#include "carma/szaarrayutils/szaconst.h"

//#include "carma/szautil/AntNum.h"
#include "carma/szautil/SzaPorts.h"

namespace sza {
  namespace array {
    
    /*-----------------------------------------------------------------------
     * Define the ids of control-program to controller messages along with
     * the corresponding message containers.
     */
    typedef enum {
      NET_LOG_MSG,              // A message to be logged 
      NET_PMAC_DONE_MSG,        // A pmac transaction-completion message 
      NET_SOURCE_SET_MSG,       // A warning message that the source has set 
      NET_SETREG_DONE_MSG,      // A setreg transaction completion message 
      NET_TV_OFFSET_DONE_MSG,   // A tv_offset transaction completion message 
      NET_ID_MSG,               // A message from an antenna to identify itself 
      NET_GREETING_MSG,         // A handshake message
      NET_NAV_UPDATE_MSG,       // A request from the controller for a
				// re-initialization of ephemeris
				// positions from the navigator thread
      NET_CALTERT_DONE_MSG,     // A caltert transaction-completion message 
      NET_IFMOD_DONE_MSG,       // An antenna IF transaction-completion message 
      NET_CAN_DONE_MSG,         // A CAN transaction-completion message 
      NET_NOISE_DONE_MSG,       // A noise source completion message 
      NET_FRAME_DONE_MSG        // A frame transaction-completion message 
    } NetMsgId;
    
    /**
     * Send a greeting message
     */
    typedef struct {
      NetUlong revision;
      NetUlong nReg;
      NetUlong nByte;
    } NetGreetingMsg;

    /*
     * The following object is used to forward log messages from the real-time
     * control system to the control program. Note that cnt_send_log_message()
     * in controller.c relies on the text[] member of NetLogMsg being the
     * last member (in order not to have to copy all NET_LOG_MAX+1 bytes of
     * text[] if the string in text[] is shorter).
     */
    enum {NET_LOG_MAX=127};
    typedef struct {
      unsigned seq;             // A unique sequence number associated
				// with this message
      short bad;                // True if the text describes an error
				// condition
      char text[NET_LOG_MAX+1]; // The text of the log message 

      bool end;
    } NetLogMsg;
    
    /*
     * The following object is used by the Scanner task to report
     * the completion of noise source transactions.
     */
    typedef struct {
      NetUlong seq;      /* The sequence number of the transaction that completed */
    } NetNoiseDoneMsg;
    
    /*
     * The following object is used by the receiver-control task to report
     * the completion of a setreg transactions.
     */
    typedef struct {
      NetUlong seq;      /* The sequence number of the transaction that completed */
    } NetSetregDoneMsg;
    
    /*
     * The following object is used by the scanner task to report
     * the completion of a tv_offset transaction.
     */
    typedef struct {
      NetUlong seq;      /* The sequence number of the transaction that completed */
    } NetTvOffsetDoneMsg;
    
    /*
     * The following object is used by the tracker task to report
     * target acquisition.
     */
    typedef struct {
      NetUlong seq;      /* The sequence number of the transaction
			    that completed */
    } NetCalTertDoneMsg;

    /*
     * The following object is used by the tracker task to report
     * target acquisition.
     */
    typedef struct {
      NetUlong seq;      /* The sequence number of the transaction
			    that completed */
    } NetFrameDoneMsg;
    
    /*
     * The following object is used by the tracker task to report
     * target acquisition.
     */
    typedef struct {
      NetUlong seq;      // The sequence number of the transaction
			 // that completed
    } NetCanDoneMsg;

    /*
     * The following object is used by the rx task to report
     * target IF switch acquisition.
     */
    typedef struct {
      NetUlong seq;      /* The sequence number of the transaction
			    that completed */
    } NetIFModDoneMsg;

    /*
     * The following object is used by the tracker task to report
     * target acquisition.
     */
    typedef struct {
      NetUlong seq;      /* The sequence number of the transaction
			    that completed */
    } NetPmacDoneMsg;
    
    /*
     * The following object is used by the tracker task to report
     * if the telescope can't reach the current source due to it being
     * too low in the sky.
     */
    typedef struct {
      NetUlong seq;      /* The sequence number of the transaction that resulted */
      /*  in the telescope needing to point too low. */
    } NetSourceSetMsg;
    
    /*
     * Create a union of the above message containers.
     */
    typedef union {
      NetGreetingMsg greeting;    // A greeting message
      NetLogMsg log;              /* A message to be logged */
      NetNoiseDoneMsg noise_done; /* A noise source completion message */
      NetPmacDoneMsg pmac_done;   /* A tracker transaction-completion message */
      NetSourceSetMsg source_set; /* A source-has-set advisory message */
      NetSetregDoneMsg setreg_done; /* A setreg transaction completion message */
      NetTvOffsetDoneMsg tv_offset_done;// A tv_offset transaction
				      // completion message
      NetCalTertDoneMsg calTertDone;  // A caltert transaction
				      // completion message
      NetIFModDoneMsg IFModDone;      // An antenna IF transaction
				      // completion message
      NetCanDoneMsg canDone;          // A CAN completion message
      NetFrameDoneMsg frameDone;      // A FRAME completion message
      NetNoiseDoneMsg noiseDone;      // A noise source completion message
    } NetMsg;
    
    struct RtcNetMsg {
      NetUlong antenna;
      NetMsg msg;
      unsigned size();
    };
  }
}
/*
 * The following object description table, defined in rtcnetcoms.c,
 * describes the messages that are sent from the real-time controller
 * to the control program.
 */
extern const NetObjTable rtc_msg_table;

namespace sza {
  namespace array {    
    /*-----------------------------------------------------------------------
     * Define the ids of controller to control-program commands along with
     * the corresponding message containers.
     */
    typedef enum {
      NET_SHUTDOWN_CMD,    // A request to shutdown the control system 
      NET_INTERVAL_CMD,    // A request to change the hardware integration time 
      NET_INHIBIT_CMD,     // Inhibit or resume integration readout 
      NET_STROBE_CMD,      // A request to take a register snapshot 
      NET_SETREG_CMD,      // Change the value of a given register 
      NET_GETREG_CMD,      // Change the value of a given register 
      NET_SETDIO_CMD,      // Modify the output register of a digital I/O board 
      NET_UNFLAG_CMD,      // Resume monitoring of a previously unreachable board 
      NET_PHASE_MOTOR_CMD, // Switch on or off a given set of phase shift motors 
      NET_PHASE_SHIFT_CMD, // Set the positions of a given set of phase shifters 
      NET_SELECT_RX_CMD,   // Select a receiver type as the target of
			   // the next command
      NET_SET_BIAS_CMD,    // Set a receiver bias
      NET_RX_HEATER_CMD,   // Switch on or off a given set of heaters 
      NET_RX_COLDHEAD_CMD, // Switch on or off a given set of coldheads 
      NET_RX_TEMP_CMD,     // Set the temperatures of a given set of heaters 
      NET_LO_CMD,          // A command to configure the LO chain
      NET_RX_QUAD_CMD,     // Set the state of the phase quadrature network 
      NET_RX_POLAR_CMD,    // Define a polarization state-encoder
			   // association for a receiver
      NET_POLWALSH_CMD,    // Define a polarization state-encoder
			   // association for a receiver
      NET_SITE_CMD,        // Specify the location of the SZA 
      NET_LOCATION_CMD,    // Specify the location of an antenna
      NET_DELAYREF_CMD,    // Specify the location of the delay reference
      NET_INIT_CMD,        // Mandatory controller initialization command 
      NET_HALT_CMD,        // Halt the telescope drives 
      NET_SLEW_CMD,        // Slew to a given az,el,dk 
      NET_TRACK_CMD,       // Append to the track of the current source 
      NET_MOUNT_OFFSET_CMD,// Adjust the az,el,dk tracking offsets 
      NET_EQUAT_OFFSET_CMD,// Adjust the equatorial ra and dec tracking offsets 
      NET_TV_OFFSET_CMD,   // Adjust the tracking offsets to move a
			   // star by a on the tv monitor of the
			   // optical telescope. 
      NET_TV_ANGLE_CMD,    // The deck angle at which the vertical
			   // direction on the tv monitor of the
			   // optical telescope matches the direction
			   // of increasing topocentric elevation.
      NET_SKY_OFFSET_CMD,  // Tell the tracker to continually adjust
			   // the tracking offsets to maintain the
			   // telescope pointed at a given fixed sky
			   // offset from the normal pointing center.
      NET_UT1UTC_CMD,      // Append to the UT1-UTC interpolation table 
      NET_EQNEQX_CMD,      // Append to the equation-of-the-equinoxes
			   // interpolation table
      NET_ENCODER_CALS_CMD,  // Tell the tracker task the current encoder scales
      NET_ENCODER_LIMITS_CMD,// Tell the tracker task the current encoder limits
      NET_ENCODER_ZEROS_CMD, // Set the zero points of the telescope encoders 
      NET_SLEW_RATE_CMD,  // Set the slew rate of each telescope axis 
      NET_TILTS_CMD,      // Calibrate the axis tilts of the telescope 
      NET_FLEXURE_CMD,    // Calibrate the axis flexure of the telescope 
      NET_COLLIMATE_CMD,  // Calibrate the collimation of the telescope 
      NET_MODEL_CMD,      // Select the optical or radio pointing model 
      NET_YEAR_CMD,       // Tell the control system what the current year is 
      NET_DECK_MODE_CMD,  // Tell the tracker how to position the deck
			  // axis while tracking sources.
      NET_ATMOS_CMD,      // Supply atmospheric conditions to weather task 
      NET_FEATURE_CMD,    // Change the set of feature markers to be
			  // recorded in one or more subsequent
			  // archive frames.
      NET_GPIB_SEND_CMD,  // Send a message to a given GPIB device  
      NET_GPIB_READ_CMD,  // Read a message from a given GPIB device  
      NET_POWER_CMD,      // Turn the power on/off to an antennas power strip
      NET_POWER_DT_CMD,   // Set the interval of noise-cal power-meter readings 
      NET_POWER_METER_CMD,// Send a command to the power meter 
      NET_NOISE_CAL_CMD,  // Connect or disconnect the noise-calibration source 
      NET_CHZR_POWER_CMD, // Initiate a command to search for the
			  // attenuations that result in outputs from
			  // specified total-power detectors that are
			  // close to a given power level.
      NET_CHZR_ZERO_CMD,  // Initiate a command to measure the
			  // zero-offset of one or more total-power
			  // detectors
      NET_CHZR_ATTN_CMD,  // Set the attenuations of one or more IF channels 
      NET_CHZR_SWITCH_CMD,// Turn one or more IF amplifiers on or off 
      NET_CHZR_ENABLE_CMD,// Temporarily disable, or reenable the channelizer 
      NET_RXSIM_CMD,      // Configure the receiver simulation box 
      NET_THERMO_CMD,     // Send a command to the DS1820 digital thermometers 
      NET_DS_DT_CMD,      // Set the interval for DS1820 temperature readouts. 
      NET_CAMERA_CMD,     // Forward a command to the optical camera controller. 
      NET_OPTCAM_CNTL_CMD,// Turn the optical camera/stepper motor on/off 
      NET_STEPPER_CMD,    // Step the stepper motor 
      NET_GRABBER_CMD,    // Step the stepper motor 
      NET_FLATFIELD_CMD,  // Toggle flat fielding of frame grabber frames 
      NET_FG_CMD,         // Write to a register on the frame grabber board 
      NET_CONFIGURE_FG_CMD, // Configure the frame grabber 
      NET_TPCAL_CMD,      // Install total power calibration factors 
      NET_WALSH_CMD,      // Toggle slow walshing on or off 
      NET_PAGER_CMD,      // Turn the pager on or off 
      NET_REBOOT_PMAC_CMD,// Reboot the pmac 
      
      // Downconverter control commands
      
      NET_PSYS_CMD,        // Set the DC power
      NET_PSYS_ATTEN_CMD,  // Set the Psys attenuation
      NET_IFOUT_CMD,       // Set the IF output power
      NET_IFOUT_ATTEN_CMD, // Set the IF output attenuation
      NET_RF_AMP_CMD,      // Enable the RF amp
      NET_IF_ALC_CMD,      // Enable the IF automatic level control
      
      // Noise Source control commands
      
      NET_NOISE_POWER_CMD,       // Set the noise source power.
      NET_NOISE_ATTEN_CMD,       // Set the noise source attenuation.
      NET_TONE_ATTEN_CMD,        // Set the tone attenuation.
      NET_NOISE_CMD,             // Turn the noise source on/off
      NET_TONE_CMD,              // Turn the tone on/off
      
      // Quadrature Modulator control commands
      
      NET_QUAD_POWER_CMD,        // Set the noise source power.
      NET_QUAD_ATTEN_CMD,        // Set the noise source attenuation.
      NET_QUAD_WALSH_COLUMN_CMD, // Set the tone attenuation.
      NET_QUAD_CMD,              // Turn the noise source on/off.
      NET_QUAD_WALSH_TABLE_CMD,  // Set the tone attenuation.
      NET_QUAD_PHASE_CMD,        // Set the phase state of the QuadMod.
      
      // CAN module reset command
      
      NET_RESET_CMD,             // Reset a module.

      // Receiver CAN commands

      NET_FAST_SAMPLING_CMD,     // Set fast sampling

      // Delay commands

      NET_SET_ANTENNA_COORDS_CMD,// Set antenna coordinates

      // Lobe Rotator commands

      NET_DDS_CMD,               // Command the output state of the
				 // DDS channels of the lobe rotator
      NET_SET_ANTENNA_DDS_CMD,   // Associate a DDS channel with an
				 // antenna

      NET_SET_LR_PHASE_CMD,      // Set the phase on a LR DDS or input channel    
      NET_SET_LR_FREQ_CMD,       // Set the frequency on a LR of input channel
      NET_ENABLE_DDS_WALSHING_CMD,// Set the walshing state on a DDS channel
      NET_SET_DDS_WALSH_COLUMN_CMD,// Set the walsh state on a DDS channel
      NET_SET_OUTPUT_REGS_CMD,   // Set the delay on a LR input channel
      NET_SET_LR_DELAY_CMD,      // Set the delay on a LR input channel
      NET_SET_ANTENNA_PHASE_CMD, // Set the phase on an antenna channel    
      NET_SET_ANTENNA_FREQ_CMD,  // Set the frequency on an antenna channel
      NET_SET_ANTENNA_PARAMS_CMD,// Set antenna parameters
      NET_SET_DELAY_CMD,         // Set a delay
      NET_SET_DEFAULT_DELAY_CMD, // Set a default delay
      NET_REF_ANT_CMD,           // Select a reference antenna
      NET_SET_WEATHER_PARAMS_CMD,// Set weather parameters
      NET_USE_DELAY_CMD,         // Use a delay
      NET_SCAN_CMD,              // Scan

      // CalTert command

      NET_CALTERT_CMD,

      // Antenna IF command

      NET_IFMOD_CMD,

      // IntMod command

      NET_INTMOD_CMD,

      // Flip delay signs?

      NET_FLIP_DELAY_CMD,

      // Toggle fringe tracking on/off

      NET_FRINGE_TRACKING_CMD,

      // A command for the Thermal module

      NET_THERMAL_CMD,

      // A command for the Tiltmeter module

      NET_TILTMETER_CMD,

      // A command for the synthesizer

      NET_SYNTH_CMD,

      NET_ENABLE_FREQUENCY_OFFSET_CMD, // Enable lobe-rotating with a
				       // frequency offset
    } NetCmdId;
    
    /**
     * Enumerate control-system shutdown options.
     */
    typedef enum {
      HARD_RESTART,             /* Restart the control system via a reboot */
      SOFT_RESTART,             /* Re-initialize the running control-system */
      HARD_SHUTDOWN,            /* Reboot the CPU and wait at the boot prompt */
      SOFT_SHUTDOWN             /* Terminate the control system but don't reboot */
    } RtcShutdownMethod;
    
#define SCAN_NET_NPT 10
    
    /*
     * The NetScanCmd is used to command the telescope to perform a scan.
     * Sequential commands containing the same scan name will be combined
     * to form a scrolling table of offsets of up to SCAN_NET_NPT points
     * at a time.
     *
     * A non-zero sequence number will indicate the start of a new scan.
     */
    typedef struct {
      char name[SCAN_LEN]; /* The name of the scan (see cbiregs.h for
			      SCAN_LEN) */
      NetUlong seq;        /* The sequence number of this command */
      NetUlong index[SCAN_NET_NPT];
      NetUlong npt;        /* The number of points sent with this
			      command */
      long azoff[SCAN_NET_NPT];
      long eloff[SCAN_NET_NPT];
      long dkoff[SCAN_NET_NPT];
    } NetScanCmd;
    
    /*
     * Define the contents of the shutdown command.
     */
    typedef struct {             /* The context of NET_SHUTDOWN_CMD */
      NetEnum method;            /* A RtcShutdownMethod enumeration identifier */
    } NetShutdownCmd;
    
    /*
     * The following command sets the hardware integration interval.
     */
    typedef struct {             /* The context of NET_INTERVAL_CMD */
      unsigned short exponent;   /* Integration time is (12.8us * 2^(6+exponent)) */
    } NetIntervalCmd;
    
    /*
     * The following command sets or clears the integration inhibit flag.
     */
    typedef struct {             /* The context of NET_INHIBIT_CMD */
      NetBool flag;              /* True to inhibit, false to resume readout */
    } NetInhibitCmd;
    
    /*
     * The slow_walsh command toggles slow walshing on or off.
     */
    
    typedef struct {
      NetBool on;   /* True to start slow walshing. False to turn it off. */
    } NetWalshStateCmd;
    
    /*
     * The pager command turns the pager on or off
     */
    typedef struct {
      NetEnum state;              /* A PagerState enumerator */
    } NetPagerCmd;
    /*
     * Enumerate the supported pager states.
     */
    typedef enum {
      PAGER_ON,     /* Turn the pager on */
      PAGER_OFF,    /* Turn the pager off */
      PAGER_IP,
      PAGER_EMAIL,
      PAGER_ENABLE, /* Enable paging */
      PAGER_DISABLE,/* Disable paging */
      PAGER_CLEAR,  /* clear pager */
      PAGER_LIST    /* list pager regs */
    } PagerState;
    /*
     * The setreg command sets the value of a specified register to a given
     * value.
     */
    typedef struct {
      unsigned long value;       /* The value to write to the register */
      unsigned short board;      /* The host-board of the register */
      unsigned short block;      /* The block number of the register */
      unsigned short index;      /* The index of the first element to be set */
      unsigned short nreg;       /* The number of elements to be set */
      unsigned long seq;         /* The sequence number of this transaction */
    } NetSetregCmd;
    
    /*
     * The getreg command reads the value of a specified register out of the
     * register map
     */
    typedef struct {
      unsigned short board;      /* The host-board of the register */
      unsigned short block;      /* The block number of the register */
      unsigned short index;      /* The index of the first element to be set */
    } NetGetregCmd;
    
    /*
     * The setdio command sets the value of the 4-byte output register of
     * a digital I/O card as a 32-bit bit-mask.
     *
     * Specify the operations that can be used to combine a template bit mask with
     * an established bit mask.
     */
    typedef enum {
      BIT_MASK_SET,    /* For each bit that is set in the template, set the */
      /*  corresponding bit in the established bit mask. */
      BIT_MASK_CLEAR,  /* For each bit that is set in the template, clear the */
      /*  corresponding bit in the established bit mask. */
      BIT_MASK_ASSIGN, /* Set the established bit mask equal to the template. */
    } BitMaskOper;
    
    typedef struct {
      unsigned long value;       /* The value to write to the register */
      unsigned short board;      /* The host-board of the register */
      NetEnum oper;              /* A BitMaskOper enumerator */
    } NetSetDioCmd;
    
    /*
     * A board becomes flagged as unreachable after a bus or access error occurs
     * wrt any of its registers. Once this happens, modules such as the scanner
     * stop trying to access the board. After fixing the problem, or if the
     * problem is transient, the unflag command can be used to mark the board
     * as reachable again. If this turns out to be incorrect and another bus
     * error occurs on a subsequent register access, the board will be reflagged.
     */
    typedef struct {
      unsigned short board;  /* The register map index of the board to unflag */
    } NetUnflagCmd;
    
    /*
     * The following command turns on or off a given set of phase-shifter
     * motors.
     */
    typedef struct {
      NetBool on;         /* True to switch the specified motors on, false */
      /*  to switch them off */
      NetMask receivers;  /* The bit-set of receivers whose shifters are to */
      /*  be switched. The phase shifter of receiver n */
      /*  (n=0..12) is to be switched if set&(1<<n))!=0. */
    } NetPhaseMotorCmd;
    
    /*
     *  The NetSetPolarCmd command sets up an association between a
     *  polarization state and an encoder position for a given set of
     *  receivers.  
     */
    typedef enum {
      LEFT,       /* Left circular polarization */
      RIGHT       /* Right circular polarization */
    } PolarState;
    
    typedef struct {
      NetMask receivers;  /* The bit-set of receivers whose shifters are to */
      /*  be switched. The phase shifter of receiver n */
      /*  (n=0..12) is to be switched if set&(1<<n))!=0. */
      NetEnum state;      /* The PolarState enumeration */
      NetUlong posn;      /* The desired encoder position (1..1023) */
      
    } NetRxPolarCmd;
    
    /*
     * The NetPolWalshCmd command sets the polarization state of receivers to the
     * requested step of receiver Walsh functions
     */
    typedef struct {
      NetUlong seq;       /* The sequence number of this command */
      NetMask receivers;  /* The bit-set of receivers whose shifters are to */
      /*  be switched. The phase shifter of receiver n */
      /*  (n=0..12) is to be switched if set&(1<<n))!=0. */
      NetBool half_step;  /* True to use a half-step for extra precision */
      NetUlong walshstep; /* The desired step of the Walsh cycle */
    } NetPolWalshCmd;
    
    /*
     * The following command sets the positions of a given set of phase-shifter
     * motors.
     */
    typedef struct {
      NetUlong seq;      /* The sequence number of this command */
      NetUlong posn;     /* The desired encoder position (0..1023) */
      NetBool half_step; /* True to use a half-step for extra precision */
      NetMask receivers; /* The bit-set of receivers whose shifters are to */
      /*  be moved. The phase shifter of receiver n, */
      /*  (n=0..12) is to be switched if set&(1<<n))!=0. */
    } NetPhaseShiftCmd;
    
    /*
     * The following command selects a receiver
     */
    typedef struct {
      NetEnum band;   /* The receiver to select */
      unsigned seq;
    } NetSelectRxCmd;
    
    /*
     * The following command sets a bias
     */
    typedef enum {
      AMP  = 0x1, // Biasing an amplifier
      RX   = 0x2, // Biasing a whole receiver
    } BiasType;

    typedef struct {
      NetEnum amp;  // The amplifier bias to select 
      short bias;   // The value to set
      NetEnum biasType; // Are we biasing an amplifier or a receiver?
      NetEnum rxId;     // The receiver to select 
      bool isDefault; // If true, just set the values as defaults
      unsigned seq;
    } NetSetBiasCmd;

    /*
     * The following command turns on or off a given set of heaters.
     */
    typedef struct {
      NetBool on;        /* True to switch the specified heaters on, false */
      /*  to switch them off */
      NetMask heaters;   /* The bit-set of heater types to be switched, the 3 */
      /*  least significant bits correspond to the first */
      /*  three elements of the rxn.htr_on[] register */
      NetMask receivers; /* The bit-set of receivers whose heaters are to */
      /*  be switched. The heater of receiver n */
      /*  (n=0..12) is to be switched if set&(1<<n))!=0. */
    } NetRxHeaterCmd;
    
    /*
     * The following command turns on or off a given set of coldheads.
     */
    typedef struct {
      NetMask receivers; /* The bit-set of receivers whose heaters are to */
      /*  be switched. The heater of receiver n */
      /*  (n=0..12) is to be switched if set&(1<<n))!=0. */
      NetBool on;        /* True to switch the specified coldheads on, false */
      /*  to switch them off */
    } NetRxColdheadCmd;
    
    /*
     * The following command sets the target sensor output voltages of the
     * control-loops of a given set of heaters.
     */
    typedef struct {
      NetUlong value;    /* The output voltage multiplied by 409.6, (0-4096) */
      NetMask heaters;   /* The bit-set of heater types to be switched, the 3 */
      /*  least significant bits correspond to the first */
      /*  three elements of the rxn.htr_req[] register */
      NetMask receivers; /* The bit-set of receivers whose heaters are to */
      /*  be switched. The heater of receiver n */
      /*  (n=0..12) is to be set if set&(1<<n))!=0. */
    } NetRxTempCmd;
    
    typedef enum {
      LO_NONE,
      LO_DAMPGAIN,
      LO_FREQ,
      LO_LO_FREQ,
      LO_DEFAULT_FREQ,
      LO_LOOPGAIN,
      LO_TOGGLE,
      LO_VOLTAGE,
      LO_DEFAULT_VOLTAGE,
      LO_ID,
      LO_TUNINGTABLE,
      LO_ONEWIRE,
      LO_AUTOLOCK,
      LO_SETDACCOEFF,
      LO_POS_GUNN_DEVICE,
      LO_JOG_GUNN_DEVICE,
      LO_HOME_GUNN_DEVICE
    } LoMsgId;

    /*
     * The following command enables or disables a given set of local
     * oscillator stages in a given set of receivers.
     */
    typedef struct {
      NetEnum msgId;  // Which LO message is this?
      NetEnum oscs;   // The bit-set of oscillators whose stages we
		      // wish to switch on or off. 
      NetEnum stages; // The bit-set of stages to switch on or off. The 
      NetEnum rxId;   // A mask of Rx::Ids
      NetBool on;     // True to switch the specified stages on,
		      // false to switch them off
      unsigned short dampGain;  // Used to set the damping gain of the Yig
      unsigned short frequency; // Used to set the frequency of the Yig
      unsigned short loopGain;  // Used to set the loop gain of Var/Yig
      unsigned short voltage;   // Used to set the voltage of the Yig
      unsigned char id;         // Used to set the Yig/Gunn ID
      unsigned char month;      // Used to set the Yig/Gunn ID
      unsigned char day;        // Used to set the Yig/Gunn ID
      unsigned char year;       // Used to set the Yig/Gunn ID
      unsigned char npt;        // Used to set the Gunn ID
      float coeff;              // Used to set the DAC coefficient
      int tunerPos;
      int backshortPos;
      int attenPos;
      int position;             // A position to command 

    } NetLoCmd;
    
    /*.......................................................................
     * The following command turns fast sampling on/off
     */
    typedef struct {
      NetUlong channel;
      NetBool start;
    } NetFastSamplingCmd;

    /**.......................................................................
     * Enumerate types of delays we know about.
     */
    typedef enum {
      FIXED        = 0x1,
      ADJUSTABLE   = 0x2,
      GEOMETRIC    = 0x4,
      THERMAL      = 0x8,
      IONOSPHERIC  = 0x10,
      TROPOSPHERIC = 0x20,
      CORR_OFFSET  = 0x40,
      NIA          = 0x80
    } DelayType;

    /**.......................................................................
     * The following command specifies an antenna location
     */
    typedef struct {
      double x;
      double y;
      double z;
      double longitude;
      double latitude;
      double axisMis;
    } NetSetAntennaCoordsCmd;

    /**.......................................................................
     * Enumerate DDS states we know about
     */
    typedef enum {
      DDS_ENABLE,
      DDS_DISABLE,
      DDS_RESET,
    } DDSState;

    /**.......................................................................
     * The following command associates a DDS channel with an antenna
     */
    typedef struct {
      NetEnum state;
    } NetDDSCmd;

    /**.......................................................................
     * The following command associates a DDS channel with an antenna
     */
    typedef struct {
      long ddsId;
    } NetSetAntennaDDSCmd;

    /**.......................................................................
     * The following command sets a phase on the specified DDS or input channel
     */
    typedef struct {
      bool enable;
    } NetEnableLrFrequencyOffsetCmd;

    /**.......................................................................
     * The following command sets a phase on the specified DDS or input channel
     */
    typedef struct {
      long input;
      short phase;
      NetEnum type;
    } NetSetLRPhaseCmd;

    /**.......................................................................
     * The following command sets a frequency on the specified DDS or input channel
     */
    typedef struct {
      long input;
      double freq;
      NetEnum type;
    } NetSetLRFreqCmd;

    typedef enum {
      LR_DDS,
      LR_INPUT
    } LRType;

    /**.......................................................................
     * The following command sets a frequency on the specified DDS or
     * input channel
     */
    typedef struct {
      long input;
      long column;
    } NetSetDDSWalshColumnCmd;

    /**.......................................................................
     * The following command sets a frequency on the specified DDS or input channel
     */
    typedef struct {
      long input;
      bool enable;
    } NetEnableDDSWalshingCmd;

    /**.......................................................................
     * The following command sets a delay on the specified LR input channel
     */
    typedef struct {
      long input;
      double delay;
      double mjd;
      NetBool disc;
    } NetSetLRDelayCmd;

    /**.......................................................................
     * The following command sets a delay on the specified LR input channel
     */
    typedef struct {
      long input;
      long freg;
      long preg;
    } NetSetOutputRegsCmd;

    /**.......................................................................
     * The following command sets a phase for the specified antenna
     */
    typedef struct {
      short phase;
    } NetSetAntennaPhaseCmd;

    /**.......................................................................
     * The following command sets a frequency for the specified antenna
     */
    typedef struct {
      double freq;
    } NetSetAntennaFreqCmd;

    /**.......................................................................
     * The following command sets up parameters for an antenna
     */
    typedef struct {
      double pntRa;
      double pntDec;
      double phsRa;
      double phsDec;
      double freq;
      double distance;
      double mjd;
      NetBool discontinuity;
    } NetSetAntennaParamsCmd;

    /**
     * The following command sets weather parameters
     */
    typedef struct {
      double airTemp;
      double atmPressure;
      double dewPoint;
      double relHumid;
    } NetSetWeatherParamsCmd;

    /**.......................................................................
     * The following command sets up delays for a set of antennas
     */
    typedef struct {
      double  delay;
      NetEnum delayType;
      NetEnum rxId;
    } NetSetDelayCmd;

    /**.......................................................................
     * The following command toggles delays for a set of antennas
     */
    typedef struct {
      NetBool use;
      NetEnum delayType;
    } NetUseDelayCmd;

    /**.......................................................................
     * The following command toggles fringe tracking on/off
     */
    typedef struct {
      NetBool on;
      NetEnum target;
    } NetFringeTrackingCmd;

    /*
     * The following command selects between 4 quadrature phase shifts per
     * receiver.
     */
    
    typedef enum {   /* Enumerate the possible quadrature network states */
      QUAD_0  = 0,   /* No phase shift */
      QUAD_90 = 1,   /* A phase shift of 90 degrees */
      QUAD_180 = 2,  /* A phase shift of 180 degrees */
      QUAD_270 = 3   /* A phase shift of 270 degrees */
    } QuadPhase;
    
    typedef struct {
      NetEnum state;     /* A QuadPhase enumerator */
      NetMask receivers; /* The bit-set of receivers whose oscillators are to */
      /*  be switched. The oscillator of receiver n */
      /*  (n=0..12) is to be switched if set&(1<<n))!=0. */
    } NetRxQuadCmd;
    
    /*
     * The following command is used to inform the control system of the
     * location of the SZA.
     */
#define SZA_MAX_ALT 10000.0 /* The max altitude above or below sea-level (m) */
    
    typedef struct {
      long lon;          /* The SZA longitude (east +ve) [-pi..pi] (milli-arcsec) */
      long lat;          /* The SZA latitude [-pi/2..pi/2] (milli-arcsec) */
      long alt;          /* The SZA altitude (mm) */
    } NetSiteCmd;
    
    typedef struct {
      double north;    /* The north offset of an antenna */
      double east;     /* The east offset of an antenna */
      double up;       /* The up offset of an antenna */
    } NetLocationCmd;

    typedef struct {
      double east;     /* The east offset of an antenna */
      double north;    /* The north offset of an antenna */
      double up;       /* The up offset of an antenna */
    } NetDelayRefCmd;

    /*
     * The first command sent to the controller after startup must be
     * the following. Note that this command can not be sent at any
     * other time.  When a connection is first initiated to the
     * translator layer, the scheduler automatically runs its
     * initialization script.  On start-up, the controller will send
     * this message with start=true, and after the scheduler has
     * completely sent the initialization script, it will queue this
     * command to be sent with start=false. 
     */
    typedef struct {
      NetBool start;
    } NetInitCmd;
    
    /*
     * The following object is sent to halt the telescope.
     */
    typedef struct {
      NetUlong seq;      // The tracker sequence number of this command 
      NetMask antennas;  // A bitwise union of antennas to control
    } NetHaltCmd;
    
    /*
     * The following object is sent to reboot the pmac
     */
    typedef struct {
      NetUlong seq;      /* The tracker sequence number of this command */
    } NetRebootPmacCmd;
    
    /*
     * The following object is used to request a telescope slew to a given
     * mount position.
     */
    typedef enum {        /* This must be kept in step with source.h::SourceAxes*/
      DRIVE_AZ_AXIS=1,    /* Slew the azimuth axis */
      DRIVE_EL_AXIS=2,    /* Slew the elevation axis */
      DRIVE_DK_AXIS=4,    /* Slew the deck axis */
      DRIVE_ALL_AXES = DRIVE_AZ_AXIS | DRIVE_EL_AXIS | DRIVE_DK_AXIS
    } DriveAxes;
    
    typedef struct {
      char source[SRC_LEN]; /* The name of the source (see szaregs.h for SRC_LEN) */
      NetUlong number;      /* The catalog number of this source */
      NetUlong seq;         /* The sequence number of this command */
      NetMask mask;         /* A bitwise union of DriveAxes enumerated */
      /*  bits used to specify which of the following axis */
      /*  positions are to be used. */
      long az;              /* The target azimuth (0..360 degrees in mas) */
      long el;              /* The target elevation (0..90 degrees in mas) */
      long dk;              /* The target deck angle (-180..180 in mas) */
      NetEnum type;         // An sza::util::Tracking::Type enumerator
    } NetSlewCmd;
    
    /*
     * The NetTrackCmd is used to command the telescope to track a source.
     * Sequential commands containing the same source name will be
     * combined to form a scrolling interpolation table of up to 3 points
     * at a time.
     *
     * On source changes the control program is expected to send three
     * ephemeris entries for the source, one preceding the current time
     * and two following it. It is thereafter expected to send a new
     * ephemeris entry whenever the current time passes that of the
     * second point in the table. The new entry must be for a later time
     * than the existing 3rd entry.
     *
     * On receipt of a track command with a new source name, the tracker
     * task will immediately command a slew to the first position received.
     * It is anticipated that by the time the slew ends, the control program
     * will have had more than enough time to send two more entries. If only
     * one more entry has been received, linear interpolation will be used. If
     * no new entries have been received then the details of the single entry
     * will be used without any interpolation.
     */

    typedef struct {
      char source[SRC_LEN]; /* The name of the source (see szaregs.h
			       for SRC_LEN) */
      NetUlong number;      /* The catalog number of the source */
      NetEnum srcType;      /* The source type */   
      long seq;             /* The sequence number of this command */
      long mjd;             /* The Terrestrial Time at which ra,dec are valid, */
      /*  as a Modified Julian Day number */
      long tt;              /* The number of TT milliseconds into day 'mjd' */
      long ra;              /* The desired apparent Right Ascension */
      /*  (0..360 degrees in mas) */
      long dec;             /* The desired apparent Declination */
      /*  (-180..180 degrees in mas) */
      long dist;            // The distance to the source if it is
			    // near enough for parallax to be
			    // significant. Specify the distance in
			    // micro-AU Send 0 for distant sources.
      NetEnum type;         // An sza::util::Tracking::Type enumerator
    } NetTrackCmd;
    
    /*
     * The following enumerators specify how new offsets are to effect
     * existing offsets.
     */
    typedef enum {
      OFFSET_ADD,     /* Add the new offsets to any existing offsets */
      OFFSET_SET      /* Replace the existing offsets with the new offsets */
    } OffsetMode;
    
    /*
     * The following command establishes new horizon pointing offsets.
     */
    typedef struct {
      NetUlong seq;   /* The tracker sequence number of this command */
      NetMask axes;   /* The set of axes to offset, as a union of SkyAxis */
      /*  enumerators. */
      NetEnum mode;   /* The effect of the offsets on existing offsets, chosen */
      /*  from the above OffsetMode enumerators. */
      long az,el,dk;  /* The offsets for the azimuth, elevation and deck axes */
      /*  Only those values that correspond to axes included in */
      /*  the 'axes' set will be used. */
    } NetMountOffsetCmd;
    
    /*
     * The following command establishes new equatorial pointing offsets.
     */
    typedef enum {      /* The set of offsetable equatorial axes */
      EQUAT_RA_AXIS=1,  /* The Right-Ascension axis */
      EQUAT_DEC_AXIS=2  /* The declination axis */
    } EquatAxis;
    
    typedef struct {
      NetUlong seq;   /* The tracker sequence number of this command */
      NetMask axes;   /* The set of equatorial axes to offset, as a union of */
      /*  EquatAxis enumerators */
      NetEnum mode;   /* The effect of the offsets on existing offsets, chosen */
      /*  from OffsetMode enumerators. */
      long ra,dec;    /* The offsets for the right-ascension and declination axes */
      /*  Only those values that correspond to axes included in */
      /*  the 'axes' set will be used. */
    } NetEquatOffsetCmd;
    
    /*
     * The following command asks the tracker to add to the az and el
     * tracking offsets such that the image on the tv monitor of the
     * optical-pointing telescope moves by given amounts horizontally
     * and vertically.
     */
    typedef struct {
      NetUlong seq; /* The tracker sequence number of this command */
      long up;      /* The amount to move the image up on the display (mas) */
      long right;   /* The amount to move the image right on the display (mas) */
    } NetTvOffsetCmd;
    
    /*
     * The following command sets the deck angle at which the vertical
     * direction on the tv monitor of the optical telescope matches
     * the direction of increasing topocentric elevation.
     */
    typedef struct {
      long angle;   /* The deck angle at which the camera image is upright (mas) */
    } NetTvAngleCmd;
    
    /*
     * The SkyOffset command tells the tracker to track a point at a given
     * fixed sky offset from the normal pointing center, regardless of
     * elevation or declination. This is used primarily for making beam
     * maps.
     */
    typedef enum {      /* Set members for the NetSkyOffsetCmd axes member. */
      SKY_X_AXIS = 1,   /* The NetSkyOffsetCmd::x axis */
      SKY_Y_AXIS = 2,   /* The NetSkyOffsetCmd::y axis */
    } SkyXYAxes;
    
    typedef struct {
      NetUlong seq; /* The tracker sequence number of this command */
      NetMask axes; /* The set of axes to offset, as a union of SkyXYAxes */
      /*  enumerators. */
      NetEnum mode; /* The effect of the new offsets on any existing offsets, */
      /*  chosen from OffsetMode enumerators. */
      long x,y;     /* The 2-dimensional angular offset, expressed as distances */
      /*  along two great circles that meet at right angles at the */
      /*  normal pointing center. The y offset is directed along */
      /*  the great circle that joins the normal pointing center */
      /*  to the zenith. The x offset increases along the */
      /*  perpendicular great circle to this, increasing from */
      /*  east to west. Both offsets are measured in */
      /*  milli-arcseconds. */
    } NetSkyOffsetCmd;
    
    /*
     * The NetUt1UtcCmd and NetEqnEqxCmd commands are used to send occasional
     * updates of variable earth orientation parameters.
     *
     * For each command the control system retains a table of the 3 most
     * recently received updates. These three values are quadratically
     * interpolated to yield orientation parameters for the current time.
     * On connection to the control system, the control program is expected
     * to send values for the start of the current day, the start of the following
     * day and the start of the day after that. Thereafter, at the start
     * of each new day, it should send parameters for a time two days
     * in the future.
     *
     * On startup of the control system, requests for ut1utc and eqex
     * will return zero. On receipt of the first earth-orientation command,
     * requests for orientation parameters will return the received values.
     * On the receipt of the second, requesters will receive a linear
     * interpolation of the parameters. On receipt of the third and subsequent
     * commands, requesters will receive quadratically interpolated values
     * using the parameters of the three most recently received commands.
     */
    typedef struct {
      long mjd;          /* The UTC to which this command refers as a Modified */
      /*  Julian Day number */
      long utc;          /* The number of UTC milliseconds into day 'mjd' */
      long ut1utc;       /* The value of ut1 - utc (us) */
    } NetUt1UtcCmd;
    
    typedef struct {
      long mjd;          /* The Terrestrial Time to which this command refers, */
      /*  as a Modified Julian day number */
      long tt;           /* The number of TT milliseconds into day 'mjd' */
      long eqneqx;       /* The equation of the equinoxes (mas) */
    } NetEqnEqxCmd;
    
    /*
     * The NetEncoderCalsCmd is used to calibrate the scales and directions
     * of the telescope encoders.
     */
    typedef struct {
      NetUlong seq;      /* The tracker sequence number of this command */
      long az;           /* Azimuth encoder counts per turn */
      long el;           /* Elevation encoder counts per turn */
      long dk;           /* Deck encoder counts per turn */
    } NetEncoderCalsCmd;

    /*
     * The NetEncoderZerosCmd is used to set the zero points of the telescope
     * encoders. The angles are measured relative to the position at which the
     * encoders show zero counts.
     */
    typedef struct {
      NetUlong seq; // The tracker sequence number of this command 
      double az;    // Azimuth encoder angle at zero azimuth, measured in
      // the direction of increasing azimuth (radians).
      double el;    // Elevation encoder angle at zero elevation,
      // measured in the direction of increasing elevation
      // (radians).
      double dk;    // Deck encoder angle at the deck reference position,
      // measured clockwise when looking towards the sky
      // (radians).
    } NetEncoderZerosCmd;

    /*
     * The NetSlewRateCmd is used to set the slew speeds of each of the
     * telescope axes. The speed is specified as a percentage of the
     * maximum speed available.
     */
    typedef struct {
      NetUlong seq;  /* The tracker sequence number of this command */
      NetMask mask;  /* A bitwise union of DriveAxes enumerated */
      /*  bits, used to specify which of the following axis */
      /*  rates are to be applied. */
      long az;       /* Azimuth slew rate (0-100) */
      long el;       /* Elevation slew rate (0-100) */
      long dk;       /* Deck slew rate (0-100) */
    } NetSlewRateCmd;

    /*
     * The NetTiltsCmd is used to calibrate the axis tilts of the
     * telescope.
     */
    typedef struct {
      NetUlong seq;  /* The tracker sequence number of this command */
      long ha;       /* The hour-angle component of the azimuth-axis tilt (mas) */
      long lat;      /* The latitude component of the azimuth-axis tilt (mas) */
      long el;       /* The tilt of the elevation axis perpendicular to the */
      /*  azimuth ring, measured clockwise around the direction */
      /*  of the azimuth vector (mas) */
    } NetTiltsCmd;

    /*
     * The NetFlexureCmd is used to calibrate the gravitational flexure of the
     * telescope.
     */
    typedef struct {
      NetUlong seq;  /* The tracker sequence number of this command */
      NetEnum mode;  /* An sza::util::PointingMode enumeration */
      long sFlexure;  /* Gravitational flexure (milli-arcsec per sine elevation) */
      long cFlexure;  /* Gravitational flexure (milli-arcsec per cosine elevation) */
    } NetFlexureCmd;

    /*
     * The NetCollimateCmd command is used to calibrate the collimation of
     * the optical or radio axes.
     */
    typedef struct {
      NetUlong seq; /* The tracker sequence number of this command */
      NetEnum mode; /* An sza::util::PointingMode enumeration */
      long x;      /* The magnitude of the azimuthal offset (mas) */
      long y;      /* The magnitude of the elevation offset (mas) */
      NetEnum addMode; // The effect of the new offsets on any
		       // existing offsets, chosen from OffsetMode
		       // enumerators.
    } NetCollimateCmd;

    /*
     * The NetEncoderLimitsCmd command tells the tracker task what the limits
     * on encoder values are.
     */
    typedef struct {
      NetUlong seq;     /* The tracker sequence number of this command */
      long az_min;      /* The lower azimuth limit (encoder counts) */
      long az_max;      /* The upper azimuth limit (encoder counts) */
      long el_min;      /* The lower elevation limit (encoder counts) */
      long el_max;      /* The upper elevation limit (encoder counts) */
      long pa_min;      /* The lower pa limit (encoder counts) */
      long pa_max;      /* The upper pa limit (encoder counts) */
    } NetEncoderLimitsCmd;

    /*
     * The NetModelCmd command selects between the optical and radio pointing
     * models.
     */
    typedef struct {
      NetUlong seq;     /* The tracker sequence number of this command */
      NetEnum mode;     /* A PointingMode enumeration */
    } NetModelCmd;

    /*
     * The NetYearCmd command tells the control system what the
     * current year is. This is necessary because the gps time-code reader
     * doesn't supply year information.
     */
    typedef struct {
      short year;       /* The current Gregorian year */
    } NetYearCmd;

    /*
     * The NetDeckModeCmd command tells the track task how to position
     * the deck axis while tracking a source.
     */
    typedef struct {
      NetUlong seq;     /* The tracker sequence number of this command */
      NetEnum mode;     /* A DeckMode enumerator from szaregs.h */
    } NetDeckModeCmd;

    /*
     * The atmosphere command is used to supply atmospheric parameters for
     * refraction computations in the weather-station task. It is not
     * needed when the weather station is functioning.
     */
    typedef struct {
      double temperature;         /* The outside temperature (mC) */
      double humidity;            /* The relative humidity (0-1)*1000 */
      double pressure;            /* The atmospheric pressure (micro-bar) */
    } NetAtmosCmd;

    /*
     * The feature command conveys a bit-mask to be added or removed from the
     * set of feature bits to be recorded with one or more subsequent archive
     * frames. Once a feature bit has been added to the transient or persistent
     * set of feature bits, it is guaranteed to be recorded in at least one
     * frame. For transient markers the feature bits are recorded in the next
     * frame only, whereas persistent feature markers will continue to appear
     * in subsequent frames until they are cancelled.
     */
    typedef enum {
      FEATURE_ADD,    /* Add the new set of feature bits to those that are */
      /*  to be recorded in subsequent frames. */
      FEATURE_REMOVE, /* Remove the specified set of features from those that */
      /*  have previously been registered with FEATURE_ADD */
      FEATURE_ONE     /* Add the new set of feature bits to the transient set */
      /*  which is to be recorded just in the next frame. */
    } FeatureMode;

    typedef struct {
      NetUlong seq;         /* The mark-command sequence number of this message */
      NetEnum mode;         /* What to do with the bit mask */
      unsigned long mask;   /* The bit-mask to merge with any existing bit mask. */
    } NetFeatureCmd;

    /*
     * Specify the maximum size of a GPIB data message.
     * Note that this effects both the size of network communications
     * buffers and the size of some message queue nodes, so it shouldn't
     * be made too large.
     */
#define GPIB_MAX_DATA 80

    /*
     * The gpib-send command tells the GPIB control task to try to send the
     * specified message to a given GPIB device.
     */
    typedef struct {
      unsigned short device;          /* The generic address of the target */
      /*  GPIB device (0..30). */
      char message[GPIB_MAX_DATA+1];  /* The message to be sent. */
    } NetGpibSendCmd;

    /*
     * The gpib-read command tells the GPIB control task to try to read a
     * message from a given GPIB device.
     */
    typedef struct {
      unsigned short device;          /* The generic address of the source */
      /*  GPIB device (0..30). */
    } NetGpibReadCmd;

    /*
     * Turns power on/off to one or more antenna breakers
     */
    typedef struct {
      NetUlong breaker;
      NetBool power;
    } NetPowerCmd;

    /*
     * The noise-times command changes the time between noise-cal power
     * readouts.
     */
    typedef struct {
      unsigned long seconds;   /* The interval between power readings (seconds) */
    } NetPowerDtCmd;

    /*
     * The NetPowerMeterCmd sends a command to the noise-cal power meter.
     *
     * Enumerate the supported power-meter commands in order of decreasing
     * priority. Commands with higher priority will be transacted before
     * those of lower priority.
     */
    typedef enum {
      NCAL_POWER_RESET,      /* Reinstate the power meter's power-on settings */
      NCAL_POWER_ZERO,       /* Perform a zero calibration */
      NCAL_POWER_READ,       /* Take periodic power readings */
      /* The next enumerator must always be last */
      NCAL_POWER_NCMD        /* The number of commands enumerated above */
    } NcalPowerCmd;

    typedef struct {
      NetEnum cmd;           /* One of the commands listed above */
    } NetPowerMeterCmd;

    /*
     * The chzr_power command initiates a search for the IF attenuations
     * that result in outputs from specified total-power detectors that
     * are close to a given power level.
     */
    typedef struct {
      NetUlong seq;         /* The sequence number of this command */
      NetMask bands;        /* The bit-set of bands whose attenuators are to */
      /*  be configured. The attenuator of band n */
      /*  (n=0..N_BAND-1) is to be configured if */
      /*  set&(1<<n))!=0. */
      NetMask receivers;    /* The bit-set of receivers whose attenuators are to */
      /*  be configured. The attenuator of receiver n */
      /*  (n=0..N_ANT-1) is to be configured if */
      /*  set&(1<<n))!=0. */
      float power;          /* The desired output of the total-power 
			       In calibrated units */
    } NetChzrPowerCmd;

    /*
     * The chzr_zero command initiates a procedure that measures the
     * zero-offset of a given set of total-power detectors.
     */
    typedef struct {
      NetUlong seq;         /* The sequence number of this command */
      NetMask bands;        /* The bit-set of bands whose sensor zero-offsets */
      /*  are to be found. Band n (n=0..N_BAND-1) is to */
      /*  be processed if (bands & (1<<n))!=0. */
      NetMask receivers;    /* The bit-set of receivers whose sensor */
      /*  zero-offsets are to be measured. Receiver n */
      /* (n=0..N_ANT-1) is to be configured if */
      /*  (receivers & (1<<n))!=0. */
    } NetChzrZeroCmd;
    /*
     * The tpcal command installs total powers calibration factors for a given set'* of receivers and bands.
     */
    typedef struct {
      NetMask bands;         /* The bit-set of bands whose total power  
				calibration sare to be changed */
      NetMask receivers;     /* The bit-set of receivers whose total offsets
				are to be changed. */
      float offset;          /* The offset to apply to the total powers */
      NetBool offset_present;/* Was an offset change requested in this command? */
      float factor;          /* The factor to apply to the total powers */
      NetBool factor_present;/* Was an factor change requested in this command? */
    } NetTpcalCmd;

    /*
     * The following enumerators specify if the attn avlue is absolute
     * or additive
     */
    typedef enum {
      ATTN_ADD,     /* Add the new attn to the existing attn */
      ATTN_SET      /* Replace the existing attn with the new attn */
    } AttnMode;

    /*
     * The chzr_attn command sets the inserted attenuations of a given
     * set of IF channels.
     */
    typedef struct {
      NetMask bands;        /* The bit-set of bands whose attenuators */
      /*  are to be set. Band n (n=0..N_BAND-1) is to */
      /*  be configured if (bands & (1<<n))!=0. */
      NetMask receivers;    /* The bit-set of receivers whose attenuators */
      /*  are to be set. Receiver n (n=0..N_ANT-1) is */
      /*  to be configured if (receivers & (1<<n))!=0. */
      long attn;            /* The desired attenuation (-31-31db) */
      NetEnum mode;         /* Additive or absolute attenuation value */
    } NetChzrAttnCmd;

    /*
     * The chzr_switch command sets the on/off states of a given
     * set of IF channels.
     */
    typedef struct {
      NetMask bands;        /* The bit-set of bands whose switches */
      /*  are to be set. Band n (n=0..N_BAND-1) is to */
      /*  be configured if (bands & (1<<n))!=0. */
      NetMask receivers;    /* The bit-set of receivers whose switches */
      /*  are to be set. Receiver n (n=0..N_ANT-1) is */
      /*  to be configured if (receivers & (1<<n))!=0. */
      NetBool on;           /* True to switch the specified channels on, false */
      /*  to switch them off */
    } NetChzrSwitchCmd;

    /*
     * The chzr_enable command temporarily disables or reenables all of
     * the channelizer IF amplifiers. Note that the switch states are
     * ORed with those set by the chzr_switch command, so that on using
     * this command to reenable the IFs, those that had been disabled
     * separately by the chzr_switch command, remain disabled.
     */
    typedef struct {
      NetBool on;           /* True to switch the IFs on, false to switch */
      /*  them off */
    } NetChzrEnableCmd;

    /*
     * The noise_cal command connects or disconnects the noise-calibration
     * signal from all of the receivers.
     */
    typedef struct {
      NetBool on;           /* True to connect the noise-source, false to */
      /*  disconnect it. */
    } NetNoiseCalCmd;

    /*
     * The NetRxSimCmd configures the receiver simulation box.
     */
    typedef enum {          /* Enumerated bit values for NetRxSimCmd::what */
      RX_SIM_NOISE=1,       /* Configure the noise generators */
      RX_SIM_SKY=2,         /* Configure the fake-sky signal generator */
      RX_SIM_QUAD=4         /* Configure the quadrature phase-shifters */
  
    } RxSimSel;

    typedef struct {
      NetMask what;         /* A bitwise union of RxSimSel enumeration values */
      /*  specifying which of the following members should */
      /*  used to configure the receiver simulator. */
      NetMask noise;        /* The bit-set of receivers whose noise sources */
      /*  are to be enabled. Receiver n (n=0..N_ANT-1) is */
      /*  to be enabled if (noise & (1<<n))!=0. */
      NetMask quad;         /* The bit-set of receivers whose 90 degree */
      /*  phase-shifters are to be enabled. */
      /*  Receiver n (n=0..N_ANT-1) is to be enabled */
      /*  if (noise & (1<<n))!=0. */
      NetBool sky;          /* True to switch the fake sky signal on */
    } NetRxSimCmd;
    /*
     * Enumerate the allowed DS commands
     */
    typedef enum {
      DS_READ_CMD,    /* Read the temp from a single device */
      DS_READALL_CMD, /* Read the temp from all devices */
      DS_ADDRESS_CMD, /* Call out an address */
      DS_RESET_CMD,   /* Reset the controller */
      DS_INIT_CMD,    /* Re-initialize the serial port connection */
      DS_SEARCH_CMD,  /* Search the bus for devices */
      DS_ROMID_CMD,   /* Install a ROM id in the list of known devices */
      DS_FLAG_CMD,    /* Flag a device as unreachable */
      DS_UNFLAG_CMD,  /* Flag a device as reachable */
      DS_DISPLAY_CMD, /* Display known devices */
      DS_CHECK_CMD,   /* Add checksumming to commands */
      DS_NOCHECK_CMD, /* Remove checksumming */
    } DsCommand;

    /*
     * The thermometer command sends commands to the DS1820 digital thermometers.
     */
    typedef struct {
      NetEnum command;   /* The enumerated command above to send */
      unsigned char address[16];/* The optional address to send */
      NetUlong index;    /* The index to assign this address to */
      NetBool checksum;  /* Add checksumming? */
    } NetThermoCmd;
    /*
     * The ds-times command changes the time between DS1820 temperature
     * readouts.
     */
    typedef struct {
      unsigned long seconds;   /* The interval between power readings (seconds) */
    } NetDsDtCmd;
    /*
     * The NetStepperCmd controls the stepper motor.
     */
    typedef struct {
      long count;        /* The stepper motor count. */
    } NetStepperCmd;
    /*
     * The NetOptCamCntlCmd controls the camera/stepper motor power.
     */
    typedef struct {
      NetEnum target;    /* Which device to control */
      NetBool on;        /* Turn device on/off? */
    } NetOptCamCntlCmd;
    /*
     * The NetOptCamCmd controls miscellaneous camera functions.
     */
    typedef struct {
      NetEnum target;       /* The enumerated target of this operation */
      NetEnum action;       /* The action to take. */
    } NetOptCamCmd;
    /*
     * Enumerate the optical camera target options.
     */
    typedef enum {
      OPTCAM_FRAME,     /* Target is the frame grabber */
      OPTCAM_CAMERA,    /* Controls power to the camera */
      OPTCAM_CONTROLLER,/* Commands intended for the controller box. */
      OPTCAM_STEPPER,   /* Controls power to the stepper motor */
      OPTCAM_FOCUS,     /* Controls the stepper motor count. */
      OPTCAM_IRIS,      /* Camera iris commands. */
      OPTCAM_SHUT,      /* Controls camera shutter speed. */
      OPTCAM_SENS_AUTO, /* Electronic sensitivity enhancement. */
      OPTCAM_SENS_MANU, /* Electronic sensitivity enhancement. */
      OPTCAM_AGC,       /* Auto gain control. */
      OPTCAM_ALC,       /* Auto light control setting. */
      OPTCAM_MANU_IRIS, /* Manual Iris (Toggles to ELC?) */
      OPTCAM_SUPERD,    /* Super D mode */
    } OptCamTarget;
    /*
     * Enumerate the optical camera action options.
     */
    typedef enum {
      OPTCAM_ID=20000,/* Request the controller id. Note that the first element in
			 this list must be set to 20000 */
      OPTCAM_GRAB,    /* Tell the frame grabber to take a frame */
      OPTCAM_RBC,     /* Request the controller to drop buffered commands rbc. */
      OPTCAM_OPEN,    /* Open the iris */
      OPTCAM_CLOSE,   /* Close the iris */
      OPTCAM_STOP,    /* Stop the iris? */
      OPTCAM_PRESET,  /* Reset the iris, or when used with the
			 OPTCAM_CAMERA Target, reset the entire camera to
			 factory presets */
      OPTCAM_ON,      /* Turn target on */
      OPTCAM_OFF,     /* Turn target off */
      OPTCAM_INC,     /* Increment target */
      OPTCAM_DEC,     /* Decrement target */
      OPTCAM_LOW,     /* Set the gain to low */
      OPTCAM_MID,     /* Set the gain to mid */
      OPTCAM_HIGH,    /* Set the gain to high */
      OPTCAM_100,     /* Set shutter speed to 1/100 s */
      OPTCAM_250,     /* Set shutter speed to 1/250 s */
      OPTCAM_500,     /* Set shutter speed to 1/500 s */
      OPTCAM_1000,    /* Set shutter speed to 1/1000 s */
      OPTCAM_2000,    /* Set shutter speed to 1/2000 s */
      OPTCAM_4000,    /* Set shutter speed to 1/4000 s */
      OPTCAM_10000,   /* Set shutter speed to 1/10000 s */
      OPTCAM_CENTER,  /* Move to the peak of the image */
    } OptCamAction;
    /*
     * The NetFgCmd writes to miscellaneous frame grabber registers.
     */
    typedef struct {
      NetEnum target;       /* The enumerated target of this operation */
      unsigned long value;  /* The value to write to the register */
    } NetFgCmd;
    /*
     * The NetFlatFieldCmd toggles frame grabber flat fielding
     */
    typedef struct {
      NetBool on;  /* If true, flat field frame grabber images */
    } NetFlatFieldCmd;

    /*
     * Enumerate the frame grabber register options.
     */
    typedef enum {
      FG_INCSR1_REG,
      FG_INCSR2_REG,
      FG_OUTCSR_REG,
      FG_CURSOR_REG,
      FG_INDEX_REG, 
      FG_INLUT_REG, 
      FG_REDGRN_REG,
      FG_BLUE_REG,  
    } FgReg;

    /*
     * Enumerate the frame grabber peak options.
     */
    typedef enum {
      PEAK_X,     
      PEAK_Y,
      PEAK_XABS,
      PEAK_YABS,
    } Peak;

    /*
     * Enumerate the frame grabber image statistic options.
     */
    typedef enum {
      IMSTAT_PEAK,
      IMSTAT_SNR
    } Imstat;

    /**
     * Enumerate frame grabber configuration parameters
     */
    typedef enum {
      FG_NONE=0x0,
      FG_CHAN=0x1,
      FG_COMBINE=0x2,
      FG_FLATFIELD=0x4
    } FgOpt;

    /*
     * The NetConfigureFrameGrabberCmd configures the frame grabber.
     */
    typedef struct {
      unsigned mask;     /* The mask of parameters to configure */
      unsigned channel;  /* The channel number to selec */
      unsigned nCombine; /* The number of frames to combine */
      NetBool flatfield; // If true, flatfield the image
    } NetConfigureFrameGrabberCmd;

    //------------------------------------------------------------
    // Downconverter control commands.
    //------------------------------------------------------------

    // Set the Psys power

    typedef struct {
      double power;
      NetBool preset;
      NetMask bands;
    } NetPsysCmd;

    // Set the Psys attenuation

    typedef struct {
      double atten;
      NetMask bands;
    } NetPsysAttenCmd;

    // Set the Ifout power

    typedef struct {
      double power;
      NetBool preset;
      NetMask bands;
    } NetIfoutCmd;

    // Set the Ifout attenuation

    typedef struct {
      double atten;
      NetMask bands;
    } NetIfoutAttenCmd;

    // Enable the RF amplifier

    typedef struct {
      bool enable;
      NetMask bands;
    } NetRfAmpCmd;

    // Enable the IF automatic level control

    typedef struct {
      bool enable;
      NetMask bands;
    } NetIfAlcCmd;

    //-----------------------------------------------------------------------
    // Noise source commands.
    //-----------------------------------------------------------------------

    // Set the output power

    typedef struct {
      double power;
      NetBool preset;
    } NetNoisePowerCmd;

    // Set the noise source attenuation

    typedef struct {
      unsigned short atten;
    } NetNoiseAttenCmd;

    // Set the tone (?) attenuation

    typedef struct {
      unsigned short atten;
    } NetToneAttenCmd;

    //------------------------------------------------------------
    // Noise diode commands
    //------------------------------------------------------------

    typedef enum {
      NOISE_SIMPLE,  // Toggle just the noise diode
      NOISE_ALL      // Toggle noise diode, quadmods, and configure
		     // the correlator
    } NoiseType;

    // Turn the noise source on

    typedef struct {
      NetMask mask;   // A bitwise union of NoiseType enumerations 
      bool enable;    // True if the noise source is to be enabled  
      NetUlong seq;   // The sequence number of the transaction that completed 
    } NetNoiseCmd;

    // Turn the tone on

    typedef struct {
      bool enable;
    } NetToneCmd;

    //-----------------------------------------------------------------------
    // Quad Mod commands.
    //-----------------------------------------------------------------------

    // Set the output power

    typedef struct {
      double power;
      NetBool preset;
    } NetQuadPowerCmd;

    // Set the quad attenuation

    typedef struct {
      unsigned short atten;
    } NetQuadAttenCmd;

    // Set the quad walsh state column

    typedef struct {
      unsigned short column;
    } NetQuadWalshColCmd;

    // Turn quad modulation on

    typedef struct {
      bool enable;
    } NetQuadCmd;

    // Set the quad phase state

    typedef struct {
      unsigned short phase;
    } NetQuadPhaseCmd;

    //-----------------------------------------------------------------------
    // CAN module control
    //-----------------------------------------------------------------------

    typedef struct {
      NetEnum modules;
      NetMask bands;
      NetBool hard;
    } NetResetCmd;

    //-----------------------------------------------------------------------
    // CalTert control
    //-----------------------------------------------------------------------

    enum CalTertMsg {
      CALTERT_POSITION_CAL,
      CALTERT_HOME_TERT,
      CALTERT_POSITION_TERT,
      CALTERT_ENABLE_TERT,
      CALTERT_RESET_STEPPER,
      CALTERT_INDEX_TERT,
      CALTERT_ONE_WIRE,
      CALTERT_SET_ENCODER,
      CALTERT_STORE_ENCODER,
      CALTERT_NEW_TERT_POS_ANGLE,
      CALTERT_NEW_TERT_POS_RX
    };

    // Enumerate special values which will signify indexed tertiary positions

    enum TertPos {
      TERTPOS_RX30GHZ  = 0x10000, // Note that these must all be >= 2^16 (see
			 // szatypes.c, where they are used)
      TERTPOS_RX90GHZ  = 0x20000,
      TERTPOS_RX230GHZ = 0x40000,
    };

    typedef struct {
      NetEnum msgId;
      NetEnum rxId;
      short tertPosition;
      NetEnum calPosition;
      NetBool enable;
      NetEnum owDevice;
      NetEnum owCommand;
      NetUlong seq;
    } NetCalTertCmd;

    enum ThermalMsg {
      THERMAL_SET_TEMP,
      THERMAL_SET_MODE,
      THERMAL_SET_LOOP_GAIN,
      THERMAL_SET_INTEG_CONST,
      THERMAL_SET_LOOP_BW,
      THERMAL_SET_RATE_CONST,
      THERMAL_SET_PROP_CONST,
      THERMAL_SET_VOLTAGE_OFFSET,
      THERMAL_SET_EBOX_EQ_STATE,
      THERMAL_SET_EBOX_INT_ERROR
    };

    typedef struct {
      NetEnum msgId;
      NetEnum target;
      float value;
      NetEnum mode;
      NetEnum state;
    } NetThermalCmd;

    enum TiltmeterMsg {
      TILTMETER_SET_TEMP,
      TILTMETER_REGULATE_TEMP,
      TILTMETER_SET_LOOP_GAIN,
      TILTMETER_SET_INTEG_CONST,
      TILTMETER_SET_RATE_CONST,
      TILTMETER_SET_LOOP_BW,
      TILTMETER_WRITE_TO_EEPROM,
      TILTMETER_ZEROS,
    };

    enum TiltmeterMode {
      ON,
      OFF,
      MANUAL
    };

    typedef struct {
      NetEnum msgId;
      NetEnum mode;
      float value;
    } NetTiltmeterCmd;

    // Antenna IntMod command

    enum IntModMsg {
      INTMOD_SET_ATTEN,
      INTMOD_SET_DEFAULT_ATTEN,
      INTMOD_PRESET_POWER,
    };

    typedef struct {
      NetEnum msgId;
      unsigned char atten;
    } NetIntModCmd;

    // Antenna IF command

    enum IFModMsg {
      IFMOD_SET_LEVEL,
      IFMOD_SET_ATTEN,
      IFMOD_SET_DEFAULT_ATTEN,
      IFMOD_POSITION_SWITCH,
      IFMOD_SET_TO_DEFAULT
    };

    typedef struct {
      NetEnum msgId;
      NetEnum band;
      double level;
      NetEnum attenSet;
      double total;
      double input;
      double output;
      NetUlong seq;
      NetEnum pos;
    } NetIFModCmd;

    enum FlipDelayTarget {
      FLIP_DELAY = 0x1,
      FLIP_RATE  = 0x2,
      FLIP_BOTH  = FLIP_DELAY | FLIP_RATE
    };

    enum DelayTarget {
      DELAY_CORR = 0x1,
      DELAY_LR   = 0x2,
      DELAY_ALL  = DELAY_CORR | DELAY_LR
    };

    typedef struct {
      NetEnum target;
      NetEnum delayTarget;
      bool delay;
      bool rate;
    } NetFlipDelayCmd;

    enum SynthMsg {
      SYNTH_FREQ,
      SYNTH_POW,
      SYNTH_RFOUTPUT,
      SYNTH_OUTPUTMOD,
    };

    typedef struct {
      NetEnum msgId;
      double val;
      bool enable;
    } NetSynthCmd;

    /*
     * Create a union of the above command containers.
     */
    typedef union {
      NetShutdownCmd shutdown;
      NetIntervalCmd interval;
      NetInhibitCmd inhibit;
      NetSetregCmd setreg;
      NetSetDioCmd setdio;
      NetUnflagCmd unflag;
      NetPhaseMotorCmd phase_motor;
      NetPhaseShiftCmd phase_shift;
      NetSelectRxCmd selectRx;
      NetSetBiasCmd setBias;
      NetRxHeaterCmd rx_heater;
      NetRxColdheadCmd rx_coldhead;
      NetRxTempCmd rx_temp;
      NetLoCmd lo;
      NetRxQuadCmd rx_quad;
      NetRxPolarCmd rx_polar;
      NetPolWalshCmd rx_polwalsh;
      NetSiteCmd site;
      NetLocationCmd location;
      NetDelayRefCmd delayref;
      NetInitCmd init;
      NetHaltCmd halt;
      NetSlewCmd slew;
      NetTrackCmd track;
      NetMountOffsetCmd mount_offset;
      NetEquatOffsetCmd equat_offset;
      NetTvOffsetCmd tv_offset;
      NetTvAngleCmd tv_angle;
      NetSkyOffsetCmd sky_offset;
      NetUt1UtcCmd ut1utc;
      NetEqnEqxCmd eqneqx;
      NetEncoderCalsCmd encoder_cals;
      NetEncoderLimitsCmd encoder_limits;
      NetEncoderZerosCmd encoder_zeros;
      NetSlewRateCmd slew_rate;
      NetTiltsCmd tilts;
      NetFlexureCmd flexure;
      NetCollimateCmd collimate;
      NetModelCmd model;
      NetYearCmd year;
      NetDeckModeCmd deck_mode;
      NetAtmosCmd atmos;
      NetFeatureCmd feature;
      NetGpibSendCmd gpib_send;
      NetGpibReadCmd gpib_read;
      NetPowerCmd power;
      NetPowerDtCmd power_dt;
      NetPowerMeterCmd power_meter;
      NetNoiseCalCmd noise_cal;
      NetChzrPowerCmd chzr_power;
      NetChzrZeroCmd chzr_zero;
      NetTpcalCmd tpcal;
      NetChzrAttnCmd chzr_attn;
      NetChzrSwitchCmd chzr_switch;
      NetChzrEnableCmd chzr_enable;
      NetRxSimCmd rxsim;
      NetThermoCmd thermometer;
      NetDsDtCmd ds_dt;
      NetStepperCmd stepper;
      NetOptCamCmd camera;
      NetOptCamCntlCmd optcam_cntl;
      NetFgCmd fg;
      NetFlatFieldCmd flatfield;
      NetConfigureFrameGrabberCmd configureFrameGrabber;
      NetWalshStateCmd walshstate;
      NetPagerCmd pager;
      NetRebootPmacCmd reboot_pmac;
  
      // Downconverter control
  
      NetPsysCmd       psys;
      NetPsysAttenCmd  psys_atten;
      NetIfoutCmd      ifout;
      NetIfoutAttenCmd ifout_atten;
      NetRfAmpCmd      rf_amp;
      NetIfAlcCmd      if_alc;
  
      // Noise source control
  
      NetNoisePowerCmd noise_power;
      NetNoiseAttenCmd noise_atten;
      NetToneAttenCmd  tone_atten;
      NetNoiseCmd      noise;
      NetToneCmd       tone;
  
      // Quad Mod control
  
      NetQuadPowerCmd      quad_power;
      NetQuadAttenCmd      quad_atten;
      NetQuadWalshColCmd   quad_walshcol;
      NetQuadCmd           quad;
      NetQuadPhaseCmd      quad_phase;
  
      // CAN module commands
  
      NetResetCmd          reset;
      NetFastSamplingCmd   fast_sampling;

      // Delay commands
      
      NetSetAntennaCoordsCmd setAntennaCoords;
      NetDDSCmd              DDS;
      NetSetAntennaDDSCmd    setAntennaDDS;
      NetSetLRPhaseCmd       setLRPhase;
      NetSetLRFreqCmd        setLRFreq;
      NetEnableDDSWalshingCmd enableDDSWalshing;
      NetSetDDSWalshColumnCmd setDDSWalshColumn;
      NetSetOutputRegsCmd    setOutputRegs;
      NetSetLRDelayCmd       setLRDelay;
      NetSetAntennaPhaseCmd  setAntennaPhase;
      NetSetAntennaFreqCmd   setAntennaFreq;

      NetSetAntennaParamsCmd setAntennaParams;
      NetSetDelayCmd         setDelay;
      NetSetWeatherParamsCmd setWeatherParams;
      NetUseDelayCmd         useDelay;
      NetScanCmd             scan;

      // CalTert commands

      NetCalTertCmd          caltert;

      // Antenna IF command

      NetIFModCmd            IFMod;

      // IntMod 

      NetIntModCmd           intmod;

      // Flip delays

      NetFlipDelayCmd        flipDelay;

      // Fringe tracking

      NetFringeTrackingCmd   fringeTracking;

      // Thermal commands

      NetThermalCmd          thermal;

      // Tiltmeter commands

      NetTiltmeterCmd        tiltmeter;

      // Synth commands

      NetSynthCmd            synth;

      NetEnableLrFrequencyOffsetCmd enableLrFrequencyOffset;

    } NetCmd;

    /*
     * Create a structure that merges the above union with a mask of
     * antennas for whom the command is intended.
     */
    struct RtcNetCmd {
      NetMask antennas;
      NetCmd cmd;
    };
  }
}

/*
 * The following object description table, defined in rtcnetcoms.c,
 * describes the commands that are sent from the control program to
 * the real-time controller.
 */
extern const NetObjTable rtc_cmd_table;
#endif

