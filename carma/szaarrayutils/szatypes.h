#ifndef szatypes_h
#define szatypes_h

#include "carma/szaarrayutils/source.h"   /* SRC_NAME_MAX */
#include "carma/szaarrayutils/scan.h"     /* SCAN_NAME_MAX */
#include "carma/szaarrayutils/arraymap.h"   /* (RegMap *) */

/*-----------------------------------------------------------------------
 * The Date datatype uses astrom.h::input_utc() to read UTC date and
 * time values like:
 *
 *  dd-mmm-yyyy hh:mm:ss.s
 *
 * where mmm is a 3-letter month name. It stores the date as a Modified
 * Julian Date in DoubleVariable's. It provides + and - operators for
 * the addition and subtraction of Interval values, along with all of the
 * relational operators except the ~ operator.
 */
DataType *add_DateDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The Register datatype uses regmap.h::input_RegMapReg() to read
 * standard SZA archive-register specifications (eg. board.name[3-4]).
 *
 * It is stored in a RegisterVariable and supports the != == !~ ~ operators.
 */
DataType *add_RegisterDataType(Script *sc, char *name, ArrayMap *arraymap);

typedef struct {
  Variable v;       /* The base-class members of the variable (see script.h) */
  short regmap;     /* Register-map number */
  short board;      /* Register-board number */
  short block;      /* Block number on specified board */
  short index;      /* The first element of the block to be addressed */
  short nreg;       /* The number of elements to be addressed */
} RegisterVariable;

#define REGISTER_VARIABLE(v)  ((RegisterVariable *)(v))


/*-----------------------------------------------------------------------
 * The Wdir datatype is used to record writable directory names.
 * The path name is stored in a StringVariable. It supports
 * tilde expansion of home directories.
 *
 * It is stored in a StringVariable and supports the != == operators.
 */
DataType *add_WdirDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The Dir datatype is used to record the names of accessible directories.
 *
 * The path name is stored in a StringVariable. It supports
 * tilde expansion of home directories.
 *
 * It is stored in a StringVariable and supports the != == operators.
 */
DataType *add_DirDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The Dev datatype is used to record the names of accessible devices
 *
 * The path name is stored in a StringVariable. It supports
 * tilde expansion of home directories.
 *
 * It is stored in a StringVariable and supports the != == operators.
 */
DataType *add_DevDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The IntTime datatype is used to specify the hardware integration time
 * of the SZA electronics, as a power-of-2 exponent to be used to scale
 * the basic sample interval of 4.096e-4 seconds.
 *
 * It is stored in a UintVariable and supports != == <= >= < >.
 */
DataType *add_IntTimeDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The Board datatype is used to specify an archive register board by
 * name.
 *
 * It is stored in a UintVariable and supports the != == operator.
 */
DataType *add_BoardDataType(Script *sc, char *name, ArrayMap *arraymap);

/*-----------------------------------------------------------------------
 * The Time datatype is used to specify a time of day (UTC or LST).
 *
 * It is stored as decimal hours in a DoubleVariable. It supports the
 * standard arithmentic != == <= >= < > operators.
 */
DataType *add_TimeDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The Interval datatype is used to specify a time interval.
 *
 * It is stored as decimal seconds in a DoubleVariable. It supports the
 * standard arithmentic != == <= >= < > operators.
 */
DataType *add_IntervalDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The Outlet datatype is used to select an outlet
 */ 
DataType *add_OutletDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The Antennas datatype is used to select a sub-set of the SZA
 * receivers.
 *
 * It is stored in a SetVariable and supports the standard
 * set + - !~ ~ != == operators.
 */ 
DataType *add_AntennasDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The DDSChannel datatype is used to select a sub-set of the SZA
 * receivers.
 *
 * It is stored in a SetVariable and supports the standard
 * set + - !~ ~ != == operators.
 */ 
DataType *add_DDSChannelDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The Receivers datatype is used to select a sub-set of the 13 SZA
 * receivers.
 *
 * It is stored in a SetVariable and supports the standard
 * set + - !~ ~ != == operators.
 */ 
DataType *add_ReceiversDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The RxBand datatype is used to specify one or more receivers
 * generators.
 *
 * It is stored in a SetVariable and supports the standard
 * set + - !~ ~ != == operators.
 */
DataType *add_RxStageDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The RxBand datatype is used to specify one or more receivers
 * generators.
 *
 * It is stored in a SetVariable and supports the standard
 * set + - !~ ~ != == operators.
 */
DataType *add_RxBandDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The WalshStages datatype is used to specify one or more walsh function
 * generators.
 *
 * It is stored in a SetVariable and supports the standard
 * set + - !~ ~ != == operators.
 */
DataType *add_WalshStagesDataType(Script *sc, char *name);
/*
 * Define the bit-set values of walsh-generator set members. These
 * must be powers of two.
 */
typedef enum {
  WALSH_MOD = 1,   /* The modulation stage */
  WALSH_DEMOD = 2  /* The demodulation stage */
} WalshStages;

/*-----------------------------------------------------------------------
 * The WalshStep datatype is used to select one step of a 16-step Walsh 
 * functions.
 *
 * It is stored in a UintVariable and supports the standard arithmetic
 * != == <= >= < > operators.
 */
DataType *add_WalshStepDataType(Script *sc, char *name);


/*-----------------------------------------------------------------------
 * The WalshFunction datatype is used to select one of 32 walsh functions.
 *
 * It is stored in a UintVariable and supports the standard arithmetic
 * != == <= >= < > operators.
 */
DataType *add_WalshFunctionDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The SysType datatype is used to tell the shutdown and reboot commands
 * which sub-system to reset.
 *
 * It is stored in a ChoiceVariable and supports the != == operators.
 */
DataType *add_SysTypeDataType(Script *sc, char *name);
/*
 * Enumerate the subsystems addressed by the SysType datatype.
 */
typedef enum {
  SYS_CPU,        /* Reboot or shutdown the CPU */
  SYS_RTC,        /* Reboot or shutdown the real-time-control system */
  SYS_SZACONTROL, /* Restart or shutdown the control program */
  SYS_PMAC        /* Restart the pmac */
} SysType;

/*-----------------------------------------------------------------------
 * The TimeScale datatype is used with Time variables to specify which
 * time system they refer to.
 *
 * It is stored in a ChoiceVariable and supports the != == operators.
 */
DataType *add_TimeScaleDataType(Script *sc, char *name);
/*
 * Enumerate the supported time systems.
 */
typedef enum {
  TIME_UTC,       /* Universal Coordinated Time */
  TIME_LST,       /* Local Sidereal Time */
} TimeScale;

/*-----------------------------------------------------------------------
 * The SwitchState datatype is used to specify whether to turn a switch
 * on or off.
 *
 * It is stored in a ChoiceVariable and supports the != == operators.
 */
DataType *add_SwitchStateDataType(Script *sc, char *name);
/*
 * Enumerate the supported switch states.
 */
typedef enum {SWITCH_ON, SWITCH_OFF} SwitchState;

/*-----------------------------------------------------------------------
 * The Heaters datatype is used to specify a sub-set of receiver heaters
 * to be controlled.
 *
 * It is stored in a SetVariable and supports the standard
 * set + - !~ ~ != == operators.
 */
DataType *add_HeatersDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The HeaterVoltage datatype is used to specify the target sensor voltage
 * in the feedback loop of a receiver heater.
 *
 * It is stored in a DoubleVariable with -ve values used to specify
 * a heater to be switched off. It supports the standard arithmentic
 * != == <= >= < > operators.
 */
DataType *add_HeaterVoltageDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The ThermalTargets datatype is used to select a subset of receiver
 * local-oscillator stages.
 *
 * It is stored in a SetVariable and supports the standard
 * set + - !~ ~ != == operators.
 */
DataType *add_ThermalTargetDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The ThermalMode datatype is used to select a subset of receiver
 * local-oscillator stages.
 *
 * It is stored in a ChoiceVariable and supports the != == operators.
 */
DataType *add_ThermalModeDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The LoStages datatype is used to select a subset of receiver
 * local-oscillator stages.
 *
 * It is stored in a SetVariable and supports the standard
 * set + - !~ ~ != == operators.
 */
DataType *add_LoStagesDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The AcquireTargets datatype is used to tell the until() command which
 * operations to wait for.
 *
 * It is stored in a SetVariable and supports the standard
 * set + - !~ ~ != == operators.
 */
DataType *add_DelayTargetDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The AcquireTargets datatype is used to tell the until() command which
 * operations to wait for.
 *
 * It is stored in a SetVariable and supports the standard
 * set + - !~ ~ != == operators.
 */
DataType *add_AcquireTargetsDataType(Script *sc, char *name);
/*
 * Enumerate bit-set values of the available targets. Each should be
 * a power of two.
 */
typedef enum {
  ACQ_PHASE       = 0x1,  // The last phase-shifter move command 
  ACQ_SOURCE      = 0x2,  // The last tracker command 
  ACQ_CHANNELIZER = 0x4,  // The last long-duration channelizer command 
  ACQ_MARK        = 0x8,  // The last mark-command 
  ACQ_GRAB        = 0x10, // The last grabber command 
  ACQ_SETREG      = 0x20, // The last grabber command 
  ACQ_TV_OFFSET   = 0x40, // The last tv_offset command 
  ACQ_CALTERT     = 0x80, // The last caltert command 
  ACQ_IFMOD       = 0x100,// The last antennaif command 
  ACQ_CAN         = 0x200,// The last CAN command 
  ACQ_FRAME       = 0x400,// The last newFrame command 
  ACQ_NOISE       = 0x800 // The last noise source command 
} AcquireTargets;

/*-----------------------------------------------------------------------
 * The PhaseStep datatype is used to specify whether to allow a
 * stepper-motor to be positioned on a half-step boundary, or whether
 * it must always stop on a full step.
 *
 * It is stored in a ChoiceVariable and supports the != == operators.
 */
DataType *add_PhaseStepDataType(Script *sc, char *name);
/*
 * Enumerate the phase-shifter stepper-motor increment options.
 */
typedef enum {STEP_HALF, STEP_FULL} PhaseStep;
/*-----------------------------------------------------------------------
 * The PolarState datatype is used to specify which phase state to set
 *
 * It is stored in a ChoiceVariable and supports the != == operators.
 */
DataType *add_PolarStateDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The PhaseShift datatype is used to specify the target encoder position
 * of a phase-shifter motor over the range 0..1023. A value of 1024 is
 * used to turn the motor off.
 *
 * It is stored in a UintVariable supports the standard arithmetic
 * != == <= >= < > operators.
 */
DataType *add_PhaseShiftDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The QuadPhase datatype is used to select between the 4 possible states
 * of a quadrature phase shift network. The 4 allowed phases are
 * 0,90,180 and 270 degrees.
 *
 * It is stored in a UintVariable and supports the standard arithmetic
 * != == <= >= < > operators.
 */
DataType *add_QuadPhaseDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The Scan datatype is used for specification of a scan by its name
 * in the scan catalog.
 *
 * It is stored in a ScanVariable and supports the != == operators.
 */
DataType *add_ScanDataType(Script *sc, char *name);

typedef struct {
  Variable v;              /* Generic members of the variable (see script.h) */
  char name[sza::array::SCAN_NAME_MAX]; /* The name of the scan */
} ScanVariable;

#define SCAN_VARIABLE(v)  ((ScanVariable *)(v))

/*-----------------------------------------------------------------------
 * The Source datatype is used for specification of a source by its name
 * in the source catalog.
 *
 * It is stored in a SourceVariable and supports the != == operators.
 */
DataType *add_SourceDataType(Script *sc, char *name);

typedef struct {
  Variable v;              /* Generic members of the variable (see script.h) */
  char name[sza::array::SRC_NAME_MAX]; /* The name of the source */
} SourceVariable;

#define SOURCE_VARIABLE(v)  ((SourceVariable *)(v))

/*-----------------------------------------------------------------------
 * The TransDev datatype is used for specification of a device by its name
 * in the transaction catalog.
 *
 * It is stored in a TransDevVariable and supports the != == operators.
 */
DataType *add_TransDevDataType(Script *sc, char *name);

typedef struct {
  Variable v;              // Generic members of the variable (see script.h) 
  // The name of the device
  char name[sza::array::TransactionManager::DEV_NAME_MAX+1]; 
} TransDevVariable;

#define TRANSDEV_VARIABLE(v)  ((TransDevVariable *)(v))

/*-----------------------------------------------------------------------
 * The TransLocation datatype is used for specification of a location
 * by its name in the transaction catalog.
 *
 * It is stored in a TransLocationVariable
 */
DataType *add_TransLocationDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The TransSerial datatype is used for specification of a serial
 * by its name in the transaction catalog.
 *
 * It is stored in a TransSerialVariable
 */
DataType *add_TransSerialDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The TransWho datatype is used for specification of the person
 * logging the transaction
 *
 * It is stored in a TransWhoVariable
 */
DataType *add_TransWhoDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The TransComment datatype is used for specification of the person
 * logging the transaction
 *
 * It is stored in a TransCommentVariable
 */
DataType *add_TransCommentDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The Model datatype is used to select between the optical and radio
 * pointing models.
 *
 * It is stored in a ChoiceVariable and supports the != == operators.
 */
DataType *add_ModelDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The Latitude datatype is used to specify the latitude of a location
 * on the surface of the Earth (-90..90 degrees).
 *
 * It is stored in degrees as a DoubleVariable and supports the
 * standard arithmetic != == <= >= < > operators.
 */
DataType *add_LatitudeDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The Longitude datatype is used to specify the longitude of a location
 * on the surface of the Earth (-180..180 degrees).
 *
 * It is stored in degrees as a DoubleVariable and supports the
 * standard arithmetic != == <= >= < > operators.
 */
DataType *add_LongitudeDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The Azimuth datatype is used to specify a target azimuth for the
 * telescope. The azimuths of the compass points are N=0, E=90, S=180
 * and W=270 degrees (-360..360 degrees).
 *
 * It is stored in degrees as a DoubleVariable and supports the
 * standard arithmetic != == <= >= < > operators.
 */
DataType *add_AzimuthDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The DeckAngle datatype is used to specify the position of the rotating
 * platform on which the dishes are mounted (-360..360 degrees).
 *
 * It is stored in degrees as a DoubleVariable and supports the
 * standard arithmetic != == <= >= < > operators.
 */
DataType *add_DeckAngleDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The Elevation datatype is used to specify the target elevation angle
 * of the telescope (-90..90 degrees).
 *
 * It is stored in degrees as a DoubleVariable and supports the
 * standard arithmetic != == <= >= < > operators.
 */
DataType *add_ElevationDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The PointingOffset datatype is used to specify a temporary offset of
 * one of the telescope axes from the current pointing (-180..180).
 *
 * It is stored in degrees as a DoubleVariable and supports the
 * standard arithmetic != == <= >= < > operators.
 */
DataType *add_PointingOffsetDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The Flexure datatype is used to specify the degree of gravitational
 * drooping of the telescope as a function of elevation (-90..90 degrees).
 *
 * It is stored in degrees per cosine of elevation as a DoubleVariable
 * and supports the standard arithmetic != == <= >= < > operators.
 */
DataType *add_FlexureDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The LoFrequency datatype is used to specify the target frequency for
 * a tunable oscillator (MHz)
 *
 * It is stored in degrees as a DoubleVariable and supports the
 * standard arithmetic != == <= >= < > operators.  */
DataType *add_LoFrequencyDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The Tilt datatype is used to specify the misalignment tilt of a
 * telescope axis (-90..90 degrees).
 *
 * It is stored in degrees as a DoubleVariable and supports the
 * standard arithmetic != == <= >= < > operators.
 */
DataType *add_TiltDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The Tracking data type is used for specifying the type of tracking,
 * phase tracking or pointing tracking
 */
DataType *add_TrackingDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The Altitude datatype is used to specify the height of the telescope
 * above the standard geodetic spheroid (-10000..10000 meters).
 *
 * It is stored in meters as a DoubleVariable and supports the
 * standard arithmetic != == <= >= < > operators.
 */
DataType *add_AltitudeDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The GpibDev datatype is used to select a device on the GPIB bus (0..30).
 *
 * It is stored in a UintVariable and supports the standard arithmetic
 * != == <= >= < > operators.
 */
DataType *add_GpibDevDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The GpibCmd datatype is used to specify a command string to be sent
 * to a device on the GPIB bus.
 *
 * It is stored as a string of at most rtcnetcoms.h::GPIB_MAX_DATA
 * characters in a StringVariable and supports the != == !~ ~ operators.
 */
DataType *add_GpibCmdDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The Features datatype is used to specify one or more features to
 * be highlighted in a subsequent archive frame.
 */
DataType *add_FeaturesDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The DeckMode datatype is used to tell the tracker task how to
 * position the deck axis while tracking sources.
 *
 * It is stored in a ChoiceVariable and supports the != == operators.
 */
DataType *add_DeckModeDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The Attenuation datatype is used for configuring channelizer
 * attenuators to insert attenuations in steps of 1db over the range
 * 0..31db.
 *
 * It is stored in a UintVariable and supports != == <= >= < >.
 */
DataType *add_AttenuationDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The CanModule datatype is used to select a sub-set of recognized CAN
 * modules.
 *
 * It is stored in a SetVariable and supports the standard
 * set + - !~ ~ != == operators.
 */ 
DataType *add_CanModulesDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The Bands datatype is used to select a sub-set of the 16 SZA
 * frequency bands.
 *
 * It is stored in a SetVariable and supports the standard
 * set + - !~ ~ != == operators.
 */ 
DataType *add_BandsDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The LoOsc datatype is used to select a sub-set of the possible LO
 * frequency bands.
 *
 * It is stored in a SetVariable and supports the standard
 * set + - !~ ~ != == operators.
 */ 
DataType *add_LoOscDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The TotalPower datatype is used to specify the target sensor voltage
 * of a set of total power detectors.
 *
 * It is stored in a DoubleVariable with -ve values used to specify
 * a heater to be switched off. It supports the standard arithmentic
 * != == <= >= < > operators.
 */
DataType *add_TotalPowerDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The ArcType datatype is used to tell the close, flush and open
 * commands which archiving file to operate on.
 *
 * It is stored in a ChoiceVariable and supports the != == operators.
 * The enumerators that become stored in ArcType values are taken from
 * arcfile.h::ArcFileType.
 */
DataType *add_ArcFileDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The FeatureChange datatype is used to tell the mark command what to
 * do with the set of features that it is given.
 *
 * It is stored in a ChoiceVariable and supports the != == operators.
 * The enumerators that become stored in FeatureChange values are taken from
 * rtcnetcoms.h::FeatureMode.
 */
DataType *add_FeatureChangeDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The PowerMeterCommand datatype tells the power_meter command
 * what type of command to send to the noise-cal power meter.
 *
 * It is stored in a ChoiceVariable and supports the != == operators.
 * The enumerators that become stored in PowerMeterCommand values are
 * taken from rtcnetcoms.h::NcalPowerCmd.
 */
DataType *add_PowerMeterCommandDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The BitMask datatype is used to set bit-mask as a set of 32 bits.
 *
 * It is stored in a SetVariable and supports the standard
 * set + - !~ ~ != == operators.
 */ 
DataType *add_BitMaskDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The BitMaskOper datatype is used to specify whether a given bit-mask
 * should be used to set bits, clear bits or to be used verbatim.
 *
 * It is stored in a ChoiceVariable using the enumerators defined in
 * rtcnetcoms.h, and supports the standard set != and == operators.
 */
DataType *add_BitMaskOperDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The DioBoard datatype is used to specify a digital I/O board.
 *
 * It is stored in a UintVariable and supports the != == operator.
 */
DataType *add_DioBoardDataType(Script *sc, char *name, ArrayMap *arraymap);

/*-----------------------------------------------------------------------
 * The Script datatype is used for entering the name and arguments
 * of a script.
 */
DataType *add_ScriptDataType(Script *sc, char *name);

typedef struct {
  Variable v;       /* The base-class members of the variable (see script.h) */
  Script *sc;       /* The compiled script */
} ScriptVariable;

#define SCRIPT_VARIABLE(v)  ((ScriptVariable *)(v))

/*-----------------------------------------------------------------------
 * The DsCommand datatype is used to specify the target of a thermometer
 * command.
 */
DataType *add_DsCommandDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The OptCamTarget datatype is used to specify the target of an optical 
 * camera command.
 */
DataType *add_OptCamTargetDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The OptCamAction datatype is used to specify the action of an optical 
 * camera command.
 */
DataType *add_OptCamActionDataType(Script *sc, char *name);

/*.......................................................................
 * The OptCamCount datatype is used for specifying optical camera integer 
 * stepper motor steps.
 *
 * It is stored in an IntVariable with +10000 used to specify
 * a device to be switched on, and -10000 used to specify a device to be 
 * switched off. It supports the standard arithmentic
 * != == <= >= < > operators.
 */
DataType *add_OptCamCountDataType(Script *sc, char *name);

/*.......................................................................
 * The FgReg datatype is used for specifying which register of the frame
 * grabber board to write to.
 */
DataType *add_FgRegDataType(Script *sc, char *name);

/*.......................................................................
 * The Peak datatype is used for specifying which peak offset from the
 * frame grabber to return.
 */
DataType *add_PeakDataType(Script *sc, char *name);

/*.......................................................................
 * The Imstat datatype is used for specifying which frame grabber image 
 * statistic to return
 */
DataType *add_ImstatDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The following datatype is used to set slew rates, in terms of a
 * percentage of the maximum speed available.
 *
 * It is stored in a UintVariable and supports != == <= >= < >.
 */
DataType *add_SlewRateDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The WalshState datatype is used to specify whether to turn slow walshing
 * on or off.
 *
 * It is stored in a ChoiceVariable and supports the != == operators.
 */
DataType *add_WalshStateDataType(Script *sc, char *name);
/*
 * Enumerate the supported walsh states.
 */
typedef enum {WALSH_ON, WALSH_OFF} WalshState;
/*
 * Enumerate special phase-shifter encoder positions
 */
enum {
  ENC_RIGHT_REQ=2048,
  ENC_LEFT_REQ=4096,
};

typedef enum {EMAIL_ADD, EMAIL_CLEAR, EMAIL_LIST} EmailAction;

/*-----------------------------------------------------------------------
 * The EmailAction datatype is used to specify whether to enable,
 * disable, or turn the pager on
 *
 * It is stored in a ChoiceVariable using enumerators defined in
 * rtcnetcoms.h, and supports the != == operators.  
*/
DataType *add_EmailActionDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The PagerState datatype is used to specify whether to enable,
 * disable, or turn the pager on
 *
 * It is stored in a ChoiceVariable using enumerators defined in
 * rtcnetcoms.h, and supports the != == operators.  
*/
DataType *add_PagerStateDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The PagerDev datatype is used to specify a pager device
 *
 * It is stored in a ChoiceVariable using enumerators defined in
 * rtcnetcoms.h, and supports the != == operators.  
*/
DataType *add_PagerDevDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The DcPower datatype is used to specify the target power level for
 * the downconverter.  A value < 0 is used to denote a preset level.
 *
 * It is stored in a Double variabel and supports the standard
 * arithmetic != == <= >= < > operators.
 */
DataType *add_DcPowerDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The DelayType datatype is used to specify the type of delay to
 * configure
 */
DataType *add_DelayTypeDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The DDSState datatype is used to enable/disable output from the DDS
 * channels, or to reset the lobe rotator
 */
DataType *add_DDSStateDataType(Script *sc, char *name);

/*.......................................................................
 * Create a new datatype for specifying a calibator position
 */
DataType *add_CalPosDataType(Script *sc, char *name);

/**.......................................................................
 * Create a new datatype for specifying stepper tertiary positions
 */
DataType *add_TertPosDataType(Script *sc, char *name);

/**.......................................................................
 * Create a new datatype for specifying caltert one-wire devices
 */
DataType *add_CalTertOWDeviceDataType(Script *sc, char *name);

/**.......................................................................
 * Create a new datatype for specifying caltert one-wire commands
 */
DataType *add_CalTertOWCommandDataType(Script *sc, char *name);

/**.......................................................................
 * The IFAttenuation datatype is used to specify the target antenna IF
 * attenuation 
 */
DataType *add_IFAttenuationDataType(Script *sc, char *name);

/**.......................................................................
 * The IFLevel datatype is used to specify the target antenna IF
 * attenuation 
 */
DataType *add_IFLevelDataType(Script *sc, char *name);

/*-----------------------------------------------------------------------
 * The GunnDevice datatype is used to specify the device to control
 */
DataType *add_GunnDeviceDataType(Script *sc, char *name);

//-----------------------------------------------------------------------
// Data types to do with Array configurations
//-----------------------------------------------------------------------

DataType* add_ArrayNameDataType(Script *sc, char *name);
DataType* add_ArrayConfigDataType(Script *sc, char *name);
DataType* add_AntennaTypeDataType(Script *sc, char *name);

#endif


