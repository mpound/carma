#include "carma/antenna/sza/antenna/control/AntennaMasterMsg.h"

#include "carma/szautil/Debug.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::util;

/**.......................................................................
 * Write the contents of a TrackerMsg to an ostream
 */
std::ostream& sza::antenna::control::operator<<(std::ostream& os, TrackerMsg* msg)
{
  switch (msg->type) {

    /**------------------------------------------------------------
     * Reboot the Pmac
     */
  case TrackerMsg::REBOOT_PMAC:
    {
      COUT(std::endl << "Got a REBOOTPMAC command: " << std::endl 
	   << "  seq = " << msg->body.rebootPmac.seq);
    }
    break;

	  
    /**------------------------------------------------------------
     * Slew the telescope to a demanded position
     */
  case TrackerMsg::SLEW:
    {
      COUT(std::endl << "Got a SLEW command: " << std::endl 
	   << "  seq = " << msg->body.slew.seq << std::endl
	   << "  axes = " << msg->body.slew.axes << std::endl
	   << "  source = " << msg->body.slew.source << std::endl
	   << "  az = " << msg->body.slew.az << std::endl
	   << "  el = " << msg->body.slew.el << std::endl
	   << "  pa = " << msg->body.slew.pa);
    }
    break;

	  
    /**------------------------------------------------------------
     * Halt the telescope immediately
     */
  case TrackerMsg::HALT:
    {
      COUT(std::endl << "Got a HALT command: " << std::endl 
	   << "  seq = " << msg->body.halt.seq);
    }
    break;

	  
    /**------------------------------------------------------------
     * A 1-pps tick
     */
  case TrackerMsg::TICK:
    {
      COUT(std::endl << "Got a TICK command: " << std::endl
	   << "  tick = " << msg->body.tick);
    }
    break;

    /**------------------------------------------------------------
     * Extend the track of a source
     */
  case TrackerMsg::TRACK:
    {

      COUT(std::endl << "Got a TRACK command: " << std::endl 
	   << "  seq = " << msg->body.track.seq << std::endl
	   << "  source = " << msg->body.track.source << std::endl
	   << "  mjd = " << msg->body.track.mjd << std::endl  	   	   
	   << "  ra = " << msg->body.track.ra << std::endl   
	   
	   << "  dec = " << msg->body.track.dec << std::endl  
	   
	   << "  dist = " << msg->body.track.dist);
    }
    break;

	  
    /**------------------------------------------------------------
     * Adjust the tracking offsets
     */
  case TrackerMsg::OFFSET:
    {

      COUT(std::endl << "Got a OFFSET command: " << std::endl 
	   << "  seq = " << msg->body.offset.seq << std::endl 
	   << "  offset = " << &msg->body.offset.offset
	   );
    }
    break;

	  
    /**------------------------------------------------------------
     * Set the PA angle at which the vertical direction on the TV
     * monitor of the optical telescope matches the direction of
     * increasing topocentric elevation
     */
  case TrackerMsg::TV_ANGLE:
    {

      COUT(std::endl << "Got a TVANGLE command: " << std::endl 
	   << "  angle = " << msg->body.tvAngle.angle);
    }
    break;

	  
    /**------------------------------------------------------------
     * Update the refraction correction
     *
     */
  case TrackerMsg::REFRACTION:
    {

      COUT(std::endl << "Got a REFRACTION command: " << std::endl 
	   << "  mode = " << msg->body.refraction.mode << std::endl 
	   
	   << "  a = " << msg->body.refraction.a << std::endl 
	   << "  b = " << msg->body.refraction.b);
    }
    break;

	  
    /**------------------------------------------------------------
     * Commands used to send occasional updates of variable earth
     * orientation parameters.
     *
     * For each command the control system retains a table of
     * the 3 most recently received updates. These three values
     * are quadratically interpolated to yield orientation
     * parameters for the current time.  On connection to the
     * control system, the control program is expected to send
     * values for the start of the current day, the start of the
     * following day and the start of the day after
     * that. Thereafter, at the start of each new day, it should
     * send parameters for a time two days in the future.
     *
     * On startup of the control system, requests for ut1utc and
     * eqeqx will return zero. On receipt of the first
     * earth-orientation command, requests for orientation
     * parameters will return the received values.  On the
     * receipt of the second, requesters will receive a linear
     * interpolation of the parameters. On receipt of the third
     * and subsequent commands, requesters will receive
     * quadratically interpolated values using the parameters of
     * the three most recently received commands.
     */
    /**------------------------------------------------------------
     * The UT1-UTC correction
     */
  case TrackerMsg::EXTEND_UT1UTC:
    {

      COUT(std::endl << "Got a EXTENDUT1UTC command: " << std::endl 
	   << "  mjd = " << msg->body.extendUt1Utc.mjd << std::endl     
	   
	   << "  ut1utc = " << msg->body.extendUt1Utc.ut1utc);
    }
    break;

	  
    /**------------------------------------------------------------
     * The equation of the equinoxes.
     */
  case TrackerMsg::EXTEND_EQNEQX:
    {

      COUT(std::endl << "Got a EXTENDEQNEQX command: " << std::endl 
	   << "  mjd = " << msg->body.extendEqnEqx.mjd << std::endl     
	   
	   
	   << "  eqneqx = " << msg->body.extendEqnEqx.eqneqx);
    }
    break;

	  
    /**------------------------------------------------------------
     * The slew_rate is used to set the slew speeds of each of
     * the telescope axes. The speed is specified as a
     * percentage of the maximum speed available.
     */
  case TrackerMsg::SLEWRATE:
    {

      COUT(std::endl << "Got a SLEWRATE command: " << std::endl 
	   << "  seq = " << msg->body.slewRate.seq << std::endl 
	   << "  axes = " << msg->body.slewRate.axes << std::endl   
	   
	   
	   
	   
	   
	   << "  az = " << msg->body.slewRate.az << std::endl           
	   << "  el = " << msg->body.slewRate.el << std::endl           
	   << "  dk = " << msg->body.slewRate.dk);
    }
    break;

	  
    /**------------------------------------------------------------
     * Calibrate the axis tilts of the telescope.
     */
  case TrackerMsg::TILTS:
    {

      COUT(std::endl << "Got a TILTS command: " << std::endl 
	   << "  seq = " << msg->body.tilts.seq << std::endl  
	   << "  ha = " << msg->body.tilts.ha << std::endl     
	   
	   << "  lat = " << msg->body.tilts.lat << std::endl    
	   
	   << "  el = " << msg->body.tilts.el);
    }
    break;

	  
    /**------------------------------------------------------------
     * Set the gravitational flexure of the telescope.
     */
  case TrackerMsg::FLEXURE:
    {

      COUT(std::endl << "Got a FLEXURE command: " << std::endl 
	   << "  seq = " << msg->body.flexure.seq << std::endl  
	   
	   << "  mode = " << msg->body.flexure.mode << std::endl 
	   
	   

	   << "  sFlexure = " << msg->body.flexure.sFlexure << std::endl    
	   
	   << "  cFlexure = " << msg->body.flexure.cFlexure);
    }
    break;

	  
    /**------------------------------------------------------------
     * Calibrate the collimation of the optical or radio axes.
     */
  case TrackerMsg::COLLIMATION:
    {

      COUT(std::endl << "Got a COLLIMATION command: " << std::endl 
	   << "  seq = " << msg->body.collimation.seq << std::endl 
	   
	   << "  mode = " << msg->body.collimation.mode << std::endl 
	   
	   << "  x = " << msg->body.collimation.x << std::endl        
	   
	   << "  y = " << msg->body.collimation.y << std::endl        
	   
	   << "  addMode = " << msg->body.collimation.addMode);
    }
    break;

	  
    /**------------------------------------------------------------
     * Set the calibation factors of the telescope encoders. 
     */
  case TrackerMsg::ENCODER_CALS:
    {

      COUT(std::endl << "Got a ENCODERCOUNTSPERTURN command: " << std::endl 
	   << "  seq = " << msg->body.encoderCountsPerTurn.seq << std::endl 
	   << "  az = " << msg->body.encoderCountsPerTurn.az << std::endl
	   << "  el = " << msg->body.encoderCountsPerTurn.el << std::endl
	   << "  dk = " << msg->body.encoderCountsPerTurn.dk);
    }
    break;


    /**------------------------------------------------------------
     * Tell the drive task what the limits on encoder values
     * are.
     */
  case TrackerMsg::ENCODER_LIMITS:
    {

      COUT(std::endl << "Got a ENCODERLIMITS command: " << std::endl 
	   << "  seq = " << msg->body.encoderLimits.seq << std::endl 
	   << "  az_min = " << msg->body.encoderLimits.az_min << std::endl      
	   << "  az_max = " << msg->body.encoderLimits.az_max << std::endl      
	   << "  el_min = " << msg->body.encoderLimits.el_min << std::endl      
	   << "  el_max = " << msg->body.encoderLimits.el_max << std::endl      
	   << "  pa_min = " << msg->body.encoderLimits.pa_min << std::endl      
	   << "  pa_max = " << msg->body.encoderLimits.pa_max);
    }
    break;

	  
    /**------------------------------------------------------------
     * Set the zero points of the telescope encoders. The angles
     * are measured relative to the position at which the
     * encoders show zero counts.
     */
  case TrackerMsg::ENCODER_ZEROS:
    {

      COUT(std::endl << "Got a ENCODERZEROS command: " << std::endl 
	   << "  seq = " << msg->body.encoderZeros.seq << std::endl 
	   << "  az = " << msg->body.encoderZeros.az << std::endl  
	   
	   
	   << "  el = " << msg->body.encoderZeros.el << std::endl  
	   
	   
	   << "  dk = " << msg->body.encoderZeros.dk);
    }
    break;

	  
    /**------------------------------------------------------------
     * Select between the optical and radio pointing models.
     */
  case TrackerMsg::SELECT_MODEL:
    {

      COUT(std::endl << "Got a SELECTMODEL command: " << std::endl 
	   << "  seq = " << msg->body.selectModel.seq << std::endl  
	   
	   << "  mode = " << msg->body.selectModel.mode);
    }
    break;

	  
    /**------------------------------------------------------------
     * Tell the control system what the current year is. This is
     * necessary because the gps time-code reader doesn't supply
     * year information.
     */
  case TrackerMsg::YEAR:
    {

      COUT(std::endl << "Got a YEAR command: " << std::endl 
	   << "  year = " << msg->body.year.year);
    }
    break;

	  
    /**------------------------------------------------------------
     * The following command is used to inform the control system of the
     * site of this antenna
     */
  case TrackerMsg::SITE:
    {

      COUT(std::endl << "Got a SITE command: " << std::endl 
	   << "  lon = " << msg->body.site.lon << std::endl  
	   << "  lat = " << msg->body.site.lat << std::endl  
	   << "  alt = " << msg->body.site.alt);
    }
    break;

	  
    /**------------------------------------------------------------
     * The following command is used to inform the control system of the
     * site of this antenna
     */
  case TrackerMsg::LOCATION:
    {

      COUT(std::endl << "Got a LOCATION command: " << std::endl 
	   << "  north = " << msg->body.location.north << std::endl 
	   << "  east = " << msg->body.location.east << std::endl  
	   << "  up = " << msg->body.location.up);
    }
    break;


    /**------------------------------------------------------------
     * Flag a board.
     */
  case TrackerMsg::FLAG_BOARD:
    {

      COUT(std::endl << "Got a FLAGBOARD command: " << std::endl 
	   << "  board = " << msg->body.flagBoard.board << std::endl 
	   
	   << "  flag = " << msg->body.flagBoard.flag);
    }
    break;

	  
  case TrackerMsg::WEATHER:
    {

      COUT(std::endl << "Got a WEATHER command: " << std::endl 
	   << "  airTemperatureInK = " << msg->body.weather.airTemperatureInK << std::endl
	   << "  relativeHumidity = " << msg->body.weather.relativeHumidity << std::endl
	   << "  pressureInMbar = " << msg->body.weather.pressureInMbar);
    }
    break;


  case TrackerMsg::RX:
    {

      COUT(std::endl << "Got a RX command: " << std::endl 
	   << "  id = " << msg->body.rx.id);
    }
    break;
    
  default:
    break;
  }

  COUT("carmaSeqNo = " << msg->carmaSeqNo_);

  return os;
}

/**.......................................................................
 * Write the contents of an AntennaRxMsg to an ostream
 */
std::ostream& sza::antenna::control::operator<<(std::ostream& os, AntennaRxMsg* msg)
{
  switch (msg->type) {

    /**------------------------------------------------------------
     * Flag a board.
     */
  case AntennaRxMsg::FLAG_BOARD:
    {

      COUT(std::endl << "Got a FLAGBOARD command: " << std::endl 
	   << "  board = " << msg->body.flagBoard.board << std::endl // The register map index of the
	   // board to un/flag
	   << "  flag = " << msg->body.flagBoard.flag);
    }
    break;

	  
    /**------------------------------------------------------------
     * Control the LO chain
     */
  case AntennaRxMsg::LO:
    {
      COUT(std::endl << "Got a LO command: " << std::endl 
	   << "  msgId = " << msg->body.lo.msgId << std::endl
	   << "  oscs = " << msg->body.lo.oscs << std::endl
	   << "  stages = " << msg->body.lo.stages << std::endl
	   << "  rxId = " << msg->body.lo.rxId << std::endl
	   << "  on = " << msg->body.lo.on << std::endl
	   << "  dampGain = " << msg->body.lo.dampGain << std::endl
	   << "  frequency = " << msg->body.lo.frequency << std::endl
	   << "  loopGain = " << msg->body.lo.loopGain << std::endl
	   << "  voltage = " << msg->body.lo.voltage << std::endl
	   << "  id = " << msg->body.lo.id << std::endl
	   << "  day = " << msg->body.lo.day << std::endl
	   << "  month = " << msg->body.lo.month << std::endl
	   << "  year = " << msg->body.lo.year << std::endl
	   << "  npt = " << msg->body.lo.npt << std::endl
	   << "  coeff = " << msg->body.lo.coeff << std::endl
	   << "  tunerPos = " << msg->body.lo.tunerPos << std::endl
	   << "  backshortPos = " << msg->body.lo.backshortPos << std::endl
	   << "  attenPos = " << msg->body.lo.attenPos << std::endl
	   << "  position = " << msg->body.lo.position);
    }
    break;

	  
    /**------------------------------------------------------------
     * Install offsets needed to bring this receiver on-axis
     */
  case AntennaRxMsg::OFFSET:
    {
      COUT(std::endl << "Got a OFFSET command: " << std::endl 
	   << "  az = " << msg->body.offset.az << std::endl
	   << "  el = " << msg->body.offset.el);
    }
    break;

	  
    /**------------------------------------------------------------
     * Set a input voltage
     */
  case AntennaRxMsg::SET_INPUT_VOLTAGE:
    {
      COUT(std::endl << "Got a INPUTVOLTAGE command: " << std::endl 
	   << "  rx = " << msg->body.inputVoltage.rx << std::endl
	   << "  amp = " << msg->body.inputVoltage.amp << std::endl
	   << "  stage = " << msg->body.inputVoltage.stage << std::endl
	   << "  biasType = " << msg->body.inputVoltage.biasType << std::endl
	   << "  bias = " << msg->body.inputVoltage.bias);
    }
    break;


    /**------------------------------------------------------------
     * Set a drain current
     */
  case AntennaRxMsg::SET_DRAIN_CURRENT:
    {
      COUT(std::endl << "Got a DRAINCURRENT command: " << std::endl 
	   << "  rx = " << msg->body.drainCurrent.rx << std::endl
	   << "  amp = " << msg->body.drainCurrent.amp << std::endl
	   << "  stage = " << msg->body.drainCurrent.stage << std::endl
	   << "  current = " << msg->body.drainCurrent.current);
    }
    break;


    /**------------------------------------------------------------
     * Toggle fast sampling
     */
  case AntennaRxMsg::TOGGLE_FAST_SAMPLING:
    {
      COUT(std::endl << "Got a FASTSAMPLING command: " << std::endl 
	   << "  channel = " << msg->body.fastSampling.channel << std::endl
	   << "  start = " << msg->body.fastSampling.start);
    }
    break;


    /**------------------------------------------------------------
     * Control the Calibrator/Tertiary CAN module
     */
  case AntennaRxMsg::CALTERT:
    {
      COUT(std::endl << "Got a CALTERT command: " << std::endl 
	   << "  msgId = " << msg->body.calTert.msgId << std::endl
	   << "  rxId = " << msg->body.calTert.rxId << std::endl
	   << "  tertPosition = " << msg->body.calTert.tertPosition << std::endl
	   << "  calPosition = " << msg->body.calTert.calPosition << std::endl
	   << "  enable = " << msg->body.calTert.enable << std::endl
	   << "  owDevice = " << msg->body.calTert.owDevice << std::endl
	   << "  owCommand = " << msg->body.calTert.owCommand << std::endl
	   << "  seq = " << msg->body.calTert.seq);
    }
    break;


    /**------------------------------------------------------------
     * Control the Berkeley interface module
     */
  case AntennaRxMsg::INTMOD:
    {
      COUT(std::endl << "Got a INTMOD command: " << std::endl 
	   << "  msgId = " << msg->body.intMod.msgId << std::endl
	   << "  atten = " << msg->body.intMod.atten);
    }
    break;


    /**------------------------------------------------------------
     * Set a bias
     */
  case AntennaRxMsg::SET_BIAS:
    {
      COUT(std::endl << "Got a SETBIAS command: " << std::endl 
	   << "  amp = " << msg->body.setBias.amp << std::endl
	   << "  bias = " << msg->body.setBias.bias << std::endl
	   << "  biasType = " << msg->body.setBias.biasType << std::endl
	   << "  rxId = " << msg->body.setBias.rxId << std::endl
	   << "  seq = " << msg->body.setBias.seq << std::endl
	   << "  isDefault = " << msg->body.setBias.isDefault);
    }
    break;


    /**------------------------------------------------------------
     * Control the Antenna IF module
     */
  case AntennaRxMsg::IFMOD:
    {
      COUT(std::endl << "Got a IFMOD command: " << std::endl 
	   << "  msgId = " << msg->body.IFMod.msgId << std::endl
	   << "  band = " << msg->body.IFMod.band << std::endl
	   << "  level = " << msg->body.IFMod.level << std::endl
	   << "  attenSet = " << msg->body.IFMod.attenSet << std::endl
	   << "  total = " << msg->body.IFMod.total << std::endl
	   << "  input = " << msg->body.IFMod.input << std::endl
	   << "  output = " << msg->body.IFMod.output << std::endl
	   << "  seq = " << msg->body.IFMod.seq << std::endl
	   << "  pos = " << msg->body.IFMod.pos);
    }
    break;


    /**------------------------------------------------------------
     * Control the Thermal Module
     */
  case AntennaRxMsg::THERMAL:
    {
      COUT(std::endl << "Got a THERMAL command: " << std::endl 
	   << "  msgId = " << msg->body.thermal.msgId << std::endl
	   << "  target = " << msg->body.thermal.target << std::endl
	   << "  value = " << msg->body.thermal.value << std::endl
	   << "  mode = " << msg->body.thermal.mode << std::endl
	   << "  eqState = " << msg->body.thermal.eqState);
    }
    break;


    /**------------------------------------------------------------
     * Control the Tiltmeter Module
     */
  case AntennaRxMsg::TILTMETER:
    {
      COUT(std::endl << "Got a TILTMETER command: " << std::endl 
	   << "  msgId = " << msg->body.tiltmeter.msgId << std::endl
	   << "  mode = " << msg->body.tiltmeter.mode << std::endl
	   << "  value = " << msg->body.tiltmeter.value);
    }
    break;


    // CAN module commands
	  
  case AntennaRxMsg::RESET:
    {
      COUT(std::endl << "Got a RESET command: " << std::endl 
	   << "  modules = " << msg->body.reset.modules << std::endl
	   << "  hardwareReset = " << msg->body.reset.hardwareReset);
    }
    break;

	  
    // Select a receiver

  case AntennaRxMsg::SELECT_RX:
    {
      COUT(std::endl << "Got a SELECTRX command: " << std::endl 
	   << "  rxBand = " << msg->body.selectRx.rxBand << std::endl
	   << "  seq = " << msg->body.selectRx.seq);
    }
    break;


  case AntennaRxMsg::EXECUTE_NEXT_CAN_INSTRUCTION:
    {
      COUT(std::endl << "Got a CANCOMMAND command: " << std::endl 
	   << "  type = " << msg->body.canCommand.type);
    }
    break;

  default:
    break;
  }

  COUT("carmaSeqNo = " << msg->carmaSeqNo_);

  return os;
}

