#include "carma/szautil/CalPos.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Directives.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/IFAtten.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/Rx.h"

#include "carma/antenna/sza/antenna/control/AntNetCmdForwarder.h"
#include "carma/antenna/sza/antenna/control/AntennaMaster.h"
#include "carma/antenna/sza/antenna/control/WrapMode.h"

using namespace sza::array;
using namespace sza::util;
using namespace sza::antenna::control;

/**.......................................................................
 * Constructor.
 */
AntNetCmdForwarder::AntNetCmdForwarder(AntennaMaster* parent) 
{
  parent_ = parent;
}

/**.......................................................................
 * Destructor.
 */
AntNetCmdForwarder::~AntNetCmdForwarder() {}

/**.......................................................................
 * Forward a tracker net command
 */
void AntNetCmdForwarder::forwardTrackerNetCmd(sza::util::NetCmd* netCmd)
{
  LogStream errStr;
  AntennaMasterMsg msg;
  TrackerMsg* trackerMsg = msg.getDriveMsg()->getTrackerMsg();
  RtcNetCmd* rtc = &netCmd->rtc_;
  NetCmdId opcode = netCmd->opcode_;

  // Default to no CARMA sequence number for commands forwarded from
  // the SZA control system

  trackerMsg->setCarmaSequenceNumber();

  switch(opcode) {
  case NET_COLLIMATE_CMD:
    
    // Convert from mas to radians
    
    trackerMsg->
      packCollimationMsg(rtc->cmd.collimate.seq, 
			 (PointingMode::Type)rtc->cmd.collimate.mode, 
			 (double)rtc->cmd.collimate.x * mastor, 
			 (double)rtc->cmd.collimate.y * mastor,
			 (OffsetMsg::Mode)rtc->cmd.collimate.addMode);
    break;
  case NET_ENCODER_CALS_CMD:
    trackerMsg->packEncoderCountsPerTurnMsg(rtc->cmd.encoder_cals.seq, 
					    rtc->cmd.encoder_cals.az, 
					    rtc->cmd.encoder_cals.el,
					    rtc->cmd.encoder_cals.dk);
    break;
  case NET_ENCODER_LIMITS_CMD:
    trackerMsg->packEncoderLimitsMsg(rtc->cmd.encoder_limits.seq, 
				     rtc->cmd.encoder_limits.az_min, 
				     rtc->cmd.encoder_limits.az_max, 
				     rtc->cmd.encoder_limits.el_min, 
				     rtc->cmd.encoder_limits.el_max, 
				     rtc->cmd.encoder_limits.pa_min, 
				     rtc->cmd.encoder_limits.pa_max);
    break;
  case NET_ENCODER_ZEROS_CMD:
    
    // The control program will send us zero points in radians.
    
    trackerMsg->packEncoderZerosMsg(rtc->cmd.encoder_zeros.seq, 
				    rtc->cmd.encoder_zeros.az, 
				    rtc->cmd.encoder_zeros.el,
				    rtc->cmd.encoder_zeros.dk);
    break;
  case NET_FLEXURE_CMD:
    
    // Convert from mas to radians
    
    trackerMsg->packFlexureMsg(rtc->cmd.flexure.seq, 
			       (PointingMode::Type)rtc->cmd.flexure.mode, 
			       (double)rtc->cmd.flexure.sFlexure * mastor,
			       (double)rtc->cmd.flexure.cFlexure * mastor);
    break;
  case NET_EQNEQX_CMD:
    {
      DBPRINT(true, Debug::DEBUG7, "Got a EQNEQX command");
      if(Debug::debugging(Debug::DEBUG7))
	Debug::addLevel(Debug::DEBUG2);

      double mjd = rtc->cmd.eqneqx.mjd + 
	(double)rtc->cmd.eqneqx.tt/daysec/1000.0;
      
      // Convert from mas to radians
      
      trackerMsg->packExtendEqnEqxMsg(mjd, rtc->cmd.eqneqx.eqneqx * mastor);
    }
    break;
  case NET_EQUAT_OFFSET_CMD:
    {
      OffsetMsg offset;
      
      // Convert from mas to radians
      
      offset.packEquatOffsets((OffsetMsg::Mode)rtc->cmd.equat_offset.mode,
			      (OffsetMsg::Axis)rtc->cmd.equat_offset.axes,
			      (double)rtc->cmd.equat_offset.ra * mastor,
			      (double)rtc->cmd.equat_offset.dec * mastor);
      
      trackerMsg->packOffsetMsg(rtc->cmd.equat_offset.seq, offset);
    }
    break;
  case NET_HALT_CMD:
    trackerMsg->packHaltMsg(rtc->cmd.halt.seq);
    break;
  case NET_MODEL_CMD:
    trackerMsg->
      packSelectModelMsg(rtc->cmd.model.seq, 
			 (sza::util::PointingMode::Type)rtc->cmd.model.mode);
    break;
  case NET_MOUNT_OFFSET_CMD:
    {
      OffsetMsg offset;
      
      // Convert from mas to radians
      
      offset.packMountOffsets((OffsetMsg::Mode)rtc->cmd.mount_offset.mode,
			      (OffsetMsg::Axis)rtc->cmd.mount_offset.axes,
			      (double)rtc->cmd.mount_offset.az * mastor,
			      (double)rtc->cmd.mount_offset.el * mastor,
			      (double)rtc->cmd.mount_offset.dk * mastor);
      
      trackerMsg->packOffsetMsg(rtc->cmd.mount_offset.seq, offset);
    }
    break;
  case NET_SITE_CMD:
    
    // Convert angles to radians, and altitude to meters
    
    trackerMsg->packSiteMsg((double)rtc->cmd.site.lon * mastor, 
			    (double)rtc->cmd.site.lat * mastor, 
			    (double)rtc->cmd.site.alt / 1000.0);
    break;
  case NET_SLEW_CMD:
    
    // Convert angles to radians
    
    trackerMsg->packSlewMsg(rtc->cmd.slew.seq,
			    rtc->cmd.slew.source,
			    (Axis::Type)rtc->cmd.slew.mask,
			    (double)rtc->cmd.slew.az * mastor,
			    (double)rtc->cmd.slew.el * mastor, 
			    (double)rtc->cmd.slew.dk * mastor);
    break;
  case NET_SLEW_RATE_CMD:
    
    // Convert angles to radians
    
    COUT("Got a slew rate masK: " << rtc->cmd.slew_rate.mask);

    trackerMsg->packSlewRateMsg(rtc->cmd.slew_rate.seq,
				(Axis::Type)rtc->cmd.slew_rate.mask,
				rtc->cmd.slew_rate.az,
				rtc->cmd.slew_rate.el, 
				rtc->cmd.slew_rate.dk);
    break;
  case NET_SKY_OFFSET_CMD:
    {
      OffsetMsg offset;
      
      // Convert from mas to radians
      
      offset.packSkyOffsets((OffsetMsg::Mode)rtc->cmd.sky_offset.mode,
			    (OffsetMsg::Axis)rtc->cmd.sky_offset.axes,
			    (double)rtc->cmd.sky_offset.x * mastor,
			    (double)rtc->cmd.sky_offset.y * mastor);
      
      trackerMsg->packOffsetMsg(rtc->cmd.sky_offset.seq, offset);
    }
    break;
  case NET_TV_OFFSET_CMD:
    {
      OffsetMsg offset;
      DBPRINT(true, Debug::DEBUG3, "Got a tvoffset command");
      // Convert from mas to radians
      
      offset.packTvOffsets((double)rtc->cmd.tv_offset.up    * mastor,
			   (double)rtc->cmd.tv_offset.right * mastor);
      
      trackerMsg->packOffsetMsg(rtc->cmd.tv_offset.seq, offset);
    }
    break;
  case NET_TILTS_CMD:
    
    // Convert from mas to radians
    
    trackerMsg->
      packTiltsMsg(rtc->cmd.tilts.seq, 
		   (double)rtc->cmd.tilts.ha  * mastor, 
		   (double)rtc->cmd.tilts.lat * mastor, 
		   (double)rtc->cmd.tilts.el  * mastor);
    break;
  case NET_TRACK_CMD:
    {
      double mjd = rtc->cmd.track.mjd + 
	(double)rtc->cmd.track.tt/daysec/1000.0;
      
      // Convert to internal units
      
      trackerMsg->packTrackMsg(rtc->cmd.track.seq, rtc->cmd.track.source, 
			       mjd,
			       rtc->cmd.track.ra  * mastor,  
			       rtc->cmd.track.dec * mastor,
			       rtc->cmd.track.dist / 1.0e6,
			       WrapMode::NONE); // micro AU -> AU
    }
    break;
  case NET_TV_ANGLE_CMD:
    
    // Convert from mas to radians
    
    trackerMsg->packTvAngleMsg((double)rtc->cmd.tv_angle.angle * mastor);
    break;
  case NET_UT1UTC_CMD:
    {
      DBPRINT(true, Debug::DEBUG7, "Got a UT1UTC command"
	      << " pthread_id is: " << pthread_self());

      if(Debug::debugging(Debug::DEBUG7)) 
	Debug::addLevel(Debug::DEBUG2);

      double mjd = rtc->cmd.ut1utc.mjd + 
	(double)rtc->cmd.ut1utc.utc/daysec/1000.0;
      
      // Convert from microseconds to seconds.
      
      trackerMsg->packExtendUt1UtcMsg(mjd, 
				      (double)rtc->cmd.ut1utc.ut1utc / 1.0e6);
    }
    break;
  case NET_LOCATION_CMD:
    
    trackerMsg->packLocationMsg(rtc->cmd.location.north,
				rtc->cmd.location.east,
				rtc->cmd.location.up);
    break;
  case NET_ATMOS_CMD:
    trackerMsg->packWeatherMsg(rtc->cmd.atmos.temperature, rtc->cmd.atmos.humidity, 
			       rtc->cmd.atmos.pressure);

    break;
  default:
    errStr.appendMessage(true, "Unrecognized message");
    ErrorDef(err, errStr);
    break;
  }
  
  // And forward the message to the AntennaControl task.
  
  parent_->forwardMasterMsg(&msg);

  if(Debug::debugging(Debug::DEBUG7))
    Debug::remLevel(Debug::DEBUG2);
}

/**.......................................................................
 * Receiver commands
 */
void AntNetCmdForwarder::forwardRxNetCmd(sza::util::NetCmd* netCmd) 
{
  LogStream errStr;
  AntennaMasterMsg msg;
  AntennaRxMsg* rxMsg = msg.getRxMsg();
  RtcNetCmd* rtc = &netCmd->rtc_;
  NetCmdId opcode = netCmd->opcode_;
  
  // Default to no CARMA sequence number for commands forwarded from
  // the SZA control system

  rxMsg->setCarmaSequenceNumber();

  switch(opcode) {
    
  case NET_SET_BIAS_CMD:
    DBPRINT(true, Debug::DEBUG5, "Got a set bias command: " 
	    << rtc->cmd.setBias.amp << ", "
	    << rtc->cmd.setBias.bias);

    rxMsg->packSetBiasMsg(rtc->cmd.setBias.amp, rtc->cmd.setBias.bias,
			  (sza::array::BiasType)rtc->cmd.setBias.biasType, 
			  (sza::util::Rx::Id)rtc->cmd.setBias.rxId,
			  rtc->cmd.setBias.seq, rtc->cmd.setBias.isDefault);
    break;
  case NET_INTMOD_CMD:
    DBPRINT(true, Debug::DEBUG5, "Got an intmod command: " 
	    << (int)rtc->cmd.intmod.atten);
    rxMsg->packIntModMsg((sza::array::IntModMsg)rtc->cmd.intmod.msgId, 
			 rtc->cmd.intmod.atten);
    break;
  case NET_CALTERT_CMD:
    DBPRINT(true, DEBUG_CALTERT, "Got a CalTert message, seq = " 
	    << rtc->cmd.caltert.seq);

    rxMsg->packCalTertMsg((sza::array::CalTertMsg)rtc->cmd.caltert.msgId,
			  (sza::util::Rx::Id)rtc->cmd.caltert.rxId,
			  rtc->cmd.caltert.tertPosition,
			  (sza::util::CalPos::Pos)rtc->cmd.caltert.calPosition,
			  rtc->cmd.caltert.enable,
			  (sza::util::CalTertTypes::OwDevice)rtc->cmd.caltert.owDevice,
			  (sza::util::CalTertTypes::OwCommand)rtc->cmd.caltert.owCommand,
			  rtc->cmd.caltert.seq);
    break;
  case NET_THERMAL_CMD:
    rxMsg->packThermalMsg((sza::array::ThermalMsg) rtc->cmd.thermal.msgId,
			  (sza::util::Thermal::Target) rtc->cmd.thermal.target,
			  rtc->cmd.thermal.value,
			  (sza::util::Thermal::BoxMode) rtc->cmd.thermal.mode,
			  (bool) rtc->cmd.thermal.state);
    break;
  case NET_TILTMETER_CMD:
    rxMsg->packTiltmeterMsg((sza::array::TiltmeterMsg) rtc->cmd.tiltmeter.msgId,
			    (sza::array::TiltmeterMode) rtc->cmd.tiltmeter.mode,
			    rtc->cmd.tiltmeter.value);
    break;
  case NET_IFMOD_CMD:
    DBPRINT(true, Debug::DEBUG7, "Got an antenna IF message"
	    << " " << rtc->cmd.IFMod.total 
	    << ", " << rtc->cmd.IFMod.input
	    << ", " << rtc->cmd.IFMod.output);

    rxMsg->packIFModMsg((sza::array::IFModMsg)rtc->cmd.IFMod.msgId,
			(sza::util::Rx::Id)rtc->cmd.IFMod.band,
			rtc->cmd.IFMod.level,
			(sza::util::IFAtten::Type)rtc->cmd.IFMod.attenSet,
			rtc->cmd.IFMod.total,
			rtc->cmd.IFMod.input,
			rtc->cmd.IFMod.output,
			rtc->cmd.IFMod.seq,
			(sza::util::CalPos::Pos)rtc->cmd.IFMod.pos);

    break;
  case NET_LO_CMD:
    DBPRINT(true, Debug::DEBUG9, "Got a net_LO_cmd");
    rxMsg->packLoMsg((sza::array::LoMsgId)rtc->cmd.lo.msgId, 
		     (sza::util::LoOsc::Osc)rtc->cmd.lo.oscs,
		     (sza::util::LoStage::Stage)rtc->cmd.lo.stages,
		     (sza::util::Rx::Id)rtc->cmd.lo.rxId,
		     rtc->cmd.lo.on,
		     rtc->cmd.lo.dampGain,
		     rtc->cmd.lo.frequency,
		     rtc->cmd.lo.loopGain,
		     rtc->cmd.lo.voltage,
		     rtc->cmd.lo.id,
		     rtc->cmd.lo.day,
		     rtc->cmd.lo.month,
		     rtc->cmd.lo.year,
		     rtc->cmd.lo.npt,
		     rtc->cmd.lo.coeff,
		     rtc->cmd.lo.tunerPos,
		     rtc->cmd.lo.backshortPos,
		     rtc->cmd.lo.attenPos,
		     rtc->cmd.lo.position);
    break;
    
    // CAN module reset command

  case NET_RESET_CMD:    
    rxMsg->packResetMsg(static_cast<sza::util::CanModule::Id>
			(rtc->cmd.reset.modules),
			rtc->cmd.reset.hard);
    break;

  case NET_SELECT_RX_CMD:
    rxMsg->packSelectRxMsg((sza::util::Rx::Id)rtc->cmd.selectRx.band, 
			   rtc->cmd.selectRx.seq);
    break;

  default:
    ThrowError("Unrecognized message");
    break;
  }
  
  // And forward the message to the AntennaControl task.
  
  parent_->forwardMasterMsg(&msg);
};

/**.......................................................................
 * Forward a strip control net command
 */
void AntNetCmdForwarder::forwardUmacControlNetCmd(sza::util::NetCmd* netCmd)
{
  LogStream errStr;
  AntennaMasterMsg msg;
  UmacControlMsg* umacControlMsg = msg.getUmacControlMsg();
  RtcNetCmd* rtc = &netCmd->rtc_;
  NetCmdId opcode = netCmd->opcode_;

  switch(opcode) {
  case NET_POWER_CMD:
    umacControlMsg->packPowerMsg(rtc->cmd.power.breaker, rtc->cmd.power.power);
    break;
  default:
    ThrowError("Unrecognized message");
    break;
  }

  // And forward the message to the AntennaControl task.
  
  parent_->forwardMasterMsg(&msg);
}
