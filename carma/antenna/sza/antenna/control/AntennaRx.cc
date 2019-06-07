#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <iostream>

#include "carma/szautil/Attenuation.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Directives.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LoBand.h"
#include "carma/szautil/Temperature.h"

#include "carma/antenna/sza/antenna/control/AntennaMaster.h"
#include "carma/antenna/sza/antenna/control/AntennaRx.h"

#if DIR_HAVE_CARMA
#include "carma/antenna/sza/antenna/canbus/BiasTunedGunn.h"
#include "carma/antenna/sza/antenna/canbus/CalTertOld.h"
#include "carma/antenna/sza/antenna/canbus/CalTertNew.h"
#include "carma/antenna/sza/antenna/canbus/IntMod.h"
#include "carma/antenna/sza/antenna/canbus/Receiver.h"
#include "carma/antenna/sza/antenna/canbus/Thermal.h"
#include "carma/antenna/sza/antenna/canbus/TiltMeter.h"
#include "carma/antenna/sza/antenna/canbus/BiasTunedGunn.h"
#include "carma/antenna/sza/antenna/canbus/VaractorTunedGunn.h"
#include "carma/antenna/sza/antenna/canbus/Yig.h"
#include "carma/antenna/sza/antenna/canbus/IFMod.h"

using namespace sza::antenna::canbus;
#endif

using namespace std;
using namespace sza::antenna::control;
using namespace sza::util;

AntennaRx* AntennaRx::antennaRx_ = 0;

/**.......................................................................
 * Create an AntennaRx class in namespace carma
 */
AntennaRx::AntennaRx(AntennaMaster* parent) :
  SzaTask(), sza::util::GenericTask<AntennaRxMsg>::GenericTask()
{ 
  // Initialize internal pointers
  
  parent_   = 0;
  
#if DIR_HAVE_CARMA
  IFMod_     = 0;
  gunn_      = 0;
  calTert_   = 0;
  intMod_    = 0;
  thermal_   = 0;
  tiltmeter_ = 0;
  varactor_  = 0;
  receiver_  = 0;
  yig_       = 0;
#endif
  
  // Sanity check arguments:
  
  if(parent == 0) {
    LogStream errStr;
    errStr.appendMessage(true, "Received NULL arguments\n");
    throw Error(errStr);
  }
  
  // Keep a pointer to the parent task resources
  
  parent_ = parent;
  share_  = parent->getShare();
  
#if DIR_HAVE_CARMA
  canMaster_ = parent->getCanMaster();
  
  // Add CAN bus devices in which we are interested to the network.

  installCanDevices();  
#endif

  antennaRx_ = this;

  // Register a handler to be called whenever a rx positioning command
  // has completed

#if DIR_HAVE_CARMA
  selectRxCommand_.ignoreErrors(true);
  selectRxCommand_.installCommandDoneHandler(sendSelectRxCommandDoneMsg);
  selectRxCommand_.installCommandFailedHandler(selectRxCommandFailed);
  selectRxCommand_.installExecuteDoneHandler(sendExecuteNextCanInstructionMsg,
					     AntennaRxMsg::SELECT_RX);

  setBiasCommand_.installCommandDoneHandler(sendCanCommandDoneMsg);
  setBiasCommand_.installCommandFailedHandler(canCommandFailed);
  setBiasCommand_.installExecuteDoneHandler(sendExecuteNextCanInstructionMsg,
					    AntennaRxMsg::SET_BIAS);
#endif
};

/**.......................................................................
 * Destructor function
 */
AntennaRx::~AntennaRx() {};

/**.......................................................................
 * Process a message received on the AntennaRx message queue
 */
void AntennaRx::processMsg(AntennaRxMsg* msg)
{
  DBPRINT(true, Debug::DEBUG7, msg);

  // Only register CARMA sequence numbers if this isn't a CAN
  // instruction

  if(msg->type != AntennaRxMsg::EXECUTE_NEXT_CAN_INSTRUCTION)
    registerCarmaRequest(msg);

  switch (msg->type) {
  case AntennaRxMsg::FREQ:
    DBPRINT(false, Debug::DEBUG31, "Got a frequency message");
    break;
  case AntennaRxMsg::OFFSET:
    DBPRINT(false, Debug::DEBUG31, "Got an offset message");
    break;
  case AntennaRxMsg::SELECT_RX:
    rxId_ = msg->body.selectRx.rxBand;
#if DIR_HAVE_CARMA
    selectRxCommand_.run(this, rxId_,
			 msg->body.selectRx.seq, moveTertiary(rxId_));
#endif
    sendTrackerRxMsg(msg->body.selectRx.rxBand);
    break;
  case AntennaRxMsg::EXECUTE_NEXT_CAN_INSTRUCTION:
#if DIR_HAVE_CARMA
    stepCanCommand(msg->body.canCommand.type);
#endif
    break;
  case AntennaRxMsg::IFMOD:
  case AntennaRxMsg::CALTERT:
  case AntennaRxMsg::THERMAL:
  case AntennaRxMsg::TILTMETER:
  case AntennaRxMsg::INTMOD:
  case AntennaRxMsg::SET_BIAS:
  case AntennaRxMsg::LO:
  case AntennaRxMsg::RESET:
  case AntennaRxMsg::SET_DRAIN_CURRENT:
  case AntennaRxMsg::SET_INPUT_VOLTAGE:
  case AntennaRxMsg::TOGGLE_FAST_SAMPLING:
#if DIR_HAVE_CARMA
    processCanMsg(msg);
#endif
    break;
  default:
    ReportError("AntennaRx::processMsg: Unrecognized message type: " << msg->type);
    break;
  };
};

#if DIR_HAVE_CARMA
/**.......................................................................
 * Process a message received on the AntennaRx message queue
 */
void AntennaRx::processCanMsg(AntennaRxMsg* msg)
{
  switch(msg->type) {
  case AntennaRxMsg::THERMAL:
    processThermalMsg(msg);
    break;
  case AntennaRxMsg::TILTMETER:
    processTiltMeterMsg(msg);
    break;
  case AntennaRxMsg::CALTERT:
    processCalTertMsg(msg);
    break;
  case AntennaRxMsg::INTMOD:
    processIntModMsg(msg);
    break;
  case AntennaRxMsg::IFMOD:
    processIFModMsg(msg);
    break;
  case AntennaRxMsg::LO:
    processLoMsg(msg);
    break;
  case AntennaRxMsg::SET_BIAS:
    {
      switch(msg->body.setBias.biasType) {
      case sza::array::AMP:
	receiver_->setAmpBias((sza::antenna::canbus::Receiver::Amp)
			      msg->body.setBias.amp, 
			      msg->body.setBias.bias);
	break;
      case sza::array::RX:

	if(msg->body.setBias.isDefault)
	  setBiasCommand_.setDefaultBias(getAnt()->getId(),
					 (sza::antenna::canbus::Receiver::Amp)msg->body.setBias.amp, 
					 msg->body.setBias.bias);

	else
	  setBiasCommand_.run(this, msg->body.setBias.rxId, 
			      msg->body.setBias.seq);
	break;
      default:
	break;
      }
    }

    break;
  case AntennaRxMsg::RESET:

    if(msg->body.reset.hardwareReset)
      canMaster_->issueHardwareReset();
    else {
      if(msg->body.reset.modules & CanModule::BIAS)
	gunn_->carma::canbus::Device::reset();
      if(msg->body.reset.modules & CanModule::CALTERT)
	calTert_->carma::canbus::Device::reset();
      if(msg->body.reset.modules & CanModule::IFMOD)
	IFMod_->carma::canbus::Device::reset();
      if(msg->body.reset.modules & CanModule::INTMOD)
	intMod_->carma::canbus::Device::reset();
      if(msg->body.reset.modules & CanModule::RECEIVER)
	receiver_->carma::canbus::Device::reset();
      if(msg->body.reset.modules & CanModule::THERMAL)
	thermal_->carma::canbus::Device::reset();
      if(msg->body.reset.modules & CanModule::TILTMETER)
	tiltmeter_->carma::canbus::Device::reset();
      if(msg->body.reset.modules & CanModule::VARACTOR)
	varactor_->carma::canbus::Device::reset();
      if(msg->body.reset.modules & CanModule::YIG)
	yig_->carma::canbus::Device::reset();
    }
    break;
  case AntennaRxMsg::SET_INPUT_VOLTAGE:
    receiver_->setAmpBias(msg->body.inputVoltage.rx,
			  msg->body.inputVoltage.amp,
			  msg->body.inputVoltage.stage,
			  msg->body.inputVoltage.biasType,
			  (short)msg->body.inputVoltage.bias);
    break;
  case AntennaRxMsg::TOGGLE_FAST_SAMPLING:
    receiver_->toggleFastSampling(msg->body.fastSampling.channel,
				  msg->body.fastSampling.start);
  default:
    break;
  }
}

/**.......................................................................
 * Add CAN bus devices to the network.
 */
void AntennaRx::installCanDevices()
{
  if(canMaster_ == 0)
    throw Error("AntennaRx::installCanDevices: "
		" canMaster_ is NULL.\n");
  
  // Allocate new CAN devices here.  These will be deleted in
  // ~SzaTask()
  
  // Node number is unknown for the caltert module -- use a node
  // number of 0 to send to all devices of that API (may not be
  // allowed by the constructor, as node number 0 is reserved)
  
  IFMod_     = new                         IFMod(this, share_, "ifmod",    1, *canMaster_);
  gunn_      = new                 BiasTunedGunn(      share_, "bias",     1, *canMaster_);

  if(parent_->newCaltert()) {
    calTert_ = new                    CalTertNew(this, share_, "caltert",  1, *canMaster_);
  } else {
    calTert_ = new                    CalTertOld(this, share_, "caltert",  1, *canMaster_);  
  }

  intMod_    = new                        IntMod(      share_, "intmod",   1, *canMaster_);
  receiver_  = new                      Receiver(      share_, "rx",       1, *canMaster_);
  thermal_   = new sza::antenna::canbus::Thermal(      share_, "thermal",  1, *canMaster_);
  tiltmeter_ = new sza::antenna::canbus::TiltMeter(    share_, "tiltmeter",1, *canMaster_);
  varactor_  = new             VaractorTunedGunn(      share_, "varactor", 1, *canMaster_);
  yig_       = new                           Yig(      share_, "yig",      1, *canMaster_);
  
  // And add them to the list of known devices.
  
  canDevices_.push_back(IFMod_);
  canDevices_.push_back(gunn_);
  canDevices_.push_back(calTert_);
  canDevices_.push_back(intMod_);
  canDevices_.push_back(receiver_);
  canDevices_.push_back(thermal_);
  canDevices_.push_back(tiltmeter_);
  canDevices_.push_back(varactor_);
  canDevices_.push_back(yig_);
  
  // Add these devices to the network
  
  addCanDevices();
  
  // And enable monitoring.
  
  enableCanMonitoring(true, true, true);

  // Finally, initialize the receiver id to 30 GHz:

  rxId_ = sza::util::Rx::RX30GHZ; 
}

/**.......................................................................
 * Process a Message to control the CalTert module
 */
void AntennaRx::processCalTertMsg(AntennaRxMsg* msg)
{
  // If using the new API, we expect only the following messages

  if(parent_->newCaltert()) {

    switch(msg->body.calTert.msgId) {
    case sza::array::CALTERT_POSITION_CAL:
      COUT("About to issue positon cal command");
      calTert_->positionCalibrator(msg->body.calTert.calPosition, msg->body.calTert.seq, true);
      break;
    case sza::array::CALTERT_HOME_TERT:
      COUT("About to issue home tertiary command");
      calTert_->homeTertiary();
      break;
    case sza::array::CALTERT_NEW_TERT_POS_RX:
      COUT("About to issue positon tert rx");
      calTert_->positionTertiary(msg->body.calTert.rxId, msg->body.calTert.seq, true);
      break;
    case sza::array::CALTERT_NEW_TERT_POS_ANGLE:
      {
	COUT("About to issue positon tert angle (deg) = " << msg->body.calTert.tertPosDegrees);
	sza::util::Angle position;
	position.setDegrees(msg->body.calTert.tertPosDegrees);
	COUT("About to issue positon tert angle (0.01 deg) = " << msg->body.calTert.tertPosDegrees);
	calTert_->positionTertiary(position, msg->body.calTert.seq, true);
      }
      break;
    default:
      ReportError("Received unrecognized calTert message id: " 
		  << msg->body.calTert.msgId);
      break;
    }

    // Else process old-style messages

  } else {

    switch(msg->body.calTert.msgId) {
    case sza::array::CALTERT_POSITION_CAL:
      calTert_->registerRequest(msg->body.calTert.seq);
      calTert_->positionCalibrator(msg->body.calTert.calPosition);
      break;
    case sza::array::CALTERT_HOME_TERT:
      calTert_->registerRequest(msg->body.calTert.seq);
      calTert_->homeTertiary();
      break;
    case sza::array::CALTERT_POSITION_TERT:
      calTert_->registerRequest(msg->body.calTert.seq);
      if(msg->body.calTert.rxId != sza::util::Rx::RXNONE)
	calTert_->positionTertiary(msg->body.calTert.rxId);
      else
	calTert_->positionTertiary(msg->body.calTert.tertPosition);
      break;
    case sza::array::CALTERT_ENABLE_TERT:
      calTert_->enableTertiary(msg->body.calTert.enable);
      break;
    case sza::array::CALTERT_RESET_STEPPER:
      calTert_->resetStepper();
      break;
    case sza::array::CALTERT_INDEX_TERT:
      calTert_->indexCurrentEncoderPosition(msg->body.calTert.rxId);
      break;
    case sza::array::CALTERT_SET_ENCODER:
      calTert_->setEncoderPositionIndex(msg->body.calTert.rxId,
					msg->body.calTert.tertPosition);
      break;
    case sza::array::CALTERT_STORE_ENCODER:
      calTert_->storeEncoderPositionIndex(msg->body.calTert.rxId,
					  msg->body.calTert.tertPosition);
      break;
    case sza::array::CALTERT_ONE_WIRE:
      calTert_->oneWireInterface(msg->body.calTert.owDevice, 
				 msg->body.calTert.owCommand);
      break;
    default:
      ReportError("Received unrecognized calTert message id: " 
		  << msg->body.calTert.msgId);
    }
  }
}
/**.......................................................................
 * Process a Message to control the IntMod module
 */
void AntennaRx::processIntModMsg(AntennaRxMsg* msg)
{
  LogStream errStr;
  switch(msg->body.intMod.msgId) {
  case sza::array::INTMOD_SET_ATTEN:
    COUT("Setting PAM Atten to: " << msg->body.intMod.atten);
    intMod_->setPAMAttenuation(msg->body.intMod.atten);
    break;
  case sza::array::INTMOD_PRESET_POWER:
    COUT("Setting PAM output power to preset");
    intMod_->presetPAMOutputPower();
    break;
  case sza::array::INTMOD_SET_DEFAULT_ATTEN:
    sza::util::LoBand::setLoTermAttenuation(parent_->getAnt()->getId(),
					    Attenuation(Attenuation::dBUnit(), msg->body.intMod.atten));
    break;
  default:
    ThrowError("Unrecognized intmod message id: " 
	       << msg->body.intMod.msgId);
    break;
  }
}

#endif

/**.......................................................................
 * Send a message to the parent that the caltert has completed a
 * transaction
 */
void AntennaRx::sendCalTertDoneMsg(unsigned int seq)
{
  AntennaMasterMsg msg;
  NetMsg* netMsg = msg.getControlMsg()->getNetMsg();
  
  netMsg->setAntId(parent_->getAnt()->getId());
  netMsg->packCalTertDoneMsg(seq);
  
  parent_->forwardMasterMsg(&msg);

  // And write a sequence number to the CARMA monitor system if requested

  sendCarmaSeqNoMsg(true);
}

/**.......................................................................
 * Send a message to the parent that the antennaif has completed a
 * transaction
 */
void AntennaRx::sendIFModDoneMsg(unsigned int seq)
{
  AntennaMasterMsg msg;
  NetMsg* netMsg = msg.getControlMsg()->getNetMsg();
  
  netMsg->setAntId(parent_->getAnt()->getId());
  netMsg->packIFModDoneMsg(seq);
  
  parent_->forwardMasterMsg(&msg);

  // And write a sequence number to the CARMA monitor system if requested

  sendCarmaSeqNoMsg(true);
}

#if DIR_HAVE_CARMA
/**.......................................................................
 * Send a message to the parent to execute the next queued CAN
 * instruction
 */
CAN_EXECUTE_DONE_HANDLER(AntennaRx::sendExecuteNextCanInstructionMsg)
{
  AntennaMasterMsg msg;
  AntennaRxMsg* rxMsg = msg.getRxMsg();

  rxMsg->packExecuteNextCanInstructionMsg(type);
  rxMsg->setCarmaSequenceNumber();
  antennaRx_->parent_->forwardMasterMsg(&msg);
}

/**.......................................................................
 * Send a message to the parent that a generic CANbus command has
 * completed
 */
CAN_COMMAND_DONE_HANDLER(AntennaRx::sendCanCommandDoneMsg)
{
  AntennaMasterMsg msg;
  NetMsg* netMsg = msg.getControlMsg()->getNetMsg();

  netMsg->setAntId(antennaRx_->parent_->getAnt()->getId());
  netMsg->packCanCommandDoneMsg(seq);
  
  antennaRx_->parent_->forwardMasterMsg(&msg);

  ReportMessage("Can command completed");

  // And write a sequence number to the CARMA monitor system if requested

  antennaRx_->sendCarmaSeqNoMsg(true);
}

/**.......................................................................
 * Send a message to the parent that a selectRx command has completed
 */
CAN_COMMAND_DONE_HANDLER(AntennaRx::sendSelectRxCommandDoneMsg)
{
  AntennaMasterMsg msg;
  NetMsg* netMsg = msg.getControlMsg()->getNetMsg();

  netMsg->setAntId(antennaRx_->parent_->getAnt()->getId());
  netMsg->packCanCommandDoneMsg(seq);
  
  antennaRx_->parent_->forwardMasterMsg(&msg);

  ReportMessage("selectRx command completed");

  // And set the rxId to be checked by relevant CAN devices

  antennaRx_->calTert_->setRxId(antennaRx_->rxId_);
  antennaRx_->gunn_->setRxId(antennaRx_->rxId_);
  antennaRx_->varactor_->setRxId(antennaRx_->rxId_);

  // Tell the rx task that tuning is now done

  antennaRx_->setTuningPending(false);

  // And write a sequence number to the CARMA monitor system if
  // requested

  antennaRx_->sendCarmaSeqNoMsg(true);
}

/**.......................................................................
 * Send a message to the parent that a selectRx command has failed
 */
CAN_COMMAND_FAILED_HANDLER(AntennaRx::selectRxCommandFailed)
{
  ReportSimpleError(commandName << " timed out (" << message << ")");

  // And set the rxId to be checked by relevant CAN devices.  Note: we
  // set these even if the tuning fails so that the system will
  // correctly report, for example, that an unlocked BTG represents an
  // error because we're supposed to be tuned to 90 GHz, or that an
  // unlocked varactor represents an error because we're supposed to
  // be tuned to 30 GHz.

  antennaRx_->calTert_->setRxId(antennaRx_->rxId_);
  antennaRx_->gunn_->setRxId(antennaRx_->rxId_);
  antennaRx_->varactor_->setRxId(antennaRx_->rxId_);

  // Tell the rx task that the tuning is now done

  antennaRx_->setTuningPending(false);

  // And write a sequence number to the CARMA monitor system if
  // requested

  antennaRx_->sendCarmaSeqNoMsg(false);
}

/**.......................................................................
 * Send a message to the parent that a CAN command has failed
 */
CAN_COMMAND_FAILED_HANDLER(AntennaRx::canCommandFailed)
{
  ReportSimpleError(commandName << " timed out (" << message << ")");

  // And write a sequence number to the CARMA monitor system if requested

  antennaRx_->sendCarmaSeqNoMsg(false);
}

#endif

#if DIR_HAVE_CARMA
//-----------------------------------------------------------------------
// Antenna IF processing
//-----------------------------------------------------------------------

/**.......................................................................
 * Process a Message to control the IFMod module
 */
void AntennaRx::processIFModMsg(AntennaRxMsg* msg)
{
  switch(msg->body.IFMod.msgId) {
    
    //------------------------------------------------------------
    // Position the IF switch
    //------------------------------------------------------------
    
  case sza::array::IFMOD_POSITION_SWITCH:
    IFMod_->registerRequest(msg->body.IFMod.seq);
    IFMod_->selectBand(msg->body.IFMod.band);
    break;
    
    //------------------------------------------------------------
    // Set the IF level
    //------------------------------------------------------------
    
  case sza::array::IFMOD_SET_LEVEL:
    IFMod_->setLevel(msg->body.IFMod.level);
    break;
    
    //------------------------------------------------------------
    // Set the Default IF attenuation
    //------------------------------------------------------------

  case sza::array::IFMOD_SET_DEFAULT_ATTEN:
    {
      // If the total was specified
    
      
      if(msg->body.IFMod.attenSet & sza::util::IFAtten::ATTEN_TOTAL) {
	
	sza::util::Rx::setIfTotalAtten(Attenuation(Attenuation::dBUnit(), msg->body.IFMod.total),
				       parent_->getAnt()->getId(), 
				       msg->body.IFMod.band,
				       msg->body.IFMod.pos);
	
	// Else if the input was specified
	
      } else if(msg->body.IFMod.attenSet & sza::util::IFAtten::ATTEN_INPUT) {
	
	// If both input and output were specified, set them together
	
	if(msg->body.IFMod.attenSet & sza::util::IFAtten::ATTEN_OUTPUT) {
	  
	  sza::util::Rx::setIfInputAtten(Attenuation(Attenuation::dBUnit(), msg->body.IFMod.input),
					 parent_->getAnt()->getId(), 
					 msg->body.IFMod.band,
					 msg->body.IFMod.pos);
	  
	  sza::util::Rx::setIfOutputAtten(Attenuation(Attenuation::dBUnit(), msg->body.IFMod.output),
					  parent_->getAnt()->getId(), 
					  msg->body.IFMod.band,
					  msg->body.IFMod.pos);
	  
	} else // Else just set the input
	  
	  sza::util::Rx::setIfInputAtten(Attenuation(Attenuation::dBUnit(), msg->body.IFMod.input),
					 parent_->getAnt()->getId(), 
					 msg->body.IFMod.band,
					 msg->body.IFMod.pos);
	
      } else if(msg->body.IFMod.attenSet & sza::util::IFAtten::ATTEN_OUTPUT) {
	
	// If both input and output were specified, set them together
	
	if(msg->body.IFMod.attenSet & sza::util::IFAtten::ATTEN_INPUT) {
	  
	  sza::util::Rx::setIfInputAtten(Attenuation(Attenuation::dBUnit(), msg->body.IFMod.input),
					 parent_->getAnt()->getId(), 
					 msg->body.IFMod.band,
					 msg->body.IFMod.pos);
	  
	  sza::util::Rx::setIfOutputAtten(Attenuation(Attenuation::dBUnit(), msg->body.IFMod.output),
					  parent_->getAnt()->getId(), 
					  msg->body.IFMod.band,
					  msg->body.IFMod.pos);
	  
	} else // Else just set the output
	  
	  sza::util::Rx::setIfOutputAtten(Attenuation(Attenuation::dBUnit(), msg->body.IFMod.output),
					  parent_->getAnt()->getId(), 
					  msg->body.IFMod.band,
					  msg->body.IFMod.pos);
      }
    }

    break;

    //------------------------------------------------------------
    // Set the IF attenuation
    //------------------------------------------------------------
    
  case sza::array::IFMOD_SET_ATTEN:
    
    // If the total was specified
    
    if(msg->body.IFMod.attenSet & sza::util::IFAtten::ATTEN_TOTAL) {
      IFMod_->setAtten(msg->body.IFMod.total);
      
      // Else if the input was specified
      
    } else if(msg->body.IFMod.attenSet & sza::util::IFAtten::ATTEN_INPUT) {
      
      // If both input and output were specified, set them together
      
      if(msg->body.IFMod.attenSet & sza::util::IFAtten::ATTEN_OUTPUT) 
	IFMod_->setAtten(msg->body.IFMod.input, msg->body.IFMod.output);
      else // Else just set the input
	IFMod_->setInputAtten(msg->body.IFMod.input);
      
    } else if(msg->body.IFMod.attenSet & sza::util::IFAtten::ATTEN_OUTPUT) {
      
      // If both input and output were specified, set them together
      
      if(msg->body.IFMod.attenSet & sza::util::IFAtten::ATTEN_INPUT) 
	IFMod_->setAtten(msg->body.IFMod.input, msg->body.IFMod.output);
      else // Else just set the output
	IFMod_->setOutputAtten(msg->body.IFMod.output);
    }
    break;
    
    //------------------------------------------------------------
    // Set the IF attenuation to defaults
    //------------------------------------------------------------
    
  case sza::array::IFMOD_SET_TO_DEFAULT:
    
    // Just set the total attenuation
      
    IFMod_->setAtten(Rx::getIfTotalAtten(getAnt()->getId(), rxId_, msg->body.IFMod.pos));

    break;

  default:
    break;
  }
}  

/**.......................................................................
 * Process a message to configure the LO chain
 */
void AntennaRx::processLoMsg(AntennaRxMsg* msg)
{
  switch (msg->body.lo.msgId) {

    // A command to toggle LO stages

  case sza::array::LO_TOGGLE:

    toggleLo(msg->body.lo.oscs, msg->body.lo.stages,
	     msg->body.lo.on);

    break;

    // Set the loop gain 

  case sza::array::LO_LOOPGAIN:

    if(msg->body.lo.oscs & sza::util::LoOsc::YIG)
      yig_->setLoopGainResistance(msg->body.lo.loopGain);

    if(msg->body.lo.oscs & sza::util::LoOsc::VARACTOR)
      varactor_->setLoopGainResistance(msg->body.lo.loopGain);

    if(msg->body.lo.oscs & sza::util::LoOsc::GUNN)
      gunn_->setLoopGainResistance(msg->body.lo.loopGain);
    
    break;

    // Set the Yig damping gain

  case sza::array::LO_DAMPGAIN:
    yig_->setDampingResistance(msg->body.lo.dampGain);
    break;

    // Set the LO frequency for the yig or gunn

  case  sza::array::LO_LO_FREQ:

    if(msg->body.lo.oscs & sza::util::LoOsc::YIG)
      yig_->setLoFrequency(msg->body.lo.frequency);

    if(msg->body.lo.oscs & sza::util::LoOsc::GUNN)
      gunn_->setLoFrequency(msg->body.lo.frequency);

    break;
    
    // Set the default yig frequency

  case  sza::array::LO_DEFAULT_FREQ:

    if(msg->body.lo.oscs & sza::util::LoOsc::YIG)
      Rx::setYigFrequency(Frequency(Frequency::MegaHz(), msg->body.lo.frequency), 
			  parent_->getAnt()->getId(), 
			  msg->body.lo.rxId);
    break;
    
    // Set the operating frequency for the gunn

  case  sza::array::LO_FREQ:

    if(msg->body.lo.oscs & sza::util::LoOsc::GUNN)
      gunn_->setFrequency(msg->body.lo.frequency);

    break;

    // Set the Yig operating voltage

  case  sza::array::LO_VOLTAGE:

    if(msg->body.lo.oscs & sza::util::LoOsc::YIG)
      yig_->setOperatingVoltage(msg->body.lo.voltage);

    if(msg->body.lo.oscs & sza::util::LoOsc::GUNN)
      gunn_->setVoltage(msg->body.lo.voltage);

    break;

    // Set the default Yig or Gunn operating voltage                                                                                             
  case  sza::array::LO_DEFAULT_VOLTAGE:
    
    if(msg->body.lo.oscs & sza::util::LoOsc::GUNN) {
      Rx::setGunnVoltage(Voltage(Voltage::CentiVolts(), msg->body.lo.voltage), parent_->getAnt()->getId());
    }

    break;

    // Set the Yig ID

  case  sza::array::LO_ID:

    if(msg->body.lo.oscs & sza::util::LoOsc::YIG)
      yig_->downloadId(msg->body.lo.id, msg->body.lo.month, msg->body.lo.day, 
		       msg->body.lo.year);

    if(msg->body.lo.oscs & sza::util::LoOsc::GUNN)
      gunn_->downloadId(msg->body.lo.id, msg->body.lo.month, msg->body.lo.day, 
			msg->body.lo.year, msg->body.lo.voltage, msg->body.lo.npt);

    break;

    // Send a tuning table entry to the Yig

  case  sza::array::LO_TUNINGTABLE:

    if(msg->body.lo.oscs & sza::util::LoOsc::YIG)
      yig_->downloadTuningTableEntry(msg->body.lo.voltage, msg->body.lo.frequency);

    if(msg->body.lo.oscs & sza::util::LoOsc::GUNN)
      gunn_->downloadTuningTableEntry(msg->body.lo.frequency, msg->body.lo.tunerPos, 
				      msg->body.lo.backshortPos, msg->body.lo.attenPos);
    
    break;

  case  sza::array::LO_ONEWIRE:

    if(msg->body.lo.oscs & sza::util::LoOsc::YIG)
      yig_->downloadTuningTableToOneWire();

    if(msg->body.lo.oscs & sza::util::LoOsc::YIG)
      gunn_->downloadTuningTableToOneWire();

    break;

    // Enable/Disable auto lock

  case  sza::array::LO_AUTOLOCK:

    if(msg->body.lo.oscs & sza::util::LoOsc::YIG)
      yig_->enableAutoLock(msg->body.lo.on);

    if(msg->body.lo.oscs & sza::util::LoOsc::GUNN)
      gunn_->enableAutoRelock(msg->body.lo.on);

    break;

    // Set DAC calibration coefficient

  case  sza::array::LO_SETDACCOEFF:

    yig_->setDACCalibrationCoefficient(msg->body.lo.coeff);

    break;

    // Set a Gunn device position

  case  sza::array::LO_POS_GUNN_DEVICE:
    
    if(msg->body.lo.stages & sza::util::LoStage::LO_TUNER) 
      gunn_->setDevicePosition(sza::util::LoStage::LO_TUNER, msg->body.lo.position);
    if(msg->body.lo.stages & sza::util::LoStage::LO_BACKSHORT) 
      gunn_->setDevicePosition(sza::util::LoStage::LO_BACKSHORT, msg->body.lo.position);
    if(msg->body.lo.stages & sza::util::LoStage::LO_ATTEN) 
      gunn_->setDevicePosition(sza::util::LoStage::LO_ATTEN, msg->body.lo.position);
    
    break;

    // Set a Gunn device position

  case  sza::array::LO_JOG_GUNN_DEVICE:
    
    if(msg->body.lo.stages & sza::util::LoStage::LO_TUNER) 
      gunn_->jogDevicePosition(sza::util::LoStage::LO_TUNER, msg->body.lo.position);
    if(msg->body.lo.stages & sza::util::LoStage::LO_BACKSHORT) 
      gunn_->jogDevicePosition(sza::util::LoStage::LO_BACKSHORT, msg->body.lo.position);
    if(msg->body.lo.stages & sza::util::LoStage::LO_ATTEN) 
      gunn_->jogDevicePosition(sza::util::LoStage::LO_ATTEN, msg->body.lo.position);

    break;

    // Home a Gunn device

  case  sza::array::LO_HOME_GUNN_DEVICE:
    
    gunn_->homeDevice(msg->body.lo.stages);

  default:
    break;
  }
}

/**.......................................................................
 * Toggle LO stages
 */
void AntennaRx::toggleLo(sza::util::LoOsc::Osc osc, 
			 sza::util::LoStage::Stage stages,
			 bool on)
{
  // Control YIG stages?
  
  if(osc & sza::util::LoOsc::YIG) {
    if(stages & sza::util::LoStage::LO_SWEEP) 
      yig_->turnSweepOn(on);
  }
  
  // Control Varactor stages?
  
  if(osc & sza::util::LoOsc::VARACTOR) {
    if(stages & sza::util::LoStage::LO_GUNN) 
      varactor_->turnGunnOn(on);
    if(stages & sza::util::LoStage::LO_SWEEP) 
      varactor_->turnSweepOn(on);
    if(stages & sza::util::LoStage::LO_RFMONITOR) 
      varactor_->turnMonitorOn(on);
  }

  // Control bias-tuned Gunn stages?
  
  if(osc & sza::util::LoOsc::GUNN) {
    if(stages & sza::util::LoStage::LO_GUNN) 
      gunn_->setDeviceState(LoStage::LO_GUNN, on);
    if(stages & sza::util::LoStage::LO_SWEEP) 
      gunn_->setDeviceState(LoStage::LO_SWEEP, on);
    if(stages & sza::util::LoStage::LO_IFMONITOR) 
      gunn_->setDeviceState(LoStage::LO_IFMONITOR, on);
  }
}  

/**.......................................................................
 * Process a Message to control the Thermal module
 */
void AntennaRx::processThermalMsg(AntennaRxMsg* msg)
{
  LogStream errStr;
  
  switch(msg->body.thermal.msgId) {
  case sza::array::THERMAL_SET_TEMP:
    thermal_->setTemperature(msg->body.thermal.target, msg->body.thermal.value);
    break;
  case sza::array::THERMAL_SET_MODE:
    thermal_->setMode(msg->body.thermal.target, msg->body.thermal.mode);
    break;
  case sza::array::THERMAL_SET_LOOP_GAIN:
    thermal_->setLoopGain(msg->body.thermal.target, msg->body.thermal.value);
    break;
  case sza::array::THERMAL_SET_INTEG_CONST:
    thermal_->setIntegrationConstant(msg->body.thermal.target, msg->body.thermal.value);
    break;
  case sza::array::THERMAL_SET_LOOP_BW:
    thermal_->setLoopBandWidth(msg->body.thermal.target, msg->body.thermal.value);
    break;
  case sza::array::THERMAL_SET_RATE_CONST:
    thermal_->setRateConstant(msg->body.thermal.target, msg->body.thermal.value);
    break;
  case sza::array::THERMAL_SET_PROP_CONST:
    thermal_->setCirculationFanPropConst(msg->body.thermal.value);
    break;
  case sza::array::THERMAL_SET_VOLTAGE_OFFSET:
    thermal_->setVoltageOffset(msg->body.thermal.value);
    break;
  case sza::array::THERMAL_SET_EBOX_EQ_STATE:
    thermal_->setEboxEqState(msg->body.thermal.eqState);
    break;
  case sza::array::THERMAL_SET_EBOX_INT_ERROR:
    thermal_->setEboxIntError(msg->body.thermal.value);
    break;
  default:
    break;
  }
}

/**.......................................................................
 * Process a Message to control the Thermal module
 */
void AntennaRx::processTiltMeterMsg(AntennaRxMsg* msg)
{
  switch(msg->body.tiltmeter.msgId) {
  case sza::array::TILTMETER_SET_TEMP:
    tiltmeter_->setTemperature(Temperature(Temperature::Centigrade(), msg->body.tiltmeter.value));
    break;
  case sza::array::TILTMETER_REGULATE_TEMP:
    tiltmeter_->regulateTemperature(msg->body.tiltmeter.mode,
				    msg->body.tiltmeter.value);
    break;
  case sza::array::TILTMETER_SET_LOOP_GAIN:
    tiltmeter_->setLoopGain(msg->body.tiltmeter.value);
    break;
  case sza::array::TILTMETER_SET_INTEG_CONST:
    tiltmeter_->setLoopIntegrationConstant(msg->body.tiltmeter.value);
    break;
  case sza::array::TILTMETER_SET_RATE_CONST:
    tiltmeter_->setLoopRateConstant(msg->body.tiltmeter.value);
    break;
  case sza::array::TILTMETER_SET_LOOP_BW:
    tiltmeter_->setLoopBandwidth(msg->body.tiltmeter.value);
    break;
  case sza::array::TILTMETER_WRITE_TO_EEPROM:
    tiltmeter_->writeToEeprom();
    break;
  case sza::array::TILTMETER_ZEROS:
    {
      Angle afZero, lrZero;
      afZero.setArcMinutes(msg->body.tiltmeter.afZero);
      lrZero.setArcMinutes(msg->body.tiltmeter.lrZero);
    
      tiltmeter_->setZeros(afZero, lrZero);
    }
    break;
  default:
    break;
  }
}

void AntennaRx::stepCanCommand(AntennaRxMsg::MsgType type)
{
  CanInstruction::Type instrType;

  switch (type) {
  case AntennaRxMsg::SELECT_RX:
    DBPRINT(true, Debug::DEBUG15, "Stepping instruction");
    selectRxCommand_.executeNextInstruction();
    break;
  case AntennaRxMsg::SET_BIAS:
    DBPRINT(true, Debug::DEBUG15, "Stepping instruction");
    setBiasCommand_.executeNextInstruction();
    break;
  default:
    break;
  }
}
#endif

sza::util::AntNum* AntennaRx::getAnt()
{
  return parent_->getAnt();
}

/**........................................................................
 * Forward a message to the tracker task that we are changinf frequencies
 */
void AntennaRx::sendTrackerRxMsg(sza::util::Rx::Id rxId)
{
  AntennaMasterMsg masterMsg;
  TrackerMsg* trackerMsg = masterMsg.getDriveMsg()->getTrackerMsg();

  trackerMsg->packRxMsg(rxId);

  parent_->forwardMasterMsg(&masterMsg);
}

/**.......................................................................
 * Register the receipt of a CARMA control-program command that needs to be
 * acknowledged when has taken effect.
 */
void AntennaRx::registerCarmaRequest(AntennaRxMsg* msg)
{
  if(msg->carmaSeqNoType_ != sza::util::GenericTaskMsg::NONE) {
    carmaSeqNoType_                     = msg->carmaSeqNoType_;
    lastReqCarmaSeqNo_[carmaSeqNoType_] = msg->carmaSeqNo_;
  }

  // Now that we can have multiple subarray controllers talking to
  // us, the sequence numbers are no longer guaranteed to be
  // monotonic!  
  //
  // As long as the sequence number received is greater than the
  // last one we acknowledged, it's ok.  But if the sequence numbers
  // step backwards, then when we check if lastAckSeqNo <
  // lastReqSeqNo, it will fail, and we will not acknowledge the
  // request.  
  //
  // Therefore, if this sequence number is less than the last one we
  // acknowledged, we now reset the last acknowledged seq number so
  // that the check will succeed:
  
  if((msg->carmaSeqNo_ > 0) && (msg->carmaSeqNo_ < lastAckCarmaSeqNo_[carmaSeqNoType_])) {
    lastAckCarmaSeqNo_[carmaSeqNoType_] = msg->carmaSeqNo_-1;
  }
  
#if 0
  // Just as a test - write these now

  sendCarmaSeqNoMsg(true);
#endif
}

/**.......................................................................
 * Send a message to write a CARMA sequence number
 */
void AntennaRx::sendCarmaSeqNoMsg(bool success)
{
  AntennaMasterMsg msg;

  if(carmaSeqNoType_ != sza::util::GenericTaskMsg::NONE) {
    if(lastAckCarmaSeqNo_[carmaSeqNoType_] < lastReqCarmaSeqNo_[carmaSeqNoType_]) {

      msg.getControlMsg()->packCarmaSeqNoMsg(lastReqCarmaSeqNo_[carmaSeqNoType_], carmaSeqNoType_, success);
      parent_->forwardMasterMsg(&msg);

      lastAckCarmaSeqNo_[carmaSeqNoType_] = lastReqCarmaSeqNo_[carmaSeqNoType_];
    }
  }
}

bool AntennaRx::moveTertiary(sza::util::Rx::Id rxId)
{
  // New-style CalTert API

  if(parent_->newCaltert()) {

    unsigned char tertStateVal;
    share_->readReg("caltert", "tertState", &tertStateVal);

    CalTertNew::TertState tertState = (CalTertNew::TertState) tertStateVal;
    if(tertState == CalTertNew::rxIdToTertState(rxId)) {
      COUT("Returning false for moveTertiary: rxId = " << rxId 
	   << " tertState = " << tertState << " val = " 
	   << CalTertNew::rxIdToTertState(rxId));
      return false;
    } else {
      COUT("Returning true for moveTertiary");
      return true;
    }

    // Old-style CalTert API

  } else {

    unsigned char posnCode;
    share_->readReg("caltert", "posnCode", &posnCode);
    
    unsigned char tertState;
    share_->readReg("caltert", "tertState", &tertState);
    
    if(rxId == sza::util::Rx::RX30GHZ && posnCode == 0 && tertState == 0) {
      return false;
    } else if(rxId == sza::util::Rx::RX90GHZ && posnCode == 1 && tertState == 0) {
      return false;
    } else {
      return true;
    }
  }
}

void AntennaRx::setTuningPending(bool pending)
{
  share_->setTuningPending(pending);
}
