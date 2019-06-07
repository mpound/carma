#include "carma/antenna/sza/antenna/control/AntennaMaster.h"
#include "carma/antenna/sza/antenna/control/AntennaRx.h"

#include "carma/szautil/Directives.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LoBand.h"

#include "carma/antenna/sza/antenna/canbus/BiasTunedGunn.h"
#include "carma/antenna/sza/antenna/canbus/CalTert.h"
#include "carma/antenna/sza/antenna/canbus/CalTertNew.h"
#include "carma/antenna/sza/antenna/canbus/IFMod.h"
#include "carma/antenna/sza/antenna/canbus/IntMod.h"
#include "carma/antenna/sza/antenna/canbus/VaractorTunedGunn.h"
#include "carma/antenna/sza/antenna/canbus/Yig.h"

#include "carma/antenna/sza/antenna/canbus/CanDevice.h"
#include "carma/antenna/sza/antenna/canbus/SelectRxCommand.h"

using namespace std;
using namespace sza::util;
using namespace sza::antenna::canbus;
using namespace sza::antenna::control;

/**.......................................................................
 * Constructor.
 */
SelectRxCommand::SelectRxCommand() 
{
  initialize();
}

/**.......................................................................
 * Constructor.
 */
SelectRxCommand::SelectRxCommand(AntennaRx* parent, sza::util::Rx::Id rxId, 
				 unsigned seq, bool moveTertiary) 
{
  initialize(parent);

  compile(rxId, seq, moveTertiary);
}

void SelectRxCommand::initialize(AntennaRx* parent)
{
  parent_      = 0;
  calTert_     = 0;
  yig_         = 0;
  ifMod_       = 0;
  intMod_      = 0;
  commandName_ = "selectRx";

  if(parent != 0) {
    parent_   = parent;
    calTert_  = parent_->calTert_;
    yig_      = parent_->yig_;
    ifMod_    = parent_->IFMod_;
    intMod_   = parent_->intMod_;
    gunn_     = parent_->gunn_;
    varactor_ = parent_->varactor_;

    COUT("caltert  = " << calTert_);
    COUT("yig      = " << yig_);
    COUT("ifMod    = " << ifMod_);
    COUT("intmod   = " << intMod_);
    COUT("gunn     = " << gunn_);
    COUT("varactor = " << varactor_);
  }

}

void SelectRxCommand::run(AntennaRx* parent, sza::util::Rx::Id rxId, 
			  unsigned seq, bool moveTertiary)
{
  COUT("SelectRx called with seq = " << seq << " rxId = " << rxId);
  initialize(parent);
  compile(rxId, seq, moveTertiary);
  CanCommand::run();
}

void SelectRxCommand::compile(AntennaRx* parent, sza::util::Rx::Id rxId, 
			      unsigned seq, bool moveTertiary)
{
  initialize(parent);
  compile(rxId, seq, moveTertiary);
}

/**.......................................................................
 * Compile this command
 */
void SelectRxCommand::compile(sza::util::Rx::Id rxId, unsigned seq, bool moveTertiary)
{
  if(parent_ == 0)
    ThrowError("Parent pointer is NULL");

  Rx rx(parent_->getAnt()->getId(), rxId);

  init(seq);

  CanMonitorPoint* monitor;
  DataType dataType;
  CanMonitorCondition condition;

  // The first step is to enable the tertiary

  CanInstruction enableTertiary(calTert_, calTert_->enableTertiary(true, false));

  // Next we want to wait until the tertiary has been enabled

  monitor = calTert_->findMonitorPoint("moveMirOk");
  condition.setTo(DataTypeTruthFn::equals, (unsigned char)1);
  CanInstruction waitForTertiaryToEnable(monitor, condition, 
					 "failed to enable the tertiary");

  // Next we want to make sure the tertiary has its position
  
  CanInstruction setEncoderVal(calTert_, 
			       calTert_->setDefaultEncoderPositionIndex(rxId, false));

  // Next we want to wait until the mirror stops moving

  monitor = calTert_->findMonitorPoint("mirStable");
  condition.setTo(DataTypeTruthFn::equals, (unsigned char)0, 1, 6, 60);
  CanInstruction waitForTertiaryToStabilize(monitor, condition, 
					    "mirror failed to stabilize");

  // Next we want to position the tertiary

  CanInstruction positionTertiaryOld(calTert_, 
				     calTert_->positionTertiary(rxId, false));

  CanInstruction positionTertiaryNew(calTert_, 
				     calTert_->positionTertiary(rxId, 0, false));

  // And wait until it has acquired

  monitor = calTert_->findMonitorPoint("tertState");

  if(parent_->parent()->newCaltert()) {
    COUT("Setting condition for tert state to match " << (int)CalTertNew::rxIdToTertState(rxId));
    condition.setTo(DataTypeTruthFn::equals, 
		    (unsigned char)CalTertNew::rxIdToTertState(rxId), 1, 6, 60);
  } else {
    condition.setTo(DataTypeTruthFn::equals, (unsigned char)0, 1, 6, 60);
  }

  CanInstruction waitForTertiaryToAcquire(monitor, condition, 
				       "tertiary couldn't acquire the requested position");

  // Set the IF attenuation 

  CanInstruction setIFAtten(ifMod_, ifMod_->setAtten(rx.getIfTotalAtten(CalPos::SKY), false));

  // Set the IF switch to the correct position

  CanInstruction setIFSwitch(ifMod_, ifMod_->selectBand(rxId, false));
  CanInstruction sendIFDummyMsg(ifMod_, ifMod_->sendDummyMsg(false));
  
  // Turn the varactor on (30 GHz) or off (90 GHz)

  CanInstruction turnVaractorOn(     varactor_, varactor_->turnGunnOn(true,   false));
  CanInstruction turnVaractorOff(    varactor_, varactor_->turnGunnOn(false, false));

  CanInstruction turnVaractorSweepOn( varactor_, varactor_->turnSweepOn(true,   false));
  CanInstruction turnVaractorSweepOff(varactor_, varactor_->turnSweepOn(false, false));

  // Turn the Gunn sweep on (90 GHz) or off (30 GHz) and set the Gunn
  // voltage and frequency (for 90 GHz only)

  COUT("Constructing commands turn on the Gunn, set the Gunn voltage (to " << sza::util::Rx::getGunnVoltage(parent_->getAnt()->getId()).gunnUnits()
       << ") and set the Gunn Frequency (to " << sza::util::Rx::getGunnFrequency(parent_->getAnt()->getId()).gunnUnits() << ")");

  CanInstruction turnGunnOn(        gunn_, gunn_->setDeviceState(sza::util::LoStage::LO_GUNN,  true,  false));
  CanInstruction turnGunnOff(       gunn_, gunn_->setDeviceState(sza::util::LoStage::LO_GUNN,  false, false));

  CanInstruction turnGunnSweepOn(   gunn_, gunn_->setDeviceState(sza::util::LoStage::LO_SWEEP, true,  false));
  CanInstruction turnGunnSweepOff(  gunn_, gunn_->setDeviceState(sza::util::LoStage::LO_SWEEP, false, false));

  CanInstruction setGunnVoltage(    gunn_, gunn_->setVoltage(sza::util::Rx::getGunnVoltage(parent_->getAnt()->getId()).gunnUnits(), false));
  CanInstruction setGunnLoFrequency(gunn_, gunn_->setLoFrequency(sza::util::Rx::getGunnFrequency(parent_->getAnt()->getId()).gunnUnits(), false));

  // Set the LO Term power to preset value

  CanInstruction setPresetPower(intMod_, intMod_->presetPAMOutputPower(false));

  // Set the LO Term attenuation

  CanInstruction setLOTermAtten(intMod_, 
				intMod_->setPAMAttenuation(sza::util::LoBand::getLoTermAttenuation(parent_->getAnt()->getId()).intModUnits(), false));

  monitor = intMod_->findMonitorPoint("pamAtten");
  condition.setTo(DataTypeTruthFn::equals, sza::util::LoBand::getLoTermAttenuation(parent_->getAnt()->getId()).intModUnits());
  CanInstruction waitForLoTermAtten(monitor, condition, "LO Termination attenuation failed to acquire");

  // Lock the yig at the magic frequency for this antenna

  CanInstruction setYigFrequency(yig_, 
				 yig_->setLoFrequency(sza::util::Rx::getYigFrequency(parent_->getAnt()->getId(), rxId).yigUnits(), false));

  monitor = yig_->findMonitorPoint("lockState");
  condition.setTo(DataTypeTruthFn::equals, (unsigned char)3);
  CanInstruction waitForYigToLock(monitor, condition, "yig failed to lock");

  // Wait for the 1cm Gunn to report that it's locked

  monitor = varactor_->findMonitorPoint("lockStatus");
  condition.setTo(DataTypeTruthFn::equals, (unsigned char)1, 1, 6, 60);
  CanInstruction waitFor1cmGunnToLock(monitor, condition, "varactor failed to lock");

  // Wait for the 1cm Gunn to report that it's turned on

  monitor = varactor_->findMonitorPoint("gunnStatus");
  condition.setTo(DataTypeTruthFn::equals, (unsigned char)1, 1, 6, 60);
  CanInstruction waitFor1cmGunnToTurnOn(monitor, condition, "varactor failed to turn on");

  // Wait for the 3mm Gunn to report that it's locked

  monitor = gunn_->findMonitorPoint("phaseLockState");
  condition.setTo(DataTypeTruthFn::equals, (unsigned char)7, 1, 6, 60);
  CanInstruction waitFor3mmGunnToLock(monitor, condition, "3mm gunn failed to lock");

  // Wait for the 3mm Gunn to report that it's turned on

  monitor = gunn_->findMonitorPoint("gunnStatus");
  condition.setTo(DataTypeTruthFn::equals, (unsigned char)1, 1, 6, 60);
  CanInstruction waitFor3mmGunnToTurnOn(monitor, condition, "3mm gunn failed to turn on");

  //------------------------------------------------------------
  // Finally, put it all together
  //------------------------------------------------------------

  // Only move the tertiary if we have to

  COUT("Move tertiary = " << moveTertiary);

  if(moveTertiary) {

    // Insert a wait on the caltert to ensure that the module is online

    insertWait(calTert_);
  
    // Now add the other commands

    // Only add commands to enable & set encoder values if we are
    // using the old-style caltert API

    if(!parent_->parent()->newCaltert()) {
      instructions_.push_back(enableTertiary);
      instructions_.push_back(waitForTertiaryToEnable);
      instructions_.push_back(setEncoderVal);
      instructions_.push_back(waitForTertiaryToStabilize);
      instructions_.push_back(positionTertiaryOld);
    } else {

    // But we always instruct the tertiary to go to the requested
    // position

      COUT("Pushing back new position tertiary instruction");
      instructions_.push_back(positionTertiaryNew);
    }

    instructions_.push_back(waitForTertiaryToAcquire);
  }

  // Bias the receiver

  SetBiasCommand setBias(parent_, rxId, seq);
  insert(setBias);

  instructions_.push_back(setIFSwitch);

  // EML inserting a dummy message as of Mar 25 2014, since the IF
  // switch appears not to be switching on occasion -- the sign of
  // messages being sent to the canbus too rapidly

  instructions_.push_back(sendIFDummyMsg);

  // EML making this change as of Feb 1 2011.  James asked that we set
  // the LO term to preset power instead of commanding specific LO
  // term attenuations

#if 1
  instructions_.push_back(setPresetPower);
#else
  instructions_.push_back(setLOTermAtten);
  instructions_.push_back(waitForLoTermAtten);
#endif

  instructions_.push_back(setIFAtten);

  // Don't lock the yig in this command anymore: very intermittently,
  // the yigs can lock at the wrong frequency.  Also, some of the
  // Gunns will consistently lock at the wrong frequency when the yigs
  // are re-locked, so we want to avoid this.

  // EML adding back in 08 June 2010, as requested.

  instructions_.push_back(setYigFrequency);
  instructions_.push_back(waitForYigToLock);

  //------------------------------------------------------------
  // If the requested receiver is the 3mm one, add commands to control
  // the Bias-tuned Gunn.  Turn the varactor off too, just in case it
  // generates RFI at 3mm
  //------------------------------------------------------------

  if(rxId == Rx::RX90GHZ) {
    instructions_.push_back(turnVaractorOff);
    instructions_.push_back(turnGunnOn);

    // EML adding a wait for gunn status here since it appears that
    // sometimes the modules don't get the second command if the
    // second comes too soon after the first

    instructions_.push_back(waitFor3mmGunnToTurnOn);

    // James says we don't need to do this anymore -- EML, 8 Nov 2012
    //
    //    instructions_.push_back(setGunnVoltage);

    instructions_.push_back(setGunnLoFrequency);
    instructions_.push_back(waitFor3mmGunnToLock);

    //------------------------------------------------------------
    // If tuning to 1cm, turn the Gunn off, since it is known to
    // generate RFI at 1cm.  Turn the varactor on, in case we were
    // previously observing at 90 GHz with it off.
    //------------------------------------------------------------

  } else {
    instructions_.push_back(turnGunnOff);
    instructions_.push_back(turnVaractorOn);
    instructions_.push_back(waitFor1cmGunnToLock);
  }
}

/**.......................................................................
 * Destructor.
 */
SelectRxCommand::~SelectRxCommand() {}
