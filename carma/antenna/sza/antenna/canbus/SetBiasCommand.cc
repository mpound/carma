#include "carma/antenna/sza/antenna/control/AntennaRx.h"

#include "carma/antenna/sza/antenna/canbus/SetBiasCommand.h"

#include "carma/szautil/Directives.h"

using namespace std;
using namespace sza::util;
using namespace sza::antenna::canbus;
using namespace sza::antenna::control;


sza::antenna::canbus::Receiver::Amp SetBiasCommand::biasTypes30GHz_[nBias_] = 
  {
    sza::antenna::canbus::Receiver::Amp30GHzRFStage4Vg,
    sza::antenna::canbus::Receiver::Amp30GHzRFStage4Id,
    sza::antenna::canbus::Receiver::Amp30GHzRFStage3Vg,
    sza::antenna::canbus::Receiver::Amp30GHzRFStage3Id,
    sza::antenna::canbus::Receiver::Amp30GHzRFStage2Vg,
    sza::antenna::canbus::Receiver::Amp30GHzRFStage2Id,
    sza::antenna::canbus::Receiver::Amp30GHzRFStage1Vg,
    sza::antenna::canbus::Receiver::Amp30GHzRFStage1Id,
  };

short SetBiasCommand::biasVals30GHz_[nBias_][sza::util::AntNum::NANT] = 
  {
    //ant0  ant1  ant2  ant3  ant4  ant5  ant6  ant7  
    {1300, 1000,  999, 1000, 1500,  500, 1300, 1200},
    { 300,  300,  300,  300,  500,  300,  300,  300},
    {1300, 1000, 1003, 1000, 1500, 1500, 1300, 1000},
    { 300,  300,  299,  300,  300,  300,  300,  300},
    {1300, 1000, 1001, 1000, 1500, 1300, 1300, 1000},
    { 300,  300,  299,  300,  300,  300,  300,  300},
    {1000, 1000, 1004, 1000, 1000, 1100, 1100, 1000},
    { 200,  200,  199,  200,  200,  200,  200,  200}
  };

sza::antenna::canbus::Receiver::Amp SetBiasCommand::biasTypes90GHz_[nBias_] = 
  {
    sza::antenna::canbus::Receiver::Amp90GHzRF1Stage1Vg,
    sza::antenna::canbus::Receiver::Amp90GHzRF1Stage2Vg,
    sza::antenna::canbus::Receiver::Amp90GHzRF2Stage1Vg,
    sza::antenna::canbus::Receiver::Amp90GHzRF2Stage2Vg,
    sza::antenna::canbus::Receiver::Amp90GHzRF1Vd,      
    sza::antenna::canbus::Receiver::Amp90GHzRF2Vd,      
    sza::antenna::canbus::Receiver::Amp90GHzIFVg,       
    sza::antenna::canbus::Receiver::Amp90GHzIFVd,       
  };
	
short SetBiasCommand::biasVals90GHz_[nBias_][sza::util::AntNum::NANT] = 
  {
    //ant0  ant1  ant2  ant3  ant4  ant5  ant6  ant7  
    { 200,  175,  225,  200,  200,  175,  225,  150},
    { 200,  150,  225,  200,  200,  175,  200,  150},
    { 175,  175,  275,  200,  275,  325,  225,  300},
    { 200,  200,  275,  200,  275,  300,  225,  300},
    { 450, 1079,  517,  580,  650, 1170,  450,  107},
    {1030, 1133,  724, 1750,  675,  685, 1325,  640},
    {-410, 1500, -439, -440, -410, -410, -410, -410},
    {1230, -400, 2894, 2930, 1650, 1245, 1500, 1500}
  };

/**.......................................................................
 * Constructor.
 */
SetBiasCommand::SetBiasCommand() 
{
  initialize();
}

SetBiasCommand::SetBiasCommand(AntennaRx* parent, 
			       sza::util::Rx::Id rxId, unsigned seq)
{
  initialize(parent);

  compile(rxId, seq);
}

/**.......................................................................
 * Initialize data members
 */
void SetBiasCommand::initialize(AntennaRx* parent)
{
  parent_            = 0;
  receiver_          = 0;
  drainCurrent30GHz_ = 0;
  gateVoltage30GHz_  = 0;
  commandName_       = "setBias";

  if(parent != 0) {
    parent_   = parent;
    iAnt_     = parent_->getAnt()->getIntId();
    receiver_ = parent_->receiver_;

    COUT("receiver_ = " << receiver_);

    drainCurrent30GHz_      = receiver_->findMonitorPoint("drainCurrent30GHz");
    gateVoltage30GHz_       = receiver_->findMonitorPoint("gateVoltage30GHz");
    gateCurrent30GHz_       = receiver_->findMonitorPoint("gateCurrent30GHz");
    ifAmpVoltage30GHz_      = receiver_->findMonitorPoint("ifAmpVoltage30GHz");
    ifAmpCurrent30GHz_      = receiver_->findMonitorPoint("ifAmpCurrent30GHz");
    mixerCurrent30GHz_      = receiver_->findMonitorPoint("mixerCurrent30GHz");
    ledCurrent30GHz_        = receiver_->findMonitorPoint("ledCurrent30GHz");

    gateCurrent90GHz_       = receiver_->findMonitorPoint("gateCurrent90GHz");
    drainCurrent90GHz_      = receiver_->findMonitorPoint("drainCurrent90GHz");
    ifAmpDrainCurrent90GHz_ = receiver_->findMonitorPoint("ifAmpDrainCurrent90GHz");
    ifAmpGateCurrent90GHz_  = receiver_->findMonitorPoint("ifAmpGateCurrent90GHz");
  }
}

/**.......................................................................
 * Destructor.
 */
SetBiasCommand::~SetBiasCommand() {}

/**.......................................................................
 * Compile this command
 */
void SetBiasCommand::compile(sza::util::Rx::Id rxId, unsigned seq)
{
  if(parent_ == 0)
    ThrowError("Parent pointer is NULL");

  init(seq);

  compile(rxId);
}

/**.......................................................................
 * Run this command
 */
void SetBiasCommand::run(AntennaRx* parent, sza::util::Rx::Id rxId, 
			 unsigned seq)
{
  initialize(parent);
  compile(rxId, seq);
  CanCommand::run();
}

/**.......................................................................
 * Compile this command
 */
void SetBiasCommand::compile(AntennaRx* parent, sza::util::Rx::Id rxId, 
			     unsigned seq)
{
  initialize(parent);
  compile(rxId, seq);
}

/**.......................................................................
 * Compile this command
 */
void SetBiasCommand::compile(sza::util::Rx::Id rxId)
{
#if DIR_HAVE_CARMA
  DataType dataType;
  CanMonitorCondition condition;
  CanInstruction instruction;

  for(unsigned iBias=0; iBias < nBias_; iBias++) {

    Receiver::Amp biasType = biasTypeOf(rxId, iBias);
    short biasVal          = biasValOf(rxId, iBias);
    short biasLimit        = biasLimitOf(rxId, iBias);

    COUT("Compiling setBias: iBias = " << iBias << " type = " << biasType << " val = " << biasVal);

    insertWait(receiver_);

    // First instruction is to set the requested bias


    instruction.setTo(receiver_, 
		      receiver_->setAmpBias(biasType, biasVal, false));
    instructions_.push_back(instruction);

    // Next instruction is to wait until the bias has been achieved

    //    condition.setTo(DataTypeTruthFn::greaterThanEqLessThenEq, 
    //		    (short)(biasVal - biasLimit(rxId, iBias)), 
    //		    (short)(biasVal + biasLimit(rxId, iBias)));

    //    instruction.setTo(monitorPointOf(bias), condition, coordOf(bias));
    //    instructions_.pushback(instruction);

    // For now, insert a true condition on an arbitrary monitor point,
    // just to insert a wait after the last CAN command

    instruction.setTo(ledCurrent30GHz_, condition, "");
    instructions_.push_back(instruction);
  }
#endif
}

/**.......................................................................
 * Return the value of the bias corresponding to index iBias
 */
short SetBiasCommand::biasValOf(sza::util::Rx::Id rxId, unsigned iBias)
{
  switch (rxId) {
  case sza::util::Rx::RX30GHZ:
    return biasVals30GHz_[iBias][iAnt_];
    break;
  case sza::util::Rx::RX90GHZ:
    return biasVals90GHz_[iBias][iAnt_];
    break;
  default:
    ThrowError("Unrecognized receiver designation");
    break;
  }
}

/**.......................................................................
 * Return the type of the bias corresponding to index iBias
 */
Receiver::Amp SetBiasCommand::biasTypeOf(sza::util::Rx::Id rxId, unsigned iBias)
{
  switch (rxId) {
  case sza::util::Rx::RX30GHZ:
    return biasTypes30GHz_[iBias];
    break;
  case sza::util::Rx::RX90GHZ:
    return biasTypes90GHz_[iBias];
    break;
  default:
    ThrowError("Unrecognized receiver designation");
    break;
  }
}

/**.......................................................................
 * Return the limits for the bias corresponding to index iBias
 */
short SetBiasCommand::biasLimitOf(sza::util::Rx::Id rxId, unsigned iBias)
{
  return biasLimit_; 
}

/**.......................................................................
 * Return the type of the bias corresponding to index iBias
 */
CanMonitorPoint* SetBiasCommand::
monitorPointOf(sza::util::Rx::Id rxId, unsigned iBias)
{
  switch (rxId) {
  case sza::util::Rx::RX30GHZ:
    {
      switch (biasTypes30GHz_[iBias]) {
      case Receiver::Amp30GHzRFStage1Vg:
      case Receiver::Amp30GHzRFStage2Vg:
      case Receiver::Amp30GHzRFStage3Vg:
      case Receiver::Amp30GHzRFStage4Vg:
	return gateVoltage30GHz_;
	break;
      case Receiver::Amp30GHzRFStage1Id:
      case Receiver::Amp30GHzRFStage2Id:
      case Receiver::Amp30GHzRFStage3Id:
      case Receiver::Amp30GHzRFStage4Id:
	return drainCurrent30GHz_;
	break;
      default:
	break;
      }
    }
  break;
  case sza::util::Rx::RX90GHZ:
    {
      switch (biasTypes90GHz_[iBias]) {
      case Receiver::Amp90GHzRF1Stage1Vg: 
      case Receiver::Amp90GHzRF1Stage2Vg:
	break;
      case Receiver::Amp90GHzRF2Stage1Vg:
      case Receiver::Amp90GHzRF2Stage2Vg:
	break;
      case Receiver::Amp90GHzRF1Vd:
	break;
      case Receiver::Amp90GHzRF2Vd:
	break;
      case Receiver::Amp90GHzIFVd:
	break;
      case Receiver::Amp90GHzIFVg:
	break;
      default:
	break;
      }
    }
    break;
  default:
    ThrowError("Unrecognized receiver designation");
    break;
  }

  return 0;
}

void SetBiasCommand::setDefaultBias(sza::util::AntNum::Id antId, 
				    sza::antenna::canbus::Receiver::Amp amp,
				    short bias) 
{
  AntNum ant(antId);
  unsigned iAnt = ant.getIntId();

  for(unsigned iType=0; iType < nBias_; iType++) {
    if(biasTypes30GHz_[iType] == amp) {
      biasVals30GHz_[iType][iAnt] = bias;
      return;
    }
  }
  
  for(unsigned iType=0; iType < nBias_; iType++) {
    if(biasTypes90GHz_[iType] == amp) {
      biasVals90GHz_[iType][iAnt] = bias;
      return;
    }
  }
}
