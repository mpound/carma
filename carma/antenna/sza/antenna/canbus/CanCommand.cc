#include "carma/antenna/sza/antenna/canbus/CanCommand.h"
#include "carma/szautil/Debug.h"

using namespace std;
using namespace sza::antenna::canbus;
using namespace sza::util;

unsigned CanCommand::lastAck_ = 0;
unsigned CanCommand::lastReq_ = 0;

/**.......................................................................
 * Constructor.
 */
CanCommand::CanCommand() 
{
  init(0);

  active_               = false;
  ignoreErrors_         = false; // Default to abort this command if
				 // any step fails (times out)
  commandDoneHandler_   = 0;
  commandFailedHandler_ = 0;
  executeDoneHandler_   = 0;

  commandName_ = "generic Can command";
}

/**.......................................................................
 * Destructor.
 */
CanCommand::~CanCommand() {}

/**.......................................................................
 * (Re)Initialize this command
 */
void CanCommand::init(unsigned seq)
{
  instructions_.resize(0);
  lastReq_ = seq;

  // If the new sequence number is < the last sequence number we
  // acknowledged, reset our sequence number tracking.  This can
  // happen now that commands are being received from different
  // systems

  if(seq < lastAck_)
    lastAck_ = seq;

  reset();
}

void CanCommand::insertWait(CanDevice* device)
{
  CanMonitorPoint* received_ = device->findMonitorPoint("received");
  CanInstruction instruction;
  CanMonitorCondition condition;

  instruction.setTo(received_, condition, "");
  instructions_.push_back(instruction);
}

/**.......................................................................
 * (Re)Initialize this command
 */
void CanCommand::reset()
{
  active_  = true;
  nextInstruction_ = instructions_.begin();
}

/**.......................................................................
 * A handler for signalling that a condition was satisfied
 */
CAN_MONITOR_CONDITION_HANDLER(CanCommand::conditionSatisfied)
{
  CanCommand* command = (CanCommand*) arg;

  if(satisfied) {

    if(command->executeDoneHandler_ != 0) 
      command->executeDoneHandler_(command->executeDoneArg_);

  } else {

    //    COUT("Condition was not satisfied(2): " << command->ignoreErrors_);

    // If we are not ignoring errors, deactivate this command

    if(!command->ignoreErrors_)
      command->active_ = false;

    // Notify anyone who's listening that an error occurred

    if(command->commandFailedHandler_ != 0) 
      command->commandFailedHandler_(command->commandName_, message);

    // If we are ignoring errors, call the command done handler

    if(command->ignoreErrors_)
      command->executeDoneHandler_(command->executeDoneArg_);
  }
}

/**.......................................................................
 * Execute the next instruction in the list
 */
CanInstruction::Type CanCommand::executeNextInstruction()
{
  CanInstruction::Type type = CanInstruction::NONE;

  // If this command is complete, call our wrap-up function
  
  if(isComplete()) {
    COUT("Registering completion");
    registerCompletion();
  }

  // Else execute the next instruction if this command is still active.  

  else if(active_) {
    type = nextInstruction_->execute(this);

    nextInstruction_++;

    // If the instruction was a message sent to a CAN module, call the
    // handler to indicate that the next instruction can be executed.

    if(executeDoneHandler_ != 0 && type == CanInstruction::MESSAGE) {
      executeDoneHandler_(executeDoneArg_);
    }
  }

  return type;
}

/**.......................................................................
 * Set the state of a condition
 */
void CanCommand::registerCompletion()
{
  // Mark this command as inactive

  active_ = false;

  // If the sequence number of this command is still of interest, call
  // any handler which may have been registered with us.

  COUT("lastReq_ = " << lastReq_ << " lastAck_ = " << lastAck_ << " handler = " << commandDoneHandler_);

  if(lastReq_ >= lastAck_)
    if(commandDoneHandler_ != 0)
      commandDoneHandler_(lastReq_);

  lastAck_ = lastReq_;
}

/**.......................................................................
 * Return true if this command is complete
 */
bool CanCommand::isComplete()
{
  return nextInstruction_ == instructions_.end();
}

/**.......................................................................
 * Install a handler to be called when this command is complete
 */
void CanCommand::
installCommandDoneHandler(CAN_COMMAND_DONE_HANDLER(*handler))
{
  commandDoneHandler_ = handler;
}

/**.......................................................................
 * Install a handler to be called when this command is complete
 */
void CanCommand::
installCommandFailedHandler(CAN_COMMAND_FAILED_HANDLER(*handler))
{
  commandFailedHandler_ = handler;
}

/**.......................................................................
 * Install a handler to be called when this command is complete
 */
void CanCommand::
installExecuteDoneHandler(CAN_EXECUTE_DONE_HANDLER(*handler),
			  sza::antenna::control::AntennaRxMsg::MsgType type)
{
  executeDoneHandler_ = handler;
  executeDoneArg_     = type;

  DBPRINT(true, Debug::DEBUG15, "Installing execute done handler: handler =  " 
	  << executeDoneHandler_ << " arg = " << executeDoneArg_ 
	  << " " << pthread_self());
}


/**.......................................................................
 * Run this command
 */
void CanCommand::run()
{
  DBPRINT(true, Debug::DEBUG15, "Inside run");
  
  for(unsigned i=0; i < instructions_.size(); i++)
    DBPRINT(true, Debug::DEBUG15, "Instruction " << i << " is of type: " 
	    << instructions_[i].type_);

  reset();
  executeNextInstruction();
}

/**.......................................................................
 * Insert another command
 */
void CanCommand::insert(CanCommand& command)
{
  for(unsigned iInstr=0; iInstr < command.instructions_.size(); iInstr++)
    instructions_.push_back(command.instructions_[iInstr]);
}

/**.......................................................................
 * Continue executing instructions even if an error occurs
 */
void CanCommand::ignoreErrors(bool ignore)
{
  ignoreErrors_ = ignore;
}
