#include "carma/antenna/sza/antenna/canbus/CanCommand.h"
#include "carma/antenna/sza/antenna/canbus/CanInstruction.h"

#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"

using namespace std;
using namespace sza::util;
using namespace sza::antenna::canbus;

/**.......................................................................
 * Constructor for a CAN message
 */
CanInstruction::CanInstruction()
{
  type_      = NONE;
  device_    = 0;
  monitor_   = 0;
}

/**.......................................................................
 * Constructor for a CAN message
 */
CanInstruction::CanInstruction(const CanInstruction& instruction)
{
  setTo(instruction);
}

void CanInstruction::setTo(const CanInstruction& instruction)
{
  type_      = instruction.type_;
  device_    = instruction.device_;
  messages_  = instruction.messages_;
  monitor_   = instruction.monitor_;
  message_   = instruction.message_;
  condition_ = instruction.condition_;
  coord_     = instruction.coord_;
}

/**.......................................................................
 * Constructor for a CAN message
 */
CanInstruction::CanInstruction(CanDevice* device, 
			       std::vector<carma::canbus::Message> messages)
{
  setTo(device, messages);
}

void CanInstruction::setTo(CanDevice* device, 
			   std::vector<carma::canbus::Message> messages)
{
  type_     = MESSAGE;
  device_   = device;
  messages_ = messages;
}

/**.......................................................................
 * Constructor for a monitor point condition
 */
CanInstruction::CanInstruction(CanMonitorPoint* monitor, 
			       CanMonitorCondition& condition,
			       char* message,
			       sza::util::Coord* coord)
{
  setTo(monitor, condition, message, coord);
}

void CanInstruction::setTo(CanMonitorPoint* monitor, 
			   CanMonitorCondition& condition,
			   char* message,
			   sza::util::Coord* coord)
{
  type_      = CONDITION;
  monitor_   = monitor;
  condition_ = condition;
  message_   = message;
  coord_     = sza::util::Coord(coord);
}

/**.......................................................................
 * Destructor.
 */
CanInstruction::~CanInstruction() {}

/**.......................................................................
 * Execute this instruction
 */
CanInstruction::Type CanInstruction::execute(CanCommand* command)
{
  if(type_ == MESSAGE) {

    for(unsigned iMsg=0; iMsg < messages_.size(); iMsg++) {

      COUT("Posting message " << iMsg << " for device: " << device_ 
	   << " id = " << messages_[iMsg].getId()
	   << " size = " << messages_[iMsg].getData().size());

      try {
	device_->postMessage(messages_[iMsg]);
      } catch(...) {
	COUT("Caught an error");
      }

    }

  } else {
    
    monitor_->registerConditionHandler(CanCommand::conditionSatisfied, command, 
				       message_, condition_, &coord_);
  }
  
  return type_;
}

