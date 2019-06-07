
#ifndef SZA_ANTENNA_CANBUS_CANCOMMAND_H
#define SZA_ANTENNA_CANBUS_CANCOMMAND_H

/**
 * @file CanCommand.h
 * 
 * Tagged: Sat Oct 23 13:35:14 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/control/AntennaRxMsg.h"
#include "carma/antenna/sza/antenna/canbus/CanDevice.h"
#include "carma/antenna/sza/antenna/canbus/CanInstruction.h"

#define CAN_COMMAND_DONE_HANDLER(fn) void (fn)(unsigned seq)
#define CAN_COMMAND_FAILED_HANDLER(fn) void (fn)(std::string commandName, std::string message)
#define CAN_EXECUTE_DONE_HANDLER(fn) void (fn)(sza::antenna::control::AntennaRxMsg::MsgType type)

namespace sza {
  namespace antenna {
    namespace canbus {
      
      class CanCommand {
      public:
	
	/**
	 * Constructor.
	 */
	CanCommand();
	
	/**
	 * Destructor.
	 */
	virtual ~CanCommand();

	/**
	 * (Re)Initialize this command
	 */
	virtual void init(unsigned seq);

	/**
	 * Continue executing instructions even if an error occurs?
	 */
	void ignoreErrors(bool ignorexo);

	/**
	 * Return true if this command is complete
	 */
	bool isComplete();

	// A wrap up function to be called when this command is complete

	void registerCompletion();

	/**
	 * Install a handler to be called when this command is complete
	 */
	void installCommandDoneHandler(CAN_COMMAND_DONE_HANDLER(*handler));

	/**
	 * Install a handler to be called if this command fails
	 */
	void installCommandFailedHandler(CAN_COMMAND_FAILED_HANDLER(*handler));

	/**
	 * Install a handler to be called when an instruction has been executed
	 */
	void installExecuteDoneHandler(CAN_EXECUTE_DONE_HANDLER(*handler), 
				       sza::antenna::control::AntennaRxMsg::MsgType type);

	// Reset to the beginning of the instruction list

	void reset();

	// Run this command

	void run();

	// Insert another command into this one

	void insert(CanCommand& command);

      protected:

	friend class CanInstruction;

	// An optional string identifying this command

	std::string commandName_;

	// True if this command is active

	bool active_;

	// True if we should ignore errors

	bool ignoreErrors_;

	// A vector of instructions

	std::vector<CanInstruction> instructions_;

	// The current instruction

	std::vector<CanInstruction>::iterator nextInstruction_;

	// Execute the next isntruction of this command

	CanInstruction::Type executeNextInstruction();

	// A callback for setting the state of a condition

	static CAN_MONITOR_CONDITION_HANDLER(conditionSatisfied);

	// A handler called when this command is complete

	CAN_COMMAND_DONE_HANDLER(*commandDoneHandler_);

	// A handler called if this command fails

	CAN_COMMAND_FAILED_HANDLER(*commandFailedHandler_);

	// A handler called when an instruction has been executed

	CAN_EXECUTE_DONE_HANDLER(*executeDoneHandler_);
	sza::antenna::control::AntennaRxMsg::MsgType executeDoneArg_;

	void insertWait(CanDevice* device);

      protected:

	// The last requested sequence number

	static unsigned lastReq_;

	// The last acknowledged sequence number

	static unsigned lastAck_;

      }; // End class CanCommand
      
    } // End namespace canbus
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CANBUS_CANCOMMAND_H
