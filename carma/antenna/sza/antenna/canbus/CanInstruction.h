#ifndef SZA_ANTENNA_CANBUS_CANINSTRUCTION_H
#define SZA_ANTENNA_CANBUS_CANINSTRUCTION_H

/**
 * @file CanInstruction.h
 * 
 * Tagged: Thu Oct 28 07:32:15 PDT 2004
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace antenna {
    namespace canbus {
      
      class CanCommand;

      class CanInstruction {
      public:
	
	// Instructions can be either CAN messages or monitor
	// conditions
	
	enum Type {
	  NONE,
	  MESSAGE,
	  CONDITION
	};
	
	/**
	 * Constructors
	 */
	CanInstruction(); // Needed so that we can do
			  // vector<CanInstruction>::resize()
	CanInstruction(const CanInstruction& instruction);
	
	CanInstruction(CanDevice* device, 
		       std::vector<carma::canbus::Message> messages);
	
	CanInstruction(CanMonitorPoint* monitor, 
		       CanMonitorCondition& condition, 
		       char* message, 
		       sza::util::Coord* coord=0);
	
	/**
	 * Destructor.
	 */
	virtual ~CanInstruction();
	
	// Set methods, corresponding to each constructor type
	
	void setTo(const CanInstruction& instruction);
	
	void setTo(CanDevice* device, 
		   std::vector<carma::canbus::Message> messages);
	
	void setTo(CanMonitorPoint* monitor, 
		   CanMonitorCondition& condition, 
		   char* message, sza::util::Coord* coord=0);
	
	// Convenience methods
	
	Type execute(CanCommand* command);
	
      private:
	
	friend class CanCommand;

	/**
	 * Each instruction of a CanCommand is encapsulated in an
	 * object of the following type:
	 */
	
	Type type_;  // The type of instruction
	
	//------------------------------------------------------------
	// Instruction for a CAN command
	//------------------------------------------------------------
	
	// The CAN device for whom this intruction is intended
	
	CanDevice* device_;     
	
	// The vector of node messages to send
	
	std::vector<carma::canbus::Message> messages_; 
	
	//------------------------------------------------------------
	// Instruction for a monitor point condition
	//------------------------------------------------------------
	
	CanMonitorPoint* monitor_;      // The monitor point to test
	CanMonitorCondition condition_; // The monitor condition to be met
	sza::util::Coord coord_; // The coordinate of the monitor point
	std::string message_;    // A message to report if the
				 // condition times out
	
      }; // End class CanInstruction
      
    } // End namespace canbus
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CANBUS_CANINSTRUCTION_H
