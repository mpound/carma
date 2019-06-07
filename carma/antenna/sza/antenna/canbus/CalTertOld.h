#ifndef SZA_ANTENNA_CANBUS_CALTERTOLD_H
#define SZA_ANTENNA_CANBUS_CALTERTOLD_H

/**
 * @file CalTertOld.h
 * 
 * Tagged: Thu Jun 17 20:58:48 UTC 2004
 * 
 * @author 
 */
#include "carma/antenna/sza/antenna/canbus/CanDevice.h"

#include "carma/antenna/sza/antenna/canbus/CalTert.h"

namespace sza {
  namespace antenna {
    
    namespace control {
      class AntennaRx;
    }
    
    namespace canbus {
      
      class CalTertOld : public CalTert {
      public:
	
	// Enumerate known mirror positions
	
	enum MirPos {
	  RX30GHZ  = 0,
	  RX90GHZ  = 1,
	  RX230GHZ = 2,
	};
	
	// Enumerate known tertiary states

	enum TertState {
	  IN_POSITION    = 0,
	  MOVING         = 1,
	  HOMING         = 2,
	  STOPPED        = 3,
	  POS_SOFT_LIMIT = 4,
	  NEG_SOFT_LIMIT = 5,
	  HARD_LIMIT     = 6,
	  ERROR          = 7,
	};

	/**
	 * Constructor.
	 */
	CalTertOld(sza::antenna::control::AntennaRx* parent,
		sza::antenna::control::SzaShare* share, 
		std::string boardName,
		carma::canbus::nodeType node, 
		carma::canbus::CanOutput& io);
	
	/**
	 * Destructor.
	 */
	virtual ~CalTertOld();
	
	/**
	 * Interface with a 1-wire device
	 */
	std::vector<carma::canbus::Message>
	  oneWireInterface(sza::util::CalTertTypes::OwDevice device, 
			   sza::util::CalTertTypes::OwCommand command, 
			   bool send=true);
	
	/**
	 * Request a calibrator position
	 */
	std::vector<carma::canbus::Message>
	  positionCalibrator(sza::util::CalPos::Pos position, bool send=true);
	
	/**
	 * Home the tertiary
	 */
	std::vector<carma::canbus::Message>
	  homeTertiary(bool send=true);
	
	/**
	 * Move the tertiary
	 */
	std::vector<carma::canbus::Message>
	  positionTertiary(sza::util::Rx::Id id, bool send=true);
	
	/**
	 * Move the tertiary
	 */
	std::vector<carma::canbus::Message>
	  positionTertiary(unsigned short position, bool send=true);
	
	/**
	 * Enable the tertiary
	 */
	std::vector<carma::canbus::Message>
	  enableTertiary(bool enable, bool send=true);
	
	/**
	 * Reset the stepper driver
	 */
	std::vector<carma::canbus::Message>
	  resetStepper(bool send=true);
	
	/**
	 * Write the current encoder position to one of the positions
	 * indexed as 30GHz, 90GHz or 230GHz
	 */
	std::vector<carma::canbus::Message>
	  indexCurrentEncoderPosition(sza::util::Rx::Id id, bool send=true);
	
	/**
	 * Write the current encoder position to one of the positions
	 * indexed as 30GHz, 90GHz or 230GHz
	 */
	std::vector<carma::canbus::Message>
	  setEncoderPositionIndex(sza::util::Rx::Id id, 
				  unsigned short index, bool send=true);
	
	std::vector<carma::canbus::Message>
	  setDefaultEncoderPositionIndex(sza::util::Rx::Id id, bool send=true);
	
	void storeEncoderPositionIndex(sza::util::Rx::Id id, 
				       unsigned short index);
	
	unsigned short getEncoderPosition(sza::util::Rx::Id id);

	/**
	 * Register the receipt of a control-program command that needs to be
	 * acknowledged when it has taken effect.
	 */
	void registerRequest(unsigned seq);
	
	/**
	 * Register a handler to be called when the tertiary reports
	 * an in-position message
	 */
	void registerTertiaryInPositionHandler(CAN_STATUS_MSG_HANDLER(*handler),
					       void* arg1, unsigned arg2);
	
	/**
	 * Convert from integer state to bit state
	 */
	unsigned char tertStateToBit(unsigned char);

      private:
	
	sza::antenna::control::AntennaRx* parent_;
	
	/**
	 * Maintain information about the current tertiary position
	 * code
	 */
	unsigned char posnCode_;

	/**
	 * The encoder index for the 30GHz position
	 */
	unsigned short rx30GHzIndex_;

	/**
	 * The encoder index for the 90GHz position
	 */
	unsigned short rx90GHzIndex_;

	/**
	 * The last sequence number received from the control program
	 */
	long lastReq_;
	
	/**
	 * The last sequence number acknowledged
	 */
	long lastAck_;
	
	/**
	 * Register completion of a sequence-number marked transaction
	 */
	void registerDone();
	
	//------------------------------------------------------------
	// Blanking-frame (half-second) monitor members.
	//------------------------------------------------------------
	
	/**
	 * Return a map of half-second monitor points.
	 */ 
	std::map<carma::canbus::msgType, std::string> 
	  getHalfSecMonitors() const;
	
	// Methods to deal with monitor packets for this device.
	
	/**
	 * Process monitor packet 1.
	 */
	void processBlankingFrameMonitor1(std::vector<carma::canbus::byteType>& 
					  data, bool isSim);
	
	/**
	 * Process monitor packet 2.
	 */
	void processBlankingFrameMonitor2(std::vector<carma::canbus::byteType>& 
					  data, bool isSim);
	
	/**
	 * Process monitor packet 3.
	 */
	void processBlankingFrameMonitor3(std::vector<carma::canbus::byteType>& 
					  data, bool isSim);
	
	/**
	 * Process monitor packet 4.
	 */
	void processBlankingFrameMonitor4(std::vector<carma::canbus::byteType>& 
					  data, bool isSim);
	
	//------------------------------------------------------------
	// Status messages
	//------------------------------------------------------------
	
	enum statusMessages {
	  STATUS_MSG_1  = 0x130, // Calibrator in position
	  STATUS_MSG_2  = 0x131, // Mirror in position
	};
	
	/**
	 * Return a map of status messages
	 */
	virtual std::map<carma::canbus::msgType, std::string> 
	  getStatusMessages() const;
	
	// Methods for processing status messages
	
	void processStatusMessage1(bool isSim);
	void processStatusMessage2(bool isSim);
	
	//------------------------------------------------------------
	// Commands for this device.
	//------------------------------------------------------------
	
	/**
	 * Engineering commands specific to the Caltert
	 */
	enum engineeringCommands 
	  {
	    ENGCMD_1WIRE_INTERFACE = 0x080,
	    ENGCMD_POS_CAL         = 0x081,
	    ENGCMD_HOME_TERT       = 0x082,
	    ENGCMD_POS_TERT        = 0x083,
	    ENGCMD_ENABLE_TERT     = 0x084,
	    ENGCMD_RESET_STEPPER   = 0x085,
	    ENGCMD_IND_CURR_ENC_POS= 0x086,
	    ENGCMD_SET_ENC_POS     = 0x087,
	  };
	
	// Utility conversion
	
	MirPos rxIdToMirPos(sza::util::Rx::Id rxId);
	
	// Check the tertiary position, and determine whether or not
	// it is an error, given the current receiver id

	void checkTertPos();

      }; // End class CalTertOld
      
    } // End namespace canbus
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CANBUS_CALTERTOLD_H
