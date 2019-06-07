#ifndef SZA_ANTENNA_CANBUS_CALTERTNEW_H
#define SZA_ANTENNA_CANBUS_CALTERTNEW_H

/**
 * @file CalTertNew.h
 * 
 * Tagged: Thu Jun 17 20:58:48 UTC 2004
 * 
 * @author 
 */
#include "carma/szautil/Angle.h"

#include "carma/antenna/sza/antenna/canbus/CanDevice.h"

#include "carma/antenna/sza/antenna/canbus/CalTert.h"

namespace sza {
  namespace antenna {
    
    namespace control {
      class AntennaRx;
    }
    
    namespace canbus {
      
      class CalTertNew : public CalTert {
      public:
	
	// Enumerate known mirror positions
	
	enum MirPos {
	  RX30GHZ  = 0,
	  RX90GHZ  = 1,
	  RX230GHZ = 2,
	  RX1CM    = RX30GHZ,
	  RX3MM    = RX90GHZ,
	  RX1MM    = RX230GHZ
	};
	
	// Enumerate known tertiary states

	enum TertState {
	  TS_IDLE            = 0,
	  TS_HOMING          = 1,
	  TS_HOME            = 2,
	  TS_HOME_ERROR      = 3,
	  TS_MOVING          = 4,
	  TS_RX1CM_SELECTED  = 5,
	  TS_RX3MM_SELECTED  = 6,
	  TS_RX1MM_SELECTED  = 7,
	  TS_MANUAL_POSITION = 8,
	  TS_STUCK           = 9,
	};

	enum CalState {
	  CS_IDLE              = 0,
	  CS_MOVING            = 1,
	  CS_SKY               = 2,
	  CS_AMBIENT           = 3,
	  CS_WAIT_FOR_TERTIARY = 4,	  
	  CS_STUCK             = 5,
	};

	/**
	 * Constructor.
	 */
	CalTertNew(sza::antenna::control::AntennaRx* parent,
		sza::antenna::control::SzaShare* share, 
		std::string boardName,
		carma::canbus::nodeType node, 
		carma::canbus::CanOutput& io);
	
	/**
	 * Destructor.
	 */
	virtual ~CalTertNew();
	
	/**
	 * Request a calibrator position
	 */
	std::vector<carma::canbus::Message>
	  positionCalibrator(sza::util::CalPos::Pos position, unsigned seq=0, bool send=true);
	
	/**
	 * Select a receiver
	 */
	std::vector<carma::canbus::Message>
	  positionTertiary(sza::util::Rx::Id id, unsigned seq=0, bool send=true);
	
	/**
	 * Move the tertiary
	 */
	std::vector<carma::canbus::Message>
	  positionTertiary(sza::util::Angle position, unsigned seq=0, bool send=true);
	
	/**
	 * Home the tertiary
	 */
	std::vector<carma::canbus::Message>
	  homeTertiary(bool send=true);
	
	/**
	 * Register a handler to be called when the tertiary reports
	 * an in-position message
	 */
	void registerTertiaryInPositionHandler(CAN_STATUS_MSG_HANDLER(*handler),
					       void* arg1, unsigned arg2);
	
	static TertState rxIdToTertState(sza::util::Rx::Id rxId);

      private:
	
	CalState expectedCalState_;
	sza::util::Mutex calStateGuard_;
	unsigned lastReqCalStateSeq_;
	unsigned lastAckCalStateSeq_;

	TertState expectedTertState_;
	sza::util::Mutex tertStateGuard_;
	unsigned lastReqTertStateSeq_;
	unsigned lastAckTertStateSeq_;

	short expectedTertPos_;
	sza::util::Mutex tertPosGuard_;
	unsigned lastReqTertPosSeq_;
	unsigned lastAckTertPosSeq_;

	sza::antenna::control::AntennaRx* parent_;
	
	/**
	 * Maintain information about the current tertiary position
	 * code
	 */
	unsigned char tertState_;

	/**
	 * Register completion of a sequence-number marked transaction
	 */
	void registerDone(unsigned& req, unsigned& ack);
	
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
	
	/**
	 * Process monitor packet 5.
	 */
	void processBlankingFrameMonitor5(std::vector<carma::canbus::byteType>& 
					  data, bool isSim);
	
	//------------------------------------------------------------
	// Commands for this device.
	//------------------------------------------------------------
	
	/**
	 * Engineering commands specific to the Caltert
	 */
	enum engineeringCommands 
	  {
	    ENGCMD_POS_CAL         = 0x080,
	    ENGCMD_SELECT_RX       = 0x081,
	    ENGCMD_POS_TERT        = 0x082,
	    ENGCMD_HOME_TERT       = 0x083
	  };
	
	// Utility conversion
	
	MirPos    rxIdToMirPos(sza::util::Rx::Id rxId);

	// Check the tertiary position, and determine whether or not
	// it is an error, given the current receiver id

	void checkTertPos();

      }; // End class CalTertNew
      
    } // End namespace canbus
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CANBUS_CALTERTNEW_H
