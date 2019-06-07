#ifndef SZA_ANTENNA_CANBUS_IFMOD_H
#define SZA_ANTENNA_CANBUS_IFMOD_H

/**
 * @file IFMod.h
 * 
 * Tagged: Wed Aug 11 12:58:06 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/canbus/CanDevice.h"

#include "carma/szautil/Attenuation.h"
#include "carma/szautil/Rx.h"

#define CARMA_MODULES

namespace sza {
  namespace antenna {

    namespace control {
      class AntennaRx;
    }

    namespace canbus {
      
      class IFMod : public CanDevice {
      public:
	
	/**
	 * Constructor.
	 */
	IFMod(sza::antenna::control::AntennaRx* parent,
		  sza::antenna::control::SzaShare* share, 
		  std::string boardName,
		  carma::canbus::nodeType node, 
		  carma::canbus::CanOutput& io);
	
	/**
	 * Destructor.
	 */
	virtual ~IFMod();
	
	/**
	 * Select a band number
	 */
	std::vector<carma::canbus::Message>
	  selectBand(sza::util::Rx::Id switchPos, bool send=true);

	/**
	 * Set the IF attenutation.
	 *
	 * @param atten 0-63 dB, in steps of 0.5 dB
	 */
	std::vector<carma::canbus::Message>
	  setAtten(float atten, bool send=true);

	std::vector<carma::canbus::Message>
	  setAtten(sza::util::Attenuation atten, bool send=true);

	std::vector<carma::canbus::Message>
	  setAtten(float input, float output, bool send=true);

	/**
	 * Set IF level
	 *
	 * @param level Total power, in mW
	 */
	std::vector<carma::canbus::Message>
	  setLevel(float level, bool send=true);

	/**
	 * Set input/output atten levels 
	 */
	std::vector<carma::canbus::Message>
	  setInputAtten(float inputLevel, bool send=true);

	std::vector<carma::canbus::Message>
	  setOutputAtten(float outputLevel, bool send=true);

	/**
	 * Register the receipt of a control-program command that
	 * needs to be acknowledged when it has taken effect.
	 */
	void registerRequest(unsigned seq);

      private:

	sza::antenna::control::AntennaRx* parent_;

	RegMapBlock* ifTotalPowerReg_;
	RegMapBlock* pamTemperatureReg_;
	RegMapBlock* totalAttenReg_;
	RegMapBlock* pamStatusReg_;
	RegMapBlock* ifSwitchStateReg_;
	RegMapBlock* laserStatusReg_;
	RegMapBlock* laserPowerReg_;
	RegMapBlock* laserRegErrorReg_;
	RegMapBlock* inputAttenReg_;
	RegMapBlock* outputAttenReg_;
	RegMapBlock* laserIdReg_;

	float ifTotalPower_;	  
	float pamTemperature_; 

	float totalAtten_;	  
	unsigned char pamStatus_;	  
	unsigned char ifSwitchState_;  
	unsigned char laserStatus_;	  

	float laserPower_;	  
	float laserRegError_;  

	float inputAtten_;	  
	float outputAtten_;	  

	unsigned char laserId_[8];     

	float lastInputAtten_;
	float lastOutputAtten_;

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
	
#ifndef CARMA_MODULES
	/**
	 * Define the message ids (directly from the API) and group
	 * according to half sec monitors/ slow monitors / control.
	 */
	enum halfSecMonitors {
	  MONITOR_PACKET_1    = 0x110,
	  MONITOR_PACKET_2    = 0x111,
	  MONITOR_PACKET_3    = 0x112,
	  MONITOR_PACKET_4    = 0x113,
	  MONITOR_PACKET_5    = 0x114,
	};
#endif

	/**
	 * Return a map of half-second monitor points.
	 */ 
	std::map<carma::canbus::msgType, std::string> 
	  getHalfSecMonitors() const;

	// Methods to deal with monitor packets for this device.

	/**
	 * Process monitor packet 1.
	 */
	void processBlankingFrameMonitor1(std::vector<carma::canbus::byteType>& data, bool isSim);
	
	/**
	 * Process monitor packet 2.
	 */
	void processBlankingFrameMonitor2(std::vector<carma::canbus::byteType>& data, bool isSim);
	
	/**
	 * Process monitor packet 3.
	 */
	void processBlankingFrameMonitor3(std::vector<carma::canbus::byteType>& data, bool isSim);
	
	/**
	 * Process monitor packet 4.
	 */
	void processBlankingFrameMonitor4(std::vector<carma::canbus::byteType>& data, bool isSim);


	/**
	 * Process monitor packet 5.
	 */
	void processBlankingFrameMonitor5(std::vector<carma::canbus::byteType>& data, bool isSim);

	//------------------------------------------------------------
	// Status messages
	//------------------------------------------------------------

	enum statusMessages {
	  STATUS_MSG_1  = 0x130, // PAM status
	  STATUS_MSG_2  = 0x131, // switch status
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
	 * Engineering commands specific to the IF module
	 */
#ifdef CARMA_MODULES
	enum engineeringCommands 
	  {
	    ENGCMD_SELECT_BAND         = 0x080,
	    ENGCMD_SET_IF_TOTAL_ATTEN  = 0x081,
	    ENGCMD_SET_IF_LEVEL        = 0x082,
	    ENGCMD_SET_IF_INPUT_ATTEN  = 0x083,
	    ENGCMD_SET_IF_OUTPUT_ATTEN = 0x084,

	    ENGCMD_SET_IF_INOUT_ATTEN  = 0x103,
	    ENGCMD_QUERY_ATTEN_VS_FREQ = 0x105
	  };
#else
	enum engineeringCommands 
	  {
	    ENGCMD_SELECT_BAND         = 0x100,
	    ENGCMD_SET_IF_TOTAL_ATTEN  = 0x101,
	    ENGCMD_SET_IF_LEVEL        = 0x102,
	    ENGCMD_SET_IF_INOUT_ATTEN  = 0x103,
	    ENGCMD_QUERY_ATTEN_VS_FREQ = 0x105
	  };
#endif

      }; // End class IFMod
      
    } // End namespace canbus
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CANBUS_IFMOD_H
