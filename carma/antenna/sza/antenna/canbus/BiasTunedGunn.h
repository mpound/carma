#ifndef BIASTUNEDGUNN_H
#define BIASTUNEDGUNN_H

#include "carma/antenna/sza/antenna/canbus/CanDevice.h"
#include "carma/antenna/sza/antenna/canbus/Oscillator.h"

#define ZABER_MAX_DEV 255

/**
 * @file BiasTunedGunn.h
 * 
 * Started: Fri Nov 21 19:16:50 UTC 2003
 * 
 * @author Erik Leitch
 */
#include <string>

namespace sza {
  namespace antenna {
    namespace canbus {
      
      class BiasTunedGunn : public Oscillator {
      public:
	
	enum {
	  GUNN      = 0x1,
	  SWEEP     = 0x2,
	  IFMON     = 0x4,

	  TUNER     = 0x8,
	  BACKSHORT = 0x10,
	  ATTEN     = 0x20,

	  ALL       = TUNER | BACKSHORT | ATTEN
	};

	/**
	 * Constructor.
	 */
	BiasTunedGunn(sza::antenna::control::SzaShare* share, 
		      std::string name,
		      carma::canbus::nodeType node, 
		      carma::canbus::CanOutput& io);
	
	//------------------------------------------------------------
	// Blanking-frame monitor members.
	//------------------------------------------------------------
	
	/**
	 * Return a map of half-second monitor points.
	 */ 
	std::map<carma::canbus::msgType, std::string> 
	  getHalfSecMonitors() const;
	
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
	
	/**
	 * Process monitor packet 6.
	 */
	void processBlankingFrameMonitor6(std::vector<carma::canbus::byteType>& data, bool isSim);

	/**
	 * Process monitor packet 7.
	 */
	void processBlankingFrameMonitor7(std::vector<carma::canbus::byteType>& data, bool isSim);

	/**
	 * Process monitor packet 8
	 */
	void processBlankingFrameMonitor8(std::vector<carma::canbus::byteType>& data, bool isSim);
	
	/**
	 * Process monitor packet 9
	 */
	void processBlankingFrameMonitor9(std::vector<carma::canbus::byteType>& data, bool isSim);

	//------------------------------------------------------------
	// Commands for this device.
	//------------------------------------------------------------
	
	/**
	 * Commands from the host.
	 */
	enum hostCommands 
	  {
	    HOSTCMD_SET_LO_FREQ      = 0x040, // Set the LO frequency 
	    HOSTCMD_SET_GUNN_VOLTAGE = 0x080, // Set the Gunn operating voltage
	    HOSTCMD_SET_LOOP_GAIN    = 0x081, // Set the loop gain for the phase-lock loop.

	    HOSTCMD_SET_GUNN_STATE   = 0x082, // Turn Gunn on or off
	    HOSTCMD_SET_SWEEP_STATE  = 0x083, // Turn sweep on or off
	    HOSTCMD_SET_IFMON_STATE  = 0x084, // Turn phase-lock IF monitor output on or off

	    HOSTCMD_SET_TUNER_POS    = 0x085, // Set the tuner position
	    HOSTCMD_SET_BACKSHORT_POS= 0x086, // Move the Gunn backshort to a given position
	    HOSTCMD_SET_ATTEN_POS    = 0x087, // Move the LO attenuator to a given position

	    HOSTCMD_JOG_TUNER        = 0x088, // Move the tuner by a given number of micro-steps
	    HOSTCMD_JOG_BACKSHORT    = 0x089, // Move the backshort by a given number of micro-steps
	    HOSTCMD_JOG_ATTEN        = 0x08A, // Move the attenuator by a given number of micro-steps

	    HOSTCMD_TOGGLE_AUTO_RELOCK=0x08B, // Turns auto-relock on or off
	    HOSTCMD_HOME_ACTUATORS   = 0x08C, // Set the specified actuators to home position
	  };
	
	/**
	 * Set the operating frequency.
	 */
	std::vector<carma::canbus::Message>
	  setLoFrequency(unsigned short frequency, bool send=true);

	/**
	 * Set the Gunn operating voltage.
	 */
	std::vector<carma::canbus::Message>
	  setVoltage(unsigned short voltage, bool send=true);

	/**
	 * Set the loop gain
	 */
	std::vector<carma::canbus::Message>
	  setLoopGainResistance(unsigned short gain, bool send=true);

	/** 
	 * Turn the requested stage on/off
	 */
	std::vector<carma::canbus::Message>
	  setDeviceState(unsigned device, bool state, bool send=true);

	/** 
	 * Set the position for the requested device
	 */
	std::vector<carma::canbus::Message>
	  setDevicePosition(unsigned device, unsigned int position, bool send=true);

	/** 
	 * Jog the position for the requested device
	 */
	std::vector<carma::canbus::Message>
	  jogDevicePosition(unsigned device, int step, bool send=true);

	/** 
	 * Toggle auto relock
	 */
	std::vector<carma::canbus::Message>
	  enableAutoRelock(bool enable, bool send=true);

	/** 
	 * Home actuators
	 */
	std::vector<carma::canbus::Message>
	  homeDevice(unsigned int mask, bool send=true);

	/**
	 * Engineering commands specific to the bias-tuned Gunn
	 */
	enum engineeringCommands 
	  {
	    ENGCMD_SET_GUNN_FREQ        = 0x300,
	    ENGCMD_DL_GUNN_ID           = 0x301,
	    ENGCMD_DL_TUNING_TABLE      = 0x302,
	    ENGCMD_DL_TO_OWIRE          = 0x303,
	    // Note: there is presently no command 0x304 in the API
	    ENGCMD_CONTROL_ZABER        = 0x305,
	    ENGCMD_ZABER_REQUEST        = 0x306,
	  };
	
	/** 
	 * Set the Gunn operating voltage.
	 */
	std::vector<carma::canbus::Message>
	  setFrequency(unsigned short frequency, bool send=true);
	
	/** 
	 * Set the loop gain resistance.
	 */
	std::vector<carma::canbus::Message>
	  downloadId(unsigned short gunnId, unsigned char calMonth, unsigned char calDay,
		     unsigned char calYear,
		     unsigned short calVoltage, unsigned char nRow, bool send=true);
	
	/** 
	 * Download a single entry of the tuning table
	 */
	std::vector<carma::canbus::Message>
	  downloadTuningTableEntry(unsigned int freq, 
				   unsigned int tunerPos, 
				   unsigned int backshortPos, 
				   unsigned int attenPos, 
				   bool send=true);
	/** 
	 * Download the tuning table to the 1-wire device
	 */
	std::vector<carma::canbus::Message>
	  downloadTuningTableToOneWire(bool send=true);
	
	/** 
	 * Control the zaber
	 */
	std::vector<carma::canbus::Message>
	  controlZaber(unsigned short deviceNo,
		       unsigned char zaberCmdCode,
		       std::vector<unsigned char>& zaberData, 
		       bool send);
	
	/** 
	 * Zaber CAN monitor request
	 */
	std::vector<carma::canbus::Message>
	  requestZaberPackets(bool request, bool send=true);
	
	/**
	 * Return a map of controls.
	 */ 
	virtual std::map<carma::canbus::msgType, std::string> 
	  getSpecificControls() const;
	
	/**
	 * Return the prefix to use when
	 * constructing monitor points
	 */
	std::string controlPrefix();
	
	// Check the current lock status, and determine whether or not
	// it is an error, given the current receiver id

	void checkHwLockStatus();

	void setRxId(sza::util::Rx::Id rxId);

	// Store the current hardware lock status

	unsigned char hwLockStatus_;

      }; // End class BiasTunedGunn
      
    }; // End namespace canbus
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 


