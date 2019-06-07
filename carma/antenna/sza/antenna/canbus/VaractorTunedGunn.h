#ifndef VARACTORTUNEDGUNN_H
#define VARACTORTUNEDGUNN_H

#include "carma/antenna/sza/antenna/canbus/Oscillator.h"

/**
 * @file VaractorTunedGunn.h
 * 
 * Started: Fri Nov 21 19:16:50 UTC 2003
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace antenna {
    namespace canbus {
      
      class VaractorTunedGunn : public Oscillator {
      public:
	
	/**
	 * Enumerate lock states as returned from the module
	 */
	enum {
	  LOCKED    = 0x1,
	  RF_GOOD   = 0x2,
	  SWEEP_ON  = 0x4,
	  GUNN_ON   = 0x8,
	};

	/**
	 * Constructor.
	 */
	VaractorTunedGunn(sza::antenna::control::SzaShare* share, 
			  std::string boardName,
			  carma::canbus::nodeType node, 
			  carma::canbus::CanOutput& io);
	
	/** 
	 * Turn the Gunn on/off
	 */
	std::vector<carma::canbus::Message>
	  turnGunnOn(bool on, bool send=true);
	
	/** 
	 * Turn the sweep on/off
	 */
	std::vector<carma::canbus::Message>
	  turnSweepOn(bool on, bool send=true);
	
	/** 
	 * Turn the monitor on/off
	 */
	std::vector<carma::canbus::Message>
	  turnMonitorOn(bool on, bool send=true);
	
	/** 
	 * Set the loop gain resistance
	 */
	std::vector<carma::canbus::Message>
	  setLoopGainResistance(unsigned short voltage, bool send=true);
	
	/*
	 * Convert from an integral lock state to an orthogonal bit so
	 * that states can be unioned
	 */
	unsigned char statusToBit(unsigned char lockState);
	unsigned char statusToBit(unsigned char lockStatus, unsigned char refStatus, unsigned char sweepStatus, unsigned char gunnStatus);
	unsigned char validityToBit(unsigned char ifmonStatus, unsigned char dataValid);

	enum halfSecMonitors {
	  VAR_MONITOR_PACKET_1 = 0x110,
	  VAR_MONITOR_PACKET_2 = 0x111,
	  VAR_MONITOR_PACKET_3 = 0x112,
	  VAR_MONITOR_PACKET_4 = 0x113,
	};

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
	
	//------------------------------------------------------------
	// Commands for this device.
	//------------------------------------------------------------
	
	/**
	 * Commands from the host.
	 */
	enum hostCommands 
	  {
	    HOSTCMD_GUNN                 = 0x080,
	    HOSTCMD_SWEEP                = 0x081,
	    HOSTCMD_MONITOR              = 0x082,
	    HOSTCMD_LOOP_GAIN_RESISTANCE = 0x083,
	  };
	
	/**
	 * Engineering commands specific to the Varactor tuned Gunn
	 *
	 * Note: these are no longer valid engineering commands, as of
	 * the new 2009 API (EML)
	 */
	enum engineeringCommands 
	  {
	    ENGCMD_GUNN                 = 0x300,
	    ENGCMD_SWEEP                = 0x301,
	    ENGCMD_MONITOR              = 0x302,
	    ENGCMD_LOOP_GAIN_RESISTANCE = 0x303,
	  };
	
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

      }; // End class VaractorTunedGunn
      
    }; // End namespace canbus
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 


