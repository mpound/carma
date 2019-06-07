#ifndef YIG_H
#define YIG_H

#include <string>

#include "carma/antenna/sza/antenna/canbus/Oscillator.h"

/**
 * @file Yig.h
 * 
 * Started: Fri Nov 21 19:16:50 UTC 2003
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace antenna {
    namespace canbus {
      
      class Yig : public Oscillator {
      public:
	
	/*
	 * Lock states from the CAN API
	 */
	enum {
	  UNLOCKED  = 0,
	  SEARCHING = 1,
	  REFINING  = 2,
	  LOCKED    = 3,
	  RELOCK    = 4
	};

	/*
	 * Lock bit states from the CAN API
	 */
	enum {
	  BIT_UNLOCKED  = 0,
	  BIT_LOCKED    = 1,
	};

	/**
	 * Constructor.
	 */
	Yig(sza::antenna::control::SzaShare* share, 
	    std::string boardName,
	    carma::canbus::nodeType node, 
	    carma::canbus::CanOutput& io);
	
	/**
	 * Set the operating frequency.
	 */
	std::vector<carma::canbus::Message>
	  setLoFrequency(unsigned short frequency, bool send=true);
	
	/**
	 * Upload the YIG id
	 */
	std::vector<carma::canbus::Message>
	  downloadId(unsigned char id, unsigned char month,
		     unsigned char day, unsigned char year, bool send=true);

	/**
	 * Send a message to download the tuning table _from_ the one-wire
	 */
	std::vector<carma::canbus::Message>
	  getTuningTable(bool send=true);

	/**
	 * Install a tuning table entry.
	 */
	std::vector<carma::canbus::Message>
	  downloadTuningTableToOneWire(bool send=true);

	/**
	 * Install a tuning table entry.
	 */
	std::vector<carma::canbus::Message>
	  downloadTuningTableEntry(unsigned short voltage, 
				   unsigned short frequency, bool send=true);
	/**
	 * Install a tuning table entry.
	 */
	std::vector<carma::canbus::Message>
	  setOperatingVoltage(unsigned short voltage, bool send=true);
	
	/** 
	 * Set the loop gain resistance
	 */
	std::vector<carma::canbus::Message>
	  setLoopGainResistance(unsigned short voltage, bool send=true);
	
	/** 
	 * Set the damping gain resistance.
	 */
	std::vector<carma::canbus::Message>
	  setDampingResistance(unsigned short resistance, bool send=true);
	
	/** 
	 * Turn the sweep on/off
	 */
	std::vector<carma::canbus::Message>
	  turnSweepOn(bool on, bool send=true);

	/** 
	 * Enable/Disable auto lock
	 */
	std::vector<carma::canbus::Message>
	  enableAutoLock(bool enable, bool send=true);

	/** 
	 * Set DAC calibration coefficient
	 */
	std::vector<carma::canbus::Message>
	  setDACCalibrationCoefficient(float coeff, bool send=true);
	
	/*
	 * Convert from an integral lock state to an orthogonal bit so
	 * that states can be unioned
	 */
	unsigned char lockStateToBit(unsigned char lockState);

	/*
	 * Convert from an integral lock bit to an orthogonal bit so
	 * that bits can be unioned
	 */
	unsigned char lockBitToBit(unsigned char lockBit);

      private:
	
	// Methods to deal with monitor packets for this device.

	/**
	 * Return a map of half-second monitor points.
	 */ 
	std::map<carma::canbus::msgType, std::string> 
	  getHalfSecMonitors() const;
	
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

	/**
	 * Process monitor packet 6.
	 */
	void processBlankingFrameMonitor6(std::vector<carma::canbus::byteType>& 
					  data, bool isSim);

	//------------------------------------------------------------
	// Status messages
	//------------------------------------------------------------

	enum statusMessages {
	  STATUS_MSG_1  = 0x130, // ??
	};

	/**
	 * Return a map of status messages
	 */
	virtual std::map<carma::canbus::msgType, std::string> 
	  getStatusMessages() const;

	// Methods for processing status messages

	void processStatusMessage1(bool isSim);

	//------------------------------------------------------------
	// Commands for this device.
	//------------------------------------------------------------
	
	/**
	 * Commands from the host.
	 */
	enum hostCommands 
	  {
	    HOSTCMD_TEST_MODE     = 0x00F,   // Set firmware test flags
	    HOSTCMD_SET_FREQUENCY = 0x040,   // Set the output frequency
					     // and start the lock
					     // sequence.
	    HOSTCMD_GET_TUNING_TABLE = 0x041 // Get the tuning table
					     // from the one-wire
					     // device
	  };
	
	/**
	 * Engineering commands specific to the YIG
	 */
	enum engineeringCommands 
	  {
	    ENGCMD_SET_OUTPUT_FREQ      = 0x300,
	    ENGCMD_UPLOAD_YIGID         = 0x301,
	    ENGCMD_DOWNLOAD_TUNING      = 0x302,
	    ENGCMD_DOWNLOAD_TO_ONEWIRE  = 0x303,
	    ENGCMD_OPERATING_VOLTAGE    = 0x304,
	    ENGCMD_LOOP_GAIN_RESISTANCE = 0x305,
	    ENGCMD_DAMPING_RESISTANCE   = 0x306,
	    ENGCMD_SWEEP                = 0x307,
	    ENGCMD_AUTOLOCK             = 0x308,
	    ENGCMD_SET_DAC_COEFF        = 0x309,
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
	
      }; // End class Yig
      
    }; // End namespace canbus
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 


