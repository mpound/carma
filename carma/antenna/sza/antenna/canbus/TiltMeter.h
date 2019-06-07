#ifndef SZA_ANTENNA_CANBUS_TILTMETER_H
#define SZA_ANTENNA_CANBUS_TILTMETER_H

/**
 * @file TiltMeter.h
 * 
 * Tagged: Fri Apr 13 21:01:53 PDT 2007
 * 
 * @author SZA data acquisition
 */
#include "carma/antenna/sza/antenna/canbus/CanDevice.h"

#include "carma/antenna/sza/antenna/control/SzaShare.h"

#include "carma/szautil/Temperature.h"

#include "carma/szaarrayutils/rtcnetcoms.h"

namespace sza {
  namespace antenna {
    namespace canbus {
      
      class TiltMeter : public CanDevice {
      public:
	
	/**
	 * Constructor.
	 */
	TiltMeter(sza::antenna::control::SzaShare* share, 
		  std::string boardName,
		  carma::canbus::nodeType node, 
		  carma::canbus::CanOutput& io);
	
	/**
	 * Destructor.
	 */
	virtual ~TiltMeter();
	
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

	//------------------------------------------------------------
	// Commands for this device.
	//------------------------------------------------------------
	
	enum OpMode {
	  ON,
	  OFF,
	  MANUAL
	};

	/**
	 * Commands from the host.
	 */
	enum hostCommands 
	  {
	    HOSTCMD_SET_TEMP            = 0x080, // Set the desired temp of the tiltmeter
	    HOSTCMD_REGULATE_TEMP       = 0x081, // Regulate the temperature
	    HOSTCMD_SET_LOOP_GAIN       = 0x082, // Set the loop gain
	    HOSTCMD_SET_LOOP_INT_CONST  = 0x083, // Set the loop integration constant
	    HOSTCMD_SET_LOOP_RATE_CONST = 0x084, // Set the loop rate constant
	    HOSTCMD_SET_LOOP_BW         = 0x085, // Set the loop bandwidth
	    HOSTCMD_WRITE_EEPROM        = 0x086, // Write loop parameters to EEPROM
	  };
	
	sza::util::Angle afZero_;
	sza::util::Angle lrZero_;

	/**
	 * Set the desired operating temperature
	 */
	std::vector<carma::canbus::Message>
	  setTemperature(sza::util::Temperature temp, bool send=true);

	/**
	 * Set the Gunn operating voltage.
	 */
	std::vector<carma::canbus::Message>
	  regulateTemperature(sza::array::TiltmeterMode opmode, float pwrFract, bool send=true);

	/**
	 * Set the loop gain
	 */
	std::vector<carma::canbus::Message>
	  setLoopGain(float gain, bool send=true);

	/** 
	 * Set the loop integration constant
	 */
	std::vector<carma::canbus::Message>
	  setLoopIntegrationConstant(float constant, bool send=true);

	/** 
	 * Set the loop rate constant
	 */
	std::vector<carma::canbus::Message>
	  setLoopRateConstant(float constant, bool send=true);

	/** 
	 * Set the loop banadwidth
	 */
	std::vector<carma::canbus::Message>
	  setLoopBandwidth(float bw, bool send=true);

	/** 
	 * Set the position for the requested device
	 */
	std::vector<carma::canbus::Message>
	  writeToEeprom(bool send=true);

	/**
	 * Set the tiltmeter zeros
	 */
	void setZeros(sza::util::Angle& afZero, sza::util::Angle& lrZero);

      }; // End class TiltMeter
      
    } // End namespace canbus
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CANBUS_TILTMETER_H
