#ifndef SZA_ANTENNA_CANBUS_INTMOD_H
#define SZA_ANTENNA_CANBUS_INTMOD_H

/**
 * @file IntMod.h
 * 
 * Tagged: Mon Aug 23 15:05:53 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/canbus/CanDevice.h"

namespace sza {
  namespace antenna {

    namespace control {
      class AntennaRx;
    }

    namespace canbus {
      
      class IntMod : public CanDevice {
      public:
	
	/**
	 * Constructor.
	 */
	IntMod(sza::antenna::control::SzaShare* share, 
	       std::string boardName,
	       carma::canbus::nodeType node, 
	       carma::canbus::CanOutput& io);

	/**
	 * Destructor.
	 */
	virtual ~IntMod();
	
	/**
	 * Set the PAM attenuation
	 */
	std::vector<carma::canbus::Message>
	  setPAMAttenuation(unsigned char atten, bool send=true);

	// Set the output power

	std::vector<carma::canbus::Message>
	  setPAMOutputPower(short power, bool send=true);
	
	// Set the output power to a preset level

	std::vector<carma::canbus::Message>
	  presetPAMOutputPower(bool send=true);

      private:

	//------------------------------------------------------------
	// Blanking-frame (half-second) monitor members.
	//------------------------------------------------------------
	
	/**
	 * Return a map of half-second monitor points.
	 */ 
	std::map<carma::canbus::msgType, std::string> 
	  getHalfSecMonitors() const;

	// Methods to deal with monitor packets for this device.

	void processBlankingFrameMonitor1(std::vector<carma::canbus::byteType>& data, bool isSim);
	void processBlankingFrameMonitor2(std::vector<carma::canbus::byteType>& data, bool isSim);
	void processBlankingFrameMonitor3(std::vector<carma::canbus::byteType>& data, bool isSim);
	void processBlankingFrameMonitor4(std::vector<carma::canbus::byteType>& data, bool isSim);
	void processBlankingFrameMonitor5(std::vector<carma::canbus::byteType>& data, bool isSim);

	//------------------------------------------------------------
	// Commands for this device.
	//------------------------------------------------------------
	
	/**
	 * Engineering commands specific to the interface module
	 */
	enum engineeringCommands 
	  {
	    ENGCMD_SET_PAM_ATTEN    = 0x080,
	    ENGCMD_PRESET_PAM_POWER = 0x081,
	    ENGCMD_SET_PAM_POWER    = 0x082
	  };

      }; // End class IntMod
      
    } // End namespace canbus
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CANBUS_INTMOD_H
