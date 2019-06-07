#ifndef SZA_ANTENNA_CANBUS_THERMAL_H
#define SZA_ANTENNA_CANBUS_THERMAL_H

/**
 * @file Thermal.h
 * 
 * Tagged: Wed Sep  1 21:46:06 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/canbus/CanDevice.h"
#include "carma/szautil/Thermal.h"

namespace sza {
  namespace antenna {
    namespace canbus {
      
      class Thermal : public CanDevice {
      public:
	
	/**
	 * Constructor.
	 */
	Thermal(sza::antenna::control::SzaShare* share, 
		       std::string boardName,
		       carma::canbus::nodeType node, 
		       carma::canbus::CanOutput& io);

	/**
	 * Destructor.
	 */
	virtual ~Thermal();
	
	/**
	 * Set the temperature. (C)
	 */
	std::vector<carma::canbus::Message>
	  setTemperature(sza::util::Thermal::Target target, float celsius, bool send=true);

	/**
	 * Set the mode
	 */
	std::vector<carma::canbus::Message>
	  setMode(sza::util::Thermal::Target target, 
		  sza::util::Thermal::BoxMode mode, bool send=true);

	/**
	 * Set the loop gain
	 */
	std::vector<carma::canbus::Message>
	  setLoopGain(sza::util::Thermal::Target target, float gain, bool send=true);

	/**
	 * Set the integration constant
	 */
	std::vector<carma::canbus::Message>
	  setIntegrationConstant(sza::util::Thermal::Target target, float constant, bool send=true);

	/**
	 * Set the loop bw
	 */
	std::vector<carma::canbus::Message>
	  setLoopBandWidth(sza::util::Thermal::Target target, float bw, bool send=true);

	/**
	 * Set the rate constant
	 */
	std::vector<carma::canbus::Message>
	  setRateConstant(sza::util::Thermal::Target target, float rate, bool send=true);

	/**
	 * Set a voltage offset
	 */
	std::vector<carma::canbus::Message>
	  setVoltageOffset(float voltageOffset, bool send=true);

	/**
	 * Set the ebox equilibrium state
	 */
	std::vector<carma::canbus::Message>
	  setEboxEqState(bool eqStateOn, bool send=true);

	/**
	 * Set the ebox integral error
	 */
	std::vector<carma::canbus::Message>
	  setEboxIntError(float value, bool send=true);

	/**
	 * Commit last entered values to EEPROM
	 */
	std::vector<carma::canbus::Message>
	  setCirculationFanPropConst(float rate, bool send=true);

      private:

	//------------------------------------------------------------
	// Blanking-frame (half-second) monitor members.
	//------------------------------------------------------------
	
	/**
	 * Define the message ids (directly from the API) and group
	 * according to half sec monitors/ slow monitors / control.
	 */
	enum halfSecMonitors {
	  MONITOR_PACKET_1    = 0x0E0,
	  MONITOR_PACKET_2    = 0x0E1,
	  MONITOR_PACKET_3    = 0x0E2,
	  MONITOR_PACKET_4    = 0x0E3, 
	  MONITOR_PACKET_5    = 0x0E4, 
	  MONITOR_PACKET_6    = 0x0E5, 
	  MONITOR_PACKET_7    = 0x0E6, 
	  MONITOR_PACKET_8    = 0x0E7, 
	};

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
	void processBlankingFrameMonitor6(std::vector<carma::canbus::byteType>& data, bool isSim);
	void processBlankingFrameMonitor7(std::vector<carma::canbus::byteType>& data, bool isSim);
	void processBlankingFrameMonitor8(std::vector<carma::canbus::byteType>& data, bool isSim);

	//------------------------------------------------------------
	// Commands for this device.
	//------------------------------------------------------------
	
	/**
	 * Engineering commands specific to the interface module
	 */
	enum engineeringCommand
	  {
	    ENGCMD_SET_RBOX_TEMP        = 0x080,

	    ENGCMD_SET_RBOX_MODE        = 0x081,
	    ENGCMD_SET_RBOX_LOOP_GAIN   = 0x082,
	    ENGCMD_SET_RBOX_INTEG_CONST = 0x083,
	    ENGCMD_SET_RBOX_LOOP_BW     = 0x084,
	    ENGCMD_SET_RBOX_RATE_CONST  = 0x085,

	    ENGCMD_SET_EBOX_TEMP        = 0x086,

	    ENGCMD_SET_EBOX_MODE        = 0x087,
	    ENGCMD_SET_EBOX_LOOP_GAIN   = 0x088,
	    ENGCMD_SET_EBOX_INTEG_CONST = 0x089,
	    ENGCMD_SET_EBOX_LOOP_BW     = 0x08A,
	    ENGCMD_SET_EBOX_RATE_CONST  = 0x08B,
	    ENGCMD_SET_CIRC_PROP_CONST  = 0x08C,
	    ENGCMD_SET_VOLTAGE_OFFSET   = 0x08D,
	    ENGCMD_SET_EBOX_EQ_STATE    = 0x08E,
	    ENGCMD_SET_EBOX_INT_ERROR   = 0x08F
	  };

      }; // End class Thermal
      
    } // End namespace canbus
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CANBUS_THERMAL_H
