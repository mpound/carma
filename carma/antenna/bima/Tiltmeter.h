/** @file
 * CAN Device implementation for CARMA CANbus API No. 040 - Tiltmeter 
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.2 $
 * $Date: 2011/01/03 18:48:03 $
 * $Id: Tiltmeter.h,v 1.2 2011/01/03 18:48:03 iws Exp $
 */

#ifndef CARMA_ANTENNA_BIMA_TILTMETER_H
#define CARMA_ANTENNA_BIMA_TILTMETER_H

// Carma includes
#include "carma/canbus/devices/XacDevice.h"
#include "carma/canbus/Types.h"
#include "carma/monitor/BimaSubsystem.h"

namespace log4cpp {
    // Forward dec
    class Category;
} // End namespace log4cpp

namespace carma
{
  namespace antenna
  {
    namespace ovro
    {

      /**
       * Tiltmeter device class.
       * This class implements API No. 040 for the OVRO Tiltmeter CAN module.
       */
      class Tiltmeter : public carma::canbus::devices::XacDevice
      {
	public:

	  /**
	   * Constructor
	   * @param node Location id of this instance (node location id).
	   * @param io Reference to CanOutput class.
	   * @param antNo BIMA (6-m) Antenna number [7..15].
	   */
	  Tiltmeter(
	      carma::canbus::nodeType node, 
	      carma::canbus::CanOutput &io,
	      carma::monitor::BimaSubsystem &bmon
	     );

	  /**
	   * Destructor
	   */
	  virtual ~Tiltmeter();

	  /**
	   * Retrieve a map of this devices half second monitor points.
	   * The monitor points returned from this routine will be 
	   * simulated if the device is in the OFFLINE state.
	   * @return a map of the devices half second monitor points string
	   * descriptions keyed by message id.
	   */
	  std::map<carma::canbus::msgType, std::string> getHalfSecMonitors() const;

	  /**
	   * Return a map of this devices slow monitor points.
	   * These monitor points will be simulated every 5 seconds if 
	   * the device is in the OFFLINE state.
	   * @return a map of the devices slow (5 sec) monitor points string
	   * descriptions keyed by message id.
	   */
	  std::map<carma::canbus::msgType, std::string> getSlowMonitors() const;

	  /**
	   * Process a CAN message addressed to the CryoCompressor module.
	   * This routine is responsible for processing all CAN messages 
	   * addressed to this device.
	   * @param mid the 10bit message id (carma::canbus::msgType)
	   * @param data reference to the byte vector containing the raw data.
	   * @param sim Indicates if message is simulate or real.
	   * @see carma::canbus::Device::processMsg
	   */
	  void processMsg(carma::canbus::msgType mid, 
	      std::vector<carma::canbus::byteType> &data, 
	      bool sim);

	  /**
	   * Produce a simulated CAN message for a given msgType.
	   * This routine creates a Message with simulated data for an  
	   * input message id.  The returned message is automatically 
	   * placed in the CAN message queue for retrieval and processing 
	   * by the Master class.  It thus can be used to test the processMsg 
	   * method above. 
	   * @param mid the 10bit message id (carma::canbus::msgType)
	   */
	  carma::canbus::Message simulateMsg(carma::canbus::msgType mid);

	  /**
	   * Update Frame Data. 
	   */
	  void updateFrameData(); 

	  // Public Control Commands.
	  /**
	   * Set temperature of the tiltmeter.
	   * @param temp Temperature in C.
	   */
	  void setTemperature(float temp);

	  /**
	   * Enumeration for Thermal control operation mode.
	   */
	  enum OpMode {LOOP_ON, LOOP_OFF, MANUAL};

	  /**
	   * Regulate temperature.
	   * @param opMode Operation mode of thermal control loop. 
	   * @param pwrFract Fraction of maximum power - only used if opMode = MANUAL).
	   */
	  void regulateTemperature(OpMode opMode, float pwrFract);

	  /**
	   * Set loop gain.
	   * Set a new value for the loop gain. 
	   * @param gain Loop gain in (percent max pwr)/K.
	   * @see writeLoopParametersToEEPROM
	   */
	  void setLoopGain(float gain);

	  /**
	   * Set loop integration constant.
	   * @param loopInteg Loop integration constant in (percent max pwr)/K.
	   * @see writeLoopParametersToEEPROM
	   */
	  void setLoopIntegrationConstant(float loopInteg);

	  /**
	   * Set loop rate constant.
	   * @param rateConst Loop derivative gain in (percent max. pwr)/K.
	   * @see writeLoopParametersToEEPROM
	   */
	  void setLoopRateConstant(float rateConst);

	  /**
	   * Set loop bandwidth.
	   * @param bw Loop BW (inverse of sample and correction interval) in Hz.
	   * @see writeLoopParametersToEEPROM.
	   */
	  void setLoopBandwidth(float bw);

	  /**
	   * Write loop parameters to EEPROM.
	   */
	  void writeLoopParametersToEEPROM();

	private:

	  // API Id for this device.
	  static const carma::canbus::apiType API_ID                    = 40;

	  // API version this class was implemented from 
	  static const char API_VERSION                                 = 'E';

	  // Late packet timeout in ms
	  static const double PACKET_LATE_THRESHOLD                     = 150.0;

	  // Control command message ids.
	  static const carma::canbus::msgType SET_TEMPERATURE           = 0x080;
	  static const carma::canbus::msgType REGULATE_TEMPERATURE      = 0x081;
	  static const carma::canbus::msgType SET_LOOP_GAIN             = 0x082;
	  static const carma::canbus::msgType SET_LOOP_INT_CONSTANT     = 0x083; 
	  static const carma::canbus::msgType SET_LOOP_RATE_CONSTANT    = 0x084;
	  static const carma::canbus::msgType SET_LOOP_BANDWIDTH        = 0x085;
	  static const carma::canbus::msgType WRITE_LOOP_PARAMS         = 0x086;

	  // Blanking frame message ids.
	  static const carma::canbus::msgType BLANKING_FRAME_PACKET_1   = 0x0E0;
	  static const carma::canbus::msgType BLANKING_FRAME_PACKET_2   = 0x0E1;
	  static const carma::canbus::msgType BLANKING_FRAME_PACKET_3   = 0x0E2;
	  static const carma::canbus::msgType BLANKING_FRAME_PACKET_4   = 0x0E3;
	  static const carma::canbus::msgType BLANKING_FRAME_PACKET_5   = 0x0E4;
	  static const carma::canbus::msgType BLANKING_FRAME_PACKET_6   = 0x0E5;

	  // Disallow assignment and copy construction.
	  Tiltmeter(const Tiltmeter &);
	  Tiltmeter &operator=(const Tiltmeter &);

	  // Routines to process individual blanking frame CAN packets.
	  // These routines are called by processMsg.
	  void processBlankingFramePacket1(
	      std::vector<carma::canbus::byteType> &data);
	  void processBlankingFramePacket2(
	      std::vector<carma::canbus::byteType> &data);
	  void processBlankingFramePacket3(
	      std::vector<carma::canbus::byteType> &data);
	  void processBlankingFramePacket4(
	      std::vector<carma::canbus::byteType> &data);
	  void processBlankingFramePacket5(
	      std::vector<carma::canbus::byteType> &data);
	  void processBlankingFramePacket6(
	      std::vector<carma::canbus::byteType> &data);

	  // Routines to produce individual simulated blanking frame 
	  // CAN packets.  These routines are called by simulateMsg.
	  carma::canbus::Message simBlankingFramePacket1();
	  carma::canbus::Message simBlankingFramePacket2();
	  carma::canbus::Message simBlankingFramePacket3();
	  carma::canbus::Message simBlankingFramePacket4();
	  carma::canbus::Message simBlankingFramePacket5();
	  carma::canbus::Message simBlankingFramePacket6();

	  // Other helper routines

	  // Units in arcminutes.
	  void updateTiltMagAndDir(double lrTilt, double afTilt);

	  // Member variables
	  log4cpp::Category &log_;  // Reference to the system logger
	  carma::monitor::TiltmeterModule &mon_;
	  carma::monitor::BimaSubsystem &inputMon_; // For pointing c's and offsets.

      }; // End class Tiltmeter
    }
  }
} // End namespace carma::antenna::ovro
#endif

