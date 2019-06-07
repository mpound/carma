/** @file
 * CAN Device implementation for CARMA CANbus API No. 040 - Tiltmeter 
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.2 $
 * $Date: 2011/01/03 18:48:05 $
 * $Id: Tiltmeter.h,v 1.2 2011/01/03 18:48:05 iws Exp $
 */

#ifndef CARMA_ANTENNA_OVRO_TILTMETER_H
#define CARMA_ANTENNA_OVRO_TILTMETER_H

#include "carma/canbus/devices/XacDevice.h"
#include "carma/canbus/Types.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/services/Angle.h"

#include <memory>

namespace log4cpp {
    // Forward dec
    class Category;
} // End namespace log4cpp

namespace carma {
namespace antenna {
namespace ovro {

/**
 * Tiltmeter device class.
 * This class implements API No. 040 for the OVRO Tiltmeter CAN module.
 */
class Tiltmeter : public carma::canbus::devices::XacDevice {
public:
    
    /**
     * Constructor
     * @param node Location id of this instance (node location id).
     * @param io Reference to CanOutput class.
     * @param antNo OVRO (10-m) Antenna number [1..6].
     */
    Tiltmeter(
        carma::canbus::nodeType node, 
        carma::canbus::CanOutput &io,
        carma::monitor::OvroSubsystem & mon,
        unsigned short antNo );

    /**
     * Destructor
     */
    virtual ~Tiltmeter();

    /**
     * Get most recent aft-forward tilt measurement.
     * Note that this is the raw tilt value - no tilt zero has been applied.
     * @return Most recent aft-forward tilt.
     */
    carma::services::Angle getMostRecentAftForwardTilt( ) const;

    /**
     * Get most recent left-right tilt measurement.
     * Note that this is the raw tilt value - no tilt zero has been applied.
     * @return Most recent left-right tilt measurement.
     */
    carma::services::Angle getMostRecentLeftRightTilt( ) const;
    
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

    struct Shared;

    // Member variables
    unsigned short antennaNo_;
    log4cpp::Category &log_;  // Reference to the system logger
    carma::monitor::TiltmeterModule &mon_;
    ::std::auto_ptr< Shared > shared_;


}; // End class Tiltmeter
}}} // End namespace carma::antenna::ovro
#endif

