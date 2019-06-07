/** @file
 * Declaration of 10-m Electronics Temperature Controller class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.2 $
 * $Date: 2007/10/12 05:50:12 $
 * $Id: RxTemperatures.h,v 1.2 2007/10/12 05:50:12 abeard Exp $
 *
 * $CarmaCopyright$
 */
#ifndef CARMA_ANTENNA_OVRO_RXTEMPERATURES_H
#define CARMA_ANTENNA_OVRO_RXTEMPERATURES_H

// Carma includes
#include "carma/canbus/devices/XacDevice.h"
#include "carma/monitor/OvroSubsystem.h"

// STL includes
#include <map>
#include <vector>

namespace log4cpp {
    // Forward dec
    class Category;
} // End namespace log4cpp

namespace carma {
namespace antenna {
namespace ovro {

class RxTemperatures : public carma::canbus::devices::XacDevice {
public:

    /**
     * Constructor
     * @param node Location id of this instance.
     * @param io Reference to CanOutput class.
     * @param ovroSubsys Reference to ovro monitor subsystem.
     */
    explicit RxTemperatures( carma::canbus::nodeType node, 
                             carma::canbus::CanOutput & io,
                             carma::monitor::OvroSubsystem & ovroSubsys );

    /**
     * Destructor
     */
    virtual ~RxTemperatures( );

    /**
     * Retrieve a map of this devices half second monitor points.
     * The monitor points returned from this routine will be simulated
     * if the device is in the OFFLINE state.
     * @return Map with descriptions of the device's half second monitor
     * points keyed by message id.
     */
    std::map<carma::canbus::msgType, std::string> getHalfSecMonitors() const;

    /**
     * Return a map of this devices slow monitor points.
     * These monitor points will be simulated every 5 seconds if the device
     * is in the OFFLINE state.
     * @return Map with descriptions of the device's slow (5 second) monitor
     * points keyed by message id.
     */
    std::map<carma::canbus::msgType, std::string> getSlowMonitors() const;

    /**
     * Process a CAN message addressed to this module.
     * This method is responsible for processing all CAN messages addressed
     * to this device.  It is a callback routine that gets called by the
     * carma::canbus::Master class.
     * @param mid Message identifier (carma::canbus::msgType).
     * @param data Reference to the byte vector containing raw data.
     * @param sim True if message is simulated, false normally.
     * @see carma::canbus::Device::processMsg
     */
    void processMsg( carma::canbus::msgType mid,
                     std::vector< carma::canbus::byteType > & data,
                     bool sim);

    /**
     * Produce a simulated CAN message for a give msgType.
     * This routine creates a canbus::Message with simulated data.  
     * The returned message is automatically placed in the CAN
     * message queue for retrieval and processing by the Master class. It
     * thus can be used to test the processMsg method above as well as to
     * place real (albeit simulated) data into the monitor stream.
     * @param mid Message identifier (carma::canbus::msgType).
     * @see carma::Device::simulateMsg
     */
    carma::canbus::Message simulateMsg( carma::canbus::msgType mid );

    /**
     * Update Frame Data.
     * This method is called (or called back) every 1/2 second in order to
     * allow this class to update monitor data on the frame timescale.  Note
     * that this method is called regardless of the modules STATE or the
     * number of messages the device is receiving.
     */
    void updateFrameData();

    // Public control commands and enumerations.

    /**
     * Loop enumerator
     */
    enum LoopId { LOOP_1, LOOP_2 };
    /** 
     * Thermal control loop modes.
     */
    enum OpMode { LOOP_ON, LOOP_OFF, MANUAL };

    /**
     * Set desired temperature.
     * @param loop Loop to apply parameter to.
     * @param temp Target temperature in C.
     */
    void setTemperature( enum LoopId loop, float temp );

    /** 
     * Regulate temperature.
     * Regulate the temperature by turning thermal control loop 
     * to OpMode.  Module defaults to LOOP_ON after any reset.
     * @param loop Loop to apply parameter to.
     * @param mode Operation mode of thermal control loop 1.
     * @param pwr % of maximum power from PWM controller (OpMode = 2 only).
     */
    void regulateTemperature( enum LoopId loop, 
                              enum OpMode mode, 
                              float pwr = 0.0 );

    /**
     * Set loop gain.
     * Set a new value for the loop 1 gain.  This parameter is updated 
     * immediately but you must use the writeParametersToEEPROM method to store
     * loop parameters for reuse after resets.
     * @param loop Loop to apply parameter to.
     * @param gain Loop gain in units of % max power/K
     */
    void setLoopGain( enum LoopId loop, float gain );

    /**
     * Set loop integration constant.
     * Set a new value for the loop integration constant.   The parameter is 
     * updated immediately but an additional command is required to store 
     * them into EEPROM for use upon reset.
     * @param loop Loop to apply parameter to.
     * @param integration Integration constant in units of % max power/K/min.
     */
    void setLoopIntegrationConstant( enum LoopId loop, float integration );

    /**
     * Set loop rate constant.
     * Set a new value for the error derivative gain.  This parameter is
     * updated immediately but you must use writeParametersToEEPROM to 
     * permanently store loop parameters.
     * @param loop Loop to apply parameter to.
     * @param rate Loop derivative rate constant in (% max power) s / K
     */
    void setLoopRateConstant( enum LoopId loop, float rate );

    /**
     * Set loop bandwidth.
     * Set a new value for the loop bandwidth.  The loop bandwidth is the 
     * inverse of the sample and correction interval.  This parameter is 
     * updated immediately but you must use writeParametersToEEPROM to 
     * permanently store loop parameters.
     * @param loop Loop to apply parameter to.
     * @param bandwidth Loop bandwidth in Hz.
     */
    void setLoopBandwidth( enum LoopId loop, float bandwidth );

    /**
     * Write loop parameters to EEPROM.
     * This command writes the current set of loop parameters to EEPROM. 
     */
    void writeParametersToEEPROM( );
                            
protected:

    // No protected data of methods.

private:

    // API Id for this device.
    static const carma::canbus::apiType API_ID                        = 168;

    // API version this class was imlemented from 
    static const char API_VERSION                                     = 'B';

    // Late packet timeout in ms
    static const double PACKET_LATE_THRESHOLD                         = 150.0;

    // Control command message ids.
    static const carma::canbus::msgType SET_TEMPERATURE               = 0x080;
    static const carma::canbus::msgType REGULATE_TEMPERATURE          = 0x081;
    static const carma::canbus::msgType SET_LOOP_GAIN                 = 0x082;
    static const carma::canbus::msgType SET_LOOP_INTEGRATION_CONSTANT = 0x083;
    static const carma::canbus::msgType SET_LOOP_RATE_CONSTANT        = 0x084;
    static const carma::canbus::msgType SET_LOOP_BANDWIDTH            = 0x085;
    static const carma::canbus::msgType WRITE_PARAMS_TO_EEPROM        = 0x08C;
    // Not a control command
    static const carma::canbus::msgType LOOP2_COMMAND_OFFSET          = 0x006;
    

    // Blanking frame message ids.
    static const carma::canbus::msgType BLANKING_FRAME_PACKET_1       = 0x0E0;
    static const carma::canbus::msgType BLANKING_FRAME_PACKET_2       = 0x0E1;
    static const carma::canbus::msgType BLANKING_FRAME_PACKET_3       = 0x0E2;
    static const carma::canbus::msgType BLANKING_FRAME_PACKET_4       = 0x0E3;
    static const carma::canbus::msgType BLANKING_FRAME_PACKET_5       = 0x0E4;
    static const carma::canbus::msgType BLANKING_FRAME_PACKET_6       = 0x0E5;
    static const carma::canbus::msgType BLANKING_FRAME_PACKET_7       = 0x0E6;

    // Disallow assignment and copy construction.
    RxTemperatures( const RxTemperatures & from );
    RxTemperatures & operator=( const RxTemperatures & from );

    // Routines to process individual blanking frame CAN packets.
    // These routines are called internally by processMsg.
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
    void processBlankingFramePacket7(
            std::vector<carma::canbus::byteType> &data);

    // Routines to produce individual simulated blanking frame
    // CAN packets.  These routines are called by simulateMsg.
    carma::canbus::Message simBlankingFramePacket1();
    carma::canbus::Message simBlankingFramePacket2();
    carma::canbus::Message simBlankingFramePacket3();
    carma::canbus::Message simBlankingFramePacket4();
    carma::canbus::Message simBlankingFramePacket5();
    carma::canbus::Message simBlankingFramePacket6();
    carma::canbus::Message simBlankingFramePacket7();

    // Member variables
    log4cpp::Category &log_; // Reference to system logger.
    carma::monitor::OvroSubsystem::RxThermalControl & mon_; // Monitor system

}; // End class RxTemperatures
}}} // End namespace carma::antenna::ovro
#endif
