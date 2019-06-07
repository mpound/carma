/** @file
 * CAN Device implementation for CARMA CANbus API No. 160 - Cryo Temperatures.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.9 $
 * $Date: 2009/08/28 21:48:39 $
 * $Id: CryoTemperatures.h,v 1.9 2009/08/28 21:48:39 abeard Exp $
 */

#ifndef CARMA_ANTENNA_OVRO_CRYOTEMPERATURES_H
#define CARMA_ANTENNA_OVRO_CRYOTEMPERATURES_H

// Carma includes
#include "carma/canbus/devices/XacDevice.h"
#include "carma/canbus/Types.h"
#include "carma/monitor/OvroSubsystem.h"

namespace log4cpp {
    // Forward declaration
    class Category;
} // End namespace log4cpp

namespace carma {
namespace antenna {
namespace ovro {

/**
 * Cryo Temperature device class.
 * This class implements API No. 160 for the OVRO Cryo Temperatures CAN
 * module.
 */
class CryoTemperatures : public carma::canbus::devices::XacDevice 
{
public:
    
    /**
     * Constructor
     * @param node id of device
     * @param io reference to CanOutput class.
     * @param ovroSubsys Reference to ovro monitor subsystem.
     */
    CryoTemperatures(carma::canbus::nodeType node, 
                     carma::canbus::CanOutput & io,
                     carma::monitor::OvroSubsystem & ovroSubsys );

    /**
     * Destructor
     */
    ~CryoTemperatures();

    /** 
     * Retrieve a map of this devices half second monitor points.
     * The monitor points returned from this routine will be
     * simulated if the device is in the OFFLINE state.
     */
    std::map<carma::canbus::msgType, std::string> getHalfSecMonitors() const;

    /**
     * Return a map of this devices slow monitor points.
     * These monitor points will be simulated every 5 seconds if
     * the device is in the OFFLINE state.
     */
    std::map<carma::canbus::msgType, std::string> getSlowMonitors() const;

    /**
     * Process a CAN message.
     * This routine is responsible for processing a CAN message
     * addressed to this device.
     * @param mid the 10bit message id (carma::canbus::msgType)
     * @param data reference to the byte vector containing the raw data.
     * @param sim Indicated if message is real or simulated.
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
     */
    carma::canbus::Message simulateMsg(carma::canbus::msgType mid);

    /**
     * Update Frame Data
     */
    void updateFrameData();

private:
    
   // API Id for this device.
   static const carma::canbus::apiType API_ID                  = 160;

   // API version this class was implemented from
   static const char API_VERSION                                = 'F';

   // Late packet timeout in ms
   static const double PACKET_LATE_THRESHOLD                    = 150.0;

   // There are no unique control commands for this device.

   // Blanking frame message ids.
   static const carma::canbus::msgType BLANKING_FRAME_PACKET_1  = 0x0E0;
   static const carma::canbus::msgType BLANKING_FRAME_PACKET_2  = 0x0E1;
   static const carma::canbus::msgType BLANKING_FRAME_PACKET_3  = 0x0E2;
    
    // Disallow assignment and copy construction
    CryoTemperatures(const CryoTemperatures &);
    CryoTemperatures &operator=(const CryoTemperatures &);

    // Routines to process individual blanking frame CAN packets
    // These routines are called internally by processMsg.
    void processBlankingFramePacket1(
        std::vector<carma::canbus::byteType> &data);
    void processBlankingFramePacket2(
        std::vector<carma::canbus::byteType> &data);
    void processBlankingFramePacket3(
        std::vector<carma::canbus::byteType> &data);

    // Routines to produce simulated blanking frame CAN packets.  
    // These routines are called internally by simulateMsg.
    carma::canbus::Message simBlankingFramePacket1();
    carma::canbus::Message simBlankingFramePacket2();
    carma::canbus::Message simBlankingFramePacket3();

    // Member variables
    log4cpp::Category &log_; // Reference to the system logger.
    carma::monitor::OvroSubsystem::Dewar & mon_;
    carma::monitor::AntennaCommon & comMon_;

};  // End class CryoTemperatures
}}} // End namespace carma::antenna::ovro
#endif
