/** @file
 * carma::antenna::ovro::EnvironmentalMonitor class definition.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.14 $
 * $Date: 2011/09/21 21:57:31 $
 * $Id: EnvironmentalMonitor.h,v 1.14 2011/09/21 21:57:31 abeard Exp $
 */

#ifndef CARMA_ANTENNA_OVRO_ENVIRONMENTALMONITOR_H
#define CARMA_ANTENNA_OVRO_ENVIRONMENTALMONITOR_H

// Carma includes
#include "carma/canbus/devices/XacDevice.h"
#include "carma/canbus/Types.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/util/PthreadMutex.h"

namespace log4cpp {
    class Category;
} // End namespace log4cpp

namespace carma {
namespace antenna {
namespace ovro {

/**
 * CAN Device class implementation for 10-m sidecab environmental monitor.
 */
class EnvironmentalMonitor : public carma::canbus::devices::XacDevice {
public:

    /**
     * Constructor
     * @param node Location id of this instance (node location id).
     * @param io Reference to CanOutput class.
     */
    EnvironmentalMonitor(
        carma::canbus::nodeType node, 
        carma::canbus::CanOutput &io,
        carma::monitor::OvroSubsystem & mon);

    /**
     * Destructor
     */
    ~EnvironmentalMonitor();

    /**
     * Retrieve a map of this devices half second monitor points.
     * The monitor points returned from this routine will be 
     * simulated if the device is in the OFFLINE state.
     * @return a map of the devices half second monitor points string
     * descriptions keyed by message id.
     */
    carma::canbus::MsgBriefMap getHalfSecMonitors() const;

    /**
     * Return a map of this devices slow monitor points.
     * These monitor points will be simulated every 5 seconds if 
     * the device is in the OFFLINE state.
     * @return a map of the devices slow (5 sec) monitor points string
     * descriptions keyed by message id.
     */
    carma::canbus::MsgBriefMap getSlowMonitors() const;

    /**
     * Process a CAN message addressed to the SecondaryMirror module.
     * This routine is responsible for processing all CAN messages 
     * addressed to this device.
     * @param mid the 10bit message id (carma::canbus::msgType)
     * @param data reference to the byte vector containing the raw data.
     * @param sim Indicates if message is real or simulated.
     * @see carma::canbus::Device::processMsg
     */
    void processMsg( carma::canbus::msgType      mid, 
                     carma::canbus::DataVector & data, 
                     bool                        sim );

    /**
     * Produce a simulated CAN message for a given msgType.
     * This routine creates a Message with simulated data for an  
     * input message id.  The returned message is automatically 
     * placed in the CAN message queue for retrieval and processing 
     * by the Master class.  It thus can be used to test the processMsg 
     * method above. 
     * @param mid the 10bit message id (carma::canbus::msgType)
     */
    carma::canbus::Message simulateMsg( carma::canbus::msgType mid );

    /**
     * Update Frame Data. 
     */
    void updateFrameData(); 

    // Public control commands
    /**
     * Enable or disable the optical pointing camera.
     * If instructed to enable the camera, this method will cause the lens
     * cap to be removed and the camera to be switched on.  If instructed
     * to disable the camera, the reverse will occur.
     * @param on Enable camera if true, disable if false.
     */
    void enableCamera(bool on);

    /**
     * Turn sidecab power off!!!
     * Yes this is a dangerous command and should only be used in an emergency.
     * This shuts off power to the entire sidecab, including the linux host
     * such that no hardware can be monitored or controlled any longer.  IT
     * REQUIRES THAT THE POWER BE RESTORED MANUALLY!!! This could mean a long
     * drive for somebody if nobody is at the high site!!!
     */
    void turnSidecabPowerOff();

    /**
     * Enable or disable one of three 24 Volt power supplies.
     * Turns on or off one of the sidecab 24V supplies.
     * @param supplyNo 0 for all, 1 - 3 for specific power supply.
     * @param on True to enable power supply, false to disable it.
     */
    void enable24vPs(unsigned short supplyNo, bool on);
    
protected:
    // No protected members
private:
    
    // Disallow assignment and copy construction.
    EnvironmentalMonitor(const EnvironmentalMonitor &);
    EnvironmentalMonitor &operator=(const EnvironmentalMonitor &);

    // Routines to process individual blanking frame CAN packets.
    // These routines are called by processMsg.
    void processBlankingFramePacket1( carma::canbus::DataVector & data );
    void processBlankingFramePacket2( carma::canbus::DataVector & data );
    void processBlankingFramePacket3( carma::canbus::DataVector & data );
    void processBlankingFramePacket4( carma::canbus::DataVector & data );

    // Routines to produce individual simulated blanking frame CAN packets.
    // These routines are called by simulateMsg.
    carma::canbus::Message simBlankingFramePacket1();
    carma::canbus::Message simBlankingFramePacket2();
    carma::canbus::Message simBlankingFramePacket3();
    carma::canbus::Message simBlankingFramePacket4();

    log4cpp::Category &                                   log_; 
    carma::monitor::OvroSubsystem::EnvironmentalMonitor & mon_;
    carma::monitor::OvroSubsystem::Track &                trackMon_;
    carma::monitor::AntennaCommon &                       comMon_;
    
}; // End class EnvironmentalMonitor
}}} // End namespace carma::antenna::ovro
#endif
