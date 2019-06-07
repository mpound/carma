/** @file
 * carma::antenna::common::LOReferenceMonitor class declaration.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.6 $
 * $Date: 2011/01/03 18:48:04 $
 * $Id: LOReferenceMonitor.h,v 1.6 2011/01/03 18:48:04 iws Exp $
 */
#ifndef CARMA_ANTENNA_COMMON_LOREFERENCEMONITOR_H
#define CARMA_ANTENNA_COMMON_LOREFERENCEMONITOR_H

// Carma includes
#include "carma/canbus/devices/XacDevice.h"
#include "carma/canbus/Types.h"

namespace log4cpp {
    // Forward dec
    class Category;
} // End namespace log4cpp

namespace carma {

namespace monitor {
    class LoReference;
    class StateMonitorPointEnum;
    class Xac;
} // End namespace monitor

namespace antenna {
namespace common {

/**
 * CAN Device class implementation for LO Reference Monitor Module.
 * This module is also know as the Berkeley Interface Module. Note that the
 * module is shared among all antenna types.
 */
class LOReferenceMonitor : public carma::canbus::devices::XacDevice {
public:

    /**
     * Constructor
     * 
     * @param node Location id of this instance (node location id).
     * @param io Reference to CanOutput class.
     */
    LOReferenceMonitor(
        carma::canbus::nodeType node,
        carma::canbus::CanOutput &io,
        carma::monitor::StateMonitorPointEnum & state,
        carma::monitor::LoReference& lorefMon,
        carma::monitor::Xac& xac);

    /**
     * Destructor
     */
    ~LOReferenceMonitor();

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
     * @see carma::canbus::Device::processMsg
     */
    void processMsg(carma::canbus::msgType mid,
            carma::canbus::DataVector &data,
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

    // Public control commands
    /**
     * Set the LO Terminator Attenuation.
     * @param atten Attenuation value on the LO terminator (0-31dB)
     */
    void setLOTerminatorAttenuation(unsigned char atten);
    
    /**
     * Set power level to preset value.
     */
    void setPowerLevelToPreset( );

    /**
     * Set power level to requested level.
     * @param power in dBm
     */
    void setPowerLevel( double power );

protected:

    // There are no protected methods

private:

    // Disallow assignment and copy construction.
    LOReferenceMonitor(const LOReferenceMonitor &);
    LOReferenceMonitor &operator=(const LOReferenceMonitor &);

    // Routines to process individual blanking frame CAN packets.
    // These routines are called by processMsg.
    void processBlankingFramePacket1( carma::canbus::DataVector & data );
    void processBlankingFramePacket2( carma::canbus::DataVector & data );
    void processBlankingFramePacket3( carma::canbus::DataVector & data );
    void processBlankingFramePacket4( carma::canbus::DataVector & data );
    void processBlankingFramePacket5( carma::canbus::DataVector & data );

    // Routines to produce individual simulated blanking frame
    // CAN packets.  These routines are called by simulateMsg.
    carma::canbus::Message simBlankingFramePacket1();
    carma::canbus::Message simBlankingFramePacket2();
    carma::canbus::Message simBlankingFramePacket3();
    carma::canbus::Message simBlankingFramePacket4();
    carma::canbus::Message simBlankingFramePacket5();

    // Member variables
    log4cpp::Category &log_; // Reference to the system logger.
    carma::monitor::StateMonitorPointEnum & state_;
    carma::monitor::LoReference& mon_;
    carma::monitor::Xac& xacMon_;

}; // End class LOReferenceMonitor
}}} // End namespace carma::antenna::common
#endif
