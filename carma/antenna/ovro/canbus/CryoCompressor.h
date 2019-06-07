/** @file
 * CAN Device implementation for CARMA CANbus API No. 32 - Cryo Compressor 
 * Monitor and Control.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.12 $
 * $Date: 2008/05/22 18:50:47 $
 * $Id: CryoCompressor.h,v 1.12 2008/05/22 18:50:47 abeard Exp $
 */

#ifndef CARMA_ANTENNA_OVRO_CRYOCOMPRESSOR_H
#define CARMA_ANTENNA_OVRO_CRYOCOMPRESSOR_H

// Carma includes
#include "carma/canbus/devices/XacDevice.h"
#include "carma/canbus/Types.h"
#include "carma/monitor/OvroSubsystem.h"

namespace log4cpp {
    // Forward dec
    class Category;
} // End namespace log4cpp

namespace carma {
namespace antenna {
namespace ovro {

/**
 * Cryo Compressor device class.
 * This class implements API No. 32 for the OVRO Cryo Compressor CAN module. 
 * Note that this class doesn't directly subclass IDL generated interfaces.  
 * I've decided instead to use delegation which should provide a small level of
 * indirection if we ever decide to switch the underlying high level 
 * communication model.
 */
class CryoCompressor : public carma::canbus::devices::XacDevice {
public:

    /** 
     * Constructor
     * @param node Location id of this instance (node location id).
     * @param io Reference to CanOutput class.
     */
    CryoCompressor( carma::canbus::nodeType node,
                    carma::canbus::CanOutput & io,
                    carma::monitor::OvroSubsystem & ovroSubsys );

    /**
     * Destructor
     */
    ~CryoCompressor();

    /**
     * Retrieve a map of this devices controls.
     * @return a map of control message string descriptions keyed by
     * message id.
     */
    std::map<carma::canbus::msgType, std::string> getControls() const;

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
     * @param sim Indicates if message is real or simulated.
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

    // Public control commands 
    
    /**
     * Turn helium compressor on or off.
     * @param on flag indicating ON or OFF (on if true).
     */
    void enableCompressor(bool on);

    /**
     * Reset the helium compressor.
     */
    void resetCompressor();

    /**
     * Fill the helium compressor.
     * This command will cause a one second mesasured charge of helium
     * gas to be added to the helium compressor.
     */
    void fillCompressor();

    /**
     * Purge the helium compressor.
     * This command will cause a 0.5 second measured charge of helium
     * gas to be purged from the helium compressor.
     */
    void purgeCompressor();

    /**
     * Turn temperature servo on or off.
     * This control command enables or disables the temperature servo loop.
     * @param on Enable temperature servo (true = enable, false = disable).
     */
    void enableTemperatureServo(bool on);

    /**
     * Set inlet louver position.
     * This control commands sets the position of the cabinet inlet louver.
     * If the temperature servo is on, the temperature control loop will 
     * override this setting.
     * @param volts Command voltage for louver (1.8-8.0 V).
     */
    void setInletLouver(float volts);

    /**
     * Set outlet louver position.
     * This control command sets the position of the cabinet outlet louver.
     * Again, if the temperature servo is on, the temperature control loop 
     * will override this setting.
     * @param volts Command voltage for louver (1.8-8.0 V).
     */
    void setOutletLouver(float volts);
    
private:

    
    // Disallow assignment and copy construction.
    CryoCompressor(const CryoCompressor &);
    CryoCompressor &operator=(const CryoCompressor &);

    // Routines to process individual blanking frame CAN packets.
    // These routines are called by processMsg.
    void processBlankingFramePacket1( carma::canbus::DataVector &data );
    void processBlankingFramePacket2( carma::canbus::DataVector &data );
    void processBlankingFramePacket3( carma::canbus::DataVector &data );
    void processBlankingFramePacket4( carma::canbus::DataVector &data );
    void processBlankingFramePacket5( carma::canbus::DataVector &data );
    void processBlankingFramePacket6( carma::canbus::DataVector &data );

    // Routines to produce individual simulated blanking frame 
    // CAN packets.  These routines are called by simulateMsg.
    carma::canbus::Message simBlankingFramePacket1();
    carma::canbus::Message simBlankingFramePacket2();
    carma::canbus::Message simBlankingFramePacket3();
    carma::canbus::Message simBlankingFramePacket4();
    carma::canbus::Message simBlankingFramePacket5();
    carma::canbus::Message simBlankingFramePacket6();

    // Member variables
    
    log4cpp::Category &log_;  // Reference to the system logger
    carma::monitor::OvroSubsystem::Compressor & mon_;

}; // End class CryoCompressor
}}} // End namespace carma::antenna::ovro
#endif
