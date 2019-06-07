/** @file
 * CAN Device implementation for 10-m Antenna Optics.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.13 $
 * $Date: 2012/05/15 20:08:58 $
 * $Id: Optics.h,v 1.13 2012/05/15 20:08:58 abeard Exp $
 */

#ifndef CARMA_ANTENNA_OVRO_OPTICS_H
#define CARMA_ANTENNA_OVRO_OPTICS_H

// Carma includes
#include "carma/canbus/devices/XacDevice.h"
#include "carma/canbus/Types.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/util/PthreadMutex.h"

namespace log4cpp {
    // Forward dec
    class Category;
} // End namespace log4cpp

namespace carma {

namespace antenna {
namespace ovro {

/**
 * 10-m Antenna Optics CAN device class.
 * This class implements API No. 72 for the OVRO Antenna Optics CAN module. 
 * Note that this class doesn't directly subclass IDL generated interfaces.  
 * I've decided instead to use delegation which should provide a small level of
 * indirection if we ever decide to switch the underlying high level 
 * communication model.
 */
class Optics : public carma::canbus::devices::XacDevice {
public:

    /** 
     * Constructor
     * @param node Location id of this instance (node location id).
     * @param io Reference to CanOutput class.
     * @param ovroSubsys Reference to ovro monitor subsystem.
     */
    Optics( carma::canbus::nodeType node, 
            carma::canbus::CanOutput & io,
            carma::monitor::OvroSubsystem & ovroSubsys );

    /**
     * Destructor
     */
    ~Optics();

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
     * Process a CAN message addressed to the SecondaryMirror module.
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
     * Calibrator wheel position enumeration. 
     * It is quite likely that this is the third or fourth duplication of 
     * this enumeration.  However, it is necessary to prevent the CORBA 
     * or IDL defined enumerations from being included herin. Separation of
     * church and state or something like that.  In reality, this is 
     * just creating more work for me with the tradeoff being that the class 
     * is less coupled.
     */
    enum CAL_POS {
        SKY               = 0x00,  // Sky 
        AMBIENT           = 0x01,  // Sidecab temperature
        PARTIAL           = 0x02,  // Partial reflecting vane
        HOT               = 0x03,  // Temperature controlled load
    }; 
    
    /**
     * Polarization State enumeration.
     * Ditto description for CAL_POS enum.
     */
    enum POLARIZATION_STATE {
        LINEAR_VERTICAL    = 0x00,
        LINEAR_HORIZONTAL  = 0x01,
        LEFT_CIRCULAR      = 0x02,
        RIGHT_CIRCULAR     = 0x03
    };
    
    /**
     * Receiver enumeration.
     */
    enum RECEIVER {
        RX_CM           = 0x00,   // Centimeter Receiver
        RX_3MM          = 0x01,   // 3mm Receiver
        RX_1MM          = 0x02,   // 1mm Receiver
        RX_GRID         = 0x03    // Grid (3mm and 1mm)
    };
    
    /**
     * Mm Rx selection enumerator
     */
    enum MM_RX_SELECTION {
        MIRROR          = 0x00, // Mirror (3-mm Rx)
        THROUGH         = 0x01, // Through (1-mm)
        GRID            = 0x02  // Grid (3-mm and 1-mm)
    }; 
        
    /**
     * Select receiver.
     * @param rx Specific receiver to place in beam.
     */
    void selectReceiver( enum RECEIVER rx );

    /**
     * Set Calibrator Position.
     * @param pos Calibrator position.
     */
    void setCalPosition( enum CAL_POS pos );

    /** 
     * Set calibrator position with a sequence number.
     * @param pos Calibrator position.
     * @param sequenceNo Sequence number to return in the monitor stream.
     */
    void setCalPosition( enum CAL_POS pos,
                         unsigned long sequenceNo,
                         bool sequenceNoFromRx );

    /**
     * Rotate the receiver select mechanism in the sidecab.
     * @param mmRx Positions specified by RECEIVER enum - RX_CM is not used.
     */
    void moveMmRxSelect( enum MM_RX_SELECTION mmRx );

    /**
     * Move the tertiary mirror to a selected position.
     * @param in If true move tertiary in for mm rx, else move it out for cm rx.
     */
    void moveTertiary( bool in );

    /**
     * Set millimeter calibrator position.
     * Set the millimeter receiver calibration wheel to requested position.  
     * This will move the mm cal wheel regardless of the current receiver 
     * selection.
     * @param pos Calibrator position.
     */
    void setMmCalPosition( enum CAL_POS pos ); 

    /**
     * Set centimeter calibrator position.
     * Set the centimeter receiver calibration wheel to requested position.  
     * This will move the cm cal wheel regardless of the current receiver 
     * selection. The partial and hot load states are ignored for this command.
     * @param pos Cm calibrator position.
     */
    void setCmCalPosition( enum CAL_POS pos ); 

    /**
     * Set mm rx selector position.
     * Set mm receiver select mechanism to an arbitrary specified angle
     * @param angleInDegrees Angle in degrees.
     */
    void setMmRxSelectorAngle( float angleInDegrees );

private:

    
    // Disallow assignment and copy construction.
    Optics(const Optics &);
    Optics &operator=(const Optics &);

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
    
    typedef carma::monitor::OvroSubsystem::CalStateMonitorPointEnum OvroCalStateMPE;

    void
    setAntennaCommonCalibratorState( 
        OvroCalStateMPE::CALSTATE instantaneousCalState );

    void 
    handlePendingCalibratorPositionRequests( 
        OvroCalStateMPE::CALSTATE instantaneousCalState );

    typedef struct {
        OvroCalStateMPE::CALSTATE requestedPosition;
        bool                      requestPending;
        unsigned int              requestPendingFrames; 
        unsigned long             pendingSequenceNo;  
        bool                      sequenceNoFromRx;
        carma::util::PthreadMutex mutex;
    } CalPosRequestType;

    // Member variables
    log4cpp::Category &log_;  // Reference to the system logger
    carma::monitor::AntennaCommon &common_; // Reference to common MPs.
    carma::monitor::OvroSubsystem::Optics &mon_;

    CalPosRequestType calPosRequest_;
    unsigned long currentSequenceNo_;  

    OvroCalStateMPE::CALSTATE lastInstantaneousCalState_; 

}; // End class Optics 
}}} // End namespace carma::antenna::ovro
#endif
