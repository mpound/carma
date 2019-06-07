/** @file
 * CAN Device implementation for 10-m Secondary Mirror.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.15 $
 * $Date: 2009/03/11 18:44:03 $
 * $Id: SecondaryMirror.h,v 1.15 2009/03/11 18:44:03 abeard Exp $
 */

#ifndef CARMA_ANTENNA_OVRO_SECONDARYMIRROR_H
#define CARMA_ANTENNA_OVRO_SECONDARYMIRROR_H

// Carma includes
#include "carma/canbus/devices/XacDevice.h"
#include "carma/canbus/Types.h"
#include "carma/monitor/OvroSubsystem.h"

#include <memory>

namespace log4cpp {
    // Forward declarations.
    class Category;
} // End namespace log4cpp

namespace carma {

namespace monitor {
    class AntennaCommon;
} // End namespace monitor

namespace antenna {
namespace ovro {

class SharedOpticsSeqNo;

/**
 * 10-m Secondary Mirror CAN device class.
 * This class implements API No. 56 for the OVRO Secondary Mirror CAN module. 
 * Note that this class doesn't directly subclass IDL generated interfaces.  
 * I've decided instead to use delegation which should provide a small level of
 * indirection if we ever decide to switch the underlying high level 
 * communication model.
 */
class SecondaryMirror : public carma::canbus::devices::XacDevice {
public:

    /** 
     * Constructor
     * @param node Location id of this instance (node location id).
     * @param io Reference to CanOutput class.
     * @param ovroSubsys Reference to existing ovro monitor subsystem. 
     */
    SecondaryMirror( carma::canbus::nodeType node, 
                     carma::canbus::CanOutput & io,
                     carma::monitor::OvroSubsystem & ovroSubsys, 
                     carma::antenna::ovro::SharedOpticsSeqNo & sharedSeqNo );

    /**
     * Destructor
     */
    ~SecondaryMirror();

    /**
     * Retrieve a map of this devices half second monitor points.
     * The monitor points returned from this routine will be 
     * simulated if the device is in the OFFLINE state.
     * @return a map of the devices half second monitor points string
     * descriptions keyed by message id.
     */
    carma::canbus::MsgBriefMap getHalfSecMonitors( ) const;

    /**
     * Return a map of this devices slow monitor points.
     * These monitor points will be simulated every 5 seconds if 
     * the device is in the OFFLINE state.
     * @return a map of the devices slow (5 sec) monitor points string
     * descriptions keyed by message id.
     */
    carma::canbus::MsgBriefMap getSlowMonitors( ) const;

    /**
     * Process a CAN message addressed to the SecondaryMirror module.
     * This routine is responsible for processing all CAN messages 
     * addressed to this device.
     * @param mid the 10bit message id (carma::canbus::msgType)
     * @param data reference to the byte vector containing the raw data.
     * @param sim Indicates if message is real or simulated.
     * @see carma::canbus::Device::processMsg
     */
    void processMsg( carma::canbus::msgType mid, 
                     carma::canbus::DataVector & data, 
                     bool sim );

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
    void updateFrameData( ); 

    // Public control commands 
    
    /**
     * Set position along horizontal axis (relative to horizon).
     * @param posInMM X position in millimeters.
     */
    void setXPosition( float posInMM, long seqNo ); 
    
    /**
     * Set position along vertical axis (relative to horizon).
     * @param posInMM Y position in millimeters.
     */
    void setYPosition( float posInMM, long seqNo );
    
    /**
     * Set position parallel to beam (focus).
     * @param posInMM Z position in millimeters.
     */
    void setZPosition( float posInMM, long seqNo ); 

    /**
     * Cycle LVDT 12 V power.
     */
    void cycleLvdtPower( );

    /**
     * Stop motion.
     */
    void stopMotion( );

    /**
     * Do Z Tracking.
     */
    void doZTracking( bool ztrack, long seqNo );

private:

    // No copy of assignment allowed
    SecondaryMirror( const SecondaryMirror & );
    SecondaryMirror &operator=( const SecondaryMirror & );

    // Routines to process individual blanking frame CAN packets.
    void processBlankingFramePacket1( carma::canbus::DataVector & data );
    void processBlankingFramePacket2( carma::canbus::DataVector & data );
    void processBlankingFramePacket3( carma::canbus::DataVector & data );
    void processBlankingFramePacket4( carma::canbus::DataVector & data );

    // Routines to produce simulated blanking frame CAN packets. 
    carma::canbus::Message simBlankingFramePacket1( );
    carma::canbus::Message simBlankingFramePacket2( );
    carma::canbus::Message simBlankingFramePacket3( );
    carma::canbus::Message simBlankingFramePacket4( );

    // Member variables
    carma::monitor::AntennaCommon & comMon_; // Reference to common MPs.
    carma::monitor::OvroSubsystem::Secondary & mon_; // Reference to secondary.

    carma::antenna::ovro::SharedOpticsSeqNo & sharedSeqNo_;

    struct Shared;

    ::std::auto_ptr< Shared > shared_;

}; // End class SecondaryMirror 
}}} // End namespace carma::antenna::ovro
#endif
