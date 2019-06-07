/** @file
 * CAN Device class declaration for the Block Downconverter (API 4).
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.7 $
 * $Date: 2011/05/11 17:35:23 $
 * $Id: BlockDownconverter.h,v 1.7 2011/05/11 17:35:23 iws Exp $
 *
 * $CarmaCopyright$
 */
#ifndef CARMA_DOWNCONVERTER_BLOCKDOWNCONVERTER_H
#define CARMA_DOWNCONVERTER_BLOCKDOWNCONVERTER_H

#include "carma/corba/corba.h"
#include "carma/canbus/devices/XacDevice.h"
#include "carma/downconverter/spectral/BlockDownconverterControl.h"

namespace carma {

namespace monitor {
    class BlockDownconverter;
    class SldcSubsystem;
    class StateMonitorPointEnum;
    class Xac;
} // namespace monitor

namespace downconverter {

/**
 * CAN device implementation for block downconverter (API #4).
 */
class BlockDownconverter : public carma::canbus::devices::XacDevice {
public:

    /**
     * Constructor.
     * @param node Node id of device.
     * @param io Reference to CanOutput class.
     */
    explicit BlockDownconverter( carma::canbus::nodeType node,
                                 carma::canbus::CanOutput & co,
                                 carma::monitor::SldcSubsystem & subsys );

    /**
     * Destructor.
     */
    virtual ~BlockDownconverter( );

    /**
     * Retrieve a map of this devices half second monitor points.
     * @return Map containing message ids and a textual description.
     * @see carma::canbus::Device::getHalfSecMonitors
     */
    carma::canbus::MsgIdInfoMap getHalfSecMonitors() const;

    /**
     * Retrieve a map of this devices slow monitor points.
     * @return Map containing message ids and a textual description.
     * @see carma::canbus::Device::getSlowMonitors
     */
    carma::canbus::MsgIdInfoMap getSlowMonitors() const;

    /**
     * Process a CAN message addressed to this module.
     * @param mid Message identifier.
     * @param data Reference to the byte vector containing raw CAN data.
     * @param sim True if message is simulated, false normally.
     * @see carma::canbus::Device::processMsg
     */
    void processMsg( ::carma::canbus::msgType mid,
                     ::carma::canbus::DataVector & data,
                     bool sim );

    /**
     * Produce a simulated CAN message for a given msgType.
     * @param mid Message identifier to simulate.
     * @see carma::canbus::Device::simulateMsg
     */
    carma::canbus::Message simulateMsg( carma::canbus::msgType mid );

    /**
     * Update Frame Data.
     * @see carma::canbus::Device::updateFrameData
     */
    void updateFrameData( );

    /**
     * Set block and polarization for a single output.
     * @param block Block selector.
     * @param polarization Polarization selector.
     * @param inputNo to control (range 1-8).
     */
    void setBlockAndPolarization(
      carma::downconverter::BlockDownconverterControl::Block block,
      carma::downconverter::BlockDownconverterControl::Polarization polarization,
      CORBA::UShort bandNo ) const;

    /**
     * Set block for a single output.
     * @param block Block selector.
     * @param inputNo to control (range 1-8).
     */
    void setBlock(
        carma::downconverter::BlockDownconverterControl::Block block,
        CORBA::UShort bandNo ) const;


    /**
     * Set polarization for single output.
     * @param polarization Polarization selector.
     * @param inputNo to control (range 1-8).
     */
    void setPolarization(
        carma::downconverter::BlockDownconverterControl::Polarization polarization,
        CORBA::UShort bandNo ) const;

    /**
     * Reset the module via a software reset CAN message.
     */
    void reset();

protected:

    // Nothing

private:

    BlockDownconverter( const BlockDownconverter & ); // No copy
    BlockDownconverter & operator=( const BlockDownconverter & ); // No copy

    void processBlankingFramePacket1( ::carma::canbus::DataVector & data );
    void processBlankingFramePacket2( ::carma::canbus::DataVector & data );
    void processBlankingFramePacket3( ::carma::canbus::DataVector & data );
    void processBlankingFramePacket4( ::carma::canbus::DataVector & data );
    void processBlankingFramePacket5( ::carma::canbus::DataVector & data );
    void processBlankingFramePacket6( ::carma::canbus::DataVector & data );

    ::carma::canbus::Message simBlankingFramePacket1( );
    ::carma::canbus::Message simBlankingFramePacket2( );
    ::carma::canbus::Message simBlankingFramePacket3( );
    ::carma::canbus::Message simBlankingFramePacket4( );
    ::carma::canbus::Message simBlankingFramePacket5( );
    ::carma::canbus::Message simBlankingFramePacket6( );

    ::carma::monitor::StateMonitorPointEnum * state_;
    ::carma::monitor::BlockDownconverter * devMon_;
    ::carma::monitor::Xac * xacMon_;

}; // Class BlockDownconverter
}} // namespace carma::downconverter
#endif
