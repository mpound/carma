/** @file
 * CAN Device class declaration for the Varactor-Tuned Gunn PLL (API 48).
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.2 $
 * $Date: 2011/01/03 18:48:05 $
 * $Id: Varactor.h,v 1.2 2011/01/03 18:48:05 iws Exp $
 *
 * $CarmaCopyright$
 */
#ifndef CARMA_ANTENNA_COMMON_VARACTOR_H
#define CARMA_ANTENNA_COMMON_VARACTOR_H

#include "carma/canbus/devices/XacDevice.h"

namespace carma {

namespace monitor {
    class StateMonitorPointEnum;
    class VaractorModule;
    class Xac;
} // namespace monitor

namespace antenna {
namespace common {

/**
 * Varactor CAN device class.
 */
class Varactor : public carma::canbus::devices::XacDevice {
public:

    /**
     * Constructor
     * @param node Location id of this instance (node location id).
     * @param co Reference to CanOutput class.
     * @param varMon Reference to Varactor monitor instance.
     * @param xacMon Reference to Xac monitor instance.
     * @param stateMon Reference to StateMonitorPointEnum monitor instance.
     */
    Varactor( carma::canbus::nodeType node,
              carma::canbus::CanOutput & co,
              carma::monitor::VaractorModule & varMon,
              carma::monitor::Xac & xacMon,
              carma::monitor::StateMonitorPointEnum & stateMon );

    /**
     * Destructor
     */
    virtual ~Varactor();

    /**
     * Retrieve a map of this devices half second monitor points.
     * @return Map with string descriptions of the devices frame rate monitor
     *      points (half second) keyed by message id.
     * @see carma::canbus::Device::getHalfSecMonitors
     */
    carma::canbus::MsgIdInfoMap getHalfSecMonitors() const;
  
    /**
     * Retrieve a map of this devices slow (5 second) monitor points.
     * @return MsgIdInfoMap containing a string description of the device's
     * slow monitor points keyed by message id.
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
                     bool sim);

    /**
     * Produce a simulated CAN message for a given msgType.
     * @param mid Message identifier to simulate.
     * @see carma::canbus::Device::simulateMsg
     */
    carma::canbus::Message simulateMsg(carma::canbus::msgType mid);

    /**
     * Update Frame Data.
     * @see carma::canbus::Device::updateFrameData
     */
    void updateFrameData();

    /**
     * Turn the gunn oscillator on or off.
     * @param on Turn gunn on if true, off otherwise.
     */
    void enableGunn( bool on ) const;

    /**
     * Turn the sweep on or off.
     * @param on Turn sweep on if true, off otherwise.
     */
    void enableSweep( bool on ) const;

    /** 
     * Turn the IF monitor on or off.
     * @param on Turn IF monitor on if true, off otherwise.
     */
    void enableIFmonitor( bool on ) const;

    /**
     * Set the PLL loop gain resistance (controlled by a digital pot).
     * @param resistance Loop gain resistance in ohms.
     */
    void setLoopGainResistance( unsigned short resistanceInOhms ) const;

    /**
     * Set dummy LO Frequency.
     * The frequency is not used for anything other than to pass down the
     * monitor stream.  This is to assure consistency with Gunn modules 
     * and antenna common MPs.
     * @param freq in GHz.
     */
    void setDummyLoFreq( float freqInGHz );

protected:

    // Nothing

private:

    Varactor( const Varactor & ); // No copy
    Varactor & operator=( const Varactor & ); // No copy
    
    void processBlankingFramePacket1(::carma::canbus::DataVector & data);
    void processBlankingFramePacket2(::carma::canbus::DataVector & data);
    void processBlankingFramePacket3(::carma::canbus::DataVector & data);
    void processBlankingFramePacket4(::carma::canbus::DataVector & data);
    void processBlankingFramePacket5(::carma::canbus::DataVector & data);

    ::carma::canbus::Message simBlankingFramePacket1();
    ::carma::canbus::Message simBlankingFramePacket2();
    ::carma::canbus::Message simBlankingFramePacket3();
    ::carma::canbus::Message simBlankingFramePacket4();
    ::carma::canbus::Message simBlankingFramePacket5();

    ::carma::monitor::StateMonitorPointEnum & state_;
    ::carma::monitor::VaractorModule & varMon_;
    ::carma::monitor::Xac & xacMon_;
    float freqInGHz_;

}; // Class Varactor
}}} // carma::antenna::common
#endif
