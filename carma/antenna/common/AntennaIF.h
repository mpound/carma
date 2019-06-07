/** @file
 * CAN Device implementation for Antenna IF Module.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.11 $
 * $Date: 2011/01/03 18:48:04 $
 * $Id: AntennaIF.h,v 1.11 2011/01/03 18:48:04 iws Exp $
 *
 * $CarmaCopyright$
 */
#ifndef CARMA_ANTENNA_COMMON_ANTENNAIF_H
#define CARMA_ANTENNA_COMMON_ANTENNAIF_H

// Carma includes
#include "carma/canbus/devices/XacDevice.h"
#include "carma/canbus/Types.h"

// Forward dec
namespace log4cpp {
    class Category;
} // End namespace log4cpp

namespace carma
{

  // Forward dec
  namespace monitor {
      class AntennaIF;
      class StateMonitorPointEnum;
      class Xac;
  } // End namespace monitor

  namespace antenna
  {
    namespace common
    {

/** 
 * Base Antenna IF CAN module device class.
 * All antenna types use the same physical AntennaIF CAN module, also known as,
 * the PreAmplifier Modules (PAM).  This base class handles the majority
 * of message processing and control functionality while providing hooks for
 * the BIMA, OVRO and SZA specific versions. 
 */
class AntennaIF :
   public carma::canbus::devices::XacDevice
{
public:

    /**
     * Constructor
     * @param node Location id of this instance (node location id).
     * @param io Reference to CanOutput class.
     * @param ifMon Reference to AntennaIF monitor system class.
     * @param xacMon Reference to Xac monitor system class.
     */
    AntennaIF(
        carma::canbus::nodeType node, 
        carma::canbus::CanOutput &io,
        carma::monitor::StateMonitorPointEnum & state,
        carma::monitor::AntennaIF &ifMon,
        carma::monitor::Xac &xacMon);

    /**
     * Destructor
     */
    ~AntennaIF();

    /**
     * Retrieve a map of this devices half second monitor points.
     * The monitor points returned from this routine will be simulated
     * if the device is in the OFFLINE state.
     * @return Map with descriptions of the device's half second monitor
     * points keyed by message id.
     */
    carma::canbus::MsgBriefMap getHalfSecMonitors() const;

    /**
     * Return a map of this devices slow monitor points.
     * These monitor points will be simulated every 5 seconds if the device
     * is in the OFFLINE state.
     * @return Map with descriptions of the device's slow (5 second) monitor
     * points keyed by message id.
     */
    carma::canbus::MsgBriefMap getSlowMonitors() const;

    /**
     * Process a CAN message addressed from the AntennaIF module.
     * This routine is responsible for processing all CAN messages addressed
     * to this device.  It is a callback routine that gets called by 
     * the carma::canbus::Master class.
     * @param mid 10bit message id (carma::canbus::msgType).
     * @param data Reference to the byte vector containing the raw data.
     * @param sim True if message is simulated, false normally.
     * @see carma::canbus::Device::processMsg
     */
    void processMsg(carma::canbus::msgType mid,
            carma::canbus::DataVector &data,
            bool sim);

    /**
     * Produce a simulated CAN message for a given msgType.
     * This routine creates a Message with simulated data for an input
     * message id.  The returned message is automatically placed in the CAN
     * message queue for retrieval and processing by the Master class. It
     * thus can be used to test the processMsg method above as well as to
     * place real (albeit simulated) data into the monitor stream.
     * @param mid 10 bit message id (carma::canbus::msgType).
     */
    carma::canbus::Message simulateMsg(carma::canbus::msgType mid);

    // Public control commands.

    /**
     * Select band
     * Select a particular band as an input to the PAM, by setting the 
     * position of the IF switch.
     * @param band IF switch position 1,2,3 or 4.
     */
    void selectBand( unsigned short band );

    /**
     * Set IF total attenuation.
     * Set the IF attenuator to a nominal value.
     * @param atten Attenuator setting, 0-63 dB in 0.5 dB steps.
     */
    void setIFtotalAttenuation(float atten);

    /**
     * Set IF level.
     * Set the IF attenuator so that the IF total power detector returns the 
     * closest value to the input parameter.  Current PAM will only be able to 
     * produce about +8dBm.
     * @param pow Target IF total power, in mW.
     */
    void setIFlevel(float pow);

    /**
     * Set input IF attenuator.
     * @param inputAtten Input attenuator setting, 0-31.5dB in 0.5 db steps.
     */
    void setInputIFattenuator(float inputAtten);

    /**
     * Set output IF attenuator.
     * @param outputAtten Output attenuator setting, 0-31.5 dB in 0.5 db steps.
     */
    void setOutputIFattenuator(float outputAtten);

    /**
     * Query Total Power.
     * Query value read from total power detector in PAM.
     */
    void queryTotalPower();

    /**
     * Set output power to preset.
     * Go to preset IF level.  Current preset IF level is 0 dBm. Thus it is
     * functionally equivaletn to setIFlevel command with a power level of
     * 1.0 mW.
     */
    void setOutputPowerToPreset();

    /**
     * Internally create simulated total power vector.
     * Default implementation is a no-op.
     * @param nsamps Size of simulated total power vector.
     */
    virtual void simTotalPower( unsigned int nsamps );

    static const carma::canbus::msgType IF_LEFT_POL_NODE_ID      = 1;
    static const carma::canbus::msgType IF_RIGHT_POL_NODE_ID     = 2;
    
protected:
    
    void updateFrameData();

    // A little different than your typical CAN Device class impl...
    // Below are blankingFrameHooks that must be overloaded by a relative.
    // They are called in addition to processBlankingFrameX
    virtual void blankingFrameHook1( float ifoutTotPower, float pamTemp );
    virtual void blankingFrameHook2( float attenSet, 
                                     unsigned char pamStat,
                                     unsigned char ifSwitchStat,
                                     unsigned char laserStat,
                                     unsigned char nErrors );
    virtual void blankingFrameHook3( float laserOpticalPow, 
                                     float laserTemp );
    virtual void blankingFrameHook4( float inputAttenSet, 
                                     float outputAttenSet );

    virtual void processTotalPowerResponse(
        carma::canbus::DataVector &data) = 0;
    virtual void processFastTotalPowerPacket(
        carma::canbus::DataVector & data) = 0;
    virtual void processSwitchStatusOnChange(
        carma::canbus::DataVector &data) = 0;
    virtual void processPAMStatusOnChange(carma::canbus::DataVector &data) = 0;
    
    virtual void processFastChannel1Packet( carma::canbus::DataVector &data ) = 0;
    virtual void processFastChannel2Packet( carma::canbus::DataVector &data ) = 0;
    
private:

    // Disallow assignment and copy construction.
    AntennaIF(const AntennaIF &);
    AntennaIF &operator=(const AntennaIF &);

    // Routines to process individual blanking frame CAN packets.
    // These routines are called by processMsg.
    void processBlankingFramePacket1(carma::canbus::DataVector &data);
    void processBlankingFramePacket2(carma::canbus::DataVector &data);
    void processBlankingFramePacket3(carma::canbus::DataVector &data);
    void processBlankingFramePacket4(carma::canbus::DataVector &data);
    void processBlankingFramePacket5(carma::canbus::DataVector &data);

    void processLogMsg( carma::canbus::DataVector &data );

    // Routines to produce individual simulated blanking frame CAN packets.
    // These routines are called by simulateMsg.
    carma::canbus::Message simBlankingFramePacket1();
    carma::canbus::Message simBlankingFramePacket2();
    carma::canbus::Message simBlankingFramePacket3();
    carma::canbus::Message simBlankingFramePacket4();
    carma::canbus::Message simBlankingFramePacket5();

    // Member variables
    log4cpp::Category & log_;
    carma::monitor::StateMonitorPointEnum & state_;
    carma::monitor::AntennaIF & ifMon_;
    carma::monitor::Xac & xacMon_;

}; // End class carma::antenna::common::AntennaIF
}}} // End namespace carma::antenna::common
#endif
