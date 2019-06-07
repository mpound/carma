/** @file
 * CAN Device class declaration for the Bias-Tuned Gunn PLL (API 16).
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.9 $
 * $Date: 2011/01/03 18:48:06 $
 * $Id: GunnPll.h,v 1.9 2011/01/03 18:48:06 iws Exp $
 *
 * $CarmaCopyright$
 */
#ifndef CARMA_ANTENNA_OVRO_GUNNPLL_H
#define CARMA_ANTENNA_OVRO_GUNNPLL_H

#include "carma/canbus/devices/XacDevice.h"

namespace log4cpp {
    // Forward declaration
    class Category;
} // End namespace log4cpp

namespace carma {

namespace monitor {
    class GunnPll;
    class OvroSubsystem;
    class StateMonitorPointEnum;
    class Xac;
} // End namespace :monitor

namespace antenna {
namespace ovro {

/**
 * GunnPll CAN device class.
 */
class GunnPll : public carma::canbus::devices::XacDevice {
public:

    /**
     * Constructor
     * @param node Location id of this instance (node location id).
     * @param co Reference to CanOutput class.
     * @param subsys Reference to OvroSubsystem monitor instance.
     */
    GunnPll( carma::canbus::nodeType node, 
             carma::canbus::CanOutput & co,
             carma::monitor::OvroSubsystem & subsys );

    /**
     * Destructor
     */
    virtual ~GunnPll();

    /**
     * Retrieve a map of this devices half second monitor points.
     * The monitor points returned from this routine will be simulated if the
     * device is in the SIMULATED state.
     * @return Map with string descriptions of the devices's half frame rate
     * monitor points (half second) keyed by message id.
     */
    carma::canbus::MsgIdInfoMap getHalfSecMonitors() const;

    /**
     * Retrieve a map of this devices sow (5 second) monitor points.
     * The monitor points returned from this routine will be simulated if
     * the device is in the SIMULATED state.
     * @return MsgIdInfoMap containing a string description of the device's
     * slow monitor points keyed by message id.
     */
    carma::canbus::MsgIdInfoMap getSlowMonitors() const;

    /**
     * Process a CAN message addressed to this module.
     * This routine is responsible for processing all CAN messages addressed
     * TO this device.  It's effectively a callback routine that gets called by
     * the carma::canbus::Master object.
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
     * Creates a CAN message with simulated data for an input message id. 
     * This routine is called automatically by the carma::canbus::Master object
     * and the returned message is placed in the CAN message queue and 
     * subsequently processMsg is called with the message. 
     * @param mid Message identifier.
     * @see carma::canbus::Device::simulateMsg
     */
    carma::canbus::Message simulateMsg(carma::canbus::msgType mid);

    /**
     * Update Frame Data.
     * This method is called automatically every 1/2 second by the 
     * carma::canbus::Master object.  It is required to allow this class to 
     * update monitor data on the frame timescale that is not directly
     * associated with a CAN packet.  Examples of this are the state of the 
     * module (ONLINE, OFFLINE, etc) and the number of messages being sent
     * and receive for this module.
     */
    void updateFrameData();

    // Control commands
    /**
     * Set the LO frequency and start the lock sequence.  
     * Note that the LO frequency may be the Gunn frequency or three times 
     * the Gunn frequency for the 1-mm LO which utilizes a tripler.
     * @param freq Requested LO frequency in GHz units.
     */
    void setLoFrequency(double freq) const;

    /**
     * Set nominal bias voltage for the Gunn.
     * The actual voltage may be slightly different from this by the 
     * error voltage added by the phase-lock loop.
     * @param volts Gunn operating voltage in 0.01V units. 
     */
    void setGunnOperatingVoltage(float voltage) const;

    /**
     * Set the loop gain for the phase-lock loop.
     * @param gain Requested gain in 0.1% units.
     */
    void setLoopGain(float gain) const;

    /**
     * Turn Gunn on or off.
     * @param on Enable Gunn if true, disable if false.
     */
    void enableGunn(bool on) const;

    /**
     * Disable all Gunn modules.
     * This routine turns off the LO for all Gunn modules.
     */
    void disableAllGunns() const;

    /**
     * Turn phase-lock loop sweep on or off.
     * @param on Enable sweep if true, disable if false.
     */
    void toggleSweep(bool on) const;

    /**
     * Turn phase-lock IF monitor output on or off.
     * @param on Enable monitor output if true, disable if false.
     */
    void enableIfMonitorOutput(bool on) const;

    /**
     * Move the Gunn tunder to a given position.
     * @param pos Tuner position in micro-step units.
     */
    void setTuner(unsigned long pos) const;

    /**
     * Move the Gunn backshort to a given position.
     * @param pos Gunn backshort position in micro-step units.
     */
    void setBackshort(unsigned long pos) const;

    /**
     * Move the LO attenuator to a given position.
     * @param pos LO attenuator position in micro-step units.
     */
    void setAttenuator(unsigned long pos) const;

    /**
     * Move the tuner by a given number of microsteps.
     * @param microsteps Distance to move the Gunn tuner in microsteps.
     */
    void jogTuner(short microsteps) const;

    /**
     * Move the backshort by a given number of microsteps.
     * @param microsteps Distance to move the backshort in microsteps.
     */
    void jogBackshort(short microsteps) const;

    /**
     * Move the attenuator by a given number of microsteps.
     * @param microsteps Distance to move the attenuator in microsteps.
     */
    void jogAttenuator(short microsteps) const;

protected:

    // Nothing

private:

    // Disallow assignment and copy construction.
    GunnPll(const GunnPll &);
    GunnPll &operator=(const GunnPll &);

    // Routines to process individual blanking frame CAN packets.
    // These routines are called by processMsgs.
    void processBlankingFramePacket1(::carma::canbus::DataVector & data);
    void processBlankingFramePacket2(::carma::canbus::DataVector & data);
    void processBlankingFramePacket3(::carma::canbus::DataVector & data);
    void processBlankingFramePacket4(::carma::canbus::DataVector & data);
    void processBlankingFramePacket5(::carma::canbus::DataVector & data);
    void processBlankingFramePacket6(::carma::canbus::DataVector & data);
    void processBlankingFramePacket7(::carma::canbus::DataVector & data);
    void processBlankingFramePacket8(::carma::canbus::DataVector & data);
    void processBlankingFramePacket9(::carma::canbus::DataVector & data);

    // Routines to produce individual simulated blanking frame CAN packets.
    // These routines are called by simulateMsg.
    ::carma::canbus::Message simBlankingFramePacket1();
    ::carma::canbus::Message simBlankingFramePacket2();
    ::carma::canbus::Message simBlankingFramePacket3();
    ::carma::canbus::Message simBlankingFramePacket4();
    ::carma::canbus::Message simBlankingFramePacket5();
    ::carma::canbus::Message simBlankingFramePacket6();
    ::carma::canbus::Message simBlankingFramePacket7();
    ::carma::canbus::Message simBlankingFramePacket8();
    ::carma::canbus::Message simBlankingFramePacket9();

    ::carma::monitor::StateMonitorPointEnum * state_;
    ::carma::monitor::GunnPll * mon_;
    ::carma::monitor::Xac *     xacMon_;
    ::log4cpp::Category &       log_;

}; // End class GunnPll
}}} // End namespace carma::antenna::ovro
#endif
