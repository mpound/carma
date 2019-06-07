/** @file
 * CAN Device implementation for YIG PLL.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.13 $
 * $Date: 2010/06/24 21:16:30 $
 * $Id: YigPll.h,v 1.13 2010/06/24 21:16:30 abeard Exp $
 *
 * $CarmaCopyright$
 */
#ifndef CARMA_ANTENNA_OVRO_YIGPLL_H
#define CARMA_ANTENNA_OVRO_YIGPLL_H

// Carma includes
#include "carma/canbus/devices/XacDevice.h"
#include "carma/canbus/Types.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/util/PthreadCond.h"
#include "carma/util/PthreadMutex.h"

namespace log4cpp {
    // Forward dec
    class Category;
} // End namespace log4cpp

namespace carma {
namespace antenna {
namespace ovro {

/**
 * YIG PLL CAN module device class.
 */
class YigPll : public carma::canbus::devices::XacDevice {
public:

    /**
     * Constructor
     * @param node Location id of this instance (node location id).
     * @param io Reference to CanOutput class.
     * @param mon Reference to OvroSubsystem instance.
     */
    YigPll( carma::canbus::nodeType node, 
            carma::canbus::CanOutput & io,
            carma::monitor::OvroSubsystem & mon );

    /**
     * Destructor
     */
    ~YigPll();

    /**
     * Retrieve a map of this devices half second monitor points.
     * The monitor points returned from this routine will be simulated
     * if the device is in the OFFLINE state.
     * @return Map with descriptions of the device's half second monitor 
     * points keyed by message id.
     */
    std::map<carma::canbus::msgType, std::string> getHalfSecMonitors() const;

    /**
     * Return a map of this devices slow monitor points.
     * These monitor points will be simulated every 5 seconds if the device
     * is in the OFFLINE state.
     * @return Map with descriptions of the device's slow (5 second) monitor
     * points keyed by message id.
     */
    std::map<carma::canbus::msgType, std::string> getSlowMonitors() const;

    /**
     * Process a CAN message addressed from the YigPll module.
     * This routine is responsible for processing all CAN messages addressed
     * to this device. It is essentially a callback routine.
     * @param mid the 10bit message id (carma::canbus::msgType).
     * @param data reference to the byte vector containing the raw data.
     * @param sim True if message is simulated, false normally.
     * @see carma::canbus::Device::processMsg
     */
    void processMsg(carma::canbus::msgType mid,
                    std::vector<carma::canbus::byteType> &data, 
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
    
    /**
     * Update Frame Data.
     * This method is called (or called back) every 1/2 second in order to 
     * allow this class to update monitor data on the frame timescale.  Note
     * that this method is called regardless of the modules STATE or the 
     * number of messages the device is receiving.
     */
    void updateFrameData();
    
    // Public control commands.
    
    /**
     * Indicates the result of the lockYigFrequency command.
     */
    typedef enum LockResult {
        YIG_LOCKED,    /**< Yig locked successfully */
        YIG_UNLOCKED,  /**< Yig did not lock (error) */
        YIG_TIMEDOUT   /**< lockYigFrequency command timed out (error) */
    } LockResultType;
        
    /**
     * Set the YIG lock frequency and lock.
     * This method does not block while waiting for lock to complete.  
     * @param freqInGhz YIG lock frequency in GHz.
     * @see setYigFrequencyAndLock
     * @see setYigFrequencyWithoutLock
     */
    void setYigFrequencyAndLockNoBlock( double freqInGhz );

    /**
     * Set the YIG lock frequency and lock.
     * This command atomically sets the YIG output frequency, starts the lock 
     * sequence and waits for lock to complete.  It returns only after the YIG
     * locks, fails to lock or times out.  
     * @param freqInGhz YIG frequency value in GHz.
     * @return lock result.
     * @see setYigFrequency
     */
    LockResultType setYigFrequencyAndLock( double freqInGhz );
    
    /**
     * Set the Yig frequency but don't lock - Engineering only.
     * Sets the YIG output frequency.  The module will NOT try to 
     * phase lock.
     * @param freqInGhz Yig output frequency in GHz
     * @see setYigFrequencyAndLock
     */
    void setYigFrequencyWithoutLock( double freqInGhz );

    /**
     * Extract the tune table.
     * This command instructs the module to extract the tuning table 
     * from the 1-wire device and save it to ram.
     */
    void extractTuneTable();

    /**
     * Toggle sweep - Engineering only.
     * Turns the sweep on or off.  This is an engineering command.  The 
     * default sweep mode is on.
     * @param on Turn sweep on if true, off if false.
     */
    void toggleSweep(bool on);

    /**
     * Set damping resistance - Engineering only.
     * Sets PLL damping factor.
     * @param damping Damping factor in Ohms.
     */
    void setDampingResistance(unsigned short damping);

protected:
    
    // There are no protected methods.

private:

    // Disable assignment and copy construction.
    YigPll(const YigPll &);
    YigPll &operator=(const YigPll &);
    
    // Routines to process individual blanking frame CAN packets.
    // These routines are called by processMsg.
    void processBlankingFramePacket1(carma::canbus::DataVector &data);
    void processBlankingFramePacket2(carma::canbus::DataVector &data);
    void processBlankingFramePacket3(carma::canbus::DataVector &data);
    void processBlankingFramePacket4(carma::canbus::DataVector &data);
    void processBlankingFramePacket5(carma::canbus::DataVector &data);
    void processBlankingFramePacket6(carma::canbus::DataVector &data);

    // Routine to process state change broadcast message.
    void processLockStateChangePacket(carma::canbus::DataVector &data);

    // Routines to produce individual simulated blanking frame CAN packets.
    // These routines are called by simulateMsg.
    carma::canbus::Message simBlankingFramePacket1();
    carma::canbus::Message simBlankingFramePacket2();
    carma::canbus::Message simBlankingFramePacket3();
    carma::canbus::Message simBlankingFramePacket4();
    carma::canbus::Message simBlankingFramePacket5();
    carma::canbus::Message simBlankingFramePacket6();
    
    // Member variables
    log4cpp::Category &log_;  // Reference to the system logger
    carma::monitor::AntennaCommon &common_; // Reference to common MPs.
    carma::monitor::OvroSubsystem::Yig &mon_;
    
    typedef enum { // For internal state machine 
        IDLE,
        WAITING,
        UNLOCKED,
        LOCKED
    } SequenceStateType;

    // To cause the lockYigFrequency command to block while waiting for
    // the yig to lock, we incorporate a condition variable which is wrapped 
    // up in the below structure (along with the state (contains predicate data)
    // and associated mutex).  The processLockStateChange command is responsible
    // for updating the state and signalling to lockYigFrequency (the waiter),
    // when finished.
    struct {
        volatile SequenceStateType state; // Predicate state LOCKED || UNLOCKED
        carma::util::PthreadMutex mutex;  // Mutex to wait on lock
        carma::util::PthreadCond cond;    // Condition variable to signal lock
    } lockSequence_;

}; // End class YigPll
}}} // End namespace carma::antenna::ovro
#endif
