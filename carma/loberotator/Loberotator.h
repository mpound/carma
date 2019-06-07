/**
 * @file
 * Loberotator Device class definition for Carma CANbus API 113.
 *
 * @author Colby Kraybill, Steve Scott
 * $Revision: 1.68 $
 * $Date: 2012/02/01 21:45:07 $
 * $Id: Loberotator.h,v 1.68 2012/02/01 21:45:07 abeard Exp $
 *
 * $CarmaCopyright$
 */

#ifndef CARMA_LOBEROTATOR_H
#define CARMA_LOBEROTATOR_H

// UNIX includes, usleep...
#include <unistd.h>

// C++ Standard library includes
#include <map>
#include <string>
#include <vector>


// Carma includes
#include "carma/corba/corba.h"
#include "carma/canbus/devices/XacDevice.h"
#include "carma/monitor/LoberotatorSubsystem.h"
#include "carma/util/QuadraticInterpolatorNormal.h"
#include "carma/util/PhaseSwitchingImpl.h"

#include "carma/loberotator/Chassis.h"
#include "carma/loberotator/SimData.h"
#include "carma/loberotator/LoberotatorControl_skel.h"

#include <log4cpp/CategoryStream.hh>

namespace log4cpp {
    // Forward declaration
    class Category;
} // End namespace log4cpp

namespace carma {

namespace monitor {
    // Forward declaration
    class QuadMod;
    class Xac;
} // End namespace monitor


/**
 * The loberotator subsystem controls and monitors the
 * loberotator boards.  These boards provide phase
 * tracking of sources by applying phase and frequency
 * information to a 50 MHz carrier that is added to the reference LO,
 * based on inputs from the DelayEngine.
 *
 * Critical details in dealing with the hardware:
 * <UL>
 *  <LI>The phase and rate are updated every half second and
 *       should be sent in the middle of the blanking
 *       frame, i.e., 250msec after the integral half second.
 *  <LI>The Janz hardware on the Linux host has an output buffer
 *      with a capacity of 400 messages.
 *  <LI>The CANnode XAC hardware has a buffer for each channel
 *      that is only one deep. Two consecutive messages **must not** be sent
 *      to a node or it will scramble the contents.
 *  <LI>The bus time for a single message is about 120 microseconds,
 *      giving about 8 messages per millisecond. This can used to advantage
 *      by placing other messages inbetween any two messages to the same
 *      node.
 *  <LI>Interference between random CANbus control messages and the
 *      half second phase and rate updates can be handled by sending
 *      a bogus guard message before any control command. This uses
 *      the output queue and the bus timing to give a 120usec delay
 *      between receipt of any two messages at the XAC.
 *  <LI>Changing the number of channels in the system (Chassis::N_CHAN)
 *      could have a deleterious effect on the system because it changes
 *      the timing.
 * </UL>
 *
 * The relevant classes containing public interfaces of
 * interest to client programs:
 * <UL>
 * <LI><b>LoberotatorControl</b> is the distributed object
 * interface. This the what the DelayEngine talks to.</LI>
 *
 * <LI><b>carmaLoberotatorHost</b> is the program to start
 * the Loberotator class.</LI>
 * </UL>
 *
 * The canbus interface for the Loberotator systems is documented in
 * CANBus API No. 113.
 * @see http://www.mmarray.org/project/system/CanAPI/Docs/API_113_LobeRotatorVerG.pdf
 * @see http://www.mmarray.org/project/WP/LobeRotator/hw/
 * @see http://www.mmarray.org/project/WP/LobeRotator/sw/
 *
 */
namespace loberotator {

// Forward declaration
class LoberotatorMaster;

/**
 * Loberotator implementation
 *
 * This class implements:
 * <UL>
 *  <LI>carma::loberotator::LoberotatorControl</LI>
 *  <LI>carma::canbus::Device</LI>
 * </UL>
 */
class Loberotator :
        public carma::canbus::devices::XacDevice,
        public carma::util::PhaseSwitchingImpl
{
public:

    /**
     * Constructor for a loberotator channel.
     * Creates a Loberotator device with the given node id.
     * This is a single loberotator channel that is implemented in
     * hardware by a single DDS (Direct Digital Synthesizer).
     * There are four DDS channels per loberotator board, with each
     * board having an XAC. The XAC communications is non-standard:
     * each board is assigned an address in groups of four, so that
     * the boards have base addresses of 1,5,9... Each individual
     * DDS is command addressable using the four addresses assigned
     * to the board. The monitor packets are only sent back for the base
     * address and contain info for all 4 DDS channels. <BR>
     * The CORBA commands are all handled by the "Global" loberotator,
     * which is assigned node address zero. The implementation gets the
     * channel number from the command and then invokes the command on
     * on the appropriate DDS. The individual Loberotator objects are
     * used to hold control information specific to that device.
     *
     * @param node Node id of device.
     * @param mon Pointer to monitor subsystem for passing back monitor
     *   information.
     */
    Loberotator(carma::canbus::nodeType node,
           carma::loberotator::LoberotatorMaster* master,
           carma::monitor::LoberotatorSubsystem* mon);

    /**
     * Destructor
     */
    virtual ~Loberotator();

    /**
     * Compute phase and rates for this channel and leave internally
     */
    void updatePhaseAndRate();

    /**
     * Send multiplier/divisor for this channel to the CANnode
     */
    void sendMulDiv();

    /**
     * Send phase and rates for this channel to the CANnode
     */
    void sendPhaseAndRate();


    //------------------------------------------------------------
    // External API
    //------------------------------------------------------------
#if 0
    void setDelay(const carma::loberotator::LoberotatorControl::Delays& delay )
     __attribute__((deprecated));

    void setLOFreq(
        const carma::loberotator::LoberotatorControl::Frequencies& freq)
     __attribute__((deprecated));
#endif

    void updateDelayAndFreq(
        const loberotator::LoberotatorControl::DelayFreqPacket& delayFreq);

    void loadPhaseSwitchColumn( CORBA::Long inputId, CORBA::Short columnId);

    void assignWalshColumn(
            const loberotator::LoberotatorControl::WalshAssignmentSeq& walshAssignments);

    void enableDDS(CORBA::Long chan, bool state);

    void enable90PhaseSwitching(CORBA::Long chan, bool state);

    void enable180PhaseSwitching(CORBA::Long chan, bool state);

    void enablePhaseSwitching(CORBA::Long chan, bool state);

    void enableFringeTracking(CORBA::Long chan, bool state);

    void enableDDSfringeTracking(CORBA::Long chan, bool state );

    void resetBoard(CORBA::Long BoardNo);

    void hardReset();

    void enableTrace(CORBA::Long chan, bool enable);

    void enableUpdate(bool enable);

    //------------------------------------------------------------
    // Offset (artificial) fringe tracking.
    //------------------------------------------------------------
    void setOffsetControl(CORBA::Long chan, bool state);

    void setOffsetPhase(CORBA::Long chan, double phaseOffset);

    void setOffsetRate(CORBA::Long chan, double rateOffset);

    //------------------------------------------------------------
    // External interface helper methods
    //------------------------------------------------------------
    void enableFringeTracking(bool state);
    void setOffsetControl(bool state);
    void setOffsetPhase(double phaseOffset);
    void setOffsetRate(double rateOffset);

private:
    // Copy and assignment not permitted for this class.
    Loberotator( const Loberotator& );
    Loberotator& operator=( const Loberotator& );
    //------------------------------------------------------------
    // Internal fringe tracking methods
    //------------------------------------------------------------

    // Update the delay interpolator for a DDS channel
    void setDelay(
	    const carma::loberotator::LoberotatorControl::DelayChan & delayChan);

    // Send phase and rate to the XAC,
    // to be clocked in on the next half-second boundary.
    void setPhaseAndRate(CORBA::Long ddsId,
                CORBA::ULong phase,
                CORBA::Long dphase);
    // Send walsh column to the XAC
    void sendWalshColumn(unsigned char walshColumn);

    //------------------------------------------------------------
    // Utility methods
    //------------------------------------------------------------
    // Board index [0-5], derived from the node numbers (1,5,9...)
    int getBoardIndex();
    // Get a ref to another loberotator; chanIndex=0 gives nodeID=1
    Loberotator& lr(int chanIndex);
    // CORBA range check channelNo input [1-N_CHAN];
    // throws CORBA UserException if out of range
    void checkChanRange(int chanNo);
    // CORBA range check channelNo input [0-N_CHAN], including broadcast chan
    // throws CORBA UserException if out of range
    void checkFullChanRange(int chanNo);
    // Create a message destined for a specific channel
    carma::canbus::Message createMsgToChan(int chanNo,
            carma::canbus::msgType msgID);
    // Puts a bogus guard message into the output queue.
    void sendGuardMessage();
    // Get the monitor channel in which to put command values
    monitor::LoberotatorSubsystem::Channel& getMonCommandChannel();
    // As above, but input LR channel number
    monitor::LoberotatorSubsystem::Channel& getMonCommandChannel(int chan);

    //------------------------------------------------------------
    // Monitoring methods
    //------------------------------------------------------------
    // Methods to process individual CAN messages.  These routines
    // are called by processMsg.
    void processPhasePacket(std::vector<carma::canbus::byteType> &data, int pkt);
    void processPacket5(std::vector<carma::canbus::byteType> &data);
    void processPacket6(std::vector<carma::canbus::byteType> &data);
    void processPacket7(std::vector<carma::canbus::byteType> &data);
    void processPacket8(std::vector<carma::canbus::byteType> &data);
    void processPacket9(std::vector<carma::canbus::byteType> &data);
    void processPacket10(std::vector<carma::canbus::byteType> &data);
    void processPacket11(std::vector<carma::canbus::byteType> &data);

    void processFastMonitorChan1(std::vector<carma::canbus::byteType> &data);
    void processFastMonitorChan2(std::vector<carma::canbus::byteType> &data);
    void processFastMonitorChan1Data(
            std::vector<carma::canbus::byteType> &data );
    void processFastMonitorChan2Data(
            std::vector<carma::canbus::byteType> &data );

    //------------------------------------------------------------
    // Data members
    //------------------------------------------------------------
    // Reference to masterLoberotator
    LoberotatorMaster& master_;
    LoberotatorMaster& master();

    //------------------------------------------------------------
    // Data members, simulators
    //------------------------------------------------------------
    SimData*        phaseSim_;
    SimData&        phaseSim();
    SimData*        rateSim_;
    SimData&        rateSim();
    SimData*        tempSim_;
    SimData&        tempSim();
    SimData*        ps5vaSim_;
    SimData&        ps5vaSim();
    SimData*        ps5vdSim_;
    SimData&        ps5vdSim();
    SimData*        psNeg5vSim_;
    SimData&        psNeg5vSim();
    SimData*        ps24vSim_;
    SimData&        ps24vSim();
    SimIntegerData* psColumnStateSim_;
    SimIntegerData& psColumnStateSim();
    SimIntegerData* controlStateSim_;
    SimIntegerData& controlStateSim();
    SimIntegerData* ppsStateSim_;
    SimIntegerData& ppsStateSim();
    SimIntegerData* hbStateSim_;
    SimIntegerData& hbStateSim();
    SimIntegerData* psStateSim_;
    SimIntegerData& psStateSim();
    SimIntegerData* dataValidSim_;
    SimIntegerData& dataValidSim();
    SimIntegerData* timeOffsetSim_;
    SimIntegerData& timeOffsetSim();

    // Data items for calculating and controlling phase and rate
    carma::util::QuadraticInterpolatorNormal delayInterp_;
    double LO1Freq_;       // In GHz
    long   multiplier_;
    long   divisor_;
    long   sign_;
    double phaseOffset_;    // Phase in turns
    double rateOffset_;     // Rate in Hz (turns/sec)
    bool   offsetControl_;  // Turn on/off
    bool   fringeTracking_; // Turn on/off
    double delayUpdateTimestamp_;  // Time of last update
    static bool updateEnabled_;
    unsigned long outputPhase_;
    long          outputRate_;
    short         outputMul_;
    short         outputDiv_;

    carma::util::PhaseSwitchTable* phaseSwitchTable_;

    // Reference to the monitor subsystem
    carma::monitor::LoberotatorSubsystem* mon_;
    carma::monitor::LoberotatorSubsystem& mon();

    // Logger
    log4cpp::Category& logger_;
    log4cpp::Category& log();

    // Command log
    bool cmdLoggingEnabled();
    // Maybe needs to be DEBUG?
    static const log4cpp::Priority::PriorityLevel
            CMD_LOG_PRIORITY = log4cpp::Priority::INFO;
    log4cpp::CategoryStream cmdlog();

    // Trace
    bool traceEnabled_;
    static const int traceInterval_ = 2;

    // Override device methods
    std::map<carma::canbus::msgType, std::string> getHalfSecMonitors() const;
    std::map<carma::canbus::msgType, std::string> getSlowMonitors() const;
    carma::canbus::Message simulateMsg(carma::canbus::msgType mid);

    static const carma::canbus::apiType API_ID = 113;
    static carma::canbus::apiType getApiId();

    // Message processing
    void processMsg(carma::canbus::msgType mid,
                    std::vector<carma::canbus::byteType> &data, bool sim);
    carma::canbus::Message simPhasePacket(int chanNo);
    carma::canbus::Message simPacket5();
    carma::canbus::Message simPacket6();
    carma::canbus::Message simPacket7();

    // Commands
    static const carma::canbus::msgType NODE_RESET             = 0x000;
    static const carma::canbus::msgType SET_PHASE_AND_RATE     = 0x040;
    static const carma::canbus::msgType SET_MULTIPLIER         = 0x041;
    static const carma::canbus::msgType DISABLE_PHASESWITCH90  = 0x042;
    static const carma::canbus::msgType DISABLE_PHASESWITCH180 = 0x043;
    static const carma::canbus::msgType DISABLE_LOBEROTATION   = 0x044;
    static const carma::canbus::msgType BEGIN_LOAD_COLUMN      = 0x045;
    static const carma::canbus::msgType LOAD_COLUMN            = 0x046;
    static const carma::canbus::msgType SELECT_WALSH_COLUMN    = 0x047;

    // Half second packets
    static const carma::canbus::msgType BLANK_FRAME_PACKET_1   = 0x0E0;
    static const carma::canbus::msgType BLANK_FRAME_PACKET_2   = 0x0E1;
    static const carma::canbus::msgType BLANK_FRAME_PACKET_3   = 0x0E2;
    static const carma::canbus::msgType BLANK_FRAME_PACKET_4   = 0x0E3;
    static const carma::canbus::msgType BLANK_FRAME_PACKET_5   = 0x0E4;
    static const carma::canbus::msgType BLANK_FRAME_PACKET_6   = 0x0E5;
    static const carma::canbus::msgType BLANK_FRAME_PACKET_7   = 0x0E6;
    static const carma::canbus::msgType BLANK_FRAME_PACKET_8   = 0x0E7;
    static const carma::canbus::msgType BLANK_FRAME_PACKET_9   = 0x0E8;
    static const carma::canbus::msgType BLANK_FRAME_PACKET_10  = 0x0E9;
    static const carma::canbus::msgType BLANK_FRAME_PACKET_11  = 0x0EA;


    // Fast monitoring packets
    static const carma::canbus::msgType FAST_CHAN_1      = 0x110;
    static const carma::canbus::msgType FAST_CHAN_2      = 0x111;
    static const carma::canbus::msgType FAST_CHAN_1_DATA = 0x112;
    static const carma::canbus::msgType FAST_CHAN_2_DATA = 0x113;

};  // Class Loberotator


} // Namespace loberotator
} // Namespace carma


#endif // CARMA_LOBEROTATOR_H
