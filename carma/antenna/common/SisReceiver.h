/** @file
 * CAN Device declaration for 10m & 6m SIS Receiver Control.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.7 $
 * $Date: 2011/01/03 18:48:05 $
 * $Id: SisReceiver.h,v 1.7 2011/01/03 18:48:05 iws Exp $
 *
 * $CarmaCopyright$
 */
#ifndef CARMA_ANTENNA_COMMON_SISRECEIVER_H
#define CARMA_ANTENNA_COMMON_SISRECEIVER_H

// Carma includes
#include "carma/canbus/devices/XacDevice.h"
#include "carma/canbus/Types.h"
#include "carma/util/PthreadMutex.h"

// C++ Standard Library Includes
#include <map>


namespace log4cpp {
    // Forward dec
    class Category;
} // End namespace log4cpp

namespace carma {

namespace monitor {
    class AntennaCommon;
    class SisReceiver;
    class StateMonitorPointEnum;
    class Xac;
} // End namespace monitor

namespace antenna {
namespace common {

class AntennaIF;

/**
 * 10m & 6m SIS Receiver Control CAN module device class.
 */
class SisReceiver : public carma::canbus::devices::XacDevice {
public:

    /**
     * Constructor
     * @param node Location id of this instance (node location code).
     * @param io Reference to CanOutput class.
     * @param antCommon Reference to antenna common monitor instance.
     * @param state Reference to canbus state monitor instance.
     * @param sis Reference to SisReceiver monitor instance.
     * @param xac Reference to Xac monitor instance.
     * @param antIF Reference to AntennaIF monitor instance.
     */
    SisReceiver( carma::canbus::nodeType node, 
                 carma::canbus::CanOutput & io,
                 carma::monitor::AntennaCommon & antCommon,
                 carma::monitor::StateMonitorPointEnum & state,
                 carma::monitor::SisReceiver & sis,
                 carma::monitor::Xac & xac,
                 carma::antenna::common::AntennaIF & antIF );

    /**
     * Destructor
     */
    virtual ~SisReceiver();

    /**
     * Retrieve brief description of this devices half second monitor points.
     * @see carma::canbus::Device::getHalfSecMonitors
     * @return MsgBriefMap containing string descriptions of device's half 
     * second monitor points keyed by message id.
     */
    carma::canbus::MsgBriefMap getHalfSecMonitors() const;

    /**
     * Retrieve brief description of this devices slow (5 second) monitor 
     * points.
     * @see carma::canbus::Device::getSlowMonitors
     * @return MsgBriefMap containing string descriptions of device's slow
     * monitor points keyed by message id.
     */
    carma::canbus::MsgBriefMap getSlowMonitors() const;

    /**
     * Process a CAN message from the SisReceiver module.
     * This routine is responsible for processing all CAN message addressed
     * to this device.  
     * @see carma::canbus::Device::processMsg
     * @param mid the 10bit message id (carma::canbus::msgType).
     * @param data Reference to the DataVector containing raw data.
     * @param sim True if message is a simulated message.
     */
    void processMsg(carma::canbus::msgType mid,
                    carma::canbus::DataVector &data,
                    bool sim);

    /**
     * Simulate a CAN message for the given message id.
     * @param Id of message to simulate.
     * @see carma::canbus::Device::simulateMsg
     */
    carma::canbus::Message simulateMsg(carma::canbus::msgType mid);

    /**
     * Update Frame Data.
     * Callback routine to update frame timescale, device dependent information
     * in the monitor stream (e.g. ONLINE state).
     */
    void updateFrameData();

    // Public control commands.
    /**
     * Tune mixer.
     * Instructs mixer to adjust the junction voltage and current to the 
     * optimum values for the specified frequency.
     * @param frequencyInGhz LO frequency in GHz.
     */
    void tuneMixer( float frequencyInGhz ) const;

    /**
     * Set Vj.
     * Sets the SIS junction bias voltage.
     * @param VjInMilliVolts Junction voltage in milliVolts.
     */
    void setVj( float VjInMilliVolts ) const;

    /**
     * Set Ij
     * Sets the SIS junction current.
     * @param IjInMicroAmps Junction current in microAmps.
     */
    void setIj( float IjInMicroAmps ) const;

    /**
     * Set LO attenuation
     * Sets the LO attenuation.
     * @param loAttenPercent LO Attenuation in %.
     */
    void setLoAttenuation( float loAttenPercent ) const;

    /**
     * Set Vd.
     * Sets the drain voltage on the specified HEMT stage.
     * @param stageNo HEMT stage to set drain voltage on [1-4].
     * @param VdInVolts HEMT drain voltage in Volts.
     */
    void setVd( short stageNo, float VdInVolts ) const;

    /**
     * Set Id.
     * Sets the drain current on the specified HEMT stage.
     * @param stageNo HEMT stage to set drain current on [1-4].
     * @param Id HEMT drain current in milliAmps.
     */
    void setId( short stageNo, float IdInMilliAmps ) const; 

    /**
     * Set Vg.
     * Sets the gate voltage on the specified HEMT stage.
     * @param stageNo HEMT stage to set gate voltage on [1-4].
     * @param Vg HEMT gate voltage in Volts.
     */
    void setVg( short stageNo, float Vg ) const;

    /**
     * Do IV-Curve
     * Measures and reports the junction IV-curve.  The number of points that 
     * will be returned is:
     *  N=trunc((Finish - Start)/Step) + 1)
     * The points are returned in message type 0x170.  The step may be 
     * positive or negative to control the direction the curve is traced in.
     * @param start Starting junction voltage in milliVolts.
     * @param stop Stopping junction voltage in milliVolts.
     * @param step Voltage stop size in milliVolts.
     * @param delta Time interval between samples in milliseconds.
     * @param seqNo Sequence number to set in monitor stream when complete:
     *        if 0 it is ignored otherwise antennacommon.receivers.tuneSeqNo
     *        is set upon completion.
     * @param doPower If true, module will also instruct the PAM module to
     *        send back total power measurements.
     */
    void doIVCurve( float startInMilliVolts, 
                    float stopInMilliVolts, 
                    float stepInMilliVolts, 
                    int timeDeltaMilliSeconds,
                    int seqNo,
                    bool doPower );

    /**
     * Current Mode Enumerator
     */
    typedef enum {
        STORED   = 0x00,   /**< Use value stored in module */
        MESSAGE  = 0x01,   /**< Use value in this message */
        IV_CURVE = 0x02    /**< Determine current from IV-curve measurement */
    } CurrentModeType;
    
    /**
     * Get Vgap
     * Measures the junction gap voltage, which is reported in the blanking 
     * frames. The LO attenuation is set to maximum during the measurement.
     * The new value is reported in the blanking frame monitor packets.
     * @param mode What current mode to measure the gap at.
     * @param Igap Current to use for Vgap measurement in microAmps.
     */
    void getVgap( CurrentModeType mode, float Igap ) const;

    /**
     * Set Igap
     * Sets the value of current to be used as the default for Igap 
     * measurements.
     * @param Igap Default current to use for Igap in microAmps.
     */
    void setIgap( float Igap ) const;

    /** 
     * Loop Mode Enumerator
     */
    typedef enum {
        VJ_CLOSED = 0x00, /**< Continuous adjustment of Vj to set value. */
        VJ_OPEN   = 0x01, /**< DAC is set to nominal value for mixer voltage. */
        VJ_FINITE = 0x02  /**< Use a fixed number of iterations. */ 
    } VjLoopModeType;

    /**
     * Set Vj Loop Mode
     * Determines whether the voltage on the mixer is controlled open or 
     * or closed (software) loop.
     * @param mode LoopMode to set.
     */
    void setVjLoopMode(VjLoopModeType mode) const;

    /**
     * Ij Loop Mode Enumerator
     */
    typedef enum {
        IJ_CLOSED = 0x00, /**< Continuous adjustment of Ij to set value. */
        IJ_FINITE = 0x01  /**< Use a fixed number of iterations. */
    } IjLoopModeType;

    // These IV Curve definitions exist independently from the idl definitions
    // to maintain separation of canbus device impls from specific control 
    // communication mechanisms.

    /**
     * Set Ij Loop Mode
     * Determines whether the current in the mixer is controlled continuously
     * or periodically adjusted.
     * @param mode IjLoopMode to set.
     */
    void setIjLoopMode(IjLoopModeType mode) const;

    /**
     * A point on the IV curve.
     */
    struct IVPoint {
        double fjd; /**< Fractional Julian Day */
        float Ij; /**< Current in microAmps */
        float Vj; /**< Voltage in milliVolts */
    };

    typedef ::std::vector< IVPoint > IVDataVec;

    /**
     * An IV curve.
     */
    struct IVCurve {
        IVDataVec data;
        bool doPowerRequested;
        IVCurve( ) : doPowerRequested( false ) { };
    };

    /**
     * Retrieve IV Curve.
     * Retrieve an IV Curve initiated by the doIVCurve command. Returns an
     * empty map if either an IV curve hasn't been collected, is in progress 
     * or didn't complete properly before hand.
     */
    IVCurve getIVCurve();

    static const carma::canbus::msgType RX_3MM_LEFT_POL_NODE_ID      = 1;
    static const carma::canbus::msgType RX_3MM_RIGHT_POL_NODE_ID     = 2;
    static const carma::canbus::msgType RX_1MM_LEFT_POL_NODE_ID      = 3;
    static const carma::canbus::msgType RX_1MM_RIGHT_POL_NODE_ID     = 4;
    
protected:

    // Method to process IV Curve Point
    virtual void processIVCurvePoint(carma::canbus::DataVector &data);
    virtual void processBlankingFramePacket1(carma::canbus::DataVector &data);
    virtual void processBlankingFramePacket3(carma::canbus::DataVector &data);

private:

    // Copying or assignment not allowed.
    SisReceiver(const SisReceiver &);
    SisReceiver &operator=(const SisReceiver &);

    // Methods to process individual blanking frame packets.
    virtual void processBlankingFramePacket2(carma::canbus::DataVector &data);
    virtual void processBlankingFramePacket4(carma::canbus::DataVector &data);
    virtual void processBlankingFramePacket5(carma::canbus::DataVector &data);
    virtual void processBlankingFramePacket6(carma::canbus::DataVector &data);
    virtual void processBlankingFramePacket7(carma::canbus::DataVector &data);
    virtual void processBlankingFramePacket8(carma::canbus::DataVector &data);

    
    // Methods to produce individual simulated blanking frame CAN packets.
    carma::canbus::Message simBlankingFramePacket1();
    carma::canbus::Message simBlankingFramePacket2();
    carma::canbus::Message simBlankingFramePacket3();
    carma::canbus::Message simBlankingFramePacket4();
    carma::canbus::Message simBlankingFramePacket5();
    carma::canbus::Message simBlankingFramePacket6();
    carma::canbus::Message simBlankingFramePacket7();
    carma::canbus::Message simBlankingFramePacket8();

    // Member variables
    log4cpp::Category &log_; // Reference to system logger.
    carma::monitor::AntennaCommon & common_;
    carma::monitor::StateMonitorPointEnum & state_;
    carma::monitor::SisReceiver & mon_;
    carma::monitor::Xac & xacMon_;
        
    IVCurve ivCurve_; // Last IV curve.
    carma::util::PthreadMutex ivCurveMutex_; // Mutex protecting it..

    // Structure to facilitate the collection of IV Curve points and 
    // assembly into a complete IV curve.
    typedef struct {
        int pendingSequenceNumber;
        unsigned int nPointsExpected;
        unsigned int nPointsReceived;
        IVCurve tempIVCurve; // Temporary IV curve. 
        carma::util::PthreadMutex mutex; // Mutex to protect shared data.
    } IVCurveCollationInfo;

    IVCurveCollationInfo ivCurveCollationInfo_; 
    carma::antenna::common::AntennaIF & antIF_;
    
}; // End class SisReceiver
}}} // End namespace carma::antenna::common
#endif
