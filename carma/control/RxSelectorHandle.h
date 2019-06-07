
#ifndef CARMA_RX_SELECTOR_HANDLE_H
#define CARMA_RX_SELECTOR_HANDLE_H

/**
 * @file
 *
 * Carma control interface to the antenna receiver selector,
 * and any receiver methods from the selected receivers.
 *
 * @author: Steve Scott
 *
 * $CarmaCopyright$
 *
 */
 

#include "carma/corba/corba.h"
#include "carma/antenna/common/RxControl.h"
#include "carma/antenna/common/RxSelector.h"
#include "carma/control/RemoteObjHandleT.h"
#include "carma/control/SubarrayControl_skel.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/MonitorSystem.h"

namespace carma {

namespace monitor {

    class MonitorSystem;

} // namespace monitor
namespace control {

typedef RemoteObjHandleT<carma::antenna::common::RxSelector> 
                           RxSelectorRemoteObjHandle;

//! @brief Manages antenna Rx selector control DO connections
class RxSelectorHandle : public RxSelectorRemoteObjHandle {

public:
    /**
     * Constructor
     *
     * @param carmaAntNo  Carma antenna number
     * @param monitorSystem carma::monitor::MonitorSystem&
     *              monitor system, which allows RxSelect handle to get a 
     *              reference to its own monitor stream.
     * @param antenna Antenna control monitor system, which contains control
     *                and monitor points set by the control subsystem.
     *
     */
    RxSelectorHandle( const unsigned short                     carmaAntNo, 
                      monitor::MonitorSystem &                 monitorSystem,
                      monitor::ControlSubsystemBase::Antenna & antenna );

    /**
     * Destructor - releases object (DO) references.
     */
     virtual ~RxSelectorHandle();

    /**
     * Get a receiver and set its frequency
     * The preferred sequence number is compared to the one 
     * currently returned by the monitor system, and if they are the same
     * then one that is ten greater will be used.
     * This sequence number is 
     * stored internally so that it can be compared against
     * the one returned by the antenna in the monitor stream.
     * @param rxType receiver to set freq on (3mm/1mm/1cm) 
     * @param yigFreq in GHz
     * @param loFreq in GHz
     * @param refFreq in GHz
     * @param harmonic multiplier of yig to get oscillator freq
     * @param endWithAbsorberInBeam control variable
     * @param optimizeReceiver control variable
     * @param forceRelock control variable, only used by 6m
     * @param controlSubsystem used to record refFreq & harmonics
     * @param monsys Monitor system to be used to check seq no
     * @param preferredSequenceNo will be used unless it is already
     * @see isActionComplete
     */
     void setFrequency( antenna::common::RxControl::Type rxType, 
                        double yigFreq, 
                        double loFreq, 
                        double refFreq, 
                        int    harmonic, 
                        bool endWithAbsorberInBeam,
                        bool optimizeReceiver,
                        bool forceRelock,
                        monitor::ControlSubsystem* controlSubsystem,
                        monitor::MonitorSystem* monsys, 
                        const int preferredSeqNo );   

    /**
     * Get a receiver and set its IF output power
     * @param rxType receiver to set power level on (3mm/1mm/1cm) 
     * @param power level in volts
     */
    void antennaIFpower( antenna::common::RxControl::Type rxType,
                         double power);   

    /**
     * Get a receiver and set its IF output power to the preset level
     * @param rxType receiver to set power level on (3mm/1mm/1cm) 
     * @param polarization (IF selection; IF1 or IF2)
     */
    void antennaIFpresetPower( antenna::common::RxControl::Type rxType );
 
    /**
     * Get a receiver and set its IF attenuation to a specific value
     * @param rxType receiver to set power level on (3mm/1mm/1cm) 
     * @param polarization (IF selection; IF1 or IF2)
     * @param atten attenuation in dB
     */
    void antennaIFatten( antenna::common::RxControl::Type rxType,
                         antenna::common::RxControl::IF_Type polarization,
                         double atten);  
	    
    /**
     * Get a receiver and set its front end junction voltage
     * @param rxType receiver to set voltage on (3mm/1mm/1cm) 
     * @param vj juntion voltage
     */
     void vj( antenna::common::RxControl::Type rxType, 
              antenna::common::RxControl::Pol_Type pol,
              float vj );
 
    /**
     * Get a receiver and set its front end junction current
     * @param rxType receiver to set current on (3mm/1mm/1cm) 
     * @param ij juntion current
     */
     void ij( antenna::common::RxControl::Type rxType, 
              antenna::common::RxControl::Pol_Type pol,
              float ij );

    /**
     * Set the reference LO attenuator
     * @param atten attenuation in dB
     */
     void setRefAtten(unsigned short atten);

    /**
     * Kick off an IV curve with a preferred sequence number.
     * @see carma::control::SubarrayControl::doIVcurve
     * @param rxType receiver to set freq on (3mm/1mm/1cm) 
     * @param startVjInMv Starting junction voltage in mV.
     * @param stopVjInMv Ending junction voltage in mV.
     * @param stepVjInMv Voltage step size in mV.
     * @param deltaInMs Time between samples in ms (rounded to 100ms step).
     * @param doPower If true, instruct the PAM module to send total power 
     *        values along with the IV curve information from the receivers
     *        (currently OVRO only).
     * @param preferredSequenceNo will be used unless it is already
     */

    struct IVcurveArgs {
        CORBA::Float startVjInMv;
        CORBA::Float stopVjInMv;
        CORBA::Float stepVjInMv;
        CORBA::UShort deltaInMs;
        CORBA::Boolean doTotalPower;
    };
        
    void doIVcurve( carma::antenna::common::RxControl::Type rx,
                    carma::antenna::common::RxControl::Pol_Type pol,
                    const IVcurveArgs ivCurveParams,
                    carma::monitor::MonitorSystem * const   monsys, 
                    int preferredSeqNo );

    /**
     * Retrieve that same IV curve.
     */
    carma::antenna::common::IVCurve * getIVcurve( );

    /*
     * Get the carma antenna number
     * @return the carma antenna number
     */
     unsigned short getCarmaAntennaNo( ) const;

    /**
     * Compare next sequence number with one returned by monitor system.
     * If they are the same then the last action is complete.
     * @param monsys monitor system from which to retrieve completion
     * The monsys should already be placed in the queue, *don't* do
     * a read or anything that will disturb the queue.
     * @param monitorDataErrorLimit number of consecutive monitor data
     * invalid limit before thowing an exception
     * @return true if last action is complete
     * @throws if number of consecutive monitor data errors is exceeed
     */
    bool isActionComplete( const monitor::MonitorSystem & monsys,
                           int                            monDataErrorLimit );


private:
    void handleNilRx(antenna::common::RxControl::Type rxType ) const;
    void handleNilIF(antenna::common::RxControl::Type rxType ) const;
    void handleNilFrontEnd(antenna::common::RxControl::Type rxType ) const;
    const unsigned short carmaAntNo_;
    int                  nextSequenceNo_;
    int                  consecutiveErrorCount_;
    antenna::common::RxControl::Type pendingIvCurveRxType_;
    antenna::common::RxControl::Pol_Type pendingIvCurvePolType_;
};


} }  // End namespace carma/control


#endif // CARMA_CONTROL_RX_SELECTOR_HANDLE_H

