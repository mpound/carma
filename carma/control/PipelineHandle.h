#ifndef CARMA_CONTROL_PIPELINE_HANDLE_H
#define CARMA_CONTROL_PIPELINE_HANDLE_H

/**
 * @file
 *
 * Carma control interface to the pipeline control.
 *
 * $CarmaCopyright$
 *
 */
 

#include "carma/corba/corba.h"
#include "carma/control/RemoteObjHandleT.h"
#include "carma/control/SubarrayControl.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/pipeline/pipelineControl.h"
#include "carma/util/SeqTypedefs.h"

typedef carma::monitor::ControlBandPoints CBP;

namespace carma {
namespace control {


typedef RemoteObjHandleT< pipeline::PipelineControl > 
        PipelineControlRemoteObjHandle;


//! @brief Manages pipeline control DO connections
class PipelineHandle : public PipelineControlRemoteObjHandle {
    public:
        /**
         * Constructor
         * @param DOname name of the pipeline DO
         * @param controlSubsystem  writable control subsystem that will be
         *        used to store changes to decimation and keepEndChannels
         * @param monitorSystem monitor system reference, used for readNewest()
         * @param reachableMP the reachable MP in the Control subsystem,
         *        array specific reachability section
         * @param pipelineMonitorSystem subsystem that is used to see if
         *        monitor system is current for use in reachability algorithm
         * @param msgPrefix prepended to some messages to indicate which 
         *        pipeline the message originates (e.g. C3gMax23)
         */
        PipelineHandle(
            const std::string&         DOname,
            monitor::ControlSubsystem& controlSubsystem,
            monitor::MonitorSystem&    monitorSystem,
            monitor::MonitorPointBool& reachableMP,
            monitor::MonitorSubsystem& pipelineMonitorSystem,
            const int                  firstAstroBandNo,
            const std::string&         msgPrefix);
    
        virtual ~PipelineHandle();
        
        void setDecimationMode(const DecimationMode decimationMode,
                               const unsigned short astroBandNo);

        void integrate(double intTime, CORBA::Long numInts,
                       double gap, CORBA::Boolean science, 
                       int preferredSeqNo );  

        /**
         * Compare next sequence number with one returned by monitor system.
         * If they are the same then the last integration is complete.
         * @param monsys monitor system from which to retrieve completion
         * @param monitorDataErrorLimit number of consecutive monitor data
         * invalid limit before thowing an exception
         * @return true if last action is complete
         * @throws if number of consecutive monitor data errors is exceeed
         */
        bool isIntegrationComplete(const monitor::MonitorSystem& monsys,
                                   int monErrLimit);

        /**
         * Reset tsys for given carma antenna number sequence.
         * @param carmaAntNoSeq Sequence of antennas - zero NOT accepted as
         *         subarray membership is determined by caller.
         */
        void resetTsys(carma::util::SeqShort carmaAntNoSeq); 

    protected:
        virtual monitor::MonitorPointInt&   getSeqNoMP() = 0;
        virtual void decimate(const bool dec, const unsigned short astroBandNo);
        monitor::ControlSubsystem& controlSubsystem_;
        monitor::MonitorSystem&    monitorSystem_;
        const int                  firstAstroBandNo_;
        // Prefix for messages (e.g. "C3Gmax8")        
        const std::string          msgPrefix_;
        std::vector<CBP*>          CBPvector_;
        
    private:
        void keepEndChannels(const bool value, const unsigned short astroBandNo);

        // Sequence number boilerplate
        int nextSequenceNo_;
        int consecutiveErrors_;
        int errLimit_;
 };

// ===========================================================================
// ************** Derived classes for specific pipeline handles **************
// Derived classes for specific pipeline handles
class SLPipelineHandle : public PipelineHandle {
    public:
        /**
         * Constructor
         * @param controlSubsystem  writable control subsystem that will be
         *        used to store changes to decimation and keepEndChannels
         * @param monitorSystem monitor system reference 
         * @param reachableContainer reference to the subarray specific
         *        monitor system container for reachabilities
         */
        SLPipelineHandle(
            monitor::ControlSubsystem&                controlSubsystem,
            monitor::MonitorSystem&                   monitorSystem,
            monitor::ControlSubsystemBase::Reachable& reachableContainer);

        monitor::MonitorPointInt& getSeqNoMP();
};

class WBPipelineHandle : public PipelineHandle {
    public:
        /**
         * Constructor
         * @param controlSubsystem  writable control subsystem that will be
         *        used to store changes to decimation and keepEndChannels
         * @param monitorSystem monitor system reference 
         * @param reachableContainer reference to the subarray specific
         *        monitor system container for reachabilities
        */
        WBPipelineHandle(
            monitor::ControlSubsystem&                controlSubsystem,
            monitor::MonitorSystem&                   monitorSystem,
            monitor::ControlSubsystemBase::Reachable& reachableContainer);

        monitor::MonitorPointInt& getSeqNoMP();
        void decimate(const bool dec, const unsigned short astroBandNo);
};
 
class C3gMax8PipelineHandle : public PipelineHandle {
    public:
        /**
         * Constructor
         * @param controlSubsystem  writable control subsystem that will be
         *        used to store changes to decimation and keepEndChannels
         * @param monitorSystem monitor system reference 
         * @param reachableContainer reference to the subarray specific
         *        monitor system container for reachabilities
         */
        C3gMax8PipelineHandle(
            monitor::ControlSubsystem&                controlSubsystem,
            monitor::MonitorSystem&                   monitorSystem,
            monitor::ControlSubsystemBase::Reachable& reachableContainer);

        monitor::MonitorPointInt& getSeqNoMP();
};

class C3gMax23PipelineHandle : public PipelineHandle {
    public:
        /**
         * Constructor
         * @param controlSubsystem  writable control subsystem that will be
         *        used to store changes to decimation and keepEndChannels
         * @param monitorSystem monitor system reference 
         * @param reachableContainer reference to the subarray specific
         *        monitor system container for reachabilities
         */
        C3gMax23PipelineHandle(
            monitor::ControlSubsystem&                controlSubsystem,
            monitor::MonitorSystem&                   monitorSystem,
            monitor::ControlSubsystemBase::Reachable& reachableContainer);

        monitor::MonitorPointInt& getSeqNoMP();
};

}  // namespace carma::control
}  // namespace carma


#endif
