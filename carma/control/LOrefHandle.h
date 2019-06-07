#ifndef CARMA_CONTROL_LOREF_HANDLE_H
#define CARMA_CONTROL_LOREF_HANDLE_H

/**
 * @file
 *
 * Carma control interface to the LO reference.
 *
 * @author: Amar Amarnath
 *
 * $CarmaCopyright$
 *
 */
 

#include "carma/control/RemoteObjHandleT.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/loref/LOReferenceControl.h"


namespace carma {
namespace control {


typedef RemoteObjHandleT< ::carma::loref::LOReferenceControl >
        LOrefControlRemoteObjHandle;


//! @brief Manages LO reference control DO connections
class LOrefHandle : public LOrefControlRemoteObjHandle {
    public:
        /**
         * Constructor
         *
         * @param carmaMonitor carma::monitor::MonitorSystem&
         *              monitor system, which allows LOref handle to get a 
         *              reference to its own monitor stream.
         * @param subarrayMonitor 
         *        const carma::monitor::ControlSubsystemBase:Subarray&
         *              monitor system, which allows LOref handle to set 
         *              monitor points for the subarray within control monitor 
         *              subsystem .
         */
        LOrefHandle(
            carma::monitor::MonitorSystem &                   carmaMonitor, 
            carma::monitor::ControlSubsystemBase::Reachable & reachable );
    
    
        virtual ~LOrefHandle();
        
        /**
         * Set a synthesizer frequency and power.
         *
         * @param double frequency Frequency to set (Hertz)
         * @param double power Power to set (dBm)
         * @return none.
         */
        void setFrequencyPower(double frequency, double power) ;
    
        /**
         * Set a synthesizer frequency.
         *
         * @param double frequency Frequency to set (Hertz)
         * @return none.
         */
        void setFrequency(double frequency) ;
    
        /**
         * Set a synthesizer power.
         *
         * @param double power Power to set (dBm)
         * @return none.
         */
        void setPower(double power) ;
    
        /**
         * Assign the synthesizer for this subarray.
         *
         * @param synthIndex [1-3]
         * @return none.
         */
         void assignSynth(const int synthIndex);
    
    private:                                                                                
        int synthIndex_;
};


}  // namespace carma::control
}  // namespace carma


#endif
