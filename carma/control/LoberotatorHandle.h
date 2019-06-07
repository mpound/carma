#ifndef CARMA_CONTROL_LOBEROTATOR_HANDLE_H
#define CARMA_CONTROL_LOBEROTATOR_HANDLE_H

/**
 * @file
 *
 * Carma control interface to the loberotator.
 *
 * @author: Amar Amarnath
 *
 * $CarmaCopyright$
 *
 */
 
#include <vector>

#include "carma/control/RemoteObjHandleT.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/loberotator/LoberotatorControl.h"


namespace carma {
namespace control {


typedef RemoteObjHandleT<carma::loberotator::LoberotatorControl>
        LoberotatorControlRemoteObjHandle;


//! @brief Manages lobe rotator control DO connections
class LoberotatorHandle : public LoberotatorControlRemoteObjHandle {
    public:
        /**
        * Constructor
        *
        * @param carmaMonitor ::carma::monitor::MonitorSystem&
        *              monitor system, which allows loberotator handle to get a 
        *              reference to its own monitor stream.
        * @param subarrayMonitor 
        *        const ::carma::monitor::ControlSubsystemBase:Subarray&
        *              monitor system, which allows loberotator handle to set 
        *              monitor points for the subarray within control monitor 
        *              subsystem .
        */
        LoberotatorHandle(
            carma::monitor::MonitorSystem&                   carmaMonitor,
            carma::monitor::ControlSubsystemBase::Reachable& reachable);
                
        virtual ~LoberotatorHandle();
        
	/**
	 * Update entire set of loberotator channels with complete
	 * triplet data created from latest Delay Engine monitor frames.
	 * @param delayFreq The CORBA packet containing all the data.
	 */
	void updateDelayAndFreq( 
	    loberotator::LoberotatorControl::DelayFreqPacket delayFreq);
	    
	/**
	 * Update loberotator channels 1-8 with data for ants 16-23
	 * for the szaLR DO (the sza LR hardware).
	 * Used during transition from SZA to carma.
	 * @param delayFreq The CORBA packet containing all the data.
	 */
	void updateSZAdelayAndFreq( 
	    loberotator::LoberotatorControl::DelayFreqPacket delayFreq);
	
};


}  // namespace carma::control
}  // namespace carma


#endif
