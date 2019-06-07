#ifndef CARMA_CONTROL_CLOCK_HANDLE_H
#define CARMA_CONTROL_CLOCK_HANDLE_H

/**
 * @file
 *
 * Carma control interface to the master clock control.
 *
 * $CarmaCopyright$
 *
 */
 

#include "carma/corba/corba.h"
#include "carma/control/RemoteObjHandleT.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/clock/ClockControl.h"


namespace carma {
namespace control {


typedef RemoteObjHandleT< clock::ClockControl > ClockControlRemoteObjHandle;


//! @brief Manages master clock control DO connections
class ClockHandle : public ClockControlRemoteObjHandle {
    public:
        /**
         * Constructor
         *
         * @param monitorSystem monitor system reference which allows this
         *                      handle to get a reference to its own monitor
         *                      stream.
         */
        ClockHandle( monitor::MonitorSystem &                   monitorSys,
                     monitor::ControlSubsystemBase::Reachable & reachable );
    
        virtual ~ClockHandle( );
};


}  // namespace carma::control
}  // namespace carma


#endif
