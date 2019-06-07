#ifndef CARMA_CONTROL_CRYO_HANDLE_H
#define CARMA_CONTROL_CRYO_HANDLE_H

/**
 * @file
 *
 * Carma control interface to an antenna cryogenics control.
 *
 * $CarmaCopyright$
 *
 */
 

#include "carma/corba/corba.h"
#include "carma/control/RemoteObjHandleT.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/antenna/common/CryoControl.h"


namespace carma {
namespace control {


typedef RemoteObjHandleT< antenna::common::CryoControl >
        CryoControlRemoteObjHandle;


//! @brief Manages antenna cryo control DO connections
class CryoHandle : public CryoControlRemoteObjHandle {
    public:
        /**
         * Constructor
         *
         * @param monitorSystem monitor system reference which allows this
         *                      handle to get a reference to its own monitor
         *                      stream.
         */
        CryoHandle(
            unsigned short                           carmaAntNo,
            monitor::MonitorSystem &                 monitorSys,
            monitor::ControlSubsystemBase::Antenna & antenna );
    
        virtual ~CryoHandle( );
};


}  // namespace carma::control
}  // namespace carma


#endif
