#ifndef CARMA_CONTROL_SWITCHYARDHANDLES_H
#define CARMA_CONTROL_SWITCHYARDHANDLES_H

#include "carma/control/RemoteObjHandleT.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/switchyard/SwitchyardControl.h"

namespace carma {

namespace monitor {
    class MonitorSystem;
} // namespace monitor

namespace control {

typedef 
RemoteObjHandleT< switchyard::SwitchyardControl > SwitchyardRemoteObjHandle;

// Super thin handles for the various switchyard control DOs.
// They just call through to DO and contain no local state.
class IFSwitchyardHandle : public SwitchyardRemoteObjHandle {
public:
    
    IFSwitchyardHandle( 
        carma::monitor::MonitorSystem & monitorSystem,
        carma::monitor::ControlSubsystemBase::Reachable & reachable );

    ~IFSwitchyardHandle( );

    void setSwitches(std::vector<carma::switchyard::SwitchPosition>& swVec);

};

class LOSwitchyardHandle : public SwitchyardRemoteObjHandle {
public:
    
    LOSwitchyardHandle( 
        carma::monitor::MonitorSystem & monitorSystem,
        carma::monitor::ControlSubsystemBase::Reachable & reachable );

    ~LOSwitchyardHandle( );

    void setSwitches(std::vector<carma::switchyard::SwitchPosition>& swVec);

};

class LLSwitchyardHandle : public SwitchyardRemoteObjHandle {
public:
    
    LLSwitchyardHandle( 
        carma::monitor::MonitorSystem & monitorSystem,
        carma::monitor::ControlSubsystemBase::Reachable & reachable );

    ~LLSwitchyardHandle( );

    void setSwitches(std::vector<carma::switchyard::SwitchPosition>& swVec);
};

class DCLOSwitchyardHandle : public SwitchyardRemoteObjHandle {
public:

    DCLOSwitchyardHandle(
        carma::monitor::MonitorSystem & monitorSystem,
        carma::monitor::ControlSubsystemBase::Reachable & reachable );

    ~DCLOSwitchyardHandle( );

    void setSwitches(std::vector<carma::switchyard::SwitchPosition>& swVec);
}; 
        

}} // namespace carma::control
#endif
