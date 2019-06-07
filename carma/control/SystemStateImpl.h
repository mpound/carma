#ifndef CARMA_CONTROL_SYSTEMSTATEIMPL_H
#define CARMA_CONTROL_SYSTEMSTATEIMPL_H

#include "carma/corba/corba.h"
#include "carma/control/AgingPolicy.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/control/SystemState_skel.h"

namespace carma {

namespace dbms {
    class TagIDAuthority;
}

namespace control {

class StateManager;

class SystemStateImpl {
public:

    SystemStateImpl( const StateManager & manager,
                     const PolicyChain & policies );

    ~SystemStateImpl();

    CORBA::ULong getNewestStateFrame( );

    CORBA::ULong getOldestStateFrame( );

    ::carma::control::SeqULong *
    getStateChangeFrames( const ::carma::control::SeqString& names,
                          ::CORBA::ULong begin,
                          ::CORBA::ULong end );

    ::carma::control::StateTransportMonitorValueSeq *
    getStateValues( const ::carma::control::SeqString& names,
                    ::CORBA::ULong frame );


private:

    const StateManager & manager_;
    const PolicyChain & policyChain_;

    const carma::dbms::TagIDAuthority & authority_;
    carma::monitor::ControlSubsystem localControlSubsys_;

}; // class SystemStateImpl

} // namespace control
} // namespace carma
#endif
