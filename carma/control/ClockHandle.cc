#include "carma/control/ClockHandle.h"

#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/MasterClockSubsystem.h"


using namespace ::std;
using namespace carma;
using namespace carma::control;
using namespace carma::monitor;


ClockHandle::ClockHandle(
    MonitorSystem &                   monitorSys,
    ControlSubsystemBase::Reachable & reachable ) :
ClockControlRemoteObjHandle( "carma.clock.clockControl",
                             &(reachable.clock()),
                             &(monitorSys.masterclock()),
                             &monitorSys,
                             true,
                             false )
{
    // nothing to do here?
}


ClockHandle::~ClockHandle( )
try {
    // nothing to do here?
} catch ( ... ) {
    // Just stifle any exceptions
    
    return;
}
