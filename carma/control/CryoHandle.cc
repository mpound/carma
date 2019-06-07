#include "carma/control/CryoHandle.h"

#include "carma/control/antennaHandleUtils.h"

using namespace ::std;
using namespace carma;
using namespace carma::antenna;
using namespace carma::antenna::common;
using namespace carma::control;
using namespace carma::monitor;


CryoHandle::CryoHandle(
    const unsigned short            carmaAntNo,
    MonitorSystem &                 monitorSys,
    ControlSubsystemBase::Antenna & antenna ) :
CryoControlRemoteObjHandle(
    makeAntennaDoName( carmaAntNo, CRYO_NAME ),
    &(antenna.antennaReachable( ).cryo( )),
    &(getAntennaSubsystem( carmaAntNo, monitorSys )),
    &monitorSys,
    true,
    false )
{
    // nothing to do here?
}


CryoHandle::~CryoHandle( )
try {
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}
