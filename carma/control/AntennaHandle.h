#ifndef CARMA_CONTROL_ANTENNAHANDLE_H
#define CARMA_CONTROL_ANTENNAHANDLE_H

#include "carma/corba/corba.h"
#include "carma/antenna/common/AntennaControl.h"
#include "carma/control/RemoteObjHandleT.h"
#include "carma/monitor/ControlSubsystemExt.h"

namespace carma {
namespace control {

typedef RemoteObjHandleT<carma::antenna::common::AntennaControl> AntennaRemoteObjHandle;

//! @brief Manages antenna Antenna control DO connections
class AntennaHandle : public AntennaRemoteObjHandle {
public:
    /**
     * Constructor
     *
     * @param carmaAntNo  Carma antenna number
     * @param monitorSystem carma::monitor::MonitorSystem&
     *              monitor system, which allows antenna handle to get a 
     *              reference to its own monitor stream.
     * @param antenna Antenna control monitor system, which contains control
     *                and monitor points set by the control subsystem.
     *
     */
    AntennaHandle( const unsigned short                     carmaAntNo, 
                   carma::monitor::MonitorSystem &                 monitorSystem,
                   carma::monitor::ControlSubsystemBase::Antenna & antenna );

    /**
     * Destructor - releases object (DO) references.
     */
     virtual ~AntennaHandle();

     /**
      * Set the initialization flag.
      */
     void setInitialization( CORBA::Boolean state );

protected:

private:

}; // class AntennaHandle

} // namespace control
} // namespace carma
#endif
