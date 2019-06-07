#ifndef CARMA_CONTROL_FAULTHANDLE_H
#define CARMA_CONTROL_FAULTHANDLE_H

//! @file
//!
//! Carma control interface to the fault system control.
//!
//! $CarmaCopyright$
//!

#include <string>
#include <vector>

#include "carma/corba/corba.h"
#include "carma/control/RemoteObjHandleT.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/fault/FaultControl.h"


namespace carma {
namespace control {


typedef RemoteObjHandleT< carma::fault::FaultControl > FaultControlRemoteObjHandle;


//! @brief Manages fault system control DO connections
class FaultHandle : public FaultControlRemoteObjHandle {
    public:
        //! @brief Constructor
        //!
        //! @param monitorSystem monitor system reference which allows this
        //!                      handle to get a reference to its own monitor
        //!                      stream.
        FaultHandle( monitor::MonitorSystem &                   monitorSys,
                     monitor::ControlSubsystemBase::Reachable & reachable );

        virtual ~FaultHandle( );

        //! @brief Disable fault system alarms for the given monitor points.
        //!
        //! @param mpNames vector of monitor point names. Passed by value to
        //!                safely cross the thread boundary without issues
        //!                if the invoker completes before this call itself
        //!                actually completes
        void disableAlarms( ::std::vector< ::std::string > mpNames );

        //! @brief Restore fault system alarms to their default enable states
        //!        for the given monitor points.
        //!
        //! @param mpNames vector of monitor point names. Passed by value to
        //!                safely cross the thread boundary without issues
        //!                if the invoker completes before this call itself
        //!                actually completes
        void restoreAlarms( ::std::vector< ::std::string > mpNames );

        //! @brief Set the alarm enable state for the given subarray
        //!
        //! @param subarrayNo the subarray number
        //! @param on true if the alarm is enabled
        void setAlarmEnable( int subarrayNo, bool on );
};


}  // namespace carma::control
}  // namespace carma


#endif
