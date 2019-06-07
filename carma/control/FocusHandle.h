#ifndef CARMA_CONTROL_FOCUS_HANDLE_H
#define CARMA_CONTROL_FOCUS_HANDLE_H

//! @file
//! Carma control interface to the antenna focus control.
//!
//! @author: Marc Pound
//!
//! $CarmaCopyright$
//!

#include "carma/corba/corba.h"
#include "carma/antenna/common/FocusControl.h"
#include "carma/control/SubarrayControl_skel.h"
#include "carma/control/RemoteObjHandleT.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/ControlSubsystem.h"


namespace carma {
namespace control {


typedef RemoteObjHandleT< antenna::common::FocusControl > 
        FocusControlRemoteObjHandle;


//! @brief Manages antenna focus control DO connections
class FocusHandle : public FocusControlRemoteObjHandle {
    public:
        //! @brief Constructor
        //!
        //! @param carmaAntNo Carma antenna number
        //!
        //! @param monitorSystem carma::monitor::MonitorSystem&
        //!              monitor system, which allows Focus handle to get a 
        //!              reference to its own monitor stream.
        //!
        //! @param antenna Antenna control monitor system, which contains control
        //!                and monitor points set by the control subsystem.
        FocusHandle( unsigned short                           carmaAntNo, 
                     monitor::MonitorSystem &                 monitorSystem,
                     monitor::ControlSubsystemBase::Antenna & antenna );
        
        virtual ~FocusHandle( );

        void setX( float positionMm, int preferredSeqNo ); 
        void setY( float positionMm, int preferredSeqNo ); 
        void setZ( float positionMm, int preferredSeqNo ); 

        /**
         * Compare next sequence number with one returned by the monitor system.
         * If they are the same then the last focus action is complete.
         * @param monsys monitor system from which to retrieve completion
         * @param monitorDataErrorLimit number of consecutive monitor data
         * invalid limit before thowing an exception
         * @return true if last action is complete, false otherwise
         * @throws if number of consecutive monitor data errors is exceeed
         */
        bool isActionComplete( const monitor::MonitorSystem & monsys,
                               int monDataErrorLimit );
        
    private:

        void setNextSequenceNo( int preferredSequenceNo );

        int nextSequenceNo_; 
        int errLimit_;
        int consecutiveErrors_;

        monitor::MonitorSystem & monitorSystem_;
        const unsigned short carmaAntNo_;

};


}  // namespace carma::control
}  // namespace carma


#endif
