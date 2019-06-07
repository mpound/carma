#ifndef CARMA_CONTROL_LINE_LENGTH_HANDLE_H
#define CARMA_CONTROL_LINE_LENGTH_HANDLE_H

/**
 * @file
 *
 * Carma control interface to the line length control.
 *
 * $CarmaCopyright$
 *
 */


#include "carma/control/RemoteObjHandleT.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/linelength/LineLengthControl.h"
#include "carma/util/Time.h"

namespace carma {
namespace control {


typedef RemoteObjHandleT< linelength::LineLengthControl >
        LineLengthControlRemoteObjHandle;


//! @brief Manages line length control DO connections
class LineLengthHandle : public LineLengthControlRemoteObjHandle {
    public:
        /**
         * Constructor
         *
         * @param monitorSystem monitor system reference which allows this
         *                      handle to get a reference to its own monitor
         *                      stream.
         */
        LineLengthHandle(
            monitor::MonitorSystem &                   monitorSys,
            monitor::ControlSubsystemBase::Reachable & reachable );

        virtual ~LineLengthHandle( );

        void setAntennaLORef(const unsigned short ant, const unsigned short synth);
        void setLORefFreq(const unsigned short synth, const double freq_hz);
};


}  // namespace carma::control
}  // namespace carma


#endif
