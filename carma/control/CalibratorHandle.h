#ifndef CARMA_CONTROL_CALIBRATOR_HANDLE_H
#define CARMA_CONTROL_CALIBRATOR_HANDLE_H

/**
 * @file
 *
 * Carma control interface to an antenna optical telescope control.
 *
 * $CarmaCopyright$
 *
 */
 

#include "carma/corba/corba.h"
#include "carma/control/RemoteObjHandleT.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/antenna/common/CalibratorControl.h"


namespace carma {
namespace control {


typedef RemoteObjHandleT< antenna::common::CalibratorControl >
        CalibratorControlRemoteObjHandle;


//! @brief Manages antenna calibrator control DO connections
class CalibratorHandle : public CalibratorControlRemoteObjHandle {
    public:
    /**
     * Constructor
     *
     * @param monitorSystem monitor system reference which allows this
     *                      handle to get a reference to its own monitor
     *                      stream.
     */
    CalibratorHandle(
        unsigned short                           carmaAntNo,
        monitor::MonitorSystem &                 monitorSys,
        monitor::ControlSubsystemBase::Antenna & antenna );
    
    virtual ~CalibratorHandle( );
        
    /**
     * Moves the calibration device and sets the sequence number
     * that will be returned in the monitor stream on completion.
     * The sequence number is also stored internally so that it 
     * can be compared against the one returned by the antenna 
     * in the monitor stream.
     * @param monsys Monitor system to be used to check seq no
     * This pointer is not saved in this object.
     * @param calPos requested calibrator position
     * @param preferredSequenceNo will be used unless it is already
     * the one returned by the monitor system, in which case one that is
     * ten greater will be used.
     * @see isActionComplete
     */
    void setCalibrator(
        monitor::MonitorSystem* monsys, 
        const antenna::common::CalibratorControl::Position calPos,
        const int preferredSequenceNo);

    /**
     * Compare next sequence number with one returned by monitor system.
     * If they are the same then the last action is complete.
     * @param monsys monitor system from which to retrieve completion
     * The monsys should already be placed in the queue, *don't* do
     * a read or anything that will disturb the queue.
     * @param monitorDataErrorLimit number of consecutive monitor data
     * invalid limit before thowing an exception
     * @return true if last action is complete
     * @throws if number of consecutive monitor data errors is exceeed
     */
    bool isActionComplete( const monitor::MonitorSystem & monsys,
                           int                            monDataErrorLimit );

private:
    unsigned short carmaAntNo_;
    int            nextSequenceNo_;
    int            consecutiveErrorCount_;

};


}  // namespace carma::control
}  // namespace carma


#endif
