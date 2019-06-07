#ifndef CARMA_CONTROL_OPTICAL_TEL_HANDLE_H
#define CARMA_CONTROL_OPTICAL_TEL_HANDLE_H

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
#include "carma/antenna/common/OpticalTelControl.h"


namespace carma {
namespace control {


typedef RemoteObjHandleT< antenna::common::OpticalTelControl >
        OpticalTelControlRemoteObjHandle;


//! @brief Manages antenna optical telescope control DO connections
class OpticalTelHandle : public OpticalTelControlRemoteObjHandle {
    public:
        /**
         * Constructor
         *
         * @param monitorSystem monitor system reference which allows this
         *                      handle to get a reference to its own monitor
         *                      stream.
         */
        OpticalTelHandle(
            unsigned short                           carmaAntNo,
            monitor::MonitorSystem &                 monitorSys,
            monitor::ControlSubsystemBase::Antenna & antenna );
    
        virtual ~OpticalTelHandle( );
        
        /** 
         * Find centroid centroid while passing in a sequence number.
         * The sequence number is later used to determine if centroiding
         * has finished.
         * @see carma::antenna::common::OpticalTelControl::findCentroidWithSeqNo
         * @see isActionComplete
         */
        void findCentroidWithSeqNo( 
            monitor::MonitorSystem * monsys, // Pointer allows polymorphic casts
            CORBA::UShort numFramesPerImage,
            CORBA::UShort minValidCentroids,
            CORBA::UShort maxCentroidAttempts,
            CORBA::UShort numEdgePixels,
            CORBA::UShort apertureRadiusPixels,
            CORBA::Float pixelThresholdSigma,
            CORBA::Boolean subBackground,
            CORBA::Boolean normalizeMedian,
            int preferredSequenceNo );

        /**
         * Take background image while passing in a sequence number.
         * The sequence number is later used to determine if centroiding
         * has finished.
         * @see isActionComplete
         */
        void takeBackgroundWithSeqNo(
            monitor::MonitorSystem * monsys, // Pointer allows polymorphic casts
            CORBA::UShort numFrames,
            int preferredSequenceNo );

        /**
         * Compare next sequence number with one returned by monitor system.
         * If they are the same then the last drive action is complete.
         * @param monsys monitor system from which to retrieve completion
         * @param monitorDataErrorLimit number of consecutive monitor data
         * invalid limit before thowing an exception
         * @return true if last action is complete
         * @throws if number of consecutive monitor data errors is exceeed
         */
        bool isActionComplete( const monitor::MonitorSystem & monsys,
                                    int   monDataErrorLimit );

    private:

        int nextSequenceNo_;
        int consecutiveErrors_;
        int errorLimit_;

        const unsigned short carmaAntNo_;
};


}  // namespace carma::control
}  // namespace carma


#endif
