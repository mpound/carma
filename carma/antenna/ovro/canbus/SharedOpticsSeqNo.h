#ifndef CARMA_ANTENNA_OVRO_SHAREDOPTICSSEQNO_H
#define CARMA_ANTENNA_OVRO_SHAREDOPTICSSEQNO_H

#include "carma/util/PthreadMutex.h"
#include "carma/util/types.h"

namespace carma {

namespace monitor {
    class AntennaCommon;
}

namespace antenna {
namespace ovro {

class SharedOpticsSeqNo {
public:

    SharedOpticsSeqNo( carma::monitor::AntennaCommon & mon ); 

    ~SharedOpticsSeqNo( );

    enum Axis { X = 0, Y = 1, Z = 2 };

    void setPolarizerRequestPending( long seqNo );
    void setFocusPositionRequestPending( long seqNo, enum Axis axis );
    void setFocusZTrackingRequestPending( long seqNo );
    void setBeamSelectionRequestPending( long seqNo );

    void markPolarizerRequestCompleteIfPending( 
        carma::util::frameType minWaitFrames );
    void markFocusPositionRequestCompleteIfPending( 
        enum Axis axis, carma::util::frameType minWaitFrames );
    void markFocusZTrackingRequestCompleteIfPending(
        carma::util::frameType minWaitFrames );
    void markBeamSelectionRequestCompleteIfPending(
        carma::util::frameType minWaitFrames );

    void writeToMonitorSystem( );

private:
    
    void completeRequestHoldingLock( );

    void verifyRequestExclusivityHoldingLock( );

    bool polarizerRequestPending_;
    bool focusPositionRequestPending_[3];
    bool focusZTrackingRequestPending_;
    bool beamSelectionRequestPending_;
    long pendingSequenceNo_;
    long currentSequenceNo_;
    carma::util::frameType requestFrame_;
    mutable carma::util::PthreadMutex mutex_;
    carma::monitor::AntennaCommon & mon_; 
};

}}} 
#endif
