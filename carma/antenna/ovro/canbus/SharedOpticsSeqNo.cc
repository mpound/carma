#include "carma/antenna/ovro/canbus/SharedOpticsSeqNo.h"
#include "carma/monitor/AntennaCommon.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/Time.h"

#include <iostream>

using namespace carma::antenna::ovro;
using namespace carma::monitor;
using namespace carma::util;
using namespace std;

SharedOpticsSeqNo::SharedOpticsSeqNo( carma::monitor::AntennaCommon & mon ) : 
    polarizerRequestPending_( false ),
    focusZTrackingRequestPending_( false ),
    beamSelectionRequestPending_( false ),
    pendingSequenceNo_( 0 ),
    currentSequenceNo_( 0 ),
    requestFrame_( 0 ),
    mon_( mon ) 
{ 
    focusPositionRequestPending_[X] = false;
    focusPositionRequestPending_[Y] = false; 
    focusPositionRequestPending_[Z] = false;
}

SharedOpticsSeqNo::~SharedOpticsSeqNo( ) { }

void 
SharedOpticsSeqNo::setPolarizerRequestPending( const long seqNo )
{ 
    ScopedPthreadMutexLock scopelock( mutex_ );
    verifyRequestExclusivityHoldingLock();
    polarizerRequestPending_ = true;
    pendingSequenceNo_ = seqNo;
    requestFrame_ = Time::computeCurrentFrame( );
}
    
void 
SharedOpticsSeqNo::setFocusPositionRequestPending( const long seqNo, 
                                                   const enum Axis axis )
{
    ScopedPthreadMutexLock scopelock( mutex_ );
    verifyRequestExclusivityHoldingLock();
    focusPositionRequestPending_[ axis ] = true;
    pendingSequenceNo_ = seqNo;
    requestFrame_ = Time::computeCurrentFrame( );
}

void
SharedOpticsSeqNo::setFocusZTrackingRequestPending( const long seqNo )
{
    ScopedPthreadMutexLock scopelock( mutex_ );
    verifyRequestExclusivityHoldingLock();
    focusZTrackingRequestPending_ = true;
    pendingSequenceNo_ = seqNo;
    requestFrame_ = Time::computeCurrentFrame( );
}
    
void 
SharedOpticsSeqNo::setBeamSelectionRequestPending( const long seqNo )
{
    ScopedPthreadMutexLock scopelock( mutex_ );
    verifyRequestExclusivityHoldingLock();
    focusZTrackingRequestPending_ = true;
    pendingSequenceNo_ = seqNo;
    requestFrame_ = Time::computeCurrentFrame( );
}
    
void
SharedOpticsSeqNo::markPolarizerRequestCompleteIfPending(
    const carma::util::frameType minWaitFrames )
{
    ScopedPthreadMutexLock scopelock( mutex_ );

    if ( !polarizerRequestPending_ ) 
        return;
        
    const frameType elapsed = Time::computeCurrentFrame() - requestFrame_;

    if ( elapsed >= minWaitFrames ) 
        completeRequestHoldingLock( );
}

void 
SharedOpticsSeqNo::markFocusPositionRequestCompleteIfPending(
    const enum Axis axis, 
    const carma::util::frameType minWaitFrames )
{
    ScopedPthreadMutexLock scopelock( mutex_ );

    if ( !focusPositionRequestPending_[ axis ] ) 
        return;

    const frameType elapsed = Time::computeCurrentFrame() - requestFrame_;

    if ( elapsed >= minWaitFrames ) 
        completeRequestHoldingLock( );
}
   
void 
SharedOpticsSeqNo::markFocusZTrackingRequestCompleteIfPending(
    const carma::util::frameType minWaitFrames )
{
    ScopedPthreadMutexLock scopelock( mutex_ );

    if ( !focusZTrackingRequestPending_ ) 
        return;

    const frameType elapsed = Time::computeCurrentFrame() - requestFrame_;

    if ( elapsed >= minWaitFrames ) 
        completeRequestHoldingLock( );
}
    
void 
SharedOpticsSeqNo::markBeamSelectionRequestCompleteIfPending(
    const carma::util::frameType minWaitFrames )
{
    ScopedPthreadMutexLock scopelock( mutex_ );

    if ( !beamSelectionRequestPending_ ) 
        return;
    
    const frameType elapsed = Time::computeCurrentFrame() - requestFrame_;

    if ( elapsed >= minWaitFrames ) 
        completeRequestHoldingLock( );
}

void 
SharedOpticsSeqNo::completeRequestHoldingLock( )
{

    currentSequenceNo_ = pendingSequenceNo_;

    polarizerRequestPending_ = false;
    focusPositionRequestPending_[X] = false;
    focusPositionRequestPending_[Y] = false;
    focusPositionRequestPending_[Z] = false;
    focusZTrackingRequestPending_ = false;
    beamSelectionRequestPending_ = false;
    pendingSequenceNo_ = 0;
    requestFrame_ = 0;
}

void
SharedOpticsSeqNo::writeToMonitorSystem( )
{
    ScopedPthreadMutexLock scopelock( mutex_ );
    mon_.optics().opticsSeqNum().setValue( currentSequenceNo_ );
}

void
SharedOpticsSeqNo::verifyRequestExclusivityHoldingLock( )
{
    unsigned short numRequestsPending = 0;
    
    if ( polarizerRequestPending_ ) ++numRequestsPending;
    if ( focusPositionRequestPending_[X] ) ++numRequestsPending;
    if ( focusPositionRequestPending_[Y] ) ++numRequestsPending;
    if ( focusPositionRequestPending_[Z] ) ++numRequestsPending;
    if ( focusZTrackingRequestPending_ ) ++numRequestsPending;
    if ( beamSelectionRequestPending_ ) ++numRequestsPending;

    if ( numRequestsPending >= 1 ) {
        ostringstream warn;
        warn << "Multiple requests (" << numRequestsPending << ") are "
            << "pending for optics sequence number " << pendingSequenceNo_
            << " when there should be none!";
        programLogWarnIfPossible( warn.str() );
    }
}



