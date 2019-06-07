/**
 *
 * Carma Focus control interface implementation.
 *
 * @author: Marc Pound
 *
 * $Id: FocusHandle.cc,v 1.16 2009/03/11 23:19:04 abeard Exp $
 *
 * $CarmaCopyright$
 *
 */
 

#include "carma/control/FocusHandle.h"

#include "carma/control/antennaHandleUtils.h"
#include "carma/monitor/AntennaCommon.h"
#include "carma/util/programLogging.h"
#include "carma/util/Time.h"

using namespace ::std;
using namespace carma;
using namespace carma::antenna;
using namespace carma::antenna::common;
using namespace carma::control;
using namespace carma::monitor;
using namespace carma::util;


FocusHandle::FocusHandle(
    const unsigned short            carmaAntNo,
    MonitorSystem &                 monitorSystem,
    ControlSubsystemBase::Antenna & antenna ) :
FocusControlRemoteObjHandle(
    makeAntennaDoName( carmaAntNo, FOCUS_NAME ),
    &(antenna.antennaReachable( ).focus( )),
    &(getAntennaSubsystem( carmaAntNo, monitorSystem )),
    &monitorSystem,
    true,
    false ),
    nextSequenceNo_( 0 ),
    errLimit_( 0 ),
    consecutiveErrors_( 0 ),
    monitorSystem_( monitorSystem ),
    carmaAntNo_( carmaAntNo )
{
    // nothing to do here?
}


FocusHandle::~FocusHandle( )
try {
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}

void
FocusHandle::setX( const float positionMm, const int preferredSequenceNo )
{
    if ( !isObjReachable() )
        return;

    setNextSequenceNo( preferredSequenceNo );

    string remoteCallString;
    {
        ostringstream oss;
        oss << "FocusControl::setX( pos=" << positionMm 
            << ", seqNo=" << nextSequenceNo_ << " ).";
        remoteCallString = oss.str();
    }

    try {
        const double sendTime = Time::MJD();

        remoteObj( )->setX( positionMm, nextSequenceNo_ );

        logSentCommandIfNeeded( remoteCallString, sendTime );
    } catch ( const CORBA::Exception & ex ) {
        processException( remoteCallString, ex );
    }
}

void
FocusHandle::setY( const float positionMm, const int preferredSequenceNo )
{
    if ( !isObjReachable() )
        return;

    setNextSequenceNo( preferredSequenceNo );

    string remoteCallString;
    {
        ostringstream oss;
        oss << "FocusControl::setY( pos=" << positionMm 
            << ", seqNo=" << nextSequenceNo_ << " ).";
        remoteCallString = oss.str();
    }

    try {
        const double sendTime = Time::MJD();

        remoteObj( )->setY( positionMm, nextSequenceNo_ );

        logSentCommandIfNeeded( remoteCallString, sendTime );
    } catch ( const CORBA::Exception & ex ) {
        processException( remoteCallString, ex );
    }
}


void
FocusHandle::setZ( const float positionMm, const int preferredSequenceNo )
{
    if ( !isObjReachable() )
        return;

    setNextSequenceNo( preferredSequenceNo );

    string remoteCallString;
    {
        ostringstream oss;
        oss << "FocusControl::setZ( pos=" << positionMm 
            << ", seqNo=" << nextSequenceNo_ << " ).";
        remoteCallString = oss.str();
    }

    try {
        const double sendTime = Time::MJD();

        remoteObj( )->setZ( positionMm, nextSequenceNo_ );

        logSentCommandIfNeeded( remoteCallString, sendTime );
    } catch ( const CORBA::Exception & ex ) {
        processException( remoteCallString, ex );
    }
}

bool 
FocusHandle::isActionComplete( const monitor::MonitorSystem & monsys,
                               const int monDataErrorLimit )
{
    const MonitorPointInt & completionMP = 
        getAntennaCommon(carmaAntNo_, monsys).optics().opticsSeqNum();

    // Non-zero limit triggers resetting
    if ( monDataErrorLimit > 0 ) {
        errLimit_ = monDataErrorLimit;
        consecutiveErrors_ = 0; 
    }

    if ( !completionMP.isValid() ) {
        ++consecutiveErrors_;
        if ( consecutiveErrors_ >= errLimit_ ) {
            ostringstream oss;
            oss << "FocusHandle::isActionComplete " << consecutiveErrors_
                << "consecutive invalid monitor frames.";
                
            throw CARMA_ERROR( oss );
        }
       
        return false;
    }
    
    // Valid monitor point
    if ( consecutiveErrors_ > 0 ) {
        ostringstream oss;
        oss << "FocusHandle::isActionComplete: C" << carmaAntNo_
            << " had " << consecutiveErrors_
            << " consecutive invalid monitor frames.";
        programLogInfoIfPossible( oss.str() );
    }

    consecutiveErrors_ = 0;

    return ( completionMP.getValue() == nextSequenceNo_ );
}

void 
FocusHandle::setNextSequenceNo( const int preferredSequenceNo )
{
    monitorSystem_.readNewest(); // Get to the top of the queue

    const int currentSeqNo = getAntennaCommon( carmaAntNo_, monitorSystem_ ).
        optics().opticsSeqNum().getValue();

    if ( currentSeqNo == preferredSequenceNo ) {
        nextSequenceNo_ = preferredSequenceNo + 10;
    } else {
        nextSequenceNo_ = preferredSequenceNo;
    }
}

