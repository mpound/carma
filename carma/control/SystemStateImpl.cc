#include "carma/control/SystemStateImpl.h"

#include "carma/control/AgingPolicy.h"
#include "carma/control/SystemStateManager.h"
#include "carma/dbms/TagIDAuthority.h"
#include "carma/monitor/MonitorContainerFileIO.h"
#include "carma/monitor/types.h"
#include "carma/util/corbaSequenceUtils.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/Trace.h"
#include "carma/util/types.h"

#include <vector>
#include <string>

using namespace carma;
using namespace carma::control;
using namespace carma::monitor;
using namespace carma::util;
using namespace std;

SystemStateImpl::SystemStateImpl( const StateManager & manager,
                                  const PolicyChain & policies ) :
    manager_( manager ),
    policyChain_( policies ),
    authority_( dbms::TagIDAuthority::getAuthority() )
{
}

SystemStateImpl::~SystemStateImpl()
{
}

CORBA::ULong
SystemStateImpl::getNewestStateFrame( )
try {
    return static_cast<CORBA::ULong>( manager_.getMostRecentSavedState() );
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
    return 0;
}

CORBA::ULong
SystemStateImpl::getOldestStateFrame( )
try {
    return static_cast<CORBA::ULong>( manager_.getOldestSavedState() );
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
    return 0;
}

SeqULong *
SystemStateImpl::getStateChangeFrames(
        const ::carma::control::SeqString& names,
        ::CORBA::ULong begin,
        ::CORBA::ULong end )
try {

    {
        ostringstream infoMsg;
        infoMsg << "SystemStateImpl::getStateChangeFrames called with range "
            << begin << " to " << end;

        programLogInfoIfPossible( infoMsg.str() );
    }

    typedef vector< string > StringVec;

    const frameType beginFrame = static_cast< frameType >( begin );
    const frameType endFrame = static_cast< frameType >( end );

    StringVec canonicalNames;
    assignSequenceToVector( names, canonicalNames );

    tagIDType tagId;

    FrameSet aggregateFrameSet;

    const StringVec::const_iterator cnBegin = canonicalNames.begin();
    const StringVec::const_iterator cnEnd = canonicalNames.end();
    for ( StringVec::const_iterator cn = cnBegin; cn != cnEnd; ++cn ) {
        // Retrieve tag Id
        tagId = authority_.lookupID( *cn );

        // Retrieve list of frames within begin and end
        const FrameSet frames = manager_.getStateChangeFrames( tagId );

        CARMA_CPTRACE( Trace::TRACE2, "Retrieved list of " << frames.size()
            << " frames." );

        // Now combine them into the answer set but using begin and end frames
        const FrameSet::const_iterator lower = frames.lower_bound( beginFrame );
        const FrameSet::const_iterator upper = frames.upper_bound( endFrame );
        for ( FrameSet::const_iterator f = lower; f != upper; ++f ) {
            CARMA_CPTRACE( Trace::TRACE2, "Aggregating frame " << *f << ".");
            aggregateFrameSet.insert( *f );
        }

    }

    // Convert to a sequence
    SeqULong * answer = new SeqULong( aggregateFrameSet.size() );
    assignSetToSequence( aggregateFrameSet, *answer );

    return answer;

} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
    return 0;
}

control::StateTransportMonitorValueSeq *
SystemStateImpl::getStateValues( const ::carma::control::SeqString& names,
                                 ::CORBA::ULong frame )
try {

    // First using the input frame, get the state frame value
    const frameType closestStateFrame = manager_.getClosestSavedState( frame );

    if ( closestStateFrame == 0 )
        return new StateTransportMonitorValueSeq( ); // Empty

    // Retrieve the file for the given frame.
    const string filename =
        retrieveFilenameForFrame( policyChain_, closestStateFrame );

    setContainerFromFile( localControlSubsys_, filename );

    // Loop over MPs in sequence and retrieve transportMP
    typedef vector<string> StringVec;
    StringVec canonicalNames;
    assignSequenceToVector( names, canonicalNames );

    vector< StateTransportMonitorValue > answerVec;

    tagIDType tagId;
    const StringVec::const_iterator cnBegin = canonicalNames.begin();
    const StringVec::const_iterator cnEnd = canonicalNames.end();
    for ( StringVec::const_iterator cn = cnBegin; cn != cnEnd; ++cn ) {
        // Retrieve tag Id
        tagId = authority_.lookupID( *cn );

        const MonitorPoint & mp = localControlSubsys_.getMonitorPoint( tagId );
        const MonitorPointSample mps = mp.getMonitorPointSample0();

        StateTransportMonitorValue stmv;
        mps.setTransportValue( mp.getValuetype(), stmv.tmv );

        // Did it change (remember we're retrieving lots of values which may
        // not have actually changed)?
        if ( manager_.tagChangedForFrame( tagId, closestStateFrame ) )
            stmv.changed = true;
        else
            stmv.changed = false;

        answerVec.push_back( stmv );
    }

    StateTransportMonitorValueSeq * answerSeq =
        new StateTransportMonitorValueSeq( answerVec.size() );

    assignVectorToSequence( answerVec, *answerSeq );

    return answerSeq;

} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
    return 0;
}

