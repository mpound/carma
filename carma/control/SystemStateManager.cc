#include "carma/corba/Client.h"
#include "carma/control/SystemStateManager.h"
#include "carma/dbms/TagIDAuthority.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/MonitorContainerFileIO.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/util/AutoPthreadQuitAndJoinGroup.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/FileUtils.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedExclusiveLock.h"
#include "carma/util/ScopedPthreadMutexLockManager.h"
#include "carma/util/ScopedSharedLock.h"
#include "carma/util/StartPthread.h"
#include "carma/util/Trace.h"

#include <boost/date_time/gregorian/gregorian_types.hpp>
#include <boost/foreach.hpp>
#include <fstream>
#include <iomanip>
#include <functional>
#include <string>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

using namespace boost;
using namespace boost::gregorian;
using namespace carma;
using namespace carma::control;
using namespace carma::monitor;
using namespace carma::util;
using namespace std;

namespace {

const int SUBARRAY_IDX = 0;

const frameType kSettleFrames = 10;  // 5 seconds
const frameType kMaxUnsettlePeriod = 240; // 2 minutes
const frameType kMinRestorePeriod = 60; // 30 seconds

const Trace::TraceLevel TRACE_RETRIEVAL = Trace::TRACE3;
const Trace::TraceLevel TRACE_INDEXING = Trace::TRACE2;
const Trace::TraceLevel TRACE_INDEX_CREATION = Trace::TRACE1;

frameType
parseFrameFromFilename( const string & filename,
                        const string & prefix,
                        const string & suffix )
{
    // Tokenize filename of form prefix + frame + suffix.
    const string::size_type prefixPos = filename.find( prefix );
    const string::size_type suffixStartPos = filename.size() - suffix.size();
    const string::size_type suffixPos = filename.find( suffix, suffixStartPos );

    if ( prefixPos != string::npos && suffixPos != string::npos ) {
        const string::size_type framePos = prefixPos + prefix.size();
        const string::size_type frameSize = suffixPos - framePos;
        const string frameToken =  filename.substr( framePos, frameSize );

        istringstream mjdIn( frameToken );
        frameType frame;
        mjdIn >> frame;
        return frame;
    }

    return 0;
}

typedef map< frameType, string > FileFrameMap;
typedef FileFrameMap::const_iterator FFMCI;

FileFrameMap createFileFrameMap( const string & dir )
{
    typedef vector<string>::iterator sIterator;

    vector<string> fileNames = FileUtils::scandir( dir );

    FileFrameMap answer;

    for ( sIterator i = fileNames.begin(); i != fileNames.end(); ++i ) {

        const frameType frame = parseFrameFromFilename( *i, "state-", ".txt" ); 

        if ( frame != 0 ) {
            answer.insert( FileFrameMap::value_type( frame, dir + "/" + *i ) );
        }
    }
        
    return answer;
}

void createFileFrameMap( const vector<string> & directories,
                         FileFrameMap & frameFileMap )
{
    // For each policy add files to master file map
    const vector<string>::const_iterator pBegin = directories.begin();
    const vector<string>::const_iterator pEnd = directories.end();
    for ( vector<string>::const_iterator p = pBegin; p != pEnd; ++p ) {

        const FileFrameMap pffm = createFileFrameMap( *p );
        const FFMCI ffBegin = pffm.begin();
        const FFMCI ffEnd = pffm.end();
        for ( FFMCI ff = ffBegin; ff != ffEnd; ++ff )   
            frameFileMap.insert( *ff );
    }

}

void traceReverseIndex( const string & desc, const ReverseTagIndex & rti ) 
{
    ostringstream trace;
    trace << desc << "  Reverse index contains frames ";
    const ReverseTagIndexConstIter rtiBegin = rti.begin();
    const ReverseTagIndexConstIter rtiEnd = rti.end();
    for ( ReverseTagIndexConstIter rtiIt = rtiBegin; rtiIt != rtiEnd; ++rtiIt )
        trace << rtiIt->first << " ";
    
    CARMA_CPTRACE( TRACE_INDEX_CREATION, trace.str() );
}
    
} // namespace < unnamed >

StateManager::StateManager( const CmsSelector cmsType,
                            const string & stateDirName,
                            const bool throwOnStateDirErrors ) :
    unsettleStartFrame_( 0 ),
    lastStateRestoreFrame_( 0 ),
    noFileFrameCount_( 0 ),
    savePending_( false ),
    stateDir_( stateDirName ),
    filePrefix_( "state-" ),
    fileSuffix_( ".txt" ),
    subarrayControl_( SubarrayControl::_nil() ),
    indexFilename_( stateDirName + "/tagIndex.txt" ),
    throwOnStateDirError_( throwOnStateDirErrors )
{
    CARMA_CPTRACE( TRACE_RETRIEVAL, "Checking if " + stateDir_ + " exists." );

    if ( !FileUtils::exists( stateDir_ ) ) {
        // Try to create it...
        programLogInfoIfPossible( "Directory " + stateDir_ + " does not exist."
            + "  Attempting to create." );
        try {
            FileUtils::makeDirectory( stateDir_ );
        } catch (...) {
            if ( throwOnStateDirError_ ) throw;
        }
    } 

    // Retrieve last saved state frame from the filesystem.
    lastStateSaveFrame_ = retrieveLastStateSaveFrame( );

    string cmsOutName( "< not set >" );
    inputCms_ = makeCms( cmsType, cmsOutName );
    inputCms_->readNewest();
    try {
        // Try to resolve subarray control DO, if this fails,
        // soldier on and re-resolve at a later date.
        ostringstream sacName;
        sacName << SUBARRAY_CONTROL_NAME << ( SUBARRAY_IDX + 1 );

        corba::Client & client = Program::getProgram().getCorbaClient();
        subarrayControl_ = client.resolveName<SubarrayControl>( sacName.str( ) );
    } catch (...) {
        programLogWarnIfPossible( "Unable to resolve subarray control DO - "
            "don't fret, I'll try again later." );
        subarrayControl_ = SubarrayControl::_nil();
    }
}

StateManager::~StateManager( ) 
{
    // Save state prior to termination.
}

bool
StateManager::update( )
{
    inputCms_->read( );
    
    restoreStateWhenUninitialized( );

    return saveStateIfChanged( ); 
}
    
void
StateManager::restoreStateWhenUninitialized( )
{
    // Wait a minimum number of frames before restoring the state again
    const frameType currentFrame = Time::computeCurrentFrame();

    const frameType framesSinceLastRestore = currentFrame - 
                                             lastStateRestoreFrame_;

    if ( framesSinceLastRestore < kMinRestorePeriod ) return; 

    const monitor::ControlSubsystemBase::Subarray & inSub = 
        inputCms_->control().subarray( SUBARRAY_IDX );

    MonitorPointBool & initialized = inSub.controllerInitialized();
    MonitorPointBool & restored = inSub.stateRestored();

    if ( !restored.isValid() || !initialized.isValid() ) 
        return; // SAC likely isn't running, nothing to be done.

    // Resolve subarray control if necessary.  If it doesn't work
    // this time, something is wrong so throw to main.
    if ( CORBA::is_nil( subarrayControl_ ) ) {
        try {
            ostringstream sacName;
            sacName << SUBARRAY_CONTROL_NAME << ( SUBARRAY_IDX + 1 );
            corba::Client & client = Program::getProgram().getCorbaClient();
            subarrayControl_ = client.resolveName<SubarrayControl>( sacName.str( ) );
        } catch (...) {
            programLogErrorIfPossible( "Unable to resolve subarray control." );
            throw;
        }
    }

    // Note: this relies on the subarray controller resetting the 
    // ControlSubsystem stateRestored monitor point to false on startup.
    if ( !initialized.getValue() && !restored.getValue() ) { // Time to restore

        // Retrieve most recent restored state text file.
        const string filename = retrieveMostRecentFilename();

        if ( filename == "" ) {

            if ( noFileFrameCount_ % 120 == 0 ) { // Don't overlog (1 per min)
                ostringstream error;
                error << "Unable to restore state as there were no restore "
                    << "files found in " << stateDir_;
                if ( noFileFrameCount_ )
                    error << " for last " << noFileFrameCount_ << " frames.";
                else
                    error << ".";

                programLogErrorIfPossible( error.str() );
            }

            ++noFileFrameCount_;

            return;
        } else {
            noFileFrameCount_ = 0;
        }

        programLogInfoIfPossible( "Restoring state from " + filename + "." );

        subarrayControl_->restoreControlSubsystemFromFile( filename.c_str() );
        subarrayControl_->signalControlSubsystemRestored( );
        lastStateRestoreFrame_ = currentFrame;
    } 
}
    
frameType
StateManager::retrieveLastStateSaveFrame( ) const
{
    vector<string> fileNames = FileUtils::scandir( stateDir_ );

    // Only consider names which fit our file naming syntax.
    frameType newestFrame = 0;
    const vector<string>::iterator iBegin = fileNames.begin( );
    const vector<string>::iterator iEnd = fileNames.end( );
    for ( vector<string>::iterator i = iBegin; i != iEnd; ++i ) {

        frameType frame = parseFrameFromFilename( *i,   
                                                  filePrefix_,
                                                  fileSuffix_ );

        if ( frame > newestFrame ) {
            newestFrame = frame;
        }
    }

    return newestFrame;
}

string 
StateManager::retrieveMostRecentFilename( ) const
{
    try {
        // First look for the newest file
        vector<string> fileNames;
        try { 
            CARMA_CPTRACE( TRACE_RETRIEVAL, "Scanning stateDir_=" + stateDir_ );
            fileNames = FileUtils::scandir( stateDir_ );
        } catch (...) {
            CARMA_CPTRACE( TRACE_RETRIEVAL, stateDir_ + " does not exist." );
            if ( throwOnStateDirError_ ) throw;
        }

        string newestFilename = "";
        frameType newestFrame = 0;
        const vector<string>::iterator iBegin = fileNames.begin( );
        const vector<string>::iterator iEnd = fileNames.end( );
        for ( vector<string>::iterator i = iBegin; i != iEnd; ++i ) {

            frameType frame = parseFrameFromFilename( *i, 
                    filePrefix_, 
                    fileSuffix_ );

            if ( frame > newestFrame ) {
                newestFrame = frame;
                newestFilename = *i;
            }
        } 

        if ( newestFrame > 0 ) {
            newestFilename = stateDir_ + "/" + newestFilename;
            CARMA_CPTRACE( TRACE_RETRIEVAL, 
                           "Retrieved " + newestFilename + " for restore." ); 
            return newestFilename;
        }

        // Next fallback on trying to find the currentState file.
        string currentStateFilename(stateDir_ + "/current" + fileSuffix_);

        CARMA_CPTRACE( TRACE_RETRIEVAL, "Trying to find current state file "
            + currentStateFilename + "." );

        if ( FileUtils::exists( currentStateFilename ) ) {
            return currentStateFilename;
        } else {
            CARMA_CPTRACE( TRACE_RETRIEVAL, 
                           currentStateFilename + " does not exist!" );
        }

        // Fallback to CVS'ed conf data directories
        const string confDataFile( "data/state/current" + fileSuffix_ );
        
        CARMA_CPTRACE( TRACE_RETRIEVAL, "Falling back to CVS'ed version "
            + confDataFile + "." );

        // getConfFile will retrieve the correct file on both install and 
        // development systems.
        const string absConfDataFile = Program::getConfFile( confDataFile );
        if ( FileUtils::exists( absConfDataFile ) ) {
            CARMA_CPTRACE( TRACE_RETRIEVAL, 
                           "Retrieved " + absConfDataFile + " for restore." );
            return absConfDataFile;
        } else {
            CARMA_CPTRACE( TRACE_RETRIEVAL, 
                           absConfDataFile + " does not exist!" );
        }

        return "";

    } catch (...) {
        if ( throwOnStateDirError_ ) {
            logCaughtAsError( );
            throw; // Rethrow
        } else {
            return "";
        }
    }
}

void 
StateManager::saveIndexFilesHoldingWriteLock( ) const
{
    // Write the forward index map to a file for persistence.
    // We only use canonical name when writing these files to avoid 
    // problems with on-the-fly tag ids.  
    std::fstream out( indexFilename_.c_str(), ios::out ); // Open for writing

    if ( !out )
        throw CARMA_ERROR("Unable to open " + indexFilename_ + " for writing.");

    dbms::TagIDAuthority & authority = dbms::TagIDAuthority::getAuthority();

    const TagIndexConstIter tiBegin = tagIndex_.begin();
    const TagIndexConstIter tiEnd = tagIndex_.end();
    for ( TagIndexConstIter ti = tiBegin; ti != tiEnd; ++ti ) {
        try {
            ostringstream line;
            line << authority.lookupName( ti->first );

            const FrameSetConstIter siBegin = ti->second.begin();
            const FrameSetConstIter siEnd = ti->second.end();
            for ( FrameSetConstIter si = siBegin; si != siEnd; ++si ) {
                line << " " << *si; 
            }
            line << endl;
            out << line.str();
        } catch (...) {
            // Stifle
        }
    }
}

bool 
StateManager::verifyIndexIntegrity( )
{
    ScopedExclusiveLock<PthreadRWLock> scopedWriteLock( indexRWLock_ );

    // Start by going through the tagIndex and making sure that all 
    // frames seem reasonable (e.g. are within a sane period of time).
    FrameSet invalidFrames;
    const util::frameType currentFrame = Time::computeCurrentFrame();
    
    BOOST_FOREACH( TagIndex::value_type value, tagIndex_ ) {
        const FrameSet frames = value.second;
        BOOST_FOREACH( util::frameType frame, frames ) {
            if ( frame > currentFrame ) 
                invalidFrames.insert( frame );
        }
    }

    if ( !invalidFrames.empty() ) {
        ostringstream err;
        err << "The tag index contains " << invalidFrames.size() 
            << " invalid frames which will be deleted.";
        programLogErrorIfPossible( err.str() );

        BOOST_FOREACH( util::frameType frame, invalidFrames ) {
            removeFrameFromIndexHoldingWriteLock( frame );
        }
        return false;
    }

    return true;
}

void 
StateManager::removeFrameFromIndex( frameType frame )
{
    ScopedExclusiveLock<PthreadRWLock> scopedWriteLock( indexRWLock_ );
    removeFrameFromIndexHoldingWriteLock( frame );
}

void
StateManager::removeFrameFromIndexHoldingWriteLock( frameType frame )
{
    // The reverse index table is kept specifically for this purpose of 
    // removing a frame from the index tables.  

    {
        ostringstream info;
        info << "Removing frame " << frame << " from index.";
        programLogInfoIfPossible( info.str() );
    }

    // Find frame in reverse tag index...
    const ReverseTagIndexIter frameIter = reverseTagIndex_.find( frame );

    if ( frameIter == reverseTagIndex_.end() ) {
        ostringstream err;
        err << "Unable to find requested deletion frame " << frame << " in "
            << "reverse tag index.  Index tables are likely corrupt.";
        programLogErrorIfPossible( err.str() );
        return;
    }

    // Retrieve the next frame iter... we use this to reindex changes.  
    // If a frame is removed from an index, any changes will be indexed in 
    // the next frame which holds the new state.  
    ReverseTagIndexIter nextFrameIter( frameIter );
    ++nextFrameIter;

    // Iterate through the tag list
    const TagSetConstIter tiBegin = frameIter->second.begin();
    const TagSetConstIter tiEnd = frameIter->second.end();
    for ( TagSetConstIter ti = tiBegin; ti != tiEnd; ++ti ) {
        
        // For each tag, remove this frame from the forward index.
        TagIndexIter tag = tagIndex_.find( *ti ); 
        if ( tag == tagIndex_.end() ) {
            ostringstream err;
            err << "Unable to find referenced tag " << *ti << " in the "
                << "tag index.  Index tables are likely corrupt.";
            programLogErrorIfPossible( err.str() );
        }

        const FrameSet::size_type nRemoved = tag->second.erase( frame );
        if ( !nRemoved ) {
            ostringstream err;
            err << "Tables indicated removal of frame " << frame << " from "
                << "forward index set tagId " << tag->first << " but erase "
                << "returned having removed " << nRemoved << " elements!  "
                << "Index tables are likely corrupt.";
            programLogErrorIfPossible( err.str() );
        }

        // Now add the next frame value to this tag's frame set and 
        // simultaneously add the tag to the next frame in the reverse map.  
        // In both cases, we can ignore the return value since we don't care
        // if the frame/tag already exists (it just means there is a change at
        // that frame already and this change is lost). 
        if ( nextFrameIter != reverseTagIndex_.end() ) {
            tag->second.insert( nextFrameIter->first ); 
            nextFrameIter->second.insert( tag->first );
        } 

        // If there are no more frames for this tag ID (i.e. we've removed them
        // all), delete the whole map entry.
        if ( tag->second.empty() )
            tagIndex_.erase( tag ); 
            
    }
    
    // Finally remove the reverse index map entry.
    reverseTagIndex_.erase( frameIter );

    // Rewrite our tables 
    saveIndexFilesHoldingWriteLock();

    ostringstream trace;
    trace << "After removing frame " << frame << ".";
    traceReverseIndex( trace.str(), reverseTagIndex_ ); 
}

FrameSet
StateManager::getStateChangeFrames( const tagIDType tag ) const
{
    ScopedSharedLock<PthreadRWLock> scopedReadLock( indexRWLock_ );

    const TagIndexConstIter pos = tagIndex_.find( tag ); 
    if ( pos != tagIndex_.end() ) 
        return pos->second;
    else 
        return FrameSet();
}

bool
StateManager::tagChangedForFrame( const tagIDType tag, 
                                  const frameType frame ) const
{
    ScopedSharedLock<PthreadRWLock> scopedReadLock( indexRWLock_ );

    const ReverseTagIndexConstIter framePos = 
        reverseTagIndex_.find( frame );
    if ( framePos != reverseTagIndex_.end() ) {

        // Find tag in tag set
        const TagSetConstIter tagPos = framePos->second.find( tag );
        if ( tagPos != framePos->second.end() )
            return true;
    }

    return false;
}

carma::util::frameType
StateManager::getMostRecentSavedState( ) const
{
    ScopedSharedLock<PthreadRWLock> scopedReadLock( indexRWLock_ );

    const ReverseTagIndex::const_reverse_iterator lastPos = 
        reverseTagIndex_.rbegin();

    if ( lastPos == reverseTagIndex_.rend() ) 
        throw CARMA_ERROR( "No saved state exists." );

    return lastPos->first;
}

carma::util::frameType
StateManager::getOldestSavedState( ) const
{
    ScopedSharedLock<PthreadRWLock> scopedReadLock( indexRWLock_ );

    const ReverseTagIndexConstIter firstPos = reverseTagIndex_.begin();

    if ( firstPos == reverseTagIndex_.end() )
        throw CARMA_ERROR( "No saved state exists." );

    return firstPos->first;
}

frameType
StateManager::getClosestSavedState( frameType frame ) const
{
    ScopedSharedLock<PthreadRWLock> scopedReadLock( indexRWLock_ );

    const ReverseTagIndexConstRevIter rbegin = reverseTagIndex_.rbegin();
    const ReverseTagIndexConstRevIter rend = reverseTagIndex_.rend();
    for ( ReverseTagIndexConstRevIter riter = rbegin; riter != rend; ++riter ) {
        if ( frame >= riter->first ) 
            return riter->first;
    }
    return 0;
}

TagIndex::value_type
StateManager::parseIndexFileLine( const string & line ) const
{
    // Extract canonical name (the first field)
    const string::size_type canonEnd = line.find( " " ); // First space

    if ( canonEnd == string::npos ) return TagIndex::value_type(0, FrameSet());

    const string canonicalName = line.substr( 0, canonEnd );

    dbms::TagIDAuthority & authority = dbms::TagIDAuthority::getAuthority();

    ostringstream trace;

    tagIDType tagId;
    try {
        tagId = authority.lookupID( canonicalName );
        trace << "Read tag " << tagId << endl;
        trace << "    frames: ";
    } catch (...) {
        programLogErrorIfPossible( "Error parsing index with canonical name "
            + canonicalName + " which doesn't have a tagId." );
        return TagIndex::value_type( 0, FrameSet() );
    }

    TagIndex::value_type answer( tagId, FrameSet( ) );

    istringstream iss( line.substr( canonEnd ) ); 
    while ( true ) {
        frameType frame;
        iss >> frame;
        if ( iss ) {
            answer.second.insert( frame );
            trace << " " << frame;
        } else 
            break;
    }

    trace << endl;
    CARMA_CPTRACE( TRACE_INDEXING, trace.str() );
    
    return answer;
}

void 
StateManager::buildIndexFromScratch( const vector<string> & directories,
                                     const int numThreads )
{
    // Build master file map - map contains absolute state filenames keyed
    // by frame and automatically sorted oldest first.
    FileFrameMap frameFileMap; // This will be large

    createFileFrameMap( directories, frameFileMap );
    
    // Compare adjacent files, oldest first until 
    const FFMCI fBegin = frameFileMap.begin();
    const FFMCI fEnd = frameFileMap.end();
    FFMCI fOld = fBegin;
    FFMCI fNew = fOld;
    ++fNew;
    if ( fOld != fEnd ) initializeIndexTablesHoldingWriteLock( fOld->first );

    IndexDiffThreadArgs args;

    args.This = this;

    for ( ; fNew != fEnd; ++fOld, ++fNew ) {
        IndexDiffRequest request;
        request.newFrame = fNew->first;
        request.newFilename = fNew->second;
        request.oldFrame = fOld->first;
        request.oldFilename = fOld->second;
        
        // Oldest will be first in the deque
        args.indexDiffRequestDeque.push_back( request );
    }

    // Now start up our threads and let them get going...
    {
        AutoPthreadQuitAndJoinGroup joiner;

        for ( int t = 0; t < numThreads; ++t ) {
            pthread_t threadId = 
                StartPthreadWithRef< StateManager::IndexDiffThreadArgs >( 
                    StateManager::indexDiffThread,
                    args,
                    "StateManager::indexDiffThread" );
            joiner.insert( threadId );
        }
    } // joiner destructor blocks until all threads are done.

    // Now we should have a complete tagIndex and reverseTagIndex in the 
    // thread args.  
    ScopedExclusiveLock<PthreadRWLock> scopedWriteLock( indexRWLock_ );

    tagIndex_ = args.temporaryTagIndex;
    reverseTagIndex_ = args.temporaryReverseTagIndex;

    saveIndexFilesHoldingWriteLock();

    traceReverseIndex( "After building table from scratch.",
                       reverseTagIndex_ );
}

void
StateManager::indexDiffThread( IndexDiffThreadArgs & args )
{
    monitor::ControlSubsystem tmpControlSubsys;
            
    // Sit on the request deque and pull requests until there are none to pull
    ScopedPthreadMutexLockManager scopelock( args.indexDiffMutex );
    scopelock.LockMutex();
    while ( !args.indexDiffRequestDeque.empty() ) {

        IndexDiffRequest request = args.indexDiffRequestDeque.front( );
        args.indexDiffRequestDeque.pop_front();

        try { 

            cerr << "Indexing differences between " 
                << request.oldFilename << " (frame " << request.oldFrame << ") "
                << "and " 
                << request.newFilename << " (frame " << request.newFrame << ")."
                << endl;

            // The below operations take a long time so we take care not to 
            // serialize on the mutex. These are the main calls which benefit
            // from threadup. 
            scopelock.UnlockMutex( );

            tmpControlSubsys.setValidity( MonitorPoint::INVALID_NO_DATA );
    
            setContainerFromFile( tmpControlSubsys, request.newFilename );

            const TagSet tagIdDiffs = 
                compareContainerToFile( tmpControlSubsys, 
                                        request.oldFilename );

            scopelock.LockMutex();

            args.This->addDiffSetToTagIndices( request.newFrame, tagIdDiffs, 
                                               args.temporaryTagIndex,
                                               args.temporaryReverseTagIndex );

        } catch (...) {
            ostringstream err;
            err << "Failed to index differences for files " 
                << request.newFilename << " and " << request.oldFilename
                << " - skipping.  Error: " << getStringForCaught();
            cout << err.str() << endl;
            programLogErrorIfPossible( err.str() );
        }
    }
    scopelock.UnlockMutex( );
}

void
StateManager::restoreIndexFromFile( ) 
{ 
    std::fstream in( indexFilename_.c_str(), ios::in ); // Open for reading 

    if ( !in ) {
        const string msg("Unable to open " + indexFilename_ + " for reading.");
        if ( throwOnStateDirError_ ) {
            throw CARMA_ERROR( msg );
        } else {
            programLogWarnIfPossible( msg );
            return;
        }
    }

    TagIndex indices;

    FrameSet::size_type totalFrames = 0;
    string line;
    while ( getline( in, line ) ) {
        TagIndex::value_type index = parseIndexFileLine( line );
        totalFrames += index.second.size();
        indices.insert( index );
    }
    
    // Now rip through the map and build up the reverse index table.
    ReverseTagIndex rindices;
    
    TagSet::size_type totalTags = 0;
    const TagIndexConstIter tiBegin = indices.begin();
    const TagIndexConstIter tiEnd = indices.end();
    for ( TagIndexConstIter ti = tiBegin; ti != tiEnd; ++ti ) {
        const FrameSetConstIter fiBegin = ti->second.begin();
        const FrameSetConstIter fiEnd = ti->second.end();
        for ( FrameSetConstIter fi = fiBegin; fi != fiEnd; ++fi ) {
            ReverseTagIndexIter ri = rindices.find( *fi );
            if ( ri == rindices.end() ) {
                TagSet tagSet;
                tagSet.insert( ti->first ); 
                rindices[ *fi ] = tagSet;
                CARMA_CPTRACE( TRACE_INDEX_CREATION, "Create: Add frame " 
                    << *fi << "." );
            } else {
                ri->second.insert( ti->first );
            }
            ++totalTags;
        }
    }
        
    {  // Trace reverse iterator
        ostringstream trace;
        const ReverseTagIndexConstIter rtiBegin = rindices.begin();
        const ReverseTagIndexConstIter rtiEnd = rindices.end();
        for ( ReverseTagIndexConstIter rti = rtiBegin; rti != rtiEnd; ++rti ) {
            trace << "Frame: " << rti->first << endl;
            trace << " tags:";
            const TagSetConstIter tiBegin = rti->second.begin();
            const TagSetConstIter tiEnd = rti->second.end();
            for ( TagSetConstIter ti = tiBegin; ti != tiEnd; ++ti ) 
                trace << " " << *ti;

            trace << endl;
        }
        CARMA_CPTRACE( TRACE_INDEXING, trace.str() );
    }   

    {
        ostringstream warn;
        warn << "Restoring index tables from file: tagIndex_ contains "
            << totalFrames << " frames in " << indices.size() << " map "
            << "elements.  reverseTagIndex_ contains "
            << totalTags << " tags in " << rindices.size() << " map entries. "
            << "Total memory size for forward and reverse tables is "
            << "approximately " <<  totalFrames * sizeof( FrameSet::value_type )
            << " and " << totalTags * sizeof( TagSet::value_type ) 
            << " respectively.";
         
        const ReverseTagIndexConstIter rtiBegin = rindices.begin();
        if ( rtiBegin != rindices.end() ) {
            warn << "  Reverse tag index starts at " << rtiBegin->first;
            typedef ReverseTagIndex::const_reverse_iterator RevTagConstRevIter;
            const RevTagConstRevIter rtirBegin = rindices.rbegin();
            const RevTagConstRevIter rtirEnd = rindices.rend();
            if ( rtirBegin != rtirEnd ) 
                warn << " and ends at " << rtirBegin->first << ".";
            else 
                warn << ".";
        }

        programLogWarnIfPossible( warn.str() );
    }

    // Set the forward and reverse index only after we're all done.
    ScopedExclusiveLock<PthreadRWLock> scopelock( indexRWLock_ );  
    tagIndex_ = indices;
    reverseTagIndex_ = rindices;
    
    traceReverseIndex( "After restoring table from file.",
                       reverseTagIndex_ );
}

void
StateManager::initializeIndexTablesHoldingWriteLock( const frameType frame )
{
    // Lookup MP by tagId (can also use canonical name)
    // Currently we use the canonical name which is SLOOOW but necessary
    // to get around the fact that unique id to canonical names aren't 
    // guaranteed for on-the-fly generated MPs.
    const dbms::TagIDAuthority & authority = dbms::TagIDAuthority::getAuthority();
    const string mpCanonName( "Control.stateChangeTimestamp" );
    const tagIDType tag = authority.lookupID( mpCanonName );

    // Start the forward tag index off with the 
    tagIndex_.clear();
    FrameSet newFrameSet; 
    newFrameSet.insert( frame );
    tagIndex_[ tag ] = newFrameSet;

    // Now for the reverse index
    reverseTagIndex_.clear();
    TagSet newTagSet;
    newTagSet.insert( tag );
    reverseTagIndex_[ frame ] = newTagSet;

}

void
StateManager::indexDifferences( const frameType stateChangeFrame )
{
    if ( lastStateSaveFrame_ > 0 ) {
        ostringstream lastSavedFilename;
        lastSavedFilename << stateDir_ << "/" << filePrefix_;
        lastSavedFilename << lastStateSaveFrame_ << fileSuffix_;

        ScopedExclusiveLock<PthreadRWLock> scopedWriteLock( indexRWLock_ );

        indexDifferencesHoldingWriteLock( inputCms_->control(), 
                                          lastSavedFilename.str(),
                                          stateChangeFrame ); 

        saveIndexFilesHoldingWriteLock();
    } else { // This is the first state save file - don't index.
        ScopedExclusiveLock<PthreadRWLock> scopedWriteLock( indexRWLock_ );

        initializeIndexTablesHoldingWriteLock( stateChangeFrame );

        saveIndexFilesHoldingWriteLock();
    }
}

void 
StateManager::indexDifferencesHoldingWriteLock( MonitorContainer & container, 
                                                string filename,
                                                const frameType frame )
{
    // Make sure that filename exists.  If it doesn't, remove the corresponding
    // frame from the index tables and try the next most recent file until we
    // get a winner. 
    while ( !FileUtils::exists( filename ) ) {
        programLogErrorIfPossible( "StateManager::indexDifferences( ) - "
            + filename + " does not exist!  Removing frame from index and "
            "reverse index tables and retrieving next most recent file." );

        const frameType deadFrame = parseFrameFromFilename( 
            filename, filePrefix_, fileSuffix_ );
        removeFrameFromIndexHoldingWriteLock( deadFrame );
        filename = retrieveMostRecentFilename( );
    }
    
    const TagSet tagIdDiffs = compareContainerToFile( container, filename );

    if ( tagIdDiffs.empty() ) {
        ostringstream warn;
        warn << "Comparison of monitor container for frame " << frame 
            << " and file " << filename << " reveal they are identical!  "
            << "Frame (and tags) will not be indexed.";
        programLogWarnIfPossible( warn.str() ); 
        return;
    }

    addDiffSetToTagIndices( frame, tagIdDiffs, tagIndex_, reverseTagIndex_ );
}

void
StateManager::addDiffSetToTagIndices( 
    const frameType frame,
    const TagSet & tagIdDiffs,
    TagIndex & tagIndex,
    ReverseTagIndex & reverseTagIndex )
{
    for ( TagSetConstIter i = tagIdDiffs.begin(); i != tagIdDiffs.end(); ++i )
    {
        // For each tag, add the current frame to the tagIndex
        TagIndexIter tagIter = tagIndex.find( *i );

        if ( tagIter == tagIndex.end() ) {
            FrameSet frameSet;
            frameSet.insert( frame );
            tagIndex[ *i ] = frameSet; 
        } else {
            tagIter->second.insert( frame );
        }
    }

    // For the current frame add all the tags to the reverse map.
    reverseTagIndex[ frame ] = tagIdDiffs;
}
    

bool
StateManager::saveStateIfChanged( ) 
{
    const monitor::ControlSubsystemBase::Subarray & sub = 
        inputCms_->control().subarray( SUBARRAY_IDX );

    // Check that the timestamp is valid and make sure that the 
    // subarray controller is running and initialized.
    const MonitorPointAbstime & changed = 
        inputCms_->control().stateChangeTimestamp( );
    const MonitorPointBool & running = sub.controllerRunning();
    const MonitorPointBool & initialized = sub.controllerInitialized();

    bool stateSaved = false; 

    if ( !changed.isValid() || !running.isValid() || !initialized.isValid() ) {
        if ( savePending_ ) cancelPendingSave( true );
        return stateSaved;
    }

    if ( !running.getValue() || !initialized.getValue() ) {
        if ( savePending_ ) cancelPendingSave( true );
        return stateSaved;
    }

    if ( savePending_ ) {
        finalizeSave( ); // Finalize previous save pending
        stateSaved = true;
    }

    const double stateChangeMjd = changed.getValue( );
    const frameType stateChangeFrame = Time::computeFrame( stateChangeMjd );
    const frameType currentFrame = Time::computeCurrentFrame();
    
    // Check to see if the state has changed.  
    if ( stateChangeFrame > lastStateSaveFrame_ ) {

        // Let things settle by waiting several frames for additional changes. 
        // By waiting for things to settle down, we risk starving state saves
        // so compensate by forcing a write after so many unsettled frames.
        if ( unsettleStartFrame_ <= lastStateSaveFrame_ )
            unsettleStartFrame_ = stateChangeFrame;

        const frameType settledFrameCount = currentFrame - stateChangeFrame;
        const frameType unsettledFrameCount = stateChangeFrame - 
                                              unsettleStartFrame_;

        if ( settledFrameCount >= kSettleFrames ||
             unsettledFrameCount > kMaxUnsettlePeriod ) {

            ostringstream filename;
            filename << stateDir_ << "/" << filePrefix_;
            filename << stateChangeFrame << fileSuffix_;

            vector< string > skip;
            skip.push_back( "SlcBlankingContainer" );

            // Tear off a copy of the control subsystem.
            writeContainerToFile( inputCms_->control(), 
                                  filename.str() + ".pending", skip );

            // Now due to the fact that SAC can crash mid write and produce
            // incomplete monitor subsystems, we wait one additional frame
            // before 'finalizing' the save. This is done by marking the
            // file as 'pending' and then waiting for one more successful
            // update cycle before removing the pending label and hard linking.
            savePendingFrame_ = stateChangeFrame;
            savePendingFilename_ = filename.str();
            savePending_ = true;
        }
    }
    return stateSaved;
} // saveStateIfChanged

void
StateManager::cancelPendingSave( const bool deleteFile )
{
    if ( savePending_ ) {
        if ( deleteFile ) {
            FileUtils::removeFile( savePendingFilename_ + ".pending" );
            programLogInfoIfPossible( "Cancelling pending save for " 
                                      + savePendingFilename_ + "." );
        }
        savePending_ = false;
        savePendingFrame_ = 0;
        savePendingFilename_ = "";
    } else {
        programLogErrorIfPossible( "cancelPendingSave called but no save "
            "pending." );
    }
}

void
StateManager::finalizeSave( )
{
    if ( !savePending_ ) {
        programLogErrorIfPossible( "finalizeSave called but a save is not "
            "pending!" );
        return;
    }
    
    // TODO: The below logic needs to be transactional - if anything here 
    // fails we should properly merge out changes (to the index file in 
    // particular).

    indexDifferences( savePendingFrame_ );

    lastStateSaveFrame_ = savePendingFrame_; 

    FileUtils::rename(savePendingFilename_ + ".pending", savePendingFilename_);
    
    programLogInfoIfPossible( "State saved to " + savePendingFilename_ );

    // Create a  hard link to current state
    const string hardLinkPath(stateDir_ + "/current" + fileSuffix_ );
    if ( FileUtils::exists( hardLinkPath ) ) 
        FileUtils::removeFile( hardLinkPath ); 

    FileUtils::hardLink( savePendingFilename_, hardLinkPath );

    cancelPendingSave( false );
}

