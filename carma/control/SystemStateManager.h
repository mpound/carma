#ifndef CARMA_CONTROL_SYSTEMSTATEMANAGER_H
#define CARMA_CONTROL_SYSTEMSTATEMANAGER_H

#include "carma/corba/corba.h"
#include "carma/control/SubarrayControl.h"

#include "carma/monitor/MonitorSystemSelector.h" 
#include "carma/monitor/types.h"
#include "carma/util/types.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/PthreadRWLock.h"

#include <deque>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace carma {

namespace monitor {
    class MonitorContainer;
} // namespace monitor

namespace control {

typedef std::set< ::carma::util::frameType > FrameSet; 
typedef FrameSet::iterator FrameSetIter;
typedef FrameSet::const_iterator FrameSetConstIter;

typedef std::map< ::carma::monitor::tagIDType, FrameSet > TagIndex;
typedef TagIndex::iterator TagIndexIter;
typedef TagIndex::const_iterator TagIndexConstIter;

typedef std::set< ::carma::monitor::tagIDType > TagSet;
typedef TagSet::iterator TagSetIter;
typedef TagSet::const_iterator TagSetConstIter;

typedef std::map< ::carma::util::frameType, TagSet > ReverseTagIndex;
typedef ReverseTagIndex::iterator ReverseTagIndexIter;
typedef ReverseTagIndex::const_iterator ReverseTagIndexConstIter;
typedef ReverseTagIndex::const_reverse_iterator ReverseTagIndexConstRevIter;


class StateManager {
public:
    
    explicit StateManager ( const ::carma::monitor::CmsSelector cmsType,
                            const ::std::string & stateDirName,
                            const bool throwOnStateDirErrors );

    ~StateManager( ); // Will attempt to save state on exit

    /**
     * Update system state.  
     * @return True if filesystem state has been modified.
     */
    bool update( );

    // Big performance ouch, don't do this unless absolutely necessary. 
    void buildIndexFromScratch( const std::vector< std::string > & directories,
                                int numThreads );

    void restoreIndexFromFile( );

    void removeFrameFromIndex( ::carma::util::frameType frame );

    FrameSet getStateChangeFrames( carma::monitor::tagIDType tag ) const;

    bool tagChangedForFrame( carma::monitor::tagIDType tag, 
                             carma::util::frameType frame ) const; 

    carma::util::frameType getMostRecentSavedState( ) const;

    carma::util::frameType getOldestSavedState( ) const;

    carma::util::frameType getClosestSavedState( 
        carma::util::frameType frame ) const;

    bool verifyIndexIntegrity( );

private:
    
    bool saveStateIfChanged( ); 
    
    void restoreStateWhenUninitialized( );

    ::carma::util::frameType retrieveLastStateSaveFrame( ) const;

    ::std::string retrieveMostRecentFilename( ) const;


    TagIndex::value_type parseIndexFileLine( const ::std::string & line ) const;
    
    void saveIndexFilesHoldingWriteLock( ) const;

    void removeFrameFromIndexHoldingWriteLock( ::carma::util::frameType frame );

    void initializeIndexTablesHoldingWriteLock(::carma::util::frameType frame);

    void indexDifferences( ::carma::util::frameType stateChangeFrame );

    void indexDifferencesHoldingWriteLock ( 
        ::carma::monitor::MonitorContainer & container, // Most recent container
        ::std::string filename, // Previous saved state filename
        const ::carma::util::frameType frame ); 

    static void addDiffSetToTagIndices( ::carma::util::frameType frame,
                                 const TagSet & tagIdDiffs,
                                 TagIndex & tagIndex,
                                 ReverseTagIndex & reverseTagIndex );

    void cancelPendingSave( bool deleteFile );

    void finalizeSave( );

    ::carma::util::frameType lastStateSaveFrame_;
    ::carma::util::frameType unsettleStartFrame_;
    ::carma::util::frameType lastStateRestoreFrame_;
    ::carma::util::frameType noFileFrameCount_; 
    ::carma::monitor::CmsAP inputCms_;

    bool savePending_; 
    ::carma::util::frameType savePendingFrame_;
    ::std::string savePendingFilename_;
    
    const ::std::string stateDir_;
    const ::std::string filePrefix_;
    const ::std::string fileSuffix_;
    ::carma::control::SubarrayControl_var subarrayControl_;

    struct IndexDiffRequest {
        ::carma::util::frameType newFrame;
        ::std::string newFilename;
        ::carma::util::frameType oldFrame;
        ::std::string oldFilename;
    };

    struct IndexDiffThreadArgs {
        StateManager * This;
        ::std::deque< IndexDiffRequest > indexDiffRequestDeque;
        TagIndex temporaryTagIndex;
        ReverseTagIndex temporaryReverseTagIndex;
        carma::util::PthreadMutex indexDiffMutex; // Big mutex for whole struct
    };

    static void indexDiffThread( IndexDiffThreadArgs & args );

    // Our index maps for keeping track of what changed when.
    // The reverse index is kept for the purpose of easily removing frames but 
    // can also be used for determining which monitor points changed over
    // specific periods of time.
    mutable carma::util::PthreadRWLock indexRWLock_;
    TagIndex tagIndex_;
    ReverseTagIndex reverseTagIndex_;

    const ::std::string indexFilename_;
    const bool throwOnStateDirError_;

}; // class StateManager 

} // namespace control
} // namespace carma
#endif
