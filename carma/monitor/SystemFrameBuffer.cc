/**
 * @file
 *
 * Class for managing monitor system IPQ.
 *
 * @author: N. S. Amarnath
 *
 * $Id: SystemFrameBuffer.cc,v 1.43 2008/11/11 21:16:20 scott Exp $
 *
 * $CarmaCopyright$
 *
 */


#include <map>
#include <set>
#include <iomanip>
#include <log4cpp/Category.hh>

#include "carma/monitor/SystemFrameBuffer.h"
#include "carma/monitor/MonitorPointSet.h"
#include "carma/monitor/SystemFrame.h"
#include "carma/monitor/SystemFrameHeader.h"
#include "carma/util/IPQbuffer.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/rangeFormatting.h"
#include "carma/util/ScopedExclusiveLock.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/ScopedSharedLock.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

using namespace ::std;
using namespace ::log4cpp;
using namespace carma;
using namespace carma::util;
using namespace carma::monitor;


namespace {


typedef ScopedExclusiveLock< PthreadRWLock > ScopedWriteLock;
typedef ScopedSharedLock< PthreadRWLock > ScopedReadLock;


const size_t kVmPageBytes = 4096;
const size_t kVmPageTricksMinBytes = 16 * kVmPageBytes;


bool
pointerIsAligned( const void * const ptr,
                  const size_t       alignment ) {
    const size_t ptrAsScalar = reinterpret_cast< size_t >( ptr );
    
    return ((ptrAsScalar % alignment) == 0);
}


} // namespace < anonymous >


class SystemFrameBuffer::InternalIpq : public IPQbuffer {
    public:
        InternalIpq( SystemHeader & frame,
                     size_t         frameStorageSizeInBytes,
                     const string & fname,
                     bool           isCreator,
                     int            queueDepth );
                     
        void write( );
};


SystemFrameBuffer::InternalIpq::InternalIpq(
    SystemHeader & frame,
    const size_t   frameStorageSizeInBytes,
    const string & fname,
    const bool     isCreator,
    const int      queueDepth ) :
IPQbuffer( &frame,
           frameStorageSizeInBytes,
           fname,
           isCreator,
           queueDepth ) {
    init();
}


void
SystemFrameBuffer::InternalIpq::write( ) {
    IPQbuffer::write( );
}


SystemFrameBuffer::SystemFrameBuffer( const string & fname,
                                      const long     maxSubsystems,
                                      const long     maxMonitorPoints,
                                      const long     maxSamples,
                                      const bool     isCreator,
                                      const int      queueDepth ) :
SystemFrame( maxSubsystems, maxMonitorPoints, maxSamples ),
ipq_()
{
    const ScopedLogNdc ndc( "SystemFrameBuffer::SystemFrameBuffer " + fname );
    
    SystemHeader & writableFrameData = getSystemFrameDataHoldingWriteLock();

    if ( frameStorageSizeInBytes_ >= kVmPageTricksMinBytes ) {
        const void * const ptr = &writableFrameData;
        
        if ( pointerIsAligned( ptr, kVmPageBytes ) == false )
            programLogWarnIfPossible( "Frame storage is not page aligned" );

        if ( (frameStorageSizeInBytes_ % kVmPageBytes) != 0 )
            programLogWarnIfPossible( "Frame size is not a page multiple" );
    }

    ipq_ = auto_ptr< InternalIpq >( new InternalIpq( writableFrameData,
                                                     frameStorageSizeInBytes_,
                                                     fname,
                                                     isCreator,
                                                     queueDepth ) );
               
    syncSystemAndSubsystemsToNewFrameDataHoldingWriteLock( );
    
    clearAllTimes();
    setFrameCount( 0 );
    ipq_->setNoneAvailable();
}


SystemFrameBuffer::~SystemFrameBuffer( )
try {
} catch ( ... ) {
    // Just stifle any exceptions
    
    return;
}


void
SystemFrameBuffer::write( ) // update to shared memeory
{
    // I take write lock here instead of a read lock because I want to use the
    // lock to also coordinate all use of ipq_ in case the IPQ code isn't
    // multithread safe w.r.t. a single IPQbuffer instance
    const ScopedWriteLock writeLock( guard_ );

    ipq_->write();
}


SystemFrameBuffer &
SystemFrameBuffer::getSystemFrameBuffer( const string & name,
                                         const long     maxSubsystems,
                                         const long     maxMonitorPoints,
                                         const long     maxSamples,
                                         const int      queueDepth )
{
    const string systemName  = name + "_sysframe";

    SystemFrameBuffer * const frameBuffer =
        new SystemFrameBuffer( systemName,
                               maxSubsystems,
                               maxMonitorPoints,
                               maxSamples,
                               true,
                               queueDepth );

    return *frameBuffer;
}



MonitorPointSet &
SystemFrameBuffer::getMonitorPointSet( const ushort subsystemID,
                                       const int    numMonitorPoints,
                                       const int    numSamples )
{
    const ScopedLogNdc ndc( "SystemFrameBuffer::getMonitorPointSet()" );

    CARMA_CPTRACE(Trace::TRACE7, "Entering for subsystemID=" << subsystemID );

    int ssIndex;
    
    {
        const ScopedReadLock readLock( guard_ );
    
        ssIndex = getSubsystemIndexHoldingLock( subsystemID );
    }
    
    if ( ssIndex == SUBSYSTEM_FRAME_ABSENT ) {
        const ScopedWriteLock writeLock( guard_ );

        ssIndex = allocateSubsystemHoldingWriteLock( subsystemID,
                                                     numMonitorPoints,
                                                     numSamples );
    }

    const ScopedReadLock readLock( guard_ );

    CARMA_CPTRACE( Trace::TRACE7, "Before makeSubsystemFrameForIndexHoldingLock" );

    auto_ptr< SubsystemFrame >
        frameObj( makeSubsystemFrameForIndexHoldingLock( ssIndex ) );

    CARMA_CPTRACE(Trace::TRACE7, "Before new MonitorPointSet" );

    MonitorPointSet * monitorSet = new MonitorPointSet( frameObj );

    CARMA_CPTRACE(Trace::TRACE7, "Leaving for subsystemID=" << subsystemID );

    return *monitorSet;
}


namespace {


typedef map< ushort, size_t > DataOffsetMap;


DataOffsetMap
getDataOffsetMap( const int            numSubsystems,
                  const int * const    ssIndexArray,
                  const size_t * const ssDataOffsetArray ) {
    typedef map< ushort, size_t > BoolMap;

    DataOffsetMap result;
    
    BoolMap duplicateMap;
    
    for ( int i = 0; i < numSubsystems; ++i ) {
        const ushort ssId = ssIndexArray[ i ];
        const size_t dataOffset = ssDataOffsetArray[ i ];
        
        const DataOffsetMap::const_iterator j = result.find( ssId );
        
        if ( j != result.end() ) {
            const bool datOffsetsMatch = j->second == dataOffset;
            
            const BoolMap::iterator k = duplicateMap.find( ssId );

            if ( k == duplicateMap.end() )
                duplicateMap.insert( make_pair( ssId, datOffsetsMatch ) );
            else if ( datOffsetsMatch == false )
                k->second = false;
        } else
            result.insert( make_pair( ssId, dataOffset ) );
    }

    if ( duplicateMap.empty() == false ) {
        ostringstream oss;
        
        oss << "monitor::SystemFrameBuffer";
        
        const BoolMap::const_iterator kEnd = duplicateMap.end();

        bool firstOne = true;
        BoolMap::const_iterator k = duplicateMap.begin();
        
        for ( ; k != kEnd; ++k ) {
            if ( k->second == true )
                continue;
                
            if ( firstOne ) {
                firstOne = false;
                oss << " Subsystem IDs";
            }
            
            oss << " " << k->first;
        }
        
        if ( firstOne == false )
            oss << " have multiple data offsets that do not match.";

        firstOne = true;
        k = duplicateMap.begin();
        
        for ( ; k != kEnd; ++k ) {
            if ( k->second == false )
                continue;
                
            if ( firstOne ) {
                firstOne = false;
                oss << " Subsystem IDs";
            }
            
            oss << " " << k->first;
        }
        
        if ( firstOne == false )
            oss << " have multiple data offsets that all match.";
            
        programLogErrorIfPossible( oss.str() );
    }
    
    return result;
}


void
checkForInvalidChanges( const DataOffsetMap & oldDataOffsetMap,
                        const DataOffsetMap & newDataOffsetMap ) {
    DataOffsetMap::const_iterator i = oldDataOffsetMap.begin( );
    const DataOffsetMap::const_iterator iEnd = oldDataOffsetMap.end( );
    
    const DataOffsetMap::const_iterator jEnd = newDataOffsetMap.end( );

    multiset< ushort > disappearedSsIds;

    for ( ; i != iEnd; ++i ) {
        const ushort ssId = i->first;
        
        const DataOffsetMap::const_iterator j =
            newDataOffsetMap.find( ssId );
    
        if ( j == jEnd )
            disappearedSsIds.insert( ssId );
        else if ( i->second != j->second ) {
            ostringstream oss;
            
            oss << "Subsys id " << ssId << " changed data offset from "
                << i->second << " to " << j->second;
                
            programLogErrorIfPossible( oss.str() );
        }
    }

    if ( disappearedSsIds.empty() == false ) {
        string msg;
        
        if ( disappearedSsIds.size() == 1 ) {
            msg = "Subsys id " + formatAsRanges( disappearedSsIds ) +
                  " disappeared";
        } else {
            msg = "Subsys ids [" + formatAsRanges( disappearedSsIds ) +
                  "] disappeared";
        }
        
        programLogErrorIfPossible( msg );
    }
}


}  // namespace < anonymous >


unsigned int
SystemFrameBuffer::read( )
{
    const ScopedWriteLock writeLock( guard_ );

    const SystemHeader & frameData = getSystemFrameDataHoldingLock();

    const DataOffsetMap oldDataOffsetMap =
        getDataOffsetMap( frameData.numSubsystems,
                          frameDataSsIndexArray_,
                          frameDataSsDataOffsetArray_ );
        
    const unsigned int result = ipq_->read();
    
    syncSystemAndSubsystemsToNewFrameDataHoldingWriteLock( );

    const DataOffsetMap newDataOffsetMap =
        getDataOffsetMap( frameData.numSubsystems,
                          frameDataSsIndexArray_,
                          frameDataSsDataOffsetArray_ );
        
    checkForInvalidChanges( oldDataOffsetMap, newDataOffsetMap );

    return result;
}


bool
SystemFrameBuffer::readNewest( )
{
    const ScopedWriteLock writeLock( guard_ );

    const SystemHeader & frameData = getSystemFrameDataHoldingLock();

    const DataOffsetMap oldDataOffsetMap =
        getDataOffsetMap( frameData.numSubsystems,
                          frameDataSsIndexArray_,
                          frameDataSsDataOffsetArray_ );
        
    const bool readSuccessful = ipq_->readNewest();

    if ( readSuccessful )
        syncSystemAndSubsystemsToNewFrameDataHoldingWriteLock( );

    const DataOffsetMap newDataOffsetMap =
        getDataOffsetMap( frameData.numSubsystems,
                          frameDataSsIndexArray_,
                          frameDataSsDataOffsetArray_ );
        
    checkForInvalidChanges( oldDataOffsetMap, newDataOffsetMap );

    return readSuccessful;
}


bool
SystemFrameBuffer::readNewestIfStale( )
{
    // First check if we are stale just holding a read lock
    {
        const ScopedReadLock readLock( guard_ );
    
        if ( checkIsCurrentFrameHoldingLock( 0 ) )
            return false;
    }    
        
    const ScopedWriteLock writeLock( guard_ );

    // Check if we are still stale now that we hold a write lock
    if ( checkIsCurrentFrameHoldingLock( 0 ) )
        return false;
        
    const SystemHeader & frameData = getSystemFrameDataHoldingLock();

    const DataOffsetMap oldDataOffsetMap =
        getDataOffsetMap( frameData.numSubsystems,
                          frameDataSsIndexArray_,
                          frameDataSsDataOffsetArray_ );
        
    const bool readSuccessful = ipq_->readNewest();

    if ( readSuccessful )
        syncSystemAndSubsystemsToNewFrameDataHoldingWriteLock( );
    
    const DataOffsetMap newDataOffsetMap =
        getDataOffsetMap( frameData.numSubsystems,
                          frameDataSsIndexArray_,
                          frameDataSsDataOffsetArray_ );
        
    checkForInvalidChanges( oldDataOffsetMap, newDataOffsetMap );
    
    if ( readSuccessful ) {
        double mjdDelta = 0.0;
        
        if ( checkIsCurrentFrameHoldingLock( &mjdDelta ) == false ) {
            // Not perfect but hey it's a pretty close match to the time
            // used to determine staleness somewhere within the call to
            // checkIsCurrentFrameHoldingLock
            const double mjdNow = Time::MJD();

            const double millisecondsDelta =
                mjdDelta * Time::SECONDS_PER_DAY * 1000.0;
                
            ostringstream oss;
            
            oss << "Read the newest but still stale with a staleness of "
                << setprecision( 2 ) << millisecondsDelta << "ms (now="
                << Time::getTimeString( mjdNow, 3 ) << ", collatorWrite="
                << Time::getTimeString( frameData.collatorWriteTime, 3 )
                << ")";
                
            programLogWarnIfPossible( oss.str() );
        }
    }
    
    return readSuccessful;
}


bool
SystemFrameBuffer::readNewestConditionalCopy()
{
       
    const ScopedWriteLock writeLock( guard_ );
        
    const SystemHeader & frameData = getSystemFrameDataHoldingLock();

    const DataOffsetMap oldDataOffsetMap =
        getDataOffsetMap( frameData.numSubsystems,
                          frameDataSsIndexArray_,
                          frameDataSsDataOffsetArray_ );
        
    const bool copy = ipq_->readNewestConditionalCopy();
    if (false) {
        Category&  log = carma::util::Program::getLogger(); 
        log << Priority::ERROR << "readNCC returns " << copy ;
    }          
    if (copy == false) return copy;
    
    const DataOffsetMap newDataOffsetMap =
        getDataOffsetMap( frameData.numSubsystems,
                          frameDataSsIndexArray_,
                          frameDataSsDataOffsetArray_ );
        
    checkForInvalidChanges(oldDataOffsetMap, newDataOffsetMap);
    
    return copy;
}


void
SystemFrameBuffer::setNoneAvailable( ) {
    // I take write lock here instead of a read lock because I want to use the
    // lock to also coordinate all use of ipq_ in case the IPQ code isn't
    // multithread safe w.r.t. a single IPQbuffer instance
    const ScopedWriteLock writeLock( guard_ );

    ipq_->setNoneAvailable();
}
