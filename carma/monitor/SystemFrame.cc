/**
 * @file
 *
 * Mthod definitions for class wrapper for monitor system frame structure 
 * that manages storage for a monitor system frame.
 *
 * @author: N. S. Amarnath
 *
 * $Id: SystemFrame.cc,v 1.52 2010/09/30 17:17:34 abeard Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <iosfwd>
#include <map>

#include "carma/monitor/SystemFrame.h"
#include "carma/monitor/SystemFrameHeader.h"
#include "carma/monitor/SubsystemFrame.h"
#include "carma/monitor/SubsystemFrameHeader.h"
#include "carma/monitor/IncompatibleFrameDataError.h"
#include "carma/util/Time.h"
#include "carma/util/programLogging.h"
#include "carma/util/checking.h"
#include "carma/util/compileTimeCheck.h"
#include "carma/util/ErrorException.h"
#include "carma/util/memoryUtils.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/ScopedExclusiveLock.h"
#include "carma/util/ScopedSharedLock.h"
#include "carma/util/Trace.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


namespace carma  {
    namespace monitor  {
    // The following two constants are status flag bits
    const unsigned short  SUBFRAME_WRITTEN_FLAG = 0x100;
    const unsigned short  FRAME_SUBFRAMES_COMPLETE = 0x001;

    };
};


const int SystemFrame::SUBSYSTEM_FRAME_ABSENT = -1;


namespace {


typedef ScopedExclusiveLock< PthreadRWLock > ScopedWriteLock;
typedef ScopedSharedLock< PthreadRWLock > ScopedReadLock;

const size_t BOGUS_SUBSYSTEM_DATA_OFFSET = ~0UL;


void *
allocateStorage( const size_t bytes ) 
{
    const ScopedLogNdc ndc( "SystemFrame allocateStorage" );
    
    void * storage = 0;

    if ( valueIsVmPageMultiple( bytes ) ) {
        const size_t vmPageSize = roundUpToVmPageMultiple( 1 );
        
        const int err = posix_memalign( &storage, vmPageSize, bytes );
        
        if ( err != 0 ) {
            ostringstream oss;
            
            oss << "posix_memalign failed ("
                << err << ", \""
                << strerror( err ) << "\")";
            
            if ( storage == 0 )
                programLogWarnIfPossible( oss.str() );
            else {
                oss << " but still returned a non-NULL pointer ("
                    << storage << ")";
                
                storage = 0;

                programLogErrorIfPossible( oss.str() );
            }
        } else if ( storage == 0 ) {
            programLogErrorIfPossible( "posix_memalign did not fail"
                                       " but still returned a NULL pointer" );
        } else if ( pointerIsVmPageAligned( storage ) != true ) {
            ostringstream oss;
        
            oss << "Start address " << storage << " of " << bytes 
                << " byte posix_memalign  block not aligned"
                << " with the start of a VM page";
            
            programLogWarnIfPossible( oss.str() );
        }
    }
    
    if ( storage == 0 ) {
        storage = ::malloc( bytes );

        if ( storage == 0 )
            throw CARMA_ERROR( "SystemFrame allocateStorage malloc failed" );
    }
        
    return storage;
}


void
deallocateStorage( void * const ptr )
{
    if ( ptr != 0 )
        ::free( ptr );
}


}  // namespace < anonymous >


IncompatibleFrameDataError::IncompatibleFrameDataError(
    const string &     msg,
    const char * const filename,
    const int          lineNo ) :
util::ErrorException( msg, filename, lineNo )
{
}


class SystemFrame::SsDataPointersManager {
    public:
        explicit SsDataPointersManager( );

        ~SsDataPointersManager( );
        
        SubsystemDataPointers & findOrAdd( ushort subsysId );
        
        SubsystemDataPointers & find( ushort subsysId ) const;

    private:
        typedef map< ushort, SubsystemDataPointers * > Map;
        
        Map map_;
};


SystemFrame::SsDataPointersManager::SsDataPointersManager( )
{
}


SystemFrame::SsDataPointersManager::~SsDataPointersManager( )
try {
    Map::const_iterator i = map_.begin();
    const Map::const_iterator iEnd = map_.end();
    
    for ( ; i != iEnd; ++i )
        delete (i->second);
} catch ( ... ) {
    // Just stifle any exceptions
    
    return;
}


SubsystemDataPointers &
SystemFrame::SsDataPointersManager::find( const ushort subsysId ) const
{
    const Map::const_iterator i = map_.find( subsysId );
    
    if ( i == map_.end() ) {
        const string msg = "subsystem id not found"
                           " in SystemFrame::SsDataPointersManager::find";
    
        programLogErrorIfPossible( msg );
    
        throw CARMA_ERROR( msg );
    }

    return *(i->second);
}


SubsystemDataPointers &
SystemFrame::SsDataPointersManager::findOrAdd( const ushort subsysId )
{
    SubsystemDataPointers * ssDP = 0;

    const Map::iterator i = map_.find( subsysId );
    
    if ( i == map_.end() ) {
        ssDP = new SubsystemDataPointers;

        map_.insert( make_pair( subsysId, ssDP ) );
    } else
        ssDP = i->second;
    
    return *ssDP;
}


SystemFrame::SystemFrame( const long maxSubsystems, 
                          const long maxMonitorPoints, 
                          const long maxSamples ) :
maxSubsystems_( maxSubsystems ),
maxMonitorPoints_( maxMonitorPoints ),
maxSamples_( maxSamples ),
frameStorageSizeInBytes_( getSystemFrameStorageSizeInBytes( maxSubsystems,
                                                            maxMonitorPoints,
                                                            maxSamples ) ),
guard_( ),
frameDataSsIndexArray_( 0 ),
frameDataSsDataOffsetArray_( 0 ),
frameStorage_( 0 ),
writableFrameDataSsIndexArray_( 0 ),
writableFrameDataSsDataOffsetArray_( 0 ),
ssDataPointersManager_( new SsDataPointersManager ),
numCleanSubsystemFrames_( 0 ),
nextFreeSubsystemFrame_( 0 )
{

#if __WORDSIZE == 64
    compileTimeCheck< sizeof( SystemHeader ) == 64 >( );
#else
    compileTimeCheck< sizeof( SystemHeader ) == 60 >( );
#endif

    frameStorage_ = allocateStorage( frameStorageSizeInBytes_ );

    if ( frameStorage_ == 0 )
        throw CARMA_ERROR( "Frame storage allocation failed" );

    const size_t ssIndexArrayOffset = sizeof( SystemHeader );
        
    writableFrameDataSsIndexArray_ =
        static_cast< int * >( byteOffsetPointer( frameStorage_,
                                                 ssIndexArrayOffset ) );

    frameDataSsIndexArray_ = writableFrameDataSsIndexArray_;
    
    memset( frameStorage_, 0, frameStorageSizeInBytes_ );
    
    SystemHeader & writableFrameData = getSystemFrameDataHoldingWriteLock();
    
    writableFrameData.maxSubsystems = maxSubsystems;
    writableFrameData.maxMonitorPoints = maxMonitorPoints;
    writableFrameData.maxSamples = maxSamples;
    writableFrameData.numSubsystems = 0;
    
    syncSystemAndSubsystemsToNewFrameDataHoldingWriteLock();
}


SystemFrame::~SystemFrame( )
try {
    // BULLSHIT_TWC: change this to an auto_ptr
    delete ssDataPointersManager_;

    if ( frameStorage_ != 0 ) {
        void * deadManWalking = 0;
        
        ::std::swap( deadManWalking, frameStorage_ );
        
        deallocateStorage( deadManWalking );
    }
} catch ( ... ) {
    // Just stifle any exceptions
    
    return;
}


SystemFrame::SystemHeader &
SystemFrame::getSystemFrameDataHoldingWriteLock( ) {
    return *(static_cast< SystemHeader *>( frameStorage_ ));
}


const SystemFrame::SystemHeader &
SystemFrame::getSystemFrameDataHoldingLock( ) const {
    return *(static_cast< const SystemHeader *>( frameStorage_ ));
}


void
SystemFrame::synchronize( const SystemFrame & src )
{
    const ScopedLogNdc ndc( "SystemFrame::synchronize()" );
    
    if ( (src.frameStorageSizeInBytes_ != frameStorageSizeInBytes_) ||
         (src.maxSubsystems_ != maxSubsystems_) ||
         (src.maxMonitorPoints_ != maxMonitorPoints_) ||
         (src.maxSamples_ != maxSamples_) )  {
        ostringstream oss;
        
        oss << "objects incommensurate -";
        
        if ( src.frameStorageSizeInBytes_ != frameStorageSizeInBytes_ ) {
            oss << " Frame storage size in bytes I have is " 
                << frameStorageSizeInBytes_
                << "!= frame storage size in bytes source has " 
                << src.frameStorageSizeInBytes_ << ".";
        }
        
        if ( src.maxSubsystems_ != maxSubsystems_ ) {
            oss << " Max # of subsystems I can have is " 
                << maxSubsystems_
                << "!= max # of subsystems source can have " 
                << src.maxSubsystems_ << ".";
        }
        
        if ( src.maxMonitorPoints_ != maxMonitorPoints_ ) {
            oss << " Max # of monitor points I can have is " 
                << maxMonitorPoints_
                << "!= max # of monitor points source can have is " 
                << src.maxMonitorPoints_ << ".";
        }
        
        if ( src.maxSamples_ != maxSamples_ ) {
            oss << " Max # of monitor samples I can have is " 
                << maxSamples_
                << "!= max # of monitor samples source can have is " 
                << src.maxSamples_ << ".";
        }

        throw CARMA_ERROR( oss.str() );
    }
    
    const ScopedReadLock srcReadLock( src.guard_ );
    
    const SystemHeader & srcFrameData = src.getSystemFrameDataHoldingLock();
    
    if ( (srcFrameData.maxSubsystems != maxSubsystems_) ||
         (srcFrameData.maxMonitorPoints != maxMonitorPoints_) ||
         (srcFrameData.maxSamples != maxSamples_) )  {
        ostringstream oss;
        
        oss << "Source frame data is incompatible -";
        
        if ( srcFrameData.maxSubsystems != maxSubsystems_ ) {
            oss << " Max # of subsystems I can have is " 
                << maxSubsystems_
                << " != max # of subsystems source frame data can have " 
                << srcFrameData.maxSubsystems << ".";
        }
        
        if ( srcFrameData.maxMonitorPoints != maxMonitorPoints_ ) {
            oss << " Max # of monitor points I can have is " 
                << maxMonitorPoints_
                << " != max # of monitor points source frame data can have is "
                << srcFrameData.maxMonitorPoints << ".";
        }
        
        if ( srcFrameData.maxSamples != maxSamples_ ) {
            oss << " Max # of monitor samples I can have is " 
                << maxSamples_
                << " != max # of monitor samples source frame data can have is "
                << srcFrameData.maxSamples << ".";
        }

        throw CARMA_EXCEPTION( IncompatibleFrameDataError, oss.str() );
    }
    
    const ScopedWriteLock writeLock( guard_ );

    SystemHeader & writableFrameData = getSystemFrameDataHoldingWriteLock();

    memcpy( &writableFrameData, &srcFrameData, src.frameStorageSizeInBytes_ );
    
    syncSystemAndSubsystemsToNewFrameDataHoldingWriteLock( );
}


bool        
SystemFrame::isCurrentFrame( ) const
{
    const ScopedReadLock readLock( guard_ );
    
    return checkIsCurrentFrameHoldingLock( 0 );
}    
    
    
bool        
SystemFrame::checkIsCurrentFrameHoldingLock( double * const mjdDelta ) const
{
    const double mjdTimestamp =
        getSystemFrameDataHoldingLock().collatorWriteTime;
    
    return SubsystemFrame::mjdIsCurrentFrame( mjdTimestamp, mjdDelta );
}


auto_ptr< SubsystemFrame >
SystemFrame::makeSubsystemFrame( const ushort subsystemID ) const
{
    const ScopedReadLock readLock( guard_ );

    const int ssIndex = getSubsystemIndexHoldingLock( subsystemID );
    
    if ( ssIndex == SUBSYSTEM_FRAME_ABSENT )
        throw CARMA_ERROR( "Bad subsystem ID" );
        
    SubsystemHeader & ssFrame =
        getSubsystemFrameStructHoldingLock( ssIndex );

    SubsystemDataPointers & ssDataPointers =
        ssDataPointersManager_->find( ssFrame.subsystemID );

    auto_ptr< SubsystemFrame >
        result( new SubsystemFrame( ssFrame, ssDataPointers, true ) );

    return result;
}


auto_ptr< SubsystemFrame >
SystemFrame::makeSubsystemFrameForIndexHoldingLock( const int ssIndex ) const
{
    SubsystemHeader & ssFrame =
        getSubsystemFrameStructHoldingLock( ssIndex );

    SubsystemDataPointers & ssDataPointers =
        ssDataPointersManager_->find( ssFrame.subsystemID );

    auto_ptr< SubsystemFrame >
        result( new SubsystemFrame( ssFrame, ssDataPointers, true ) );

    return result;
}


auto_ptr< SubsystemFrame >
SystemFrame::makeSubsystemFrameForIndex( const int ssIndex ) const
{
    const ScopedReadLock readLock( guard_ );

    return makeSubsystemFrameForIndexHoldingLock( ssIndex );
}


bool        
SystemFrame::isCurrent (ushort subsystemID) const
{
    const auto_ptr< SubsystemFrame >
        subsystem = makeSubsystemFrame( subsystemID );
    
    return subsystem->isCurrentFrame();
}


long
SystemFrame::getFrameCount( ) const
{
    const ScopedReadLock readLock( guard_ );

    return getSystemFrameDataHoldingLock().frameCount;
}


long        
SystemFrame::setFrameCount( const long frameCount )
{
    const ScopedWriteLock writeLock( guard_ );

    SystemHeader & writableFrameData = getSystemFrameDataHoldingWriteLock();

    writableFrameData.frameCount = frameCount;
    
    return writableFrameData.frameCount;
}


int
SystemFrame::getSubsystemIndexHoldingLock( const ushort subsystemID ) const
{
    const SystemHeader & frameData = getSystemFrameDataHoldingLock();
    
    if (frameData.numSubsystems == 0)
        return SUBSYSTEM_FRAME_ABSENT;

    int         start = 0;
    int         end = (frameData.numSubsystems - 1);
    int         indexSubsystemID = 0;
    int         ssIndex = 0;
    int         interval = 0;

    int startIndex = frameDataSsIndexArray_[start];
    int endIndex = frameDataSsIndexArray_[end];
    long minSubsystemID = getSubsystemFrameStructHoldingLock(startIndex).subsystemID;
    long maxSubsystemID = getSubsystemFrameStructHoldingLock(endIndex).subsystemID;
    if ((subsystemID < minSubsystemID)  ||  (subsystemID > maxSubsystemID))
        return SUBSYSTEM_FRAME_ABSENT;

    while ((interval = (end - start)) > 2)  {
        int middle = (start+end) >> 1;
        int middleIndex = frameDataSsIndexArray_[middle];
        long middleSubsystemID = getSubsystemFrameStructHoldingLock(middleIndex).subsystemID;
        if (subsystemID > middleSubsystemID)  {
            start = middle;
            minSubsystemID = middleSubsystemID;
        }  else  {
            end = middle;
            maxSubsystemID = middleSubsystemID;
        }
    }

    ssIndex = frameDataSsIndexArray_[start],
    indexSubsystemID = getSubsystemFrameStructHoldingLock(ssIndex).subsystemID;
    for (int  i = start;  i <= end  &&  subsystemID != indexSubsystemID;  i++)  {
        ssIndex = frameDataSsIndexArray_[i];
        indexSubsystemID = getSubsystemFrameStructHoldingLock(ssIndex).subsystemID;
    }

    if (subsystemID == indexSubsystemID)
        return ssIndex;

    return SUBSYSTEM_FRAME_ABSENT;
}


bool        
SystemFrame::isComplete( ) const
{
    return (numCleanSubsystemFrames_ == 0);
}

void        
SystemFrame::writeSubsystemFrame(
    const TransportSubsystemFrame & transportFrame )
{
    const ScopedWriteLock writeLock( guard_ );

    int ssIndex = getSubsystemIndexHoldingLock( transportFrame.subsystemID );

    if ( ssIndex == SUBSYSTEM_FRAME_ABSENT )
        return; // perhaps throw an exception ?

    SubsystemHeader & ssFrame =
        getSubsystemFrameStructHoldingLock( ssIndex );

    SubsystemDataPointers & ssDataPointers =
        ssDataPointersManager_->find( transportFrame.subsystemID );

    // If we arent talking about the same subsystem, we're in trouble anyway
    CARMA_CHECK (ssFrame.subsystemID == transportFrame.subsystemID);
    if ((ssFrame.maxMonitorPoints != transportFrame.maxMonitorPoints)
            ||  (ssFrame.maxSamples != transportFrame.maxSamples))  {
        //
        // FIX - WHAT SHOULD REALLY HAPPEN HERE?
        // Should it be logged?  Should this whole if block be removed?
        // What happens if CARMA_CHECK fails --  cerr goes to /dev/null
        // in the running system!
        // MWP 9/1/2005
        // BULLSHIT_TWC 8 Sept 2005
        // log4cpp::Category& logger = Program::getLogger();
        ostringstream os;
        os << "ssFrame.subsystemID: " << ssFrame.subsystemID
           << "\nssFrame.maxMonitorPoints = " << ssFrame.maxMonitorPoints
           << "\nssFrame.maxSamples = " << ssFrame.maxSamples
           << "transportFrame.subsystemID: " << ssFrame.subsystemID
           << "\ntransportFrame.maxMonitorPoints = " << ssFrame.maxMonitorPoints
           << "\ntransportFrame.maxSamples = " << ssFrame.maxSamples
           << "\n";
    }
    CARMA_CHECK (ssFrame.maxMonitorPoints == transportFrame.maxMonitorPoints);
    CARMA_CHECK (ssFrame.maxSamples == transportFrame.maxSamples);

    SubsystemFrame ssWriteFrame( ssFrame, ssDataPointers, true );

    ssWriteFrame.writeFromTransportFrame (transportFrame);

    // If subsystem was written, then decrease the number of 
    // clean subsystems. 
    if (! GetFlag (ssFrame.statusFlags, SUBFRAME_WRITTEN_FLAG))  {
        SetFlag (ssFrame.statusFlags, SUBFRAME_WRITTEN_FLAG);

        if ( numCleanSubsystemFrames_ < 1 ) {
            CARMA_CPTRACE( Trace::TRACE2, "numCleanSubsystemFrames_ underflow" );
            // programLogErrorIfPossible( "numCleanSubsystemFrames_ underflow" );
        } else
            --numCleanSubsystemFrames_;

        if ( numCleanSubsystemFrames_ == 0 ) {
            SystemHeader & writableFrameData =
                getSystemFrameDataHoldingWriteLock();
            
            SetFlag( writableFrameData.statusFlags, FRAME_SUBFRAMES_COMPLETE );
        }
    }
}

long        
SystemFrame::getMaxNumSubsystemFrames ()  const
{
    return maxSubsystems_;
}


long        
SystemFrame::getMaxTotalMonitorPoints ()  const
{
    return maxMonitorPoints_;
}


long        
SystemFrame::getMaxTotalSamples ()  const
{
    return maxSamples_;
}


long        
SystemFrame::getNumSubsystemFrames( ) const
{
    const ScopedReadLock readLock( guard_ );

    return getSystemFrameDataHoldingLock().numSubsystems;
}


void        
SystemFrame::setNumSubsystemFrames( const ushort numSubsystems )
{
    const ScopedWriteLock writeLock( guard_ );

    getSystemFrameDataHoldingWriteLock().numSubsystems = numSubsystems;
}


void        
SystemFrame::setCollatorWriteDelay( const double writeDelay )
{
    const ScopedWriteLock writeLock( guard_ );

    getSystemFrameDataHoldingWriteLock().collatorWriteDelay = writeDelay;
}



double      
SystemFrame::getCollatorWriteDelay( ) const
{
    const ScopedReadLock readLock( guard_ );

    return getSystemFrameDataHoldingLock().collatorWriteDelay;
}


void
SystemFrame::setCollatorWriteTime( const double mjdTimestamp )
{
    const ScopedWriteLock writeLock( guard_ );

    getSystemFrameDataHoldingWriteLock().collatorWriteTime = mjdTimestamp;
}


/*
void
SystemFrame::setCollatorWriteTime( )
{
//    setCollatorWriteTime( Time::MJD() );
}
*/


double      
SystemFrame::getCollatorWriteTime( ) const
{
    const ScopedReadLock readLock( guard_ );

    return getSystemFrameDataHoldingLock().collatorWriteTime;
}


void
SystemFrame::setRawReadTime( const double mjdTimestamp )
{
    const ScopedWriteLock writeLock( guard_ );

    getSystemFrameDataHoldingWriteLock().rawReadTime = mjdTimestamp;
}


/*
void
SystemFrame::setRawReadTime( )
{
//    setRawReadTime( Time::MJD() );
}
*/


double      
SystemFrame::getRawReadTime( ) const
{
    const ScopedReadLock readLock( guard_ );

    return getSystemFrameDataHoldingLock().rawReadTime;
}


void
SystemFrame::setFinalWriteTime( const double mjdTimestamp )
{
    const ScopedWriteLock writeLock( guard_ );

    getSystemFrameDataHoldingWriteLock().finalWriteTime = mjdTimestamp;
}


/*
void
SystemFrame::setFinalWriteTime( )
{
//    setFinalWriteTime( Time::MJD() );
}
*/


double      
SystemFrame::getFinalWriteTime( ) const
{
    const ScopedReadLock readLock( guard_ );

    return getSystemFrameDataHoldingLock().finalWriteTime;
}


void        
SystemFrame::clearAllTimes ()
{
    const double resetTime = 0.0;

    setCollatorWriteTime (resetTime);
    setRawReadTime (resetTime);
    setFinalWriteTime (resetTime);
}


unsigned char
SystemFrame::getStatusFlags( ) const
{
    const ScopedReadLock readLock( guard_ );

    return getSystemFrameDataHoldingLock().statusFlags;
}


void        
SystemFrame::setStatusFlags( const unsigned char flags )
{
    const ScopedWriteLock writeLock( guard_ );

    getSystemFrameDataHoldingWriteLock().statusFlags = flags;
}


bool        
SystemFrame::subsystemFrameIsWritten( const ushort subsystemID ) const
{
    const ScopedReadLock readLock( guard_ );

    const int ssIndex = getSubsystemIndexHoldingLock( subsystemID );
    
    if ( ssIndex == SUBSYSTEM_FRAME_ABSENT )
        return false; // perhaps throw an exception ?

    SubsystemHeader & ssFrame = getSubsystemFrameStructHoldingLock( ssIndex );

    return (GetFlag( ssFrame.statusFlags, SUBFRAME_WRITTEN_FLAG ) == SUBFRAME_WRITTEN_FLAG);
}


int
SystemFrame::allocateSubsystemHoldingWriteLock(
    const ushort newSubsystemID, 
    const long   newSsMaxMonitorPoints, 
    const long   newSsMaxSamples )
{
    {
        const int ssIndex = getSubsystemIndexHoldingLock( newSubsystemID );
        
        if ( ssIndex != SUBSYSTEM_FRAME_ABSENT )
            return ssIndex;
    }
    
    const int newSsIndex = getSystemFrameDataHoldingLock().numSubsystems;
    
    CARMA_CHECK (newSsIndex >= 0  &&  newSsIndex <= maxSubsystems_);

    if (newSsIndex == maxSubsystems_)  {
        ostringstream os;
        os << "Cannot allocate space for subsystemID " << newSubsystemID 
           << " - exceeds max # of subsystems = " << maxSubsystems_;
        throw CARMA_ERROR (os.str());
    }

    writableFrameDataSsDataOffsetArray_[ newSsIndex ] =
        nextFreeSubsystemFrame_;

    SubsystemHeader & newSsFrame =
        getSubsystemFrameStructHoldingLock( newSsIndex );
        
    SubsystemDataPointers & newSsDataPointers =
        ssDataPointersManager_->findOrAdd( newSubsystemID );

    newSsFrame.subsystemID = newSubsystemID;
    
    SubsystemFrame::setFrameSize( newSsFrame,
                                  newSsDataPointers,
                                  newSsMaxMonitorPoints,
                                  newSsMaxSamples );
    
    newSsFrame.subsystemID = newSubsystemID;

    indexInsertKeyHoldingWriteLock( newSubsystemID, newSsIndex );

    nextFreeSubsystemFrame_ = getNextFreeSubsystemFrameHoldingLock( );
    
    return newSsIndex;
}


int
SystemFrame::allocateSubsystemFrame( const ushort newSubsystemID, 
                                     const long   newSsMaxMonitorPoints, 
                                     const long   newSsMaxSamples )
{
    const ScopedWriteLock writeLock( guard_ );

    return allocateSubsystemHoldingWriteLock( newSubsystemID,
                                              newSsMaxMonitorPoints,
                                              newSsMaxSamples );
}


void
SystemFrame::resetNumCleanFrames()
{
    const ScopedWriteLock writeLock( guard_ );

    numCleanSubsystemFrames_ = getSystemFrameDataHoldingLock().numSubsystems;
}


size_t        
SystemFrame::getSystemFrameStorageSizeInBytes( const long maxSubsystems,
                                               const long maxMonitorPoints,
                                               const long maxSamples)
{
    ushort remainder;
    const  size_t wordsize = sizeof(int);  // compiler definition - see <bits/typesizes.h>

    long numSamples = SubsystemFrame::maxNumSamplesIncludingAverages 
                                              (maxMonitorPoints, maxSamples);
    size_t frameSize = (sizeof (SystemHeader) 
                     + maxSubsystems*(sizeof(SubsystemHeader) + sizeof (int) + sizeof (size_t))
                     + maxMonitorPoints*(sizeof(MonitorHeader) + sizeof (int))
                     + numSamples*sizeof(MonitorSampleValue));

    // round up to wordsize to ensure that we are aligned to 
    // a word boundary
    if ((remainder = frameSize % wordsize) > 0)
        frameSize += (wordsize - remainder);

    // to allow for roundup at the subsystem frame level
    size_t resultBytes = frameSize + maxSubsystems * wordsize;

    if ( resultBytes >= getDefaultVmMemoryCopyMinWinBytes() )
        resultBytes = roundUpToVmPageMultiple( resultBytes );
    
    return resultBytes;
}


void
SystemFrame::syncSystemAndSubsystemsToNewFrameDataHoldingWriteLock( )
{
    const SystemHeader & frameData = getSystemFrameDataHoldingLock();

    if ( (frameData.maxSubsystems != maxSubsystems_) ||
         (frameData.maxMonitorPoints != maxMonitorPoints_) ||
         (frameData.maxSamples != maxSamples_) )  {
        ostringstream oss;
        
        oss << "New frame data is incompatible -";
        
        if ( frameData.maxSubsystems != maxSubsystems_ ) {
            oss << " Max # of subsystems I can have is " 
                << maxSubsystems_
                << " != max # of subsystems frame data can have " 
                << frameData.maxSubsystems << ".";
        }
        
        if ( frameData.maxMonitorPoints != maxMonitorPoints_ ) {
            oss << " Max # of monitor points I can have is " 
                << maxMonitorPoints_
                << " != max # of monitor points frame data can have is " 
                << frameData.maxMonitorPoints << ".";
        }
        
        if ( frameData.maxSamples != maxSamples_ ) {
            oss << " Max # of monitor samples I can have is " 
                << maxSamples_
                << " != max # of monitor samples frame data can have is " 
                << frameData.maxSamples << ".";
        }

        programLogErrorIfPossible( oss.str() );

        // NOTE: I was thinking about gating this throw with 
        //       "if ( frameData.maxSubsystems != 0 )" but we'll see if
        //       it causes problems to not gate it
        throw CARMA_EXCEPTION( IncompatibleFrameDataError, oss.str() );
    }
    
    const int ssArrayCount = frameData.maxSubsystems;

    const size_t ssIndexArrayOffset = sizeof( SystemHeader );
    const size_t ssIndexArrayBytes =
        ssArrayCount * sizeof( frameDataSsIndexArray_[ 0 ] );
        
    const size_t ssDataOffsetArrayOffset =
        ssIndexArrayOffset + ssIndexArrayBytes;

    writableFrameDataSsDataOffsetArray_ =
        static_cast< size_t * >( byteOffsetPointer( frameStorage_,
                                                    ssDataOffsetArrayOffset ) );

    frameDataSsDataOffsetArray_ = writableFrameDataSsDataOffsetArray_;

    const int ssCount = frameData.numSubsystems;

    for ( int i = ssCount; i < ssArrayCount; ++i ) {
        if ( frameDataSsIndexArray_[ i ] != SUBSYSTEM_FRAME_ABSENT )
            writableFrameDataSsIndexArray_[ i ] = SUBSYSTEM_FRAME_ABSENT;
    }
    
    for ( int i = ssCount; i < ssArrayCount; ++i ) {
        if ( frameDataSsDataOffsetArray_[ i ] != BOGUS_SUBSYSTEM_DATA_OFFSET )
            writableFrameDataSsDataOffsetArray_[ i ] = BOGUS_SUBSYSTEM_DATA_OFFSET;
    }
    
    for ( int i = 0;  i < ssCount; ++i ) {
        SubsystemHeader & ssFrame =
            getSubsystemFrameStructHoldingLock( i );
            
        SubsystemDataPointers & ssDataPointers =
            ssDataPointersManager_->findOrAdd( ssFrame.subsystemID );
            
        SubsystemFrame::syncSubsystemToNewFrameData( ssFrame, ssDataPointers );
    }
    
    numCleanSubsystemFrames_ = frameData.numSubsystems;
}


SubsystemHeader &
SystemFrame::getSubsystemFrameStructHoldingLock( const int ssIndex ) const
{
    const size_t ssDataOffset = frameDataSsDataOffsetArray_[ ssIndex ];
    
    if ( ssDataOffset == BOGUS_SUBSYSTEM_DATA_OFFSET ) {
        const string msg = "Bogus ssDataOffset";
        
        programLogErrorIfPossible( msg );
        
        throw CARMA_ERROR( msg );
    }

    const int ssArrayCount = getSystemFrameDataHoldingLock().maxSubsystems;

    const size_t ssIndexArrayOffset = sizeof( SystemHeader );
    const size_t ssIndexArrayBytes =
        ssArrayCount * sizeof( frameDataSsIndexArray_[ 0 ] );

    const size_t ssDataOffsetArrayOffset =
        ssIndexArrayOffset + ssIndexArrayBytes;
    const size_t ssDataOffsetArrayBytes =
        ssArrayCount * sizeof( frameDataSsDataOffsetArray_[ 0 ] );

    const size_t ssDataBaseOffset =
        ssDataOffsetArrayOffset + ssDataOffsetArrayBytes;

    void * const ssData =
        byteOffsetPointer( frameStorage_, (ssDataBaseOffset + ssDataOffset) );

    return *(static_cast< SubsystemHeader * >( ssData ));
}


void
SystemFrame::indexInsertKeyHoldingWriteLock( const ushort newSubsystemID,
                                             const int    newSsIndex )
{
    const ScopedLogNdc ndc( "SystemFrame::indexInsertKeyHoldingWriteLock()" );
    
    const int ssCount =
        getSystemFrameDataHoldingLock().numSubsystems;
        
    // Check we have room left for another subsystem
    if ( ssCount >= maxSubsystems_ ) {
        const string msg = "Subsystem count overflow";
        
        programLogErrorIfPossible( msg );
        
        throw CARMA_ERROR( msg );
    }

    // Check the new index is okay
    if ( (newSsIndex < 0) || (newSsIndex >= maxSubsystems_) ) {
        const string msg = "New ssIndex is invalid";
        
        programLogErrorIfPossible( msg );
        
        throw CARMA_ERROR( msg );
    }

    // Check the subsystem frame has the proper subsystem ID set
    {
        const SubsystemHeader & newSsFrame =
            getSubsystemFrameStructHoldingLock( newSsIndex );
            
        if ( newSsFrame.subsystemID != newSubsystemID )
            programLogWarnIfPossible( "Subsystem ID mismatch" );
    }
    
    // Scan forward looking for our insertion point
    int i;
    for ( i = 0; i < ssCount; ++i ) {
        const int ssIndex = frameDataSsIndexArray_[ i ];
        
        if ( ssIndex == newSsIndex ) {
            const string msg = "New ssIndex is already in use";
            
            programLogErrorIfPossible( msg );
            
            throw CARMA_ERROR( msg );
        }
        
        const SubsystemHeader & ssFrame =
            getSubsystemFrameStructHoldingLock( ssIndex );

        if ( ssFrame.subsystemID == newSubsystemID ) {
            const string msg = "New subsystem ID is already in use";
            
            programLogErrorIfPossible( msg );
            
            throw CARMA_ERROR( msg );
        }

        if ( ssFrame.subsystemID > newSubsystemID )
            break;
    }

    // Shift all the elements after the insertion point forward to make a gap
    for ( int j = ssCount; j > i; --j ) {
        const int ssIndex = frameDataSsIndexArray_[ j - 1 ];

        if ( ssIndex == newSsIndex ) {
            const string msg = "New ssIndex is already in use";
            
            programLogErrorIfPossible( msg );
            
            throw CARMA_ERROR( msg );
        }

        writableFrameDataSsIndexArray_[ j ] = ssIndex;
    }
    
    // Insert the new element in the gap
    writableFrameDataSsIndexArray_[ i ] = newSsIndex;

    getSystemFrameDataHoldingWriteLock().numSubsystems = ssCount + 1;
}


size_t
SystemFrame::getSystemHeaderSizeInBytes( ) {
    return sizeof( SystemHeader );
}


size_t
SystemFrame::getSystemTotalSizeInBytes( ) const {
    return getSystemFrameStorageSizeInBytes( maxSubsystems_,
                                             maxMonitorPoints_,
                                             maxSamples_ );
}


size_t        
SystemFrame::getNextFreeSubsystemFrameHoldingLock( )
{
    const SystemHeader & frameData = getSystemFrameDataHoldingLock();

    // take care of simple cases first
    if ( maxSubsystems_ == 0 )
        return 0;

    if ( frameData.numSubsystems == maxSubsystems_ )
        return 0;

    if ( (frameData.numSubsystems == 0) && (maxSubsystems_ > 0) )
        return 0;

    SubsystemHeader & ssFrame = 
                getSubsystemFrameStructHoldingLock( frameData.numSubsystems - 1 );

    const size_t lastOffset = 
       frameDataSsDataOffsetArray_[ frameData.numSubsystems - 1 ];

    if ( lastOffset == BOGUS_SUBSYSTEM_DATA_OFFSET ) {
        const string msg = "Bogus lastOffset";
        
        programLogErrorIfPossible( msg );
        
        throw CARMA_ERROR( msg );
    }

    return static_cast< size_t >(lastOffset + SubsystemFrame::sizeFrame (ssFrame.maxMonitorPoints, ssFrame.maxSamples));
}
