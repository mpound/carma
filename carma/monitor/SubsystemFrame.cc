/* *
 * @file
 * SubsystemFrame.cc - Contains method definitions for subsystem frame
 * class, that manages storage for all monitor points and samples
 * associated with a monitor point subsystem.
 *
 * @author: N. S. Amarnath
 *
 * $Id: SubsystemFrame.cc,v 1.90 2011/12/22 22:20:58 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <iostream>
#include <sstream>
#include <algorithm>
#include <iomanip>

#include "carma/corba/corba.h"
#include "carma/dbms/TagIDAuthority.h"
#include "carma/monitor/SubsystemFrame.h"
#include "carma/monitor/monitorframe.h"
#include "carma/monitor/MonitorPointSet.h"
#include "carma/monitor/MonitorPointSample.h"
#include "carma/monitor/MonitorPointHeader.h"
#include "carma/util/checking.h"
#include "carma/util/compileTimeCheck.h"
#include "carma/util/memoryUtils.h"
#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Time.h"
#include "carma/util/Backtrace.h"
#include "carma/util/loggingUtils.h"
#include "carma/util/programExtras.h"

using namespace ::std;
using namespace ::log4cpp;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


const int carma::monitor::SubsystemFrame::MONITOR_POINT_ABSENT;


namespace {


const unsigned short FRAME_PUBLISHED_FLAG = 0x40;
const unsigned short FRAME_RECEIVED_FLAG = 0x20;
const unsigned short FRAME_REQUIRES_CONSOLIDATION_FLAG = 0x10;


void *
allocateStorage( const size_t bytes )
{
    const ScopedLogNdc ndc( "SubsystemFrame allocateStorage" );

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
            throw CARMA_ERROR( "SubsystemFrame allocateStorage malloc failed" );
    }

    return storage;
}


void
deallocateStorage( void * const storage )
{
    if ( storage != 0 )
        ::free( storage );
}


SubsystemHeader &
allocateFrameStorage( const unsigned short maxNumMonitorPoints,
                      const unsigned short maxNumSamples )
{
    size_t frameStorageBytes =
        SubsystemFrame::sizeFrame( maxNumMonitorPoints, maxNumSamples );

    if ( frameStorageBytes >= getDefaultVmMemoryCopyMinWinBytes() )
        frameStorageBytes = roundUpToVmPageMultiple( frameStorageBytes );

    void * const frameStorage = allocateStorage( frameStorageBytes );

    memset( frameStorage, 0, frameStorageBytes );

    return *(static_cast< SubsystemHeader * >( frameStorage ));
}


void
deallocateFrameStorage( SubsystemHeader & frame )
{
    deallocateStorage( &frame );
}

void
preallocateMonitorSampleValues( MonitorSampleValues & vals, 
                                SubsystemFrame & ssFrame )
{
    vals.charValues = CharSeq( 
        ssFrame.getHighwaterNumMonitorSamples( MONITOR_VALUE_TYPE_BYTE ) );
    vals.shortValues = ShortSeq( 
        ssFrame.getHighwaterNumMonitorSamples( MONITOR_VALUE_TYPE_SHORT ) );
    vals.longValues = LongSeq( 
        ssFrame.getHighwaterNumMonitorSamples( MONITOR_VALUE_TYPE_INTEGER ) );
    vals.boolValues = BoolSeq(
        ssFrame.getHighwaterNumMonitorSamples( MONITOR_VALUE_TYPE_BOOLEAN ) );
    vals.floatValues = FloatSeq(
        ssFrame.getHighwaterNumMonitorSamples( MONITOR_VALUE_TYPE_FLOAT ) );
    vals.doubleValues = DoubleSeq(
        ssFrame.getHighwaterNumMonitorSamples( MONITOR_VALUE_TYPE_DOUBLE ) );
    vals.complexValues = ComplexSeq(
        ssFrame.getHighwaterNumMonitorSamples( MONITOR_VALUE_TYPE_COMPLEX ) );
    vals.stringValues = StringSeq( 
        ssFrame.getHighwaterNumMonitorSamples( MONITOR_VALUE_TYPE_STRING ) );
    vals.serialNumberValues = SerialNumberSeq(
        ssFrame.getHighwaterNumMonitorSamples( MONITOR_VALUE_TYPE_SERIAL_NUMBER ) );
}

typedef ::std::map< MonitorValueType, unsigned int > TypeHighWaterMap;

void checkHighWaterMarks( const MonitorSampleValues & samples,
                          TypeHighWaterMap & highWaterMarks )
{
    const MonitorValueType maxMvt = MONITOR_VALUE_TYPE_SERIAL_NUMBER + 1;
    for ( MonitorValueType mvt = 0; mvt < maxMvt; ++mvt ) {

        unsigned int candidateHWM;
        switch ( mvt ) {
            case MONITOR_VALUE_TYPE_BYTE:
                candidateHWM = samples.charValues.length();
                break;
            case MONITOR_VALUE_TYPE_SHORT:
                candidateHWM = samples.shortValues.length();
                break;
            case MONITOR_VALUE_TYPE_INTEGER:
                candidateHWM = samples.longValues.length();
                break;
            case MONITOR_VALUE_TYPE_BOOLEAN:
                candidateHWM = samples.boolValues.length();
                break;
            case MONITOR_VALUE_TYPE_FLOAT:
                candidateHWM = samples.floatValues.length();
                break;
            case MONITOR_VALUE_TYPE_DOUBLE:
                candidateHWM = samples.doubleValues.length();
                break;
            case MONITOR_VALUE_TYPE_COMPLEX:
                candidateHWM = samples.complexValues.length();
                break;
            case MONITOR_VALUE_TYPE_STRING:
                candidateHWM = samples.stringValues.length();
                break;
            case MONITOR_VALUE_TYPE_SERIAL_NUMBER:
                candidateHWM = samples.serialNumberValues.length();
                break;
            default:
                throw CARMA_ERROR( "MonitorPointSet::checkHighWaterMarks - "
                    "Invalid value type." );
        }

        if ( candidateHWM > highWaterMarks[mvt] )
            highWaterMarks[mvt] = candidateHWM;

    } 
}// Check highwater marks
}  // namespace < anonymous >


SubsystemDataPointers::SubsystemDataPointers( SubsystemHeader & frame ) :
writableMonitorPointIndex( 0 ),
monitorPointIndex( 0 ),
writableMonitorHeaders( 0 ),
monitorHeaders( 0 ),
writableMonitorValues( 0 ),
monitorValues( 0 )
{
    void * const frameAddr = &frame;

    const int mpArrayCount = frame.maxMonitorPoints;

    const size_t headerBytes = sizeof( SubsystemHeader );


    const size_t indexArrayOffset = headerBytes;
    const size_t indexArrayBytes =
        mpArrayCount * sizeof( monitorPointIndex[ 0 ] );

    const size_t headersArrayOffset = indexArrayOffset + indexArrayBytes;
    const size_t headersArrayBytes =
        mpArrayCount * sizeof( monitorHeaders[ 0 ] );

    const size_t valuesArrayOffset = headersArrayOffset + headersArrayBytes;


    writableMonitorPointIndex = static_cast< int * >(
        byteOffsetPointer( frameAddr, indexArrayOffset ) );

    writableMonitorHeaders = static_cast< MonitorHeader * >(
        byteOffsetPointer( frameAddr, headersArrayOffset ) );

    writableMonitorValues = static_cast< MonitorSampleValue * >(
        byteOffsetPointer( frameAddr, valuesArrayOffset ) );

    monitorPointIndex = writableMonitorPointIndex;
    monitorHeaders = writableMonitorHeaders;
    monitorValues = writableMonitorValues;
}


void
SubsystemDataPointers::syncSubsystemDataPointersToNewFrameData(
    SubsystemHeader & frame )
{
    SubsystemDataPointers newDp( frame );

    if ( *this != newDp ) {
        swap( newDp );

        // Note that newDp is now holding was in *this and vice versa
        
        bool msgNeedsLogging = false;
        string msg;
    
        if ( (newDp.writableMonitorPointIndex != 0) &&
             (newDp.writableMonitorPointIndex != writableMonitorPointIndex) ) {
             msgNeedsLogging = true;
             if ( msg.empty() == false ) msg += ",";
             msg += "writableMonitorPointIndex";
        }

        if ( (newDp.monitorPointIndex != 0) &&
             (newDp.monitorPointIndex != monitorPointIndex) ) {
             msgNeedsLogging = true;
             if ( msg.empty() == false ) msg += ",";
             msg += "monitorPointIndex";
        }

        if ( (newDp.writableMonitorHeaders != 0) &&
             (newDp.writableMonitorHeaders != writableMonitorHeaders) ) {
             msgNeedsLogging = true;
             if ( msg.empty() == false ) msg += ",";
             msg += "writableMonitorHeaders";
        }

        if ( (newDp.monitorHeaders != 0) &&
             (newDp.monitorHeaders != monitorHeaders) ) {
             msgNeedsLogging = true;
             if ( msg.empty() == false ) msg += ",";
             msg += "monitorHeaders";
        }

        if ( (newDp.writableMonitorValues != 0) &&
             (newDp.writableMonitorValues != writableMonitorValues) ) {
             msgNeedsLogging = true;
             if ( msg.empty() == false ) msg += ",";
             msg += "writableMonitorValues";
        }

        if ( (newDp.monitorValues != 0) &&
             (newDp.monitorValues != monitorValues) ) {
             msgNeedsLogging = true;
             if ( msg.empty() == false ) msg += ",";
             msg += "monitorValues";
        }

        if ( msgNeedsLogging ) {
            msg += " moved";
            
            const ScopedLogNdc
                ndc( "SubsystemDataPointers"
                     "::syncSubsystemDataPointersToNewFrameData" );
                     
            programLogErrorIfPossible( msg );
        }
    }
}


SubsystemFrame::SubsystemFrame( SubsystemHeader &       frame,
                                SubsystemDataPointers & dataPointers,
                                const bool              embedded ) :
frame_( frame ),
dataPointers_( dataPointers ),
shouldDeallocateStorage_( false ),
embeddedInAFullSystem_( embedded ),
set_( 0 )
{
    const ScopedLogNdc ndc( "SubsystemFrame::SubsystemFrame managed" );

#if __WORDSIZE == 64
    compileTimeCheck< sizeof( SubsystemHeader ) == 96 >( );
#else
    compileTimeCheck< sizeof( SubsystemHeader ) == 92 >( );
#endif

    if ( getNumMonitorPoints() == 0 ) {
        const int maxNumMonitorPoints = getMaxNumMonitorPoints();

        for ( int i = 0; i < maxNumMonitorPoints; ++i )
            dataPointers_.writableMonitorPointIndex[ i ] = -1;
    }
}


SubsystemFrame::SubsystemFrame( const ushort         subsysId,
                                const unsigned short maxNumMonitorPoints,
                                const unsigned short maxNumSamples,
                                const bool           leakStorage ) :
frame_( allocateFrameStorage( maxNumMonitorPoints, maxNumSamples ) ),
dataPointers_( *(new SubsystemDataPointers) ),
shouldDeallocateStorage_( (leakStorage == false) ),
embeddedInAFullSystem_( false ),
set_( 0 )
{
    const ScopedLogNdc ndc( "SubsystemFrame::SubsystemFrame standalone" );

#if __WORDSIZE == 64
    compileTimeCheck< sizeof( SubsystemHeader ) == 96 >( );
#else
    compileTimeCheck< sizeof( SubsystemHeader ) == 92 >( );
#endif

    setSubsystemID( subsysId );

    setFrameSize( frame_,
                  dataPointers_,
                  maxNumMonitorPoints,
                  maxNumSamples );

    setFrameCount( 0 );
    setPublishTime( 0.0 );
    setReceiveTime( 0.0 );

    for ( int  i = 0;  i < maxNumMonitorPoints; ++i )
        dataPointers_.writableMonitorPointIndex[ i ] = -1;
}


SubsystemFrame::~SubsystemFrame( )
try {
    const ScopedLogNdc ndc( "SubsystemFrame::~SubsystemFrame" );

    if ( embeddedInAFullSystem_ == false ) {
        if ( shouldDeallocateStorage_ ) {
            deallocateFrameStorage( frame_ );

            delete (&dataPointers_);
        } else {
            programLogWarnIfPossible(
                "Leaking the frame storage and data pointers" );
        }
    }
} catch ( ... ) {
    // Just stifle any exceptions

    return;
}


long
SubsystemFrame::getFrameCount ()  const
{
   return frame_.frameCount;
}


long
SubsystemFrame::setFrameCount (const long frameCount)
{
    return frame_.frameCount = frameCount;
}


bool
SubsystemFrame::isCurrentFrame () const
{
    return mjdIsCurrentFrame( getScriberWriteTime(), 0 );
}


namespace {

const double kMaxCurrentFrameMjdDelta = 1.0 / Time::SECONDS_PER_DAY;

}  // namespace < anonymous >


bool
SubsystemFrame::mjdIsCurrentFrame( const double   mjd,
                                   double * const mjdDelta )
{
    // we assume that the scriber wrote data before this time instant
    const double localMjdDelta = Time::MJD() - mjd;

    if ( localMjdDelta < 0 ) {
        programLogErrorIfPossible(
            "SubsystemFrame::mjdIsCurrentFrame() mjd is in the future" );
    }

    if ( mjdDelta != 0 )
        *mjdDelta = localMjdDelta;

    return (localMjdDelta < kMaxCurrentFrameMjdDelta);
}


uchar
SubsystemFrame::getStatusFlags() const
{
    return frame_.statusFlags;
}

void
SubsystemFrame::setStatusFlags (uchar status)
{
    frame_.statusFlags = status;
}


long
SubsystemFrame::getMaxNumMonitorPoints () const
{
    return frame_.maxMonitorPoints;
}

long
SubsystemFrame::getMaxNumSamples () const
{
    return frame_.maxSamples;
}


int
SubsystemFrame::getIndex( const tagIDType tagId ) const
{
    if ( dbms::TagIDAuthority::getSubsystemID( tagId ) != getSubsystemID() )
        return MONITOR_POINT_ABSENT;

    return this->findMonitorPoint (tagId);
}


MonitorHeader &
SubsystemFrame::getWritableMonitorHeaderRefByIndex( const int index ) const
{
    return dataPointers_.writableMonitorHeaders[ index ];
}


MonitorPointHeader
SubsystemFrame::getHeaderByIndex( const int index ) const
{
    return MonitorPointHeader( getWritableMonitorHeaderRefByIndex( index ),
                               getSamples(),
                               index,
                               set_ );
}


MonitorPointHeader
SubsystemFrame::getHeaderByTagID( const tagIDType tagId ) const
{
    return getHeaderByIndex (this->getIndex (tagId));
}


tagIDType
SubsystemFrame::getTagID( const int index ) const
{
    return getMonitorHeaderRefByIndex( index ).getTagID();
}


bool
SubsystemFrame::isValid (const tagIDType tagId) const
{
    return ((this->getIndex (tagId)) >= 0);
}


void
SubsystemFrame::setSubsystemID( const ushort subsysId )
{
    frame_.subsystemID = subsysId;
}


int
SubsystemFrame::getNumActualSamples( ) const
{
    return frame_.numActualSamples;
}


long
SubsystemFrame::getNumMonitorPoints (MonitorValueType type) const
{
    long        nPoints = 0;

    for (int  i = 0;  i <  this->getNumMonitorPoints();  i++)  {
        MonitorValueType typeOfMP = this->getValueType (i);
        if (typeOfMP == type)  {
            nPoints++;
        }
    }

    return nPoints;
}


int
SubsystemFrame::findMonitorPoint( const tagIDType tagId ) const
{
    const long mpCount = this->getNumMonitorPoints();

    if ( mpCount == 0 )
        return MONITOR_POINT_ABSENT;

    // if a quickie search works, go for it
    // if monitor points are allocated sequentially, in order of
    // pointId, then monitor headers are already nicely ordered.
    // The following will work if
    // 1) all pointId's are in ascending sequence
    // 2) pointId's begin with 1, and
    // 3) there are no"gaps" in the pointId sequence.

    const ushort pointId = dbms::TagIDAuthority::getPointID( tagId );

    CARMA_CHECK( pointId > 0 );

    if ( mpCount >= pointId ) {
        //
        // TagID in the struct should really be unsigned! - mwp
        //
        if ( dataPointers_.monitorHeaders[pointId-1].tagID == tagId )
            return (pointId-1);

        // if monitor points are not allocated sequentially, in order of
        // pointId, then monitor indices should be nicely ordered.
        // provided there are no "holes" in assignment of pointId's
        const int index = dataPointers_.monitorPointIndex[pointId-1];
        //
        // TagID in the struct should really be unsigned! - mwp
        //
        if ( dataPointers_.monitorHeaders[index].tagID == tagId )
            return index;
    }

    // all other cases fall through to the default binary search
    int start = 0;
    int end = (this->getNumMonitorPoints() - 1);

    {
        const int startIndex = dataPointers_.monitorPointIndex[start];
        const int endIndex = dataPointers_.monitorPointIndex[end];
        const tagIDType minTagId
            = dataPointers_.monitorHeaders[startIndex].tagID;
        const tagIDType maxTagId
            = dataPointers_.monitorHeaders[endIndex].tagID;
        if ((tagId < minTagId)  ||  (tagId > maxTagId))
            return MONITOR_POINT_ABSENT;
    }
    
    // Binary search for as long as we have at least 4 elements
    while ( (end - start) > 2 ) {
        const int middle = ((start + end) >> 1);
        const int index = dataPointers_.monitorPointIndex[middle];
        const tagIDType indexTagId = dataPointers_.monitorHeaders[index].tagID;

        if ( indexTagId < tagId ) {
            start = middle;
            continue;
        }
        
        if ( indexTagId > tagId ) {
            end = middle;
            continue;
        }

        // The tag id matches. Now let's make sure we find the lowest one
        // out of all matches just in case we have duplicates in the list.
        // Don't know if we really need to worry about the
        // duplicates in the list case -TWC 15 Oct 2007

        // We know end >= (start + 3) so then middle >= (start + 1) and hence
        // we know (middle - 1) should be valid to look at
        const int prevIndex = dataPointers_.monitorPointIndex[middle - 1];
        if ( dataPointers_.monitorHeaders[prevIndex].tagID != tagId )
            return index;
            
        end = middle - 1;
    }

    // Linear search when we are down to 3 or fewer elements
    for ( int i = start; i <= end; ++i ) {
        const int index = dataPointers_.monitorPointIndex[i];

        if ( dataPointers_.monitorHeaders[index].tagID == tagId )
            return index;
    }

    return MONITOR_POINT_ABSENT;
}


bool
SubsystemFrame::validSampleValue (const tagIDType tagId,
                                  const MonitorPointSample& sample) const
{
    CARMA_CHECK (this->isValid(tagId));

    if (sample.getSampleNumber()
               > this->getHeaderByTagID(tagId).getNumSamplesPerCycle())
       return false;
    // the statement below is a truism - must find more independent way to
    // verify value type
    // TBD - may check to see if flag settings are valid
    // the statement to which the above statement refers no longer exists !

    return true;
}


ushort
SubsystemFrame::allocateSamples (int headerOffset, ushort sampleOffset,
                                            const ushort nSamplesPerCycle)
{
    if (nSamplesPerCycle == 0)
        return 0;

    int         sampleIndex = 1;
    ushort      actualNumSamples = nSamplesPerCycle;

    actualNumSamples++; // add average to # of samples to allocate
    if (nSamplesPerCycle > 1)  {
        sampleIndex = 0;
    }

    // Allocate one more sample than nSamplesPerCycle for average value
    // in the case when nSamplesPerCycle > 1
    for (ushort offset = sampleOffset;  sampleIndex < actualNumSamples;
            sampleIndex++, offset++)  {
        MonitorPointSample    sample (dataPointers_.writableMonitorValues[offset]);
        sample.allocate (headerOffset, sampleIndex);
    }
    if (nSamplesPerCycle == 1)  {
        ++(frame_.numSingleSamplePoints);
        frame_.numSamples += nSamplesPerCycle;
        frame_.numActualSamples += nSamplesPerCycle;
    }  else  {
        frame_.numSamples += actualNumSamples;
        frame_.numActualSamples += actualNumSamples;
    }

    return sampleOffset;
}


namespace {


class MonitorHeaderTagIdLess {
    public:
        MonitorHeaderTagIdLess( const MonitorHeader * headers,
                                long                  numHeaders );

        bool operator()( int lhsIndex, int rhsIndex ) const;

    private:
        const MonitorHeader * const headers_;
        const long                  numHeaders_;
};


inline
MonitorHeaderTagIdLess::MonitorHeaderTagIdLess(
    const MonitorHeader * const headers,
    const long                  numHeaders ) :
headers_( headers ),
numHeaders_( numHeaders )
{
}


inline bool
MonitorHeaderTagIdLess::operator()( const int lhsIndex,
                                    const int rhsIndex ) const
{
    // Arbitrary rule - bogus data sorts last and all bogus data sorts equal
    if ( (lhsIndex < 0) || (lhsIndex >= numHeaders_) )
        return false;

    if ( (rhsIndex < 0) || (rhsIndex >= numHeaders_) )
        return true;

    return (headers_[ lhsIndex ].tagID < headers_[ rhsIndex ].tagID);
}


string
getLogNameForSubysId( const ushort subsysId )
{
    if ( (subsysId < 1) ||
         (static_cast< int >( subsysId ) > dbms::TagIDAuthority::getSubsystemCount()) )
        return "< BAD >";

    return dbms::TagIDAuthority::getSubsystemName( subsysId );
}


string
getLogStringForTagId( const tagIDType tagId )
{
    const ushort subsysId = dbms::TagIDAuthority::getSubsystemID( tagId );
    const ushort pointId = dbms::TagIDAuthority::getPointID( tagId );
    const string subsysName = getLogNameForSubysId( subsysId );

    string nameForTagId;

    try {
        nameForTagId = dbms::TagIDAuthority::getAuthority().lookupName( tagId );
    } catch ( ... ) {
        nameForTagId = "< unknown >";
    }

    ostringstream oss;

    oss << tagId << " (" << nameForTagId << ")"
        << " {" << subsysId << "(" << subsysName << ")/" << pointId << "}";

    return oss.str();
}


} // namespace < anonymous >


void
SubsystemFrame::allocateMonitorPoints( const AllocMpInfo * infos,
                                       const size_t        infosCount,
                                       const bool markMpsAsModified )
{
    const ScopedLogNdc ndc( "SubsystemFrame::allocateMonitorPoints" );

    int numNewActualSamples = 0;
    for ( size_t i = 0; i < infosCount; ++i ) {
        const tagIDType tagId = infos[ i ].tagID;

        if ( dbms::TagIDAuthority::getSubsystemID( tagId ) != frame_.subsystemID ) {
            ostringstream oss;

            oss << "frame_ and tagId disagree on the subsystem ID."
                << " frame_ says " << frame_.subsystemID << " ("
                << getLogNameForSubysId( frame_.subsystemID )
                << ") and tagId is " << getLogStringForTagId( tagId );

            throw CARMA_ERROR( oss.str() );
        }

        const ushort nSamplesPerCycle = infos[ i ].nSamplesPerCycle;

        if ( nSamplesPerCycle != 0 ) {
            if ( nSamplesPerCycle == 1 )
                ++numNewActualSamples; // Just the one sample
            else
                numNewActualSamples += (nSamplesPerCycle + 1); // Samples + avrg
        }
    }

    const int maxNumMonitorPoints = getMaxNumMonitorPoints();
    const int oldNumMps = frame_.numMonitorPoints;

    if ( (oldNumMps + static_cast< int >( infosCount )) > maxNumMonitorPoints ) {
        ostringstream oss;

        oss << "Cannot allocate space for " << infosCount
            << " monitor points: "
            << ", max # of monitor points = " << maxNumMonitorPoints
            << ", present # of monitor points " << oldNumMps;

        throw CARMA_ERROR( oss.str() );
    }

    {
        const int maxSamplesWithAverages =
            maxNumSamplesIncludingAverages( maxNumMonitorPoints,
                                            getMaxNumSamples() );

        const int oldNumActualSamples = frame_.numActualSamples;

        if ( (oldNumActualSamples + numNewActualSamples) > maxSamplesWithAverages ) {
            ostringstream oss;

            oss << "Cannot allocate space for " << numNewActualSamples
                << " new actual samples: "
                << ", max # of samples with averages = "
                << maxSamplesWithAverages
                << ", present # of actual samples " << oldNumActualSamples;

            throw CARMA_ERROR( oss.str() );
        }
    }

    // First, just add them all to the mix
    int * const indices = dataPointers_.writableMonitorPointIndex;

    for ( size_t i = 0; i < infosCount; ++i ) {
        const tagIDType tagId = infos[ i ].tagID;
        const MonitorValueType mvt = infos[ i ].valueType;
        const ushort nSamplesPerCycle = infos[ i ].nSamplesPerCycle;

        const int mpCount = frame_.numMonitorPoints;

        dataPointers_.writableMonitorHeaders[mpCount].tagID = tagId;
        dataPointers_.writableMonitorHeaders[mpCount].sampleOffset =
            allocateSamples(mpCount, getNumAllocatedSamples(), nSamplesPerCycle);
        dataPointers_.writableMonitorHeaders[mpCount].nSamples = nSamplesPerCycle;
        dataPointers_.writableMonitorHeaders[mpCount].setValueType( mvt );

        indices[ mpCount ] = mpCount;

        ++(frame_.numMonitorPoints);
    }

    const int newNumMps = frame_.numMonitorPoints;

    // Second, sort the whole index array by tag ID
    stable_sort( indices,
                 (indices + newNumMps),
                 MonitorHeaderTagIdLess( dataPointers_.monitorHeaders,
                                         newNumMps ) );

    // Finally, double check that we are ordered and have no duplicate tag IDs
    if ( newNumMps != 0 ) {
        const MonitorHeader * const headers = dataPointers_.monitorHeaders;

        bool firstOne = true;
        tagIDType prevTagId = 0;

        const int * i = indices;
        const int * const iEnd = indices + newNumMps;

        for ( ; i != iEnd; ++i ) {
            const int index = *i;

            if ( (index < 0) || (index >= newNumMps) ) {
                const string msg =
                    "Bad index in SubsystemFrame::allocateMonitorPoints."
                    " Tell Tom about this";

                programLogErrorIfPossible( msg );

                throw CARMA_ERROR( msg );
            }

            const tagIDType tagId = headers[ index ].tagID;

            if ( firstOne )
                firstOne = false;
            else {
                if ( prevTagId == tagId ) {
                    const string msg =
                        "Tag ID " + getLogStringForTagId( tagId ) +
                        " appears multiple times. Tell Tom about this";

                    programLogErrorIfPossible( msg );

                    throw CARMA_ERROR( msg );
                }

                if ( prevTagId > tagId ) {
                    string msg =
                        "Tag IDs " + getLogStringForTagId( prevTagId ) +
                        " and " + getLogStringForTagId( tagId ) +
                        " are misordered. Tell Tom about this";

                    programLogErrorIfPossible( msg );

                    throw CARMA_ERROR( msg );
                }
            }

            prevTagId = tagId;
        }
    }

    if ( set_ != 0 && markMpsAsModified ) {
        for ( int i = oldNumMps; i < newNumMps; ++i )
            set_->markMpAtIndexModified( i );
    }
}


void
SubsystemFrame::clearSamples( )
{
    const long mpCount = getNumMonitorPoints();

    for ( long i = 0; i < mpCount; ++i ) {
        const int sampCount =
            getMonitorHeaderRefByIndex( i ).getNumSamplesPerCycle();

        for ( int  j = 0;  j < sampCount; ++j ) {
            MonitorPointSample sample = getSampleValue( i, (j + 1) );

            sample.clear();
        }
    }
}


void
SubsystemFrame::setNumSamplesPerCycle (int index, const ushort newSamples)
{
    // check to see if index is legal
    if (index == MONITOR_POINT_ABSENT)
        return;

    // existing sample rate
    MonitorPointHeader header = this->getHeaderByIndex(index);
    const ushort curSamples = header.getNumSamplesPerCycle();

    // if theres no change in sampling frequency, post-conditions are satisfied
    if (newSamples == curSamples)
        return;

    int numMonitorSamples = this->getNumAllocatedSamples();
    const int maximumSamples = SubsystemFrame::maxNumSamplesIncludingAverages(
                                    getMaxNumMonitorPoints(),
                                    getMaxNumSamples());

    // The frame needs to be consolidated (re-packed) if we don't have any
    // more temporary working space at the end of the frame
    if ((numMonitorSamples + newSamples) > maximumSamples) {
        this->consolidateSamples();
        numMonitorSamples = this->getNumAllocatedSamples();
    }

    // Check to make sure the frame has enough space for the final amount of
    // samples requested. Only the final number matters, we shuffle the data
    // to make plenty of contiguous space.
    if ((numMonitorSamples - curSamples + newSamples) > maximumSamples) {
        const tagIDType tagId = this->getTagID(index);
        std::ostringstream oss;

        oss << "Cannot allocate space:"
            << " Max # of samples = " << this->getMaxNumSamples() << "."
            << " Existing # of allocated samples = " << numMonitorSamples << "."
            << " Current # of samples used for tagid = " << curSamples << "."
            << " New # of samples requested for tagid = " << newSamples << "."
            << " TagId = " << tagId
            << "{"
            << dbms::TagIDAuthority::getSubsystemID(tagId)
            << "/"
            << dbms::TagIDAuthority::getPointID(tagId)
            << "}";

        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    // Change the number of samples
    //
    // We handle the worst case here: when a frame is almost full and we
    // wish to add another sample, which will make it completely full.
    //
    // There is not enough contiguous free space to allocate a block for
    // the new  samples, so we have to do the shuffling described below.
    //
    // - copy data out of frame to temporary storage
    // - free data in the frame
    // - consolidate (re-pack) the frame
    // - allocate space for new data in the frame
    // - copy the data back into the frame
    //
    // This case is hit when a frame is almost full, and we want to
    // increase it to completely full. There is not enough contiguous
    // space to allocate a block for the new samples, so we have to
    // do the shuffling described above.

    // temporary storage
    const int copySamples = std::min(curSamples, newSamples);
    MonitorSampleValue storage[copySamples];

    // copy to temporary storage
    {
        const int curSampleOffset = header.getSampleOffset();
        const int srcOffset = (curSamples > 1) ? (curSampleOffset + 1) : curSampleOffset;

        void *dst = storage;
        const void *src = &dataPointers_.monitorValues[srcOffset];
        const size_t len = copySamples * sizeof(MonitorSampleValue);

        memcpy(dst, src, len);
    }

    // Free data in the frame (update internal bookkeeping information)
    // Consolidate (re-pack) the frame
    {
        header.clearSamples(true);

        // header.clearSamples() doesn't update any internal state,
        // so we have to do it ourselves. So much for an object model...
        header.setSamplesPerCycle(0);
        header.setSampleOffset(0);

        // Un-intuitive internal bookkeeping
        //
        // This code is required for consolidateSamples() to work correctly
        {
            const ushort prevSamples = (curSamples > 1) ? (curSamples + 1) : curSamples;
            frame_.numActualSamples -= prevSamples;
            SetFlag (frame_.statusFlags, FRAME_REQUIRES_CONSOLIDATION_FLAG);
        }

        // re-pack the frame
        this->consolidateSamples();
        numMonitorSamples = this->getNumAllocatedSamples();
    }

    //  copy back
    {
        const int newSampleOffset = this->allocateSamples(index, numMonitorSamples, newSamples);
        const int dstOffset = (newSamples > 1) ? (newSampleOffset + 1) : newSampleOffset;

        void *dst = &dataPointers_.writableMonitorValues[dstOffset];
        const void *src = storage;
        const size_t len = copySamples * sizeof(MonitorSampleValue);

        memcpy(dst, src, len);

        // update header information
        header.setSampleOffset(newSampleOffset);
        header.setSamplesPerCycle(newSamples);
    }

    if (curSamples == 1)
        frame_.numSingleSamplePoints--;

    SetFlag (frame_.statusFlags, FRAME_REQUIRES_CONSOLIDATION_FLAG);
}


void
SubsystemFrame::writeSampleValue (const int index, const MonitorSampleValue& value, ushort iSample)
{
    const ushort numSamplesPerCycle =
        getMonitorHeaderRefByIndex(index).getNumSamplesPerCycle();

    if (iSample > numSamplesPerCycle)
        return;

    const MonitorHeader & header = dataPointers_.monitorHeaders[index];

    MonitorSampleValue & sample =
        dataPointers_.writableMonitorValues[header.sampleOffset +iSample];

    sample = value;
}


long
SubsystemFrame::getNumMonitorSamples (MonitorValueType type) const 
{
    long nSamples = 0;

    for (int  i = 0;  i <  this->getNumMonitorPoints();  i++)  {
        MonitorValueType typeOfMP = this->getValueType (i);
        if (typeOfMP == type)  {
            long deltaSamples = this->getNumSamplesPerCycle(i);
            if (deltaSamples > 1) deltaSamples++;
            nSamples += deltaSamples;
        }
    }

    return nSamples;
}

void
SubsystemFrame::consolidateSamples()
{
    // Move samples over to a temporary copy of SubsystemHeader.
    // While copying sample sets into the temporary subsystem frame
    // structure, eliminate holes found while traversing the samples.
    // Copy contents of temporary subsystem frame back into the
    // "authoritative" version.
    // Holes exist in the set of allocated samples if
    // numAllocatedSamples > numActualSamples.
    // A hole exists between two sets (list1 and list2) of allocated samples if
    //      offset-first-sample(list1)+totalNumSamples(list1)
    //                                            < offset-first-sample(list2)
    // totalNumSamples includes the average sample, if applicable.
    // offset-first-sample(set) represents the offset of the first sample in
    // a set - its basically the index in the sample array.
    // Move sample sets back to cover holes, and update the corresponding
    // headers.
    CARMA_CHECK (this->getNumAllocatedSamples() >= this->getNumActualSamples());
    if (this->getNumAllocatedSamples() == this->getNumActualSamples())  {
        UnsetFlag (frame_.statusFlags, FRAME_REQUIRES_CONSOLIDATION_FLAG);
        return;
    }

    const long maxActualSamples =
        maxNumSamplesIncludingAverages( frame_.maxMonitorPoints,
                                        frame_.maxSamples );

    // copy samples if needed
    MonitorSampleValue * copyStorage = 0;
    int firstCopiedSampleOffset = 0;
    int copiedCount = 0;

    int nextNewSampleOffset = 0;
    const int numMps = getNumMonitorPoints();

    for ( int i = 0; i < numMps; ++i ) {
        MonitorHeader & header = dataPointers_.writableMonitorHeaders[ i ];

        const uchar numActualSamples =
            ((header.nSamples <= 1) ?
                (header.nSamples) :
                (header.nSamples + 1));

        if ( header.sampleOffset != nextNewSampleOffset ) {
            if ( numActualSamples > 0 ) {
                if ( copyStorage == 0 ) {
                    firstCopiedSampleOffset = nextNewSampleOffset;

                    const int maxCopyCount =
                        maxActualSamples - firstCopiedSampleOffset;

                    copyStorage = new MonitorSampleValue[ maxCopyCount ];
                }

                const int copyStorageOffset =
                    nextNewSampleOffset - firstCopiedSampleOffset;

                if ( copyStorageOffset != copiedCount ) {
                    // We must have skipped over a range of samples that
                    // did not shift but now we need to go back and copy them

                    const int unshiftedSamples =
                        copyStorageOffset - copiedCount;

                    MonitorSampleValue * const copyBase =
                        copyStorage + copiedCount;

                    const MonitorSampleValue * const oldBase =
                        dataPointers_.monitorValues +
                        (firstCopiedSampleOffset + copiedCount);

                    for ( int j = 0; j < unshiftedSamples; ++j ) {
                        copyBase[ j ] = oldBase[ j ];
                    }
                }

                {
                    MonitorSampleValue * const copyBase =
                        copyStorage + copyStorageOffset;

                    const MonitorSampleValue * const oldBase =
                        dataPointers_.monitorValues + header.sampleOffset;

                    for ( uchar j = 0; j < numActualSamples; ++j )
                        copyBase[ j ] = oldBase[ j ];
                }

                copiedCount = copyStorageOffset + numActualSamples;
            }

            header.sampleOffset = nextNewSampleOffset;
        }

        nextNewSampleOffset += numActualSamples;
    }

    if ( copyStorage != 0 ) {
        // copy from temp location back to original frame

        MonitorSampleValue * const copyDest =
            dataPointers_.writableMonitorValues + firstCopiedSampleOffset;

        memcpy( copyDest,
                copyStorage,
                (copiedCount * sizeof( copyDest[ 0 ] )) );
    }

    if ( nextNewSampleOffset < maxActualSamples ) {
        // Is this really necessary? - Tom 1 July 2006

        MonitorSampleValue * const clearDest =
            dataPointers_.writableMonitorValues + nextNewSampleOffset;

        const int clearCount = maxActualSamples - nextNewSampleOffset;

        memset( clearDest, 0, (clearCount * sizeof( clearDest[ 0 ] )) );
    }

    // set number of allocated samples
    frame_.numSamples = nextNewSampleOffset;

    UnsetFlag (frame_.statusFlags, FRAME_REQUIRES_CONSOLIDATION_FLAG);
    CARMA_CHECK (this->getNumAllocatedSamples() == this->getNumActualSamples());

    delete [] copyStorage;
}


size_t
SubsystemFrame::sizeFrame (long maxMonitorPoints, long maxSamples)
{
    ushort remainder;
    const  size_t wordsize = sizeof(int);  // compiler definition - see <bits/typesizes.h>

    long numSamples =
        SubsystemFrame::maxNumSamplesIncludingAverages
                                       (maxMonitorPoints, maxSamples);
    size_t frameSize = (sizeof (SubsystemHeader)
                     + maxMonitorPoints*(sizeof (MonitorHeader) + sizeof (int))
                     + numSamples*sizeof(MonitorSampleValue));

    // round up to wordsize to ensure that we are aligned to
    // a word boundary
    if ((remainder = frameSize % wordsize) > 0)
        return (frameSize + (wordsize - remainder));
    else
       return frameSize;
}


void
SubsystemFrame::syncSubsystemToNewFrameData(
    SubsystemHeader &       frame,
    SubsystemDataPointers & dataPointers )
{
    dataPointers.syncSubsystemDataPointersToNewFrameData( frame );
}


void
SubsystemFrame::syncSubsystemToNewFrameData( )
{
    syncSubsystemToNewFrameData( frame_, dataPointers_ );
}


void
SubsystemFrame::setFrameSize( SubsystemHeader &       frame,
                              SubsystemDataPointers & dataPointers,
                              const long              maxNumMonitorPoints,
                              const long              maxNumSamples )
{
    frame.maxMonitorPoints = maxNumMonitorPoints;
    frame.maxSamples = maxNumSamples;

    syncSubsystemToNewFrameData( frame, dataPointers );

    frame.numMonitorPoints = 0;
    frame.numSamples = 0;
    frame.numActualSamples = 0;
    frame.numSingleSamplePoints = 0;

    //char* endPtr = (char *)((char *)(frame.monitorValues)) + numSamples*sizeof(MonitorSampleValue);
    CARMA_CHECK (((size_t)(endPtr - ((char *)&frame))) <= SubsystemFrame::sizeFrame (maxNumMonitorPoints, maxNumSamples));
}


long
SubsystemFrame::computeCurrentFrameTime()
{
    return (Time::computeCurrentFrame() - 1);
}


long
SubsystemFrame::getNumMonitorSamples () const
{
    return this->getNumAllocatedSamples();
}



long
SubsystemFrame::getNumSingleSamplePoints () const
{
    return (frame_.numSingleSamplePoints);
}


void
SubsystemFrame::setNumSingleSamplePoints (const long numSingleSamples)
{
    frame_.numSingleSamplePoints = numSingleSamples;
}


MonitorPointSample
SubsystemFrame::getSampleValue (const tagIDType tagId, ushort iSample) const
{
    return  MonitorPointSample (dataPointers_.writableMonitorValues[
                    this->getHeaderByTagID(tagId).getSampleOffset() + iSample
                                                                ]
                               );
}


MonitorPointSample
SubsystemFrame::getSampleValue (const int index, ushort iSample) const
{
    return  MonitorPointSample (dataPointers_.writableMonitorValues[
                    getMonitorHeaderRefByIndex(index).getSampleOffset() + iSample
                                                                ]
                               );
}


const MonitorPointSample
SubsystemFrame::getSampleAverage (const tagIDType tagId) const
{
    return  MonitorPointSample (dataPointers_.writableMonitorValues[
                    this->getHeaderByTagID(tagId).getSampleOffset()
                                                                ]
                               );
}


const MonitorPointSample
SubsystemFrame::getSampleAverage (const int index) const
{
    return  MonitorPointSample (
                  dataPointers_.writableMonitorValues[
                   getMonitorHeaderRefByIndex(index).getSampleOffset()
                                                  ]
                               );
}

MonitorValueType
SubsystemFrame::getValueType (const tagIDType tagId) const
{
    return this->getHeaderByTagID(tagId).getValueType();
}


MonitorValueType
SubsystemFrame::getValueType( const int index ) const
{
    return getMonitorHeaderRefByIndex( index ).getValueType();
}


void
SubsystemFrame::setLastWriterDelay (double time)
{
    frame_.lastWriterDelay = time ;
}



double
SubsystemFrame::getLastWriterDelay () const
{
    return frame_.lastWriterDelay;
}


void
SubsystemFrame::setLastWriteTime( const double mjdTimestamp )
{
    frame_.lastWriteTime = mjdTimestamp;
}


void
SubsystemFrame::setLastWriteTime( )
{
    setLastWriteTime( Time::MJD() );
}


double
SubsystemFrame::getLastWriteTime () const
{
    return frame_.lastWriteTime;
}


void
SubsystemFrame::setScriberWriteDelay (double writeDelay)
{
    frame_.scriberWriteDelay = writeDelay ;
}


double
SubsystemFrame::getScriberWriteDelay () const
{
    return frame_.scriberWriteDelay;
}


void
SubsystemFrame::setScriberWriteTime( const double mjdTimestamp )
{
    frame_.scriberWriteTime = mjdTimestamp;
}


void
SubsystemFrame::setScriberWriteTime( )
{
    setScriberWriteTime( Time::MJD() );
}


double
SubsystemFrame::getScriberWriteTime () const
{
    return frame_.scriberWriteTime;
}


void
SubsystemFrame::setPublishTime( const double mjdTimestamp )
{
    frame_.publishTime = mjdTimestamp;
}


void
SubsystemFrame::setPublishTime( )
{
    setPublishTime( Time::MJD() );
}


double
SubsystemFrame::getPublishTime () const
{
    return frame_.publishTime;
}


void
SubsystemFrame::setReceiveTime( const double mjdTimestamp )
{
    frame_.receiveTime = mjdTimestamp;
}


void
SubsystemFrame::setReceiveTime( )
{
    setReceiveTime( Time::MJD() );
}


double
SubsystemFrame::getReceiveTime() const
{
    return frame_.receiveTime;
}



void
SubsystemFrame::clearAllTimes ()
{
    const double resetMjdTimestamp = 0.0;

    setLastWriteTime( resetMjdTimestamp );
    setScriberWriteTime( resetMjdTimestamp );
    setPublishTime( resetMjdTimestamp );
    setReceiveTime( resetMjdTimestamp );
}


bool
SubsystemFrame::isPublished ()  const
{
    return (frame_.statusFlags & FRAME_PUBLISHED_FLAG);
}

void
SubsystemFrame::setPublished ()
{
    frame_.statusFlags |= FRAME_PUBLISHED_FLAG;
}


void
SubsystemFrame::clearPublished ()
{
    frame_.statusFlags &= ~FRAME_PUBLISHED_FLAG;
}

bool
SubsystemFrame::received () const
{
    return (frame_.statusFlags & FRAME_RECEIVED_FLAG);
}

void
SubsystemFrame::setReceived ()
{
    frame_.statusFlags |= FRAME_RECEIVED_FLAG;
}


void
SubsystemFrame::clearReceived ()
{
    frame_.statusFlags &= ~FRAME_RECEIVED_FLAG;
}


void
SubsystemFrame::markFramePublished()
{
    this->setPublishTime();
    this->setPublished();
}



void
SubsystemFrame::markFrameReceived()
{
    this->setReceiveTime();
    this->setReceived();
}



long
SubsystemFrame::countNumSingleSamplePoints () const
{
    long    numSingleSamplePoints = 0;

    for (int i = 0;  i < frame_.numMonitorPoints;  i++)  {
        if (dataPointers_.monitorHeaders[i].nSamples == 1)  {
            numSingleSamplePoints++;
        }
    }

    return numSingleSamplePoints;
}

unsigned int 
SubsystemFrame::getHighwaterNumMonitorSamples( const MonitorValueType type )
{
    return highwaterMonitorSamples_[ type ];
}

void
SubsystemFrame::writeToTransport( TransportSubsystemFrame & transportFrame )
{
    const ScopedLogNdc ndc( "SubsystemFrame::writeToTransport" );

    const ushort subsysId = getSubsystemID();
    const int numAllocedSamples = this->getNumAllocatedSamples();

    transportFrame.numMonitorPoints = this->getNumMonitorPoints();
    transportFrame.subsystemID = subsysId;
    transportFrame.statusFlags = this->getStatusFlags();
    transportFrame.maxMonitorPoints = this->getMaxNumMonitorPoints();
    transportFrame.maxSamples = this->getMaxNumSamples();
    transportFrame.numSingleSamplePoints = this->getNumSingleSamplePoints();
    transportFrame.frameCount = this->getFrameCount();
    transportFrame.lastWriterDelay = this->getLastWriterDelay();
    transportFrame.lastWriteTime = this->getLastWriteTime();
    transportFrame.scriberWriteDelay = this->getScriberWriteDelay();
    transportFrame.scriberWriteTime = this->getScriberWriteTime();
    transportFrame.publishTime = this->getPublishTime();
    transportFrame.receiveTime = this->getReceiveTime();

    // transport only allocated samples, transporting averages
    //
    //int  numMultiSamplePoints =
    //       this->getNumMonitorPoints() - this->getNumSingleSamplePoints();
    const int dataSeqLength = getNumActualSamples();

    transportFrame.monitorValues.dataType.length( dataSeqLength );
    transportFrame.monitorValues.sequenceIdx.length( dataSeqLength );
    transportFrame.monitorValues.pointID.length( dataSeqLength );
    transportFrame.monitorValues.validityFlags.length( dataSeqLength );
    transportFrame.monitorValues.iSample.length( dataSeqLength );

    preallocateMonitorSampleValues ( transportFrame.monitorValues, *this );

    int k = 0; // cycles through monitor value sequence in
               // TransportSubsystemFrame

    for ( int i = 0; i < transportFrame.numMonitorPoints; ++i ) {
        const MonitorHeader & mh = dataPointers_.monitorHeaders[ i ];

        const tagIDType tagId      = mh.tagID;
        const int nSamples         = mh.nSamples;
        const int offset           = mh.sampleOffset;
        const MonitorValueType mvt = mh.getValueType();

        if ( tagId <= 0 )
            programLogErrorIfPossible( "tagId <= 0" );
        else if ( dbms::TagIDAuthority::getSubsystemID( tagId ) != subsysId )
            programLogErrorIfPossible( "getSubsystemID( tagId ) != subsysId" );

        if ( nSamples < 0 )
            programLogErrorIfPossible( "nSamples < 0" );

        if ( offset < 0 )
            programLogErrorIfPossible( "offset < 0" );
        else if ( offset >= numAllocedSamples ) {
            ostringstream oss;

            oss << "offset (" << offset << ") >= numAllocedSamples ("
                << numAllocedSamples << ")";

            programLogErrorIfPossible( oss.str() );
        }

        // If samples have been allocated correctly, all allocated samples
        // form a contiguous block starting from offset = 0
        // to nSamples-1
        // Collect samples, keeping in mind that single-sample
        // monitor points do not have an average value, so offset 0 is the
        // value

        // [sBegin, sEnd) is the range to write forward iterator style
        const int sBegin = 0;
        const int sEnd =
            ((nSamples <= 0) ? 0 :
                ((nSamples == 1) ? 1 : (nSamples + 1)));

        if ( (offset + sEnd) > numAllocedSamples ) {
            programLogErrorIfPossible(
                "(offset + sEnd) > numAllocedSamples" );
        }

        // Load samples of monitor point in reverse order
        for ( int j = (sEnd - 1); j >= sBegin; --j ) {
            // make sure we're still in range first
            if ( k >= dataSeqLength ) {
                ostringstream oss;

                oss << "Allocated " << dataSeqLength
                    << " spaces, but writing into " << k << " index.";

                programLogErrorIfPossible( oss.str() );

                throw CARMA_ERROR( oss.str() );
            }

            MonitorPointSample
                sample( dataPointers_.writableMonitorValues[ offset + j ] );

            sample.fillInTransportSample( 
                k, transportFrame.monitorValues, mvt, tagId, j );

            ++k;
        }
    }

    if ( k != dataSeqLength ) {
        if ( k < dataSeqLength ) {
            transportFrame.monitorValues.dataType.length( k );
            transportFrame.monitorValues.sequenceIdx.length( k );
            transportFrame.monitorValues.pointID.length( k );
            transportFrame.monitorValues.validityFlags.length( k );
            transportFrame.monitorValues.iSample.length( k );
        }

        ostringstream oss;

        oss << "Allocated " << dataSeqLength
            << " spaces, but filled in " << k << " elements.";

        programLogErrorIfPossible( oss.str() );
    }

    transportFrame.numSamples = k;

    checkHighWaterMarks( transportFrame.monitorValues, 
                         highwaterMonitorSamples_ );

}



ushort
SubsystemFrame::numSamplesToAllocate (const ushort nSamplesPerCycle)
{
    return (nSamplesPerCycle > 1)  ?  (nSamplesPerCycle+1) : nSamplesPerCycle;
}


long
SubsystemFrame::maxNumSamplesIncludingAverages (const long maxMonitorPoints,
                                                 const long maxSamples)
{
    return (maxSamples + (maxMonitorPoints >> 1)); // add in space for averages
                                            // assuming half the monitor points
                                            // will be multi-sample
}

void
SubsystemFrame::writeFromPointSet( const MonitorSampleValues & samples )
{
    const ScopedLogNdc ndc( "SubsystemFrame::writeFromPointSet" );

    const ushort subsysId = getSubsystemID();

    // write sample values for each tagId in dataSeq, making sure that
    // changes to sample rates are reflected in the frame
    // When all samples in current set are written, consolidate frame
    int k  = 0;
    tagIDType prevTagId = 0;
    const int numTransportedSamples = samples.dataType.length(); // Any metadata works here.
    CARMA_CHECK (numTransportedSamples <= this->getNumActualSamples());

    for ( int i = 0; i < numTransportedSamples; ++i ) {

        const tagIDType tagId = dbms::TagIDAuthority::composeTagID(subsysId, 
                                                             samples.pointID[i]);

        const MonitorValueType mvt = samples.dataType[i];

        int index = this->getIndex( tagId );

        if ( index == MONITOR_POINT_ABSENT ) {
            AllocMpInfo info;

            info.tagID = tagId;
            info.valueType = mvt;
            info.nSamplesPerCycle = 0;

            allocateMonitorPoints( &info, 1 );

            index = getIndex( tagId );

            if ( index == MONITOR_POINT_ABSENT )
                throw CARMA_ERROR( "Monitor point allocation failed" );
        }

        MonitorPointHeader header = this->getHeaderByIndex(index);

        // If only the average is sent then nSamples will be zero
        if ( prevTagId != tagId ) {
            // this sample belongs to another monitor point, so
            // reset sample offset and num samples
            // Samples are put into sequence in reverse order, that is,
            // sample # 10 in frame is set as first sample for that
            // monitor point in the sequence dataSeq when # of samples is 10.
            const ushort nSamples = samples.iSample[i];

            if ( nSamples != header.getNumSamplesPerCycle() )
                this->setNumSamplesPerCycle(index, nSamples);

            k = header.getSampleOffset() + static_cast< int >(samples.iSample[i]);

            if ( nSamples > 1 )
                k++;
        }

        CARMA_CHECK (mvt == header.getValueType());
        // CARMA_CHECK (header.getValueType() == inSample.value._d());
        k--;
        CARMA_CHECK ((k-header.getSampleOffset()) >= 0);

        MonitorPointSample  sample (dataPointers_.writableMonitorValues [k]);
        sample.getTransportedSample ( i, samples, index );

        prevTagId = tagId;
    }

    this->consolidateSamples();
}

void
SubsystemFrame::writeFromTransportFrame(
    const TransportSubsystemFrame & transportFrame )
{
    const ScopedLogNdc ndc( "SubsystemFrame::writeFromTransportFrame" );

    this->setFrameCount (transportFrame.frameCount);
    this->setNumMonitorPoints (transportFrame.numMonitorPoints);
    this->setStatusFlags (transportFrame.statusFlags);
    this->setLastWriterDelay (transportFrame.lastWriterDelay);
    setLastWriteTime (transportFrame.lastWriteTime);
    this->setScriberWriteDelay (transportFrame.scriberWriteDelay);
    this->setScriberWriteTime (transportFrame.scriberWriteTime);
    this->setPublishTime (transportFrame.publishTime);
    this->setReceiveTime (transportFrame.receiveTime);

    CARMA_CHECK (this->getNumAllocatedSamples() == transportFrame.numSamples);
    CARMA_CHECK (this->getNumSingleSamplePoints()
                                  == transportFrame.numSingleSamplePoints);
    // make these zero - equivalent to saying this frame has no allocated
    // samples. Doing this makes sense because the samples will be
    // repopulated by the samples in the TransportSubsystemFrame
    this->setNumMonitorSamples (0);
    this->setNumActualSamples (0);
    this->setNumSingleSamplePoints (0);

    // Walk through the sequence of samples in the transported frame
    // and put them in the subsystem frame; just remember that in the case
    // of a monitor point with only one sample, only one sample is transported -
    // in other words, explicitly allocate the average in the case where we
    // we have numSamplesPerCycle > 1.
    //

    //int  numMultiSamplePoints
    //= transportFrame.numMonitorPoints - transportFrame.numSingleSamplePoints;
    int  numTransportedSamples = transportFrame.numSamples;
    CARMA_CHECK ((uint)numTransportedSamples <= transportFrame.monitorValues.dataType.length());

    tagIDType prevTagId = 0;
    int j = 0;  // index to sample values of a monitor point in subsystem frame
                // tells us where we are in the monitor value array for a
                // monitor point
    int k = 0;  // Running counter for total # of samples in subsystem frame
                // includes averages
    for (int  i = 0;  i < numTransportedSamples;  i++)  {

        const ushort pointId = transportFrame.monitorValues.pointID[i];

        const tagIDType tagId =
            dbms::TagIDAuthority::composeTagID( transportFrame.subsystemID, pointId );

        const MonitorValueType mvt = transportFrame.monitorValues.dataType[i];

        int index = getIndex( tagId );

        if ( index == MONITOR_POINT_ABSENT ) {
            AllocMpInfo info;

            info.tagID = tagId;
            info.valueType = mvt;
            info.nSamplesPerCycle = 0;

            allocateMonitorPoints( &info, 1 );

            index = getIndex( tagId );

            if ( index == MONITOR_POINT_ABSENT )
                throw CARMA_ERROR( "Monitor point allocation failed" );
        }

        CARMA_CHECK (index >= 0  &&  index < this->getMaxNumMonitorPoints());

        MonitorPointHeader header (dataPointers_.writableMonitorHeaders[index],
                                   &(dataPointers_.writableMonitorValues[k]));

        if (prevTagId != tagId)  {
            // this sample belongs to another monitor point, so
            // reset sample offset and num samples
            // Samples are put into sequence in reverse order, that is,
            // sample # 10 in frame is set as first sample for that
            // monitor point in the sequence dataSeq when # of samples is 10.
            ushort nSamples = transportFrame.monitorValues.iSample[i];
            CARMA_CHECK (nSamples > 0);
            ushort sampleOffset = this->allocateSamples (index, k, nSamples);
            CARMA_CHECK (sampleOffset == k);
            header.setSampleOffset (sampleOffset);
            header.setSamplesPerCycle (nSamples);
            j = k + nSamples;
            if (nSamples > 1) j++;
        }
        CARMA_CHECK (mvt == header.getValueType());
        // CARMA_CHECK (header.getValueType() == inSample.value._d());
        j--;
        CARMA_CHECK ((j-header.getSampleOffset()) >= 0);

        MonitorPointSample  sample (dataPointers_.writableMonitorValues [j]);

        sample.getTransportedSample ( i, transportFrame.monitorValues, index );

        k++;
        prevTagId = tagId;
    }

    this->setPublished();    // mirror supplier end flag
    this->setReceived();
    CARMA_CHECK (numTransportedSamples == k);
}

void
SubsystemFrame::setNumMonitorPoints( const ushort mpCount )
{
    frame_.numMonitorPoints = mpCount;
}


void
SubsystemFrame::setNumMonitorSamples (ushort numSamples)
{
    frame_.numSamples = numSamples;
}

void
SubsystemFrame::setNumActualSamples (ushort numSamples)
{
    frame_.numActualSamples = numSamples;
}

//  protected methods begin here

MonitorSampleValue*
SubsystemFrame::getSamples() const
{
    return dataPointers_.writableMonitorValues;
}


//
// End SubsystemFrame methods
//

//
// Begin IllegalMonitorPointExceptionObj methods
//


namespace {


string
makeIllegalMonitorPointExceptionMessage( const tagIDType tagId,
                                         const ushort    subsysId )
{
    ostringstream oss;

    oss << "Tag ID = " << tagId
        << " cannot be found in subsystem " << subsysId;

    return oss.str();
}


}  // namespace < anonymous >


IllegalMonitorPointExceptionObj::IllegalMonitorPointExceptionObj (const char* mesg,
                                const char* fileName,
                                const int lineNum)
                      : BaseException (mesg, fileName, lineNum)
{
}


IllegalMonitorPointExceptionObj::IllegalMonitorPointExceptionObj  (ostringstream& errStream,
                                const char* fileName,
                                const int lineNum)
                    : BaseException (errStream, fileName, lineNum)
{
}


IllegalMonitorPointExceptionObj::IllegalMonitorPointExceptionObj(
    const tagIDType    tagId,
    const ushort       subsysId,
    const char * const fileName,
    const int          lineNum ) :
BaseException( makeIllegalMonitorPointExceptionMessage( tagId, subsysId ),
               fileName,
               lineNum )
{
}
