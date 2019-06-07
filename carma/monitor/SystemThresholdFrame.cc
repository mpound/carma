/*
 * SystemThresholdFrame.cc - Method definitions for class 
 * holding thresholds for carma monitor system.
 */

/**
 * $Id: SystemThresholdFrame.cc,v 1.24 2010/02/18 17:00:09 abeard Exp $
 */

/*!
 * @file SystemThresholdFrame.cc
 * This is the method definition file for the class that manages all threshold
 * objects for a monitor system (as represented by 
 * ::carma::monitor::MonitorSystem).
 *
 * @author N. S. Amarnath
 *
 * File containing method definitions for SystemThresholdFrame class.
 *
 */

#include "carma/monitor/SystemThresholdFrame.h"

#include <algorithm>
#include <sstream>
#include <vector>

#include "carma/dbms/TagIDAuthority.h"
#include "carma/monitor/types.h"
#include "carma/monitor/MonitorPoint.h"
#include "carma/monitor/MonitorContainer.h"
#include "carma/monitor/MonitorPointThreshold.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/SubsystemFrame.h"
#include "carma/monitor/SystemFrameBuffer.h"
#include "carma/util/ErrorException.h"
#include "carma/util/compileTimeCheck.h"
#include "carma/util/Trace.h"
#include "carma/util/programLogging.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


// Exception objects used by Threshold classes


// ThresholdObjectNotFound exception

ThresholdObjectNotFoundException::ThresholdObjectNotFoundException 
                        (const tagIDType tagID, 
			 const char* fileName, int lineNo) 
      : ErrorException (
            ThresholdObjectNotFoundException::makeMessage (tagID), fileName, lineNo)
{
}


string
ThresholdObjectNotFoundException::makeMessage (const tagIDType tagID)
{
    ostringstream os;
    os << "ThresholdObjectNotFoundException: Threshold object with "
       << "tagID = "
       << tagID
       << " {"
       << dbms::TagIDAuthority::getSubsystemID (tagID)
       << "/"
       << dbms::TagIDAuthority::getPointID (tagID)
       << "} not found."
       << "\n";
    return os.str();
}

 
// End of ThresholdObjectNotFound exception

// ReachedNumThresholdLimit exception


ReachedNumThresholdLimitException::ReachedNumThresholdLimitException 
            (long maxThresholds, tagIDType tagID, 
	     const char* fileName, int lineNo)
  : ErrorException (
       ReachedNumThresholdLimitException::makeMessage (maxThresholds, tagID), 
       fileName, lineNo)
{
}


string
ReachedNumThresholdLimitException::makeMessage (long maxThresholds, 
	                                        tagIDType tagID)
{
    ostringstream os;
    os << "ReachedNumThresholdLimit: Could not allocate threshold object with "
       << "tagID = "
       << tagID
       << " {"
       << dbms::TagIDAuthority::getSubsystemID (tagID)
       << "/"
       << dbms::TagIDAuthority::getPointID (tagID)
       << "} "
       << endl
       << "as the max # of thresholds (= "
       << maxThresholds
       << ") for this system threshold frame will be exceeded."
       << "\n";
    return os.str();
}


// End of ReachedNumThresholdLimit exception

// End of exception definitions

namespace {


class ThresholdTagIdLess {
    public:
        ThresholdTagIdLess( const ThresholdStruct * thresholds,
                            long                    numThresholds );

        bool operator()( int lhsIndex, int rhsIndex ) const;

    private:
        const ThresholdStruct * const thresholds_;
        const long                    numThresholds_;
};


inline
ThresholdTagIdLess::ThresholdTagIdLess(
    const ThresholdStruct * const thresholds,
    const long                    numThresholds ) :
thresholds_( thresholds ),
numThresholds_( numThresholds )
{
}


inline bool
ThresholdTagIdLess::operator()( const int lhsIndex,
                                const int rhsIndex ) const
{
    // Arbitrary rule - bogus data sorts last and all bogus data sorts equal
    if ( (lhsIndex < 0) || (lhsIndex >= numThresholds_) )
        return false;

    if ( (rhsIndex < 0) || (rhsIndex >= numThresholds_) )
        return true;
    
    return (thresholds_[ lhsIndex ].tagID < thresholds_[ rhsIndex ].tagID);
}


}  // namespace < anonymous >


static const  int THRESHOLD_ABSENT = -1;


SystemThresholdFrame::SystemThresholdFrame( MonitorSystem & ms ) :
thresholdFrame_( createThresholdFrameStruct( ms ) ),
monitorSystem_( ms ),
mpThreshPairs_()
{
    initializeThresholdValues();
}


SystemThresholdFrame::~SystemThresholdFrame( )
try {
    destroyThresholdFrameStruct( thresholdFrame_ );
} catch ( ... ) {
    // Just stifle any exceptions
    
    return;
}


long
SystemThresholdFrame::getNumThresholds ( ) const
{
    return thresholdFrame_.numThresholds;
}


long
SystemThresholdFrame::getMaxNumThresholds() const
{
    return thresholdFrame_.maxNumThresholds;
}


MonitorPointThreshold &
SystemThresholdFrame::getThreshold( const tagIDType tagID ) const
{
    const int index = findThreshold( tagID );
    
    if ( index == THRESHOLD_ABSENT )
        throw CARMA_EXCEPTION (ThresholdObjectNotFoundException, tagID);

    return *new MonitorPointThreshold( getThresholdStruct( index ) );
}


void
SystemThresholdFrame::calibrateMonitorSystem( )
{
    vector< MpThreshPair >::const_iterator i = mpThreshPairs_.begin();
    const vector< MpThreshPair >::const_iterator iEnd = mpThreshPairs_.end();
    
    for ( ; i != iEnd; ++i ) {
        const MonitorPointThreshold thresh( *(i->thresholdStruct) );
        
        if ( thresh.isSet() ) 
            i->mp->evaluateTolerance( thresh );
    }
}


ThresholdFrameStruct &
SystemThresholdFrame::getThresholdFrame () const
{
    return thresholdFrame_;
}


void
SystemThresholdFrame::initializeThresholdValues( )
{
    ::timeval beginTime;
    
    gettimeofday( &beginTime, 0 );
    
    const long maxNumThresholds = thresholdFrame_.maxNumThresholds;
    int * const indices = thresholdFrame_.thresholdIndex;

    long newNumThresholds = thresholdFrame_.numThresholds;
    
    if ( newNumThresholds > maxNumThresholds ) {
        ostringstream oss;
        
        oss << "Too many thresholds already: "
            << newNumThresholds << " > " << maxNumThresholds;
            
        throw CARMA_ERROR( oss.str() );
    }
    
    bool ranOutOfRoom = false;
    tagIDType ranOutOfRoomTagId = 0;
    
    // First, just throw them of all into the mix
    {
        MonitorPointIterator itr( monitorSystem_ );

        while ( ++itr ) {
            const MonitorPoint & monitorPoint = itr.getMonitorPoint();
        
            const tagIDType tagId = monitorPoint.getTagID();
    
            if ( newNumThresholds >= maxNumThresholds ) {
                ranOutOfRoom = true;
                ranOutOfRoomTagId = tagId;
                
                break;
            }
            
            ThresholdStruct & thresholdStr =
                thresholdFrame_.thresholds[ newNumThresholds ];
        
            thresholdStr.tagID = tagId;
            thresholdStr.type = monitorPoint.getValuetype();
            thresholdStr.flags = THRESHOLD_NONE_SET;

            MonitorPointThreshold threshold( thresholdStr );

            threshold.setThresholdValuesFromDefaults( monitorPoint );
            
            indices[ newNumThresholds ] = newNumThresholds;

            ++newNumThresholds;
        }
    }
    
    // Second, sort the whole index array by tag ID
    stable_sort( indices,
                 (indices + newNumThresholds),
                 ThresholdTagIdLess( thresholdFrame_.thresholds,
                                     newNumThresholds ) );

    // Third, double check that we are ordered and have no duplicate tag IDs
    // If duplicates do start to happen then I will need to make this code
    // more sophisticated to handle it - Tom Costa 23 June 2006
    if ( newNumThresholds != 0 ) {
        const ThresholdStruct * const thresholds = thresholdFrame_.thresholds;

        bool firstOne = true;
        tagIDType prevTagId = 0;
        
        const int * i = indices;
        const int * const iEnd = indices + newNumThresholds;
        
        for ( ; i != iEnd; ++i ) {
            const int index = *i;
            
            if ( (index < 0) || (index >= newNumThresholds) )
                break;
                
            const tagIDType tagId = thresholds[ index ].tagID;
            
            if ( firstOne )
                firstOne = false;
            else if ( prevTagId >= tagId )
                break;
            
            prevTagId = tagId;
        }
        
        if ( i != iEnd ) {
            const string msg =
                "Tag IDs are not strictly ordered. Tell Tom about this.";
            
            programLogErrorIfPossible( msg );
    
            throw CARMA_ERROR( msg );
        }
    }
                     
    thresholdFrame_.numThresholds = newNumThresholds;
    
    // Finally, produce the final list of the MPs and their paired threshold
    {
        mpThreshPairs_.reserve( newNumThresholds );

        MonitorPointIterator itr( monitorSystem_ );

        while ( ++itr ) {
            MonitorPoint & monitorPoint = itr.getMonitorPoint();
        
            const tagIDType tagId = monitorPoint.getTagID();
            const int index = findThreshold( tagId );
    
            if ( index == THRESHOLD_ABSENT ) {
                throw CARMA_EXCEPTION( ThresholdObjectNotFoundException,
                                       tagId );
            }
            
            ThresholdStruct & thresholdStruct = getThresholdStruct( index );

            {
                const MonitorPointThreshold threshold( thresholdStruct );
                
                monitorPoint.checkThreshold( threshold );
            }
            
            MpThreshPair mpThreshPair;
            
            mpThreshPair.mp = &monitorPoint;
            mpThreshPair.thresholdStruct = &thresholdStruct;
            
            mpThreshPairs_.push_back( mpThreshPair );
        }
    }
    
    {
        ::timeval endTime;
    
        gettimeofday( &endTime, 0 );
        
        const long long oneMillion = 1000LL * 1000LL;
        
        const long long beginUsec =
            oneMillion * beginTime.tv_sec + beginTime.tv_usec;
            
        const long long endUsec =
            oneMillion * endTime.tv_sec + endTime.tv_usec;
            
        const long long deltaUsec = endUsec - beginUsec;
        
        ostringstream oss;
        
        oss << "SystemThresholdFrame::initializeThresholdValues resulted in "
            << newNumThresholds << " thresholds";
            
        if ( ranOutOfRoom )
            oss << " (ran out of room doing it)";
        
        oss << " and took " << deltaUsec << " microseconds ("
            << beginTime.tv_sec << "/" << beginTime.tv_usec << "-"
            << endTime.tv_sec << "/" << endTime.tv_usec << ")";
            
        if ( ranOutOfRoom || (deltaUsec >= oneMillion) )
            programLogErrorIfPossible( oss.str() );
        else
            programLogInfoIfPossible( oss.str() );
    }

    if ( ranOutOfRoom ) {
        throw ReachedNumThresholdLimitException( maxNumThresholds,
                                                 ranOutOfRoomTagId,
                                                 __FILE__,
                                                 __LINE__ );
    }
}


ThresholdStruct &
SystemThresholdFrame::getThresholdStruct( const int index ) const
{
    return getThresholdFrame().thresholds[ index ];
}


int 
SystemThresholdFrame::findThreshold (const tagIDType tagID) const
{
    if (this->getNumThresholds() == 0)
        return THRESHOLD_ABSENT;

    int		start = 0;
    int		end = (this->getNumThresholds() - 1);
    tagIDType   indexTagID = 0;
    int		tagIndex = 0;
    int		interval = 0;

    int startIndex = thresholdFrame_.thresholdIndex[start];
    int endIndex = thresholdFrame_.thresholdIndex[end];
    tagIDType minTagID = thresholdFrame_.thresholds[startIndex].tagID;
    tagIDType maxTagID = thresholdFrame_.thresholds[endIndex].tagID;
    if ((tagID < minTagID)  ||  (tagID > maxTagID))
        return THRESHOLD_ABSENT;

    while ((interval = (end - start)) > 2)  {
        int middle = (start+end) >> 1;
        int middleIndex = thresholdFrame_.thresholdIndex[middle];
        tagIDType middleTagID = thresholdFrame_.thresholds[middleIndex].tagID;
        if (tagID > middleTagID)  {
            start = middle;
            minTagID = middleTagID;
        }  else  {
            end = middle;
            maxTagID = middleTagID;
        }
    }

    tagIndex = thresholdFrame_.thresholdIndex[start],
    indexTagID = thresholdFrame_.thresholds[tagIndex].tagID;
    for (int  i = start;  i <= end  &&  tagID != indexTagID;  i++)  {
        tagIndex = thresholdFrame_.thresholdIndex[i];
        indexTagID = thresholdFrame_.thresholds[tagIndex].tagID;
    }

    if (tagID == indexTagID)
        return tagIndex;

    return THRESHOLD_ABSENT;
}


namespace {
    const size_t kMinPtrAlignment = 16;  // Must be a power of 2
    const size_t kMinSizeAlignment = 8;
}


ThresholdFrameStruct &
SystemThresholdFrame::createThresholdFrameStruct( MonitorSystem & ms )
{
    compileTimeCheck< (kMinPtrAlignment >= kMinSizeAlignment) >();
    compileTimeCheck< ((kMinPtrAlignment % kMinSizeAlignment) == 0) >();
    compileTimeCheck< (kMinPtrAlignment >= 16) >();
    compileTimeCheck< (kMinSizeAlignment >= 8) >();

    const long maxThresholds =
        ms.systemFrameBuffer().getMaxTotalMonitorPoints();
        
    const size_t frameSize = sizeThresholdFrame( maxThresholds );
    
    void * ptr = 0;
    ThresholdFrameStruct * framePtr = 0;
    try {
        const int err = posix_memalign( &ptr, kMinPtrAlignment, frameSize );
        
        if ( (err != 0) || (ptr == 0) ) {
            ostringstream oss;
            
            oss << "posix_memalign failed (" << err << ", " << ptr << ")";
            
            throw CARMA_ERROR( oss.str() );
        }
    
        {
            const size_t p = reinterpret_cast< size_t >( ptr );
            
            ostringstream oss;
            
            oss << kMinPtrAlignment << " byte aligned";
            
            if ( (p % kMinPtrAlignment) != 0 ) {
                programLogErrorIfPossible(
                    "ThresholdFrameStruct pointer is not " + oss.str() );
            } else {
                programLogInfoIfPossible(
                    "ThresholdFrameStruct pointer is at least " + oss.str() );
            }
        }
        
        framePtr = static_cast< ThresholdFrameStruct * >( ptr );
    
        setThresholdFrameSize( *framePtr, maxThresholds );
    } catch ( ... ) {
        if ( ptr != 0 )
            free( ptr );
            
        throw;
    }
    
    return *framePtr;
}


void
SystemThresholdFrame::destroyThresholdFrameStruct(
    ThresholdFrameStruct & frame )
{
    void * const ptr = &frame;
    
    if ( ptr != 0 )
        free( ptr );
}


size_t 
SystemThresholdFrame::sizeThresholdFrame (long maxThresholds)
{
    const size_t frameSize = (sizeof(ThresholdFrameStruct))
                     + maxThresholds*(sizeof(ThresholdStruct) + sizeof(int));

    // round up to a multiple of kMinSizeAlignment
    const size_t remainder = (frameSize % kMinSizeAlignment);
    
    if ( remainder > 0 )
        return (frameSize + (kMinSizeAlignment - remainder));
    else
       return frameSize;
}


ThresholdFrameStruct& 
SystemThresholdFrame::setThresholdFrameSize (ThresholdFrameStruct& frame, 
                                                       long maxThresholds)
{
    frame.maxNumThresholds = maxThresholds;
    fixupFramePointers (frame);
    frame.numThresholds = 0;

    char* endPtr = (char *)((char *)(frame.thresholds)) + maxThresholds*sizeof(ThresholdStruct);
    
    if ( ((size_t)(endPtr - ((char *)&frame))) > sizeThresholdFrame (maxThresholds) ) {
        CARMA_CPTRACE( util::Trace::TRACE1, "Problems at the plant" );
        
        throw CARMA_ERROR( "Problems at the plant" );
    }

    return frame;
}


ThresholdFrameStruct& 
SystemThresholdFrame::fixupFramePointers (ThresholdFrameStruct& frame)
{
    frame.thresholdIndex = (int *)(((char *)&(frame))+sizeof(ThresholdFrameStruct));
    frame.thresholds = (ThresholdStruct *)(((char *)frame.thresholdIndex)
                                        + frame.maxNumThresholds*sizeof(int));

    return frame;
}


