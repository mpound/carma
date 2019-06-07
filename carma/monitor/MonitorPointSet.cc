#include "carma/monitor/MonitorPointSet.h"

#include <iosfwd>

#include <log4cpp/Category.hh>

#include "carma/corba/Client.h"
#include "carma/dbms/TagIDAuthority.h"
#include "carma/util/Time.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Trace.h"
#include "carma/util/corbaSequenceUtils.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/FrameAlignedTimer.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/monitor/monitorframe.h"
#include "carma/monitor/MonitorPointSample.h"
#include "carma/monitor/MonitorPointHeader.h"
#include "carma/monitor/SubsystemFrame.h"
#include "carma/monitor/SubsystemFrameHeader.h"
#include "carma/monitor/SubsystemFrameBuffer.h"
#include "carma/monitor/MonitorPointUpdateServant.h"
#include "carma/monitor/Runnable.h"

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/ref.hpp>

using namespace ::std;
using namespace ::log4cpp;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


namespace {

int gNoFspLogCounter = 0;

const int N_FRAMES_PER_LOG = 120; // number of frames to pass
                                  // between logging messages for
                                  // the same exception

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
            throw CARMA_ERROR( 
                "MonitorPointSet::checkHighWaterMarks - Invalid value type." );
        }

        if ( candidateHWM > highWaterMarks[mvt] ) 
            highWaterMarks[mvt] = candidateHWM;

    }
} // Check highwater marks

void
preallocateMonitorSampleValues ( MonitorSampleValues & vals,
                                 MonitorPointSet & mpSet )
{

    vals.charValues = CharSeq( 
        mpSet.getHighWaterMark( MONITOR_VALUE_TYPE_BYTE ) );
    vals.shortValues = ShortSeq( 
        mpSet.getHighWaterMark( MONITOR_VALUE_TYPE_SHORT ) );
    vals.longValues = LongSeq( 
        mpSet.getHighWaterMark( MONITOR_VALUE_TYPE_INTEGER ));
    vals.boolValues = BoolSeq(
        mpSet.getHighWaterMark( MONITOR_VALUE_TYPE_BOOLEAN ));
    vals.floatValues = FloatSeq(
        mpSet.getHighWaterMark( MONITOR_VALUE_TYPE_FLOAT ) );
    vals.doubleValues = DoubleSeq(
        mpSet.getHighWaterMark( MONITOR_VALUE_TYPE_DOUBLE ) );
    vals.complexValues = ComplexSeq(
        mpSet.getHighWaterMark( MONITOR_VALUE_TYPE_COMPLEX ));
    vals.stringValues = StringSeq( 
        mpSet.getHighWaterMark( MONITOR_VALUE_TYPE_STRING ) );
    vals.serialNumberValues = SerialNumberSeq(
        mpSet.getHighWaterMark( MONITOR_VALUE_TYPE_SERIAL_NUMBER ) );

} // preallocateMonitorSampleValues 


// maximum auto write delay in seconds
const double MAX_AUTO_WRITE_DELAY = static_cast< double >( 0.4001f );

// delay as float in fractions of a second (milliseconds) from
// the half-second
const double DEFAULT_AUTO_WRITE_DELAY = 0.10;

// typedef vector< MonitorPointSet * > MonitorPointSetVec;
// MonitorPointSetVec writerSeq_;


string
makeMonitorPointSetName( const SubsystemFrame * const frame ) {
    if ( frame == 0 )
        throw CARMA_ERROR( "makeMonitorPointSetName: frame is NULL" );

    const ushort subsysId = frame->getSubsystemID();

    const string subsysName = dbms::TagIDAuthority::getSubsystemName( subsysId );

    ostringstream oss;

    oss << "MonitorPointSet subsysID=" << subsysId
        << " (" << subsysName << ")";

    return oss.str();
}


string
makeScriberName( const SubsystemFrame * const frame ) {
    if ( frame == 0 )
        throw CARMA_ERROR( "makeScriberName: frame is NULL" );

    const ushort subsysId = frame->getSubsystemID();

    return MonitorPointUpdateServant::makeName( subsysId );
}


}  // namespace < anonymous >


MonitorPointSet::MonitorPointSet( auto_ptr< SubsystemFrame > & frame ) :
prewriteMethod_( 0 ),
frameBuffer_( 0 ),
frame_( frame ),
mpModifiedAllocCount_(frame_.get() != 0 ? frame_->getMaxNumMonitorPoints() : 0),
mpModifiedGuard_( PTHREAD_MUTEX_RECURSIVE ),
mpModifiedAny_( false ),
mpModifiedState_( 0 ),
mpModifiedSamplesCount_( 0 ),
writingGuard_( PTHREAD_MUTEX_RECURSIVE ),
writingMpModifiedState_( 0 ),
name_( makeMonitorPointSetName( frame_.get() ) ),
scriberName_( makeScriberName( frame_.get() ) ),
monUpdater_( MonitorPointUpdate::_nil() ),
autoWriterThread_( )
{
    if ( frame_.get() == 0 )
        throw CARMA_ERROR( "MonitorPointSet::MonitorPointSet frame_ is NULL" );

    mpModifiedState_ = new char[ mpModifiedAllocCount_ ];
    writingMpModifiedState_ = new char[ mpModifiedAllocCount_ ];

    for ( int i = 0; i < mpModifiedAllocCount_; ++i ) {
        mpModifiedState_[i] = 0;
        writingMpModifiedState_[i] = 0;
    }
}


MonitorPointSet::MonitorPointSet( const long subsystemID,
                                  const long maxMonitorPoints,
                                  const long maxSamples ) :
prewriteMethod_( 0 ),
frameBuffer_(
    &(SubsystemFrameBuffer::getSubsystemFrameBuffer( subsystemID,
                                                     maxMonitorPoints,
                                                     maxSamples )) ),
frame_( frameBuffer_ ),
mpModifiedAllocCount_(frame_.get() != 0 ? frame_->getMaxNumMonitorPoints() : 0),
mpModifiedGuard_( PTHREAD_MUTEX_RECURSIVE ),
mpModifiedAny_( false ),
mpModifiedState_( 0 ),
mpModifiedSamplesCount_( 0 ),
writingGuard_( PTHREAD_MUTEX_RECURSIVE ),
writingMpModifiedState_( 0 ),
name_( makeMonitorPointSetName( frame_.get() ) ),
scriberName_( makeScriberName( frame_.get() ) ),
monUpdater_( MonitorPointUpdate::_nil() ),
autoWriterThread_( )
{
    if ( frame_.get() == 0 )
        throw CARMA_ERROR( "MonitorPointSet::MonitorPointSet frame_ is NULL" );

    mpModifiedState_ = new char[ mpModifiedAllocCount_ ];
    writingMpModifiedState_ = new char[ mpModifiedAllocCount_ ];

    for ( int i = 0; i < mpModifiedAllocCount_; ++i ) {
        mpModifiedState_[i] = 0;
        writingMpModifiedState_[i] = 0;
    }
}


void
MonitorPointSet::installPrewriteMethod( const Runnable & prewriteMethod )
{
    const Runnable * oldPrewriteMethod = &prewriteMethod;

    {
        const ScopedLock< PthreadMutex > lock( writingGuard_ );

        ::std::swap( prewriteMethod_, oldPrewriteMethod );
    }

    if ( oldPrewriteMethod != 0 ) {
        if ( oldPrewriteMethod == &prewriteMethod )
            programLogWarnIfPossible( "prewrite method was installed multiple times" );
        else
            programLogErrorIfPossible( "new prewrite method clobbered old one" );
    }
}


void
MonitorPointSet::removePrewriteMethod( const Runnable & prewriteMethod )
{
    const Runnable * oldPrewriteMethod = 0;

    {
        const ScopedLock< PthreadMutex > lock( writingGuard_ );

        if ( prewriteMethod_ == &prewriteMethod )
            ::std::swap( prewriteMethod_, oldPrewriteMethod );
    }

    if ( oldPrewriteMethod != &prewriteMethod )
        programLogWarnIfPossible( "prewrite method was not the active one" );
}


MonitorPointSet::~MonitorPointSet()
try {
    // if no MonitorPointSet objects are left in
    // MonitorPointSet:;writerSeq_, theres no need to
    // delete the vector itself as it will be left in the
    // same condition it was in when the object was
    // constructed.
    //
    ScopedLogNdc logNdc( "MonitorPointSet::~MonitorPointSet()" );
    try {
        stopAutoWriter( ); // Ok to call if no autowriter.
    } catch ( ... ) {
        // Just stifle any exception
    }

    try {
        delete [] mpModifiedState_;
        mpModifiedState_ = 0;
    } catch ( ... ) {
        // Just stifle any exception
    }

    try {
        delete [] writingMpModifiedState_;
        writingMpModifiedState_ = 0;
    } catch ( ... ) {
        // Just stifle any exception
    }
} catch ( ... ) {
    // Just stifle any exception

    return;
}


SubsystemFrame &
MonitorPointSet::getSubsystemFrame( ) const
{
    frame_->setMonitorPointSet( const_cast< MonitorPointSet * >( this ) );

    return *frame_;
}


void
MonitorPointSet::markMpAtIndexModified( const int index )
{
    if ( (index < 0) || (index >= mpModifiedAllocCount_) ) {
        ostringstream oss;

        oss << name_ << " markMpAtIndexModified() range error: "
            << index << " is outside [ 0, " << mpModifiedAllocCount_ << " ]";

        programLogErrorIfPossible( oss.str() );

        return;
    }

    // Note that this guard also somewhat guards changes to the number of
    // samples and/or use of consolidateStorage().
    const ScopedLock< PthreadMutex > lock( mpModifiedGuard_ );

    if ( mpModifiedState_[ index ] == 0 ) {
        mpModifiedState_[ index ] = 1;
        mpModifiedAny_ = true;
        mpModifiedSamplesCount_ +=
            getSubsystemFrame().getNumSamplesPerCycle( index );
    }
}


void
MonitorPointSet::markMpWithTagIdModified( const tagIDType tagID )
{
    const int index = getSubsystemFrame().getIndex( tagID );

    if ( index != SubsystemFrame::MONITOR_POINT_ABSENT )
        markMpAtIndexModified( index );
}


MonitorPointSet &
MonitorPointSet::getMonitorPointSet( const long subsystemID,
                                     const long maxMonitorPoints,
                                     const long maxSamples )
{
    if (subsystemID <= 0)  {
        throw CARMA_ERROR("No MonitorPointSet available - subsystemID must be greater than zero");
    }

    MonitorPointSet * const monitorPointSet =
        new MonitorPointSet( subsystemID, maxMonitorPoints, maxSamples );

    return *monitorPointSet;
}


MonitorPointUpdate_var
MonitorPointSet::getScriberHandle( const string & scriberName )
{
    CARMA_CPTRACE( Trace::TRACE7, "Scriber DO name " << scriberName );

    MonitorPointUpdate_var monUpdater;

    try  {
        monUpdater = Program::getProgram().
            getCorbaClient().resolveName< MonitorPointUpdate >( scriberName );
    } catch (...) {
        const string errMsg( getStringForCaught() );
        Category& logger = carma::util::Program::getLogger();
        ostringstream os;
        os << "carma::monitor::MonitorPointSet::getScriberHandle: "
           << "could not obtain handle to scriber " << scriberName << "."
           << "\n" << errMsg;
        logger << Priority::ERROR
               << os.str();
        monUpdater = MonitorPointUpdate::_nil();;
    }

    return monUpdater._retn();
}

unsigned int 
MonitorPointSet::getHighWaterMark( MonitorValueType mvt ) 
{
    return typeHighWaterMarks_[mvt];
}

void
MonitorPointSet::fillMonSampleValuesHoldingMpModifiedLock( 
        MonitorSampleValues & samples,
        const SubsystemFrame & ssFrame,
        const char * const mpModifiedClone )
{
    const ushort subsysId = ssFrame.getSubsystemID();
    const int numAllocedSamples = ssFrame.getNumAllocatedSamples();

    int k = 0;

    const int frameNumMps = ssFrame.getSubsystemHeader().numMonitorPoints;

    const SubsystemDataPointers & dataPointers =
        ssFrame.getSubsystemDataPointers();

    const int iEnd = ::std::min( mpModifiedAllocCount_, frameNumMps );

    if ( iEnd != frameNumMps ) {
        ostringstream oss;
        
        oss << "frameNumMps of " << frameNumMps << " is greater than "
            << "mpModifiedAllocCount of " << mpModifiedAllocCount_;
            
        programLogErrorIfPossible( oss.str() );
    }

    const int metadataSeqLength = samples.dataType.length();

    preallocateMonitorSampleValues ( samples, *this );

    for ( int i = 0; i < iEnd; ++i ) {
        if ( mpModifiedClone[i] == 0 )
            continue;
            
        const MonitorHeader & mh = dataPointers.monitorHeaders[ i ];

        const tagIDType tagId      = mh.tagID;
        const int nSamples         = mh.nSamples;
        const int offset           = mh.sampleOffset;
        const MonitorValueType mvt = mh.getValueType();

        if ( tagId <= 0 )
            programLogErrorIfPossible( "tagId <= 0" );
        else if ( dbms::TagIDAuthority::getSubsystemID( tagId ) != subsysId ) {
            programLogErrorIfPossible(
                "TagIDAuthority::getSubsystemID( tagId ) != subsysId" );
        }

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
        // to numsamples-1

        // [sBegin, sEnd) is the range to write forward iterator style
        const int sBegin = ((nSamples <= 1) ? 0 : 1); // ADB: Skip average? 
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
            if ( k >= metadataSeqLength ) {

                ostringstream oss;
                oss << "Allocated " << metadataSeqLength
                    << " spaces, but writing into " << k << " index "
                    << " for tagId " << tagId << ".";

                programLogErrorIfPossible( oss.str() );

                throw CARMA_ERROR( oss.str() );
            }

            MonitorPointSample
                sample( dataPointers.writableMonitorValues[ offset + j ]);

            sample.fillInTransportSample( k, samples, mvt, tagId, j );

            // mark sample as having been written
            sample.clearValidityFlags();

            ++k;
        }
    }

    if ( k != metadataSeqLength ) {
        if ( k < metadataSeqLength ) {
            samples.dataType.length( k );
            samples.sequenceIdx.length( k );
            samples.pointID.length( k );
            samples.validityFlags.length( k );
            samples.iSample.length( k );
        }
        
        ostringstream oss;
        
        oss << "Allocated " << metadataSeqLength
            << " spaces, but filled in " << k << " elements.";
            
        programLogErrorIfPossible( oss.str() );
    }

    checkHighWaterMarks( samples, typeHighWaterMarks_ );
}


double
MonitorPointSet::writeToScriber( const SubsystemFrame &   ssFrame,
                                 MonitorPointUpdate_var & monUpdater,
                                 const double             delay,
                                 bool &                   writeSucceeded,
                                 const MonitorSampleValues & samples )
{
    writeSucceeded = false;

    long  monitorPointSetTime = SubsystemFrame::computeCurrentFrameTime();

    const int maxRetries = 10;
    int retryCount = 0;
    double timeCallReached = 0.0;

    bool done = false;

    while ( done == false ) {
        try {
            timeCallReached =
                static_cast< double >(
                    monUpdater->monitorPointSampleUpdate( 
                        samples,
                        monitorPointSetTime,
                        delay ) );

            // catch exceptions, if any, and unblock timer,
            // then propagate exception
            writeSucceeded = true;
            done = true;
            gNoFspLogCounter = 0;
        } catch ( ... ) {
            if ( retryCount < maxRetries ) {
                ++retryCount;
            } else {
                done = true;

                if ( (gNoFspLogCounter % N_FRAMES_PER_LOG) == 0 ) {
                    // zeroing the counter probably unnecessary - but just
                    // in case FSP is down for awhile
                    gNoFspLogCounter = 0;

                    ostringstream oss;

                    oss << "MonitorPointSet::writeToScriber: "
                        << "Caught CORBA::SystemException from "
                        << "MonitorPointUpdate::monitorPointSeqUpdate method. "
                        << "Possible cause is that frameScriberPublisher for "
                        << "subsystem with ID " << ssFrame.getSubsystemID()
                        << " is down. Exception was: ";

                    programLogErrorIfPossible( oss.str() );

                    logCaughtAsError();

                    // CSG - do NOT rethrow the exception...
                    //       just catch, log, and continue
                }

                // increment counter
                ++gNoFspLogCounter;
            }
        }
    }

    if ( writeSucceeded == false )
        throw CARMA_ERROR( "Failed to write" );

    return timeCallReached;
}


void
MonitorPointSet::remergeFailedWriteInfoHoldingWritingLock( )
{
    // MAJOR_NOTE: See the MAJOR_NOTE below and understand how this method
    //             can and is called.

    // Note that the guard also somewhat guards changes to the number of
    // samples and/or use of consolidateStorage().
    const ScopedLock< PthreadMutex > lock( mpModifiedGuard_ );

    if ( mpModifiedAny_ )
        programLogWarnIfPossible( "MonitorPointSet::"
            "remergeFailedWriteInfoHoldingWritingLock() - Monitor "
            "points have been modified since beginning write.  These MPs "
            "will not be remerged." );

    for ( int i = 0; i < mpModifiedAllocCount_; ++i ) {
        if ( (writingMpModifiedState_[i] != 0) &&
             (mpModifiedState_[i] == 0) ) {
            mpModifiedState_[i] = 1;
            mpModifiedAny_ = true;
            mpModifiedSamplesCount_ +=
                getSubsystemFrame().getNumSamplesPerCycle( i );
        }
    }
}


void
MonitorPointSet::write( const bool autoWrite, const double autoWriteDelay )
try {
    string ndcString;
    {
        ostringstream oss;

        oss << name_ << " write(" << boolalpha << autoWrite << ")";

        ndcString = oss.str();
    }

    const ScopedLogNdc ndc( ndcString );

    // MAJOR_NOTE: Notice that I am grabbing 2 locks at once. This is fraught
    //             with peril if they are not ALWAYS grabbed in the same order.
    //             If they are not always grabbed in the same order then we
    //             could deadlock two threads that grabbed them in opposite
    //             orders. Right now this is the only place where we grab both
    //             so the order should always be the same unless
    //             (and herein lies the rub) a caller of this routine is
    //             already holding a lock on writingGuard_ when this method is
    //             called.
    const ScopedLock< PthreadMutex > lock( writingGuard_ );

    // execute pre-write here so any monitor point values modified by it get
    // shipped out with this write
    if ( prewriteMethod_ != 0 )
        prewriteMethod_->execute();

    // First do a quick check if we have anything to do
    {
        const ScopedLock< PthreadMutex > lock( mpModifiedGuard_ );

        if ( mpModifiedAny_ == false )
            return;
    }

    if ( CORBA::is_nil( monUpdater_ ) )
        monUpdater_ = MonitorPointSet::getScriberHandle( scriberName_ );

    if ( CORBA::is_nil( monUpdater_ ) ) {
        const string msg = "Scriber unavailable. Write will not take place.";

        programLogErrorIfPossible( msg );

        CARMA_CPTRACE ( Trace::TRACE7, msg );

        return;
    }

    for ( int i = 0; i < mpModifiedAllocCount_; ++i )
        writingMpModifiedState_[i] = 0;

    int writingMpSamplesModifiedCount = 0;
    bool writeSucceeded = false;
        
    MonitorSampleValues samples;

    {
        // Note that this guard also somewhat guards changes to the number of
        // samples and/or use of consolidateStorage().
        const ScopedLock< PthreadMutex > lock( mpModifiedGuard_ );

        consolidateStorage(); // ensure that no holes get transported

        mpModifiedAny_ = false;

        // This probably isn't necessary now that we're just holding the lock
        // until we fill out the sequence.
        ::std::swap( mpModifiedSamplesCount_, writingMpSamplesModifiedCount );
        ::std::swap( mpModifiedState_, writingMpModifiedState_ );
    
        // Create CORBA monitor sample values sequence for transport to FSP
        const SubsystemFrame & ssFrame = getSubsystemFrame();

        const int numAllocedSamples = ssFrame.getNumAllocatedSamples();

        if ( writingMpSamplesModifiedCount > numAllocedSamples )
            programLogErrorIfPossible( 
                "writingMpSamplesModifiedCount > numAllocedSamples" );

        samples.dataType.length( writingMpSamplesModifiedCount );
        samples.sequenceIdx.length( writingMpSamplesModifiedCount );
        samples.pointID.length( writingMpSamplesModifiedCount );
        samples.validityFlags.length( writingMpSamplesModifiedCount );
        samples.iSample.length( writingMpSamplesModifiedCount );

        fillMonSampleValuesHoldingMpModifiedLock( 
            samples,
            ssFrame,
            writingMpModifiedState_ );
    }

    try {

        // From this point on we are committed to either writing the mods or
        // remerging them back into the running tally
        const double delay = (autoWrite ? autoWriteDelay : 0.0);

        startWriteTime_ = Time::MJD();

        inScriberTime_ =
            MonitorPointSet::writeToScriber( getSubsystemFrame(),
                                             monUpdater_,
                                             delay,
                                             writeSucceeded,
                                             samples );

        if ( writeSucceeded == false ) {
            const string msg =
                "writeToScriber didn't throw but write did not succeed";

            throw CARMA_ERROR( msg );
        }
    } catch ( ... ) {
        if ( writeSucceeded == false ) {
            programLogErrorIfPossible( "Remerging failed write info." );

            remergeFailedWriteInfoHoldingWritingLock( );
        }

        throw;
    }

    endWriteTime_ = Time::MJD();
} catch ( ... ) {
    programLogErrorIfPossible(
        "Coming out of MonitorPointSet::write on an exception: " +
        getStringForCaught() );

    throw;
}


unsigned int MonitorPointSet::read()
{
    if ( frameBuffer_ != 0 ) return frameBuffer_->read();
    else return 0; // Guess this is ok...
}



bool
MonitorPointSet::readNewest()
{
    if ( frameBuffer_ != 0 )
        return frameBuffer_->readNewest();
    else
        return false;
}
 
bool
MonitorPointSet::readNewestConditionalCopy()
{
    if ( frameBuffer_ != 0 )
        return frameBuffer_->readNewestConditionalCopy();
    else
        return false;
}
                              // update base frame

void
MonitorPointSet::setNumSamplesPerCycle( const int    index,
                                        const ushort nSamples )
{
    // check to see if index is legal
    if ( index == SubsystemFrame::MONITOR_POINT_ABSENT )
        return;

    // Note that the guard also somewhat guards changes to the number of
    // samples and/or use of consolidateStorage().
    const ScopedLock< PthreadMutex > lock( mpModifiedGuard_ );

    SubsystemFrame & ssFrame = getSubsystemFrame();

    const ushort origSamples = ssFrame.getNumSamplesPerCycle( index );

    if ( origSamples != nSamples ) {
        ssFrame.setNumSamplesPerCycle( index, nSamples );
        consolidateStorage();

        if ( (index >= 0) && (index < mpModifiedAllocCount_) ) {
            if ( mpModifiedState_[ index ] != 0 )
                mpModifiedSamplesCount_ += (nSamples - origSamples);
            else {
                mpModifiedState_[ index ] = 1;
                mpModifiedAny_ = true;
                mpModifiedSamplesCount_ += nSamples;
            }
        }
    }
}


ushort
MonitorPointSet::getSubsystemID( ) const
{
    return frame_->getSubsystemID();
}


SubsystemFrameBuffer &
MonitorPointSet::getBuffer() const
{
    if ( frameBuffer_ == 0 )
        throw CARMA_ERROR( "MonitorPointSet::getBuffer frameBuffer_ is NULL" );

    return *frameBuffer_;
}


void
MonitorPointSet::consolidateStorage()
{
    getSubsystemFrame().consolidateSamples();
}


// Thread related methods for automatic writing of modified monitor
// point samples

namespace {
 
void
verifyAutoWriterDelay( double & delay )
{
    if ( delay < 0.0 ) {
        ostringstream oss;

        oss << "verifyAutoWriterDeay: Requested delay "
           << delay << " is negative";

        throw CARMA_ERROR( oss.str() );
    }

    double delayToUse;

    if ( delay <= MAX_AUTO_WRITE_DELAY )
        delayToUse = delay;
    else {
        const float maxAsFloat = MAX_AUTO_WRITE_DELAY;
        const float cutoff = maxAsFloat + 0.0001f;

        if ( delay <= cutoff )
            delayToUse = MAX_AUTO_WRITE_DELAY;
        else {
            ostringstream oss;

            oss << "verifyAutoWriterDElay: Requested delay "
               << delay << " is greater than the maximum permissible delay of "
               << "MAX_AUTO_WRITE_DELAY (" << MAX_AUTO_WRITE_DELAY << ") by "
               << (delay - MAX_AUTO_WRITE_DELAY);

            throw CARMA_ERROR( oss.str() );
        }
    }

    delay = delayToUse;

}

} // namespace < unnamed > 


void
MonitorPointSet::startAutoWriter( double delay )
{
    verifyAutoWriterDelay( delay );

    const ScopedLogNdc ndc( name_ + " startAutoWriter()" );

    if ( frameBuffer_ == 0 )
        return;

    if ( autoWriterThread_.get_id() != boost::thread::id() ) 
        throw CARMA_ERROR("MonitorPointSet::startAutoWriter already started.");

    {
        ostringstream oss;

        oss << "Creating and starting auto writer thread with a delay of "
            << delay << " seconds";

        programLogInfoIfPossible( oss.str() );
    }

    boost::thread newAutoWriterThread( MonitorPointSet::autoWriterThread,
                                       boost::ref( *this ),
                                       delay );

    autoWriterThread_.swap( newAutoWriterThread );
}


void
MonitorPointSet::startAutoWriter( )
{
    startAutoWriter( DEFAULT_AUTO_WRITE_DELAY );
}


void
MonitorPointSet::stopAutoWriter( )
{
    const ScopedLogNdc ndc( name_ + " stopAutoWriter()" );

    if ( autoWriterThread_.get_id() == boost::thread::id() ) 
        return;

    programLogInfoIfPossible( "Stopping auto writer thread" );

    autoWriterThread_.interrupt();

    const boost::posix_time::time_duration joinTimeout = 
        boost::posix_time::milliseconds( 500 );

    const bool joined = autoWriterThread_.timed_join( joinTimeout );

    if ( !joined ) {
        ostringstream err;
        err << "MonitorPointSet::stopAutoWriter() - Unable to successfully "
            << "join with autowriter thread after " << joinTimeout << ".";
        programLogErrorIfPossible( err.str() );
    }

    boost::thread notAThread;
    autoWriterThread_.swap( notAThread );
}    


bool
MonitorPointSet::autoWriterIsAlive()
{
    return ( autoWriterThread_.get_id() != boost::thread::id() ); 
}

void
MonitorPointSet::autoWriterThread( MonitorPointSet & owner,
                                   const double delayInS )
try {
    const ScopedLogNdc ndc( owner.name_ + " autowriter thread" );
    
    const long delayInNanoseconds = static_cast< long >( delayInS * 1.0e+9 );

    FrameAlignedTimer timer( delayInNanoseconds, 1, true );
    timer.ResetNextFireTime();

    const int retryCount = 5;

    int numFailures_ = 0;

    while ( true ) {

        boost::this_thread::interruption_point();

        try {
            timer.ResetNextFireTimeAndWait();
        } catch ( ... ) {
            logCaughtAsError( ); // Tom's terminate handler might do this too
            throw;
        }

        boost::this_thread::interruption_point();

        try  {
            owner.write( true, delayInS );

            numFailures_ = 0;
        } catch ( const CosEventComm::Disconnected & excep ) {
            ++numFailures_;

            Category & logger = carma::util::Program::getLogger();

            ostringstream oss;

            oss << "MonitorPointSet::autoWriterThread: "
                << "could not send data to scriber "
                << owner.scriberName_ << "."
                << "\n"
                << "CosEventComm::Disconnected exception "
                << excep._info().c_str()
                << " Trial # "
                << numFailures_
                << " of " << retryCount << "\n";

            logger << Priority::ERROR << oss.str();

            // Just stifle the exception
        } catch ( const CORBA::SystemException & ex ) {
            if ( false )
                logCaughtAsError();

            throw CARMA_ERROR( "MonitorPointSet::autoWriterThread:"
                    " CORBA::SystemException encountered" );

            ++numFailures_;

            ostringstream oss;

            oss << "MonitorPointSet::autoWriterThread: CORBA::SystemException encountered"
                << ex._info().c_str()
                << " Trial # "
                << numFailures_
                << " of " << retryCount << "\n";
        } catch ( const carma::util::BaseException & ex ) {
            if ( false )
                logCaughtAsError();

            throw;

            ++numFailures_;

            ostringstream oss;
            oss << "MonitorPointSet::autoWriterThread: carma::util::BaseException encountered "
                << ex.what()
                << " Trial # "
                << numFailures_
                << " of " << retryCount << "\n";
        } catch ( const ::std::exception & ex )  {
            if ( false )
                logCaughtAsError();

            throw;

            ++numFailures_;

            ostringstream oss;

            oss << "MonitorPointSet::autoWriterThread: ::std::exception encountered "
                << ex.what()
                << " Trial # "
                << numFailures_
                << " of " << retryCount << "\n";
        }

        if ( numFailures_ == retryCount ) {
            ostringstream oss;

            oss << "MonitorPointSet::autoWriterThread: Tried to send data to scriber "
                << owner.scriberName_ << numFailures_ << " times."
                << " Failed every time."
                << " Ensure the frameScriberPublsiher for this subsystem is"
                << " running and that the IMR/Name srver are also working."
                << " Terminating auto writer and application.";

            throw CARMA_ERROR( oss.str() );
        }
    }

} catch ( boost::thread_interrupted & ) {
    programLogInfoIfPossible( "MonitorPointSet::autoWriterThread() - thread "
        "interrupted via boost::thread_interrupted exception.  "
        "Thread exiting cleanly." );
} catch (...) {

    ostringstream err;
    err << "Exiting MonitorPointSet::autoWriterThread() - thread "
        << "interrupted via exception.  Thread exiting with error: "
        << getStringForCaught();
    programLogErrorIfPossible( err.str() );
}
