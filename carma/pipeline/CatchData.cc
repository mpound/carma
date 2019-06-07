#include "carma/pipeline/CatchData.h"

#include "carma/corba/Server.h"
#include "carma/correlator/lib/CorrelatorBand.h"
#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/correlator/lib/CorrelatorListener.h"
#include "carma/monitor/PipelineSubsystem.h"
#include "carma/monitor/PipelineSubsystemWB.h"
#include "carma/monitor/PipelineSubsystemSL.h"
#include "carma/monitor/PipelineSubsystemC3G.h"
#include "carma/pipeline/DataCollectorN.h"
#include "carma/pipeline/DataContainer.h"
#include "carma/util/AutoPthreadQuitAndJoinGroup.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/FrameAlignedTimer.h"
#include "carma/util/programLogging.h"
#include "carma/util/rangeFormatting.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/StartPthread.h"
#include "carma/util/ThreadQuit.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

#include <boost/foreach.hpp>
#include <iostream>
#include <ostream>
#include <sstream>
#include <ctime>
#include <cmath>
#include <set>
#include <typeinfo>
#include <unistd.h> // for usleep

using namespace ::std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::correlator::lib;
using namespace carma::correlator::obsRecord2;
using namespace carma::pipeline;
using namespace carma::monitor;

namespace {
    
    const Trace::TraceLevel TRACE_THREADING = Trace::TRACE3;
    
    bool 
    areAllDataCollectorsActive( const vector< DataCollectorN * > & collectors ) 
    {
        // What's your vector Victor? What's your clearance Clarence? Huh? What?
        typedef vector<DataCollectorN *> DataCollectorVector;

        const DataCollectorVector::const_iterator iBegin = collectors.begin( );
        const DataCollectorVector::const_iterator iEnd = collectors.end( );
        DataCollectorVector::const_iterator i;
        for ( i = iBegin; i != iEnd; ++i ) {
            if ( !( (*i)->isCollectorActive( ) ) ) {
                return false;
            }
        }
        return true;
    }

}

struct CatchData::CorrelatorDataStatsInfo {
    double        headerMjd;
    int           maxTxOffsetMs;
    int           maxRxOffsetMs;
    set< int > bandNumbers;

    CorrelatorDataStatsInfo() : 
        headerMjd( 0.0 ),
        maxTxOffsetMs( 0 ),
        maxRxOffsetMs( 0 ) { };

};

struct CatchData::SharedMonitorInfo {

    SharedMonitorInfo( const pair< unsigned int, unsigned int > & bandRange );

    long framesProcessed_; // Aka upframes
    int maxOverallTxOffsetMs; // Since process began
    int maxOverallRxOffsetMs; // Since process began
    long nbc_;
    double pctnbc_;
    const int expectedNbc_;
    carma::util::frameType dataFC_;
    ::std::string seenBands_;
    ::std::map<int, int> recordCnt_;
    ::std::map<int, int> bandCnt_;
    ::std::map<int, float> bandPct_;
    ::std::map<int, CorbaCorrConsumerStats> corrConsumerStats_;
    CatchData::CorrelatorDataStatsInfo cdsi_;
};

CatchData::SharedMonitorInfo::SharedMonitorInfo ( 
        const pair< unsigned int, unsigned int > & bandRange ) :
    framesProcessed_( 0 ),  // counter for number of times data are processed
    maxOverallTxOffsetMs( 0 ),
    maxOverallRxOffsetMs( 0 ),
    nbc_( 0 ),          // number of bands caught in this frame.
    pctnbc_( 0.0 ),     // % of total bands caught since start of process
    expectedNbc_( (bandRange.second + 1) - bandRange.first ),  
    dataFC_( 0 ),
    seenBands_(),
    recordCnt_(),
    bandCnt_(),
    bandPct_(),
    corrConsumerStats_()
{
    const int bandNoBegin = bandRange.first;
    const int bandNoEnd = bandRange.second + 1; 
    for (int bandNo = bandNoBegin; bandNo < bandNoEnd; ++bandNo ) {
        bandCnt_[bandNo] = 0;
        recordCnt_[bandNo] = 0;
    }
}

class CatchData::MonitorUpdateTQRH : 
    public carma::util::ThreadQuitRequestHandler 
{
public:
    MonitorUpdateTQRH( CatchData & parent );

    ~MonitorUpdateTQRH();

    void HandleQuitRequest( ::pthread_t thread );

private:
    
    CatchData & parent_;

};

CatchData::MonitorUpdateTQRH::MonitorUpdateTQRH( CatchData & parent )
    : parent_( parent )
{
    // Nothing
}

CatchData::MonitorUpdateTQRH::~MonitorUpdateTQRH( )
{ 
    // Nothing
}

void
CatchData::MonitorUpdateTQRH::HandleQuitRequest( ::pthread_t thread )
{
    parent_.frameTimer_.InterruptPresentOrNextWait();
}

CatchData::CatchData( const PipelineType pt, 
                      const long monitorWriteDelayMs,
                      const int corrDataWriteDelayMs,
                      const string & channelPrefix, 
                      const string & channelSuffix,
                      carma::corba::Server & server ) : 
pt_( pt ),
waitTimeInMillis_( corrDataWriteDelayMs ),
_dataContainer( ), 
_listeners(),
smi_( new SharedMonitorInfo( getAstrobandRange( pt ) ) ),
frameTimer_( monitorWriteDelayMs * 1000L * 1000L )
{
    switch ( pt_ ) {
        case SL:
            monitorData_ = auto_ptr< PipelineSubsystem >( 
                new PipelineSubsystemSL() );
            break;
        case WB:
            monitorData_ = auto_ptr< PipelineSubsystem >(
                new PipelineSubsystemWB() );
            break;
        case C3G23:
            monitorData_ = auto_ptr< PipelineSubsystem >(
                new PipelineSubsystemC3gMax23() );
            break;
        case C3G8:
            monitorData_ = auto_ptr< PipelineSubsystem >(
                new PipelineSubsystemC3gMax8() );
            break;
        default:
            throw CARMA_ERROR( "Invalid pipeline type." );
    } 
    
    monitorUpdateThread_ = StartPthreadWithRef(
                                monitorUpdateThread,
                                ( *this ), 
                                "CatchData::MonitorUpdateThread" );

    const pair< unsigned int, unsigned int > astrobandRange = 
        getAstrobandRange( pt_ );

    const unsigned int abBegin = astrobandRange.first;
    const unsigned int abEnd = astrobandRange.second + 1;
    for ( unsigned abNo = abBegin; abNo < abEnd; ++abNo ) {
        ostringstream channelName;
        channelName << channelPrefix << abNo << channelSuffix;
        collectors_.push_back( new DataCollectorN( channelName.str(), 
                                                   _dataContainer,
                                                   server ) );
    }
}

CatchData::~CatchData( )
{
    {
        AutoPthreadQuitAndJoinGroup autoquit( monitorUpdateThread_ ); 
    }

    // Cleanup our collectors
    BOOST_FOREACH( DataCollectorN * dcp, collectors_ ) {
        delete dcp;
        dcp = 0;
    }
}

void
CatchData::harvestCorrelatorDataStatsInfo(
    CorrelatorDataStatsInfo * const cdsi,
    const CorrelatorData &          cd )
{
    cdsi->headerMjd = cd.getHeader().getMJD();
    const double dataFrameEnd = 
        Time::MJD( Time::computeClosestFrame(cdsi->headerMjd) + 1 );

    cdsi->maxTxOffsetMs = static_cast< int >( Time::MILLISECONDS_PER_DAY * 
        ( cd.getHeader().getTransmissionMJD() - dataFrameEnd ) );
    cdsi->maxRxOffsetMs = static_cast< int >( Time::MILLISECONDS_PER_DAY * 
        ( cd.getHeader().getReceivedMJD() - dataFrameEnd ) );

    const vector< CorrelatorBand > & bands = cd.getBands();
    const size_t bsize = bands.size();
    
    cdsi->bandNumbers.clear();
    for ( size_t i = 0; i < bsize; ++i )
        cdsi->bandNumbers.insert( bands[i].getBandNumber() );
}


void
CatchData::run( )
{
    int waitMillis = waitTimeInMillis_;
    
    if ( waitMillis < 50 ) {
        waitMillis = 50;

        programLogErrorIfPossible( "Pinned wait time in CatchData::run" );
    }

    {
        ostringstream oss;
        
        oss << "Wait time is " << waitMillis << " ms in CatchData::run";
        
        programLogInfoIfPossible( oss.str() );
    }

    const int waitMillisAfterFrame = waitMillis % 500;
    const int framesBuffered = 1 + ( waitMillis / 500 );

    {
        ostringstream msg;
        
        msg << "Wait time after frame is " << waitMillisAfterFrame 
            << ", buffering " << framesBuffered << " frames.";

        programLogInfoIfPossible( msg.str( ) );
    }

    FrameAlignedTimer timer( waitMillisAfterFrame * 1000 * 1000 );
    size_t numBandsHighWaterMark = 0;
    
    timer.ResetNextFireTimeAndWait( );  

    frameType currentDataFrame = Time::computeCurrentFrame( ) - framesBuffered;
    performUpdate( true, &numBandsHighWaterMark, currentDataFrame );

    bool allCollectorsActive = true;
    while ( allCollectorsActive ) {
        timer.WaitForNextFireTime();
        performUpdate( false, &numBandsHighWaterMark, ++currentDataFrame );
        allCollectorsActive = areAllDataCollectorsActive( collectors_ );
    }
}


void
CatchData::performUpdate( const bool     firstTime,
                          size_t * const numBandsHighWaterMark,
                          const frameType frame )
{
    // after sleeping for the required time, listeners are notified and a
    // new container created for the next half second
    CorrelatorData cd;
    _dataContainer.getCorrelatorData( &cd, frame );
    
    CorrelatorDataStatsInfo cdsi;
    harvestCorrelatorDataStatsInfo( &cdsi, cd );
    
    notifyListeners( &cd );

    if ( firstTime != true )
        processMonitorPoints( cdsi );
    
    size_t newHwm = cdsi.bandNumbers.size();
    if ( numBandsHighWaterMark != 0 ) {
        const size_t oldHwm = *numBandsHighWaterMark;
        
        if ( oldHwm > newHwm )
            newHwm = oldHwm;
        else if ( oldHwm < newHwm )
            *numBandsHighWaterMark = newHwm;
    }
    
    // Pin the number of band slots preemptively reserved to a max of 8
    const size_t kMaxBandsReserved = 8;
    _dataContainer.setExpectedNumberOfBands( ::std::min( kMaxBandsReserved,
                                                         newHwm ) );
    const frameType clearDataOlderThanFrame = frame + 1;
    _dataContainer.clearCorrelatorData( clearDataOlderThanFrame );
}


void
CatchData::processMonitorPoints( const CorrelatorDataStatsInfo & cdsi )
{
    computeStats( cdsi );

    { 
        ScopedPthreadMutexLock scopelock( smiMutex_ );
        smi_->cdsi_ = cdsi;
    }
}


void
CatchData::computeStats( const CorrelatorDataStatsInfo & cdsi )
{
  ScopedPthreadMutexLock scopelock( smiMutex_ );
  ++( smi_->framesProcessed_ );
  smi_->nbc_ = cdsi.bandNumbers.size();
  smi_->dataFC_ = Time::computeClosestFrame( cdsi.headerMjd );

  // following block waits until a band is caught at least once, then zero's 
  // statistics counters for that band only. This is mainly for startup since 
  // all bands come up at various times.  

  // Update caught record count.
  BOOST_FOREACH( const int & bandNum, cdsi.bandNumbers ) {
    // If first time we've seen this band, reset the record count. 
    if ( smi_->bandCnt_[bandNum] == 0 ) 
        smi_->recordCnt_[bandNum] = 0;

    smi_->bandCnt_[bandNum] += 1;
  }

  // Update record counts and % caught for all bands...
  smi_->pctnbc_ = 0.0;
  map<int, int>::iterator bandMapIt = smi_->recordCnt_.begin();
  for ( ; bandMapIt != smi_->recordCnt_.end(); ++bandMapIt ) {
    bandMapIt->second += 1;

    const int bandNo = bandMapIt->first;
    smi_->bandPct_[bandNo] = 
        smi_->bandCnt_[bandNo] * 100. / bandMapIt->second;

    smi_->pctnbc_ += smi_->bandPct_[bandNo];
  }

  // Calculate a weighted average where each band is given equal weight
  if ( smi_->pctnbc_ != 0 ) 
      smi_->pctnbc_ /= smi_->expectedNbc_;
  
  smi_->seenBands_ = formatAsRanges( cdsi.bandNumbers );

  int presumedBandIdx = 0;
  typedef vector< DataCollectorN * > CollectorVec;
  CollectorVec::const_iterator c = collectors_.begin();
  const CollectorVec::const_iterator cEnd = collectors_.end();
  for ( ; c != cEnd; ++c ) {
    smi_->corrConsumerStats_[ presumedBandIdx ] = 
        (*c)->getCorbaCorrConsumerStats( );
    ++presumedBandIdx;
  }

  // Set our max/min rx/tx latencies.
  if ( cdsi.maxTxOffsetMs > smi_->maxOverallTxOffsetMs )
    smi_->maxOverallTxOffsetMs = cdsi.maxTxOffsetMs;

  if ( cdsi.maxRxOffsetMs > smi_->maxOverallRxOffsetMs )
    smi_->maxOverallRxOffsetMs = cdsi.maxRxOffsetMs;

}

void CatchData::fillMonitorData( ) const
try {
    monitor::CatchDataStage & mon = monitorData_->getCatchDataStage( );

    ScopedPthreadMutexLock scopelock( smiMutex_ );
    mon.dataFrameCount().setValue( smi_->dataFC_ );
    mon.lastTransmissionOffset().setValue( smi_->cdsi_.maxTxOffsetMs ); 
    mon.lastReceiveOffset().setValue( smi_->cdsi_.maxRxOffsetMs );
    mon.maxTransmissionOffset().setValue( smi_->maxOverallTxOffsetMs );
    mon.maxReceiveOffset().setValue( smi_->maxOverallRxOffsetMs );
    mon.numberOfBandsCaught().setValue( smi_->nbc_ );
    mon.pctCaughtTotal().setValue( smi_->pctnbc_ );
    mon.bandsSeen().setValue( smi_->seenBands_ );
    mon.upFrames().setValue( smi_->framesProcessed_ );

    const DataContainer::LateBandMap lateBands = 
        _dataContainer.getLateBandMap(); 
    
    const DataContainer::DupBandMap duplicateBands = 
        _dataContainer.getDuplicateBandMap();

    const pair< unsigned int, unsigned int > astrobandRange = 
        getAstrobandRange( pt_ );
    const unsigned short bandNoBegin = astrobandRange.first;

    typedef ::std::map<int, CorbaCorrConsumerStats> CorrStatMap;
    const int bandCount = monitorData_->getBandCount();
    for ( int bandIdx = 0; bandIdx < bandCount; ++bandIdx ) {

        map<int, float>::const_iterator bandPctPos = 
            smi_->bandPct_.find(bandNoBegin + bandIdx);
        if ( bandPctPos == smi_->bandPct_.end() ) 
            continue;

        CorrStatMap::const_iterator corrStatPos = 
            smi_->corrConsumerStats_.find( bandIdx );
        if ( corrStatPos == smi_->corrConsumerStats_.end() ) {
            ostringstream err;
            err << "No CorbaCorrConsumerStats found for bandIdx = " 
                << bandIdx << ". Skipping setting of monitor data." ;
            programLogErrorIfPossible( err.str() ); 
            continue;
        }

        const CorbaCorrConsumerStats & corrStats = corrStatPos->second;
        
        monitor::CatchDataBand & monBand = 
            monitorData_->getCatchDataBand( bandIdx ); 
        monBand.pctCaught().setValue( bandPctPos->second );
        monBand.active( ).setValue( corrStats.active );
        monBand.corbaObjectName().setValue( corrStats.notificationChannelName );
        monBand.deserializationError().setValue( 
            corrStats.errorOnLastDeserialization );
        monBand.deserializationErrorCount().setValue( 
            corrStats.deserializationErrorCount );
        monBand.corbaDemarshallingTime().setValue( 
            corrStats.corbaDemarshalingTimeInMs );
        monBand.deserializationTime().setValue( 
            corrStats.deserializationTimeInMs );
        monBand.totalProcTime().setValue( corrStats.totalProcTimeInMs );
        monBand.assemblyLatency().setValue( corrStats.assemblyLatencyInMs );
        monBand.transmissionLatency().setValue( corrStats.transmitLatencyInMs );
        monBand.receiveLatency().setValue( corrStats.receiveLatencyInMs );

        const int bandNum = bandIdx + 1;
        const map<int, int>::const_iterator pos 
            = smi_->bandCnt_.find( bandNum );
        if ( pos == smi_->bandCnt_.end( ) ) {
            monBand.droppedRecords().setValue( 0 );
        } else if ( pos->second > 0 ) {
            // Only count as dropped if we've seen at least one record from band
            const int dropped = smi_->recordCnt_[pos->first] - pos->second;
            monBand.droppedRecords().setValue( dropped );
        } else {
            monBand.droppedRecords().setValue( 0 );
        }

        const DataContainer::LateBandMap::const_iterator lateBandIt = 
            lateBands.find( bandNum );
        if ( lateBandIt == lateBands.end() ) 
            monBand.lateRecordsCaught().setValue( 0 );
        else
            monBand.lateRecordsCaught().setValue( lateBandIt->second );

        const DataContainer::DupBandMap::const_iterator dupBandIt = 
            duplicateBands.find( bandNum );
        if ( dupBandIt == duplicateBands.end() )
            monBand.duplicateRecordsCaught().setValue( 0 );
        else 
            monBand.duplicateRecordsCaught().setValue( dupBandIt->second );
    }
        
} catch (...) {
    logCaughtAsError( );
}

void
CatchData::monitorUpdateThread( CatchData & This )
try {

    MonitorUpdateTQRH tqrh( This );
    ScopedThreadQuitRequestHandlerSelf scopedquit( tqrh );

    while ( true ) {
        ThreadQuitTestSelf();
        This.frameTimer_.ResetNextFireTimeAndWait();
        ThreadQuitTestSelf();

        This.fillMonitorData( );
        This.monitorData_->write( );
    }

} catch ( ... ) {

    if ( CaughtExceptionIsThreadQuitRequestedError( ) ) {

        CARMA_CPTRACE( TRACE_THREADING, "CatchData::monitorUpdateThread - "
            "Exiting via a thread quit request." );

        throw;
    } else {
        logCaughtAsError( );
    }
}

void
CatchData::addCorrelatorListener( CorrelatorListener * const corrListener )
{
    const string newName( typeid( *corrListener ).name() );
    ostringstream oss;
    
    oss << "CatchData::addCorrelatorListener("
        << "this=" << static_cast< const void * >( this )
        << ", corrListener=" << static_cast< const void * >( corrListener )
        << " [" << newName << "])";

    programLogInfoIfPossible( oss.str() );
 
    // see if listener is already in list
    list< CorrelatorListener * >::const_iterator i = _listeners.begin();
    const list< CorrelatorListener * >::const_iterator iEnd = _listeners.end();
    for ( ; i != iEnd; ++i ) {
        // ADB: I changed this from using the class name to instead use the
        // pointer.  Using class name is a poor test for whether or not 
        // a listener is already in the list since you can have many 
        // instances of a listener of the same class and hence the same name.
        if ( *i == corrListener ) {
            programLogInfoIfPossible( 
                "Listener \"" + newName + "\" already in list" );
            return;
        }
    }

    // if we get here, then object needs to be added to list.
    _listeners.push_back( corrListener );
}


void
CatchData::notifyListeners( CorrelatorData * const cd ) const
{
    list< CorrelatorListener * >::const_iterator i = _listeners.begin();
    const list< CorrelatorListener * >::const_iterator iEnd = _listeners.end();

    for ( ; i != iEnd; ++i ) {
        (*i)->processData( cd );
    }
}
