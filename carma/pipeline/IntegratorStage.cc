// $Id: IntegratorStage.cc,v 1.14 2014/07/01 19:46:13 scott Exp $

#include "carma/pipeline/IntegratorStage.h"

#include "carma/monitor/PipelineMonitorInput.h"
#include "carma/monitor/PipelineSubsystem.h"
#include "carma/monitor/MonitorPoint.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/rangeFormatting.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

#include <cmath>
#include <boost/foreach.hpp>
#include <iostream>

using namespace carma::correlator::lib;
using namespace carma::monitor;
using namespace carma::pipeline;
using namespace carma::util;
using namespace std;

namespace {

    const Trace::TraceLevel TRACE_VIS_FILL = Trace::TRACE5;
    const Trace::TraceLevel TRACE_BAND_FLAG = Trace::TRACE7;
    const Trace::TraceLevel TRACE_BASELINE_DELETE = Trace::TRACE2;

    const double MS_PER_S = 1000.0;

    const double MISSING_DATA_THRESH = 0.80;
    const double TRACKING_THRESHOLD = 0.36;

    // Multimap to provide for easy looping via reverse baseline->band 
    // constructs rather than band->baseline constructs.
    typedef pair< int, int > BaselinePair; 
    typedef vector< CorrelatorBand * > BandPtrVector;
    typedef map< BaselinePair, BandPtrVector > BaselineBandMap;

    BaselineBandMap
    buildBaselineBandMap( CorrelatorData & cd )
    {
        BaselineBandMap answer;

        // Loop over bands and baselines, for each baseline found, add the
        // corresponding band to the multimap
        BandVector & bands = cd.getBands( );
        const BandVector::iterator bBegin = bands.begin( );
        const BandVector::iterator bEnd = bands.end( );
        for ( BandVector::iterator b = bBegin; b != bEnd; ++b ) {
            
            const BaselineVector & baselines = b->getBaselines( );
            const BaselineVector::const_iterator blEnd = baselines.end( );
            BaselineVector::const_iterator bl = baselines.begin( );
            for ( ; bl != blEnd; ++bl ) {

                const BaselinePair blPair( bl->getInput1Number( ),
                                           bl->getInput2Number( ) );

                BaselineBandMap::iterator pos = answer.find( blPair );
                if ( pos == answer.end( ) ) {
                    pair< BaselineBandMap::iterator, bool > insertstat =  
                        answer.insert( BaselineBandMap::value_type( 
                            blPair, BandPtrVector( ) ) ); 
                    if ( insertstat.second == true ) {
                        pos = insertstat.first;
                    } else {
                        throw CARMA_EXCEPTION( ErrorException, "Insert fail." );
                    }
                }
                
                pos->second.push_back( &( *b ) );

                { 
                    ostringstream msg;
                    msg << "Adding band " <<  ( b->getBandNumber( ) )
                        << " to baseline (" 
                        << bl->getInput1Number( ) << ", " 
                        << bl->getInput2Number( ) << ").";
                    CARMA_CPTRACE( TRACE_BASELINE_DELETE, msg.str( ) );
                }
            }
        } 

        return answer;
    }

    float 
    getMaxBaselineIntegrationTime( const CorrelatorBaseline & baseline ) 
    {
        float maxIntegTime = 0.0;
        float minIntegTime = numeric_limits< float >::max( );

        // Loop over sidebands
        const SidebandVector & sidebands = baseline.getSidebands( );
        BOOST_FOREACH( const CorrelatorSideband & sideband, sidebands ) {
            // Calculate max and min integration times
            float intTime = sideband.getStats( ).getIntegrationTime( );
            maxIntegTime = std::max( maxIntegTime, intTime );
            minIntegTime = std::min( minIntegTime, intTime );
        }

        CARMA_CPTRACE( TRACE_BAND_FLAG, "getMaxBaselineIntegrationTime() - "
            << "Max and min integration times for baseline " 
            << baseline.getInput1Number( ) << "-" << baseline.getInput2Number( )
            << " are " << maxIntegTime << ", and " << minIntegTime << "." );

        return maxIntegTime;
    }

    void
    flagBandsWithMissingData( const double nominalIntegTimeInSec,
                              CorrelatorBand & cb )
    {
        ScopedLogNdc ndc( "Integrator::flagBandsWithMissingData" );
        const double nominalIntegTime = nominalIntegTimeInSec * MS_PER_S;

        BaselineVector & baselines = cb.getBaselines( );
        BOOST_FOREACH( CorrelatorBaseline & bl, baselines ) {

            double maxIntegTime = getMaxBaselineIntegrationTime( bl );

            if ( maxIntegTime < MISSING_DATA_THRESH * nominalIntegTime ) {

                CARMA_CPTRACE( TRACE_BAND_FLAG, "flagBandsWithMissingData"
                        "() - Marking baseline " << bl.getInput1Number( )
                        << "-" << bl.getInput2Number( ) 
                        << " BLANKED_FLAGGED." );

                bl.flagData( CorrelatorSideband::NO_REASON );

            } // if maxIntegTime < MISSING_DATA_THRESH * nominalIntegTime
        } // End loop over baselines
    } // flagBandsWithMissingData( )
    
    void
    flagBandsForMajorTrackingErrors( const double nominalIntegTimeInSec,
                                     CorrelatorBand & cb )
    {
        ScopedLogNdc ndc( "Integrator::flagBandsForMajorTrackingErrors" );

        BaselineVector & baselines = cb.getBaselines( );
        BOOST_FOREACH( CorrelatorBaseline & bl, baselines ) {

            SidebandVector & sidebands = bl.getSidebands();
            BOOST_FOREACH( CorrelatorSideband & sb, sidebands ) {
                pair< unsigned, unsigned > trackingCount = 
                    sb.getMinorTrackBfCount();
                pair< double, double > trackingErrPct( 
                    trackingCount.first * 0.5 / nominalIntegTimeInSec,
                    trackingCount.second * 0.5 / nominalIntegTimeInSec );

                if ( trackingErrPct.first >= TRACKING_THRESHOLD ) { 
                    sb.flagData( CorrelatorSideband::A1_MAJOR_TRACKING );
                    sb.unflagData(CorrelatorSideband::A1_MINOR_TRACKING);
                }
                if ( trackingErrPct.second >= TRACKING_THRESHOLD ) { 
                    sb.flagData( CorrelatorSideband::A2_MAJOR_TRACKING );
                    sb.unflagData(CorrelatorSideband::A2_MINOR_TRACKING);
                }

            } // End loop over sidebands
        } // End loop over baselines
    } // flagBandsForMajorTrackingErrors( )

    bool
    isCorrelatorDataValid( const CorrelatorData * cd ) 
    {
        // Verifies validity of the entire cd object based on contained data.
        // The object is only invalid if every sideband contains invalid data.
        const BandVector & bands = cd->getBands( );
        const BandVector::const_iterator bBegin = bands.begin( );
        const BandVector::const_iterator bEnd = bands.end( );
        for ( BandVector::const_iterator b = bBegin; b != bEnd; ++b ) {
            
            const BaselineVector & baselines = b->getBaselines( );
            const BaselineVector::const_iterator blEnd = baselines.end( );
            BaselineVector::const_iterator bl = baselines.begin( );
            for ( ; bl != blEnd; ++bl ) {

                if ( bl->isValid( ) ) {
                    return true;
                }
            } // loop over baselines
        } // loop over bands

        return false;
    } // isCorrelatorDataValid()

} // namespace <unnamed>

struct Integrator::SharedIntegratorInfo {

    SharedIntegratorInfo( );

    double intTime;
    double gapTime;
    int recordsToIntegrate; // no. of records
    int framesPerRecord;    // no. of frames in a single integrated record
    int framesPerGap;       // no. of frames in a single inter-record gap
    int frameCount; 
    double integStopMjd;
    frameType integrationNumber; // 1st frame of integ or 0 when not integrating
    frameType integrationStartFrame; // When to start integrating
    bool integrating;  // Simple flag to tell if integration is in progress
    bool recordIntegrating; // Flag for integrating a record (not gap)
    int lastIntegrationNumber; // Integ# for last completed integration
    unsigned long pendingSeqNo;
    unsigned long currentSeqNo;
    PthreadMutex mutex;
};

Integrator::SharedIntegratorInfo::SharedIntegratorInfo( ) :
    intTime( 10.0 ),
    recordsToIntegrate( 0 ),
    framesPerRecord( 0 ),
    framesPerGap(0),
    frameCount( 0 ),
    integStopMjd( Time::MJD( ) - 1200 / Time::SECONDS_PER_DAY ),
    integrationNumber( 0 ),
    integrationStartFrame( 0 ),
    integrating( false ),
    recordIntegrating(false),
    lastIntegrationNumber(1),
    pendingSeqNo( 0 ),
    currentSeqNo( 0 )
{ 
    // Nothing
}
    
/**.......................................................................
 * Constructor.
 */
Integrator::Integrator( PipelineSubsystem& monitor,
                        const PipelineMonitorInput & plmi ) : 
    Stage( monitor.getIntegratorStageStats( ), "Integrator" ),
    monitorData_( monitor ),
    shared_( new SharedIntegratorInfo ),
    integrateThisFrame_( false ),
    firstFrameInRecord_( false ),
    lastFrameInRecord_( false ),
    noData_( false ),
    plmi_( plmi )
{
  bfMode_ = "N/A";
}

/**.......................................................................
 * Destructor.
 */
Integrator::~Integrator() {}

void Integrator::startIntegration(const double intTime, 
                                  const int numRecords,
                                  const double gapTime,
                                  const int seqNo) 
{
    ScopedPthreadMutexLock scopelock( shared_->mutex );

    shared_->intTime               = intTime;
    shared_->gapTime               = gapTime;
    shared_->recordsToIntegrate    = numRecords;
    shared_->framesPerRecord       = static_cast<int>(round(intTime * 2.0));
    shared_->framesPerGap          = static_cast<int>(round(gapTime * 2.0));
    shared_->integrationStartFrame = Time::computeCurrentFrame( ) + 1;
    shared_->pendingSeqNo          = seqNo;
}

void Integrator::stopIntegration() 
{
    ScopedPthreadMutexLock scopelock( shared_->mutex );

    stopIntegrationHoldingLock();
}

void Integrator::stopIntegrationHoldingLock() 
{
    shared_->intTime               = 0.0;
    shared_->recordsToIntegrate    = 0;
    shared_->framesPerRecord       = 0;
    shared_->framesPerGap          = 0;
    shared_->frameCount            = 0;
    shared_->integrationStartFrame = 0;
    shared_->integStopMjd          = Time::MJD( ); 
    shared_->integrationNumber     = 0;
    shared_->integrating           = false;
    shared_->recordIntegrating     = false;
    if (shared_->pendingSeqNo != 0 && 
        shared_->pendingSeqNo != shared_->currentSeqNo ) {
            shared_->currentSeqNo = shared_->pendingSeqNo;
            shared_->pendingSeqNo = 0;
    }
}

void Integrator::resetTimeSinceLastIntegration( ) 
{
    ScopedPthreadMutexLock scopelock( shared_->mutex );
    shared_->integStopMjd     = Time::MJD( );  // Close enough to data frame
}

void
Integrator::preprocess( const CorrelatorDataPtr cd ) 
{
    // Simplified control variables derived from various counts. These are 
    // local copies of mutex protected data for the following reasons:
    // 0) To minimize serialization of multi threaded band processing.
    // 1) To guarantee transactional integration of a frame (all or nothing).
    // 2) To avoid thread contention in processBand.
    integrateThisFrame_ = false;
    firstFrameInRecord_ = false;
    lastFrameInRecord_ = false;
    noData_ = false;

    ScopedPthreadMutexLock scopelock( shared_->mutex );

    const frameType currentDataFrame = getDataFrameCount( );

    if ( shared_->recordsToIntegrate > 0 && shared_->framesPerRecord > 0 &&
         currentDataFrame >= shared_->integrationStartFrame ) {
    
        // Determine first if we are done integrating - bail if we are.
        const int recordsIntegrated = shared_->frameCount / 
                                      shared_->framesPerRecord;
        
        const bool doneIntegrating = ( recordsIntegrated >=
                                       shared_->recordsToIntegrate );

        if ( doneIntegrating ) {
            shared_->lastIntegrationNumber = shared_->integrationNumber;
            stopIntegrationHoldingLock( ); // Needs to be holding write lock
            return;
        }
        
        const int framesIntegratedInCurrentRecord = shared_->frameCount %
                                                    shared_->framesPerRecord;

        firstFrameInRecord_ = ( framesIntegratedInCurrentRecord == 0 );
        lastFrameInRecord_ = ( (framesIntegratedInCurrentRecord + 1) >=
                               shared_->framesPerRecord ); 
        
        if ( firstFrameInRecord_ ) {

            cdAccumulator_ = cd;

            // Reset time since last integration when we start integrating 
            // rather than when we finish.
            if ( shared_->integrationNumber > 0 ) {
                shared_->lastIntegrationNumber = shared_->integrationNumber;
            }

            reserveExpectedBands( );

            shared_->integrationNumber = currentDataFrame;
        }

        integrateThisFrame_        = true;
        shared_->integrating       = true;
        shared_->recordIntegrating = true;
        if ( cd->getNumberOfBands() == 0 ) 
            noData_ = true;
    } 
    
}

void
Integrator::processBand( CorrelatorBand * cb )
{
    if ( cb == 0 || cdAccumulator_ == 0 ) return;

    if ( !integrateThisFrame_ ) return;

    ScopedLogNdc ndc( "integrateThisFrame_ == true" );

    const int bandNo = cb->getBandNumber();

    // Make sure that the accumulator has an entry for the input band.
    if ( ! cdAccumulator_->hasBand( bandNo ) ) {
        ostringstream err; 
        err << "CorrelatorData accumulator does not have an entry for band "
            << bandNo << "! This band will be dropped from the integration.";
        programLogErrorIfPossible( err.str() ); 
        return;
    }


    if ( ! firstFrameInRecord_ && cdAccumulator_.get() != 0 ) {
        // This is a critical call because of the fact that underlying
        // CorrelatorData class contains a vector of bands which can 
        // grow based on discrepancies between accumulator and input bands.
        // CorrelatorData is not thread-safe and in particular the addIn
        // method can reallocate memory which can invalidate under the 
        // hood iterators being used in other processBand threads leading
        // to disaster.  Care is taken to not invalidate by always 
        // reserving max bands but ultimately CorrelatorData needs to be 
        // rewritten to be thread-safe.  Now before you go and just do this, 
        // realize that doing this with a big naive lock (or effectively the 
        // same thing) will just serialize everything the pipeline has gone 
        // out of it's way to parallelize.  See call to reserveBands in 
        // pipeline.cc and see implementation of CorrelatorData::addIn. 
        cdAccumulator_->addIn( *cb ); 
    }

    if ( lastFrameInRecord_ ) {
        
        ScopedLogNdc ndc( "lastFrameInRecord_ == true" );

        CorrelatorBand & accumulatorBand = cdAccumulator_->getBand( bandNo );

        accumulatorBand.normalize( ); // Also computes stats on integrated band

        flagBandsForMajorTrackingErrors( shared_->intTime, accumulatorBand );

        flagBandsWithMissingData( shared_->intTime, accumulatorBand );

        accumulatorBand.setMJD( Time::MJD( shared_->integrationNumber ) );
    }
   
}

CorrelatorDataPtr
Integrator::postprocess( CorrelatorDataPtr cd ) 
{
    if ( cd == 0 || cdAccumulator_ == 0 ) return CorrelatorDataPtr();

    if ( !integrateThisFrame_ ) return CorrelatorDataPtr();
    
    ScopedPthreadMutexLock scopelock( shared_->mutex );
    
    ++( shared_->frameCount );

    if ( lastFrameInRecord_ ) {

        cdAccumulator_->setHeaderMJD( Time::MJD( shared_->integrationNumber ) );

        return cdAccumulator_;
    } // If last frame in record.
    
    return CorrelatorDataPtr( ); // Again terminate the pipeline
}

void 
Integrator::fillMonitorData( ) 
try {
    carma::monitor::IntegratorStage & mon = monitorData_.getIntegratorStage( );
    mon.mode().setValue( bfMode_ );
    
    ScopedPthreadMutexLock scopelock( shared_->mutex );
        
    int recordsIntegrated = 0;
    int framesIntegratedInCurrentRecord = 0;
    if ( shared_->framesPerRecord > 0 ) {
        const int frameCountPreInteg = shared_->frameCount - 1; 
        recordsIntegrated = frameCountPreInteg / shared_->framesPerRecord;
        framesIntegratedInCurrentRecord = ( frameCountPreInteg %
                                           shared_->framesPerRecord );
    }

    mon.integrationNumber().setValue( shared_->integrationNumber );

    mon.numberOfRecords( ).setValue( shared_->recordsToIntegrate );
    mon.integrating().setValue( shared_->integrating );
    mon.recordIntegrating().setValue(shared_->recordIntegrating);
    monitorData_.lastIntegration().integNumber().setValue(
                shared_->lastIntegrationNumber);
    if ( shared_->integrating ) {
        mon.timeSinceLastIntegration( ).setValue(  0.0 );

    } else {
        mon.timeSinceLastIntegration( ).setValue(  
            ( Time::MJD() - shared_->integStopMjd ) * Time::SECONDS_PER_DAY );
    }
    mon.noData().setValue( noData_ );
    mon.frameCount().setValue( framesIntegratedInCurrentRecord );
    mon.recordCount().setValue( recordsIntegrated );
    mon.desiredIntegTime().setValue( shared_->intTime );
    mon.gapTime().setValue(shared_->gapTime);
    mon.currentSeqNo().setValue( shared_->currentSeqNo );
    mon.pendingSeqNo().setValue( shared_->pendingSeqNo );

} catch (...) {
    programLogErrorIfPossible( "Integrator::fillMonitorData"
        "( ) - Exception caught & NOT throwing to a higher context.  Message "
        "is: " + getStringForCaught() );
}

void
Integrator::reserveExpectedBands( ) 
{
    ScopedLogNdc ndc( __PRETTY_FUNCTION__ );

    // Check that the accumulator has the expected number of bands.
    // This is critical for safe threading (see note in processBand too)
    // as we must not add or remove bands once we start integrating 
    // (doing so leads to invalid iterators in separate threads).  
    const vector< int > expectedAstroBands = plmi_.getMappedAstroBandNumbers();
    BOOST_FOREACH( const int abNo, expectedAstroBands ) {

        if ( !cdAccumulator_->hasBand( abNo ) ) {
            CorrelatorBand cb;
            cb.setBandNumber( abNo );
            cdAccumulator_->addBand( cb );
        }
    }
}
