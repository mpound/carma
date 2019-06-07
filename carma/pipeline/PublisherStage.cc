/**
 *  Class used to publish Correlator Data and send data out via
 *  a notification channel
 *
 *  Author: Rick Hobbs
 *  Author: Andy Beard (modified to Stage structuring - decouple from write)
 */

#include "carma/pipeline/PublisherStage.h"

#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/correlator/obsRecord2/CorbaCorrProducer.h"
#include "carma/monitor/PipelineSubsystem.h"
#include "carma/util/AutoPthreadQuitAndJoinGroup.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/StartPthread.h"
#include "carma/util/ThreadQuit.h"
#include "carma/util/Trace.h"

#include <boost/foreach.hpp>

using namespace boost;
using namespace carma::correlator::lib;
using namespace carma::correlator::obsRecord2;
using namespace carma::monitor;
using namespace carma::pipeline;
using namespace carma::util;
using namespace ::std;

namespace {

    const Trace::TraceLevel TRACE_THREAD_TEARDOWN = Trace::TRACE1;

} // namespace <unnamed>

Publisher::Shared::Shared( ) :
    corbaSendTimeAcc( ),
    lastCorbaSendMillis( 0.0f ),
    lastCorbaSendKilobytes( 0.0f )
{

}

Publisher::PublishCorrDataTQRH::PublishCorrDataTQRH( 
    Publisher & mom ) :
        mom_( mom )
{

}

Publisher::PublishCorrDataTQRH::~PublishCorrDataTQRH( )
{
    // Nothing
}

void
Publisher::PublishCorrDataTQRH::HandleQuitRequest( ::pthread_t thread )
{
    // Push an empty correlator data ptr onto the queue 
    Publisher::PublishRequest dummyRqst;
    dummyRqst.data = CorrelatorDataPtr();

    mom_.publishRequestQueue_.push( dummyRqst );
}

Publisher::Publisher(
    PipelineSubsystem & monitor,
    const string &      channelName,
    const string &      servedObjectName,
    const carma::pipeline::PipelineType plType ) : 
Stage( monitor.getCorrelatorPublisherStageStats(), "Publisher" ),
shared_(),
publishRequestQueue_( ),
monitorData_( monitor ),
maxQueueRequests_( 10 )
{
    // Create a corr producer for each band.
    const AstrobandRange abRange( getAstrobandRange( plType ) );
    for ( unsigned bandNo = abRange.first; bandNo <= abRange.second; ++bandNo ){
        ostringstream bandNoOss;
        bandNoOss << bandNo;
        const string bandNoString( bandNoOss.str() );
        const string bandChannelName( channelName + bandNoString );
        const string bandObjectName( servedObjectName + "Band" + bandNoString );
        CorrProducerPtr corrProducer( new CorbaCorrProducer( bandChannelName ));
        corrProducers_[ bandNo ] = corrProducer;  
    }

    publisherThreadId_ = 
        StartPthreadWithRef( publishCorrDataThread,
                             *this,
                             "Publisher::publishCorrDataThread" );
}

Publisher::~Publisher( )
{
    try {
        RequestThreadQuit( publisherThreadId_ );
        AutoPthreadQuitAndJoinGroup quitter;
        quitter.insert( publisherThreadId_, 
                AutoPthreadQuitAndJoinGroup::JOIN_ONLY_ACTION );
    } catch ( ... ) {
        // Just stifle any exceptions

    }
}

void
Publisher::preprocess( CorrelatorDataPtr cd )
{
    return;
}

void
Publisher::processBand( CorrelatorBand * cb )
{
    return;
}

CorrelatorDataPtr
Publisher::postprocess( CorrelatorDataPtr cd )
{
    
    Publisher::PublishRequest rqst;
    rqst.data = cd;

    if ( publishRequestQueue_.size() > maxQueueRequests_ ) {
        ostringstream err;
        err << "Publisher::postprocess() there are already " 
            << maxQueueRequests_ << " publish requests pending.  Not queueing "
            << "any more as it could overflow allocated resources.";
        programLogErrorIfPossible( err.str() );
    } else {
        publishRequestQueue_.push( rqst );
    }
        
    return cd;
}

void
Publisher::fillMonitorData( )
{
    
    CorrelatorPublisherStage & corrPubStage =
        monitorData_.getCorrelatorPublisherStage();

    ScopedPthreadMutexLock lock( shared_.mutex );

    const std::size_t count = accumulators::count( shared_.corbaSendTimeAcc ); 

    corrPubStage.recordsPublished().setValue( count );
    if ( count > 0 ) {
        corrPubStage.lastCorbaSendLatency().setValue( 
            shared_.lastCorbaSendMillis );
        corrPubStage.corbaSendLatencyMin().setValue( 
            accumulators::min( shared_.corbaSendTimeAcc ) );
        corrPubStage.corbaSendLatencyMax().setValue(
            accumulators::max( shared_.corbaSendTimeAcc ) );
        corrPubStage.corbaSendLatencyAvg().setValue(
            accumulators::mean( shared_.corbaSendTimeAcc ) );
        corrPubStage.corbaSendLatencyStdDev().setValue(
            ::sqrt( ::fabs( accumulators::variance( 
                shared_.corbaSendTimeAcc ) ) ) );
    } else {
        corrPubStage.lastCorbaSendLatency().setValidity( 
            MonitorPoint::INVALID_NO_DATA );
        corrPubStage.corbaSendLatencyMin().setValidity( 
            MonitorPoint::INVALID_NO_DATA );
        corrPubStage.corbaSendLatencyMax().setValidity(
            MonitorPoint::INVALID_NO_DATA );
        corrPubStage.corbaSendLatencyAvg().setValidity(
            MonitorPoint::INVALID_NO_DATA );
        corrPubStage.corbaSendLatencyStdDev().setValidity(
            MonitorPoint::INVALID_NO_DATA );
    }
        
    corrPubStage.lastCorbaSendSize().setValue( shared_.lastCorbaSendKilobytes );
}

void
Publisher::publishCorrDataThread( Publisher & This )
try {
    PublishCorrDataTQRH tqrh( This );
    ScopedThreadQuitRequestHandlerSelf scopeQuit( tqrh );

    while ( true ) {
        PublishRequest publishRequest;

        This.publishRequestQueue_.wait_and_pop( publishRequest );
        
        ThreadQuitTestSelf();
            
        double corbaSendMicros = 0.0;
        size_t corbaSendBytes = 0;

        const vector<CorrelatorBand> & bands = publishRequest.data->getBands();
        BOOST_FOREACH( const CorrelatorBand & b, bands ) {

            double bandCorbaSendMicros = 0.0;
            size_t bandCorbaSendBytes = 0;
            const int bandNo = b.getBandNumber();

            CorrelatorData cd;
            cd.setHeaderMJD( publishRequest.data->getHeader().getMJD() );
            cd.addBand( b ); 

            This.corrProducers_[ bandNo ]->sendCorData( 
                    cd,
                    &bandCorbaSendMicros, 
                    &bandCorbaSendBytes );

            corbaSendMicros += bandCorbaSendMicros;
            corbaSendBytes += bandCorbaSendBytes;
        } // Loop over request correlator bands
            
        {
            ScopedPthreadMutexLock scopelock( This.shared_.mutex ); 
            This.shared_.lastCorbaSendMillis = (corbaSendMicros / 1000.0);
            This.shared_.corbaSendTimeAcc( This.shared_.lastCorbaSendMillis );
            This.shared_.lastCorbaSendKilobytes = 
                (static_cast< float >( corbaSendBytes ) / 1024.0f);
        }
        
        ThreadQuitTestSelf();
    } // Loop forever hopefully
} catch (...) {
    if ( CaughtExceptionIsThreadQuitRequestedError( ) ) {
        CARMA_CPTRACE( TRACE_THREAD_TEARDOWN,
                "Publisher::publishCorrDataThread - "
                "Exiting cleanly via a thread quit request." );
        throw;
    } else {
        logCaughtAsError( );
    }
}

