#include "carma/pipeline/CoherenceStage.h"

#include "carma/correlator/lib/CorrelatorBand.h"
#include "carma/monitor/AstroSubsystemExt.h"
#include "carma/monitor/AstroSubsystem.h"
#include "carma/monitor/CorrDesignation.h"
#include "carma/monitor/PipelineMonitorInput.h"
#include "carma/monitor/PipelineSubsystem.h"
#include "carma/util/CorrelatorSet.h"
#include "carma/util/ErrorException.h"
#include "carma/util/SimpleStatisticsAccumulators.h"
#include <boost/foreach.hpp>

using namespace boost;
using namespace boost::accumulators;
using namespace boost::numeric;
using namespace carma;
using namespace carma::pipeline;
using namespace carma::util;
using namespace std;

namespace corrlib = carma::correlator::lib;

namespace {

    namespace ba = boost::accumulators;

    const unsigned int TWO_MINUTES( 60 * 2  * 2 ); // frames
    const unsigned int THIRTY_FIVE_MINUTES( 60 * 2 * 35 ); // frames

    typedef carma::monitor::NoiseStatusMonitorPointEnum NoiseStatusMPE;

} // namespace < unnamed >

CoherenceStage::CoherenceStage( 
        carma::monitor::PipelineSubsystem & monitor, 
        const carma::monitor::PipelineMonitorInput & plmi,
        carma::monitor::AstroSubsystem & astroMonitor, 
        const MonitorCorrelatorDesignation corrDes ) :
    Stage( monitor.getCoherenceStageStats( ), "CoherenceStage" ),
    accs_(),
    coherence_(),
    astroMon_( astroMonitor ), 
    plmi_( plmi ),
    corrDes_( corrDes )
{
    // Preallocate acc_ and coherence_ astroband maps. 
    // This makes this stage threadsafe without needing a per band mutex.
    // The pipeline is only thread-safe on band boundaries in order to 
    // ensure that bands can be processed in parallel - thus a shared mutex
    // in processBand can serialize the whole process and reduce 8 or 16
    // threads to a thrashing mess with only 1 executing at a time.
    for ( int astroband = 1; astroband < 25; ++astroband ) {
        accs_[ astroband ];
        coherence_[ astroband ];
    }
}

CoherenceStage::~CoherenceStage( )
{

}

void 
CoherenceStage::preprocess( const carma::correlator::lib::CorrelatorDataPtr cd )
{
    // Clear the coherence internal maps (but keep astroband preallocation).
    BOOST_FOREACH( AstroBandCoherenceMap::value_type & val, coherence_ ) {
        val.second.clear();
    }
}

void 
CoherenceStage::processBand( carma::correlator::lib::CorrelatorBand * cb )
{
    if ( cb == 0 ) return;

    // If the noise source is on, ignore this frame.
    const NoiseStatusMPE & noiseStatus = plmi_.noiseStatus( corrDes_ );
    if ( noiseStatus.isValid() && 
         noiseStatus.getValue() != NoiseStatusMPE::DISABLED ) 
        return;

    // For this astroband...
    const int abNo( cb->getBandNumber() );

    // Explicitly (NOT via [] operator)  check that astroband entries were 
    // preallocated.  This is critical for thread safety so I throw if not.
    if ( accs_.find( abNo ) == accs_.end() ||
         coherence_.find( abNo ) == coherence_.end() ) 
        throw CARMA_EXCEPTION( ErrorException, "Map preallocation failed!!!" );

    BaselineSbAccMap & blSbAcc = accs_[ abNo ];

    // Loop over all baselines...
    const corrlib::BaselineVector & baselines = cb->getBaselines();
    BOOST_FOREACH( const corrlib::CorrelatorBaseline & baseline, baselines ) {

        // For this baseline, get accumulator map for its' antpols.
        const BaselinePair blAntPols( baseline.getAntPol1(),
                                      baseline.getAntPol2() );
        SbAccMap & sbAcc = blSbAcc[ blAntPols ];

        // Loop over all sidebands...
        const corrlib::SidebandVector & sidebands = baseline.getSidebands();
        BOOST_FOREACH( const corrlib::CorrelatorSideband & sb, sidebands ) {
    
            // For this sideband, retrieve accumulator from above map...

            if ( !sb.isValid() || sb.isAuto() ) continue; // But skip crap
            
            const corrlib::CorrelatorSideband::Flavor flavor( sb.getFlavor() );

            // If an accumulator doesn't yet exist, push one onto the map.
            SbAccMap::iterator sbAccIt = sbAcc.find( flavor );
            if ( sbAccIt == sbAcc.end() ) {

                const std::pair< SbAccMap::iterator, bool > result = 
                    sbAcc.insert( SbAccMap::value_type( flavor, 
                      ComplexFloatRollingStatAccumulator( 
                        ba::tag::rolling_window::window_size = TWO_MINUTES ) ));

                if ( !result.second ) 
                    continue; // Stifle 
                else  
                    sbAccIt = result.first;
            }
            
            // The guts according to Erik's description...

            // a) Take band-averaged visibility
            complex< float > avg = sb.getStats().getAvg();

            // b) Normalize it
            avg /= abs( avg ); 

            // c) Add to running means for each baseline & sideband.
            if ( ::isnan( avg.real() ) || ::isnan( avg.imag() ) )
                ( sbAccIt->second )( complex< float >( 0.0, 0.0 ) );
            else
                ( sbAccIt->second )( avg );

            // d) Take amplitude of running average.
            const complex< float > runningAvg = rolling_mean( sbAccIt->second );
            const float baseCoherence = abs( runningAvg );

            // e) Add it to the running means for each antenna in baseline
            coherence_[ abNo ][ blAntPols.first ][ flavor ]( baseCoherence );
            coherence_[ abNo ][ blAntPols.second ][ flavor ]( baseCoherence );

        }  // Loop over sidebands

    } // Loop over baselines
} // processBand

carma::correlator::lib::CorrelatorDataPtr 
CoherenceStage::postprocess( 
    const carma::correlator::lib::CorrelatorDataPtr cd )
{
    return cd;
}

void 
CoherenceStage::fillMonitorData( )
{
    // Use map of inputs to FloatStatAccumulators to hold all median coherence
    // values per antenna then later determine max to pop onto 30 minute max.
    typedef std::map< int, FloatStatAccumulator > AntFloatStatAccumulator;

    AntFloatStatAccumulator antBandMedians;

    BOOST_FOREACH(const AstroBandCoherenceMap::value_type & abVal, coherence_) {

        const int abNo = abVal.first;

        BOOST_FOREACH(const AntNoSbAccMap::value_type & app, abVal.second) {
            
            const corrlib::AntNoPolPair & antPol = app.first;

            ++( seenBandAntCounter_[ abNo ][ antPol.first ] );

            BOOST_FOREACH(const SbFloatAccMap::value_type& sbVal, app.second){
                
                const corrlib::CorrelatorSideband::Flavor flav = sbVal.first;

                const float meanCoherence( mean( sbVal.second ) );

                setCoherenceMp( abNo, antPol, flav, meanCoherence );

                // Don't add mean coherence to ant band median accumulators
                // unless we have a full buffer of samples.  This is due to 
                // the fact that the mean coherence starts at 1.0 for the first
                // sample and integrates down as the buffer fills.  
                if ( seenBandAntCounter_[abNo][antPol.first] > TWO_MINUTES )
                    antBandMedians[ antPol.first ]( meanCoherence );
                else 
                    antBandMedians[ antPol.first ]( 0.0 );

            } // Loop over sidebands

        } // Loop over antpols 

    } // Loop over astrobands 

    // Take the maximum antenna meanCoherence and add that to the 30 minute
    // running accumulator. 
    BOOST_FOREACH( const AntFloatStatAccumulator::value_type & antBandMedAcc, 
                   antBandMedians ) 
    {
        const int antNo = antBandMedAcc.first;
        const float maxFrameAntMedian = ba::max( antBandMedAcc.second );

        // Finally, set the maximum 30 minute coherence value.  
        max30MinAntCoherence_[ antNo ].push_front( maxFrameAntMedian );
        while ( max30MinAntCoherence_[ antNo ].size() > THIRTY_FIVE_MINUTES )
            max30MinAntCoherence_[ antNo ].pop_back();
        
        // Write the instantaneous max
        if ( pipelineSubarrayOwnsAnt( antNo ) )
            astroMon_.antenna( antNo - 1 ).frameMaxCoherence( ).setValue( 
                maxFrameAntMedian );
    }


    // Finally determine the max mean coherence over the course of the last
    // thirty minutes worth of samples and write it to the monitor system 
    // if ownership is correct.
    BOOST_FOREACH( const AntMaxCoherenceDequeMap::value_type & val, 
                   max30MinAntCoherence_ ) 
    {
        const int antNo = val.first;
        const float max = *max_element( val.second.begin(), val.second.end() );

        if ( pipelineSubarrayOwnsAnt( antNo ) )
            astroMon_.antenna( antNo - 1 ).maxCoherence( ).setValue( max );
    }
}

void
CoherenceStage::setCoherenceMp( 
    int abNo,
    const carma::correlator::lib::AntNoPolPair & antPol,
    carma::correlator::lib::CorrelatorSideband::Flavor flav,
    const float meanCoherence )
{
    const int antNo = antPol.first;
    monitor::AstroSubsystemBase::Band & bandMon = 
        astroMon_.antenna(antNo - 1).band(abNo - 1);
    
    monitor::AstroSubsystemBase::Rx * rxMonPtr;  
    if ( antPol.second == corrlib::LEFT_POL ) {
        rxMonPtr = &( bandMon.leftPol() );
    } else if ( antPol.second == corrlib::RIGHT_POL ) {
        rxMonPtr = &( bandMon.rightPol() );
    } else {
        throw CARMA_EXCEPTION( ErrorException, "Invalid polarization." ); 
    }

    if ( flav == corrlib::CorrelatorSideband::UPPER_FLAVOR ) {
        rxMonPtr->usb().coherence().setValue( meanCoherence );
    } else if ( flav == corrlib::CorrelatorSideband::LOWER_FLAVOR ) {
        rxMonPtr->lsb().coherence().setValue( meanCoherence );
    } else {
        throw CARMA_EXCEPTION( ErrorException, "Invalid sideband." ); 
    }
}

bool
CoherenceStage::pipelineSubarrayOwnsAnt( const int antNo )
{
    // We only write antenna specific MPs if the ant is owned by the subarray 
    // which owns this pipeline.  The reason for this roundabout check is that 
    // some signal path configurations allow for an antenna to still map to 
    // ANY (aka both) correlators even if the correlators are owned by separate 
    // subarrays, thus leading to multiple writers.  If the subarray still owns
    // ANY (or both) then break the tie using spectral line correlator. 

    const MonitorCorrelatorDesignation antCorrDes = plmi_.getAntSubarrayCorrDes( antNo );

    
    // This version generalizes Andy's logic to check whether the
    // antenna's correlator set includes this pipeline's
    // correlator, and to publish the coherence only if this
    // correlator is the first one in the antenna's correlator
    // set
    
    const carma::util::CorrelatorSet antCorrSet(antCorrDes);
    const carma::util::CorrelatorSet plCorrSet(corrDes_);
    if(antCorrSet == plCorrSet || 
       (antCorrSet.includes( corrDes_ ) && antCorrSet.firstCorrelatorIs( corrDes_ ))) {
      return true;
    }
    
    return false;
}


