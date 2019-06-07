// $Id: CoherenceStage.h,v 1.8 2013/05/15 15:23:00 abeard Exp $
#ifndef CARMA_PIPELINE_COHERENCESTAGE_H
#define CARMA_PIPELINE_COHERENCESTAGE_H

#include "carma/correlator/lib/CorrelatorPolarization.h"
#include "carma/correlator/lib/CorrelatorSideband.h"
#include "carma/monitor/CorrDesignation.h"
#include "carma/pipeline/Stage.h"
#include "carma/util/SimpleStatisticsAccumulators.h"

#include <map>

namespace carma {

namespace monitor {
    class AstroSubsystem;
    class PipelineMonitorInput;
    class PipelineSubsystem;
}

namespace pipeline {

class CoherenceStage : public carma::pipeline::Stage {
public:

    explicit CoherenceStage( 
        carma::monitor::PipelineSubsystem & monitor, 
        const carma::monitor::PipelineMonitorInput & plmi,
        carma::monitor::AstroSubsystem & astroMonitor,
        MonitorCorrelatorDesignation corrDes );

    ~CoherenceStage( );

    void preprocess( const carma::correlator::lib::CorrelatorDataPtr cd );
    
    void processBand( carma::correlator::lib::CorrelatorBand * cb );

    carma::correlator::lib::CorrelatorDataPtr
    postprocess( carma::correlator::lib::CorrelatorDataPtr cd );

    void fillMonitorData( );

private:

    void setCoherenceMp( 
        int abno, 
        const carma::correlator::lib::AntNoPolPair & antpol,
        carma::correlator::lib::CorrelatorSideband::Flavor flav,
        float meanCoherence );

    bool pipelineSubarrayOwnsAnt( int antNo );

    // These maps, though complicated, make the implementation trivial.
    // Essentially, we form up two maps, one for baseline coherence and one
    // for antenna coherence.  Indexing is via astroband number, 
    // antNo & Pol (via automatic pair hashing) or baseline pairs of antpols 
    // and sideband 'flavor'.  
    typedef std::map< carma::correlator::lib::CorrelatorSideband::Flavor, 
                      carma::util::ComplexFloatRollingStatAccumulator > 
        SbAccMap;
    typedef std::pair< carma::correlator::lib::AntNoPolPair, 
                       carma::correlator::lib::AntNoPolPair > BaselinePair;
    typedef std::map< BaselinePair, SbAccMap > BaselineSbAccMap;

    std::map< int, BaselineSbAccMap > accs_; // Indexed by astroband number.

    typedef std::map< carma::correlator::lib::CorrelatorSideband::Flavor,
                      carma::util::FloatStatAccumulator > 
        SbFloatAccMap;
    typedef std::map< carma::correlator::lib::AntNoPolPair, 
                      SbFloatAccMap > AntNoSbAccMap;
    typedef std::map< int, AntNoSbAccMap > AstroBandCoherenceMap; 

    AstroBandCoherenceMap coherence_;

    typedef std::map< int, std::deque< float > >  AntMaxCoherenceDequeMap;
    AntMaxCoherenceDequeMap max30MinAntCoherence_;

    std::map< int, std::map< int, unsigned long > > seenBandAntCounter_; 

    carma::monitor::AstroSubsystem & astroMon_;
    const carma::monitor::PipelineMonitorInput & plmi_;
    const MonitorCorrelatorDesignation corrDes_;

}; // End class CoherenceStage

} }
#endif
