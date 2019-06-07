#include "carma/monitor/SignalPathMapping.h"
#include "carma/monitor/SignalPathSubsystem.h"
#include "carma/util/IllegalArgumentException.h"

#include <boost/foreach.hpp>
#include <map>
#include <set>

using namespace carma::monitor;
using namespace carma::util;
using namespace std;

namespace {

    typedef PolarizationMonitorPointEnum PolMPE;
 
}

struct SignalPathMapping::Pimpl {

  Pimpl( MonitorCorrelatorDesignation corrDes, const MonitorSystem & cms );

    void updateSingleAstroband( const SignalPathSubsystem::Astroband & abmon,
                                const int astroBandNo,
                                set< int > & seenAnts  );

    vector< int > astrobands;
    vector< int > antennas;
    map< int, set< int > > antennasByBand;
    vector< BandPolPair > bandpols;
    map< int, vector< AntPolPair > > antpols;
    map< int, set< AntPolPair > > antpolsets; // Purely for fast lookups
    map< int, vector< PolType > > polarizations;
    
    int lastMappingUpdate;
    bool changed;
    const MonitorCorrelatorDesignation corrDes;
    const MonitorSystem & cms;

}; // struct SignalPathMapping::Pimpl

SignalPathMapping::Pimpl::Pimpl( const MonitorCorrelatorDesignation des, 
                                 const MonitorSystem & ms ) :
    lastMappingUpdate( 0 ),
    changed( false ),
    corrDes( des ), 
    cms( ms )
{ 

}

void 
SignalPathMapping::Pimpl::updateSingleAstroband( 
    const SignalPathSubsystem::Astroband & abmon, 
    const int astroBandNo,
    set< int > & seenAnts )
{
    set< PolType > seenPols; 
    set< int > ants;

    const int nAstroinputs = SignalPathSubsystem::Astroband::inputCount();
    for ( int aiIdx = 0; aiIdx < nAstroinputs; ++aiIdx ) {

        const monitor::Input & astroinput = abmon.input( aiIdx );

        const CorrDesignation & corrDesMP = astroinput.CORRELATOR_DESIGNATION_MP();

        if ( !corrDesMP.isValid() ) 
            continue;

        if ( corrDesMP.getValue() == (signed)CorrDesignation::NONE ) 
            continue;


        if ( (signed)corrDes != corrDesMP.getValue() )
            continue;

        const MonitorPointInt & astroBandNoMP = astroinput.astroBandNo();
        const MonitorPointInt & astroInputNoMP = astroinput.astroBandInputNo();
        const MonitorPointInt & corrBandNoMP = astroinput.corrBandNo();
        const MonitorPointInt & corrInputNoMP = astroinput.corrBandInputNo();
        const MonitorPointInt & antennaNoMP = astroinput.antennaNo();
        const PolMPE & polarizationMP = astroinput.polarization();

        if ( ! ( astroBandNoMP.isValid() && astroInputNoMP.isValid() &&
                 corrBandNoMP.isValid() && corrInputNoMP.isValid() &&
                 antennaNoMP.isValid() && polarizationMP.isValid() ) )
            continue;

        AntPolPair antPol( antennaNoMP.getValue(), polarizationMP.getValue() );
        
        antpols[ astroBandNo ].push_back( antPol );
        antpolsets[ astroBandNo ].insert( antPol );
        seenAnts.insert( antPol.first );
        ants.insert( antPol.first );
        seenPols.insert( antPol.second );
    }
    
    astrobands.push_back( astroBandNo );

    vector< PolType > pols;
    BOOST_FOREACH( const PolType pol, seenPols ) {
        BandPolPair bp( astroBandNo, pol );
        bandpols.push_back( bp );
        pols.push_back( pol );
    }

    polarizations[ astroBandNo ] = pols;
    antennasByBand[ astroBandNo ] = ants;
}

SignalPathMapping::SignalPathMapping( const MonitorSystem & monitorSystem,
                                      const MonitorCorrelatorDesignation corrDes ) :
    pimpl_( new SignalPathMapping::Pimpl( corrDes, monitorSystem ) )
{
    if ( corrDes == CorrDesignation::NONE ) {
        throw CARMA_EXCEPTION( IllegalArgumentException, "SignalPathMapping "
            " Ctor does not accept a corr des of NONE." ); 
    }
}

SignalPathMapping::~SignalPathMapping( )
{

}

bool
SignalPathMapping::update( )
{
    const SignalPathSubsystem::Mapping & spm = 
        pimpl_->cms.signalPath().mapping();

    const MonitorPointInt & lastModMP = spm.lastModified();
    if ( !lastModMP.isValid() || 
         lastModMP.getValue() == pimpl_->lastMappingUpdate ) {
        pimpl_->changed = false;
        return false;  // Mapping hasn't changed so get out of Dodge.
    }

    // Mapping has changed, update internal mapping
    pimpl_->lastMappingUpdate = lastModMP.getValue();

    // Clear all the internal maps and rebuild them.
    pimpl_->astrobands.clear();
    pimpl_->antennas.clear();
    pimpl_->antennasByBand.clear();
    pimpl_->bandpols.clear();
    pimpl_->antpols.clear();
    pimpl_->antpolsets.clear();
    pimpl_->polarizations.clear();

    set< int > seenAnts;

    const int nAstrobands = SignalPathSubsystem::Mapping::astrobandCount();
    for ( int abIdx = 0; abIdx < nAstrobands; ++abIdx ) {
        const int abNo = abIdx + 1;

        const SignalPathSubsystem::Astroband & astroband = spm.astroband(abIdx);
        const CorrDesignation & corrDes = astroband.CORRELATOR_DESIGNATION_MP();

        if ( !corrDes.isValid() ) continue;

        if ( (corrDes.getValue() != (signed)CorrDesignation::NONE)  ||
             (corrDes.getValue() == (signed)pimpl_->corrDes ) )
        {
            pimpl_->updateSingleAstroband( astroband, abNo, seenAnts );
        }
    }

    BOOST_FOREACH( const int seenAnt, seenAnts ) {
        pimpl_->antennas.push_back( seenAnt );
    }
        
    pimpl_->changed = true;

    return true;
}

bool
SignalPathMapping::changed( ) const
{
    return pimpl_->changed;
}

vector< int >
SignalPathMapping::getMappedAstroBandNumbers( ) const
{
    return pimpl_->astrobands;
}

vector< int >
SignalPathMapping::getMappedAntennaNumbers( ) const
{
    return pimpl_->antennas;
}

set< int >
SignalPathMapping::getMappedAntennaNumbers( const int astroBandNo ) const
{
    set<int> answer;
    const map< int, set< int > >::const_iterator bi = 
        pimpl_->antennasByBand.find( astroBandNo );
    if ( bi != pimpl_->antennasByBand.end() )
        answer = bi->second;

    return answer;
}

vector< PolType >
SignalPathMapping::getMappedPolarizations( const int astroBandNo ) const
{
    vector< PolType > answer;
    const map< int, vector< PolType > >::const_iterator bi = 
        pimpl_->polarizations.find( astroBandNo );
    if ( bi != pimpl_->polarizations.end() )
        answer = bi->second;

    return answer;
}

vector< AntPolPair >
SignalPathMapping::getMappedAntPolPairs( const int astroBandNo ) const
{
    vector< AntPolPair > answer;
    const map< int, vector< AntPolPair > >::const_iterator bi = 
        pimpl_->antpols.find( astroBandNo );
    if ( bi != pimpl_->antpols.end() ) 
        answer = bi->second;

    return answer;
}

vector< BandPolPair >
SignalPathMapping::getMappedBandPolPairs( ) const
{
    return pimpl_->bandpols;
}

bool
SignalPathMapping::signalPathMapped( const int astroBandNo ) const
{
    const map< int, vector< AntPolPair > >::const_iterator bi = 
        pimpl_->antpols.find( astroBandNo );
    return ( bi != pimpl_->antpols.end() );
}

bool
SignalPathMapping::signalPathMapped( const int astroBandNo, 
                                     const AntPolPair & antPol ) const
{
    const map< int, set< AntPolPair > >::const_iterator bi = 
        pimpl_->antpolsets.find( astroBandNo );
    if ( bi == pimpl_->antpolsets.end() )
        return false;

    return ( bi->second.find( antPol ) != bi->second.end() );
}

