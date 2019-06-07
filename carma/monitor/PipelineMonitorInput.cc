#include "carma/monitor/PipelineMonitorInput.h"

#include "carma/monitor/AntennaCommon.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/CorrDesignation.h"
#include "carma/monitor/FaultSubsystem.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/SignalPathSubsystem.h"
#include "carma/monitor/SldcSubsystem.h"
#include "carma/monitor/WbdcSubsystem.h"

#include "carma/util/CorrelatorSet.h"
#include "carma/util/ErrorException.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"


#include <boost/foreach.hpp>
#include <unistd.h>
#include <iostream>

#define NMAX_CORR_INPUT 32
#define DONT_CARE 255

#include <iostream>
#include <sstream>

#define COUT(statement) \
{\
    std::ostringstream _macroOs; \
    _macroOs << statement << std::endl; \
    std::cout << _macroOs.str() << std::endl;\
}

using namespace carma;
using namespace carma::monitor;
using namespace carma::util;
using namespace std;

namespace {

typedef CM::PolarizationMonitorPointEnum PolMPE;

const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE1;

const monitor::ControlBandPoints &
getControlBandPoints( const int astroBandNo,
                      const int corrBandNo,
                      const CarmaMonitorSystem & cms ) 
{
  const ControlSubsystem & ctrl = cms.control();
    
    const int idx = corrBandNo - 1;

    if ( astroBandNo < 9 ) {
        return ctrl.spectralLineCorrelator().slcBand(idx).controlBandPoints();
    } else if ( astroBandNo < 25 ) {
        return ctrl.widebandCorrelator().wbcBand(idx).controlBandPoints();
    } else if ( astroBandNo < 41 ) {
      ThrowCarmaError("Control band points have not yet been defined for the C3G correlator");
    } else {
        throw CARMA_EXCEPTION( ErrorException, "Invalid band number." );
    }
} // getControlBandPoints

} // namespace <unnamed>

struct PipelineMonitorInput::MapDetails {
    AntPolPair antPol;
    int astroBandInputNo;
    int corrBandNo;
    int corrBandInputNo;
    MonitorCorrelatorDesignation corrDes;
};

PipelineMonitorInput::PipelineMonitorInput( const MonitorCorrelatorDesignation corrDes )
    : lastMappingUpdate_( 0 ),
      corrDes_( corrDes ),
      cms_( )
{
    if ( corrDes_ == CorrDesignation::NONE ) {
        throw CARMA_EXCEPTION( IllegalArgumentException, "PipelineMonitorInput"
            " Ctor does not accept a corr des of NONE." );
    }

    CARMA_CPTRACE( TRACE_CTOR_DTOR, "PipelineMonitorInput Ctor." );

    cms_.readNewest();
}

PipelineMonitorInput::~PipelineMonitorInput( )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "PipelineMonitorInput Dtor." );
}

bool
PipelineMonitorInput::waitForInput( const carma::util::frameType frame )
{
    ScopedLogNdc logNDC( "PipelineMonitorInput::waitForInput()" ); 

    frameType cmsFrameCount = cms_.getFrameCount();

    while ( cmsFrameCount < frame ) {
        cms_.read( );
        cmsFrameCount = cms_.getFrameCount();
    }

    // Too late, this frame has come and gone.

    if ( cmsFrameCount > frame ) 
        return false; 

    // Got it, this is the currently desired frame.

    if ( cmsFrameCount == frame ) {
      internalSpmUpdate();
      internalControlUpdate();
      return true; 
    }

    return false; 
}

vector< int >
PipelineMonitorInput::getMappedAstroBandNumbers( ) const
{
    vector< int > answer;

    BOOST_FOREACH( const AstroBandInputMap::value_type & val, astroMapping_ ) {
        answer.push_back( val.first );
    }

    return answer;
}

set< int >
PipelineMonitorInput::getMappedAntennaNumbers( ) const
{
    return mappedAnts_;
}
    
set< int >
PipelineMonitorInput::getMappedInputNumbers( ) const
{
    return mappedInputs_;
}
    
set< PolType >
PipelineMonitorInput::getMappedPols( const int astroBandNo ) const
{
    set< PolType > answer;
    const map< int, set< PolType > >::const_iterator it = 
        astroBandPolMapping_.find( astroBandNo );
    if ( it != astroBandPolMapping_.end() )
        answer = it->second;

    return answer;
}

vector< AntPolPair > 
PipelineMonitorInput::getMappedAntPolPairs( const int astroBandNo ) const
{
    vector< AntPolPair > answer;

    const AstroBandInputMap::const_iterator apm = 
        astroMapping_.find( astroBandNo );

    if ( apm == astroMapping_.end() ) 
        return answer;

    BOOST_FOREACH( const AstroInputMap::value_type & val, apm->second ) {
        answer.push_back( val.second.antPol);
    }

    return answer;
}

vector< BandPolPair >
PipelineMonitorInput::getMappedBandPolPairs( ) const
{
    vector< BandPolPair > answer;
    vector< int > mappedBands = getMappedAstroBandNumbers();
    BOOST_FOREACH( const int astroband, mappedBands ) {
        set< PolType > mappedPols = getMappedPols( astroband );
        BOOST_FOREACH( const PolType pol, mappedPols ) {
            answer.push_back( make_pair( astroband, pol ) );
        }
   }
   return answer;
}

vector< BandInputPair >
PipelineMonitorInput::getMappedAstroBandInputPairs( const int carmaAntNo ) const
{
    const AntAstroPairs::const_iterator cit = 
        mappedAntAstroPairs_.find( carmaAntNo );

    if ( cit != mappedAntAstroPairs_.end() ) 
        return cit->second;
    
    const vector< BandInputPair > empty;
    return empty;
}

bool
PipelineMonitorInput::signalPathMapped( const int astroBandNo ) const
{
    return ( astroMapping_.find( astroBandNo ) != astroMapping_.end() );
}

bool
PipelineMonitorInput::signalPathMapped( const int astroBandNo,
                                        const AntPolPair & antPol ) const
{
    const AstroBandAntPolInputMap::const_iterator apim = 
        antPolMapping_.find( astroBandNo );

    if ( apim == antPolMapping_.end() ) 
        return false;

    return ( apim->second.count( antPol ) > 0 );
}

AntPolPair 
PipelineMonitorInput::getAntPolPair( const int astroBandNo, 
                                     const int astroInputNo ) const
{
    const AntPolPair bad( -1, PolMPE::UNKNOWN );

    const AstroBandInputMap::const_iterator bm = 
        astroMapping_.find( astroBandNo );
    if ( bm == astroMapping_.end() ) return bad;

    const AstroInputMap::const_iterator aim = 
        bm->second.find( astroInputNo ); 
    if ( aim == bm->second.end() ) return bad;
    
    return aim->second.antPol;
}

int 
PipelineMonitorInput::getAstroInputNo( const int astroBandNo, 
                                       const AntPolPair & antPol ) const
{
    const MapDetails & details = getMapDetails( astroBandNo, antPol );

    return details.astroBandInputNo;
}

frameType
PipelineMonitorInput::frameCount( ) const
{
    return cms_.getFrameCount();
}

std::pair< bool, bool >
PipelineMonitorInput::isRxInSideband( const int astroBandNo, 
                                      const AntPolPair & antPol,
                                      const bool usb ) const 
{
    if ( ! signalPathMapped( astroBandNo, antPol ) ) 
        return pair< bool, bool >( true, false ); // nothing in band at all!

    const SignalPathSubsystem::Mapping & spm = cms_.signalPath().mapping();

    const int astroBandIdx = astroBandNo - 1;
    
    if ( !spm.astroband( astroBandIdx ).subarrayNo().isValid() )
        return pair< bool, bool >( false, false );
    
    const int saIdx = spm.astroband( astroBandIdx ).subarrayNo().getValue() - 1;

    if ( !control().subarray( saIdx ).loFreq().isValid() )
        return pair< bool, bool >( false, false );

    const double loFreqGhz = control().subarray( saIdx ).loFreq().getValue();

    // All cm receivers are SSB in LSB only
    if ( loFreqGhz < 50.0 )
        return pair< bool, bool >( true, !usb );

    // Mm receivers 3.5m dishes are SSB in USB all others are DSB
    if ( antPol.first >= 16 )
        return pair< bool, bool >( true, usb );

    return pair< bool, bool >( true, true );
}

const monitor::AntennaCommon &
PipelineMonitorInput::antennaCommon( const int carmaAntNo ) const 
{
    return cms_.antennaCommon( carmaAntNo - 1 );
}

const monitor::ControlBandPoints &
PipelineMonitorInput::controlBandPoints( const int astroBandNo ) const
{
    const int corrBandNo = astroBandNo < 9 ? astroBandNo : astroBandNo - 8;

    return getControlBandPoints( astroBandNo, corrBandNo, cms_ );
}

const MonitorPointFloat &
PipelineMonitorInput::totalPower( const int astroBandNo,
                                  const AntPolPair & antPol ) const
{
    const MapDetails & details = getMapDetails( astroBandNo, antPol );

    const int corrBandIdx = details.corrBandNo - 1;
    const int corrInputIdx = details.corrBandInputNo - 1;
        
    CorrelatorSet corrSet(details.corrDes);

    if(!corrSet.isSingleCorrelator()) {
      ThrowCarmaError("Can't return a single monitor point for multiple correlators: " << corrSet);
    }

    if(corrSet.isSpectral()) {
      return cms_.sldc( ).band( corrBandIdx ).input( corrInputIdx ).psys( );
    } else if(corrSet.isWideband()) {
      return cms_.wbdc( ).band( corrBandIdx ).input( corrInputIdx ).psys( );
    } else if(corrSet.isC3gMax8() || corrSet.isC3gMax23()) {
      ThrowCarmaError("We have yet to determine what the total power monitor points will look like for the C3G correlator");
    } else {
      ThrowCarmaError("Invalid correlator designation: " << corrSet);
    }
    switch ( details.corrDes ) {
    case CorrDesignation::SPECTRAL:
        return cms_.sldc( ).band( corrBandIdx ).input( corrInputIdx ).psys( );
    case CorrDesignation::WIDEBAND:
        return cms_.wbdc( ).band( corrBandIdx ).input( corrInputIdx ).psys( );
    default:
        throw CARMA_EXCEPTION( ErrorException, "Invalid corr des." );
    }
}

const NoiseStatusMonitorPointEnum & 
PipelineMonitorInput::noiseStatus( MonitorCorrelatorDesignation corrDes ) const
{
  CorrelatorSet corrSet(corrDes);

  if(!corrSet.isSingleCorrelator()) {
    ThrowCarmaError("Can't return a single monitor point for multiple correlators: " << corrSet);
  }

  if(corrSet.isSpectral()) {
    return cms_.sldc( ).noiseSourceContainer().noiseSource().noiseStatus();
  } else if(corrSet.isWideband()) {
    return cms_.wbdc( ).noiseSourceContainer().noiseSource().noiseStatus();
  } else if(corrSet.isC3gMax8() || corrSet.isC3gMax23()) {
    ThrowCarmaError("We have yet to determine how the noise source status monitor points will be paritioned among subdivisions of the new correlator");
  } else {
    ThrowCarmaError("Invalid correlator designation: " << corrSet);
  }
}

const MonitorPointByte &
PipelineMonitorInput::blankStatus( const int astroBandNo, 
                                   const AntPolPair & antPol ) const
{
    const MapDetails & details = getMapDetails( astroBandNo, antPol );

    const int astroInputNo = details.astroBandInputNo;
    const int abIdx = astroBandNo - 1;
    const int aiIdx = astroInputNo - 1;
    return cms_.fault().astroband(abIdx).input(aiIdx).status();
}
    

const WeatherSubsystem &
PipelineMonitorInput::weather( ) const
{
    return cms_.weather();
}

const ControlSubsystem &
PipelineMonitorInput::control( ) const
{
    return cms_.control();
}

BandInputPair
PipelineMonitorInput::getCorrBandInputPair( const int astroBandNo, 
                                            const AntPolPair & antPol ) const
{
    const MapDetails & details = getMapDetails( astroBandNo, antPol );

    return BandInputPair( details.corrBandNo, details.corrBandInputNo );
}

MonitorCorrelatorDesignation
PipelineMonitorInput::getAntCorrDes( const int carmaAntNo ) const
{
    const int antIdx = carmaAntNo - 1;
    const SignalPathSubsystem::Mapping & spm = cms_.signalPath().mapping();
    
    const CorrDesignation & corrDesMP = 
        spm.antenna( antIdx ).CORRELATOR_DESIGNATION_MP();

    if ( !corrDesMP.isValid() ) 
      return CorrDesignation::NONE;
    else
      return corrDesMP.getValue();
}

MonitorCorrelatorDesignation 
PipelineMonitorInput::getAntSubarrayCorrDes( const int carmaAntNo ) const
{
    const int antIdx = carmaAntNo - 1;
    const SignalPathSubsystem::Mapping & spm = cms_.signalPath().mapping();
    
    const MonitorPointInt & subarrayNoMP = 
        spm.antenna( antIdx ).subarrayNo();

    if ( !subarrayNoMP.isValid() ) {
      return CorrDesignation::NONE;
    }

    const int subarrayNo = subarrayNoMP.getValue();
    if ( subarrayNo == 0 ) {
      return CorrDesignation::NONE;
    }

    const int subarrayIdx = subarrayNo - 1;
    const SignalPathSubsystem::Subarray & sub = spm.subarray( subarrayIdx );
    
    const CorrDesignation & corrDesMP = sub.CORRELATOR_DESIGNATION_MP( );

    if ( !corrDesMP.isValid() ) 
      return CorrDesignation::NONE;
    else
      return corrDesMP.getValue();
}

int
PipelineMonitorInput::getExpectedChannels( const int astroBandNo ) const
{
    const int nAstrobands = SignalPathSubsystem::Mapping::astrobandCount();
    if ( astroBandNo < 1 || astroBandNo > nAstrobands ) 
        throw CARMA_EXCEPTION( IllegalArgumentException, "Invalid astroband." );

    const int abIdx = astroBandNo - 1;
        
    const SignalPathSubsystem::Astroband & astroband = 
        cms_.signalPath().mapping().astroband( abIdx );
    
    if ( astroband.expectedChannels().isValid() ) 
        return astroband.expectedChannels().getValue();
    else
        return 0;
}

bool 
PipelineMonitorInput::astroBandOnline( const int astroBandNo ) const
{
    if ( astroBandCorrBands_.find( astroBandNo ) == astroBandCorrBands_.end() )
        return false;

    // Consider astroband offline if any mapped corr bands are invalid/offline.
    const set< int > & corrBandNos = 
        astroBandCorrBands_.find( astroBandNo )->second;
    BOOST_FOREACH( const int & corrBandNo, corrBandNos ) {

        const ControlBandPoints & cbp = getControlBandPoints( astroBandNo,
                                                              corrBandNo,
                                                              cms_ );

        if ( !cbp.online().isValid() || !cbp.online().getValue() )
            return false;
    }

    return true;
}

const CarmaMonitorSystem &
PipelineMonitorInput::getCarmaMonitorSystem( ) const
{
    return cms_;
}

void
PipelineMonitorInput::internalSpmUpdate( ) 
{
    const SignalPathSubsystem::Mapping & spm = cms_.signalPath().mapping();

    const MonitorPointInt & lastModMP = spm.lastModified();
    if ( !lastModMP.isValid() || lastModMP.getValue() == lastMappingUpdate_ ) 
        return;

    lastMappingUpdate_ = lastModMP.getValue();

    // Clear internal map and always build up new ones...
    // This is only done when the SPM has changed, not every frame so it's
    // not as expensive  .
    astroMapping_.clear();
    antPolMapping_.clear();
    astroBandPolMapping_.clear();
    mappedAnts_.clear();
    mappedInputs_.clear();
    mappedAntAstroPairs_.clear();
    astroBandCorrBands_.clear();

    const int nAstrobands = SignalPathSubsystem::Mapping::astrobandCount();
    for ( int abIdx = 0; abIdx < nAstrobands; ++abIdx ) {
      const int abNo = abIdx + 1;

      const SignalPathSubsystem::Astroband & astroband = spm.astroband(abIdx);
      const CorrDesignation & corrDes = astroband.CORRELATOR_DESIGNATION_MP();  

      COUT("astroband " << abIdx << " has corrDesignation: " << corrDes << " and our designation = " << corrDes_);

      if ( !corrDes.isValid() ) 
        continue;
      
      CorrelatorSet pipelineCorrSet(corrDes_);
      if (pipelineCorrSet.includes(corrDes.getValue())) 
      {
        internalAstrobandUpdate( astroband, abNo );
      }
    }
}

void
PipelineMonitorInput::internalAstrobandUpdate( 
    const SignalPathSubsystem::Astroband & astroband,
    const int astroBandNo ) 
{
    AstroInputMap astroInputMap;
    AntPolInputMultimap antPolInputMultimap;
    
    const int nAstroinputs = SignalPathSubsystem::Astroband::inputCount();
    for ( int aiIdx = 0; aiIdx < nAstroinputs; ++aiIdx ) {
        const monitor::Input & astroinput = astroband.input( aiIdx );
        
        const CorrDesignation & corrDesMP = astroinput.CORRELATOR_DESIGNATION_MP();
        
        if ( !corrDesMP.isValid() ) 
            continue;

        if ( corrDesMP.getValue() == CorrDesignation::NONE )
            continue;

        // Note that the above corrDesMP should never be ALL because
        // correlator designation is intrinsic to astroband/input.  Ignore
        // ALL outright as it's not sufficient to map to other monitor points.
        
        if ( corrDes_ != corrDesMP.getValue() ) 
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

        const AntPolPair antPol( antennaNoMP.getValue(), 
                                 polarizationMP.getValue() );
        const BandInputPair astroBandInput( astroBandNoMP.getValue(), 
                                            astroInputNoMP.getValue() );
        const BandInputPair corrBandInput( corrBandNoMP.getValue(),
                                           corrInputNoMP.getValue() );

        MapDetails mapEntry;

        mapEntry.antPol = antPol;
        mapEntry.astroBandInputNo = astroBandInput.second;
        mapEntry.corrBandNo = corrBandInput.first;
        mapEntry.corrBandInputNo = corrBandInput.second;
        mapEntry.corrDes = corrDesMP.getValue();

        astroInputMap[ astroInputNoMP.getValue() ] = mapEntry;
        antPolInputMultimap.insert( 
            AntPolInputMultimap::value_type( antPol,  astroBandInput.second ) );
        astroBandPolMapping_[ astroBandNo ].insert( polarizationMP.getValue() );
        mappedAnts_.insert( antennaNoMP.getValue() );
        mappedInputs_.insert( astroInputNoMP.getValue() );
        mappedAntAstroPairs_[ antennaNoMP.getValue() ].push_back( astroBandInput ); 
        astroBandCorrBands_[ astroBandNo ].insert( corrBandNoMP.getValue() );
    }

    astroMapping_[ astroBandNo ] = astroInputMap;
    antPolMapping_[ astroBandNo ] = antPolInputMultimap;
}

/**.......................................................................
 * EML: Update the internal map of manually flagged hardware from control
 * monitor points
 */
void PipelineMonitorInput::internalControlUpdate( ) 
{
  clearControlMaps();
  rebuildControlMaps();
}

/**.......................................................................
 * Clear all control-subsystem flag maps
 */
void PipelineMonitorInput::clearControlMaps() 
{
  clearControlMap(CorrDesignation::SPECTRAL);
  clearControlMap(CorrDesignation::WIDEBAND);
  clearControlMap(CorrDesignation::C3GMAX8);
  clearControlMap(CorrDesignation::C3GMAX23);
}

/**.......................................................................
 * Clear a single control-subsystem flag map
 */
void PipelineMonitorInput::clearControlMap(MonitorCorrelatorDesignation corrDes)
{
  const ControlSubsystem& ctrl = cms_.control();
  int nBand = 0;
  CorrelatorSet corrset(corrDesToCorrType(corrDes));
  if ( corrset.isSpectral() ) {
        nBand = ctrl.spectralLineCorrelator().slcBandCount();
  } else if ( corrset.isWideband() ) {
        nBand = ctrl.widebandCorrelator().wbcBandCount();
  } else if ( corrset.isC3gMax8() ) {
        nBand = ctrl.c3gMax8Correlator().c3gMax8BandCount();
  } else if ( corrset.isC3gMax23() ) {
        nBand = ctrl.c3gMax23Correlator().c3gMax23BandCount();
  } else {
        ThrowCarmaError("Unrecognized, multiple, or empty correlator designation for pipeline monitor input: " << corrDes << "["<<corrset.mpString()<<"]");
  }

  for(int iBand=1; iBand <= nBand; iBand++) {
    for(int iInput1=1; iInput1 <= NMAX_CORR_INPUT; iInput1++) {
      for(int iInput2=1; iInput2 <= NMAX_CORR_INPUT; iInput2++) {
        flagMap_[corrDes][iBand][iInput1][iInput2] = false;
      }
    }
  }
}

/**.......................................................................
 * Rebuild maps of manual control-subsystem flags
 */
void PipelineMonitorInput::rebuildControlMaps() 
{
  rebuildControlMap(CorrDesignation::SPECTRAL);
  rebuildControlMap(CorrDesignation::WIDEBAND);
  rebuildControlMap(CorrDesignation::C3GMAX8);
  rebuildControlMap(CorrDesignation::C3GMAX23);
}

/**.......................................................................
 * Rebuild a single control-subsystem flag map
 */
void PipelineMonitorInput::rebuildControlMap(MonitorCorrelatorDesignation corrDes) 
{
  const ControlSubsystem& ctrl = cms_.control();
  ControlSubsystem::ManualFlagCorrelator* corr = 0;
  int nBand = 0;

  CorrelatorSet corrset(corrDesToCorrType(corrDes));
  if ( corrset.isSpectral() ) {
        nBand = ctrl.spectralLineCorrelator().slcBandCount();
        corr = &ctrl.spectralLineCorrelator().manualFlag();
  } else if ( corrset.isWideband() ) {
        nBand = ctrl.widebandCorrelator().wbcBandCount();
        corr = &ctrl.widebandCorrelator().manualFlag();
  } else if ( corrset.isC3gMax8() ) {
        nBand = ctrl.c3gMax8Correlator().c3gMax8BandCount();
        corr = &ctrl.c3gMax8Correlator().manualFlag();
  } else if ( corrset.isC3gMax23() ) {
        nBand = ctrl.c3gMax23Correlator().c3gMax23BandCount();
        corr  = &ctrl.c3gMax23Correlator().manualFlag();
  } else {
        ThrowCarmaError("Unrecognized, multiple, or empty correlator designation for pipeline monitor input: " << corrDes << "["<<corrset.mpString()<<"]");
  }

  //------------------------------------------------------------
  // Iterate over all slots for this correlator
  //------------------------------------------------------------

  int nSlot = corr->slotCount();
  for(int iSlot=0; iSlot < nSlot; ++iSlot) {
    ControlSubsystem::ManualFlagSlot& slot = corr->slot(iSlot);
    int bandNo   = slot.band().getValue();
    int inputNo1 = slot.input1().getValue();
    int inputNo2 = slot.input2().getValue();

    flagInputs(corrDes, bandNo, inputNo1, inputNo2, nBand);
  }

#if 0
  std::cout << "Just rebuilt control map: "
	    << " SL[1][1][1] = " << flagMap_[MONITOR_CORR_SPECTRAL][1][1][1] << std::endl
	    << " SL[1][1][6] = " << flagMap_[MONITOR_CORR_SPECTRAL][1][1][6] << std::endl
	    << " SL[1][6][7] = " << flagMap_[MONITOR_CORR_SPECTRAL][1][6][7] << std::endl
	    << " SL[8][1][1] = " << flagMap_[MONITOR_CORR_SPECTRAL][8][1][1] << std::endl
	    << " SL[8][1][6] = " << flagMap_[MONITOR_CORR_SPECTRAL][8][1][6] << std::endl
	    << " WB[1][1][1] = " << flagMap_[MONITOR_CORR_WIDEBAND][1][1][1] << std::endl
	    << " WB[1][1][6] = " << flagMap_[MONITOR_CORR_WIDEBAND][1][1][6] << std::endl
	    << " WB[1][6][7] = " << flagMap_[MONITOR_CORR_WIDEBAND][1][6][7] << std::endl
	    << " WB[8][1][1] = " << flagMap_[MONITOR_CORR_WIDEBAND][8][1][1] << std::endl
	    << " WB[8][1][6] = " << flagMap_[MONITOR_CORR_WIDEBAND][8][1][6] << std::endl
	    << std::endl;
#endif
}

/**.......................................................................
 * Flag one or more band, input, or baseline of this correlator
 */
void PipelineMonitorInput::flagInputs(MonitorCorrelatorDesignation corrDes, int bandNo, int inputNo1, int inputNo2, int nMaxBand)
{
  int bandNoStart = (bandNo == DONT_CARE) ? 1 : bandNo;
  int bandNoStop  = (bandNo == DONT_CARE) ? nMaxBand : bandNo;

  int inputNo1Start = (inputNo1 == DONT_CARE) ? 1 : inputNo1;
  int inputNo1Stop  = (inputNo1 == DONT_CARE) ? NMAX_CORR_INPUT : inputNo1;

  int inputNo2Start = (inputNo2 == DONT_CARE) ? 1 : inputNo2;
  int inputNo2Stop  = (inputNo2 == DONT_CARE) ? NMAX_CORR_INPUT : inputNo2;

  for(int bandNo=bandNoStart; bandNo <= bandNoStop; bandNo++) {

    //------------------------------------------------------------
    // In theory, we can receive 255-N or N-255.  I'm going to treat
    // the baseline map as square instead of triangular, to simplfy
    // bookkeeping.  Ie, even though baseline 2-1 will never be
    // encountered in practice, we will set its flag if all baselines
    // of 2 are flagged for example. 
    //------------------------------------------------------------

    for(int inputNo1=inputNo1Start; inputNo1 <= inputNo1Stop; inputNo1++) {
      for(int inputNo2=inputNo2Start; inputNo2 <= inputNo2Stop; inputNo2++) {
	flagMap_[corrDes][bandNo][inputNo1][inputNo2] = true;
      }
    }

    for(int inputNo2=inputNo2Start; inputNo2 <= inputNo2Stop; inputNo2++) {
      for(int inputNo1=inputNo1Start; inputNo1 <= inputNo1Stop; inputNo1++) {
	flagMap_[corrDes][bandNo][inputNo2][inputNo1] = true;
      }
    }

  }
}

const PipelineMonitorInput::MapDetails &
PipelineMonitorInput::getMapDetails( const int astroBandNo,
                                     const AntPolPair & antPol ) const
{
    const AstroBandAntPolInputMap::const_iterator apim = 
        antPolMapping_.find( astroBandNo );
    if ( apim == antPolMapping_.end() )
        throw CARMA_EXCEPTION( IllegalArgumentException, "Band not mapped." );
 
    // Note: Here we return the first input that matches a particular antpol
    // which is wrong because there can be multiple inputs per antpol
    const AntPolInputMultimap::const_iterator apm = apim->second.find( antPol );
    if ( apm == apim->second.end( ) ) 
        throw CARMA_EXCEPTION( IllegalArgumentException, "AntPol not mapped.");

    return getMapDetails( astroBandNo, apm->second );
}

const PipelineMonitorInput::MapDetails &
PipelineMonitorInput::getMapDetails( const int astroBandNo,
                                     const int astroInputNo ) const
{
    const AstroBandInputMap::const_iterator bm = 
        astroMapping_.find( astroBandNo );
    if ( bm == astroMapping_.end() ) 
        throw CARMA_EXCEPTION( IllegalArgumentException, "Band not mapped." );

    const AstroInputMap::const_iterator aim = bm->second.find( astroInputNo ); 
    if ( aim == bm->second.end() ) 
        throw CARMA_EXCEPTION( IllegalArgumentException, "Input not mapped.");

    return aim->second;
}

/**.......................................................................
 * Return the corr designation, band number and input number fo the
 * given astroband number and input
 */
void PipelineMonitorInput::getCorrInfo(int astroBandNo, int astroBandInput, 
				       MonitorCorrelatorDesignation& corrDes, int& corrBandNo, int& corrBandInputNo)
{
  const MapDetails& md = getMapDetails(astroBandNo, astroBandInput);

  corrDes         = md.corrDes;
  corrBandNo      = md.corrBandNo;
  corrBandInputNo = md.corrBandInputNo;
}

/**.......................................................................
 * Return true if this input is flagged
 */
bool PipelineMonitorInput::isManuallyFlagged(int astroBandNo, int astroBandInput1, int astroBandInput2)
{
  const MapDetails& md1 = getMapDetails(astroBandNo, astroBandInput1);
  const MapDetails& md2 = getMapDetails(astroBandNo, astroBandInput2);

  MonitorCorrelatorDesignation corrDes = md1.corrDes;
  int corrBandNo                       = md1.corrBandNo;

  int corrBandInputNo1                 = md1.corrBandInputNo;
  int corrBandInputNo2                 = md2.corrBandInputNo;

  return flagMap_[corrDes][corrBandNo][corrBandInputNo1][corrBandInputNo2];
}
